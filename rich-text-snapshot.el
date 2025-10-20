;;; rich-text-snapshot.el --- Document snapshot management -*- lexical-binding: t -*-

(require 'rich-text-db)
(require 'tabulated-list)
(require 'seq)

(defgroup rich-text-snapshot nil
  "Document snapshot management for rich-text buffers."
  :prefix "rich-text-snapshot-"
  :group 'convenience)

;;;; Database

(defun rich-text-snapshot-ensure-table ()
  "Ensure the snapshot table exists."
  (condition-case err
	  (rich-text-db-crud
	   [:create-table :if-not-exists snapshot
		[id          ; file path (string)
		 timestamp   ; "YYYY-MM-DD HH:MM:SS" (string)
		 content     ; full buffer content (string)
		 note]])     ; user note (string, optional)
	(error (user-error "Failed to create snapshot table: %S" err))))

;;;; Core Functions

;;;###autoload
(defun rich-text-snapshot-save ()
  "Save a full-text snapshot of current buffer (pure text, no styles)."
  (interactive)
  (unless (buffer-file-name)
	(user-error "Buffer must be associated with a file"))

  (let* ((id (buffer-file-name))
		 (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
		 (content (buffer-string))
		 (note (read-string "Snapshot note (optional): " nil nil "")))

	(rich-text-snapshot-ensure-table)
	(condition-case err
		(rich-text-db-insert 'snapshot `([,id ,timestamp ,content ,note]))
	  (error (user-error "Failed to save snapshot: %S" err)))

	(message "✅ Snapshot saved: %s (%d chars)" timestamp (length content))))

;;;###autoload
(defun rich-text-snapshot-restore (timestamp)
  "Restore snapshot into current buffer (overwrites content, clears overlays)."
  (interactive
   (list (rich-text-snapshot--select-timestamp)))

  (unless (buffer-file-name)
	(user-error "Buffer must be associated with a file"))

  (let* ((id (buffer-file-name))
		 (snapshot (car (rich-text-db-crud
						 `[:select [content note]
						   :from snapshot
						   :where (and (= id ,id)
									   (= timestamp ,timestamp))]))))

	(unless snapshot
	  (user-error "Snapshot not found"))

	(when (and (buffer-modified-p)
			   (not (yes-or-no-p "Buffer has unsaved changes. Continue? ")))
	  (user-error "Restore cancelled"))

	;; Clear buffer and all rich-text overlays
	(erase-buffer)
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (when (overlay-get ov 'rich-text)
		(delete-overlay ov)))

	(insert (nth 0 snapshot))
	(set-buffer-modified-p t)
	(goto-char (point-min))

	(let ((note (nth 1 snapshot)))
	  (message "✅ Restored snapshot: %s%s"
			   timestamp
			   (if (string-empty-p note) "" (format " (%s)" note))))))

;;;###autoload
(defun rich-text-snapshot-delete (timestamp)
  "Delete a snapshot permanently."
  (interactive
   (list (rich-text-snapshot--select-timestamp)))

  (unless (buffer-file-name)
	(user-error "Buffer must be associated with a file"))

  (when (yes-or-no-p (format "Delete snapshot %s? " timestamp))
	(rich-text-db-crud
	 `[:delete :from snapshot
	   :where (and (= id ,(buffer-file-name))
				   (= timestamp ,timestamp))])
	(message "🗑️ Snapshot deleted: %s" timestamp)))

;;;; Helper: Select Timestamp

(defun rich-text-snapshot--select-timestamp (&optional prompt)
  "Interactively select a snapshot timestamp for current file."
  (unless (buffer-file-name)
	(user-error "Buffer must be associated with a file"))

  (let* ((id (buffer-file-name))
		 (snapshots (rich-text-db-crud
					 `[:select [timestamp note]
					   :from snapshot
					   :where (= id ,id)
					   :order-by [(desc timestamp)]])))
	(unless snapshots
	  (user-error "No snapshots found for this file"))

	(let* ((choices (mapcar (lambda (s)
							  (let ((ts (nth 0 s))
									(note (nth 1 s)))
								(if (string-empty-p note)
									ts
								  (format "%s — %s" ts note))))
							snapshots))
		   (choice (completing-read (or prompt "Select snapshot: ") choices nil t)))
	  ;; Extract timestamp (before " — ")
	  (car (split-string choice " — ")))))

;;;; Diff Utilities

(defun rich-text-snapshot--diff-strings (str1 str2 name1 name2)
  "Show diff between STR1 and STR2 using Ediff, then clean buffers and restore layout."
  (require 'ediff)
  (let* ((buf1 (generate-new-buffer (format " *%s*" name1)))
		 (buf2 (generate-new-buffer (format " *%s*" name2)))
		 (win-config (current-window-configuration)))
	(with-current-buffer buf1
	  (insert str1)
	  (text-mode))
	(with-current-buffer buf2
	  (insert str2)
	  (text-mode))

	(let (cleanup)
	  (setq cleanup
			(lambda ()
			  (when (buffer-live-p buf1) (kill-buffer buf1))
			  (when (buffer-live-p buf2) (kill-buffer buf2))
			  (set-window-configuration win-config)
			  (remove-hook 'ediff-quit-hook cleanup)))
	  (add-hook 'ediff-quit-hook cleanup))
	(let ((ediff-window-setup-function 'ediff-setup-windows-plain)
		  (ediff-split-window-function 'split-window-horizontally))
	  (ediff-buffers buf1 buf2))))

;;;###autoload
(defun rich-text-snapshot-diff-full (timestamp1 timestamp2)
  "Compare full content of two snapshots for current file."
  (interactive
   (progn
	 (unless (buffer-file-name)
	   (user-error "Buffer must be associated with a file"))
	 (let ((ts1 (rich-text-snapshot--select-timestamp "First snapshot: "))
		   (ts2 (rich-text-snapshot--select-timestamp "Second snapshot: ")))
	   (list ts1 ts2))))

  (unless (buffer-file-name)
	(user-error "Buffer must be associated with a file"))

  (let* ((id (buffer-file-name))
		 (snap1 (car (rich-text-db-crud
					  `[:select [content] :from snapshot
						:where (and (= id ,id) (= timestamp ,timestamp1))])))
		 (snap2 (car (rich-text-db-crud
					  `[:select [content] :from snapshot
						:where (and (= id ,id) (= timestamp ,timestamp2))]))))

	(unless (and snap1 snap2)
	  (user-error "One or both snapshots not found"))

	(rich-text-snapshot--diff-strings
	 (car snap1) (car snap2)
	 (format "Full @ %s" timestamp1)
	 (format "Full @ %s" timestamp2))))

;;;###autoload
(defun rich-text-snapshot-diff-current (timestamp)
  "Compare current buffer content with a snapshot."
  (interactive
   (progn
	 (unless (buffer-file-name)
	   (user-error "Buffer must be associated with a file"))
	 (list (rich-text-snapshot--select-timestamp "Compare with snapshot: "))))

  (unless (buffer-file-name)
	(user-error "Buffer must be associated with a file"))

  (let* ((id (buffer-file-name))
		 (current-content (buffer-string))
		 (snapshot (car (rich-text-db-crud
						 `[:select [content]
						   :from snapshot
						   :where (and (= id ,id)
									   (= timestamp ,timestamp))]))))

	(unless snapshot
	  (user-error "Snapshot not found"))

	(rich-text-snapshot--diff-strings
	 current-content (car snapshot)
	 "Current Buffer"
	 (format "Snapshot @ %s" timestamp))))

;;;; Tabulated List UI

(defvar-local rich-text-snapshot-list--file-id nil)
(defvar-local rich-text-snapshot--marked-for-compare nil)
(defvar-local rich-text-snapshot-list--filter-term nil
  "Current filter term for snapshot list.")

(defvar rich-text-snapshot-list-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map tabulated-list-mode-map)
	(define-key map (kbd "RET") #'rich-text-snapshot-list-restore-at-point)
	(define-key map "r"         #'rich-text-snapshot-list-restore-at-point)
	(define-key map "d"         #'rich-text-snapshot-list-delete-at-point)
	(define-key map "v"         #'rich-text-snapshot-list-view-at-point)
	(define-key map "c"         #'rich-text-snapshot-list-compare-at-point)
	(define-key map "C"         #'rich-text-snapshot-list-compare-full-at-point)
	;; REMOVED: "F" and "f" for function diff
	(define-key map "g"         #'rich-text-snapshot-list-refresh)
	(define-key map "/"         #'rich-text-snapshot-list-filter)
	(define-key map "L"         #'rich-text-snapshot-list-all)
	(define-key map "q"         #'quit-window)
	map)
  "Keymap for snapshot list mode.")

(define-derived-mode rich-text-snapshot-list-mode tabulated-list-mode "Snapshots"
  "Major mode for browsing document snapshots."
  (setq tabulated-list-format
		[("File"     25 t)
		 ("Time"     20 t)
		 ("Note"     30 t)
		 ("Size"     8 nil :right-align t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Time" t))
  (tabulated-list-init-header))

(defun rich-text-snapshot-list--build-entries (file-id &optional filter-term)
  "Build tabulated list entries, optionally filtered by FILTER-TERM."
  (let ((query (if file-id
				   `[:select [id timestamp note content]
					 :from snapshot
					 :where (= id ,file-id)
					 :order-by [(desc timestamp)]]
				 `[:select [id timestamp note content]
				   :from snapshot
				   :order-by [(desc timestamp)]])))
	(let ((all-rows (rich-text-db-crud query)))
	  (if (not filter-term)
		  (mapcar (lambda (row)
					(let ((id (nth 0 row))
						  (ts (nth 1 row))
						  (note (nth 2 row))
						  (content (nth 3 row)))
					  (list (list id ts)
							(vector (file-name-nondirectory id)
									ts
									(if (string-empty-p note) "-" note)
									(format "%d" (length content))))))
				  all-rows)
		(let ((entries (mapcar (lambda (row)
								 (let ((id (nth 0 row))
									   (ts (nth 1 row))
									   (note (nth 2 row))
									   (content (nth 3 row)))
								   (list (list id ts)
										 (vector (file-name-nondirectory id)
												 ts
												 (if (string-empty-p note) "-" note)
												 (format "%d" (length content))))))
							   all-rows)))
		  (let* ((and-mode (string-match-p "&&" filter-term))
				 (terms (mapcar #'string-trim
								(if and-mode
									(split-string filter-term "&&" t)
								  (split-string filter-term " " t)))))
			(seq-filter
			 (lambda (entry)
			   (let* ((key (car entry))
					  (vec (cadr entry))
					  (id (nth 0 key))
					  (ts (nth 1 key))
					  (note (aref vec 2))
					  (filename (aref vec 0))
					  (search-text (string-join (list filename ts note) " ")))
				 (if and-mode
					 (seq-every-p (lambda (term)
									(string-match-p (regexp-quote term) search-text))
								  terms)
				   (seq-some (lambda (term)
							   (string-match-p (regexp-quote term) search-text))
							 terms))))
			 entries)))))))

;;;###autoload
(defun rich-text-snapshot-list (&optional current-file-only)
  (interactive "P")
  (rich-text-snapshot-ensure-table)
  (let* ((file-id (when (or current-file-only (not (null current-file-only)))
					(buffer-file-name)))
		 (entries (rich-text-snapshot-list--build-entries file-id rich-text-snapshot-list--filter-term)))
	(if (null entries)
		(message "No snapshots found%s%s"
				 (if file-id " for this file" "")
				 (if rich-text-snapshot-list--filter-term
					 (format " (filter: %s)" rich-text-snapshot-list--filter-term)
				   ""))
	  (with-current-buffer (get-buffer-create "*Rich-Text Snapshots*")
		(rich-text-snapshot-list-mode)
		(setq rich-text-snapshot-list--file-id file-id)
		(setq rich-text-snapshot--marked-for-compare nil)
		(setq rich-text-snapshot-list--filter-term rich-text-snapshot-list--filter-term)
		(setq tabulated-list-entries entries)
		(tabulated-list-print t)
		(goto-char (point-min)))
	  (pop-to-buffer "*Rich-Text Snapshots*"))))

;;;###autoload
(defun rich-text-snapshot-list-all ()
  (interactive)
  (rich-text-snapshot-list nil))

;;;###autoload
(defun rich-text-snapshot-list-current ()
  (interactive)
  (unless (buffer-file-name)
	(user-error "Current buffer is not visiting a file"))
  (rich-text-snapshot-list t))

(defun rich-text-snapshot-list-refresh ()
  (interactive)
  (unless (eq major-mode 'rich-text-snapshot-list-mode)
	(user-error "Not in snapshot list buffer"))
  (let ((file-id rich-text-snapshot-list--file-id)
		(filter-term rich-text-snapshot-list--filter-term)
		(origin-id (tabulated-list-get-id)))
	(setq tabulated-list-entries (rich-text-snapshot-list--build-entries file-id filter-term))
	(tabulated-list-print t)
	(when origin-id
	  (goto-char (point-min))
	  (while (and (not (eobp))
				  (not (equal (tabulated-list-get-id) origin-id)))
		(forward-line 1)))))

;;;###autoload
(defun rich-text-snapshot-list-filter (term)
  "Filter snapshot list by TERM.
Space-separated terms = OR match.
'&&'-separated terms = AND match."
  (interactive "sFilter (space=OR, &&=AND): ")
  (unless (eq major-mode 'rich-text-snapshot-list-mode)
	(user-error "Not in snapshot list buffer"))
  (setq rich-text-snapshot-list--filter-term
		(unless (string-empty-p term) (string-trim term)))
  (rich-text-snapshot-list-refresh)
  (message "Filter: %s" (or rich-text-snapshot-list--filter-term "cleared")))

(defun rich-text-snapshot-list-restore-at-point ()
  (interactive)
  (let* ((entry (tabulated-list-get-id)))
	(unless entry (user-error "No snapshot on this line"))
	(let ((file-id (nth 0 entry))
		  (timestamp (nth 1 entry)))
	  (with-current-buffer (find-file-noselect file-id)
		(rich-text-snapshot-restore timestamp))
	  (quit-window))))

(defun rich-text-snapshot-list-delete-at-point ()
  "Delete the snapshot at point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
		 (current-line (line-number-at-pos)))
	(unless entry (user-error "No snapshot on this line"))
	(let ((file-id (nth 0 entry))
		  (timestamp (nth 1 entry)))
	  (when (yes-or-no-p (format "Delete snapshot %s? " timestamp))
		(rich-text-db-crud
		 `[:delete :from snapshot
		   :where (and (= id ,file-id)
					   (= timestamp ,timestamp))])
		(let ((inhibit-read-only t))
		  (setq tabulated-list-entries (rich-text-snapshot-list--build-entries rich-text-snapshot-list--file-id rich-text-snapshot-list--filter-term))
		  (tabulated-list-print t)
		  (goto-char (point-min))
		  (forward-line (1- current-line))
		  (when (eobp)
			(forward-line -1)
			(when (< (line-number-at-pos) 1)
			  (goto-char (point-min)))))))
	(message "🗑️ Snapshot deleted")))

(defun rich-text-snapshot-list-view-at-point ()
  (interactive)
  (let* ((entry (tabulated-list-get-id)))
	(unless entry (user-error "No snapshot on this line"))
	(let* ((file-id (nth 0 entry))
		   (timestamp (nth 1 entry))
		   (row (car (rich-text-db-crud
					  `[:select [note content]
						:from snapshot
						:where (and (= id ,file-id)
									(= timestamp ,timestamp))]))))
	  (unless row
		(user-error "Snapshot not found"))
	  (with-current-buffer (get-buffer-create "*Snapshot Preview*")
		(let ((inhibit-read-only t))
		  (erase-buffer)
		  (insert (format "File: %s\n" file-id))
		  (insert (format "Time: %s\n" timestamp))
		  (insert (format "Note: %s\n" (or (nth 0 row) "-")))
		  (insert (make-string 60 ?─) "\n\n")
		  (insert (nth 1 row))
		  (goto-char (point-min))
		  (special-mode))
		(pop-to-buffer (current-buffer))))))

(defun rich-text-snapshot-list-compare-at-point ()
  (interactive)
  (let* ((entry (tabulated-list-get-id)))
	(unless entry (user-error "No snapshot on this line"))
	(if (not rich-text-snapshot--marked-for-compare)
		(progn
		  (setq-local rich-text-snapshot--marked-for-compare entry)
		  (message "✓ Marked %s (press 'c' on another line to compare)"
				   (nth 1 entry)))
	  (let ((file-id1 (nth 0 rich-text-snapshot--marked-for-compare))
			(ts1 (nth 1 rich-text-snapshot--marked-for-compare))
			(file-id2 (nth 0 entry))
			(ts2 (nth 1 entry)))
		(unless (string= file-id1 file-id2)
		  (user-error "Can only compare snapshots from the same file"))
		(rich-text-snapshot--compare-full ts1 ts2 file-id1)
		(setq-local rich-text-snapshot--marked-for-compare nil)
		(message "Comparing %s vs %s" ts1 ts2)))))

(defun rich-text-snapshot--compare-full (ts1 ts2 file-id)
  (let* ((snap1 (car (rich-text-db-crud
					  `[:select [content] :from snapshot
						:where (and (= id ,file-id) (= timestamp ,ts1))])))
		 (snap2 (car (rich-text-db-crud
					  `[:select [content] :from snapshot
						:where (and (= id ,file-id) (= timestamp ,ts2))]))))
	(unless (and snap1 snap2)
	  (user-error "Snapshot not found"))
	(rich-text-snapshot--diff-strings
	 (car snap1) (car snap2) ts1 ts2)))

(defun rich-text-snapshot-list-compare-full-at-point ()
  (interactive)
  (let* ((entry (tabulated-list-get-id)))
	(unless entry (user-error "No snapshot on this line"))
	(let ((file-id (nth 0 entry))
		  (timestamp1 (nth 1 entry))
		  (timestamp2 (rich-text-snapshot--select-timestamp
					   (format "Compare %s with: " timestamp1))))
	  (with-current-buffer (find-file-noselect file-id)
		(rich-text-snapshot-diff-full timestamp1 timestamp2)))))

;;;###autoload
(defun rich-text-snapshot-insert-full-from-other ()
  "Insert full content of any snapshot into current buffer."
  (interactive)
  (let* ((all (rich-text-db-crud '[:select [id timestamp note] :from snapshot :order-by [(desc timestamp)]])))
	(unless all
	  (user-error "No snapshots available"))

	(let* ((choices (mapcar (lambda (row)
							  (let ((file (nth 0 row))
									(ts (nth 1 row))
									(note (nth 2 row)))
								(cons (format "%s @ %s%s"
											  (file-name-nondirectory file)
											  ts
											  (if (string-empty-p note)
												  ""
												(format " (%s)" note)))
									  ts)))
							all))
		   (selected (completing-read "Insert snapshot content: " choices nil t))
		   (timestamp (cdr (assoc selected choices))))

	  (unless timestamp
		(user-error "Snapshot selection failed"))

	  (let* ((row (car (rich-text-db-crud
						`[:select [content id]
						  :from snapshot
						  :where (= timestamp ,timestamp)])))
			 (content (nth 0 row))
			 (source-file (nth 1 row)))
		(if content
			(progn
			  (insert content)
			  (message "✅ Inserted content from snapshot of %s @ %s"
					   (file-name-nondirectory source-file)
					   timestamp))
		  (user-error "Snapshot content not found"))))))

;;;; Minor Mode

(defvar rich-text-snapshot-mode-map
  (make-sparse-keymap)
  "Keymap for rich-text-snapshot-mode (no bindings).")

;;;###autoload
(define-minor-mode rich-text-snapshot-mode
  "Enable snapshot management for current buffer."
  :init-value nil
  :lighter " Snap"
  :keymap rich-text-snapshot-mode-map
  :group 'rich-text-snapshot
  (when rich-text-snapshot-mode
	(rich-text-snapshot-ensure-table)))

;;;###autoload
(define-globalized-minor-mode global-rich-text-snapshot-mode
  rich-text-snapshot-mode
  (lambda ()
	(when (and (buffer-file-name)
			   (not (minibufferp)))
	  (rich-text-snapshot-mode 1))))

(provide 'rich-text-snapshot)

;;; rich-text-snapshot.el ends here
