;;; rich-text-snapshot.el --- Document snapshot management -*- lexical-binding: t -*-

(require 'rich-text-db)
(require 'tabulated-list)
(require 'seq)
(require 'which-func)

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

(defun rich-text-snapshot--list-functions (content)
  "List all function names in CONTENT (Emacs Lisp)."
  (let ((functions '()))
	(with-temp-buffer
	  (insert content)
	  (emacs-lisp-mode)
	  (goto-char (point-min))
	  (while (re-search-forward "^(defun \\([^ \t\n(]+\\)" nil t)
		(push (match-string-no-properties 1) functions)))
	(nreverse functions)))

(defun rich-text-snapshot--extract-function (content function-name)
  "Extract function FUNCTION-NAME from CONTENT."
  (with-temp-buffer
	(insert content)
	(emacs-lisp-mode)
	(goto-char (point-min))
	(when (re-search-forward
		   (format "^(defun %s\\b" (regexp-quote function-name)) nil t)
	  (beginning-of-line)
	  (let ((start (point)))
		(condition-case nil
			(progn
			  (forward-sexp)
			  (buffer-substring-no-properties start (point)))
		  (error nil))))))
;;;###autoload
(defun rich-text-snapshot-diff-function-current (timestamp function-name)
  "Compare a specific function between current buffer and a snapshot."
  (interactive
   (progn
	 (unless (buffer-file-name)
	   (user-error "Buffer must be associated with a file"))
	 (let* ((ts (rich-text-snapshot--select-timestamp "Snapshot: "))
			(id (buffer-file-name))
			(current-content (buffer-string))
			(snap (car (rich-text-db-crud
						`[:select [content] :from snapshot
						  :where (and (= id ,id) (= timestamp ,ts))])))
			(funcs-current (rich-text-snapshot--list-functions current-content))
			(funcs-snap (when snap (rich-text-snapshot--list-functions (car snap))))
			(all-funcs (delete-dups (append funcs-current funcs-snap)))
			(current-func (ignore-errors (which-function)))
			(default-func (when (and current-func (member current-func all-funcs))
							current-func))
			(func (completing-read
				   (if default-func
					   (format "Function (default %s): " default-func)
					 "Function: ")
				   all-funcs nil nil nil nil default-func)))
	   (list ts func))))

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

	(when (string-empty-p function-name)
	  (user-error "No function specified"))

	(let* ((func-current (rich-text-snapshot--extract-function current-content function-name))
		   (func-snap (rich-text-snapshot--extract-function (car snapshot) function-name)))

	  (if (and func-current func-snap)
		  (rich-text-snapshot--diff-strings
		   func-current func-snap
		   (format "Current: %s" function-name)
		   (format "Snapshot @ %s: %s" timestamp function-name))
		(user-error "Function '%s' not found in current buffer or snapshot"
					function-name)))))

(defun rich-text-snapshot--diff-strings (str1 str2 name1 name2)
  "Show diff between STR1 and STR2 using Ediff, then clean buffers and restore layout."
  (require 'ediff)
  (let* ((buf1 (generate-new-buffer (format " *%s*" name1)))
		 (buf2 (generate-new-buffer (format " *%s*" name2)))

		 (win-config (current-window-configuration)))
	(with-current-buffer buf1
	  (insert str1)
	  (emacs-lisp-mode))
	(with-current-buffer buf2
	  (insert str2)
	  (emacs-lisp-mode))

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
(defun rich-text-snapshot-diff-function (timestamp1 timestamp2 function-name)
  "Compare a specific function between two snapshots."
  (interactive
   (progn
	 (unless (buffer-file-name)
	   (user-error "Buffer must be associated with a file"))
	 (let* ((ts1 (rich-text-snapshot--select-timestamp "First snapshot: "))
			(ts2 (rich-text-snapshot--select-timestamp "Second snapshot: "))
			(id (buffer-file-name))
			(snap1 (car (rich-text-db-crud
						 `[:select [content] :from snapshot
						   :where (and (= id ,id) (= timestamp ,ts1))])))
			(snap2 (car (rich-text-db-crud
						 `[:select [content] :from snapshot
						   :where (and (= id ,id) (= timestamp ,ts2))])))
			(funcs1 (when snap1 (rich-text-snapshot--list-functions (car snap1))))
			(funcs2 (when snap2 (rich-text-snapshot--list-functions (car snap2))))
			(all-funcs (delete-dups (append funcs1 funcs2)))
			(current-func (ignore-errors (which-function)))
			(default-func (when (and current-func (member current-func all-funcs))
							current-func))
			(func (completing-read
				   (if default-func
					   (format "Function (default %s): " default-func)
					 "Function: ")
				   all-funcs nil nil nil nil default-func)))
	   (list ts1 ts2 func))))

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
	  (user-error "Snapshot not found"))

	(when (string-empty-p function-name)
	  (user-error "No function specified"))

	(let* ((content1 (car snap1))
		   (content2 (car snap2))
		   (func1 (rich-text-snapshot--extract-function content1 function-name))
		   (func2 (rich-text-snapshot--extract-function content2 function-name)))

	  (if (and func1 func2)
		  (rich-text-snapshot--diff-strings
		   func1 func2
		   (format "%s @ %s" function-name timestamp1)
		   (format "%s @ %s" function-name timestamp2))
		(user-error "Function '%s' not found in one or both snapshots"
					function-name)))))

;;;###autoload
(defun rich-text-snapshot-show-functions (timestamp)
  "Show all functions in a snapshot."
  (interactive
   (progn
	 (unless (buffer-file-name)
	   (user-error "Buffer must be associated with a file"))
	 (list (rich-text-snapshot--select-timestamp))))

  (unless (buffer-file-name)
	(user-error "Buffer must be associated with a file"))

  (let* ((id (buffer-file-name))
		 (snapshot (car (rich-text-db-crud
						 `[:select [content] :from snapshot
						   :where (and (= id ,id) (= timestamp ,timestamp))]))))

	(unless snapshot
	  (user-error "Snapshot not found"))

	(let ((funcs (rich-text-snapshot--list-functions (car snapshot))))
	  (if funcs
		  (with-current-buffer (get-buffer-create "*Snapshot Functions*")
			(let ((inhibit-read-only t))
			  (erase-buffer)
			  (insert (format "Functions in: %s\n" timestamp))
			  (insert (format "File: %s\n" (file-name-nondirectory id)))
			  (insert (make-string 60 ?─) "\n\n")
			  (dolist (func funcs)
				(insert (format "• %s\n" func)))
			  (insert (format "\nTotal: %d function%s\n"
							  (length funcs)
							  (if (= (length funcs) 1) "" "s")))
			  (goto-char (point-min))
			  (special-mode))
			(pop-to-buffer (current-buffer)))
		(message "No functions found in snapshot")))))

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

(defvar rich-text-snapshot-list-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map tabulated-list-mode-map)
	(define-key map (kbd "RET") #'rich-text-snapshot-list-restore-at-point)
	(define-key map "r"         #'rich-text-snapshot-list-restore-at-point)
	(define-key map "d"         #'rich-text-snapshot-list-delete-at-point)
	(define-key map "v"         #'rich-text-snapshot-list-view-at-point)
	(define-key map "c"         #'rich-text-snapshot-list-compare-at-point)
	(define-key map "C"         #'rich-text-snapshot-list-compare-full-at-point)
	(define-key map "F"         #'rich-text-snapshot-list-compare-function-at-point)
	(define-key map "f"         #'rich-text-snapshot-list-show-functions-at-point)
	(define-key map "g"         #'rich-text-snapshot-list-refresh)
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

(defun rich-text-snapshot-list--build-entries (file-id)
  (let ((query (if file-id
				   `[:select [id timestamp note content]
					 :from snapshot
					 :where (= id ,file-id)
					 :order-by [(desc timestamp)]]
				 `[:select [id timestamp note content]
				   :from snapshot
				   :order-by [(desc timestamp)]])))
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
			(rich-text-db-crud query))))

;;;###autoload
(defun rich-text-snapshot-list (&optional current-file-only)
  (interactive "P")
  (rich-text-snapshot-ensure-table)
  (let* ((file-id (when (or current-file-only (not (null current-file-only)))
					(buffer-file-name)))
		 (entries (rich-text-snapshot-list--build-entries file-id)))
	(if (null entries)
		(message "No snapshots found%s"
				 (if file-id " for this file" ""))
	  (with-current-buffer (get-buffer-create "*Rich-Text Snapshots*")
		(rich-text-snapshot-list-mode)
		(setq rich-text-snapshot-list--file-id file-id)
		(setq rich-text-snapshot--marked-for-compare nil)
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
  (rich-text-snapshot-list t))

(defun rich-text-snapshot-list-refresh ()
  (interactive)
  (unless (eq major-mode 'rich-text-snapshot-list-mode)
	(user-error "Not in snapshot list buffer"))
  (let ((file-id rich-text-snapshot-list--file-id)
		(origin-id (tabulated-list-get-id)))
	(setq tabulated-list-entries (rich-text-snapshot-list--build-entries file-id))
	(tabulated-list-print t)
	(when origin-id
	  (goto-char (point-min))
	  (while (and (not (eobp))
				  (not (equal (tabulated-list-get-id) origin-id)))
		(forward-line 1)))))

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
		;; Refresh and try to keep cursor at same line number
		(let ((inhibit-read-only t))
		  (setq tabulated-list-entries (rich-text-snapshot-list--build-entries rich-text-snapshot-list--file-id))
		  (tabulated-list-print t)
		  ;; Move to same line number if possible
		  (goto-char (point-min))
		  (forward-line (1- current-line))
		  (when (eobp)
			(forward-line -1)  ; if went past end, go to last line
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

(defun rich-text-snapshot-list-compare-function-at-point ()
  (interactive)
  (unless rich-text-snapshot--marked-for-compare
	(user-error "Mark a snapshot first with 'c'"))
  (let* ((entry (tabulated-list-get-id)))
	(unless entry (user-error "No snapshot on this line"))
	(let* ((file-id1 (nth 0 rich-text-snapshot--marked-for-compare))
		   (ts1 (nth 1 rich-text-snapshot--marked-for-compare))
		   (file-id2 (nth 0 entry))
		   (ts2 (nth 1 entry)))
	  (unless (string= file-id1 file-id2)
		(user-error "Can only compare snapshots from the same file"))
	  (let* ((snap1 (car (rich-text-db-crud
						  `[:select [content] :from snapshot
							:where (and (= id ,file-id1) (= timestamp ,ts1))])))
			 (snap2 (car (rich-text-db-crud
						  `[:select [content] :from snapshot
							:where (and (= id ,file-id2) (= timestamp ,ts2))])))
			 (funcs1 (when snap1 (rich-text-snapshot--list-functions (car snap1))))
			 (funcs2 (when snap2 (rich-text-snapshot--list-functions (car snap2))))
			 (all-funcs (delete-dups (append funcs1 funcs2)))
			 (func (completing-read "Function: " all-funcs nil nil)))
		(with-current-buffer (find-file-noselect file-id1)
		  (rich-text-snapshot-diff-function ts1 ts2 func))
		(setq-local rich-text-snapshot--marked-for-compare nil)
		(message "Comparing '%s': %s vs %s" func ts1 ts2)))))

(defun rich-text-snapshot-list-show-functions-at-point ()
  (interactive)
  (let* ((entry (tabulated-list-get-id)))
	(unless entry (user-error "No snapshot on this line"))
	(let ((timestamp (nth 1 entry)))
	  (with-current-buffer (find-file-noselect (nth 0 entry))
		(rich-text-snapshot-show-functions timestamp)))))

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
