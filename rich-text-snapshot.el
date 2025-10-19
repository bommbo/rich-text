;;; rich-text-snapshot.el --- Document snapshot management -*- lexical-binding: t -*-

(require 'rich-text-db)
(require 'tabulated-list)
(require 'seq)

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

(defun rich-text-snapshot--select-timestamp ()
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
		   (choice (completing-read "Select snapshot: " choices nil t)))
	  ;; Extract timestamp (before " — ")
	  (car (split-string choice " — ")))))

;;;; Tabulated List UI

(defvar-local rich-text-snapshot-list--file-id nil
  "File ID for current snapshot list buffer (nil = all files).")

(defvar rich-text-snapshot-list-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map tabulated-list-mode-map)
	(define-key map (kbd "RET") #'rich-text-snapshot-list-restore-at-point)
	(define-key map "r"         #'rich-text-snapshot-list-restore-at-point)
	(define-key map "d"         #'rich-text-snapshot-list-delete-at-point)
	(define-key map "v"         #'rich-text-snapshot-list-view-at-point)
	(define-key map "g"         #'rich-text-snapshot-list-refresh)
	(define-key map "q"         #'quit-window)
	map)
  "Keymap for snapshot list mode.")

(define-derived-mode rich-text-snapshot-list-mode tabulated-list-mode "Snapshots"
  "Major mode for browsing document snapshots.
\\{rich-text-snapshot-list-mode-map}"
  (setq tabulated-list-format
		[("File"     25 t)
		 ("Time"     20 t)
		 ("Note"     30 t)
		 ("Size"     8 nil :right-align t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Time" t))
  (tabulated-list-init-header))

(defun rich-text-snapshot-list--build-entries (file-id)
  "Build tabulated-list entries for snapshots."
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
				(list (list id ts)  ; unique ID: (file-path timestamp)
					  (vector (file-name-nondirectory id)
							  ts
							  (if (string-empty-p note) "-" note)
							  (format "%d" (length content))))))
			(rich-text-db-crud query))))

;;;###autoload
(defun rich-text-snapshot-list (&optional current-file-only)
  "List snapshots in a tabulated buffer.
With prefix arg, show only current file."
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
		(setq tabulated-list-entries entries)
		(tabulated-list-print t)
		(goto-char (point-min)))
	  (pop-to-buffer "*Rich-Text Snapshots*"))))

;;;###autoload
(defun rich-text-snapshot-list-all ()
  "List snapshots for all files."
  (interactive)
  (rich-text-snapshot-list nil))

;;;###autoload
(defun rich-text-snapshot-list-current ()
  "List snapshots for current file only."
  (interactive)
  (rich-text-snapshot-list t))

(defun rich-text-snapshot-list-refresh ()
  "Refresh the snapshot list."
  (interactive)
  (unless (eq major-mode 'rich-text-snapshot-list-mode)
	(user-error "Not in snapshot list buffer"))
  (let ((file-id rich-text-snapshot-list--file-id)
		(origin-id (tabulated-list-get-id)))
	(setq tabulated-list-entries (rich-text-snapshot-list--build-entries file-id))
	(tabulated-list-print t)
	;; Try to restore cursor position
	(when origin-id
	  (goto-char (point-min))
	  (while (and (not (eobp))
				  (not (equal (tabulated-list-get-id) origin-id)))
		(forward-line 1)))))

(defun rich-text-snapshot-list-restore-at-point ()
  "Restore the snapshot at point."
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
  (let* ((entry (tabulated-list-get-id)))
	(unless entry (user-error "No snapshot on this line"))
	(let ((file-id (nth 0 entry))
		  (timestamp (nth 1 entry)))
	  (when (yes-or-no-p (format "Delete snapshot %s? " timestamp))
		(rich-text-db-crud
		 `[:delete :from snapshot
		   :where (and (= id ,file-id)
					   (= timestamp ,timestamp))])
		(rich-text-snapshot-list-refresh)
		(message "🗑️ Snapshot deleted")))))

(defun rich-text-snapshot-list-view-at-point ()
  "Preview snapshot content in a temporary buffer."
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

;;;; Minor Mode (optional)

(defvar rich-text-snapshot-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c s s") #'rich-text-snapshot-save)
	(define-key map (kbd "C-c s r") #'rich-text-snapshot-restore)
	(define-key map (kbd "C-c s d") #'rich-text-snapshot-delete)
	(define-key map (kbd "C-c s l") #'rich-text-snapshot-list-current)
	(define-key map (kbd "C-c s L") #'rich-text-snapshot-list-all)
	map)
  "Keymap for rich-text-snapshot-mode.")

;;;###autoload
(define-minor-mode rich-text-snapshot-mode
  "Enable snapshot management for current buffer."
  :lighter " Snap"
  :keymap rich-text-snapshot-mode-map
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
