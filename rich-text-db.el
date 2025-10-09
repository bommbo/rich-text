(require 'emacsql)
(require 'emacsql-sqlite-builtin)

(defvar rich-text-db-file (expand-file-name "rich-text.db" user-emacs-directory))
(defvar rich-text-db-models '((ov [id beg end props])))
(defvar rich-text-db--conn (make-hash-table :test #'equal)
  "Database connection to rich-text-db.")

(defun rich-text-db--get-conn ()
  "Return the rich-text database connection with key PATH."
  (gethash rich-text-db-file rich-text-db--conn))

(defun rich-text-db--init (db)
  "Initialize database DB with `rich-text-db-models'."
  (emacsql-with-transaction db
	(pcase-dolist (`(,table . ,schema) rich-text-db-models)
	  (emacsql db `[:create-table :if-not-exists ,table ,schema]))))

(defun rich-text-db ()
  "Entrypoint to rich-text sqlite database."
  (unless (and (rich-text-db--get-conn)
			   (emacsql-live-p (rich-text-db--get-conn)))
	(make-directory (file-name-directory rich-text-db-file) t)
	(let ((conn (emacsql-sqlite-builtin rich-text-db-file)))
	  (puthash rich-text-db-file conn rich-text-db--conn)
	  ;; 总是尝试初始化表（如果表已存在则跳过）
	  (rich-text-db--init conn)))
  (rich-text-db--get-conn))

(defun rich-text-db--close (&optional db)
  "Closes the database connection for database DB.
If DB is nil, closes the database connection for current rich-text db."
  (unless db
	(setq db (rich-text-db--get-conn)))
  (when (and db (emacsql-live-p db))
	(emacsql-close db)))

(defun rich-text-db-clear ()
  "Clear all data in rich-text database."
  (interactive)
  (when (file-exists-p rich-text-db-file)
	(dolist (table (mapcar #'car rich-text-db-models))
	  (rich-text-db-crud `[:delete :from ,table]))))

(defun rich-text-db-drop ()
  "Drop the whole rich-text database."
  (interactive)
  (rich-text-db--close)
  (delete-file rich-text-db-file))

(defun rich-text-db-crud (sql &rest args)
  "Return SQL query on rich-text database with ARGS.
SQL can be either the emacsql vector representation, or a string."
  (if (stringp sql)
	  (emacsql (rich-text-db) (apply #'format sql args))
	(apply #'emacsql (rich-text-db) sql args)))

(defun rich-text-db-query (table fields &optional conds)
  (if conds
	  (rich-text-db-crud `[:select ,fields :from ,table :where ,conds])
	(rich-text-db-crud `[:select ,fields :from ,table])))

(defun rich-text-db-query-count (table &optional conds)
  (if conds
	  (caar (rich-text-db-crud `[:select (funcall count *) :from ,table :where ,conds]))
	(caar (rich-text-db-crud `[:select (funcall count *) :from ,table]))))

(defun rich-text-db-distinct-query (table fields &optional conds)
  (if conds
	  (rich-text-db-crud `[:select :distinct ,fields :from ,table :where ,conds])
	(rich-text-db-crud `[:select :distinct ,fields :from ,table])))

(defun rich-text-db-insert (table values)
  (rich-text-db-crud `[:insert :into ,table :values ,values]))

(defun rich-text-db-delete (table conds)
  (rich-text-db-crud `[:delete :from ,table :where ,conds]))

(defun rich-text-db-update (table conds)
  (rich-text-db-crud `[:delete :from ,table :where ,conds]))

;;;; 新增：多文件管理功能

;;;###autoload
(defun rich-text-db-list-files ()
  "List all files that have rich-text overlays stored in database."
  (interactive)
  (let ((files (mapcar #'car (rich-text-db-distinct-query 'ov [id]))))
	(if files
		(progn
		  (with-current-buffer (get-buffer-create "*Rich-Text Files*")
			(erase-buffer)
			(insert "Files with rich-text overlays:\n\n")
			(dolist (file files)
			  (let ((count (rich-text-db-query-count 'ov `(= id ,file))))
				(insert (format "%s (%d overlay%s)\n"
								file count (if (= count 1) "" "s")))))
			(goto-char (point-min))
			(special-mode))
		  (display-buffer "*Rich-Text Files*")
		  (message "Found %d file%s with rich-text overlays"
				   (length files)
				   (if (= (length files) 1) "" "s")))
	  (message "No files with rich-text overlays in database"))))

;;;###autoload
(defun rich-text-db-clear-file (file-id)
  "Clear all rich-text overlays for a specific file from database.
FILE-ID is the file path."
  (interactive
   (list (completing-read "Clear file: "
						  (mapcar #'car (rich-text-db-distinct-query 'ov [id]))
						  nil t)))
  (let ((count (rich-text-db-query-count 'ov `(= id ,file-id))))
	(rich-text-db-delete 'ov `(= id ,file-id))
	(message "Cleared %d overlay%s for %s"
			 count
			 (if (= count 1) "" "s")
			 file-id)))

;;;###autoload
(defun rich-text-db-export-file (file-id)
  "Export rich-text overlays for a specific file to a readable format.
FILE-ID is the file path."
  (interactive
   (list (completing-read "Export file: "
						  (mapcar #'car (rich-text-db-distinct-query 'ov [id]))
						  nil t)))
  (let ((overlays (rich-text-db-query 'ov [beg end props] `(= id ,file-id))))
	(if overlays
		(with-current-buffer (get-buffer-create "*Rich-Text Export*")
		  (erase-buffer)
		  (insert (format "Rich-text overlays for: %s\n\n" file-id))
		  (dolist (ov overlays)
			(insert (format "Position: %d-%d\n" (nth 0 ov) (nth 1 ov)))
			(insert (format "Properties: %S\n\n" (nth 2 ov))))
		  (goto-char (point-min))
		  (display-buffer (current-buffer))
		  (message "Exported %d overlay%s"
				   (length overlays)
				   (if (= (length overlays) 1) "" "s")))
	  (message "No overlays found for %s" file-id))))

;;;###autoload
(defun rich-text-db-statistics ()
  "Show statistics about rich-text database."
  (interactive)
  (let* ((total-overlays (rich-text-db-query-count 'ov))
		 (files (mapcar #'car (rich-text-db-distinct-query 'ov [id])))
		 (file-count (length files)))
	(with-current-buffer (get-buffer-create "*Rich-Text Statistics*")
	  (erase-buffer)
	  (insert "Rich-Text Database Statistics\n")
	  (insert "==============================\n\n")
	  (insert (format "Total overlays: %d\n" total-overlays))
	  (insert (format "Total files: %d\n\n" file-count))
	  (when files
		(insert "Overlays per file:\n")
		(dolist (file files)
		  (let ((count (rich-text-db-query-count 'ov `(= id ,file))))
			(insert (format "  %s: %d\n"
							(file-name-nondirectory file)
							count)))))
	  (goto-char (point-min))
	  (special-mode))
	(display-buffer "*Rich-Text Statistics*")))

;;;###autoload
(defun rich-text-db-vacuum ()
  "Vacuum the database to reclaim space and optimize performance."
  (interactive)
  (rich-text-db-crud "VACUUM")
  (message "Database vacuumed successfully"))

;;;###autoload
(defun rich-text-db-check-consistency ()
  "Check if current buffer's overlays match database records."
  (interactive)
  (if (not (derived-mode-p 'prog-mode 'text-mode 'org-mode))
	  (message "Not in a supported major mode")
	(let* ((id (buffer-file-name))
		   (buffer-overlays (seq-filter
							 (lambda (ov) (overlay-get ov 'rich-text))
							 (overlays-in (point-min) (point-max))))
		   (buffer-count (length buffer-overlays))
		   (db-count (if id (rich-text-db-query-count 'ov `(= id ,id)) 0)))
	  (if id
		  (if (= buffer-count db-count)
			  (message "✓ Consistent: %d overlay%s in buffer and database"
					   buffer-count
					   (if (= buffer-count 1) "" "s"))
			(message "✗ Inconsistent: %d in buffer, %d in database. Consider saving or refreshing."
					 buffer-count db-count))
		(message "Buffer has no associated file")))))

(provide 'rich-text-db)
