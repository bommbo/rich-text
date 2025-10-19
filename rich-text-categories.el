;;; rich-text-categories.el --- Category-based text management in rich-text -*- lexical-binding: t -*-

;; Integrate text-categories functionality into rich-text system
;; Use overlay + database instead of text-property + file

(require 'rich-text)
(require 'rich-text-db)
(require 'ov)

;;;; Variables

(defvar-local rich-text-category-current "0"
  "Current active text category.")

(defvar rich-text-category-colors
  '(("0" . nil)
	("1" . (:background "#FFEB3B" :foreground "#000000" :weight bold))
	("2" . (:background "#A8E6CF" :foreground "#000000" :weight bold))
	("3" . (:background "#ADD8E6" :foreground "#000000" :weight bold))
	("4" . (:background "#FFAAA5" :foreground "#000000" :weight bold))
	("5" . (:background "#E1BEE7" :foreground "#000000" :weight bold))
	("6" . (:background "#FFE0B2" :foreground "#000000" :weight bold))
	("7" . (:background "#B2DFDB" :foreground "#000000" :weight bold))
	("8" . (:background "#F8BBD0" :foreground "#000000" :weight bold))
	("9" . (:background "#DCEDC8" :foreground "#000000" :weight bold)))
  "Category to color mapping.")

(defvar rich-text-category-default-cycle '("0" "1" "2")
  "Default category cycle list.")

;;;; Core functions

(defun rich-text-category-register-style (category)
  "Register CATEGORY as a rich-text style."
  (let* ((style-name (intern (format "category-%s" category)))
		 (color (cdr (assoc category rich-text-category-colors))))
	(when color
	  (puthash style-name
			   (list :key nil
					 :props-fn `(lambda () '(face ,color)))
			   rich-text-style-registry))))

(defun rich-text-category-ensure-styles ()
  "Ensure all categories are registered as rich-text styles."
  (dolist (entry rich-text-category-colors)
	(let ((category (car entry)))
	  (unless (string= category "0")
		(rich-text-category-register-style category)))))

;;;; Category management commands

;;;###autoload
(defun rich-text-category-change (category)
  "Change current category to CATEGORY."
  (interactive
   (list (completing-read
		  (format "Change category (current: %s): " rich-text-category-current)
		  (mapcar #'car rich-text-category-colors)
		  nil t)))
  (setq rich-text-category-current category)
  (force-mode-line-update)
  (message "Category changed to: %s" category))

;;;###autoload
(defun rich-text-category-get-all ()
  "Get all categories used in current buffer."
  (let ((categories '()))
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (when-let* ((rt (overlay-get ov 'rich-text))
				  (name (symbol-name rt)))
		(when (string-prefix-p "category-" name)
		  (let ((cat (substring name 9)))
			(unless (member cat categories)
			  (push cat categories))))))
	(sort categories #'string<)))

;;;###autoload
(defun rich-text-category-report ()
  "Report current category and all used categories in buffer."
  (interactive)
  (let* ((all-cats (rich-text-category-get-all))
		 (msg (format "Current category: %s | Used categories: %s"
					  rich-text-category-current
					  (if all-cats
						  (mapconcat #'identity all-cats ", ")
						"none"))))
	;; Display in minibuffer and echo area
	(message "%s" msg)
	;; Also copy to kill ring for easy reference
	(kill-new msg)
	(sit-for 2)
	;; Return the message so it can be used programmatically
	msg))

;;;###autoload
(defun rich-text-category-report-detailed ()
  "Show detailed category report in a dedicated buffer."
  (interactive)
  (let* ((all-cats (rich-text-category-get-all))
		 (hidden-cats (mapcar #'car rich-text-category-hidden-overlays))
		 (cat-counts '()))

	;; Count characters in each category
	(dolist (cat all-cats)
	  (let ((style-name (intern (format "category-%s" cat)))
			(count 0))
		(dolist (ov (overlays-in (point-min) (point-max)))
		  (when (eq (overlay-get ov 'rich-text) style-name)
			(setq count (+ count (- (overlay-end ov) (overlay-start ov))))))
		(push (cons cat count) cat-counts)))  ; ← 修复：在 let 内部

	;; Create report buffer
	(with-current-buffer (get-buffer-create "*Category Report*")
	  (let ((inhibit-read-only t))
		(erase-buffer)
		(insert "Category Report\n")
		(insert "===============\n\n")
		(insert (format "Current category: %s\n\n" rich-text-category-current))

		(if cat-counts
			(progn
			  (insert "Used categories:\n")
			  (dolist (entry (sort cat-counts (lambda (a b) (string< (car a) (car b)))))
				(insert (format "  Category %s: %d character%s"
								(car entry)
								(cdr entry)
								(if (= (cdr entry) 1) "" "s")))
				(when (member (car entry) hidden-cats)
				  (insert " [HIDDEN]"))
				(insert "\n")))
		  (insert "No categories used in buffer.\n"))

		(when hidden-cats
		  (insert (format "\nHidden categories: %s\n"
						  (mapconcat #'identity hidden-cats ", "))))

		(goto-char (point-min))
		(special-mode)))

	(pop-to-buffer "*Category Report*")))

;;;; Auto-apply category on input

(defun rich-text-category-after-change (beg end len)
  "Auto-apply current category after text change."
  (when (and (not undo-in-progress)
			 (> (- end beg) 0)
			 (not (string= rich-text-category-current "0")))
	(let* ((category rich-text-category-current)
		   (style-name (intern (format "category-%s" category)))
		   (props (rich-text-get-props style-name)))
	  (when (and props (< beg end))
		(let ((ov (ov beg end props)))
		  (ov-set ov 'rich-text style-name)
		  (ov-set ov 'evaporate t)
		  (rich-text-store-ov style-name (list beg end (current-buffer) props)))))))

;;;; Delete category

;;;###autoload
(defun rich-text-category-delete (category)
  "Delete all text belonging to CATEGORY."
  (interactive
   (list (completing-read "Delete category: "
						  (rich-text-category-get-all)
						  nil t)))
  (let ((style-name (intern (format "category-%s" category)))
		(count 0)
		(id (rich-text-buffer-or-file-id)))

	;; Step 1: Collect all regions to delete
	(let ((regions '()))
	  (dolist (ov (overlays-in (point-min) (point-max)))
		(when (eq (overlay-get ov 'rich-text) style-name)
		  (push (cons (overlay-start ov) (overlay-end ov)) regions)
		  (delete-overlay ov)))

	  ;; Step 2: Delete from back to front to avoid position shifts
	  (setq regions (sort regions (lambda (a b) (> (car a) (car b)))))
	  (dolist (region regions)
		(let ((start (car region))
			  (end (cdr region)))
		  (delete-region start end)
		  (setq count (+ count (- end start))))))

	;; Step 3: Delete from database
	(when id
	  (rich-text-db-crud
	   `[:delete :from ov
		 :where (and (= id ,id)
					 (= props ,(prin1-to-string style-name)))]))

	(message "Deleted %d character%s in category %s"
			 count
			 (if (= count 1) "" "s")
			 category)))

;;;; Line-level category cycling

;;;###autoload
(defun rich-text-category-cycle-line ()
  "Cycle through categories on current line."
  (interactive)
  (save-excursion
	(let* ((line-start (line-beginning-position))
		   (line-end (line-end-position))
		   (overlays (overlays-in line-start line-end))
		   (categories '())
		   new-category)

	  ;; Collect all categories on current line
	  (dolist (ov overlays)
		(when-let* ((rt (overlay-get ov 'rich-text))
					(name (symbol-name rt)))
		  (when (string-prefix-p "category-" name)
			(let ((cat (substring name 9)))
			  (unless (member cat categories)
				(push cat categories))))))

	  ;; Determine new category
	  (setq new-category
			(cond
			 ;; Multiple categories: unify to first default
			 ((> (length categories) 1)
			  (car rich-text-category-default-cycle))
			 ;; Single category: switch to next
			 ((= (length categories) 1)
			  (or (cadr (member current rich-text-category-default-cycle))
				  (car rich-text-category-default-cycle))))
			 ;; No category: use first default
			 (t (car rich-text-category-default-cycle))))

	  ;; Remove all category styles on this line
	  (dolist (ov overlays)
		(when-let* ((rt (overlay-get ov 'rich-text))
					(name (symbol-name rt)))
		  (when (string-prefix-p "category-" name)
			(delete-overlay ov))))

	  ;; Apply new category
	  (unless (string= new-category "0")
		(let* ((style-name (intern (format "category-%s" new-category)))
			   (props (rich-text-get-props style-name)))
		  (when (and props (< line-start line-end))  ; ← 范围检查
			(let ((ov (ov line-start line-end props)))  ; ← 使用 ov
			  (ov-set ov 'rich-text style-name)
			  (ov-set ov 'evaporate t)
			  (rich-text-store-ov style-name
								 (list line-start line-end (current-buffer) props)))))))

	  (message "Line category changed to: %s" new-category))

;;;; Region category change

;;;###autoload
(defun rich-text-category-change-region (category)
  "Change category of selected region to CATEGORY."
  (interactive
   (list (completing-read "Set region category to: "
						  (mapcar #'car rich-text-category-colors)
						  nil t)))
  (when (use-region-p)
	(let* ((beg (region-beginning))
		   (end (region-end))
		   (style-name (intern (format "category-%s" category)))
		   (id (rich-text-buffer-or-file-id)))

	  ;; Remove all category style overlays in region
	  (dolist (ov (overlays-in beg end))
		(when-let* ((rt (overlay-get ov 'rich-text))
					(name (symbol-name rt)))
		  (when (string-prefix-p "category-" name)
			(let ((ov-beg (overlay-start ov))
				  (ov-end (overlay-end ov)))
			  (delete-overlay ov)
			  (when id
				(rich-text-delete-ov-from-db id ov-beg ov-end rt))))))

	  ;; Apply new category (if not "0")
	  (unless (string= category "0")
		(let ((props (rich-text-get-props style-name)))
		  (when (and props (< beg end))
			(let ((ov (ov beg end props)))
			  (ov-set ov 'rich-text style-name)
			  (ov-set ov 'evaporate t)
			  (rich-text-store-ov style-name
								 (list beg end (current-buffer) props))))))

	  (deactivate-mark)
	  (message "Region category set to: %s" category))))

;;;; Persistence

(defvar-local rich-text-category-hidden-overlays '()
  "Alist of hidden category overlays: ((category . (ov-data...)) ...).")

(defvar-local rich-text-category-stashed-text '()
  "Alist of stashed category text: ((category timestamp . text-data) ...).")

(defun rich-text-category-ensure-meta-table ()
  "Ensure meta table exists in database."
  (condition-case err
	  (rich-text-db-crud
	   [:create-table :if-not-exists meta [id key value]])
	(error
	 (message "Warning: Could not create meta table: %S" err))))

(defun rich-text-category-ensure-stash-table ()
  "Ensure stash table exists in database."
  (condition-case err
	  (rich-text-db-crud
	   [:create-table :if-not-exists stash [id category timestamp position text]])
	(error
	 (message "Warning: Could not create stash table: %S" err))))

(defun rich-text-category-save-current ()
  "Save current category setting to database."
  (when-let ((id (rich-text-buffer-or-file-id)))
	(rich-text-category-ensure-meta-table)
	(condition-case err
		(progn
		  (ignore-errors
			(rich-text-db-crud
			 `[:delete :from meta
			   :where (and (= id ,id) (= key "current-category"))]))
		  (rich-text-db-insert 'meta
							  `([,id "current-category" ,rich-text-category-current])))
	  (error
	   (message "Warning: Could not save current category: %S" err)))))

(defun rich-text-category-restore-current ()
  "Restore current category setting from database."
  (when-let ((id (rich-text-buffer-or-file-id)))
	(condition-case err
		(progn
		  (rich-text-category-ensure-meta-table)
		  (when-let ((result (car (rich-text-db-crud
								  `[:select value :from meta
									:where (and (= id ,id)
												(= key "current-category"))]))))
			(setq rich-text-category-current (car result))
			(force-mode-line-update)
			(message "Restored category: %s" rich-text-category-current)))
	  (error
	   (message "Warning: Could not restore category: %S" err)))))

;;;; Stash/Unstash categories (backup and restore)

;;;###autoload
(defun rich-text-category-stash (category)
  "Stash (backup) all text in CATEGORY to database, then delete from buffer.
The text can be restored later with unstash, even after closing the file."
  (interactive
   (list (completing-read "Stash category: "
						  (rich-text-category-get-all)
						  nil t)))
  (let ((style-name (intern (format "category-%s" category)))
		(id (rich-text-buffer-or-file-id))
		(timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
		(stashed-items '())
		(count 0))

	(unless id
	  (user-error "Buffer has no associated file"))

	;; Ensure stash table exists
	(rich-text-category-ensure-stash-table)

	;; Collect all text in this category
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (when (eq (overlay-get ov 'rich-text) style-name)
		(let* ((start (overlay-start ov))
			   (end (overlay-end ov))
			   (text (buffer-substring-no-properties start end)))
		  (push (list start end text) stashed-items)
		  (setq count (+ count (length text))))))

	(when (null stashed-items)
	  (user-error "No text found in category %s" category))

	;; Save to database with error handling
	(condition-case err
		(dolist (item (reverse stashed-items))
		  (let ((position (nth 0 item))
				(text (nth 2 item)))
			(rich-text-db-insert 'stash
								`([,id ,category ,timestamp ,position ,text]))))
	  (error
	   (user-error "Failed to save stash: %S" err)))

	;; Now delete from buffer
	(rich-text-category-delete category)

	(message "Stashed %d character%s from category %s (timestamp: %s)"
			 count
			 (if (= count 1) "" "s")
			 category
			 timestamp)))

;;;###autoload
(defun rich-text-category-unstash (category &optional timestamp)
  "Restore stashed text for CATEGORY from database.
If TIMESTAMP is nil, restores the most recent stash."
  (interactive
   (let* ((id (rich-text-buffer-or-file-id))
		  (stashes (when id
					(rich-text-db-crud
					 `[:select [category timestamp]
					   :from stash
					   :where (= id ,id)
					   :order-by [(desc timestamp)]]))))
	 (unless stashes
	   (user-error "No stashed categories found for this file"))
	 (let* ((choices (mapcar (lambda (s)
							  (format "%s (%s)" (nth 0 s) (nth 1 s)))
							stashes))
			(choice (completing-read "Unstash: " choices nil t))
			(parts (split-string choice " ("))
			(cat (car parts))
			(ts (string-trim (cadr parts) nil ")")))
	   (list cat ts))))

  (let* ((id (rich-text-buffer-or-file-id))
		 (stashed-items (rich-text-db-crud
						`[:select [position text]
						  :from stash
						  :where (and (= id ,id)
									  (= category ,category)
									  (= timestamp ,timestamp))
						  :order-by [(asc position)]]))
		 (style-name (intern (format "category-%s" category)))
		 (props (rich-text-get-props style-name))
		 (count 0))

	(unless stashed-items
	  (user-error "No stashed text found for category %s" category))

	;; Insert text back into buffer
	(save-excursion
	  (dolist (item stashed-items)
		(let ((position (nth 0 item))
			  (text (nth 1 item)))
		  ;; Find safe insertion point (don't exceed buffer bounds)
		  (goto-char (min position (point-max)))
		  (insert text)
		  (setq count (+ count (length text)))

		  ;; Apply category style
		  (when (and props (< (- (point) (length text)) (point)))
			(let ((start (- (point) (length text)))
				  (end (point)))
			  (let ((ov (ov start end props)))  ; ← 使用 ov
				(ov-set ov 'rich-text style-name)
				(ov-set ov 'evaporate t)
				(rich-text-store-ov style-name
								   (list start end (current-buffer) props))))))))

	;; Remove from stash database
	(rich-text-db-crud
	 `[:delete :from stash
	   :where (and (= id ,id)
				   (= category ,category)
				   (= timestamp ,timestamp))])

	(message "Restored %d character%s from category %s stash"
			 count
			 (if (= count 1) "" "s")
			 category)))

;;;###autoload
(defun rich-text-category-list-stashes ()
  "List all stashed categories for current file."
  (interactive)
  (let* ((id (rich-text-buffer-or-file-id))
		 (stashes (when id
				   (rich-text-db-crud
					`[:select [category timestamp (funcall count text)]
					  :from stash
					  :where (= id ,id)
					  :group-by [category timestamp]
					  :order-by [(desc timestamp)]]))))

	(if stashes
		(with-current-buffer (get-buffer-create "*Category Stashes*")
		  (let ((inhibit-read-only t))
			(erase-buffer)
			(insert "Stashed Categories\n")
			(insert "==================\n\n")
			(insert (format "File: %s\n\n" id))

			(dolist (stash stashes)
			  (let ((category (nth 0 stash))
					(timestamp (nth 1 stash))
					(items (nth 2 stash)))
				(insert (format "Category %s - %s (%d item%s)\n"
								category
								timestamp
								items
								(if (= items 1) "" "s")))))

			(insert "\nPress 'q' to close, 'u' to unstash")
			(goto-char (point-min))
			(special-mode)
			(local-set-key (kbd "u") #'rich-text-category-unstash))
		  (pop-to-buffer "*Category Stashes*"))
	  (message "No stashed categories for this file"))))

;;;###autoload
(defun rich-text-category-clear-stashes (category)
  "Permanently delete all stashes for CATEGORY in current file."
  (interactive
   (list (completing-read "Clear stashes for category: "
						  (let ((id (rich-text-buffer-or-file-id)))
							(when id
							  (mapcar #'car
									  (rich-text-db-crud
									   `[:select :distinct [category]
										 :from stash
										 :where (= id ,id)]))))
						  nil t)))
  (let ((id (rich-text-buffer-or-file-id)))
	(when (yes-or-no-p (format "Permanently delete all stashes for category %s? " category))
	  (rich-text-db-crud
	   `[:delete :from stash
		 :where (and (= id ,id) (= category ,category))])
	  (message "Cleared all stashes for category %s" category))))

;;;###autoload
(defun rich-text-category-hide (category)
  "Temporarily hide all text in CATEGORY (can be restored later)."
  (interactive
   (list (completing-read "Hide category: "
						  (rich-text-category-get-all)
						  nil t)))
  (let ((style-name (intern (format "category-%s" category)))
		(hidden-data '())
		(count 0))

	;; Check if already hidden
	(when (assoc category rich-text-category-hidden-overlays)
	  (user-error "Category %s is already hidden" category))

	;; Collect overlay data and make invisible
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (when (eq (overlay-get ov 'rich-text) style-name)
		(let ((start (overlay-start ov))
			  (end (overlay-end ov))
			  (text (buffer-substring-no-properties
					 (overlay-start ov)
					 (overlay-end ov))))
		  (push (list start end text) hidden-data)
		  (setq count (+ count (- end start)))
		  ;; Make text invisible
		  (overlay-put ov 'invisible t)
		  (overlay-put ov 'hidden-category t))))

	;; Store hidden data
	(push (cons category (nreverse hidden-data))
		  rich-text-category-hidden-overlays)

	(message "Hidden %d character%s in category %s (use unhide to restore)"
			 count
			 (if (= count 1) "" "s")
			 category)))

;;;###autoload
(defun rich-text-category-unhide (category)
  "Restore previously hidden CATEGORY text."
  (interactive
   (list (completing-read "Unhide category: "
						  (mapcar #'car rich-text-category-hidden-overlays)
						  nil t)))
  (let* ((hidden-entry (assoc category rich-text-category-hidden-overlays))
		 (style-name (intern (format "category-%s" category)))
		 (count 0))

	(unless hidden-entry
	  (user-error "Category %s is not hidden" category))

	;; Make overlays visible again
	(dolist (ov (overlays-in (point-min) (point-max)))
	  (when (and (eq (overlay-get ov 'rich-text) style-name)
				 (overlay-get ov 'hidden-category))
		(overlay-put ov 'invisible nil)
		(overlay-put ov 'hidden-category nil)
		(setq count (+ count (- (overlay-end ov) (overlay-start ov))))))

	;; Remove from hidden list
	(setq rich-text-category-hidden-overlays
		  (assoc-delete-all category rich-text-category-hidden-overlays))

	(message "Restored %d character%s in category %s"
			 count
			 (if (= count 1) "" "s")
			 category)))

;;;###autoload
(defun rich-text-category-toggle-hide (category)
  "Toggle hide/unhide for CATEGORY."
  (interactive
   (list (completing-read "Toggle hide category: "
						  (append (rich-text-category-get-all)
								  (mapcar #'car rich-text-category-hidden-overlays))
						  nil t)))
  (if (assoc category rich-text-category-hidden-overlays)
	  (rich-text-category-unhide category)
	(rich-text-category-hide category)))

;;;###autoload
(defun rich-text-category-list-hidden ()
  "List all currently hidden categories."
  (interactive)
  (if rich-text-category-hidden-overlays
	  (message "Hidden categories: %s"
			   (mapconcat #'car rich-text-category-hidden-overlays ", "))
	(message "No hidden categories")))

;;;; Minor mode

;;;###autoload
(define-minor-mode rich-text-category-mode
  "Enable category-based text management in rich-text."
  :lighter " RTC"
  :global nil
  (if rich-text-category-mode
	  (progn
		(unless rich-text-mode
		  (rich-text-mode 1))

		;; Ensure all tables exist
		(rich-text-category-ensure-meta-table)
		(rich-text-category-ensure-stash-table)

		;; Register styles and restore state
		(rich-text-category-ensure-styles)
		(rich-text-category-restore-current)

		(add-hook 'after-change-functions
				  #'rich-text-category-after-change nil t)
		(add-hook 'after-save-hook
				  #'rich-text-category-save-current nil t)

		(unless (member '(:eval (format " [Cat:%s]" rich-text-category-current))
					   mode-line-format)
		  (setq mode-line-format
				(append mode-line-format
						'((:eval (format " [Cat:%s]" rich-text-category-current))))))

		(message "Rich-text category mode enabled (current: %s)"
				 rich-text-category-current))

	(remove-hook 'after-change-functions
				 #'rich-text-category-after-change t)
	(remove-hook 'after-save-hook
				 #'rich-text-category-save-current t)
	(message "Rich-text category mode disabled")))

;;;; Keymap

(defvar rich-text-category-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c c c") #'rich-text-category-change)
	(define-key map (kbd "C-c c r") #'rich-text-category-change-region)
	(define-key map (kbd "C-c c d") #'rich-text-category-delete)
	(define-key map (kbd "C-c c l") #'rich-text-category-cycle-line)
	(define-key map (kbd "C-c c i") #'rich-text-category-report)
	(define-key map (kbd "C-c c I") #'rich-text-category-report-detailed)
	(define-key map (kbd "C-c c h") #'rich-text-category-toggle-hide)
	(define-key map (kbd "C-c c H") #'rich-text-category-list-hidden)
	(define-key map (kbd "C-c c s") #'rich-text-category-stash)
	(define-key map (kbd "C-c c u") #'rich-text-category-unstash)
	(define-key map (kbd "C-c c S") #'rich-text-category-list-stashes)
	map)
  "Keymap for rich-text-category-mode.")

;;;###autoload
(define-globalized-minor-mode global-rich-text-category-mode
  rich-text-category-mode
  (lambda ()
	(when (and (buffer-file-name)
			   (not (minibufferp)))
	  (rich-text-category-mode 1)))
  :group 'rich-text)

(provide 'rich-text-categories)

;;; rich-text-categories.el ends here
