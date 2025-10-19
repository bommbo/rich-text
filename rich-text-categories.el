;;; rich-text-categories.el --- Category-based text management in rich-text -*- lexical-binding: t -*-

;; Integrate text-categories functionality into rich-text system
;; Use overlay + database instead of text-property + file

(require 'rich-text)
(require 'rich-text-db)

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
  (let ((all-cats (rich-text-category-get-all)))
	(message "Current category: %s | Used categories: %s"
			 rich-text-category-current
			 (if all-cats
				 (mapconcat #'identity all-cats ", ")
			   "none"))))

;;;; Auto-apply category on input

(defun rich-text-category-after-change (beg end len)
  "Auto-apply current category after text change."
  (when (and (not undo-in-progress)
			 (> (- end beg) 0)
			 (not (string= rich-text-category-current "0")))
	(let* ((category rich-text-category-current)
		   (style-name (intern (format "category-%s" category)))
		   (props (rich-text-get-props style-name)))
	  (when props
		(let ((ov (make-overlay beg end)))
		  (ov-set ov props)
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
			  (let ((current (car categories)))
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
		  (when props
			(let ((ov (make-overlay line-start line-end)))
			  (ov-set ov props)
			  (ov-set ov 'rich-text style-name)
			  (ov-set ov 'evaporate t)
			  (rich-text-store-ov style-name
								 (list line-start line-end (current-buffer) props))))))

	  (message "Line category changed to: %s" new-category))))

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
			(delete-overlay ov)
			(when id
			  (rich-text-delete-ov-from-db id
										  (overlay-start ov)
										  (overlay-end ov)
										  rt)))))

	  ;; Apply new category
	  (unless (string= category "0")
		(let ((props (rich-text-get-props style-name)))
		  (when props
			(let ((ov (make-overlay beg end)))
			  (ov-set ov props)
			  (ov-set ov 'rich-text style-name)
			  (ov-set ov 'evaporate t)
			  (rich-text-store-ov style-name
								 (list beg end (current-buffer) props))))))

	  (deactivate-mark)
	  (message "Region category set to: %s" category))))

;;;; Persistence

(defun rich-text-category-ensure-meta-table ()
  "Ensure meta table exists in database."
  (condition-case err
	  (rich-text-db-crud
	   `[:create-table :if-not-exists meta
		 [(id :not-null) (key :not-null) (value)]])
	(error
	 (message "Warning: Could not create meta table: %S" err))))

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

		(rich-text-category-ensure-meta-table)
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
