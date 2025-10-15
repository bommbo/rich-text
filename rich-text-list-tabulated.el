;;; rich-text-list-tabulated.el --- Tabulated list UI for rich-text -*- lexical-binding: t -*-

(require 'rich-text-db)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface rich-text-tabulated-match-face
  '((t :inherit highlight :weight bold))
  "Face for matched terms in tabulated list content."
  :group 'rich-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom rich-text-tabulated-max-content-lines 3
  "Maximum number of lines to display for each overlay's content."
  :type '(choice (const :tag "Unlimited" nil)
				 (integer :tag "Max lines" :value 3))
  :group 'rich-text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query Parsing & Matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rich-text--parse-filter-query (query)
  (mapcar (lambda (clause)
			(split-string (string-trim clause) "[ \t]+" t))
		  (split-string query " && " t)))

(defun rich-text--match-field-p (field value entry-data)
  (let ((actual (cl-case (intern-soft field)
				  (file    (plist-get entry-data :file))
				  (pos     (plist-get entry-data :pos))
				  (style   (plist-get entry-data :style))
				  (props   (plist-get entry-data :props))
				  (otherwise nil))))
	(and actual
		 (string-match-p (regexp-quote value) actual))))

(defun rich-text--match-anywhere-p (term entry-data)
  (let ((text (mapconcat #'identity
						 (list (plist-get entry-data :file)
							   (plist-get entry-data :pos)
							   (plist-get entry-data :style)
							   (plist-get entry-data :props))
						 " ")))
	(string-match-p (regexp-quote term) text)))

(defun rich-text--match-entry-p (entry-data and-clauses)
  (cl-every
   (lambda (or-terms)
	 (cl-some
	  (lambda (term)
		(if (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" term)
			(rich-text--match-field-p (match-string 1 term) (match-string 2 term) entry-data)
		  (rich-text--match-anywhere-p term entry-data)))
	  or-terms))
   and-clauses))

(defun rich-text--highlight-matches (text terms)
  (let ((plain-terms (cl-remove-if (lambda (x) (string-match-p ":" x)) terms))
		(result text))
	(dolist (term plain-terms)
	  (let ((start 0))
		(while (string-match (regexp-quote term) result start)
		  (setq result
				(replace-regexp-in-string
				 (regexp-quote term)
				 (propertize (match-string 0 result) 'face 'rich-text-tabulated-match-face)
				 result nil nil start))
		  (setq start (1+ (match-beginning 0))))))
	result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tabulated List Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rich-text-tabulated-mode-map
  (let ((map (make-sparse-keymap)))
	(set-keymap-parent map tabulated-list-mode-map)
	(define-key map (kbd "RET") #'rich-text-tabulated-jump)
	(define-key map "d"         #'rich-text-tabulated-delete-point)
	(define-key map "o"         #'rich-text-tabulated-other-window)
	(define-key map "/"         #'rich-text-tabulated-filter)
	(define-key map "L"         #'rich-text-list-tabulated)
	map)
  "Keymap for `rich-text-tabulated-mode'.")

(defvar-local rich-text-tabulated--filter-term nil)
(defconst rich-text-tabulated--content-indent (+ 10 1 25 1 6 1))

(defun rich-text-tabulated--format-content (raw)
  (let* ((lines (split-string raw "\n" t))
		 (limited-lines
		  (cond
		   ((null rich-text-tabulated-max-content-lines) lines)
		   ((<= (length lines) rich-text-tabulated-max-content-lines) lines)
		   (t (append (cl-subseq lines 0 rich-text-tabulated-max-content-lines) '("…")))))
		 (truncated-lines
		  (mapcar (lambda (line)
					(truncate-string-to-width line 80 0 nil ""))
				  limited-lines)))
	(mapconcat (lambda (line) line)
			   truncated-lines
			   (concat "\n" (make-string rich-text-tabulated--content-indent ?\s)))))

(defun rich-text-tabulated--build-entries (&optional filter-term)
  "Build entries - show ALL records, even at same position."
  (let* ((entries '())
		 (and-clauses (and filter-term (rich-text--parse-filter-query filter-term)))
		 (all-terms (and and-clauses (apply #'append and-clauses))))
	(dolist (file (mapcar #'car (rich-text-db-distinct-query 'ov [id])))
	  (let ((overlays (rich-text-db-query 'ov [beg end props] `(= id ,file))))
		(dolist (ov overlays)
		  (let* ((beg (nth 0 ov))
				 (end (nth 1 ov))
				 (raw-props (or (nth 2 ov) ""))
				 (file-base (file-name-nondirectory file))
				 (pos-str (format "%d-%d" beg end))
				 (style-name
				  (cond
				   ((string-prefix-p "SCALE:" raw-props)
					(let ((factor (substring raw-props 6 (string-match ":" raw-props 6))))
					  (format "×%s" factor)))
				   ((string-prefix-p "FONT:" raw-props)
					"font")
				   (t raw-props)))
				 (entry-data `(:file ,file-base :pos ,pos-str :style ,style-name :props ,raw-props))
				 (match? (or (null and-clauses)
							 (rich-text--match-entry-p entry-data and-clauses))))
			(when match?
			  (let* ((props-to-display
					  (if all-terms
						  (rich-text--highlight-matches raw-props all-terms)
						raw-props))
					 (formatted-props (rich-text-tabulated--format-content props-to-display))
					 (id (list file beg end raw-props))
					 (row (vector style-name file-base pos-str formatted-props)))
				(push (list id row) entries)))))))
	(nreverse entries)))

(defun rich-text-tabulated-refresh ()
  (interactive)
  (unless (eq major-mode 'rich-text-tabulated-mode)
	(user-error "Not in rich-text tabulated list buffer"))
  (let* ((origin-line (line-number-at-pos))
		 (origin-id   (tabulated-list-get-id)))
	(setq tabulated-list-entries
		  (rich-text-tabulated--build-entries rich-text-tabulated--filter-term))
	(tabulated-list-print t)
	(when origin-id
	  (goto-char (point-min))
	  (while (and (not (eobp))
				  (not (equal (tabulated-list-get-id) origin-id)))
		(forward-line 1)))
	(unless (equal (tabulated-list-get-id) origin-id)
	  (goto-char (point-min))
	  (forward-line (1- (max 1 (min origin-line (count-lines (point-min) (point-max)))))))
	(run-at-time 0 nil
				 (lambda (win)
				   (when (and (window-live-p win)
							  (eq (window-buffer win) (current-buffer)))
					 (with-selected-window win
					   (recenter))))
				 (get-buffer-window (current-buffer)))))

(defun rich-text-tabulated-filter (term)
  (interactive "sFilter (space=OR, &&=AND): ")
  (setq rich-text-tabulated--filter-term (unless (string-empty-p term) term))
  (rich-text-tabulated-refresh))

(define-derived-mode rich-text-tabulated-mode tabulated-list-mode "RT-List"
  "Tabulated list of rich-text overlays."
  :keymap rich-text-tabulated-mode-map
  (setq tabulated-list-format
		[("Style"   10 nil)
		 ("File"    25 t)
		 ("Pos"     16 nil)
		 ("Props"    0 t)])
  (setq rich-text-tabulated--filter-term nil)
  (tabulated-list-init-header))

;; Helper and commands
(defun rich-text-tabulated--get-current-id ()
  (let ((id (tabulated-list-get-id)))
	(unless id (user-error "No overlay on this line"))
	id))

(defun rich-text-tabulated-jump ()
  "Jump to overlay location and close the list window."
  (interactive)
  (let* ((id (rich-text-tabulated--get-current-id))
		 (file (car id))
		 (pos (cadr id))
		 (list-window (selected-window)))  ; 记住当前 list window
	(when (file-exists-p file)
	  ;; 跳转到目标文件和位置
	  (find-file file)
	  (goto-char pos)
	  (pulse-momentary-highlight-one-line)
	  ;; 关闭 list window（如果还存在）
	  (when (window-live-p list-window)
		(delete-window list-window)))))

(defun rich-text-tabulated-delete-point ()
  "Delete exactly one operation and update visual effect immediately."
  (interactive)
  (let* ((id (rich-text-tabulated--get-current-id))
		 (file (car id))
		 (beg (cadr id))
		 (end (caddr id))
		 (raw-props (cadddr id))
		 (list-buf (current-buffer)))
	(when (y-or-n-p (format "Delete this operation at %s:%s-%s? "
						   (file-name-nondirectory file) beg end))
	  ;; 1. 删除数据库记录
	  (rich-text-db-crud
	   `[:delete :from ov
		 :where (and (= id ,file) (= beg ,beg) (= end ,end) (= props ,raw-props))])
	  ;; 2. 精确删除 buffer 中匹配的 overlay
	  (let ((buf (get-file-buffer file)))
		(when buf
		  (with-current-buffer buf
			(cond
			 ;; SCALE 样式：重建区域（已包含刷新）
			 ((string-prefix-p "SCALE:" raw-props)
			  (rich-text--rebuild-scale-overlays file beg end))
			 ;; FONT 样式：字符串匹配 + 强制刷新
			 ((string-prefix-p "FONT:" raw-props)
			  (let ((deleted nil))
				(dolist (ov (overlays-in beg end))
				  (when (and (stringp (overlay-get ov 'rich-text))
							 (string= (overlay-get ov 'rich-text) raw-props)
							 (= (overlay-start ov) beg)
							 (= (overlay-end ov) end))
					(delete-overlay ov)
					(setq deleted t)))
				(when deleted
				  (redisplay t))))
			 ;; 内置样式：符号匹配
			 (t
			  (let ((target-symbol (condition-case nil (read raw-props) (error nil))))
				(dolist (ov (overlays-in beg end))
				  (when (and (eq (overlay-get ov 'rich-text) target-symbol)
							 (= (overlay-start ov) beg)
							 (= (overlay-end ov) end))
					(delete-overlay ov)))))))))
	  ;; 3. 刷新列表
	  (switch-to-buffer list-buf)
	  (rich-text-tabulated-refresh)
	  (message "Deleted operation."))))

(defun rich-text-tabulated-other-window ()
  (interactive)
  (let* ((id (rich-text-tabulated--get-current-id))
		 (file (car id))
		 (pos (cadr id)))
	(when (file-exists-p file)
	  (let ((buf (or (get-file-buffer file) (find-file-noselect file))))
		(save-selected-window
		  (pop-to-buffer buf 'other-window)
		  (goto-char pos)
		  (recenter)
		  (pulse-momentary-highlight-one-line))))))

;;;###autoload
(defun rich-text-list-tabulated ()
  (interactive)
  (let ((buf (get-buffer-create "*Rich-Text List*")))
	(with-current-buffer buf
	  (rich-text-tabulated-mode)
	  (rich-text-tabulated-refresh))
	(pop-to-buffer buf)))

;;;###autoload
(defun rich-text-tabulated-show-file (file-id)
  (interactive
   (list (completing-read "Show file: "
						  (mapcar #'car (rich-text-db-distinct-query 'ov [id]))
						  nil t)))
  (let ((buf (get-buffer-create "*Rich-Text List*")))
	(with-current-buffer buf
	  (rich-text-tabulated-mode)
	  (rich-text-tabulated-filter (format "file:%s" (file-name-nondirectory file-id))))
	(pop-to-buffer buf)))

(provide 'rich-text-list-tabulated)
;;; rich-text-list-tabulated.el ends here
