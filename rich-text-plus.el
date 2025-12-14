;;; rich-text-plus.el --- Extra rich-text styles  -*- lexical-binding: t -*-

(require 'rich-text)
(require 'rich-text-db)
(require 'ov)

(declare-function rich-text-clear-at-point "rich-text")
(declare-function rich-text--word-bounds "rich-text")

;; 列表样式
(define-rich-text plain-list "pl"
  (lambda () (list 'line-prefix "• ")))

;; 删除线
(define-rich-text strikethrough "st"
  (lambda () '(face (:strike-through t))))

;; 自适应下划线样式测试
(define-rich-text-dwim underline-dwim "uu"
  :props-fn (lambda () '(face (:underline (:style wave))))
  :light-fn (lambda () '(face (:underline (:style line))))
  :dark-fn  (lambda () '(face (:underline (:style wave :color "#666")))))

;; 高亮标记（不同颜色）
(define-rich-text-dwim mark-yellow "my"
  :light-fn (lambda () '(face (:background "#FFEB3B" :foreground "#000000" :weight bold)))
  :dark-fn  (lambda () '(face (:background "#F9A825" :foreground "#000000" :weight bold))))

(define-rich-text-dwim mark-green "mg"
  :light-fn (lambda () '(face (:background "#A8E6CF" :foreground "#000000" :weight bold)))
  :dark-fn  (lambda () '(face (:background "#2C5F2D" :foreground "#98C379" :weight bold))))

(define-rich-text-dwim mark-blue "mb"
  :light-fn (lambda () '(face (:background "#ADD8E6" :foreground "#000000" :weight bold)))
  :dark-fn  (lambda () '(face (:background "#3E4B5B" :foreground "#61AFEF" :weight bold))))

(define-rich-text-dwim mark-red "mr"
  :light-fn (lambda () '(face (:background "#FFAAA5" :foreground "#000000" :weight bold)))
  :dark-fn  (lambda () '(face (:background "#4F3A3A" :foreground "#E06C75" :weight bold))))

;; 声明外部函数
(declare-function rich-text-clear-at-point "rich-text")
(declare-function rich-text--word-bounds "rich-text")

;; ┌──────────────────────────────────────────────────────────────┐
;; │  1. 字体选择（修改为单词范围，支持替换）                     │
;; └──────────────────────────────────────────────────────────────┘

;;;###autoload
(defun rich-text-set-font-overlay (font-name)
  "Apply FONT-NAME to region or current word, replacing any existing font overlay."
  (interactive
   (list (completing-read "Font: " (font-family-list) nil t)))
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (rich-text--word-bounds)))
         (beg (car bounds))
         (end (cdr bounds))
         (file-id (or buffer-file-name (buffer-name)))
         (tag (format "FONT:%s" font-name)))

    ;; Remove existing FONT: overlays in range
    (dolist (ov (overlays-in beg end))
      (let ((rt (overlay-get ov 'rich-text)))
        (when (and (stringp rt) (string-prefix-p "FONT:" rt))
          (delete-overlay ov))))

    ;; Apply new font if valid
    (when (member font-name (font-family-list))
      (let ((ov (make-overlay beg end nil t nil))) ; front-advance=t, rear-advance=nil
        (overlay-put ov 'face `(:family ,font-name))
        (overlay-put ov 'rich-text tag)
        (overlay-put ov 'evaporate t))
      (rich-text-db-insert 'ov `([,file-id ,beg ,end ,(prin1-to-string tag)])))))

;; ┌──────────────────────────────────────────────────────────────┐
;; │  2. 独立缩放操作（修改为单词范围）                           │
;; └──────────────────────────────────────────────────────────────┘

;;;###autoload
(defun rich-text-adjust-font-size (factor)
  "Apply a scale operation to region or current word."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (rich-text--word-bounds)))
         (beg (car bounds))
         (end (cdr bounds))
         (file-id (or buffer-file-name (buffer-name)))
         (unique-id (format "s%d" (random 1000000)))
         (tag (format "SCALE:%.4f:%s" factor unique-id)))

    (let ((ov (make-overlay beg end nil t nil)))
      (overlay-put ov 'face `(:height ,factor))
      (overlay-put ov 'rich-text tag)
      (overlay-put ov 'evaporate t))
    (rich-text-db-insert 'ov `([,file-id ,beg ,end ,(prin1-to-string tag)]))))

;;;###autoload
(defun rich-text-increase-font-size ()
  "Add +20% scale operation."
  (interactive)
  (rich-text-adjust-font-size 1.2))

;;;###autoload
(defun rich-text-decrease-font-size ()
  "Add -16.7% scale operation."
  (interactive)
  (rich-text-adjust-font-size 0.8333333333333334))

;; ┌──────────────────────────────────────────────────────────────┐
;; │ 3.恢复动态样式（FONT / SCALE）                               │
;; └──────────────────────────────────────────────────────────────┘

(defun rich-text-plus--restore-dynamic-overlays ()
  "Restore FONT: and SCALE: overlays from database."
  (when-let ((file-id (buffer-file-name)))
    ;; Clear existing dynamic overlays to avoid duplication
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((rt (overlay-get ov 'rich-text)))
        (when (and (stringp rt)
                   (or (string-prefix-p "FONT:" rt)
                       (string-prefix-p "SCALE:" rt)))
          (delete-overlay ov))))

    ;; Query DB for quoted strings like "\"FONT:...\""
    (let ((records (rich-text-db-query 'ov [beg end props]
                                       `(and (= id ,file-id)
                                             (or (like props "\"FONT:%\"")
                                                 (like props "\"SCALE:%\""))))))
      (dolist (row records)
        (let* ((beg (nth 0 row))
               (end (nth 1 row))
               (props-str (nth 2 row))
               (style-val (condition-case nil (read props-str) (error props-str)))
               created)

          (when (and (stringp style-val) (> (- end beg) 0))
            (cond
             ((string-prefix-p "FONT:" style-val)
              (let ((font-name (substring style-val 5)))
                (when (member font-name (font-family-list))
                  (let ((ov (make-overlay beg end nil t nil)))
                    (overlay-put ov 'face `(:family ,font-name))
                    (overlay-put ov 'rich-text style-val)
                    (overlay-put ov 'evaporate t))
                  (setq created t))))

             ((string-prefix-p "SCALE:" style-val)
              (let* ((parts (split-string style-val ":"))
                     (factor-str (nth 1 parts))
                     (factor (when factor-str (string-to-number factor-str))))
                (when (and factor (> factor 0))
                  (let ((ov (make-overlay beg end nil t nil)))
                    (overlay-put ov 'face `(:height ,factor))
                    (overlay-put ov 'rich-text style-val)
                    (overlay-put ov 'evaporate t))
                  (setq created t)))))))))))
;; ┌──────────────────────────────────────────────────────────────┐
;; │  4. 字体恢复（支持替换逻辑）                                 │
;; └──────────────────────────────────────────────────────────────┘

(defun rich-text-plus--get-font-props (style-name)
  "Get face props for FONT:... styles."
  (when (string-prefix-p "FONT:" (symbol-name style-name))
	(let ((font-name (substring (symbol-name style-name) 5)))
	  (when (member font-name (font-family-list))
		`(face (:family ,font-name))))))

(defun rich-text-plus--restore-font-overlays ()
  "Restore FONT:... overlays from database.
确保同一范围的多条记录只使用最后一条（最新的）。"
  (when (buffer-file-name)
	(let ((file-id (buffer-file-name)))
	  (let ((font-records (rich-text-db-query 'ov [beg end props]
											  `(and (= id ,file-id)
													(like props "FONT:%")))))
		;; 按 (beg, end) 分组
		(let ((groups (make-hash-table :test 'equal)))
		  (dolist (row font-records)
			(let* ((beg (nth 0 row))
				   (end (nth 1 row))
				   (key (cons beg end)))
			  (push row (gethash key groups))))

		  ;; 对每个范围，只使用最后一条记录创建 overlay
		  (maphash (lambda (range rows)
				 (let* ((latest-row (car rows))  ; 最后插入的记录
						(beg (car range))
						(end (cdr range))
						(props (nth 2 latest-row))
						(font-name (substring props 5)))
				   (when (member font-name (font-family-list))
					 (let ((ov (make-overlay beg end)))
					   (overlay-put ov 'face `(:family ,font-name))
					   (overlay-put ov 'rich-text props)
					   (overlay-put ov 'evaporate t)))))
			   groups))))))

;; ┌──────────────────────────────────────────────────────────────┐
;; │  5. 清理重复的数据库记录                                     │
;; └──────────────────────────────────────────────────────────────┘

(defun rich-text-plus--cleanup-duplicate-fonts ()
  "Remove duplicate FONT records, keeping only the latest one per range."
  (interactive)
  (when (buffer-file-name)
	(let ((file-id (buffer-file-name)))
	  (let ((font-records (rich-text-db-query 'ov [beg end props]
											  `(and (= id ,file-id)
													(like props "FONT:%")))))
		(let ((groups (make-hash-table :test 'equal)))
		  (dolist (row font-records)
			(let ((key (cons (nth 0 row) (nth 1 row))))
			  (push row (gethash key groups))))

		  ;; 删除每个范围内除了最后一条以外的所有记录
		  (maphash (lambda (range rows)
				 (dolist (old-row (cdr rows))  ; 跳过第一条（最新的）
				   (rich-text-db-delete 'ov
					 `(and (= id ,file-id)
						   (= beg ,(car range))
						   (= end ,(cdr range))
						   (= props ,(nth 2 old-row))))))
			   groups))

		(message "Cleaned up duplicate font records")))))

;; ┌──────────────────────────────────────────────────────────────┐
;; │  6. 集成到 hook                                              │
;; └──────────────────────────────────────────────────────────────┘

(with-eval-after-load 'rich-text
  (remove-hook 'find-file-hook #'rich-text-plus--restore-font-overlays)
  (remove-hook 'find-file-hook #'rich-text-plus-restore-all-scales)
  (add-hook 'find-file-hook #'rich-text-plus--restore-dynamic-overlays)
  (advice-add 'switch-to-buffer :after
              (lambda (&rest _)
                (when (buffer-file-name)
                  (rich-text-plus--restore-dynamic-overlays)))))

(provide 'rich-text-plus)
;;; rich-text-plus.el ends here
