;;; rich-text-plus.el --- Extra rich-text styles  -*- lexical-binding: t -*-

(require 'rich-text)
(require 'rich-text-db)
(require 'ov)

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
(declare-function rich-text--word-bounds "rich-text") ; ← 新增声明

;; ┌──────────────────────────────────────────────────────────────┐
;; │  1. 字体选择（修改为单词范围）                             │
;; └──────────────────────────────────────────────────────────────┘

;;;###autoload
(defun rich-text-set-font-overlay (font-name)
  "Apply FONT-NAME to region or current word."
  (interactive
   (list (completing-read "Font: " (font-family-list) nil t)))
  (let* ((bounds (if (use-region-p)
					 (cons (region-beginning) (region-end))
				   (rich-text--word-bounds)))
		 (beg (car bounds))
		 (end (cdr bounds))
		 (file-id (or buffer-file-name (buffer-name)))
		 (style-tag (format "FONT:%s" font-name)))
	(let ((ov (make-overlay beg end)))
	  (overlay-put ov 'face `(:family ,font-name))
	  (overlay-put ov 'rich-text style-tag)
	  (overlay-put ov 'evaporate t)
	  (rich-text-db-insert 'ov
		`([,file-id ,beg ,end ,style-tag]))
	  (message "Font '%s' applied to %d-%d" font-name beg end))))

;; ┌──────────────────────────────────────────────────────────────┐
;; │  2. 独立缩放操作（修改为单词范围）                         │
;; └──────────────────────────────────────────────────────────────┘

;;;###autoload
(defun rich-text-adjust-font-size (factor)
  "Add a new independent scale operation over region or current word."
  (interactive)
  (let* ((bounds (if (use-region-p)
					 (cons (region-beginning) (region-end))
				   (rich-text--word-bounds)))
		 (beg (car bounds))
		 (end (cdr bounds))
		 (file-id (or buffer-file-name (buffer-name)))
		 (unique-id (format "scale-%d" (random 1000000)))
		 (tag (format "SCALE:%.4f:%s" factor unique-id)))
	(let ((ov (make-overlay beg end)))
	  (overlay-put ov 'face `(:height ,factor))
	  (overlay-put ov 'rich-text tag)
	  (overlay-put ov 'evaporate t)
	  (rich-text-db-insert 'ov `([,file-id ,beg ,end ,tag]))
	  (message "Added scale factor %.2fx to %d-%d" factor beg end))))

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

;; 重建缩放效果（基于所有剩余操作）
(defun rich-text--rebuild-scale-overlays (file beg end)
  "Rebuild scale effect from all remaining SCALE records in region [BEG, END)."
  (let ((buf (get-file-buffer file)))
	(when buf
	  (with-current-buffer buf
		;; 1. 删除该区域所有 SCALE 相关 overlay
		(dolist (ov (overlays-in beg end))
		  (let ((rt (overlay-get ov 'rich-text)))
			(when (and (stringp rt)
					   (or (string-prefix-p "SCALE:" rt)
						   (string= rt "SCALE-TOTAL")))
			  (delete-overlay ov))))
		;; 2. 从数据库计算总 height
		(let ((rows (rich-text-db-query 'ov [beg end props]
										`(and (= id ,file)
											  (<= beg ,end)
											  (>= end ,beg)
											  (like props "SCALE:%")))))
		  (when rows
			(let ((total-height 1.0))
			  (dolist (row rows)
				(let* ((props (nth 2 row))
					   (end-pos (string-match ":" props 6)))
				  (when end-pos
					(let ((factor (string-to-number (substring props 6 end-pos))))
					  (setq total-height (* total-height factor))))))
			  ;; 3. 创建总效果 overlay
			  (let ((ov (make-overlay beg end)))
				(overlay-put ov 'face `(:height ,total-height))
				(overlay-put ov 'rich-text "SCALE-TOTAL")
				(overlay-put ov 'evaporate t)))))))))

;; ┌──────────────────────────────────────────────────────────────┐
;; │  3. 自动恢复所有效果                                        │
;; └──────────────────────────────────────────────────────────────┘

(defun rich-text-plus-restore-all-scales ()
  "Restore all SCALE overlays from database when opening a file."
  (when (buffer-file-name)
	(let ((file-id (buffer-file-name)))
	  (let ((scale-records (rich-text-db-query 'ov [beg end props]
											   `(and (= id ,file-id)
													 (like props "SCALE:%")))))
		(when scale-records
		  (let ((regions (make-hash-table :test 'equal)))
			(dolist (row scale-records)
			  (let ((beg (nth 0 row))
					(end (nth 1 row)))
				(push row (gethash (cons beg end) regions))))
			(maphash (lambda (range rows)
					   (rich-text--rebuild-scale-overlays
						file-id (car range) (cdr range)))
					 regions)))))))

;; 集成到 hook
(with-eval-after-load 'rich-text
  (add-hook 'find-file-hook #'rich-text-plus-restore-all-scales))

(provide 'rich-text-plus)
;;; rich-text-plus
