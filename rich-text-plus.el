;;; rich-text-plus.el --- Additional rich-text styles -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rich-text))

;; 运行时也需要加载（提供函数和变量）
(require 'rich-text)

;; 粗体+斜体
(define-rich-text bold-italic "bi"
  (lambda () '(face (:weight bold :slant italic))))

;; 粗体+下划线
(define-rich-text bold-underline "bu"
  (lambda () '(face (:weight bold :underline t))))

;; 斜体+下划线
(define-rich-text italic-underline "iu"
  (lambda () '(face (:slant italic :underline t))))

;; 块引用样式（支持浅色/深色主题）
(define-rich-text-dwim blockquote "qt"
  :light-fn
  (lambda ()
	;; 动态在运行时生成带 face 的前缀字符串
	(list 'wrap-prefix (propertize "┃ " 'face '(:foreground "#999"))
		  'line-prefix (propertize "┃ " 'face '(:foreground "#999"))
		  'face '(:slant italic)))
  :dark-fn
  (lambda ()
	(list 'wrap-prefix (propertize "┃ " 'face '(:foreground "#ccc"))
		  'line-prefix (propertize "┃ " 'face '(:foreground "#ccc"))
		  'face '(:slant italic))))

;; 列表样式
(define-rich-text plain-list "pl"
  (lambda () (list 'line-prefix "• ")))

;; 小号文字
(define-rich-text small-text "sm"
  (lambda () '(face (:height 0.9))))

;; 大号文字
(define-rich-text large-text "lg"
  (lambda () '(face (:height 1.2))))

;; 等宽字体（代码样式）
(define-rich-text monospace "ms"
  (lambda () '(face (:family "monospace" :background "#f5f5f5"))))

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

;; 添加使用说明
(defun rich-text-plus-show-keybindings ()
  "Show all keybindings defined in rich-text-plus."
  (interactive)
  (with-current-buffer (get-buffer-create "*Rich-Text-Plus Keys*")
	(erase-buffer)
	(insert "Rich-Text-Plus Keybindings\n")
	(insert "===========================\n\n")
	(insert "Combinations:\n")
	(insert "  bi  - Bold + Italic\n")
	(insert "  bu  - Bold + Underline\n")
	(insert "  iu  - Italic + Underline\n\n")
	(insert "Styles:\n")
	(insert "  qt  - Blockquote (with vertical bar)\n")
	(insert "  pl  - Plain list (bullet point)\n")
	(insert "  sm  - Small text\n")
	(insert "  lg  - Large text\n")
	(insert "  ms  - Monospace (code style)\n")
	(insert "  st  - Strikethrough\n")
	(insert "  uu  - Underline (theme adaptive)\n\n")
	(insert "Colored Highlights:\n")
	(insert "  my  - Mark Yellow\n")
	(insert "  mg  - Mark Green\n")
	(insert "  mb  - Mark Blue\n")
	(insert "  mr  - Mark Red\n\n")
	(insert "Usage: Select text and press the key combination.\n")
	(goto-char (point-min))
	(help-mode))
  (display-buffer "*Rich-Text-Plus Keys*"))

(provide 'rich-text-plus)

;;; rich-text-plus.el ends here
