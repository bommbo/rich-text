;;; rich-text.el --- Rich text formatting for Emacs buffers -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'rich-text-db)

;;;; Variables

(defconst rich-text-bold-types
  '(ultra-bold extra-bold bold semi-bold normal semi-light light extra-light ultra-light)
  "A list of rich-text bold face types.")

(defconst rich-text-italic-types
  '(italic oblique normal reverse-italic reverse-oblique)
  "A list of rich-text italic face types.")

;;; headline

(defvar rich-text-headline-1-height 1.8
  "Default height of rich-text headline-1 face.")

(defvar rich-text-headline-2-height 1.4
  "Default height of rich-text headline-2 face.")

(defvar rich-text-headline-3-height 1.2
  "Default height of rich-text headline-2 face.")

;;; bold

(defvar rich-text-bold-type 'bold
  "Default type of rich-text bold face,
 should be one of the symbols in `rich-text-bold-types'.")

;;; italic

(defvar rich-text-italic-type 'italic
  "Default type of rich-text italic face,
 should be one of the symbols in `rich-text-italic-types'.")

;;; font color

(defvar rich-text-fontcolor-light-color "#BA36A5"
  "Default color of rich-text font color in light theme.")

(defvar rich-text-fontcolor-dark-color "#FFCB6B"
  "Default color of rich-text font color in dark theme.")

;;; highlight

;; Light theme 颜色配置
(defvar rich-text-highlight-light-bg "#FFEAA7"
  "Background color for highlight in light theme (GUI).")

(defvar rich-text-highlight-light-fg "#000000"
  "Foreground color for highlight in light theme (GUI).")

(defvar rich-text-highlight-light-bg-term "yellow"
  "Background color for highlight in light theme (terminal).")

(defvar rich-text-highlight-light-fg-term "black"
  "Foreground color for highlight in light theme (terminal).")

;; Dark theme 颜色配置
(defvar rich-text-highlight-dark-bg "#4B5263"
  "Background color for highlight in dark theme (GUI).")

(defvar rich-text-highlight-dark-fg "#E5C07B"
  "Foreground color for highlight in dark theme (GUI).")

(defvar rich-text-highlight-dark-bg-term "blue"
  "Background color for highlight in dark theme (terminal).")

(defvar rich-text-highlight-dark-fg-term "yellow"
  "Foreground color for highlight in dark theme (terminal).")

;; 强制主题模式（用于透明背景的终端）
(defvar rich-text-force-theme-mode nil
  "Force theme mode to 'light or 'dark.
Useful when using transparent terminal background where Emacs
cannot detect the theme mode correctly.
Set to nil to use automatic detection.")

;;; mode map

(defvar rich-text-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for rich-text-mode.")

(defvar selected-keymap
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for selected-mode.")

(defvar rich-text-selected-ignore-modes nil
  "List of major modes for which selected will not be turned on.")

;;; theme background change hook

(defvar rich-text-theme-background-change-hook nil
  "Normal hook that is run after the background of theme changed.")

;;; ✓ 核心改进：统一的样式注册表
(defvar rich-text-style-registry (make-hash-table :test 'eq)
  "Registry for all rich-text styles.
Maps style-name (symbol) to a plist with :key, :props-fn, :light-fn, :dark-fn.")

;;;; Helper functions

(defun rich-text-display-graphic-p ()
  "Check if current display supports graphic (GUI)."
  (display-graphic-p))

;;; ✓ 新增：获取光标下单词范围
(defun rich-text--word-bounds ()
  "Return (beg . end) of word at point.
If no word at point, return (point . point)."
  (if (or (looking-at "\\sw") (looking-back "\\sw" 1))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (if bounds
            (cons (car bounds) (cdr bounds))
          (cons (point) (point))))
    (cons (point) (point))))

;;;; Define rich text face

(defun keys-of-command (command keymap)
  "Return all key bindins of COMMAND defined in KEYMAP."
  (mapcar (lambda (vector-keys)
            (kbd (key-description vector-keys)))
          (where-is-internal command keymap)))

(defun unset-command-keys (keymap command)
  (mapcar (lambda (key)
            (define-key keymap key nil))
          (keys-of-command command keymap)))

(defun override-keys (alist keymap)
  "Override keybindings in KEYMAP with cons-cell in ALIST.
ALIST consists with key and command."
  (mapcar (lambda (key-cmd)
            (let ((key (car key-cmd))
                  (cmd (cdr key-cmd)))
              (unset-command-keys keymap cmd)
              (define-key keymap key cmd)))
          alist))

;;; ✓ 改进的宏：统一注册所有样式
(cl-defmacro define-rich-text (name key props-fn)
  "Define a rich-text style with NAME, triggered by KEY, using PROPS-FN.
NAME should be an unquoted symbol.
PROPS-FN should be a function that returns property list."
  (declare (indent defun))
  (unless (symbolp name)
    (error "define-rich-text: NAME must be a symbol, got %S" name))
  (let* ((name-str (symbol-name name))
         (func-prefix "rich-text-render-")
         (render-func (intern (concat func-prefix name-str))))
    `(progn
       ;; 注册到统一注册表
       (puthash ',name 
                (list :key ,key :props-fn ,props-fn)
                rich-text-style-registry)
       (defun ,render-func ()
         (interactive)
         (rich-text-apply-style ',name))
       (unset-command-keys selected-keymap ',render-func)
       (define-key selected-keymap ,key #',render-func))))

(cl-defmacro define-rich-text-dwim (name key &key props-fn light-fn dark-fn)
  "Define a theme-aware rich-text style with NAME, triggered by KEY.
NAME should be an unquoted symbol.
PROPS-FN, LIGHT-FN, DARK-FN should be functions that return property lists."
  (declare (indent defun))
  (unless (symbolp name)
    (error "define-rich-text-dwim: NAME must be a symbol, got %S" name))
  (let* ((name-str (symbol-name name))
         (func-prefix "rich-text-render-")
         (render-func (intern (concat func-prefix name-str "-dwim"))))
    `(progn
       ;; 注册到统一注册表
       (puthash ',name 
                (list :key ,key 
                      :props-fn ,props-fn
                      :light-fn ,light-fn 
                      :dark-fn ,dark-fn)
                rich-text-style-registry)
       (defun ,render-func ()
         (interactive)
         (rich-text-apply-style ',name))
       (unset-command-keys selected-keymap ',render-func)
       (define-key selected-keymap ,key #',render-func))))

;;;; Rich-text face properties functions

(defun rich-text-headline-1-props ()
  `(face (:height ,rich-text-headline-1-height :weight bold)))

(defun rich-text-headline-2-props ()
  `(face (:height ,rich-text-headline-2-height :weight bold)))

(defun rich-text-headline-3-props ()
  `(face (:height ,rich-text-headline-3-height :weight bold)))

(defun rich-text-bold-props ()
  `(face (:weight ,rich-text-bold-type)))

(defun rich-text-italic-props ()
  `(face (:slant ,rich-text-italic-type)))

(defun rich-text-underline-line-props ()
  '(face (:underline (:style line))))

(defun rich-text-underline-wave-props ()
  '(face (:underline (:style wave))))

(defun rich-text-fontcolor-light-props ()
  `(face (:foreground ,rich-text-fontcolor-light-color)))

(defun rich-text-fontcolor-dark-props ()
  `(face (:foreground ,rich-text-fontcolor-dark-color)))

(defun rich-text-highlight-light-props ()
  "Get highlight properties for LIGHT theme."
  (if (rich-text-display-graphic-p)
      `(face (:background ,rich-text-highlight-light-bg 
                          :foreground ,rich-text-highlight-light-fg 
                          :weight bold))
    `(face (:background ,rich-text-highlight-light-bg-term 
                        :foreground ,rich-text-highlight-light-fg-term 
                        :weight bold))))

(defun rich-text-highlight-dark-props ()
  "Get highlight properties for DARK theme."
  (if (rich-text-display-graphic-p)
      `(face (:background ,rich-text-highlight-dark-bg 
                          :foreground ,rich-text-highlight-dark-fg 
                          :weight bold))
    `(face (:background ,rich-text-highlight-dark-bg-term 
                        :foreground ,rich-text-highlight-dark-fg-term 
                        :weight bold))))

;;;; Specific for light/dark themes

(defun rich-text-theme-dark-p ()
  "Check if current theme is dark."
  (cond
   ((eq rich-text-force-theme-mode 'dark) t)
   ((eq rich-text-force-theme-mode 'light) nil)
   ((eq (frame-parameter nil 'background-mode) 'dark) t)
   ((and (not (display-graphic-p))
         (boundp 'custom-enabled-themes)
         (seq-some (lambda (theme)
                     (string-match-p "dark\\|night\\|gruvbox-dark" 
                                     (symbol-name theme)))
                   custom-enabled-themes))
    t)
   (t nil)))

(defun rich-text-theme-light-p ()
  "Check if current theme is light."
  (cond
   ((eq rich-text-force-theme-mode 'light) t)
   ((eq rich-text-force-theme-mode 'dark) nil)
   ((eq (frame-parameter nil 'background-mode) 'light) t)
   ((and (not (display-graphic-p))
         (boundp 'custom-enabled-themes)
         (seq-some (lambda (theme)
                     (string-match-p "light\\|day\\|gruvbox-light" 
                                     (symbol-name theme)))
                   custom-enabled-themes))
    t)
   (t nil)))

;;;; ✓ 核心改进：统一的样式应用和获取函数

(defun rich-text-get-props (style-name)
  "Get properties for STYLE-NAME based on current theme.
Returns the appropriate props by calling the registered functions."
  (when-let ((style-info (gethash style-name rich-text-style-registry)))
    (let ((props-fn (plist-get style-info :props-fn))
          (light-fn (plist-get style-info :light-fn))
          (dark-fn (plist-get style-info :dark-fn)))
      (cond
       ((and (rich-text-theme-light-p) light-fn)
        (funcall light-fn))
       ((and (rich-text-theme-dark-p) dark-fn)
        (funcall dark-fn))
       (props-fn
        (funcall props-fn))
       (t nil)))))

;;;; Conflict resolution for face-based styles

(defun rich-text-clear-conflicting-styles (beg end style-name)
  "Clear styles that conflict with STYLE-NAME in region [BEG, END).
Conflicting groups:
- Highlight: my, mg, mb, mr
- Font color: fontcolor
- Bold: bold
- Italic: italic
- Underline: underline-line, underline-wave"
  (let ((conflict-groups
         '((highlight my mg mb mr)
           (color fontcolor)
           (bold bold)
           (italic italic)
           (underline underline-line underline-wave))))
    (dolist (group conflict-groups)
      (when (memq style-name (cdr group))
        (dolist (ov (overlays-in beg end))
          (let ((ov-style (overlay-get ov 'rich-text)))
            (when (and (memq ov-style (cdr group))
                       (not (eq ov-style style-name)))
              ;; 删除 overlay
              (delete-overlay ov)
              ;; 从数据库删除
              (let ((id (rich-text-buffer-or-file-id)))
                (when id
                  (rich-text-delete-ov-from-db id (overlay-start ov) (overlay-end ov) ov-style))))))))))

;;;; ✓ 应用样式到单词（唯一定义）
(defun rich-text-apply-style (style-name)
  "Apply STYLE-NAME to region or current word.
Clears conflicting styles before applying."
  (let ((props (rich-text-get-props style-name)))
    (if props
        (let (ov)
          (if (use-region-p)
              (progn
                (rich-text-clear-conflicting-styles (region-beginning) (region-end) style-name)
                (setq ov (ov-set (ov-region) props)))
            (let* ((bounds (rich-text--word-bounds))
                   (beg (car bounds))
                   (end (cdr bounds)))
              (rich-text-clear-conflicting-styles beg end style-name)
              (setq ov (ov-set (ov beg end) props))))
          ;; 设置 overlay 属性
          (setq ov (ov-set ov 'evaporate t))
          (setq ov (ov-set ov `(rich-text ,style-name)))
          ;; 根据样式决定是否需要主题切换时更新
          (when-let ((style-info (gethash style-name rich-text-style-registry)))
            (when (or (plist-get style-info :light-fn)
                      (plist-get style-info :dark-fn))
              (setq ov (ov-set ov 'reverse-p t))))
          ;; 存储到数据库
          (rich-text-store-ov style-name (car (ov-spec ov))))
      (pop-mark)
      (message "No rich-text properties found for style: %s" style-name))))

;;;; Rich text ov reverse (theme change handling)

(defun rich-text-buffer-reverse-p (buffer)
  (with-current-buffer buffer
    (ov-in 'reverse-p t)))

(defun rich-text-reverse ()
  "Update overlays when theme changes."
  (when-let ((ovs (ov-in 'reverse-p t)))
    (mapcar (lambda (ov)
              (let ((name (ov-val ov 'rich-text)))
                (when-let ((props (rich-text-get-props name)))
                  (ov-set ov props))))
            ovs)))

(defun rich-text-reverse-all (&rest _)
  "Update all overlays in all windows when theme changes."
  (save-window-excursion
    (mapcar (lambda (win)
              (let ((buffer (window-buffer win)))
                (when (rich-text-buffer-reverse-p buffer)
                  (with-current-buffer buffer
                    (rich-text-reverse)))))
            (window-list))))

(defun rich-text-change-theme-background (orig-fun &rest args)
  "Advice function when load a theme."
  (let ((before-bg (frame-parameter nil 'background-mode))
        after-bg)
    (apply orig-fun args)
    (setq after-bg (frame-parameter nil 'background-mode))
    (unless (eq before-bg after-bg)
      (run-hooks 'rich-text-theme-background-change-hook))))

;;;; ✓ 改进：统一的存储和恢复函数

(defun rich-text-buffer-or-file-id (&optional buffer-or-name)
  "Get unique ID for buffer (file path)."
  (let ((buffer-or-name (or buffer-or-name (current-buffer))))
    (buffer-file-name (get-buffer buffer-or-name))))

(defun rich-text-all-id ()
  "Get all file IDs from database."
  (when-let ((id (rich-text-buffer-or-file-id (current-buffer))))
    (mapcar #'car (rich-text-db-distinct-query 'ov [id]))))

(defun rich-text-buffer-stored-p (id)
  "Check if ID exists in database."
  (cl-member id (rich-text-all-id) :test 'string=))

(defun rich-text-store-ov (style-name ov-spec)
  "Store overlay with STYLE-NAME to database.
ov-spec format: (beg end buffer props)"
  (when-let* ((beg (nth 0 ov-spec))
              (end (nth 1 ov-spec))
              (id (rich-text-buffer-or-file-id (nth 2 ov-spec))))
    ;; 查询是否已存在相同位置和样式的记录
    (let ((old (car (rich-text-db-crud
                     `[:select rowid :from ov
                       :where (and (= id ,id) (= beg ,beg) (= end ,end) 
                                   (= props ,(prin1-to-string style-name)))]))))
      (unless old
        ;; 只插入新记录（不更新，允许同一位置有多个不同样式）
        (rich-text-db-insert
         'ov `([,id ,beg ,end ,(prin1-to-string style-name)]))))))

(defun rich-text-delete-ov-from-db (id beg end style-name)
  "Delete specific overlay record from database."
  (rich-text-db-crud
   `[:delete :from ov
     :where (and (= id ,id) (= beg ,beg) (= end ,end) 
                 (= props ,(prin1-to-string style-name)))]))

(defun rich-text-buffer-ov-specs ()
  "Get all rich-text overlay specs in current buffer."
  (mapcar (lambda (ov)
            (list (overlay-start ov)
                  (overlay-end ov)
                  (current-buffer)
                  (ov-val ov 'rich-text)))
          (ov-in 'rich-text)))

(defun rich-text-store-buffer-ov ()
  "Store all rich-text overlays in current buffer to database."
  (when-let ((id (rich-text-buffer-or-file-id)))
    ;; 清除旧记录
    (rich-text-db-delete 'ov `(= id ,id))
    ;; 存储当前所有 overlays
    (dolist (spec (rich-text-buffer-ov-specs))
      (let ((beg (nth 0 spec))
            (end (nth 1 spec))
            (style-name (nth 3 spec)))
        (when (and beg end style-name)
          (rich-text-db-insert 'ov 
            `([,id ,beg ,end ,(prin1-to-string style-name)])))))))

;;;###autoload
(defun rich-text-restore-buffer-ov ()
  "Restore overlays from database for current buffer, with full face props and font check."
  (interactive)
  (when-let* ((id (rich-text-buffer-or-file-id))
              (_ (rich-text-buffer-stored-p id))
              (specs (rich-text-db-query 'ov [beg end props]
                                         `(= id ,id))))
    (let ((count 0))
      (dolist (spec specs)
        (let* ((beg (nth 0 spec))
               (end (nth 1 spec))
               (stored-data (nth 2 spec))
               style-name props)
          ;; 解析存储的样式名称
          (condition-case nil
              (setq style-name (read stored-data))
            (error nil))

          ;; 获取完整属性
          (when (and (symbolp style-name)
                     (setq props (rich-text-get-props style-name)))
            ;; 确保 face 存在
            (unless (plist-get props 'face)
              (plist-put props 'face '()))

            ;; 检查字体是否存在
            (when-let ((family (plist-get (plist-get props 'face) :family)))
              (unless (member family (font-family-list))
                (message "Font '%s' not found! Using fallback." family)))

            ;; 创建 overlay
            (let ((ov (ov beg end props)))
              (ov-set ov 'rich-text style-name)
              (ov-set ov 'evaporate t)
              ;; 保留 DWIM 对主题切换支持
              (when-let ((style-info (gethash style-name rich-text-style-registry)))
                (when (or (plist-get style-info :light-fn)
                          (plist-get style-info :dark-fn))
                  (ov-set ov 'reverse-p t))))
            (setq count (1+ count)))))
      (when (> count 0)
        (message "%s rich-text overlay%s restored with face!" 
                 count (if (= count 1) "" "s"))))))


;;;; ✓ 精简的清除样式命令

;;;###autoload
(defun rich-text-clear-at-point (&optional no-sync)
  "Clear all rich-text overlays at point."
  (interactive)
  (let ((cleared nil)
        (id (rich-text-buffer-or-file-id))
        (overlays-to-delete '()))
    (dolist (ov (overlays-at (point)))
      (let ((rich-text-val (overlay-get ov 'rich-text)))
        (when rich-text-val
          (let ((beg (overlay-start ov))
                (end (overlay-end ov))
                (style-name rich-text-val))
            (push (list beg end style-name) overlays-to-delete)
            (delete-overlay ov)
            (setq cleared t)))))
    (unless no-sync
      (dolist (record overlays-to-delete)
        (let ((beg (nth 0 record))
              (end (nth 1 record))
              (style-name (nth 2 record)))
          (rich-text-delete-ov-from-db id beg end style-name))))
    (if cleared
        (message "Cleared overlay at point%s"
                 (if no-sync "" " (synced to DB)"))
      (message "No overlay at point"))))

;;;###autoload
(defun rich-text-clear-buffer (&optional no-sync)
  "Clear all rich-text overlays in current buffer."
  (interactive)
  (let ((count 0)
        (id (rich-text-buffer-or-file-id)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'rich-text)
        (delete-overlay ov)
        (setq count (1+ count))))
    (unless no-sync
      (when id
        (rich-text-db-delete 'ov `(= id ,id))))
    (message "Cleared %d rich-text overlay%s in buffer%s" 
             count 
             (if (= count 1) "" "s")
             (if no-sync "" " (synced to DB)"))))

;;;###autoload
(defun rich-text-clear-file (&optional no-sync)
  "Clear all rich-text overlays for a file selected from marked files."
  (interactive "P")
  (let* ((all-files (mapcar #'car (rich-text-db-distinct-query 'ov [id])))
         (file (if all-files
                   (completing-read "Clear styles for file: " all-files nil t)
                 (user-error "No files with rich-text styles found"))))
    (when (file-exists-p file)
      (let ((count 0))
        ;; 清理所有打开的 buffer 中该文件的 overlays
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (equal (buffer-file-name) file)
              (dolist (ov (overlays-in (point-min) (point-max)))
                (when (overlay-get ov 'rich-text)
                  (delete-overlay ov)
                  (setq count (1+ count)))))))
        ;; 从数据库删除
        (unless no-sync
          (rich-text-db-delete 'ov `(= id ,file)))
        (message "Cleared %d rich-text overlay%s for file %s%s" 
                 count 
                 (if (= count 1) "" "s")
                 (file-name-nondirectory file)
                 (if no-sync "" " (synced to DB)"))))))

;;;###autoload
(defun rich-text-list-overlays ()
  "List all rich-text overlays in current buffer with clickable links."
  (interactive)
  (let ((overlays '())
        (source-buffer (current-buffer)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'rich-text)
        (push (list :start (overlay-start ov)
                    :end (overlay-end ov)
                    :type (overlay-get ov 'rich-text)
                    :text (buffer-substring (overlay-start ov) (overlay-end ov))
                    :buffer source-buffer)
              overlays)))
    (if overlays
        (progn
          (with-current-buffer (get-buffer-create "*Rich-Text Overlays*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Rich-text overlays in buffer: %s\n\n" 
                              (buffer-name source-buffer)))
              (dolist (ov (reverse overlays))
                (let* ((start-pos (point))
                       ;; 捕获当前值
                       (current-buffer source-buffer)
                       (current-start (plist-get ov :start))
                       (current-end (plist-get ov :end)))
                  (insert (format "[%s] %d-%d: %s\n"
                                  (plist-get ov :type)
                                  current-start
                                  current-end
                                  (plist-get ov :text)))
                  ;; 创建可点击的按钮 - 使用反引号捕获值
                  (make-button start-pos (1- (point))
                               'action `(lambda (_button)
                                          (when (buffer-live-p ,current-buffer)
                                            (pop-to-buffer ,current-buffer)
                                            (goto-char ,current-start)
                                            (pulse-momentary-highlight-region 
                                             ,current-start ,current-end)))
                               'follow-link t
                               'help-echo "Click to jump to this overlay")))
              (goto-char (point-min))
              (special-mode)))
          (pop-to-buffer "*Rich-Text Overlays*")
          (message "Found %d rich-text overlay%s (click to jump)" 
                   (length overlays) 
                   (if (= (length overlays) 1) "" "s")))
      (message "No rich-text overlays in buffer"))))

;;;; 颜色自定义命令

;;;###autoload
(defun rich-text-set-highlight-colors (light-bg light-fg dark-bg dark-fg)
  "Set custom highlight colors for GUI display."
  (interactive
   (list
    (read-string "Light theme background (GUI): " rich-text-highlight-light-bg)
    (read-string "Light theme foreground (GUI): " rich-text-highlight-light-fg)
    (read-string "Dark theme background (GUI): " rich-text-highlight-dark-bg)
    (read-string "Dark theme foreground (GUI): " rich-text-highlight-dark-fg)))
  (setq rich-text-highlight-light-bg light-bg
        rich-text-highlight-light-fg light-fg
        rich-text-highlight-dark-bg dark-bg
        rich-text-highlight-dark-fg dark-fg)
  (message "Highlight colors updated. Refresh overlays to see changes."))

;;;###autoload
(defun rich-text-set-highlight-colors-term (light-bg light-fg dark-bg dark-fg)
  "Set custom highlight colors for terminal display."
  (interactive
   (list
    (completing-read "Light theme background (term): " 
                     '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
                     nil nil rich-text-highlight-light-bg-term)
    (completing-read "Light theme foreground (term): " 
                     '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
                     nil nil rich-text-highlight-light-fg-term)
    (completing-read "Dark theme background (term): " 
                     '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
                     nil nil rich-text-highlight-dark-bg-term)
    (completing-read "Dark theme foreground (term): " 
                     '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white")
                     nil nil rich-text-highlight-dark-fg-term)))
  (setq rich-text-highlight-light-bg-term light-bg
        rich-text-highlight-light-fg-term light-fg
        rich-text-highlight-dark-bg-term dark-bg
        rich-text-highlight-dark-fg-term dark-fg)
  (message "Terminal highlight colors updated. Refresh overlays to see changes."))

;;;###autoload
(defun rich-text-refresh-all-overlays ()
  "Refresh all rich-text overlays in current buffer."
  (interactive)
  (rich-text-reverse)
  (message "Refreshed all rich-text overlays"))

;;;; Rich text mode

(defun rich-text-use-region-keyhint (&rest _)
  "Rich text keybinding hint when a region active."
  (unless (or (minibufferp)
              (when selected-ignore-modes
                (apply #'derived-mode-p selected-ignore-modes)))
    (when (use-region-p)
      (message "[h]headline [b]bold [i]italic [u]underline \
[c]color [v]highlight"))))

;;;###autoload
(defun rich-text-install-dependencies ()
  "Install required dependencies."
  (interactive)
  (unless (package-installed-p 'selected)
    (package-install 'selected))
  (unless (package-installed-p 'ov)
    (package-install 'ov))
  (unless (package-installed-p 'dash)
    (package-install 'dash))
  (require 'selected)
  (require 'ov)
  (require 'dash))

;;;###autoload
(define-minor-mode rich-text-mode
  "Minor mode for showing rich text in buffer."
  :lighter "RT"
  :global t
  :keymap rich-text-mode-map
  (if rich-text-mode
      (progn
        (rich-text-install-dependencies)
        (selected-global-mode 1)
        (setq selected-ignore-modes rich-text-selected-ignore-modes)
        (advice-add #'handle-shift-selection :after #'rich-text-use-region-keyhint)
        (when (package-installed-p 'counsel)
          (advice-add #'counsel-load-theme :around #'rich-text-change-theme-background))
        (advice-add #'load-theme :around #'rich-text-change-theme-background)
        (advice-add #'switch-to-buffer :after #'rich-text-reverse-all)
        (add-hook 'rich-text-theme-background-change-hook #'rich-text-reverse-all)
        (add-hook 'find-file-hook #'rich-text-restore-buffer-ov)
        (add-hook 'after-save-hook #'rich-text-store-buffer-ov))
    (selected-global-mode -1)
    (advice-remove #'handle-shift-selection #'rich-text-use-region-keyhint)
    (advice-remove #'counsel-load-theme #'rich-text-change-theme-background)
    (advice-remove #'load-theme #'rich-text-change-theme-background)
    (advice-remove #'switch-to-buffer #'rich-text-reverse-all)
    (remove-hook 'rich-text-theme-background-change-hook
                 #'rich-text-reverse-all)
    (remove-hook 'find-file-hook #'rich-text-restore-buffer-ov)
    (remove-hook 'after-save-hook #'rich-text-store-buffer-ov)))

;;;; ✓ 定义所有内置样式（使用统一的宏）

(define-rich-text headline-1 "h1"
  #'rich-text-headline-1-props)

(define-rich-text headline-2 "h2"
  #'rich-text-headline-2-props)

(define-rich-text headline-3 "h3"
  #'rich-text-headline-3-props)

(define-rich-text bold "bb"
  #'rich-text-bold-props)

(define-rich-text italic "ii"
  #'rich-text-italic-props)

(define-rich-text underline-line "ul"
  #'rich-text-underline-line-props)

(define-rich-text underline-wave "uw"
  #'rich-text-underline-wave-props)

(define-rich-text-dwim fontcolor "cc"
  :light-fn #'rich-text-fontcolor-light-props
  :dark-fn #'rich-text-fontcolor-dark-props)

(define-rich-text-dwim highlight "vv"
  :light-fn #'rich-text-highlight-light-props
  :dark-fn #'rich-text-highlight-dark-props)

(provide 'rich-text)

;;; rich-text.el ends here
