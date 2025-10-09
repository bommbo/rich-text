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

;;; underline

;; (defvar rich-text-underline-color "black"
;;   "Default color of rich-text underline face.")

;; (defvar rich-text-underline-light-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
;;   "Preset a list of rich-text underline colors in light themes.")

;; (defvar rich-text-underline-dark-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
;;   "Preset a list of rich-text underline colors in dark themes.")

;;; font color

(defvar rich-text-fontcolor-light-color "#BA36A5"
  "Default color of rich-text font color in light theme.")

(defvar rich-text-fontcolor-dark-color "#FFCB6B"
  "Default color of rich-text font color in dark theme.")

;; (defvar rich-text-font-light-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
;;   "Preset a list of rich-text font colors in light themes.")

;; (defvar rich-text-font-dark-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
;;   "Preset a list of rich-text font colors in dark themes.")

;;; highlight - 改进：支持终端和 GUI

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

;; 预设多种颜色方案（可选）
(defvar rich-text-highlight-color-schemes
  '((scheme-1 
     :light-bg "#FFEAA7" :light-fg "#000000"
     :dark-bg "#4B5263" :dark-fg "#E5C07B"
     :light-term-bg "yellow" :light-term-fg "black"
     :dark-term-bg "blue" :dark-term-fg "yellow")
    (scheme-2
     :light-bg "#A8E6CF" :light-fg "#000000"
     :dark-bg "#2C5F2D" :dark-fg "#98C379"
     :light-term-bg "green" :light-term-fg "black"
     :dark-term-bg "green" :dark-term-fg "white")
    (scheme-3
     :light-bg "#FFD3B6" :light-fg "#000000"
     :dark-bg "#5C4742" :dark-fg "#D19A66"
     :light-term-bg "cyan" :light-term-fg "black"
     :dark-term-bg "cyan" :dark-term-fg "white"))
  "Predefined color schemes for highlights.")

(defvar rich-text-current-color-scheme 'scheme-1
  "Current color scheme for highlights.")

;; (defvar rich-text-highlight-light-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
;;   "Preset a list of rich-text highlight colors in light themes.")

;; (defvar rich-text-highlight-dark-colors '("#F44336" "#009688" "#FF9800" "#00BCD4")
;;   "Preset a list of rich-text highlight colors in dark themes.")

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

(defvar rich-text-alist nil)

;;;; Helper functions

(defun rich-text-display-graphic-p ()
  "Check if current display supports graphic (GUI)."
  (display-graphic-p))

(defun rich-text-get-color (light-gui dark-gui light-term dark-term)
  "Get appropriate color based on theme and display type.
Return GUI color for graphic display, terminal color otherwise."
  (if (rich-text-display-graphic-p)
      (if (rich-text-theme-light-p) light-gui dark-gui)
    (if (rich-text-theme-light-p) light-term dark-term)))

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

(cl-defmacro define-rich-text (name key props)
  "Define a rich-text style with NAME, triggered by KEY, applying PROPS.
NAME should be an unquoted symbol."
  (declare (indent defun))
  ;; 确保 name 是符号
  (unless (symbolp name)
    (error "define-rich-text: NAME must be a symbol, got %S" name))
  (let* ((name-str (symbol-name name))
         (func-prefix "rich-text-render-")
         (render-func (intern (concat func-prefix name-str))))
    `(progn
       (defun ,render-func ()
         (interactive)
         (rich-text-ov-set-dwim ',name ,props))
       (unset-command-keys selected-keymap ',render-func)
       (define-key selected-keymap ,key #',render-func))))

(cl-defmacro define-rich-text-dwim (name key &key props light dark)
  "Define a theme-aware rich-text style with NAME, triggered by KEY.
NAME should be an unquoted symbol.
PROPS is the default properties.
LIGHT and DARK are theme-specific properties."
  (declare (indent defun))
  ;; 确保 name 是符号
  (unless (symbolp name)
    (error "define-rich-text-dwim: NAME must be a symbol, got %S" name))
  (let* ((name-str (symbol-name name))
         (func-prefix "rich-text-render-")
         (render-func (intern (concat func-prefix name-str "-dwim"))))
    ;; 在宏展开时（编译时）更新 alist
    `(progn
       ;; 在运行时更新 alist
       (let ((lst (assoc ',name rich-text-alist)))
         (when lst
           (setq rich-text-alist (remove lst rich-text-alist))))
       (add-to-list 'rich-text-alist
                    (list ',name :key ,key :props ,props
                          :light ,light :dark ,dark))
       (defun ,render-func ()
         (interactive)
         (let (theme-props)
           (when (rich-text-theme-light-p)
             (setq theme-props (or ,light ,props)))
           (when (rich-text-theme-dark-p)
             (setq theme-props (or ,dark ,props)))
           (if theme-props
               (progn
                 ;; at least one of light and dark props
                 (rich-text-ov-set-dwim ',name theme-props t))
             ;; no light or dark props
             (if ,props
                 (rich-text-ov-set-dwim ',name ,props)
               (pop-mark)
               (message "No rich-text to render.")))))
       (unset-command-keys selected-keymap ',render-func)
       (define-key selected-keymap ,key #',render-func))))

;;;; Rich-text face properties

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

;; 改进：支持终端和 GUI 的高亮颜色
(defun rich-text-highlight-light-props ()
  "Get highlight properties for LIGHT theme.
Returns GUI colors if in graphic display, terminal colors otherwise."
  (if (rich-text-display-graphic-p)
      ;; GUI mode - 浅色主题
      `(face (:background ,rich-text-highlight-light-bg 
                          :foreground ,rich-text-highlight-light-fg 
                          :weight bold))
    ;; Terminal mode - 浅色主题
    `(face (:background ,rich-text-highlight-light-bg-term 
                        :foreground ,rich-text-highlight-light-fg-term 
                        :weight bold))))

(defun rich-text-highlight-dark-props ()
  "Get highlight properties for DARK theme.
Returns GUI colors if in graphic display, terminal colors otherwise."
  (if (rich-text-display-graphic-p)
      ;; GUI mode - 深色主题
      `(face (:background ,rich-text-highlight-dark-bg 
                          :foreground ,rich-text-highlight-dark-fg 
                          :weight bold))
    ;; Terminal mode - 深色主题
    `(face (:background ,rich-text-highlight-dark-bg-term 
                        :foreground ,rich-text-highlight-dark-fg-term 
                        :weight bold))))

;;;; Specific for light/dark themes

(defun rich-text-theme-dark-p ()
  "Check if current theme is dark.
Handles cases where background is unspecified in terminal.
Respects `rich-text-force-theme-mode' if set."
  (cond
   ;; 强制模式优先
   ((eq rich-text-force-theme-mode 'dark) t)
   ((eq rich-text-force-theme-mode 'light) nil)
   ;; 正常检测
   ((eq (frame-parameter nil 'background-mode) 'dark) t)
   ;; 如果无法检测，检查主题名称
   ((and (not (display-graphic-p))
         (boundp 'custom-enabled-themes)
         (seq-some (lambda (theme)
                     (string-match-p "dark\\|night\\|gruvbox-dark" 
                                     (symbol-name theme)))
                   custom-enabled-themes))
    t)
   (t nil)))

(defun rich-text-theme-light-p ()
  "Check if current theme is light.
Handles cases where background is unspecified in terminal.
Respects `rich-text-force-theme-mode' if set."
  (cond
   ;; 强制模式优先
   ((eq rich-text-force-theme-mode 'light) t)
   ((eq rich-text-force-theme-mode 'dark) nil)
   ;; 正常检测
   ((eq (frame-parameter nil 'background-mode) 'light) t)
   ;; 如果无法检测，检查主题名称
   ((and (not (display-graphic-p))
         (boundp 'custom-enabled-themes)
         (seq-some (lambda (theme)
                     (string-match-p "light\\|day\\|gruvbox-light" 
                                     (symbol-name theme)))
                   custom-enabled-themes))
    t)
   (t nil)))

;; (defun rich-text-underline-colors-by-theme ()
;;   "Return a list of underline colors according to the type of theme."
;;   (cond
;;    ((rich-text-theme-light-p) rich-text-underline-light-colors)
;;    ((rich-text-theme-dark-p) rich-text-underline-dark-colors)))

;; (defun rich-text-font-colors-by-theme ()
;;   "Return a list of font colors according to the type of theme."
;;   (cond
;;    ((rich-text-theme-light-p) rich-text-font-light-colors)
;;    ((rich-text-theme-dark-p) rich-text-font-dark-colors)))

;; (defun rich-text-highlight-colors-by-theme ()
;;   "Return a list of highlight colors according to the type of theme."
;;   (cond
;;    ((rich-text-theme-light-p) rich-text-highlight-light-colors)
;;    ((rich-text-theme-dark-p) rich-text-highlight-dark-colors)))

;; (defun rich-text-propertize-colors-by-theme (face-type)
;;   (pcase face-type
;;     ('underline
;;      (mapcar (lambda (color)
;;                (propertize color 'face `(:underline (:color ,color))))
;;              (rich-text-underline-colors-by-theme)))
;;     ('fontcolor
;;      (mapcar (lambda (color)
;;                (propertize color 'face `(:foreground ,color)))
;;              (rich-text-font-colors-by-theme)))
;;     ('highlight
;;      (mapcar (lambda (color)
;;                (propertize color 'face `(:background ,color)))
;;              (rich-text-highlight-colors-by-theme)))
;;     (_ (rich-text-font-colors-by-theme))))

;;;; Rich-text render functions

(defun rich-text-ov-set-dwim (name props &optional reverse-p)
  (let (ov)
    (if (use-region-p)
        (setq ov (ov-set (ov-region) props))
      (setq ov (ov-set (ov-line) props)))
    (setq ov (ov-set ov 'reverse-p reverse-p))
    (setq ov (ov-set ov '(evaporate t)))
    ;; indicate a overlay set by rich text.
    (setq ov (ov-set ov `(rich-text ,name)))
    ;; ✓ 只存储样式名称，不存储具体的颜色值
    (rich-text-store-curr-ov-name name (car (ov-spec ov)))))

(defun rich-text-get-props (rich-text-name)
  (let ((plist (cdr (assoc rich-text-name rich-text-alist))))
    (cond
     ((rich-text-theme-light-p) (or (plist-get plist :light)
                                    (plist-get plist :props)))
     ((rich-text-theme-dark-p) (or (plist-get plist :dark)
                                   (plist-get plist :props))))))

(defun rich-text-buffer-reverse-p (buffer)
  (with-current-buffer buffer
    (ov-in 'reverse-p t)))

(defun rich-text-reverse ()
  (when-let ((ovs (ov-in 'reverse-p t)))
    (mapcar (lambda (ov)
              (let ((name (ov-val ov 'rich-text)))
                (ov-set ov (rich-text-get-props name))))
            ovs)))

(defun rich-text-reverse-all (&rest _)
  (save-window-excursion
    (mapcar (lambda (win)
              (let ((buffer (window-buffer win)))
                (when (rich-text-buffer-reverse-p buffer)
                  (with-current-buffer buffer
                    (rich-text-reverse)))))
            (window-list))))

(defun rich-text-change-theme-background (orig-fun &rest args)
  "Advice functon when load a theme."
  (let ((before-bg (frame-parameter nil 'background-mode))
        after-bg)
    (apply orig-fun args)
    (setq after-bg (frame-parameter nil 'background-mode))
    (unless (eq before-bg after-bg)
      (run-hooks 'rich-text-theme-background-change-hook))))

;;;; Rich text ov store and restore functons

(defun rich-text-buffer-or-file-id (&optional buffer-or-name)
  "FIXME: repalce with unique id of file or buffer in system."
  (let ((buffer-or-name (or buffer-or-name (current-buffer))))
    (buffer-file-name (get-buffer buffer-or-name))))

(defun rich-text-all-id ()
  (when-let ((id (rich-text-buffer-or-file-id (current-buffer))))
    (mapcar #'car (rich-text-db-distinct-query 'ov [id]))))

(defun rich-text-buffer-stored-p (id)
  (cl-member id (rich-text-all-id) :test 'string=))

;; ✓ 新函数：只存储样式名称（支持 UPDATE）
(defun rich-text-store-curr-ov-name (style-name ov-spec)
  "Store overlay with STYLE-NAME instead of actual props.
This ensures cross-environment compatibility.
If record exists, UPDATE it; otherwise INSERT."
  (when-let* ((beg (nth 0 ov-spec))
              (end (nth 1 ov-spec))
              (id (rich-text-buffer-or-file-id (nth 2 ov-spec))))
    ;; 查询是否已存在相同位置的记录
    (let ((old (car (rich-text-db-crud
                     `[:select rowid :from ov
                       :where (and (= id ,id) (= beg ,beg) (= end ,end))]))))
      (if old
          ;; 更新已有记录
          (rich-text-db-crud
           `[:update ov :set (= props ,(prin1-to-string style-name))
             :where (= rowid ,(car old))])
        ;; 插入新记录
        (rich-text-db-insert
         'ov `([,id ,beg ,end ,(prin1-to-string style-name)]))))))

;; ✓ 新函数：精准删除单个 overlay 的 DB 记录
(defun rich-text-delete-ov-from-db (id beg end)
  "Delete the single ov record that matches ID BEG END.
This ensures only the specific overlay is removed from database."
  (rich-text-db-crud
   `[:delete :from ov
     :where (and (= id ,id) (= beg ,beg) (= end ,end))]))

;; 保留旧函数用于兼容性
(defun rich-text-store-curr-ov (ov-spec)
  (when-let* ((beg (nth 0 ov-spec))
              (end (nth 1 ov-spec))
              (id (rich-text-buffer-or-file-id (nth 2 ov-spec)))
              (props (nth 3 ov-spec)))
    (rich-text-db-insert 'ov `([,id ,beg ,end ,props]))))

(defun rich-text-buffer-ov-specs ()
  "A list of overlay spec set by `rich-text-mode',
 exclude other already exist overlays."
  (seq-filter (lambda (spec)
                (cl-member 'rich-text (nth 3 spec) :test 'eq))
              (ov-spec (ov-in))))

(defun rich-text-store-buffer-ov ()
  "Store current buffer's overlays to database.
Stores style names instead of actual props for cross-environment compatibility."
  (let ((count (rich-text-db-query-count 'ov `(= id ,(rich-text-buffer-or-file-id)))))
    (when (not (= count 0))
      (rich-text-db-delete 'ov `(= id ,(rich-text-buffer-or-file-id))))
    (when-let ((ovs (ov-in 'rich-text)))
      (dolist (ov ovs)
        (let* ((style-name (ov-val ov 'rich-text))
               (beg (overlay-start ov))
               (end (overlay-end ov))
               (id (rich-text-buffer-or-file-id)))
          (when (and style-name beg end id)
            (rich-text-db-insert 'ov 
              `([,id ,beg ,end ,(prin1-to-string style-name)]))))))))

;;;###autoload
(defun rich-text-restore-buffer-ov ()
  "Restore overlays from database, applying current environment's colors."
  (interactive)
  (when-let* ((id (rich-text-buffer-or-file-id))
              (_ (rich-text-buffer-stored-p id))
              (specs (rich-text-db-query 'ov [beg end props]
                                         `(= id ,id))))
    (dolist (spec specs)
      (let* ((beg (nth 0 spec))
             (end (nth 1 spec))
             (stored-data (nth 2 spec))
             style-name
             props)
        ;; 尝试解析存储的数据
        (condition-case nil
            (setq style-name (read stored-data))
          (error nil))
        
        ;; 如果是符号（样式名），根据当前环境生成 props
        (if (symbolp style-name)
            (progn
              (setq props (rich-text-get-props style-name))
              (when props
                (let ((ov (ov beg end props)))
                  (ov-set ov 'rich-text style-name)
                  (ov-set ov 'reverse-p t)
                  (ov-set ov 'evaporate t))))
          ;; 否则使用旧格式（直接存储的 props）
          (setq props stored-data)
          (ov beg end props))))
    (message "%s rich-text overlays restored!" (length specs))
    (rich-text-reverse-all)))

;;;; 新增：清除样式命令（同步到数据库 - 精准删除版本）

(defun rich-text-sync-clear-to-db ()
  "Sync current buffer's overlay state to database after clearing.
This ensures database matches the current buffer state."
  (when-let ((id (rich-text-buffer-or-file-id)))
    ;; 删除当前文件的所有 DB 记录
    (rich-text-db-delete 'ov `(= id ,id))
    ;; 重新保存当前剩余的 overlays
    (when-let ((specs (rich-text-buffer-ov-specs)))
      (dolist (spec specs)
        (rich-text-store-curr-ov spec)))))

;;;###autoload
(defun rich-text-clear-region (beg end &optional no-sync)
  "Clear all rich-text overlays in region from BEG to END.
If NO-SYNC is non-nil, don't sync to database.
✓ 精准删除：只删除被清除的 overlay 对应的 DB 记录。"
  (interactive "r")
  (let ((id (rich-text-buffer-or-file-id))
        (count 0))
    (dolist (ov (overlays-in beg end))
      (when (overlay-get ov 'rich-text)
        (let ((ov-beg (overlay-start ov))
              (ov-end (overlay-end ov)))
          (delete-overlay ov)
          (setq count (1+ count))
          (unless no-sync
            (rich-text-delete-ov-from-db id ov-beg ov-end)))))
    (message "Cleared %d rich-text overlay%s in region%s" 
             count 
             (if (= count 1) "" "s")
             (if no-sync "" " (synced to DB)"))))

;;;###autoload
(defun rich-text-clear-buffer (&optional no-sync)
  "Clear all rich-text overlays in current buffer.
If NO-SYNC is non-nil, don't sync to database."
  (interactive)
  (let ((count 0)
        (id (rich-text-buffer-or-file-id)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'rich-text)
        (delete-overlay ov)
        (setq count (1+ count))))
    (unless no-sync
      ;; 清空当前文件在 DB 中的所有记录
      (when id
        (rich-text-db-delete 'ov `(= id ,id))))
    (message "Cleared %d rich-text overlay%s in buffer%s" 
             count 
             (if (= count 1) "" "s")
             (if no-sync "" " (synced to DB)"))))

;;;###autoload
(defun rich-text-clear-at-point (&optional no-sync)
  "Clear rich-text overlay at point.
If NO-SYNC is non-nil, don't sync to database.
✓ 精准删除：只删除光标处的 overlay 对应的 DB 记录。"
  (interactive)
  (let ((cleared nil)
        (id (rich-text-buffer-or-file-id)))
    (dolist (ov (overlays-at (point)))
      (when (overlay-get ov 'rich-text)
        (let ((beg (overlay-start ov))
              (end (overlay-end ov)))
          (delete-overlay ov)
          (setq cleared t)
          (unless no-sync
            (rich-text-delete-ov-from-db id beg end)))))
    (if cleared
        (message "Cleared rich-text overlay at point%s"
                 (if no-sync "" " (synced to DB)"))
      (message "No rich-text overlay at point"))))

;;;###autoload
(defun rich-text-clear-all-buffers (&optional no-sync)
  "Clear all rich-text overlays in all buffers.
If NO-SYNC is non-nil, don't sync to database."
  (interactive)
  (let ((total-count 0)
        (file-ids '()))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (let ((count 0)
              (id (rich-text-buffer-or-file-id)))
          (dolist (ov (overlays-in (point-min) (point-max)))
            (when (overlay-get ov 'rich-text)
              (delete-overlay ov)
              (setq count (1+ count))))
          (setq total-count (+ total-count count))
          (when (and id (> count 0))
            (push id file-ids)))))
    (unless no-sync
      ;; 删除所有清除了 overlay 的文件的 DB 记录
      (dolist (id file-ids)
        (rich-text-db-delete 'ov `(= id ,id))))
    (message "Cleared %d rich-text overlay%s in all buffers%s" 
             total-count 
             (if (= total-count 1) "" "s")
             (if no-sync "" " (synced to DB)"))))

;;;###autoload
(defun rich-text-list-overlays ()
  "List all rich-text overlays in current buffer."
  (interactive)
  (let ((overlays '()))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'rich-text)
        (push (list :start (overlay-start ov)
                    :end (overlay-end ov)
                    :type (overlay-get ov 'rich-text)
                    :text (buffer-substring (overlay-start ov) (overlay-end ov)))
              overlays)))
    (if overlays
        (progn
          (with-current-buffer (get-buffer-create "*Rich-Text Overlays*")
            (erase-buffer)
            (insert (format "Rich-text overlays in buffer: %s\n\n" (buffer-name)))
            (dolist (ov (reverse overlays))
              (insert (format "Type: %s\nRange: %d-%d\nText: %s\n\n"
                              (plist-get ov :type)
                              (plist-get ov :start)
                              (plist-get ov :end)
                              (plist-get ov :text))))
            (goto-char (point-min)))
          (display-buffer "*Rich-Text Overlays*")
          (message "Found %d rich-text overlay%s" 
                   (length overlays) 
                   (if (= (length overlays) 1) "" "s")))
      (message "No rich-text overlays in buffer"))))

;;;; 新增：颜色自定义命令

;;;###autoload
(defun rich-text-set-highlight-colors (light-bg light-fg dark-bg dark-fg)
  "Set custom highlight colors for GUI display.
LIGHT-BG and LIGHT-FG for light theme.
DARK-BG and DARK-FG for dark theme."
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
  "Set custom highlight colors for terminal display.
LIGHT-BG and LIGHT-FG for light theme.
DARK-BG and DARK-FG for dark theme.
Use terminal color names like: black, red, green, yellow, blue, magenta, cyan, white."
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
  "Refresh all rich-text overlays in current buffer with current color settings."
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
        ;; when change the background of emacs theme, reverse the ov
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

;;;; some build-in rich-text formats.

(define-rich-text headline-1 "h1"
  (rich-text-headline-1-props))

(define-rich-text headline-2 "h2"
  (rich-text-headline-2-props))

(define-rich-text headline-3 "h3"
  (rich-text-headline-3-props))

(define-rich-text bold "bb"
  (rich-text-bold-props))

(define-rich-text italic "ii"
  (rich-text-italic-props))

(define-rich-text underline-line "ul"
  (rich-text-underline-line-props))

(define-rich-text underline-wave "uw"
  (rich-text-underline-wave-props))

(define-rich-text-dwim fontcolor "cc"
  :light (rich-text-fontcolor-light-props)
  :dark (rich-text-fontcolor-dark-props))

(define-rich-text-dwim highlight "vv"
  :light (rich-text-highlight-light-props)
  :dark (rich-text-highlight-dark-props))

(provide 'rich-text)

;;; rich-text.el ends here
