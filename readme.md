# Rich-Text
Rich-Text provides rich text styling and persistence.

This project is an upgrade of [rich-text](https://github.com/Kinneyzhang/rich-text), tested only on version 31.

## Features

### Rich Style Support
- Highlight markers (customizable), with default yellow/green/blue/red colors
- Custom fonts (scalable)
- Multiple styles can be stacked

### Unified Style Management
- Jump/preview/search/delete capabilities

## Configuration and Dependencies
```elisp
(use-package emacsql
  :straight (:host github :repo "magit/emacsql")
  :defer nil)
(use-package emacsql-sqlite-builtin
  :straight (:host github :repo "magit/emacsql"
			 :files ("sqlite-builtin/*.el"))
  :after emacsql)
(use-package selected
  :straight (:host github :repo "Kungsgeten/selected.el"))
(use-package ov
  :straight (:host github :repo "emacsorphanage/ov"))
```

```elisp
(add-to-list 'load-path "xxx/rich-text")
;; GUI
(setq rich-text-highlight-light-bg "#36648B")
(setq rich-text-highlight-light-fg "#FFFFFF")
(setq rich-text-highlight-dark-bg "#0000EE")
(setq rich-text-highlight-dark-fg "#FFFFFF")
;; Terminal
(setq rich-text-highlight-light-bg-term "cyan")
(setq rich-text-highlight-light-fg-term "white")
(setq rich-text-highlight-dark-bg-term "blue")
(setq rich-text-highlight-dark-fg-term "white")
(setq rich-text-selected-ignore-modes '(prog-mode))
(require 'rich-text)
(require 'rich-text-plus)
(require 'rich-text-list-tabulated)
(rich-text-mode 1)
```

## Basic Operations

### Highlight/Bold/Italic/Underline etc.
- `rich-text-render-highlight-dwim`
- `rich-text-render-bold`
- `rich-text-render-itaic`
- `rich-text-render-underline-line`
- ...

### Font Selection/Zoom
- `rich-text-set-font-overlay`
- `rich-text-increase-font-size`
- `rich-text-decrease-font-size`

### Clear Styles
- `rich-text-clear-at-point`
- `rich-text-clear-buffer`
- `rich-text-clear-file`

### Enter List View
- `rich-text-list-tabulated`

## List Table Keybindings

| Command                          | Key | Function                  |
|----------------------------------|-----|---------------------------|
| rich-text-tabulated-jump         | RET | Jump to style position    |
| rich-text-tabulated-delete-point | d   | Delete style              |
| rich-text-tabulated-other-window | o   | Preview                   |
| rich-text-tabulated-filter       | /   | Search and filter         |
| rich-text-list-tabulated         | L   | Switch back to full list  |
|                                  | q   | Close list                |
