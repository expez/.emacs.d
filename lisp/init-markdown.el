(require-package 'markdown-mode)
(require-package 'visual-fill-column)
(require 'markdown-mode)

(add-auto-mode 'markdown-mode "\\.md\\'")
(add-auto-mode 'gfm-mode "README\\.md\\'")

(global-visual-fill-column-mode)

(fill-keymap markdown-mode-map
             "C-c C-f" 'nil
             "C-c C-f C-b" 'markdown-insert-bold
             "C-c C-f C-<tab>" 'markdown-insert-italic
             "C-c C-f C-l" 'markdown-insert-link)

(defun my-markdown-mode-hook ()
  (change-whitespace-line-column 250)
  (visual-line-mode 1))

(add-hook 'markdown-mode-hook #'my-markdown-mode-hook)

(provide 'init-markdown)
