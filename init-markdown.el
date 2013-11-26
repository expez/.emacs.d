(require-package 'markdown-mode)

(add-auto-mode 'markdown-mode "\\.md\\'")

(fill-keymap markdown-mode-map
               "C-c C-f" 'nil
               "C-c C-f C-b" 'markdown-insert-bold
               "C-c C-f C-<tab>" 'markdown-insert-italic
               "C-c C-f C-l" 'markdown-insert-link)

(add-lambda 'markdown-mode-hook
  (visual-line-mode 1))

(provide 'init-markdown)
