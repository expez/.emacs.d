(require 'markdown-mode)

(fill-keymap markdown-mode-map
             "C-c C-f C-l" 'markdown-insert-link)

(add-auto-mode 'markdown-mode "\\.md\\'")

(add-lambda 'markdown-mode-hook
  (visual-line-mode 1))

(provide 'init-markdown)
