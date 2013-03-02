(require 'markdown-mode)

(fill-keymap markdown-mode-map
             "C-c C-f C-l" 'markdown-insert-link)

(add-auto-mode 'markdown-mode "\\.md\\'")

(provide 'init-markdown)
