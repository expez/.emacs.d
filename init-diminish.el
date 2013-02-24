(require 'diminish)
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "eproject"
  '(diminish 'eproject-mode))

(eval-after-load "yas-minor-mode"
  '(diminish 'yas-minor-mode))

(eval-after-load "paredit"
  '(diminish 'paredit-mode))

(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode))

(provide 'init-diminish)
