(require-package 'google-c-style)
(require-package 'ctypes)
(require-package 'c-eldoc)

(add-auto-mode 'fundamental-mode "\\.lex")

(defun my-c-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent)
  (c-turn-on-eldoc-mode))

(add-hook 'c-mode-hook #'my-c-mode-hook)

(setq ctypes-write-types-at-exit t)
(ctypes-read-file nil nil t t)
(ctypes-auto-parse-mode 1)

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(provide 'init-c-mode)
