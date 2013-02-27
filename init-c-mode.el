(require 'google-c-style)
(require 'ctypes)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(defun ac-c-mode-setup ()
  (setq clang-complete-executable (concat user-emacs-directory "clang-compete"))
  (setq ac-sources '(ac-source-clang-async))
  (launch-completion-proc))

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(setq ctypes-write-types-at-exit t)
(ctypes-read-file nil nil t t)
(ctypes-auto-parse-mode 1)

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(provide 'init-c-mode)
