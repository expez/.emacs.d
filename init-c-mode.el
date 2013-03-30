(require 'google-c-style)
(require 'ctypes)

(add-lambda 'c-mode-hook
  (google-set-c-style)
  (google-make-newline-indent)
  (c-turn-on-eldoc-mode)
  (ac-c-mode-setup))

(defun ac-c-mode-setup ()
  (setq clang-complete-executable (concat user-emacs-directory
                                          "vendor/emacs-clang-complete-async/clang-complete"))
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(setq ctypes-write-types-at-exit t)
(ctypes-read-file nil nil t t)
(ctypes-auto-parse-mode 1)

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(provide 'init-c-mode)
