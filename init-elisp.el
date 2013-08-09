(require 'parenface)
(require 'pretty-mode-plus)
(require 'elisp-slime-nav)
(require 'evil-paredit)
;(require 'highlight-cl)

(set-face-foreground 'paren-face "grey30")

;(add-hook 'lisp-interaction-mode-hook 'highlight-cl-add-font-lock-keywords)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (elisp-slime-nav-mode 1)
            (turn-on-redshank-mode)
            (flycheck-mode 0)
          ; (highlight-cl-add-font-lock-keywords)
            (esk-remove-elc-on-save)
            (checkdoc-minor-mode)
            (local-set-key (kbd "RET") 'newline-and-indent)
            (ac-emacs-lisp-mode-setup)
            (define-key evil-normal-state-local-map (kbd "M-.")
              'elisp-slime-nav-find-elisp-thing-at-point)))

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(let ((elisp-programming-major-modes '(emacs-lisp-mode lisp-interaction-mode
                                                      ielm-mode)))
  (dolist (mode elisp-programming-major-modes)
          (add-hook
           (intern (concat (symbol-name mode) "-hook"))
           (lambda ()
             (turn-on-eldoc-mode)
             (paredit-mode 1)
             (evil-paredit-mode 1)
             (rainbow-delimiters-mode 0)))))

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(provide 'init-lisp)
