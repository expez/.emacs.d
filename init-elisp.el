(require 'parenface)
(require 'pretty-mode-plus)
(require 'elisp-slime-nav)

(set-face-foreground 'paren-face "grey30")

(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-mode 0)
            (elisp-slime-nav-mode 1)
            (checkdoc-minor-mode)
            (ac-emacs-lisp-mode-setup)
            (define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)))

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

(defcustom elisp-programming-major-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    ielm-mode)
  "Mode that are used to do Elisp programming.")

(dolist (mode elisp-programming-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   (lambda ()
     (turn-on-eldoc-mode)
     (paredit-mode 1)
     (evil-paredit-mode 1)
     (rainbow-delimiters-mode 0))))

(provide 'init-lisp)
