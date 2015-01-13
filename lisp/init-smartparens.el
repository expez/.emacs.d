(require-package 'smartparens)
(require 'smartparens-config)
(require 'evil-smartparens)
(smartparens-global-strict-mode 1)
(show-smartparens-global-mode 1)

(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil)

(defun my-smartparens-mode-hook ()
  (eval-after-load 'evil
    (progn (evil-smartparens-mode 1)
           (evil-define-key 'normal sp-keymap
             (kbd "C-t") 'sp-transpose-sexp
             "[" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "["))
             "{" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{"))
             "(" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
           (run-with-timer 2 nil
                           (lambda ()
                             (fill-keymap evil-normal-state-local-map
                                          "H" 'sp-backward-up-sexp
                                          "L" 'sp-up-sexp
                                          "M-O" 'sp-up-sexp
                                          "C-9" 'sp-backward-barf-sexp
                                          "C-0" 'sp-forward-barf-sexp
                                          "M-9" 'sp-backward-slurp-sexp
                                          "M-0" 'sp-forward-slurp-sexp))))))
(add-hook 'smartparens-enabled-hook #'my-smartparens-mode-hook)

(fill-keymap sp-keymap
             "M-;" 'paredit-comment-dwim
             "M-s" 'sp-splice-sexp
             "M-S" 'sp-split-sexp
             "M-j" 'sp-join-sexp

             "M-o" 'sp-down-sexp
             "M-u" 'sp-backward-down-sexp

             "M-l" 'sp-forward-sexp
             "M-h" 'sp-backward-sexp
             "M-k" 'sp-splice-sexp-killing-backward-or-around
             "M-K" 'sp-splice-sexp-killing-forward
             "M-c" 'sp-convolute-sexp)

(defun sp-splice-sexp-killing-backward-or-around (&optional arg)
  (interactive "P")
  (if current-prefix-arg
      (sp-splice-sexp-killing-around 1)
    (sp-splice-sexp-killing-backward 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-sp-mode)
(defvar sp-minibuffer-commands '(eval-expression
                                 pp-eval-expression
                                 eval-expression-with-eldoc
                                 ibuffer-do-eval
                                 ibuffer-do-view-and-eval)
  "Interactive commands for which sp should be enabled in the minibuffer.")

(defun conditionally-enable-sp-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (when (memq this-command paredit-minibuffer-commands)
    (smartparens-strict-mode)))

(provide 'init-smartparens)
