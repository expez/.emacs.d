(require-package 'smartparens)
(require 'smartparens-config)
(require 'evil-smartparens)
(smartparens-global-strict-mode 1)
(show-smartparens-global-mode 1)

(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-navigate-consider-stringlike-sexp
      (concatenate 'list sp-navigate-consider-stringlike-sexp
                   '(prog-mode nxml-mode sgml-mode)))

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

(sp-with-modes '(markdown-mode
                 rst-mode)
  (sp-local-pair "`" "`"))

(fill-keymap sp-keymap
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

(provide 'init-smartparens)
