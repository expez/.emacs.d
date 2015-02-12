(require-package 'smartparens)
(require 'smartparens-config)
(require 'evil-smartparens)

(show-smartparens-global-mode 1)

(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil)

(defun turn-on-sp-navigate-consider-stringlike ()
  (unless (memq major-mode sp-navigate-consider-stringlike-sexp)
    (add-to-list 'sp-navigate-consider-stringlike-sexp major-mode)))

(defun my-smartparens-mode-hook ()
  (turn-on-sp-navigate-consider-stringlike)
  (evil-smartparens-mode 1)
  (evil-define-key 'normal sp-keymap
    (kbd "C-t") 'sp-transpose-sexp
    "[" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "["))
    "{" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "{"))
    "(" (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "(")))
  (fill-keymap evil-normal-state-local-map
               "H" 'sp-backward-up-sexp
               "L" 'sp-up-sexp
               "C-9" 'sp-backward-barf-sexp
               "C-0" 'sp-forward-barf-sexp
               "M-9" 'sp-backward-slurp-sexp
               "M-0" 'sp-forward-slurp-sexp))
;; When this hook was set on smartparens `evil-normal-state-local-map'
;; would sometimes be nil, causing a most annoying race-condition.
(add-hook 'smartparens-mode-hook #'my-smartparens-mode-hook)

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

(cl-defun sp--cleanup-killed-funcall (count &rest args)
  "`sp-splice-sexp-killing-backward' on 'foo(|bar)' => 'bar'."
  (save-excursion
    (dotimes (_ count)
      (sp-backward-up-sexp)
      (let ((sexp-start (point)))
        (ignore-errors
          (while (looking-back "\\w\\|\\\.")
            (backward-char))
          (if (looking-at "\\(\\w\\|\\\.\\)+(")
              (delete-region (point) sexp-start)
            (cl-return)))))))

(advice-add 'sp-splice-sexp-killing-backward :before #'sp--cleanup-killed-funcall)

(provide 'init-smartparens)
