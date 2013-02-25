(require 'paredit-ext)
(require 'paredit)

;; Use paredit in the minibuffer
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(eval-after-load "evil"
  (lambda ()
    (evil-define-key 'normal paredit-mode-map
       "M-l" 'paredit-forward-slurp-sexp
       "M-h" 'paredit-backward-slurp-sexp
       "M-H" 'paredit-backward-barf-sexp
       "M-L" 'paredit-forward-barf-sexp
       "M-s" 'paredit-splice-sexp
       "M-S" 'paredit-split-sexp
       "M-j" 'paredit-join-sexps
       "M-k" 'paredit-kill
       "(" 'paredit-backward
       ")" 'paredit-forward)))

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

(provide 'init-paredit)
