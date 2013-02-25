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

(defun paredit-barf-all-the-way-backward ()
  (interactive)
  (paredit-split-sexp)
  (paredit-backward-down)
  (paredit-splice-sexp))

(defun paredit-barf-all-the-way-forward ()
  (interactive)
  (paredit-split-sexp)
  (paredit-forward-down)
  (paredit-splice-sexp)
  (if (eolp) (delete-horizontal-space)))

(defun paredit-slurp-all-the-way-backward ()
  (interactive)
  (catch 'done
    (while (not (bobp))
      (save-excursion
        (paredit-backward-up)
        (if (eq (char-before) ?\()
            (throw 'done t)))
      (paredit-backward-slurp-sexp))))

(defun paredit-slurp-all-the-way-forward ()
  (interactive)
  (catch 'done
    (while (not (eobp))
      (save-excursion
        (paredit-forward-up)
        (if (eq (char-after) ?\))
            (throw 'done t)))
      (paredit-forward-slurp-sexp))))

(defun add-new-paredit-commands ()
  "Adds keybindings and documentation for new paredit commands"
  (nconc paredit-commands
       '("Extreme Barfage & Slurpage"
         (("C-M-)")
          paredit-slurp-all-the-way-forward
          ("(foo (bar |baz) quux zot)"
           "(foo (bar |baz quux zot))")
          ("(a b ((c| d)) e f)"
           "(a b ((c| d)) e f)"))
         (("C-M-}" "M-F")
          paredit-barf-all-the-way-forward
          ("(foo (bar |baz quux) zot)"
           "(foo (bar|) baz quux zot)"))
         (("C-M-(")
          paredit-slurp-all-the-way-backward
          ("(foo bar (baz| quux) zot)"
           "((foo bar baz| quux) zot)")
          ("(a b ((c| d)) e f)"
           "(a b ((c| d)) e f)"))
         (("C-M-{" "M-B")
          paredit-barf-all-the-way-backward
          ("(foo (bar baz |quux) zot)"
           "(foo bar baz (|quux) zot)"))))
  (paredit-define-keys)
  (paredit-annotate-mode-with-examples)
  (paredit-annotate-functions-with-examples))
(eval-after-load "paredit" '(add-new-paredit-commands))

(provide 'init-paredit)
