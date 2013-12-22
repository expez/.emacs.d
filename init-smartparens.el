(require-package 'smartparens)
(require 'smartparens)
(smartparens-global-strict-mode)

(sp-pair "'" nil :unless '(sp-point-after-word-p))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p)))

;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
;; mode) as a first argument. The macro `sp-with-modes' adds this
;; automatically. If you want to call sp-local-pair outside this
;; macro, you MUST supply the major mode argument.

(--each sp--html-modes
  (eval-after-load (symbol-name it) '(require 'smartparens-html)))
(eval-after-load "latex" '(require 'smartparens-latex))
(eval-after-load "tex-mode" '(require 'smartparens-latex))
(eval-after-load "lua-mode" '(require 'smartparens-lua))
(eval-after-load "ruby-mode" '(require 'smartparens-ruby))
(eval-after-load "enh-ruby-mode" '(require 'smartparens-ruby))

(eval-after-load "evil"
  '(evil-define-key 'normal sp-keymap
     (kbd "C-t") 'sp-transpose-sexp
     "M-i" 'sp-splice-sexp-killing-forward
     "M-;" 'sp-comment
     "(" 'sp-wrap-round))

(fill-keymap sp-keymap
             "M-s" 'sp-splice-sexp
             "M-S" 'sp-split-sexp
             "M-j" 'sp-join-sexp

             "M-l" 'sp-next-sexp
             "M-h" 'sp-previous-sexp
             "M-k" 'sp-kill-sexp
             "M-u" 'sp-backward-barf-sexp
             "M-o" 'sp-forward-barf-sexp
             "M-U" 'sp-backward-slurp-sexp
             "M-O" 'sp-forward-slurp-sexp)

(defun* sp-wrap-round ()
  "Wrap the following sexp"
  (interactive)
  (save-excursion
    (dolist (open (list "(" "[" "{"))
      (if (equal (string (char-after)) open)
          (progn (sp-wrap-with-pair open)
                 (return-from sp-wrap-round))
        (sp-select-next-thing)
        (sp-wrap-with-pair "(")
        (return-from sp-wrap-round)))))
