(defcustom programming-language-major-modes
  '(prog-mode     ; This is the mode perl, makefile, lisp-mode, scheme-mode,
                  ; emacs-lisp-mode, sh-mode, java-mode, c-mode, c++-mode,
                  ; python-mode inherits from.
    lua-mode
    cmake-mode
    tex-mode                            ; LaTeX inherits
    sgml-mode                           ; HTML inherits
    css-mode
    nxml-mode
    diff-mode
    haskell-mode
    rst-mode)
  "What to consider as programming languages.")

(dolist (mode programming-language-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   (lambda ()
     (font-lock-add-keywords
      nil
      '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\|NOTE\\|REFACTOR\\)"
         1
         '(:box (:color "grey10" :line-width 2) :background "red" :bold t
                :foreground "yellow")
         prepend)))
     (rainbow-mode 0)
     (rainbow-delimiters-mode 1)
     (setq show-trailing-whitespace t)
     (flyspell-prog-mode)
     (setq whitespace-style '(face lines-tail))
     (whitespace-mode 1)
     (flycheck-mode)
     (glasses-mode 1))))

(setq compilation-ask-about-save nil
      compilation-window-height 30)

(global-font-lock-mode 1)

;;Close compilation window if compile was succesful.
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "Compilation errors, press C-c n to visit")

          ;;no errors, make the compilation window go away in 2 second
          ;;(run-at-time 1 nil 'delete-windows-on buf)
          (run-at-time 1 nil 'kill-buffer buf)

          (message "Compilation succesful!"))))



(provide 'init-prog-modes)
