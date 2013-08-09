(require 'yasnippet)

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
     (rainbow-delimiters-mode 1)
     (flyspell-prog-mode)
     (yas-minor-mode-on)
     (whitespace-mode 1)
     (glasses-mode 1))))

(setq compilation-ask-about-save nil
      compilation-window-height 30)

(global-font-lock-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init-prog-modes)
