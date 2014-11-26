(require-package 'flycheck)
(require-package 'aggressive-indent)
(require 'flycheck)
(require 'fic-mode)

(after-load 'aggressive-indent
  (define-key aggressive-indent-mode-map (kbd "C-c C-q") nil))

(setq fic-highlighted-words '("FIXME" "FIXME:" "TODO" "TODO:"
                              "HACK" "HACK:" "NOTE" "NOTE:"
                              "BUG" "BUG:" "REFACTOR" "REFACTOR:"))

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
     (fic-mode 1)
     (rainbow-delimiters-mode 1)
     (unless (eq system-type 'windows-nt)
       (flyspell-prog-mode))
     (yas-minor-mode-on)
     (whitespace-mode 1)
     (aggressive-indent-mode 1)
     (glasses-mode 1))))

(setq compilation-ask-about-save nil
      compilation-window-height 30)

(global-font-lock-mode 1)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-completion-system 'ido)

(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 0.5 5.0)))

;; Each buffer gets its own idle-change-delay because of the
;; buffer-sensitive adjustment above.
(make-variable-buffer-local 'flycheck-idle-change-delay)

(add-hook 'flycheck-after-syntax-check-hook
          'magnars/adjust-flycheck-automatic-syntax-eagerness)

;; Remove newline checks, since they would trigger an immediate check
;; when we want the idle-change-delay to be in effect while editing.
(setq flycheck-check-syntax-automatically '(save
                                            idle-change
                                            mode-enabled))

(provide 'init-prog-modes)
