(require-package 'flycheck)
(require-package 'aggressive-indent)
(require-package 'form-feed)
(require 'flycheck)
(require 'fic-ext-mode)
(require 'flyspell)
(require 'init-whitespace)

(after-load 'aggressive-indent
  (define-key aggressive-indent-mode-map (kbd "C-c C-q") nil))

(setq fic-highlighted-words '("FIXME" "FIXME:" "TODO" "TODO:"
                              "HACK" "HACK:" "NOTE" "NOTE:"
                              "BUG" "BUG:" "REFACTOR" "REFACTOR:"))

(defface success-face '((t (:foreground "#859900")))
  "Face to use when something finished successfully.")

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

(defun my-prog-mode-hook ()
  (fic-ext-mode 1)
  (rainbow-delimiters-mode 1)
  (smartparens-strict-mode 1)
  (form-feed-mode)
  (define-key evil-insert-state-local-map (kbd "RET") 'newline-and-indent)
  (unless (eq system-type 'windows-nt)
    (flyspell-prog-mode))
  (yas-minor-mode-on)
  (whitespace-mode 1)
  (maybe-allow-tabs)
  (aggressive-indent-mode 1)
  (glasses-mode 1)
  (evil-exchange-install))

(dolist (mode programming-language-major-modes)
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            #'my-prog-mode-hook))

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

(setq compilation-ask-about-save nil
      compilation-scroll-output 'next-error
      compilation-skip-threshold 2)

(defcustom endless/compile-window-size 60
  "Width given to the compilation window."
  :type 'integer
  :group 'endless)

(defun endless/compile-please ()
  "Compile without confirmation."
  (interactive)
  (save-window-excursion
    (compile compile-command))
  (pop-to-buffer (get-buffer "*compilation*"))
  (enlarge-window
   (- endless/compile-window-size (window-width))
   'horizontal))

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

(provide 'init-prog-modes)
