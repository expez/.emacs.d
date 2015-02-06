(require-package 'macrostep)
(require-package 'lexbind-mode)
(require-package 'pretty-mode-plus)
(require-package 'elisp-slime-nav)
(require-package 'parenface-plus)
(require 'parenface-plus)
(require-package 'rainbow-delimiters)
(require-package 'flycheck)
(require-package 'evil-surround)
(require-package 'edebug-x)
(require 'evil-surround)

(set-face-foreground 'paren-face "grey30")

(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (x-gtk-use-system-tooltips nil)
         (description (save-window-excursion
                        (with-temp-buffer
                          (help-mode)
                          (help-xref-interned thing)
                          (buffer-string)))))
    (if (string-empty-p description)
        (message "No doc found for %s!" thing)
      (pos-tip-show description nil nil nil -1)
      (unwind-protect
          (sit-for 360)
        (pos-tip-hide)))))

(defun my-elisp-mode-hook ()
  (turn-on-redshank-mode)
  (flycheck-mode 0)
  (lexbind-mode)
  (esk-remove-elc-on-save)
  (checkdoc-minor-mode)
  (setq evil-symbol-word-search t)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist)
  (define-key evil-normal-state-local-map (kbd "M-.")
    'elisp-slime-nav-find-elisp-thing-at-point))

(add-hook 'emacs-lisp-mode-hook #'my-elisp-mode-hook)

(fill-keymap emacs-lisp-mode-map
             "C-c C-c" 'eval-defun
             "C-c C-d" 'describe-thing-in-popup
             "C-c t" 'bind-test-to
             "C-c C-e" 'pp-eval-last-sexp
             "C-c C-k" 'eval-buffer)

(let ((elisp-programming-major-modes '(emacs-lisp-mode lisp-interaction-mode
                                                       ielm-mode)))
  (dolist (mode elisp-programming-major-modes)
    (add-hook
     (intern (concat (symbol-name mode) "-hook"))
     (lambda ()
       (eldoc-mode)
       (rainbow-delimiters-mode 0)))))

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(provide 'init-elisp)
