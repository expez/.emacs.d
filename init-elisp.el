(require-package 'macrostep)
(require-package 'lexbind-mode)
(require-package 'pretty-mode-plus)
(require-package 'elisp-slime-nav)
(require-package 'parenface-plus)
(require 'parenface-plus)
(require-package 'paredit)
(require-package 'evil-paredit)
(require 'evil-paredit)
(require-package 'rainbow-delimiters)
(require-package 'flycheck)

(set-face-foreground 'paren-face "grey30")

(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (save-window-excursion
                        (with-temp-buffer
                          (help-mode)
                          (help-xref-interned thing)
                          (buffer-string)))))
    (popup-tip description
               :point (point)
               :around t
               :height 30
               :scroll-bar t
               :margin t)))

(defun my-elisp-mode-hook ()
  (turn-on-redshank-mode)
  (flycheck-mode 0)
  (lexbind-mode)
  (esk-remove-elc-on-save)
  (checkdoc-minor-mode)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-c C-e") 'eval-defun)
  (local-set-key (kbd "C-c C-d") 'describe-thing-in-popup)
  (local-set-key (kbd "C-c d") 'toggle-debug-on-error)
  (local-set-key (kbd "C-c t") 'bind-test-to)
  (ac-emacs-lisp-mode-setup)
  (push '(?` . ("`" . "'")) surround-pairs-alist)
  (define-key evil-normal-state-local-map (kbd "M-.")
    'elisp-slime-nav-find-elisp-thing-at-point))

(add-hook 'emacs-lisp-mode-hook #'my-elisp-mode-hook)

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(let ((elisp-programming-major-modes '(emacs-lisp-mode lisp-interaction-mode
                                                       ielm-mode)))
  (dolist (mode elisp-programming-major-modes)
    (add-hook
     (intern (concat (symbol-name mode) "-hook"))
     (lambda ()
       (turn-on-eldoc-mode)
       (paredit-mode 1)
       (evil-paredit-mode 1)
       (rainbow-delimiters-mode 0)))))

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun bind-test-to ()
  (interactive)
  (let ((fn (read-from-minibuffer "Bind ,t to: ")))
    (define-key leader-map "t" (intern fn))))

(provide 'init-lisp)
