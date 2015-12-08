(require-package 'macrostep)
(require-package 'lexbind-mode)
(require-package 'elisp-slime-nav)
(require-package 'paren-face)
(require-package 'rainbow-delimiters)
(require-package 'flycheck)
(require-package 'evil-surround)
(require-package 'edebug-x)
(require 'evil-surround)
(require 'ert)

(global-paren-face-mode)

(defun elisp-slime-nav-read-symbol-from-mini-buffer (oldfun &rest args)
  (funcall oldfun (read-from-minibuffer "Jump to: "
                                        (format "%s" (symbol-at-point)))))

(advice-add 'elisp-slime-nav-find-elisp-thing-at-point :around
            #'elisp-slime-nav-read-symbol-from-mini-buffer)

(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (intern-soft (read-from-minibuffer "Describe: " (format "%s" (symbol-at-point)))))
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
  (rainbow-delimiters-mode 0)
  (turn-on-redshank-mode)
  (flycheck-mode 0)
  (lexbind-mode)
  (eldoc-mode)
  (esk-remove-elc-on-save)
  (checkdoc-minor-mode)
  (setq evil-symbol-word-search t)
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist)
  (define-key evil-normal-state-local-map (kbd "M-.")
    'elisp-slime-nav-find-elisp-thing-at-point))

(add-hook 'emacs-lisp-mode-hook #'my-elisp-mode-hook)

(fill-keymap emacs-lisp-mode-map
             "C-c e" 'macrostep-expand
             "C-c C-c" 'eval-defun
             "C-c C-d" 'describe-thing-in-popup
             "C-c t" 'bind-test-to
             "C-c C-e" 'pp-eval-last-sexp
             "<f3>" 'ert-silently
             "C-M-;" #'comment-or-uncomment-sexp
             "C-c C-k" 'eval-buffer)

(let ((elisp-programming-major-modes '(lisp-interaction-mode ielm-mode)))
  (dolist (mode elisp-programming-major-modes)
    (add-hook
     (intern (concat (symbol-name mode) "-hook"))
     (lambda ()
       (after-load 'smartparens
         (smartparens-strict-mode 1))
       (rainbow-delimiters-mode 0)))))

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun ert-silently ()
  (interactive)
  "Run all the tests silently and pop to test buffer only on failure.

With a prefix just runs `ert'"
  (if current-prefix-arg
      (call-interactively #'ert)
    (let ((unexpected (ert-stats-completed-unexpected
                       (ert-run-tests t (lambda (&rest _))))))
      (if (> unexpected 0)
          (ert t)
        (message "%s" (propertize "Passed!" 'face 'success-face))))))

(provide 'init-elisp)
