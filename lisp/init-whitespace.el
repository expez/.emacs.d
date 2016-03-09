(require-package 'ethan-wspace)

(setq my-whitespace-style '(face tabs lines-tail))
(setq whitespace-style my-whitespace-style)

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [36 10])
        (tab-mark 9 [9655 9] [92 9])))

(eval-after-load "ethan-wspace"
  '(progn
     (define-minor-mode ethan-wspace-highlight-tabs-mode
       :init-value nil :lighter nil :keymap nil)))

(global-ethan-wspace-mode 1)
(setq ethan-wspace-face-customized t)

(defun toggle-tabs (force)
  "Whether we should be cleaning tabs or not.

force forces tabs to ON."
  (interactive "P")
  (if (or force
          (not (memq 'tabs whitespace-style)))
      (progn
        (ethan-wspace-type-deactivate 'tabs)
        (setq-local whitespace-style (remq 'tabs whitespace-style))
        (setq-local tab-width 2)
        (setq-local indent-tabs-mode t)
        (setq-local whitespace-line-column 120)
        (whitespace-mode 0)
        (whitespace-mode 1)
        (when (called-interactively-p)
          (message "Tabs are OK!")))
    (ethan-wspace-type-activate 'tabs)
    (setq-local whitespace-style (cons 'tabs whitespace-style))
    (when (called-interactively-p)
      (message "Tabs are not OK!")))
  (whitespace-mode 0)
  (whitespace-mode 1))

(defvar whitespace-show-all-mode nil)

(defun* toggle-whitespace-mode ()
  "Toggles whitespace modes between modes where some whitespace
is highligted and all whitespace is higlighted.

With a prefix argument whitespac-mode is turned off."
  (interactive)
  (when current-prefix-arg
    (if whitespace-mode
        (progn
          (whitespace-mode 0)
          (message "Whitespace mode off"))
      (whitespace-mode 1)
      (message "Whitespace mode on"))
    (return-from toggle-whitespace-mode))
  (if whitespace-show-all-mode
      (progn
        (setq whitespace-style my-whitespace-style)
        (setq whitespace-show-all-mode nil)
        (whitespace-mode 0)
        (whitespace-mode 1)
        (message "Highlighting some whitespace"))
    (setq whitespace-style
          '(face tabs spaces trailing lines-tail space-before-tab newline
                 indentation empty space-after-tab space-mark tab-mark
                 newline-mark))
    (setq whitespace-show-all-mode t)
    (whitespace-mode 0)
    (whitespace-mode 1)
    (message "Highlighting all whitespace")))

(defun change-whitespace-line-column (column)
  "Sets the `whitespace-line-column' to COLUMN in this buffer."
  (interactive "nHighlight beyond column: ")
  (make-local-variable 'whitespace-line-column)
  (setq-local whitespace-line-column column)
  (whitespace-mode 0)
  (whitespace-mode 1)
  (when (called-interactively-p)
    (message (format "Highlighting lines longer than %s chars." column))))

(defadvice popup-tip (around disable-ethan-wspace activate)
  (let ((whitespace-mode whitespace-mode))
    (whitespace-mode 0))
  ad-do-it
  (when whitespace-mode
    (whitespace-mode 1)))

(defvar hosts-where-tabs-are-expected nil)

(defun host-where-tabs-are-expected ()
  (cl-some (lambda (host) (string= system-name host))
           hosts-where-tabs-are-expected))

(defun maybe-allow-tabs ()
  (interactive)
  (when (and (host-where-tabs-are-expected)
             (eq (indentation-style) 'tabs))
    (toggle-tabs :on)))

(provide 'init-whitespace)
