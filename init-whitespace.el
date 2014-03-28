(require-package 'ethan-wspace)
(setq whitespace-style '(face tabs trailing lines-tail empty
                              space-before-tab tab-mark))

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [36 10])
        (tab-mark 9 [9655 9] [92 9])))

(eval-after-load "ethan-wspace"
  '(progn
     (define-minor-mode ethan-wspace-highlight-tabs-mode
       :init-value nil :lighter nil :keymap nil)

     (define-minor-mode ethan-wspace-highlight-eol-mode
       :init-value nil :lighter nil :keymap nil)))

(global-ethan-wspace-mode 1)
(setq ethan-wspace-face-customized t)

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
        (setq whitespace-style '(face tabs trailing lines-tail empty
                                      space-before-tab tab-mark))
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

(defun ethan-wspace-clean-all ()
  "Clean all whitespace errors immediately."
  (interactive)
  (dolist (type ethan-wspace-errors)
    (ethan-wspace-type-clean type))
  (indent-buffer))

(defun change-whitespace-line-column (c)
  (interactive "nHighlight beyond column: ")
  (make-local-variable 'whitespace-line-column)
  (setq whitespace-line-column c)
  (whitespace-mode 0)
  (whitespace-mode 1)
  (message (format "Highlighting lines longer than %s chars." c)))

(defadvice popup-tip (around disable-ethan-wspace activate)
  (let ((whitespace-mode whitespace-mode))
    (whitespace-mode 0))
  ad-do-it
  (when whitespace-mode
    (whitespace-mode 1)))

(provide 'init-whitespace)
