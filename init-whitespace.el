(setq whitespace-style '(face tabs trailing lines-tail empty
                                      space-before-tab tab-mark))

(eval-after-load "ethan-wspace"
  '(define-minor-mode ethan-wspace-highlight-tabs-mode
     "Minor mode to highlight tabs.

With arg, turn tab-highlighting on if arg is positive, off otherwise.
This supercedes (require 'show-wspace) and show-ws-highlight-tabs."
     :init-value nil :lighter nil :keymap nil))

(global-ethan-wspace-mode 1)
(setq ethan-wspace-face-customized t)

(defvar whitespace-show-all-mode nil)

(defun toggle-whitespace-mode ()
  (interactive)
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

(provide 'init-whitespace)
