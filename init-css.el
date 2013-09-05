(require 'sass-mode)
(require 'css-mode)

(dolist (hook '(css-mode-hook sass-mode-hook haml-mode-hook))
  (add-hook hook 'rainbow-mode))

;;; Embedding in html
(after-load 'mmm-vars
            (mmm-add-group
             'html-css
             '((css-cdata
                :submode css-mode
                :face mmm-code-submode-face
                :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
                :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
                :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                             @ "\n" _ "\n" @ "</script>" @)))
               (css
                :submode css-mode
                :face mmm-code-submode-face
                :front "<style[^>]*>[ \t]*\n?"
                :back "[ \t]*</style>"
                :insert ((?j js-tag nil @ "<style type=\"text/css\">"
                             @ "\n" _ "\n" @ "</style>" @)))
               (css-inline
                :submode css-mode
                :face mmm-code-submode-face
                :front "style=\""
                :back "\"")))
            (dolist (mode (list 'html-mode 'nxml-mode))
              (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))

(add-hook 'css-mode-hook 'skewer-css-mode)
(defvar sanityinc/skewer-less-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-k") 'sanityinc/skewer-less-save-and-reload)
    m)
  "Keymap for `sanityinc/skewer-less-mode'.")

(define-minor-mode sanityinc/skewer-less-mode
  "Minor mode allowing LESS stylesheet manipulation via `skewer-mode'."
  nil
  " LessSkew"
  sanityinc/skewer-less-mode-map
  (progn
    (add-hook 'after-save-hook 'sanityinc/skewer-less-reload nil t)))

(defun sanityinc/skewer-less-save-and-reload ()
  "When skewer appears to be active, ask for a reload."
  (interactive)
  (save-buffer)
  (sanityinc/skewer-less-reload))

(defun sanityinc/skewer-less-reload ()
  "When skewer appears to be active, ask for a reload."
  (interactive)
  (skewer-eval "less.refresh();"))

(provide 'init-css)