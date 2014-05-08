(require-package 'less-css-mode)
(require-package 'scss-mode)
(require-package 'sass-mode)
(require-package 'rainbow-mode)
(require-package 'emmet-mode)
(require 'sass-mode)
(require 'css-mode)
(require 'emmet-mode)

(dolist (hook '(css-mode-hook sass-mode-hook haml-mode-hook))
  (add-hook hook 'rainbow-mode))

(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'sass-mode-hook 'ac-css-mode-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)
(add-hook 'css-mode-hook  'emmet-mode)

(setq css-indent-offset 2)
(setq scss-compile-at-save t)

(defun my-css-mode-hook ()
  (skewer-css-mode)
  (fill-keymap skewer-css-mode-map
               "C-c C-c" 'nil
               "C-c C-r" 'skewer-css-clear-all)
  (local-set-key (kbd "<return>") 'newline-and-indent)
  (fill-keymap evil-insert-state-local-map
               (kbd "M-m") 'js-insert-block))
(add-hook 'css-mode-hook 'my-css-mode-hook)
(define-key emmet-preview-keymap (kbd "C-j") 'emmet-preview-accept)

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

(defvar rainbow-hexadecimal-colors-font-lock-keywords
  '(("[^&]\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)[^\\w-]"
     (1 (rainbow-colorize-itself 1)))
    ("^\\(#\\(?:[0-9a-fA-F]\\{3\\}\\)+\\{1,4\\}\\)[^\\w-]"
     (0 (rainbow-colorize-itself)))
    ("[Rr][Gg][Bb]:[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}/[0-9a-fA-F]\\{1,4\\}"
     (0 (rainbow-colorize-itself)))
    ("[Rr][Gg][Bb][Ii]:[0-9.]+/[0-9.]+/[0-9.]+"
     (0 (rainbow-colorize-itself)))
    ("\\(?:[Cc][Ii][Ee]\\(?:[Xx][Yy][Zz]\\|[Uu][Vv][Yy]\\|[Xx][Yy][Yy]\\|[Ll][Aa][Bb]\\|[Ll][Uu][Vv]\\)\\|[Tt][Ee][Kk][Hh][Vv][Cc]\\):[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?/[+-]?[0-9.]+\\(?:[Ee][+-]?[0-9]+\\)?"
     (0 (rainbow-colorize-itself))))
  "Font-lock keywords to add for hexadecimal colors.")

(provide 'init-css)
