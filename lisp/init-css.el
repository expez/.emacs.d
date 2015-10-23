(require-package 'less-css-mode)
(require-package 'scss-mode)
(require-package 'sass-mode)
(require-package 'rainbow-mode)
(require-package 'emmet-mode)
(require-package 'css-eldoc)
(require-package 'edit-color-stamp)
(require 'css-eldoc)
(require 'sass-mode)
(require 'scss-mode)
(require 'css-mode)
(require 'emmet-mode)
(require 'edit-color-stamp)

(defun my-scss-mode-hook ()
  (turn-on-css-eldoc))

(defun bury-compilation-buffer (buf res)
  (if (string-equal res "finished\n")
      (progn (bury-buffer "*compilation*")
             (message "%s" (propertize "Compilation successful!" 'face 'success-face)))
    (message "%s" (propertize "Compilation failed!" 'face 'font-lock-warning-face))))

(defun scss-bury-compilation-buffer-on-success (oldfun &rest args)
  (let ((compilation-finish-functions (list #'bury-compilation-buffer-on-success)))
    (save-window-excursion (apply oldfun args))))

(advice-add 'scss-compile :around #'scss-bury-compilation-buffer-on-success)

(fill-keymaps '(sass-mode-map scss-mode-map)
              "C-c C-e" 'edit-or-insert-color-stamp
              "C-m" 'js-insert-block
              "C-c b" 'web-beautify-dwim)
(add-hook 'scss-mode-hook #'my-scss-mode-hook)

(dolist (hook '(css-mode-hook sass-mode-hook haml-mode-hook scss-mode-hook))
  (add-hook hook 'rainbow-mode))

(setq css-indent-offset 2
      scss-compile-at-save t)

(defun edit-or-insert-color-stamp ()
  (interactive)
  (-if-let (color-at-point (ecs-color-at-point-hex))
      (edit-color-stamp)
    (insert "#fff")
    (backward-char)
    (edit-color-stamp)))

(defun my-css-mode-hook ()
  (emmet-mode 1)
  (local-set-key (kbd "<return>") 'newline-and-indent))
(add-hook 'css-mode-hook 'my-css-mode-hook)
(define-key emmet-preview-keymap (kbd "C-j") 'emmet-preview-accept)

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
