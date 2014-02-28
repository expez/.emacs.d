(require-package 'emmet-mode)
(require-package 'ac-emmet)
(require-package 'tagedit)
(require-package 'know-your-http-well)
(require-package 'web-mode)
(require 'sgml-mode)
(require 'know-your-http-well)
(require 'nxml-mode)
(require 'tagedit)
(require 'web-mode)
(require 'emmet-mode)

(defun my-web-mode-hook ()
  (add-hook 'local-write-file-hooks (lambda () (delete-trailing-whitespace) nil))
  ;; For some reason the manual says to apply mode settings in a hook.
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-disable-auto-pairing nil
        web-mode-enable-current-element-highlight t)
  (emmet-mode 1))
(add-hook 'web-mode-hook #'my-web-mode-hook)
(add-auto-mode 'web-mode "\\.mustache")
(add-auto-mode 'web-mode "\\.hbs")
(add-auto-mode 'web-mode "\\.erb")

(defun my-html-mode-hook ()
  (skewer-html-mode 1)
  (tagedit-mode 1))
(add-hook 'html-mode-hook #'my-html-mode-hook)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

(defun html-wrap-in-tag (beg end)
  (interactive "r")
  (let ((oneline? (= (line-number-at-pos beg) (line-number-at-pos end))))
    (deactivate-mark)
    (goto-char end)
    (unless oneline? (newline-and-indent))
    (insert "</div>")
    (goto-char beg)
    (insert "<div>")
    (unless oneline? (newline-and-indent))
    (indent-region beg (+ end 11))
    (goto-char (+ beg 4))))

(fill-keymaps '(html-mode-map nxml-mode-map web-mode-map)
              (kbd "RET") 'newline-and-indent
              (kbd "C-c C-w") 'html-wrap-in-tag
              (kbd "C-)") 'tagedit-forward-slurp-tag
              (kbd "M-)") 'tagedit-forward-barf-tag
              (kbd "M-r") 'tagedit-raise-tag
              (kbd "M-s") 'tagedit-splice-tag
              (kbd "M-S") 'tagedit-split-tag
              (kbd "M-j") 'tagedit-join-tags
              (kbd "M-?") 'tagedit-convolute-tags
              (kbd "M-k") 'tagedit-kill
              (kbd "M-K") 'tagedit-kill-attribute
              (kbd "M-<return>") 'tagedit-toggle-multiline-tag
              "M-l" 'sgml-skip-tag-forward
              "M-h" 'sgml-skip-tag-backward)

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(provide 'init-html)
