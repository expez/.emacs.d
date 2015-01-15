(require-package 'emmet-mode)
(require-package 'ac-emmet)
(require-package 'know-your-http-well)
(require-package 'web-mode)
(require-package 'angular-snippets)
(require 'sgml-mode)
(require 'know-your-http-well)
(require 'web-mode)
(require 'emmet-mode)
(require 'angular-snippets)

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
  (setq-local tab-width 2)
  (change-whitespace-line-column 120))
(add-hook 'html-mode-hook #'my-html-mode-hook)

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
(eval-after-load "sgml-mode"
  '(define-key html-mode-map (kbd "C-c C-d") 'ng-snip-show-docs-at-point))

(setq emmet-move-cursor-between-quotes t)

(defun html-create-list ()
  "Creates a unordered list from the lines in REGION.

With a prefix it creates an ordered list."
  (interactive)
  (when (use-region-p)
    (let ((list-type (if current-prefix-arg "ol" "ul"))
          last-line)
      (save-excursion
        (goto-char (region-end))
        (end-of-line)
        (open-line 1)
        (insert "<" list-type "/>")
        (setq last-line (1+ (line-number-at-pos)))
        (goto-char (region-beginning))
        (open-line 1)
        (insert "<" list-type ">")
        (forward-line)
        (while (< (line-number-at-pos) last-line)
          (html-wrap-in-tag "li" (point-at-bol) (point-at-eol))
          (forward-line))))))

(fill-keymaps '(html-mode-map web-mode-map)
              (kbd "RET") 'newline-and-indent
              "C-c l" 'html-create-list)

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(provide 'init-html)
