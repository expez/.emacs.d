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

(setq emmet-move-cursor-between-quotes t)

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

(defun html-wrap-in-tag (tag &optional beg end)
  "Add a tag to beginning and ending of current word or text selection."
  (interactive "sEnter tag name: ")
  (if (or (use-region-p)
          (and beg end))
      (progn
        (setq beg (or beg (region-beginning)))
        (setq end (or end (region-end))))
    (let ((bds (bounds-of-thing-at-point 'symbol)))
      (setq beg (car bds))
      (setq end (cdr bds))))
  (goto-char end)
  (insert "</" tag ">")
  (goto-char beg)
  (insert "<" tag ">"))

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
              "M-h" 'sgml-skip-tag-backward
              "C-c l" 'html-create-list)

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(provide 'init-html)
