(require 'sgml-mode)
(require 'know-your-http-well)

(add-hook 'html-mode-hook 'skewer-html-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

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

(add-hook 'sgml-mode-hook 'zencoding-mode)

(fill-keymap html-mode-map
             (kbd "C-c C-w") 'html-wrap-in-tag
             (kbd "C-)") 'tagedit-forward-slurp-tag
             (kbd "M-)") 'tagedit-forward-barf-tag
             (kbd "M-r") 'tagedit-raise-tag
             (kbd "M-s") 'tagedit-splice-tag
             (kbd "M-S") 'tagedit-split-tag
             (kbd "M-j") 'tagedit-join-tags
             (kbd "M-?") 'tagedit-convolute-tags
             (kbd "M-k") 'tagedit-kill-attribute
             (kbd "M-<return>") 'tagedit-toggle-multiline-tag)

(tagedit-add-experimental-features)
(add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(provide 'init-html)
