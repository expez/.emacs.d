(require-package 'emmet-mode)
(require-package 'know-your-http-well)
(require 'sgml-mode)
(require 'emmet-mode)

(defun my-html-mode-hook ()
  (change-whitespace-line-column 120)
  (fill-keymaps '(evil-insert-state-local-map evil-normal-state-local-map)
                (kbd "M-n") #'flycheck-next-error
                (kbd "M-p") #'flycheck-previous-error)
  (aggressive-indent-mode 0))
(add-hook 'html-mode-hook #'my-html-mode-hook)

(add-hook 'sgml-mode-hook 'emmet-mode)
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

(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(provide 'init-html)
