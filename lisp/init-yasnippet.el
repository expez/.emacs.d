(require-package 'yasnippet)

(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
(setq yas-also-auto-indent-first-line t
      yas-prompt-functions '(yas-ido-prompt
                             yas-completing-prompt))
(yas-reload-all)

(add-auto-mode 'snippet-mode "\\.yasnippet")

(add-hook 'yas-before-expand-snippet-hook
          #'(lambda()
              (when (evil-visual-state-p)
                (let ((p (point))
                      (m (mark)))
                  (evil-insert-state)
                  (goto-char p)
                  (set-mark m)))))

(defun my-snippet-mode-hook ()
  (ethan-wspace-highlight-no-nl-eof-mode 1))
(add-hook 'snippet-mode-hook #'my-snippet-mode-hook)

(provide 'init-yasnippet)
