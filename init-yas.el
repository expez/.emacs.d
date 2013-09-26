(require 'yasnippet)

(yas-global-mode)
(setq yas-snippet-dirs (concat user-emacs-directory "snippets")
      yas-also-auto-indent-first-line t
      yas-prompt-functions '(yas-ido-prompt
                             yas-dropdown-prompt
                             yas-completing-prompt))
(yas/reload-all)

(add-auto-mode 'snippet-mode "\\.yasnippet")

(add-hook 'yas-before-expand-snippet-hook
          #'(lambda()
              (when (evil-visual-state-p)
                (let ((p (point))
                      (m (mark)))
                  (evil-insert-state)
                  (goto-char p)
                  (set-mark m)))))
