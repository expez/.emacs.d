(require 'yasnippet)

(yas-global-mode)
(setq yas/root-directory  (concat user-emacs-directory "snippets"))
(yas/load-directory yas/root-directory)
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-dropdown-prompt
                             yas-completing-prompt))

(add-hook 'yas-before-expand-snippet-hook
          #'(lambda()
              (when (evil-visual-state-p)
                (let ((p (point))
                      (m (mark)))
                  (evil-insert-state)
                  (goto-char p)
                  (set-mark m)))))
