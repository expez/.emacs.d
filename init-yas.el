(require 'yasnippet)

(yas-global-mode 1)
(yas/reload-all)
(setq yas/root-directory  (concat user-emacs-directory "snippets"))
(yas/load-directory yas/root-directory)
(setq yas-prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

(add-hook 'yas-before-expand-snippet-hook
          #'(lambda()
              (when (evil-visual-state-p)
                (let ((p (point))
                      (m (mark)))
                  (evil-insert-state)
                  (goto-char p)
                  (set-mark m)))))
