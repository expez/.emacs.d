(require 'auto-complete-config)
(require 'flyspell)
(require 'auto-complete-latex)

(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
(ac-config-default)

(defun my-ac-config ()
  (add-hook 'LaTeX-mode-hook #'ac-l-setup)
  (setq ac-auto-start 2)
  (setq ac-quick-help-delay 0.5)
  (setq ac-auto-show-menu 0.2)
  (setq ac-fuzzy-enable t)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (ac-flyspell-workaround))
(my-ac-config)

(defun ac-exit-to-normal-state
  "Stops completing and returns to normal state"
  (interactive "")
  (ac-abort)
  (evil-force-normal-state))

(fill-keymap ac-completing-map
             "C-[" 'ac-exit-to-normal-state)

(fill-keymap ac-complete-mode-map
             "C-l" 'ac-expand-common
             "C-j" 'ac-next
             "C-k" 'ac-previous
             "ESC" 'ac-stop)

(provide 'init-auto-complete)
