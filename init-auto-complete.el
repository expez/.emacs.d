(require 'auto-complete-config)
(require 'flyspell)
(require 'auto-complete-latex)

(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
(ac-config-default)
(setq ac-auto-start 2
      ac-quick-help-delay 0.5
      ac-fuzzy-enable t
      ac-auto-show-menu 0.2)

(add-hook 'LaTeX-mode-hook #'ac-l-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(ac-flyspell-workaround)

(defun ac-exit-to-normal-state
  "Stops completing and returns to normal state"
  (interactive "")
  (ac-stop)
  (evil-force-normal-state))

(fill-keymap ac-completing-map
             "C-[" 'ac-exit-to-normal-state
             "C-l" 'ac-expand-common
             "C-j" 'ac-next
             "C-s" 'ac-isearch
             "C-k" 'ac-previous
             "ESC" 'ac-stop)

(provide 'init-auto-complete)
