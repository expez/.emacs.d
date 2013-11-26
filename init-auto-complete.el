(require-package 'auto-complete)
(require 'auto-complete-config)
(require 'flyspell)

(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
(ac-config-default)
(setq ac-auto-start 2
      ac-quick-help-delay 0.5
      ac-fuzzy-enable t
      ac-use-fuzzzy t
      ac-auto-show-menu 0.2)

(global-auto-complete-mode)

(ac-flyspell-workaround)

(defun ac-exit-to-normal-state
  "Stops completing and returns to normal state"
  (interactive "")
  (ac-stop)
  (evil-force-normal-state)
  (evil-normal-state))

(fill-keymap ac-completing-map
             "C-[" 'ac-exit-to-normal-state
             "C-l" 'ac-expand-common
             "C-j" 'ac-next
             "C-s" 'ac-isearch
             "C-k" 'ac-previous
             "C-g" 'ac-stop
             "ESC" 'ac-stop)

(provide 'init-auto-complete)
