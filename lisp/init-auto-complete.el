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

(fill-keymap ac-completing-map
             (kbd "<tab>") nil
             "C-l" 'ac-expand-common
             "C-s" 'ac-isearch)


(provide 'init-auto-complete)
