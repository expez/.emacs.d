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

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(provide 'init-auto-complete)
