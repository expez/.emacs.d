(require 'yasnippet)

(yas-global-mode 1)
(setq yas-trigger-key nil)
(yas/reload-all) ;; Needed to disable trigger key
(setq yas/root-directory  (concat user-emacs-directory "snippets"))
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))
