(require 'eshell)
(require 'em-smart)
(require 'esh-toggle)
(require 'esh-opt)
(require 'em-cmpl)
(require 'em-prompt)
(require 'em-term)

(setq eshell-where-to-jump 'begin
      eshell-history-size 1000
      eshell-save-history-on-exit t
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t
      eshell-stringify-t nil
      eshell-term-name "ansi"
       eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
      eshell-ls-dired-initial-args '("-h"))

(setq eshell-modules-list
      '(eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist
                    eshell-ls eshell-pred eshell-prompt eshell-rebind
                    eshell-script eshell-smart eshell-term eshell-unix
                    eshell-xtra))
(add-hook 'eshell-mode-hook
          '(lambda () (eshell/export "TERM" "dumb")))
(setenv "PAGER" "cat")
(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "tail")

(add-to-list 'eshell-command-completions-alist
             '("gunzip" "gz\\'"))
(add-to-list 'eshell-command-completions-alist
             '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(add-hook 'eshell-mode-hook
               '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))

(setq eshell-prompt-function
      (lambda nil
        (concat
         (abbreviate-file-name (eshell/pwd))
         (if (= (user-uid) 0) " # " " $ "))))

(provide 'init-eshell)
