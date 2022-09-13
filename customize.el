(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-font-list
   '((98 "{\\bf " "}")
     (99 "{\\sc " "}")
     (101 "{\\em " "\\/}")
     (105 "{\\it " "\\/}")
     (114 "{\\rm " "}")
     (115 "{\\sl " "\\/}")
     (115 "{\\tt " "}")
     (100 "" "" t)))
 '(auto-image-file-mode t)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(fic-background-color "#002b36")
 '(fic-foreground-color "Orange")
 '(fringe-mode (cons 8 0) nil (fringe))
 '(glasses-face 'bold)
 '(glasses-original-separator "")
 '(glasses-separate-capital-groups nil)
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(magit-use-overlays t)
 '(package-selected-packages
   '(git-modes yasnippet flycheck groovy-mode flycheck-clj-kondo helm undo-tree org-present company-ansible cider clojure-mode smartparens restclient projectile know-your-http-well emmet-mode yard-mode yaml-mode workgroups2 window-numbering wgrep-ag wgrep-ack web-mode web-beautify visual-fill-column unbound transpose-frame toml-mode sqlup-mode sql-indent solarized-theme slime-company scss-mode sass-mode rvm ruby-interpolation rspec-mode robe rinari regex-tool regex-dsl redshank rainbow-mode rainbow-delimiters projectile-rails popwin paren-face neotree mode-line-debug mediawiki markdown-mode magit-gh-pulls macrostep lorem-ipsum lexbind-mode less-css-mode key-chord js2-refactor info+ ido-vertical-mode ido-hacks ido-completing-read+ helm-pages helm-descbinds google-c-style gitignore-mode gitconfig-mode git-wip-timemachine git-messenger git-gutter-fringe gist ghci-completion ghc form-feed flycheck-clojure flx-ido feature-mode exec-path-from-shell evil-visualstar evil-surround evil-smartparens evil-paredit evil-numbers evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-args ethan-wspace eshell-manual elpy elisp-slime-nav eldoc-eval ein edit-color-stamp edebug-x dropdown-list dired+ diminish deft ctypes css-eldoc crontab-mode company-tern company-restclient company-quickhelp company-math company-flx color-moccur clojure-snippets clojure-cheatsheet clj-refactor c-eldoc bundler buffer-move bookmark+ bind-key auctex angular-snippets aggressive-indent ag ace-window ace-jump-mode ac-emmet))
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((cider-ns-refresh-before-fn . "user/stop")
     (cider-ns-refresh-after-fn . "user/start")
     (cider-compilation-regexp quote
                               ("Syntax error .*?compiling .*?at (\\(.*?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\))" 1 2 3))
     (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (nameless-current-name . "cider")
     (nameless-affect-indentation-and-filling)
     (eval push
           (file-name-directory
            (buffer-file-name))
           load-path)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t (:background "#073642"))))
 '(form-feed-line ((t (:strike-through "#839496"))))
 '(iedit-occurrence ((t (:inherit region))))
 '(tooltip ((t (:inherit variable-pitch :background "#073642" :foreground "#93a1a1"))))
 '(web-mode-builtin-face ((t (:inherit font-lock-builtin-face :foreground "font-lock-builtin-face"))))
 '(wg-divider-face ((t nil)))
 '(wg-filename-face ((t (:foreground "light sky blue"))))
 '(wg-message-face ((t (:foreground "light sky blue"))))
 '(wg-mode-line-face ((t nil)))
 '(whitespace-indentation ((t (:foreground "deep sky blue"))))
 '(whitespace-newline ((t (:foreground "deep sky blue" :weight normal))))
 '(whitespace-space ((t (:foreground "deep sky blue"))))
 '(whitespace-tab ((t (:foreground "deep sky blue")))))
