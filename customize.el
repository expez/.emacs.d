(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-font-list
   (quote
    ((98 "{\\bf " "}")
     (99 "{\\sc " "}")
     (101 "{\\em " "\\/}")
     (105 "{\\it " "\\/}")
     (114 "{\\rm " "}")
     (115 "{\\sl " "\\/}")
     (115 "{\\tt " "}")
     (100 "" "" t))))
 '(auto-image-file-mode t)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(fic-background-color "#002b36")
 '(fic-foreground-color "Orange")
 '(fringe-mode (cons 8 0) nil (fringe))
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separate-capital-groups nil)
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator "")
 '(magit-use-overlays t)
 '(package-selected-packages
   (quote
    (yard-mode yaml-mode workgroups2 window-numbering wgrep-ag wgrep-ack web-mode web-beautify visual-fill-column unbound transpose-frame toml-mode sqlup-mode sql-indent solarized-theme slime-company scss-mode sass-mode rvm ruby-interpolation rspec-mode robe rinari regex-tool regex-dsl redshank rainbow-mode rainbow-delimiters projectile-rails popwin paren-face neotree mode-line-debug mediawiki markdown-mode magit-gh-pulls macrostep lorem-ipsum lexbind-mode less-css-mode key-chord js2-refactor info+ ido-vertical-mode ido-hacks ido-completing-read+ helm-pages helm-descbinds google-c-style gitignore-mode gitconfig-mode git-wip-timemachine git-messenger git-gutter-fringe gist ghci-completion ghc form-feed flycheck-clojure flx-ido feature-mode exec-path-from-shell evil-visualstar evil-surround evil-smartparens evil-paredit evil-numbers evil-jumper evil-indent-textobject evil-iedit-state evil-exchange evil-args ethan-wspace eshell-manual elpy elisp-slime-nav eldoc-eval ein edit-color-stamp edebug-x dropdown-list dired+ diminish deft ctypes css-eldoc crontab-mode company-tern company-restclient company-quickhelp company-math company-flx color-moccur clojure-snippets clojure-cheatsheet clj-refactor c-eldoc bundler buffer-move bookmark+ bind-key auctex angular-snippets aggressive-indent ag ace-window ace-jump-mode ac-emmet)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
     (nameless-current-name . "cider")
     (nameless-affect-indentation-and-filling)
     (eval push
           (file-name-directory
            (buffer-file-name))
           load-path)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:cell-input-area ((t (:background "#073642"))))
 '(ethan-wspace-face ((t (:foreground "#882C02" :inverse-video t :weight bold))))
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
