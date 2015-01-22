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
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((eval push
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
