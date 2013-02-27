(require 'sr-speedbar)
(require 'uniquify)

(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f) (color-theme-solarized-dark)))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-up-aggressively 0
      scroll-down-aggressively 0
      scroll-preserve-screen-position t)

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(ido-mode 'both)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always)

(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

;; re-builder
(setq reb-re-syntax 'string)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;;Add newline at the end of files.
(setq require-final-newline 'visit-save)

(setq delete-by-moving-to-trash t)

(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)

(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n")

(put 'set-goal-column 'disabled nil)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq echo-keystrokes 0.1)

(if (eq system-type 'windows-nt)
    (set-default-font
     "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

(if (eq system-type 'gnu/linux)
    (set-default-font "Inconsolata-12"))

;; Remove C-m (^M) characters that appear in output

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
(setq tramp-backup-directory-alist backup-directory-alist)

(make-directory (concat user-emacs-directory "autosaves/") t)

(setq vc-make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(global-undo-tree-mode 1)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

(auto-image-file-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

(electric-pair-mode 1)

(setq org-src-fontify-natively t)

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\[ . ?\])))

(setq-default indent-tabs-mode nil)

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

(column-number-mode 1)

(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))

;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf! If using Tramp a lot.
(recentf-mode t)
(setq recentf-max-saved-items 100)
(run-with-timer (* 20 60) (* 2 60 60) (lambda () (recentf-save-list)))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq tramp-default-method "ssh")

(setq enable-recursive-minibuffers t)

(setq recentf-exclude '("\\.recentf"
                        "\\.ido\\.last"
                        "\\.keychain/.*?-sh\\(-gpg\\)?"))

(setq sr-speedbar-right-side nil)

(setq css-indent-offset 2)
