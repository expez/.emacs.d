(push user-emacs-directory load-path)

;; Add everything in and below site-lisp to load-path.
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(require 'init-util)
(require 'init-package)

(load-elisp-files-in-dir user-emacs-directory "^init-.\*")

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file)

(load-from-vendor-dir)

(require 'uniquify)
(require 'org)
(require 'server)
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f)
                 (load-theme 'solarized-dark t)
                 (set-face-foreground whitespace-space "deep sky blue")
                 (set-face-foreground whitespace-newline "deep sky blue")
                 (set-face-foreground whitespace-indentation "deep sky blue")
                 (set-face-foreground whitespace-line "deep sky blue")
                 (set-face-foreground whitespace-tab "deep sky blue")))))

(when (or (eq system-type 'windows-nt)
          (not server-clients))
  (load-theme 'solarized-dark))

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

(setq reb-re-syntax 'string)

(fset 'yes-or-no-p 'y-or-n-p)

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

(when (eq system-type 'windows-nt)
  (set-frame-font
   "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

(when (eq system-type 'gnu/linux)
  (set-frame-font "Inconsolata-12"))

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      tramp-backup-directory-alist backup-directory-alist)

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

(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})
                            (?\[ . ?\])))

(setq-default indent-tabs-mode nil)

(column-number-mode 1)

(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))

;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf! If using Tramp a lot.
(recentf-mode t)
(setq recentf-max-saved-items 300
      recentf-save-file (concat user-emacs-directory ".recentf"))
(run-with-timer 500 500 (lambda () (recentf-save-list)))

(setq recentf-exclude '("\\.recentf"
                        file-remote-p
                        "\\.ido\\.last"
                        "\\.keychain/.*?-sh\\(-gpg\\)?"))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(setq tramp-default-method "ssh")

(setq enable-recursive-minibuffers t)

(setq sr-speedbar-right-side nil)

(setq css-indent-offset 2)

(winner-mode 1)

(fill-keymap winner-mode-map
             "C-x 7" 'winner-undo
             "C-x 9" 'winner-redo)

(add-hook
 'before-save-hook
 (lambda ()
   (when buffer-file-name
     (let ((dir (file-name-directory buffer-file-name)))
       (when (and (not (file-exists-p dir))
                  (y-or-n-p
                   (format "Directory %s does not exist. Create it?" dir)))
         (make-directory dir t))))))

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [36 10])
        (tab-mark 9 [9655 9] [92 9])))

(setq bookmark-version-control 't
      bookmark-save-flag 1
      bookmark-default-file (concat user-emacs-directory "bookmarks"))

(setq tags-revert-without-query 1)

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-log-done t
      org-agenda-files (list "~/org/todo.org")
      org-agenda-skip-unavailable-files t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-start-on-weekday nil
      org-indirect-buffer-display 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'other-window
      org-agenda-show-all-dates t
      org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))

(setq save-abbrevs nil)
(setq-default abbrev-mode t)

(add-to-list 'org-structure-template-alist
             '("n" "#+BEGIN_COMMENT\n?\n#+END_COMMENT"
               "<comment>\n?\n</comment>"))

(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
