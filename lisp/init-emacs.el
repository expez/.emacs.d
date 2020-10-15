(setq custom-file (concat user-emacs-directory "customize.el")
      solarized-use-more-italic t
      blink-cursor-mode nil)
(load custom-file)

(require 'uniquify)
(require 'server)
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f)
                 (menu-bar-mode -1)
                 (tool-bar-mode -1)
                 (scroll-bar-mode -1)
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
      scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0
      scroll-preserve-screen-position t

      uniquify-buffer-name-style 'post-forward
      uniquify-separator " • "
      uniquify-strip-common-suffix t
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"

      inhibit-startup-message t
      inhibit-startup-echo-area-message t

      echo-keystrokes 0.1
      initial-scratch-message
      ";; scratch buffer created -- happy hacking\n"
      initial-major-mode 'emacs-lisp-mode

      reb-re-syntax 'string
      delete-by-moving-to-trash t

      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      tramp-backup-directory-alist backup-directory-alist
      browse-url-browser-function 'eww-browse-url
      browse-url-generic-program "conkeror"
      vc-make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      create-lockfiles nil)

(setq-default buffer-file-coding-system 'utf-8-unix)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(put 'set-goal-column 'disabled nil)

(when (or (eq system-type 'windows-nt)
          (eq system-type 'darwin))
  (set-frame-font
   "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

(when (eq system-type 'gnu/linux)
  (set-frame-font "Inconsolata-12"))

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(make-directory (concat user-emacs-directory "autosaves/") t)

(global-undo-tree-mode 1)

(auto-image-file-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function 'process-list) (lambda () (list)))) ad-do-it))

(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

(setq-default indent-tabs-mode nil)

(column-number-mode 1)

(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))

(setq recentf-auto-cleanup 'never) ;; disable before we start recentf! If using Tramp a lot.
(setq recentf-max-saved-items 300
      recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode)
(run-with-timer 500 500 (lambda () (recentf-save-list)))

(setq recentf-exclude '("\\.recentf"
                        file-remote-p
                        "\\.ido\\.last"
                        "\\.keychain/.*?-sh\\(-gpg\\)?"))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(setq tramp-default-method "ssh")

(setq enable-recursive-minibuffers t)

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

(setq bookmark-version-control 't
      bookmark-save-flag 1
      bookmark-default-file (concat user-emacs-directory "bookmarks"))

(setq tags-revert-without-query 1)
(setq save-abbrevs nil)
(setq-default abbrev-mode t)

(set-language-environment "UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq make-backup-files nil)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards)
                             activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir :make-parents)))))

(setq view-read-only t)

(add-lambda 'view-mode-hook
  (after-load 'evil
    (when evil-normal-state-local-map
      (define-key evil-normal-state-local-map "q" 'View-quit))))

(defadvice message (after message-tail activate)
  "Go to point max after a message"
  (unless (string= (buffer-name (current-buffer)) "*Message*")
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (skip-syntax-backward " ")
      (let ((p (point)))
        (walk-windows
         (lambda (window)
           (if (string-equal (buffer-name (window-buffer window)) "*Messages*")
               (set-window-point window p)))
         nil
         t)))))

(setq split-height-threshold 600
      split-width-threshold 120)

(after-load 'windmove
  (defun windmove-find-other-window (dir &optional arg window)
    "Return the window object in direction DIR.
DIR, ARG, and WINDOW are handled as by `windmove-other-window-loc'."
    (window-in-direction
     (cond
      ((eq dir 'up) 'above)
      ((eq dir 'down) 'below)
      (t dir))
     ;; Different from the original only in the use of t as the IGNORE argument
     ;; to include otherwise ignored buffers
     window t arg windmove-wrap-around t)))

(provide 'init-emacs)
