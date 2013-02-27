(require 'ac-emacs-eclim-source)
(require 'command-frequency)
(require 'color-moccur)
(require 'color-theme)
(require 'cuda-mode)
(require 'diminish)
(require 'eclim)
(require 'eclimd)
(require 'go-autocomplete)
(require 'google-c-style)
(require 'highlight-tags-mode)
(require 'ido-hacks)
(require 'info+)
(require 'java-mode-indent-annotations)
(require 'lorem-ipsum)
(require 'mediawiki)
(require 'sr-speedbar)
(require 'uniquify)
(require 'workgroups)

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
(ido-ubiquitous-mode 1)
(ido-hacks-mode)

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
      show-paren-style 'parenthesis) ;; highlight parenthesis, 'expression would only highlight entire exp within paren.

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

;;Prevent flyspell from spamming.
(setq flyspell-issue-message-flag nil)

(setq ispell-dictionary "english")
;;Make sure ispell process is started in home directory to use personal dictionaries.
(setq ispell-process-directory (expand-file-name "~/"))
(setq ispell-program-name "aspell") ;;Use aspell dictionaries.
(setq ispell-list-command "list") ;;Faster region checking, "list" for aspell, "-1" for ispell.

;;Ultra twice as slow as ispell, fast twice as slow as ultra, normal 10x slower than fast.
;;(setq ispell-extra-args '("--sug-mode=ultra"))

(workgroups-mode 1)
(setq wg-prefix-key  (kbd "C-x w"))

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-copies 'always
      ddired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")

(defadvice shell-command (after shell-in-new-buffer (command &optional output-buffer error-buffer))
  (when (get-buffer "*Async Shell Command*")
    (with-current-buffer "*Async Shell Command*"
      (rename-uniquely))))
(ad-activate 'shell-command)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

(fill-keymap ibuffer-mode-map
             "j" 'ibuffer-forward-line
             "k" 'ibuffer-backward-line
             "J" 'ibuffer-jump-to-buffer
             "K" 'ibuffer-do-kill-lines)

(toggle-diredp-find-file-reuse-dir 1)
(setq dired-dwim-target t)

(electric-pair-mode 1)

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(setq org-src-fontify-natively t)

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\[ . ?\])))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))
(ac-config-default)

(defun ac-c-mode-setup ()
  (setq clang-complete-executable (concat user-emacs-directory "clang-compete"))
  (setq ac-sources '(ac-source-clang-async))
  (launch-completion-proc))

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(setq ctypes-write-types-at-exit t)
(ctypes-read-file nil nil t t)
(ctypes-auto-parse-mode 1)

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

(column-number-mode 1)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))

(key-chord-mode 1)
;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf! If using Tramp a lot.
(recentf-mode t)
(setq recentf-max-saved-items 100)
(run-with-timer (* 20 60) (* 2 60 60) (lambda () (recentf-save-list)))

(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'java-mode-hook
          '(lambda ()
             (eclim-mode 1)
             (ac-emacs-eclim-config)
             ;; Eclim uses help to display errors
             (setq help-at-pt-display-when-idle t)
             (setq eclimd-default-workspace "~/workspace")
             (setq eclim-executable "/usr/share/eclipse/eclim")
             (setq eclim-auto-save t)
             (setq eclim-print-debug-messages t)
             (setq help-at-pt-timer-delay 0.1)
             (help-at-pt-set-timer)
             (java-mode-indent-annotations-setup)
             (custom-set-variables
              '(eclim-eclipse-dirs '("/usr/share/eclipse")))
             (setq c-basic-offset 4)))

(setq tramp-default-method "ssh")

(setq mediawiki-mode-hook
      (lambda ()
        (visual-line-mode 1)
        (turn-off-auto-fill)
        (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
        (define-key mediawiki-mode-map (kbd "C-c g") 'mediawiki-reload)
        (define-key mediawiki-mode-map (kbd "C-c <ret>") 'mediawiki-open-page-at-point)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-h") 'mediawiki-insert-header)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-e") 'mediawiki-insert-sub-header)))

(setq enable-recursive-minibuffers t)

(setq recentf-exclude '("\\.recentf"
                        "\\.ido\\.last"
                        "\\.keychain/.*?-sh\\(-gpg\\)?"))

(setq sr-speedbar-right-side nil)

(setq ace-jump-mode-scope 'window)

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


(add-auto-mode 'markdown-mode "\\.md\\'")

(autoload 'crontab-mode "crontab-mode" "Mode for editing crontab files" t)
(add-to-list 'auto-mode-alist '( "\\.?cron\\(tab\\)?\\'" . crontab-mode))

(setq deft-extension "org"
      deft-directory "~/org/deft/"
      deft-text-mode 'org-mode)

(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'sass-mode-hook 'rainbow-turn-on)
(add-hook 'scss-mode-hook 'rainbow-turn-on)
(add-hook 'haml-mode-hook 'rainbow-turn-on)

(setq css-indent-offset 2)

(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)
(setq command-frequency-table-file (concat user-emacs-directory ".emacs-frequencies"))

(windmove-default-keybindings)
(setq windmove-wrap-around t)
