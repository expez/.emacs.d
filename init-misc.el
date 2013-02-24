(require 'ac-emacs-eclim-source)
(require 'command-frequency)
(require 'color-moccur)
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
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(ido-ubiquitous-mode 1)
(ido-hacks-mode)

;; re-builder
(setq reb-re-syntax 'string)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;;Add newline at the end of files.
(setq require-final-newline 'visit-save)

(setq delete-by-moving-to-trash t)

(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis) ;; highlight parenthesis, 'expression would only highlight entire exp within paren.

(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n")

(put 'set-goal-column 'disabled nil)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq echo-keystrokes 0.1)

(if (eq system-type 'windows-nt)
    (set-default-font
     "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

(if (eq system-type 'gnu/linux)
    (set-default-font "Inconsolata-12"))

;; Would call Windows command interpreter. Change it.

;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)

;; Remove C-m (^M) characters that appear in output

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)

(setq debug-on-error t)

(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

(make-directory "~/.emacs.d/autosaves/" t)

(setq vc-make-backup-files t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defadvice he-substitute-string (after he-paredit-fix)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

(global-undo-tree-mode 1)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

(yas-global-mode 1)
(setq yas-trigger-key nil)
(yas/reload-all) ;; Needed to disable trigger key
(setq yas/root-directory "~/.emacs.d/mysnippets")
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

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

(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;;Don't want the control frame.
(setq diff-switches "-u") ;;Use unified format
(setq ediff-custom-diff-options "-U3") ;;3 lines of context.

;;Save window configuration prior to ediff, so we can jump to it from ediff if needed,
;;restore the previous window configuration when ediff terminates.
;;Taken from emacswiki/ediffmode
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
                  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
                  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)
(add-hook 'ediff-cleanup-hook (lambda () (ediff-janitor nil nil)))

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

(toggle-diredp-find-file-reuse-dir 1)
(setq dired-dwim-target t)

(electric-pair-mode 1)

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(setq org-src-fontify-natively t)

(setq TeX-autosave t
      TeX-save-query nil
      TeX-parse-self t
      TeX-PDF-mode t
      TeX-newline-function #'reindent-then-newline-and-indent
      refTeX-plug-into-AUCTeX t)
(setq-default text-master 'dwim)

(defun my-LaTeX-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (TeX-source-correlate-mode 1)
  (orgtbl-mode)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (local-set-key (kbd "C-x c") 'TeX-command-master)
  (local-set-key (kbd "C-c s") 'LaTeX-section)
  (local-set-key (kbd "C-c e") 'LaTeX-environment)
  (local-set-key (kbd "C-c i") 'LaTeX-insert-item)
  (local-set-key (kbd "C-c f") 'TeX-font))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\[ . ?\])))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(defun ac-c-mode-setup ()
  (setq clang-complete-executable "~/.emacs.d/plugins/clang-complete")
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

(diminish 'yas-minor-mode)
(diminish 'paredit-mode)
(diminish 'undo-tree-mode)
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

(setf eproject-completing-read-function 'eproject--ido-completing-read)

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
