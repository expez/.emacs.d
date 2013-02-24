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

(winner-mode 1)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;;Add newline at the end of files.
(setq require-final-newline 'visit-save)

(setq delete-by-moving-to-trash t)

(windmove-default-keybindings 'shift)

(show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis) ;; highlight parenthesis, 'expression would only highlight entire exp within paren.

(set-face-foreground 'paren-face "grey30")

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

(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

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

(defun my-ac-config ()
  (add-hook 'c-mode-hook 'ac-c-mode-setup)
  (add-hook 'LaTeX-mode-hook #'ac-l-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode)
  (setq ac-auto-start 2)
  (setq ac-quick-help-delay 0.5)
  (setq ac-auto-show-menu 0.2)
  (setq ac-fuzzy-enable t)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (ac-flyspell-workaround))

(my-ac-config)

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

(column-number-mode 1)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))

(evil-mode 1)
(setq evil-default-cursor t)
(setq evil-want-visual-char-semi-exclusive t)
(global-surround-mode 1)
(setq evil-insert-state-cursor '("red" hbar))
(setq evil-normal-state-cursor '("white" box))
(setq evil-visual-state-cursor '("green" box))
(setq evil-find-skip-newlines t)
(setq evil-move-cursor-back nil
      evil-cross-lines t
      evil-want-C-u-scroll t
      evil-ex-hl-update-delay 0.01)
(setq evil-leader/in-all-states t)
(evil-leader/set-leader ",")

(key-chord-mode 1)
;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf! If using Tramp a lot.
(recentf-mode t)
(setq recentf-max-saved-items 100)
(run-with-timer (* 20 60) (* 2 60 60) (lambda () (recentf-save-list)))

(turn-on-ex-mode)

(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
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

(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (slime-setup '(slime-fancy))
            (slime-mode 1)
            (turn-on-eldoc-mode)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)
            (rainbow-delimiters-mode 0)
            (evil-paredit-mode 1)
            (set-face-foreground 'paren-face "grey30")))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-mode-hook 'cliki:start-slime)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (paredit-mode +1)
            (evil-paredit-mode 1)))

(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))

(add-auto-mode 'gitignore-mode "\\.gitignore$")
(add-auto-mode 'gitconfig-mode "\\.gitconfig$")

(set-register ?c '(file . "~/.emacs.d/plugins/my-config.el"))
(set-register ?e '(file . "~/.emacs.d/plugins/ex-mode.el"))
(set-register ?i '(file . "~/.emacs.d/init.el"))

(setq recentf-exclude '("\\.recentf"
                        "\\.ido\\.last"
                        "\\.keychain/.*?-sh\\(-gpg\\)?"))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-mode 0)
            (evil-paredit-mode 1)
            (elisp-slime-nav-mode 1)
            (define-key evil-normal-state-local-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)))

(setq sr-speedbar-right-side nil)

(diminish 'yas-minor-mode)
(diminish 'paredit-mode)
(diminish 'undo-tree-mode)

(defadvice evil-goto-definition (around evil-clever-goto-def activate)
  "Make use of emacs', slime's and etags possibilities for finding definitions."
  (case major-mode
    (lisp-mode (if slime-mode
                   (or (slime-find-definitions (symbol-name (symbol-at-point)))
                       ad-do-it)
                 ad-do-it))
    (emacs-lisp-mode (condition-case nil
                         (find-function (symbol-at-point))
                       (error (condition-case nil
                                  (find-variable (symbol-at-point))
                                (error ad-do-it)))))
    (otherwise
     (let ((tag (symbol-name (symbol-at-point))))
       (if (and (boundp 'gtags-mode) gtags-mode)
           (gtags-goto-tag tag nil)
         (if (and tags-file-name (find-tag-noselect tag))
             (find-tag tag)
           ad-do-it))))))

(setq ace-jump-mode-scope 'window)

(defmacro evil-enclose-ace-jump (&rest body)
  `(let ((old-mark (mark)))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (progn
           ,@body
           (recursive-edit))
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (evil-enclose-ace-jump
   (ace-jump-mode 9)))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 1)))

(evil-define-motion evil-ace-jump-char-to-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)
   (forward-char -1)))

(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

(defun set-mode-to-default-emacs (mode)
  (evil-set-initial-state mode 'emacs))

(mapcar 'set-mode-to-default-emacs
        '(dired
          shell-mode
          inferior-emacs-lisp-mode
          term-mode
          eshell-mode
          slime-repl-mode
          occur-mode
          magit-branch-manager-mode
          magit-commit-mode
          magit-log-mode
          log-view-mode))

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
      deft-directory "~/Org/deft/"
      deft-text-mode 'org-mode)

(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'sass-mode-hook 'rainbow-turn-on)
(add-hook 'scss-mode-hook 'rainbow-turn-on)
(add-hook 'haml-mode-hook 'rainbow-turn-on)

(setq css-indent-offset 2)

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(defcustom elisp-programming-major-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    ielm-mode)
  "Mode that are used to do Elisp programming.")

(dolist (mode elisp-programming-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   (lambda ()
     (turn-on-eldoc-mode)
     (rainbow-delimiters-mode 0)
     (set-face-foreground 'paren-face "grey30"))))
