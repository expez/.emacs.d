(load-theme 'Darkerthanblack t)

(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0               ;; ... are very ...
  scroll-down-aggressively 0             ;; ... annoying
  scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

;;Settings for uniquify
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;;Autopair
;;(autopair-global-mode 1) ;; enable autopair in all buffers

;; re-builder
(setq reb-re-syntax 'string)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Line numbering
(global-linum-mode 1)

(setq compilation-ask-about-save nil)
;;compile window smaller:
(setq compilation-window-height 20) ;;Not entirely sure I like this.
;;Close compilation window if compile was succesful.
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "Compilation errors, press C-c n to visit")

          ;;no errors, make the compilation window go away in 2 second
          (run-at-time 1 nil 'delete-windows-on buf)
          (run-at-time 1 nil 'kill-buffer buf)

          (message "Compilation succesful!"))))

;;Trailing whitespace is unnecessary.
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(setq require-final-newline 'visit-save);;Add newline at the end of files.

;;Enable trash-can
(setq delete-by-moving-to-trash t)

;; Windmove, real hackers don't use arrows to navigate with, but might use them
;; to move between buffers!
(windmove-default-keybindings 'shift) ;; use shift-arrows to move between buffers.

(show-paren-mode 1) ;; highlight parenthesis.
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis) ;; highlight parenthesis, 'expression would only highlight entire exp within paren.

;; Set parenface
(set-face-foreground 'paren-face "grey30")

;; Initial message for scratch buffer.
(setq initial-scratch-message
  ";; scratch buffer created -- happy hacking\n")

;; Cua mode is only used for the great rectangle support.
(setq cua-enable-cua-keys nil)
(cua-mode 1)

(global-font-lock-mode 1) ;;Syntax highlighting.

;;Iswitchb mode
(iswitchb-mode 1)
(setq iswitchb-default-method 'samewindow) ;;Avoid switching to another frame.

(put 'set-goal-column 'disabled nil) ;; Enable set-goal-column

;; Enable IDO mode for buffers and files, enable flex matching.
(ido-mode 'both)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)

;; No startup message.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Remove menu bar, tool bar and scroll bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;see what you are typing
(setq echo-keystrokes 0.1)

;;Set the font I like, Inconsolata. If on windows set Consolas.

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

;;Upcase region is occasionally useful!
(put 'upcase-region 'disabled nil)

;; Whole line or region mode
(whole-line-or-region-mode 1)

;;enable another previous disabled function.
(put 'downcase-region 'disabled nil)

;; Turn on debugging to get stacktrace if something goes wrong.
(setq debug-on-error t)

;; Store auto-save files to system's temp directory.
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

(setq
backup-by-copying t
delete-old-versions t
kept-new-versions 6
kept-old-versions 2
version-control t)

;; Paredit mode
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

(defadvice he-substitute-string (after he-paredit-fix)
"remove extra paren when expanding line in paredit"
(if (and paredit-mode (equal (substring str -1) ")"))
(progn (backward-delete-char 1) (forward-char))))

;;Turn on the undo tree.
(global-undo-tree-mode 1)

;;Use Emacs-w3m
;; (setq w3m-use-cookies t)
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (setq w3m-default-display-inline-image t)

;;Yasnippet
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/mysnippets")
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/dropdown-prompt
yas/ido-prompt
yas/completing-prompt))

;;Enable Smex
(smex-initialize)

;;Enable images
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

;;Ediff setup
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


;; Workgroups
(workgroups-mode 1)
(setq wg-prefix-key  (kbd "C-x w"))

;;(wg-load "/path/to/saved/workgroups")  ;;For loading file with saved workgrups.


;;Don't open a new buffer for each opened directory in Dired.
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-copies 'always);;always recursively copy.

(electric-pair-mode 1)

(defun my-make-CR-do-indent ()
(define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;;Org-mode

(setq org-src-fontify-natively t) ;; syntax hightlighting when inserting code.)


(defun my-org-mode-hook ()
(local-set-key (kbd "C-,") 'beginning-of-buffer)
(local-set-key "\M-up" 'windmove-up)
(local-set-key "\M-right" 'windmove-right)
(local-set-key "\M-left" 'windmove-left)
(local-set-key "\M-down" 'windmove-down))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;;AucTeX config

(setq TeX-autosave t
TeX-parse-self t
TeX-PDF-mode t
refTeX-plug-into-AUCTeX t)
(setq-default text-master nil)
;;(add-hook 'LaTeX-mode-hook (setq TeX-master (guess-tex-master (buffer-file-name)))) ;;Guess which  file is the master.


(defun my-LaTeX-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (TeX-source-correlate-mode 1)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (local-set-key (kbd "C-x c") 'TeX-command-master)
  (local-set-key (kbd "C-c s") 'LaTeX-section)
  (local-set-key (kbd "C-c e") 'LaTeX-environment)
  (local-set-key (kbd "C-c i") 'LaTeX-insert-item)
  (local-set-key (kbd "C-c f") 'TeX-font)
  (local-set-key (kbd "C-,") 'beginning-of-buffer))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

(add-hook 'ace-jump-mode-before-jump-hook
(lambda () (push-mark (point) t))) ;until it's fixed in Maramalade

;;Haskell mode

(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4
        haskell-hoogle-command "hoogle"))
;; haskell-hoogle-command to "hoogle" uses local command line hoogle (cabal install hoogle)
;; setting this variable to "nil" would use haskell.org/hoogle in browser.
(defun my-haskell-mode-hook ()
   (turn-on-haskell-doc-mode)
   (turn-on-haskell-simple-indent)
   (haskell-style)
   (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
   (setq whitespace-style '(face lines-tail))
   (setq default-hpaste-nick "Expez"))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(setq electric-pair-pairs '(
                (?\" . ?\")
                (?\{ . ?\})
			    (?\' . ?\')))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(setq hippie-expand-try-functions-list (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

(column-number-mode 1)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))
