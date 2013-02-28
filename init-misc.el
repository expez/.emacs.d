(require 'workgroups)
(require 'command-frequency)
(require 'color-moccur)
(require 'color-theme)
(require 'cuda-mode)
(require 'diminish)
(require 'go-autocomplete)
(require 'highlight-tags-mode)
(require 'info+)
(require 'lorem-ipsum)
(require 'mediawiki)

(key-chord-mode 1)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

(fill-keymap ibuffer-mode-map
             "j" 'ibuffer-forward-line
             "k" 'ibuffer-backward-line
             "J" 'ibuffer-jump-to-buffer
             "K" 'ibuffer-do-kill-lines)

;; Update the buffer list for uniquify when needed
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

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

(setq mediawiki-mode-hook
      (lambda ()
        (visual-line-mode 1)
        (turn-off-auto-fill)
        (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
        (define-key mediawiki-mode-map (kbd "C-c g") 'mediawiki-reload)
        (define-key mediawiki-mode-map (kbd "C-c <ret>") 'mediawiki-open-page-at-point)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-h") 'mediawiki-insert-header)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-e") 'mediawiki-insert-sub-header)))

(add-auto-mode 'markdown-mode "\\.md\\'")

(add-to-list 'auto-mode-alist '( "\\.?cron\\(tab\\)?\\'" . crontab-mode))
(autoload 'crontab-mode "crontab-mode" "Mode for editing crontab files" t)

(setq ace-jump-mode-scope 'window)

(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'sass-mode-hook 'rainbow-turn-on)
(add-hook 'scss-mode-hook 'rainbow-turn-on)
(add-hook 'haml-mode-hook 'rainbow-turn-on)

(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)
(setq command-frequency-table-file (concat user-emacs-directory ".emacs-frequencies"))

(windmove-default-keybindings)
(setq windmove-wrap-around t)
