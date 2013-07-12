(require 'workgroups)
(require 'multiple-cursors)
(require 'color-moccur)
(require 'cuda-mode)
(require 'diminish)
(require 'go-autocomplete)
(require 'highlight-tags-mode)
(require 'info+)
(require 'lorem-ipsum)
(require 'mediawiki)
(require 'window-numbering)
(require 'ido-hacks)

(key-chord-mode 1)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

(fill-keymap ibuffer-mode-map
             "j" 'ibuffer-forward-line
             "k" 'ibuffer-backward-line
             "J" 'ibuffer-jump-to-buffer
             "K" 'ibuffer-do-kill-lines)

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

(setq flyspell-issue-message-flag nil)

(setq ispell-dictionary "english")
(setq ispell-process-directory (expand-file-name "~/"))
(setq ispell-program-name "aspell") ;;Use aspell dictionaries.
(setq ispell-list-command "list") ;;Faster region checking, "list" for aspell, "-1" for ispell.

;;Ultra twice as slow as ispell, fast twice as slow as ultra, normal 10x slower than fast.
;;(setq ispell-extra-args '("--sug-mode=ultra"))

(setq mediawiki-mode-hook
      (lambda ()
        (visual-line-mode 1)
        (turn-off-auto-fill)
        (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
        (define-key mediawiki-mode-map (kbd "C-c g") 'mediawiki-reload)
        (define-key mediawiki-mode-map (kbd "C-c <ret>") 'mediawiki-open-page-at-point)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-h") 'mediawiki-insert-header)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-e") 'mediawiki-insert-sub-header)))

(add-to-list 'auto-mode-alist '( "\\.?cron\\(tab\\)?\\'" . crontab-mode))
(autoload 'crontab-mode "crontab-mode" "Mode for editing crontab files" t)

(setq ace-jump-mode-scope 'window)

(setf ace-jump-mode-move-keys
      (nconc (loop for i from ?a to ?z collect i)
             (loop for i from ?A to ?Z collect i)
             (loop for i from ?0 to ?9 collect i)
             (loop for i from ?! to ?/ collect i)
             (loop for i from ?: to ?@ collect i)))

(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'sass-mode-hook 'rainbow-turn-on)
(add-hook 'scss-mode-hook 'rainbow-turn-on)
(add-hook 'haml-mode-hook 'rainbow-turn-on)

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(window-numbering-mode 1)

(setq helm-ff-default-directory "~/git")

(workgroups-mode 1)
(setq wg-morph-on nil
      wg-file (concat user-emacs-directory "workgroups"))
(wg-load wg-file)

(ido-hacks-mode)

(defadvice undo-tree-undo (around keep-region activate)
  "Keep region when undoing in region"
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))
