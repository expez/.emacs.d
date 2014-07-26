(require-package 'unbound)
(require-package 'eldoc-eval)
(require-package 'key-chord)
(require-package 'mmm-mode)
(require-package 'mediawiki)
(require-package 'bookmark+)
(require-package 'helm)
(require-package 'ido-ubiquitous)
(require-package 'ido-vertical-mode)
(require-package 'info+)
(require-package 'lorem-ipsum)
(require-package 'project-explorer)
(require-package 'rainbow-delimiters)
(require-package 'regex-tool)
(require-package 'regex-dsl)
(require-package 'yaml-mode)
(require-package 'crontab-mode)
(require-package 'color-moccur)
(require-package 'workgroups2)
(require-package 'flx-ido)
(require-package 'window-numbering)
(require-package 'ido-hacks)
(require-package 'solarized-theme)
(require-package 'popwin)
(require-package 'buffer-move)
(require 'popwin)
(require 'mmm-auto)
(require 'ibuffer)
(require 'workgroups2)
(require 'ido-hacks)
(require 'project-explorer)
(require 'buffer-move)

(popwin-mode 1)
(eldoc-in-minibuffer-mode 1)
(ido-vertical-mode)
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
(setq ispell-list-command "--list") ;;Faster region checking, "--list" for aspell, "-l" for ispell.

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

(windmove-default-keybindings)
(setq windmove-wrap-around t)

(window-numbering-mode 1)

(setq helm-ff-default-directory "~/git")

(setq wg-use-default-session-file nil
 wg-default-session-file (concat user-emacs-directory "workgroups"))
(setq wg-prefix-key (kbd "C-x w"))
(workgroups-mode 1)

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

(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

(defadvice httpd-start (around set-httpd-root-with-prefix activate)
  (if current-prefix-arg
      (let ((dir (file-name-directory (or (buffer-file-name) user-emacs-directory))))
        (setq httpd-root (ido-read-directory-name "Serve dir: " dir))
        ad-do-it)
    ad-do-it))

(defun net-utils-restore-windows ()
  "Restore windows and clean up after ping."
  (interactive)
  (kill-buffer (current-buffer))
  (jump-to-register :net-utils-fullscreen))

(defadvice net-utils-run-program (around net-utils-big-page activate)
  (window-configuration-to-register :net-utils-fullscreen)
  (let ((buf ad-do-it))
    (switch-to-buffer buf)
    (delete-other-windows)
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "q") 'net-utils-restore-windows)
       map))
    (message "Type \"q\" to restore other windows.")))

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

(defun split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun ido-invoke-in-other-window ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

(defun ido-invoke-in-horizontal-split ()
  "signals ido mode to split horizontally and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun ido-invoke-in-vertical-split ()
  "signals ido mode to split vertically and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defadvice ido-read-internal (around ido-read-internal-with-minibuffer-other-window activate)
  (let* (ido-exit-minibuffer-target-window
         (this-buffer (current-buffer))
         (result ad-do-it))
    (cond
     ((equal ido-exit-minibuffer-target-window 'other)
      (if (= 1 (count-windows))
          (split-window-horizontally-and-switch)
        (other-window 1)))
     ((equal ido-exit-minibuffer-target-window 'horizontal)
      (split-window-horizontally-and-switch))

     ((equal ido-exit-minibuffer-target-window 'vertical)
      (split-window-vertically-and-switch))
     ((equal ido-exit-minibuffer-target-window 'frame)
      (make-frame)))
    (switch-to-buffer this-buffer) ;; why? Some ido commands, such as textmate.el's textmate-goto-symbol don't switch the current buffer
    result))

(defadvice ido-init-completion-maps (after ido-init-completion-maps-with-other-window-keys activate)
  (mapcar (lambda (map)
            (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
            (define-key map (kbd "C-2") 'ido-invoke-in-vertical-split)
            (define-key map (kbd "C-3") 'ido-invoke-in-horizontal-split)
            (define-key map (kbd "C-4") 'ido-invoke-in-other-window)
            (define-key map (kbd "C-5") 'ido-invoke-in-new-frame))
          (list ido-buffer-completion-map
                ido-common-completion-map
                ido-file-completion-map
                ido-file-dir-completion-map)))

(setq pe/omit-regex "^\\.\\|^#\\|~$\\|node_modules")

(defun toggle-project-explorer ()
  (interactive)
  (-if-let* ((pe-buffer (pe/get-current-project-explorer-buffer))
             (pe-window (get-buffer-window pe-buffer)))
      (progn (delete-window pe-window)
             (popwin-mode 1))
    (project-explorer-open)
    (popwin-mode 0)))
