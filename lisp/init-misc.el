(require-package 'unbound)
(require-package 'eldoc-eval)
(require-package 'key-chord)
(require-package 'helm)
(require-package 'info+)
(require-package 'lorem-ipsum)
(require-package 'neotree)
(require-package 'rainbow-delimiters)
(require-package 'regex-tool)
(require-package 'regex-dsl)
(require-package 'yaml-mode)
(require-package 'crontab-mode)
(require-package 'color-moccur)
(require-package 'workgroups2)
(require-package 'window-numbering)
(require-package 'solarized-theme)
(require-package 'popwin)
(require-package 'buffer-move)
(require-package 'mode-line-debug)
(require-package 'ace-window)
(require-package 'bind-key)
(require-package 'helm-pages)
(require-package 'transpose-frame)
(require 'bind-key)
(require 'popwin)
(require 'ibuffer)
(require 'workgroups2)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(popwin-mode 1)
(eldoc-in-minibuffer-mode 1)
(key-chord-mode 1)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))))))

(fill-keymap ibuffer-mode-map
             "j" 'ibuffer-forward-line
             "k" 'ibuffer-backward-line
             "J" 'ibuffer-jump-to-buffer
             "K" 'ibuffer-do-kill-lines)

(setq flyspell-issue-message-flag nil)

(setq ispell-dictionary "english")
(setq ispell-process-directory (expand-file-name "~/"))
(setq ispell-program-name "aspell") ;;Use aspell dictionaries.
(setq ispell-list-command "--list") ;;Faster region checking, "--list" for aspell, "-l" for ispell.

;;Ultra twice as slow as ispell, fast twice as slow as ultra, normal 10x slower than fast.
;;(setq ispell-extra-args '("--sug-mode=ultra"))

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

(setq wg-use-default-session-file t
      wg-default-session-file (concat user-emacs-directory "workgroups"))
(setq wg-prefix-key (kbd "C-x w"))
(workgroups-mode 1)

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
    ;; why? Some ido commands, such as textmate.el's textmate-goto-symbol don't switch the current buffer
    (switch-to-buffer this-buffer)
    result))

(eval-after-load 'ido
  '(mapc (lambda (map)
           (define-key map (kbd "C-o") 'ido-invoke-in-other-window)
           (define-key map (kbd "C-2") 'ido-invoke-in-vertical-split)
           (define-key map (kbd "C-3") 'ido-invoke-in-horizontal-split)
           (define-key map (kbd "C-4") 'ido-invoke-in-other-window)
           (define-key map (kbd "C-5") 'ido-invoke-in-new-frame))
         (list ido-buffer-completion-map
               ido-common-completion-map
               ido-file-completion-map
               ido-file-dir-completion-map)))

;; stolen from https://github.com/pkkm/.emacs.d/blob/master/conf/minibuffer/ido.el

;; Ido provides the keymaps `ido-common-completion-map',
;; `ido-file-dir-completion-map', `ido-file-completion-map',
;; `ido-buffer-completion-map' for various kinds of completions.
;; However, it recreates them every time `ido-completing-read' is
;; called, so we need to define custom keys every time too.
(defun my-ido-bindings ()
  ;; `ido-completion-map' -- the current completion keymap.
  ;; `ido-cur-item' -- the type of item that is being read: file, dir, buffer or list.

  ;; Don't complete on SPC.
  (bind-key "SPC" nil ido-completion-map)

  ;; C-n, C-p -- cycle matches.
  (bind-key "C-n" #'ido-next-match ido-completion-map)
  (bind-key "C-p" #'ido-prev-match ido-completion-map)

  ;; C-w, C-backspace -- delete the word before point.
  (if (memq ido-cur-item '(file dir))
      (progn
        (bind-key "C-w" #'ido-delete-backward-word-updir ido-completion-map)
        (bind-key "<C-backspace>" #'ido-delete-backward-word-updir ido-completion-map))
    (bind-key "C-w" #'backward-kill-word ido-completion-map))

  ;; C-u -- delete to the beginning of input.
  (if (memq ido-cur-item '(file dir))
      (bind-key "C-u" #'ido-delete-backward-line-updir ido-completion-map)
    (bind-key "C-u" #'backward-kill-line ido-completion-map)))
(add-hook 'ido-setup-hook #'my-ido-bindings) ; Run on every completion after keymaps have been set up.

;; Function for C-u.
(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(defun ido-delete-backward-line-updir ()
  (interactive)
  (if (= (minibuffer-prompt-end) (point))
      (ido-up-directory t)
    (backward-kill-line)))

(after-load 'iedit
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
        (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max))))))))

(after-load 'neotree
  (setq projectile-switch-project-action 'neotree-projectile-action
        neo-theme 'ascii
        neo-keymap-style 'concise)
  (fill-keymap neotree-mode-map
               (kbd ",") 'leader
               (kbd "k") 'previous-line
               (kbd "j") 'next-line)

  (when neo-persist-show
    (add-hook 'popwin:before-popup-hook
              (lambda () (setq neo-persist-show nil)))
    (add-hook 'popwin:after-popup-hook
              (lambda () (setq neo-persist-show t)))))

(defun neotree-toggle-or-change-root (p)
  "Toggle neotree, with a prefix call
`neotree-change-to-buffer-root'."
  (interactive "P")
  (if current-prefix-arg
      (neotree-change-to-buffer-root)
    (neotree-toggle)))

(defun neotree-change-to-buffer-root ()
  "Changes the `neotree-root' to be the project root of the
file the current buffer is visiting.

If no project root can be found use the dir containing the file
the buffer is visiting."
  (neo-buffer--change-root (projectile-project-root)))

(mode-line-debug-mode 1)

(provide 'init-misc)
