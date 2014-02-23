(require-package 'git-messenger)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-commit-mode)
(require-package 'git-blame)
(require-package 'gist)
(require-package 'magit-gh-pulls)
(require-package 'magit)

(global-magit-wip-save-mode 1)

(setq vc-follow-symlinks t)

(add-auto-mode 'gitconfig-mode "gitconfig$")

(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

(setq magit-commit-signoff nil
      magit-save-some-buffers nil
      magit-diff-refine-hunk nil
      magit-wip-echo-area-message "Wrote %r"
      magit-repo-dirs '("~/git")
      magit-completing-read-function 'magit-ido-completing-read
      magit-remote-ref-format 'remote-slash-name
      magit-commit-all-when-nothing-staged nil
      magit-stage-all-confirm nil
      magit-unstage-all-confirm nil)


(evil-add-hjkl-bindings magit-commit-mode-map 'emacs
  "h" 'magit-key-mode-popup-diff-options
  "l" 'magit-key-mode-popup-logging
  (kbd "C-w") 'evil-window-map)

(fill-keymap magit-log-mode-map
             "j" 'magit-goto-next-section
             "k" 'magit-goto-previous-section
             "C-w" 'evil-window-map)

(evil-add-hjkl-bindings magit-diff-mode-map 'emacs)

(evil-add-hjkl-bindings git-rebase-mode-map 'emacs
  "K" 'git-rebase-kill-line
  "h" 'describe-mode)

(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "C" 'magit-key-mode-popup-committing
  "c" 'magit-commit
  "W" 'magit-toggle-whitespace
  (kbd "C-w") 'evil-window-map
  "M-w" 'magit-copy-item-as-kill
  "K" 'magit-discard-item
  (kbd "C-x C-k") 'magit-kill-file-on-line
  (kbd "C-c C-a") 'magit-commit-extend
  "l" 'magit-key-mode-popup-logging
  ":" 'magit-git-command
  "h" 'magit-toggle-diff-refine-hunk)

(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (progn
        (magit-dont-ignore-whitespace)
        (message "Showing whitespace in diffs..."))
    (magit-ignore-whitespace)
    (message "Ignoring whitespace in diffs...")))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice magit-mode-quit-window (after magit-restore-screen activate)
  (jump-to-register :magit-fullscreen))

(defadvice gist-region-or-buffer (around url-to-clipboard activate)
  "gist region or buffer does not play well with Evil-mode.  Advice it
  to put the URL into the clipboard, where previously
  was the content of REGION."
  (when (evil-visual-state-p)
    (let ((p (point))
          (m (mark)))
      (evil-insert-state)
      (goto-char p)
      (set-mark m)))
  ad-do-it
  (evil-normal-state))

(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

(defun vc-annotate-quit ()
  "Restores the previous window configuration and kills the vc-annotate buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

(eval-after-load "vc-annotate"
  '(progn
     (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))

     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

(provide 'init-vcs)
