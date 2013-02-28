(require 'git-commit-mode)
(eval-after-load "git-wip-mode"
 (lambda nil
   ((git-wip-mode 1))))

(setq vc-handled-backends '(SVN)
      vc-follow-symlinks t)

(add-auto-mode 'gitignore-mode "\\.gitignore$")
(add-auto-mode 'gitconfig-mode "\\.gitconfig$")

(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'magit-mode-hook 'turn-on-magit-push-remote)

(setq magit-commit-signoff nil
      magit-process-popup-time 10
      magit-save-some-buffers nil
      magit-diff-refine-hunk t
      magit-wip-mode t
      magit-wip-echo-area-message "Wrote %r"
      magit-repo-dirs '("~/git")
      magit-completing-read-function 'magit-ido-completing-read
      magit-remote-ref-format 'remote-slash-name
      magit-commit-all-when-nothing-staged nil)


(eval-after-load "magit"
  '(progn
     (fill-keymap magit-status-mode-map
                  "W" 'magit-toggle-whitespace
                  "q" 'magit-quit-session)

     (fill-keymap magit-log-mode-map
                  "j" 'magit-goto-next-section
                  "k" 'magit-goto-previous-section)

     (eval-after-load "evil"
       '(progn
          (evil-add-hjkl-bindings rebase-mode 'emacs
            "h" 'describe-mode)

          (evil-add-hjkl-bindings magit-status-mode-map 'emacs
            "K" 'magit-discard-item
            "l" 'magit-key-mode-popup-logging
            "h" 'magit-toggle-diff-refine-hunk)

          (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
            "K" 'magit-discard-item
            "L" 'magit-key-mode-popup-logging)))))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(provide 'init-vcs)
