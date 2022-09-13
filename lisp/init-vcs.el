(require-package 'git-modes)
(require-package 'gist)
(require-package 'magit)
(require-package 'git-gutter-fringe)
(require-package 'git-wip-timemachine)
(require-package 'ido-completing-read+)
(global-git-gutter-mode t)

(magit-wip-after-apply-mode 1)
(magit-wip-after-save-mode 1)
(magit-wip-before-change-mode 1)

(global-git-commit-mode 1)

(after-load 'projectile
  (setq magit-repo-dirs
        (mapcar
         (lambda (dir)
           (substring dir 0 -1))
         (cl-remove-if-not
          (lambda (project)
            (unless (file-remote-p project)
              (file-directory-p (concat project "/.git/"))))
          (projectile-relevant-known-projects)))))

(setq vc-follow-symlinks t)

(add-auto-mode 'gitconfig-mode "gitconfig$")

(add-hook 'git-commit-mode-hook 'turn-on-flyspell)

(setq magit-diff-refine-hunk nil
      magit-repository-directories (list "~/git")
      magit-save-repository-buffers 'dontask
      magit-completing-read-function 'magit-ido-completing-read)

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

(defun github-repo? ()
  "T if the current buffer is visiting a file in a git repository
  with github as upstream."
  (when (magit-git-repo-p (locate-dominating-file (buffer-file-name) ".git"))
    (when-let (origin (magit-get "remote" "origin" "url"))
      (string-match "github" origin ))))

(provide 'init-vcs)
