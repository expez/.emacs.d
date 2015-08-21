(require-package 'git-messenger)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'gist)
(require-package 'magit-gh-pulls)
(require-package 'magit)
(require-package 'git-gutter-fringe)
(require-package 'git-wip-timemachine)
(global-git-gutter-mode t)

(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

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

(when (fboundp 'magit-gh-pulls-mode)
  (eval-after-load 'magit
    '(define-key magit-mode-map "#gg"
       #'endless/load-gh-pulls-mode))

  (defun endless/load-gh-pulls-mode ()
    "Start `magit-gh-pulls-mode' only after a manual request."
    (interactive)
    (require 'magit-gh-pulls)
    (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls)
    (magit-gh-pulls-mode 1)
    (magit-gh-pulls-reload)))

(defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*"))
    (unless (member
             fetch-address
             (magit-get-all "remote" "origin" "fetch"))
      (when (string-match
             "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-remote)
                       "url"))
           (cdr (magit-get-remote-branch)))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'endless/visit-pull-request-url))

(add-hook 'magit-mode-hook #'endless/add-PR-fetch)

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

(defun github-repo? ()
  "T if the current buffer is visiting a file in a git repository
  with github as upstream."
  (when (magit-git-repo-p (locate-dominating-file (buffer-file-name) ".git"))
    (when-let (origin (magit-get "remote" "origin" "url"))
      (string-match "github" origin ))))

(provide 'init-vcs)
