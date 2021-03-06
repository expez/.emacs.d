(require-package 'wgrep-ag)
(require-package 'ag)
(require 'wgrep)
(require 'ag)

(setq wgrep-auto-save-buffer t)

(defun project-relative-path (path)
  "Returns a path relative to the project root"
  (let ((root (or (projectile-project-root) "")))
    (s-chop-prefix root path)))

(defun my-ag (string dir &optional regexp)
  (let ((ag-ignore-list (mapcar #'project-relative-path
                                (append (projectile-ignored-directories)
                                        (projectile-ignored-files)))))
    (ag/search string directory :regexp regexp)))

;;; `ag-regexp' and `ag' have been altered to take
;;; `projectile-ignored-directories' and `projectile-ignored-files'
;;; into account when searching.

;; This version has a prompt seeded with thing at point
(defun ag-regexp (string directory)
  "Search using ag in a given directory for a given regexp.
The regexp should be in PCRE syntax, not Emacs regexp syntax.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (read-from-minibuffer "Search regexp: " (ag/dwim-at-point))
                     (read-directory-name "Directory: ")))
  (my-ag string directory :regexp))

(defun ag (string directory)
  "Search using ag in a given DIRECTORY for a given search STRING,
with STRING defaulting to the symbol under point.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))
                     (read-directory-name "Directory: ")))
  (my-ag string directory))

(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(advice-add 'wgrep-change-to-wgrep-mode :after #'evil-normal-state)

(evil-define-key 'motion ag-mode-map
  (kbd "C-c C-e") #'wgrep-change-to-wgrep-mode
  (kbd "k") #'compilation-previous-error
  (kbd "j") #'compilation-next-error
  (kbd "C-j") #'compile-goto-error
  (kbd "q") #'kill-buffer-dont-ask)

(setq ag-reuse-buffers 't)

(defun rgrep-fullscreen (regexp &optional files dir confirm)
  "Open grep in full screen, saving windows."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (ido-read-directory-name "Base directory: "
                                              nil default-directory t))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (window-configuration-to-register ?$)
  (rgrep regexp files dir confirm)
  (switch-to-buffer "*grep*")
  (goto-char (point-min))
  (beginning-of-buffer))

(defun rgrep-quit-window ()
  (interactive)
  (kill-buffer)
  (jump-to-register ?$))

(defun rgrep-goto-file-and-close-rgrep ()
  (interactive)
  (compile-goto-error)
  (kill-buffer "*grep*")
  (delete-other-windows)
  (message "Type C-x r j $ to return to pre-rgrep windows."))

(defvar git-grep-switches "--extended-regexp -I -n"
  "Switches to pass to `git grep'.")

(defun git-grep-fullscreen (regexp &optional files dir confirm)
  (interactive
   (let* ((regexp (grep-read-regexp))
          (files (grep-read-files regexp))
          (files (if (string= "* .*" files) "*" files))
          (dir (ido-read-directory-name "Base directory: "
                                        nil default-directory t))
          (confirm (equal current-prefix-arg '(4))))
     (list regexp files dir confirm)))
  (let ((command (format "cd %s && git --no-pager grep %s %s -e %S -- '%s' "
                         dir
                         git-grep-switches
                         (if (s-lowercase? regexp) " --ignore-case" "")
                         regexp
                         files))
        (grep-use-null-device nil))
    (when confirm
      (setq command (read-shell-command "Run git-grep: " command 'git-grep-history)))
    (window-configuration-to-register ?$)
    (grep command)
    (switch-to-buffer "*grep*")
    (delete-other-windows)
    (goto-char (point-min))))

(eval-after-load "grep"
  '(progn
     ;; Don't recurse into some directories
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "node_modules")
     (add-to-list 'grep-find-ignored-directories "vendor")

     ;; Add custom keybindings
     (define-key grep-mode-map "q" 'rgrep-quit-window)
     (define-key grep-mode-map (kbd "C-<return>") 'rgrep-goto-file-and-close-rgrep)
     (define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers)

     ;; Use same keybinding as occur
     (setq wgrep-enable-key "e")))

;; Command to add cursor to all matches in wgrep

(defvar grep-match-positions nil)
(make-variable-buffer-local 'grep-match-positions)

(defun grep-register-match-positions ()
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Register all positions of matches
        (while (re-search-forward "\033\\[0?1;31m\\(.*?\\)\033\\[[0-9]*m" end 1)
          (add-to-list 'grep-match-positions (set-marker (make-marker) (match-beginning 1))))))))

(eval-after-load "grep"
  '(defadvice grep-mode (after grep-register-match-positions activate)
     (add-hook 'compilation-filter-hook 'grep-register-match-positions nil t)))

(provide 'init-grep)
