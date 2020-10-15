(require-package 'projectile)

(projectile-global-mode)

(setq projectile-use-git-grep t
      projectile-indexing-method 'alien
      projectile-enable-caching nil
      projectile-create-missing-test-files t
      projectile-globally-ignored-directories
      (append '("bower_components" "node_modules" "build" "out" "target"
                ".cljs_rhino_repl" ".cljs_nashorn_repl" ".nashorn_code_cache")
              projectile-globally-ignored-directories))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(eval-after-load 'projectile
  '(defun projectile-toggle-between-implementation-and-test (same-window)
     "Toggle between an implementation file and its test file."
     (interactive "P")
     (if same-window
         (find-file
          (projectile-find-implementation-or-test (buffer-file-name)))
       (find-file-other-window
        (projectile-find-implementation-or-test (buffer-file-name))))))

(provide 'init-projectile)
