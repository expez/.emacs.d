(require-package 'projectile)

(projectile-global-mode)

(setq projectile-use-git-grep t
      projectile-indexing-method 'alien
      projectile-enable-caching nil
      projectile-create-missing-test-files t)
(setq projectile-globally-ignored-directories
      (append '("bower_components" "node_modules" "build" "out" "target"
                ".cljs_rhino_repl" ".cljs_nashorn_repl" ".nashorn_code_cache")
              projectile-globally-ignored-directories))
(provide 'init-projectile)
