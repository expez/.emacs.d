(require-package 'projectile)

(projectile-global-mode)

(setq projectile-use-git-grep t
      projectile-indexing-method 'alien
      projectile-enable-caching nil)
(setq projectile-globally-ignored-directories
      (append '("bower_components" "node_modules" "build") projectile-globally-ignored-directories))
(provide 'init-projectile)
