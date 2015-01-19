(require-package 'projectile)

(projectile-global-mode)

(setq projectile-use-git-grep t
      projectile-indexing-method 'alien)
(setq projectile-globally-ignored-directories
      (append '("bower_components") projectile-globally-ignored-directories))
(provide 'init-projectile)
