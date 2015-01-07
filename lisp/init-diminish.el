(require-package 'diminish)

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "projectile"
  '(diminish 'projectile-mode))

(eval-after-load "yas-minor-mode"
  '(diminish 'yas-minor-mode))

(eval-after-load "paredit"
  '(diminish 'paredit-mode))

(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode))

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))

(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))

(after-load 'workgroups2
  (diminish 'workgroups-mode))

(eval-after-load "glasses"
  '(diminish 'glasses-mode))

(eval-after-load "redshank"
  '(diminish 'redshank-mode))

(eval-after-load "elisp-slime-nav"
  '(diminish 'elisp-slime-nav-mode))

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(eval-after-load "checkdoc"
  '(diminish 'checkdoc-minor-mode))

(eval-after-load "whitespace"
  '(diminish 'whitespace-mode))

(eval-after-load 'flycheck
  '(diminish 'flycheck-mode))

(diminish 'abbrev-mode)
(eval-after-load 'auto-revert-mode
  '(diminish 'auto-revert-mode))

(eval-after-load 'magit-wip
  '(diminish 'magit-wip-save-mode))

(after-load 'magit
  (diminish 'magit-auto-revert-mode))

(after-load 'git-gutter
  (diminish 'git-gutter-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     (defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(provide 'init-diminish)
