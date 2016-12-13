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
  ;; workgroups mode isn't actually added to minor-mode-alist until
  ;; the mode is turned on.
  (run-with-timer 5 nil (lambda ()
                          (when workgroups-mode
                            (diminish 'workgroups-mode)))))

(eval-after-load "glasses"
  '(diminish 'glasses-mode))

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

(after-load 'git-gutter
  (diminish 'git-gutter-mode))

(after-load 'fic-ext-mode
  (diminish 'fic-ext-mode))

(after-load 'company
  (diminish 'company-mode))

(after-load 'evil-smartparens
  (diminish 'evil-smartparens-mode))

(after-load 'smartparens
  (diminish 'smartparens-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     (defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(provide 'init-diminish)
