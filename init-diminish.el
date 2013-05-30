(require 'diminish)

(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))

(eval-after-load "eproject"
  '(diminish 'eproject-mode))

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

(eval-after-load "workgroups"
  '(diminish 'workgroups-mode))

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

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     (defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")

(provide 'init-diminish)
