(require 'package)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/" )
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages
  '(
    ac-slime
    ace-jump-mode
    auctex
    auto-complete
    backtrace-mode
    bookmark+
    bundler
    c-eldoc
    color-moccur
    command-frequency
    crontab-mode
    ctypes
    diminish
    dired+
    dropdown-list
    elisp-slime-nav
    emacs-eclim
    eproject
    evil
    evil-leader
    evil-numbers
    evil-paredit
    flycheck
    ghc
    ghci-completion
    gist
    git-blame
    git-commit
    gitconfig-mode
    gitignore-mode
    google-c-style
    haskell-mode
    ido-ubiquitous
    inf-ruby
    info+
    key-chord
    lorem-ipsum
    magit
    magithub
    markdown-mode
    mediawiki
    move-text
    paredit
    parenface
    rainbow-delimiters
    rainbow-mode
    regex-dsl
    regex-tool
    register-list
    rinari
    robe
    rspec-mode
    ruby-compilation
    ruby-electric
    ruby-interpolation
    rvm sr-speedbar surround
    unbound
    undo-tree
    workgroups
    yaml-mode
    yard-mode
    yasnippet
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'init-package)
