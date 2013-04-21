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
    ack-and-a-half
    auctex
    auto-complete
    backtrace-mode
    bookmark+
    bundler
    c-eldoc
    color-moccur
    color-theme-solarized
    crontab-mode
    ctypes
    deft
    diminish
    dired+
    dropdown-list
    eldoc-eval
    elisp-slime-nav
    emacs-eclim
    eproject
    eshell-manual
    ethan-wspace
    evil
    evil-leader
    evil-numbers
    evil-paredit
    flycheck
    ghc
    ghci-completion
    gist
    git-blame
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    google-c-style
    goto-chg
    haskell-mode
    helm
    ido-ubiquitous
    inf-ruby
    info+
    key-chord
    lorem-ipsum
    macrostep
    magit
    magit-gh-pulls
    magit-push-remote
    magithub
    markdown-mode
    mediawiki
    move-text
    multiple-cursors
    paredit
    parenface
    pretty-mode-plus
    rainbow-delimiters
    rainbow-mode
    redshank
    regex-dsl
    regex-tool
    register-list
    rinari
    robe
    rspec-mode
    ruby-compilation
    ruby-electric
    ruby-interpolation
    rvm
    sr-speedbar
    surround
    toml-mode
    unbound
    undo-tree
    workgroups
    yaml-mode
    yard-mode
    yasnippet
    wgrep-ack
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'init-package)
