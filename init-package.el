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
    ac-nrepl
    ace-jump-mode
    auto-complete-nxml
    ack-and-a-half
    align-cljlet
    auctex
    auto-complete
    backtrace-mode
    bookmark+
    bundler
    c-eldoc
    cider
    cider-tracing
    cider-decompile
    clojure-cheatsheet
    clj-refactor
    clojure-mode
    clojure-test-mode
    color-moccur
    crontab-mode
    ctypes
    deft
    diminish
    dired+
    dropdown-list
    eldoc-eval
    elein
    elisp-slime-nav
    emacs-eclim
    eproject
    eshell-manual
    ethan-wspace
    evil
    evil-numbers
    evil-paredit
    evil-visualstar
    flycheck
    ghc
    ghci-completion
    gist
    git-blame
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    git-messenger
    google-c-style
    goto-chg
    haskell-mode
    helm
    ido-ubiquitous
    ido-vertical-mode
    inf-ruby
    info+
    javap-mode
    js2-mode
    js2-refactor
    js-comint
    json
    key-chord
    know-your-http-well
    less-css-mode
    lexbind-mode
    lorem-ipsum
    macrostep
    magit
    magit-gh-pulls
    magit-push-remote
    magithub
    markdown-mode
    mediawiki
    mmm-mode
    paredit
    parenface-plus
    pretty-mode-plus
    project-explorer
    rainbow-delimiters
    rainbow-mode
    redshank
    regex-dsl
    regex-tool
    rinari
    robe
    rspec-mode
    ruby-compilation
    ruby-electric
    ruby-interpolation
    rvm
    sass-mode
    scss-mode
    skewer-mode
    slamhound
    solarized-theme
    sr-speedbar
    surround
    tagedit
    tern
    tern-auto-complete
    toml-mode
    unbound
    undo-tree
    wgrep-ack
    workgroups
    yaml-mode
    yard-mode
    yasnippet
    zencoding-mode
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'init-package)
