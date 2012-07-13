;;package.el
(require 'package)
;; Set package archives.
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                          ("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages '(ace-jump-mode auctex bookmark+ dired+ dropdown-list ghc ghci-completion gist

				    iy-go-to-char expand-region lisppaste magit magithub paredit
				    pastebin parenface register-list session smex unbound undo-tree whole-line-or-region
				    workgroups yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/plugins")

(require 'highlight-tags-mode)
(require 'workgroups)
(require 'yasnippet)
(require 'parenface)
(require 'hpaste)
(load-library "efuncs") ;; elisp functions.
(load-library "my-config") ;; One-off variable settings.
(load-library "bindings") ;; Keyboard bindings and aliases.
(load-library "customize") ;;The stuff from customize lives in here.

(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
