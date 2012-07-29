;;package.el
(require 'package)
;; Set package archives.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
                          ("melpa" . "http://melpa.milkbox.net/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages '(ace-jump-mode auctex bookmark+ dired+ dropdown-list ghc ghci-completion gist
				    expand-region lisppaste key-chord lorem-ipsum magit magithub move-text paredit
				    pastebin parenface register-list session smex unbound undo-tree whole-line-or-region workgroups yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/helm")
(add-to-list 'load-path "~/.emacs.d/plugins/evil")
(add-to-list 'load-path "~/.emacs.d/plugins/evil-surround")

(require 'highlight-tags-mode)
(require 'workgroups)
(require 'yasnippet)
(require 'parenface)
(require 'hpaste)
(require 'evil)
(require 'surround)
(require 'relative-linum)
(require 'magit)

(load-library "efuncs") ;; elisp functions.
(load-library "my-config") ;; One-off variable settings.
(load-library "bindings") ;; Keyboard bindings and aliases.
(load-library "customize") ;;The stuff from customize lives in here.

(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
