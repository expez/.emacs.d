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
				    pastebin parenface register-list session unbound undo-tree whole-line-or-region workgroups yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(let ((default-directory "~/.emacs.d/plugins/"))
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path))) ;; Shadow
               (append
                (copy-sequence (normal-top-level-add-to-load-path '(".")))
                (normal-top-level-add-subdirs-to-load-path)))
             load-path)))

(require 'highlight-tags-mode)
(require 'workgroups)
(require 'yasnippet)
(require 'parenface)
(require 'hpaste)
(require 'evil)
(require 'surround)
(require 'relative-linum)
(require 'magit)
(require 'ido-hacks)
(require 'ex-mode)
(load-library "my-config") ;; One-off variable settings.
(load-library "customize") ;;The stuff from customize lives in here.

(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
