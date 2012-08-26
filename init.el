;;package.el
(require 'package)
;; Set package archives.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
                          ("melpa" . "http://melpa.milkbox.net/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages '(ace-jump-mode auto-complete auctex bookmark+ dired+ dropdown-list ghc ghci-completion gist
				    expand-region git-commit lisppaste key-chord lorem-ipsum magit magithub move-text paredit
				    parenface register-list session unbound undo-tree whole-line-or-region workgroups yasnippet)
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

(setq custom-file "~/.emacs.d/plugins/customize.el")
(load custom-file)

(require 'auto-complete-clang-async)
(require 'auto-complete-config)
(require 'color-moccur)
(require 'evil)
(require 'parenface)
(require 'ex-mode)
(require 'highlight-tags-mode)
(require 'hpaste)
(require 'ido-hacks)
(require 'magit)
(require 'relative-linum)
(require 'surround)
(require 'workgroups)
(require 'yasnippet)
(require 'pastebin)

(load-library "my-config") ;; One-off variable settings.
(load-library "customize") ;;The stuff from customize lives in here.

(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-font-list (quote ((98 "{\\bf " "}") (99 "{\\sc " "}") (101 "{\\em " "\\/}") (105 "{\\it " "\\/}") (114 "{\\rm " "}") (115 "{\\sl " "\\/}") (115 "{\\tt " "}") (100 "" "" t))))
 '(auto-image-file-mode t)
 '(fringe-mode (cons 8 0) nil (fringe))
 '(glasses-face (quote bold))
 '(glasses-original-separator "")
 '(glasses-separate-capital-groups nil)
 '(glasses-separate-parentheses-p nil)
 '(glasses-separator ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
