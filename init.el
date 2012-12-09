;;package.el
(require 'package)
;; Set package archives.

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/" )
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

;;add evil-surround when it is added to melpa.
(defvar my-packages '(ace-jump-mode ac-slime auto-complete auctex bookmark+
                                    color-theme color-theme-sanityinc-solarized
                                    dired+ dropdown-list expand-region ghc
                                    ghci-completion gist git-commit haskell-mode
                                    key-chord lorem-ipsum magit magithub
                                    mediawiki move-text paredit parenface
                                    rainbow-delimiters rainbow-mode
                                    register-list unbound undo-tree
                                    whole-line-or-region workgroups yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Add everything in and below plugins to load-path.
(let ((default-directory "~/.emacs.d/plugins/"))
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path)))
               (append
                (copy-sequence (normal-top-level-add-to-load-path '(".")))
                (normal-top-level-add-subdirs-to-load-path)))
             load-path)))

(setq custom-file "~/.emacs.d/plugins/customize.el")
(load custom-file)

(if (file-accessible-directory-p "~/src/emacs-eclim")
    (add-to-list 'load-path (expand-file-name "~/src/emacs-eclim/")))

(if (file-accessible-directory-p (expand-file-name "~/src/emacs-eclim/vendor"))
    (add-to-list 'load-path (expand-file-name "~/src/emacs-eclim/vendor")))

(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (load (expand-file-name "~/quicklisp/slime-helper.el")))

(require 'auto-complete-clang-async)
(require 'auto-complete-config)
(require 'color-moccur)
(require 'color-theme)
(require 'parenface)
(require 'ex-mode)
(require 'highlight-tags-mode)
(require 'hpaste)
(require 'ido-hacks)
(require 'magit)
;;(require 'relative-linum)
(require 'workgroups)
(require 'yasnippet)
(require 'uniquify)
(require 'eclim)
(require 'eclimd)
(require 'ac-emacs-eclim-source)
(require 'java-mode-indent-annotations)
(require 'cuda-mode)
(require 'git)
(require 'git-blame)
(require 'mediawiki)

(load-library "my-config") ;; One-off variable settings.
(load-library "customize") ;;The stuff from customize lives in here.
