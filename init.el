;;package.el
(require 'package)
;; Set package archives.

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/" )
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages
  '(ace-jump-mode ac-slime auto-complete auctex bookmark+ c-eldoc
                  diminish dired+ dropdown-list elisp-slime-nav evil
                  eproject evil-leader evil-nerd-commenter
                  evil-numbers evil-paredit expand-region
                  emacs-eclim flycheck ghc ghci-completion gist git-commit
                  gitconfig-mode gitignore-mode git-blame google-c-style
                  haskell-mode helm inf-ruby key-chord lorem-ipsum magit
                  magithub markdown-mode mediawiki move-text paredit parenface
                  rainbow-delimiters rainbow-mode regex-dsl
                  register-list rinari rspec-mode robe ruby-compilation
                  ruby-electric rvm sr-speedbar surround unbound undo-tree
                  whole-line-or-region workgroups yaml-mode yasnippet)
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

(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (load (expand-file-name "~/quicklisp/slime-helper.el")))

(if (file-exists-p (expand-file-name "~/programs/emacs-clang-complete-async.el"))
    (load (expand-file-name "~/programs/emacs-clang-complete-async.el")))

(require 'auto-complete-clang-async)
(require 'auto-complete-config)
(require 'color-moccur)
(require 'color-theme)
(require 'diminish)
(require 'parenface)
(require 'ex-mode)
(require 'evil-paredit)
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
(require 'git-blame)
(require 'google-c-style)
(require 'mediawiki)
(require 'lorem-ipsum)
(require 'go-autocomplete)
(require 'auto-complete-latex)
(require 'info+)
(require 'elisp-slime-nav)
(require 'sr-speedbar)
(require 'evil-numbers)
(require 'evil-nerd-commenter)
(require 'evil-leader)
(require 'rspec-mode)
(require 'robe)
(require 'rinari)
(require 'inf-ruby)
(require 'ruby-compilation)
(require 'eproject)
(require 'eproject-extras)
(load-library "my-config") ;; One-off variable settings.
(load-library "customize") ;;The stuff from customize lives in here.
