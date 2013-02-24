(push user-emacs-directory load-path)

;; Add everything in and below plugins to load-path.
(let ((default-directory "~/.emacs.d/plugins/"))
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path)))
               (append
                (copy-sequence (normal-top-level-add-to-load-path '(".")))
                (normal-top-level-add-subdirs-to-load-path)))
             load-path)))

(require 'init-utils)
(require 'init-package)
(mapc #'load (directory-files user-emacs-directory t "init-.\*.el"))

(setq custom-file "~/.emacs.d/plugins/customize.el")
(load custom-file)

(if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
    (load (expand-file-name "~/quicklisp/slime-helper.el")))

(if (file-exists-p (expand-file-name "~/src/emacs-clang-complete-async.el"))
    (load (expand-file-name "~/src/emacs-clang-complete-async.el")))

(require 'auto-complete-config)
(require 'color-theme)
(require 'diminish)
(require 'parenface)
(require 'ex-mode)
(require 'evil-paredit)
(require 'highlight-tags-mode)
(require 'ido-hacks)
(require 'magit)
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
(require 'eproject)
(require 'eproject-extras)
(load-library "my-config") ;; One-off variable settings.
