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
(mapc #'load-if-exists '("~/quicklisp/slime-helper.el"
                         "~/src/emacs-clang-complete-async.el"
                         "~/src/git-wip/emacs/git-wip.el"
                         "~/src/git-wip/emacs/git-wip-mode.el"))

(require 'init-package)
(mapc #'load (directory-files user-emacs-directory t "init-.\*.el"))

(setq custom-file "~/.emacs.d/plugins/customize.el")
(load custom-file)

(require 'ac-emacs-eclim-source)
(require 'auto-complete-latex)
(require 'color-theme)
(require 'command-frequency)
(require 'color-moccur)
(require 'cuda-mode)
(require 'diminish)
(require 'eclim)
(require 'eclimd)
(require 'elisp-slime-nav)
(require 'eproject)
(require 'eproject-extras)
(require 'evil-leader)
(require 'evil-nerd-commenter)
(require 'evil-numbers)
(require 'evil-paredit)
(require 'go-autocomplete)
(require 'google-c-style)
(require 'highlight-tags-mode)
(require 'ido-hacks)
(require 'info+)
(require 'java-mode-indent-annotations)
(require 'lorem-ipsum)
(require 'mediawiki)
(require 'parenface)
(require 'sr-speedbar)
(require 'uniquify)
(require 'workgroups)
(require 'yasnippet)
(load-library "my-config") ;; One-off variable settings.
