(push user-emacs-directory load-path)

;; Add everything in and below site-lisp to load-path.
(let ((default-directory "~/.emacs.d/site-lisp/"))
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

(setq custom-file (concat user-emacs-directory "customize.el"))
(load custom-file)
