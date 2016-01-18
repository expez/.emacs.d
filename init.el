;;; init.el --- user init file  -*- no-byte-compile: t -*-
(defvar my-config-dir (concat user-emacs-directory "lisp"))
(push my-config-dir load-path)

(setq load-prefer-newer t)

;; Add everything in and below site-lisp to load-path.
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(require 'init-locale)
(require 'init-package)
(require 'init-util)

(require-package 'exec-path-from-shell)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(safe-load-init-files my-config-dir)
