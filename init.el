;;; init.el --- user init file  -*- no-byte-compile: t -*-
(defvar my-config-dir (concat user-emacs-directory "lisp"))
(push my-config-dir load-path)

(setq load-prefer-newer t)
(safe-load-init-files (concat (user-emacs-directory) "hosts"))

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

(safe-load-init-files my-config-dir)
(load-from-vendor-dir)
