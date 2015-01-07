;;; init.el --- user init file  -*- no-byte-compile: t -*-
(defvar my-config-dir (concat user-emacs-directory "lisp"))
(push my-config-dir load-path)

;; Add everything in and below site-lisp to load-path.
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(require 'init-locale)

(require 'init-package)
(require 'init-util)

(load-elisp-files-in-dir my-config-dir "^init-.\*")
(load-from-vendor-dir)
