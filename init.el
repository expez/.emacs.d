;;; init.el --- user init file  -*- no-byte-compile: t -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; This has to be all the way up here or ethan-wspace will warn about
;; `mode-require-final-newline' due to bad interactions with
;; `workgroups-mode'
(setq require-final-newline nil
      mode-require-final-newline nil)

(safe-load-init-files my-config-dir)

(let ((secrets-file (concat user-emacs-directory "secrets.el")))
  (when (file-exists-p secrets-file)
    (load secrets-file)))
