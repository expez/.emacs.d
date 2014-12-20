(defvar my-config-dir (concat user-emacs-directory "lisp"))
(setq load-prefer-newer t)
(push my-config-dir load-path)

(require 'init-locale)

;; Add everything in and below site-lisp to load-path.
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(require 'init-util)
(require 'init-package)

(load-elisp-files-in-dir my-config-dir "^init-.\*")
(load-from-vendor-dir)
