(require-package 'eproject)
(require 'eproject)
(require 'eproject-extras)
(require 'eproject-clojure-leiningen)
(setf eproject-completing-read-function 'eproject--ido-completing-read)

(fill-keymap eproject-mode-map
             "C-c C-f" 'nil
             "C-c C-b" 'nil
             "C-c b" 'nil)

(define-project-type ruby (generic)
  (or (look-for "Rakefile")
      (look-for "Gemfile")
      (look-for "\.rmvrc")
      (look-for "\.ruby-version")
      (look-for "\.rbenv-version")
      (look-for ".*\\.gemspec"))
  :irrelevant-files (".*~"))

(define-project-type enonic (generic)
  (look-for "WebDAV.iml"))

(define-project-type ruby-on-rails (generic)
  (and (look-for "Gemfile") (look-for "config/application.rb"))
  :irrelevant-files ("app/assets/images/.*" "tmp/.*" "log/.*" "public/.*"
                     "vendor/.*" ".*\\.sqlite?")
  :main-file "Gemfile")

(define-project-type emacs (generic)
  (look-for "init.el")
  :irrelevant-files ("elpa/.*" "vendor/.*" "snippets/.*" "backups/.*" "var/.*"
                     "tramp" "url/.*" "autosaves/.*" "auto-save-list/.*"
                     "eshell/.*" "sql/.*"))

(define-project-type cl (generic)
  (look-for "\.asd")
  :irrelevant-files ("\.fasl"))

;;; Until upstream merges my changes
(defun eproject--generic-switch-to-buffer (prefix switch-func)
  (let* ((root (eproject--handle-root-prefix-arg prefix :live-only t))
         (calling-buffer (current-buffer))
         (buffers (delq nil (mapcar (lambda (buf)
                                      (ignore-errors
                                        (when (equal root (eproject-root buf))
                                          (unless (eq buf calling-buffer)
                                            buf))))
                                    (buffer-list))))
         (buffers (when (eq eproject-completing-read-function
                            #'eproject--ido-completing-read)
                    (mapcar #'buffer-name buffers)))
         (chosen-buf (eproject--do-completing-read
                      "switch to buffer in project: " buffers)))
    (funcall switch-func chosen-buf)))

(provide 'init-eproject)
