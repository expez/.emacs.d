(require 'eproject)
(require 'eproject-extras)

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

(define-project-type ruby-on-rails (generic)
  (and (look-for "Gemfile") (look-for "config/application.rb"))
  :irrelevant-files ("app/assets/images/.*" "tmp/.*" "log/.*" "public/.*" "vendor/.*" ".*\\.sqlite?")
  :main-file "Gemfile")

(define-project-type emacs (generic)
  (look-for "init.el")
  :irrelevant-files ("elpa/.*" "vendor/.*" "snippets/.*" "backups/.*" "var/.*"
                     "url/.*" "autosaves/.*" "auto-save-list/.*" "eshell/.*"))

(define-project-type cl (generic)
  (or (look-for "*asd")
      (look-for "packages.lisp"))
  :irrelevant-files ("*.fasl"))

(provide 'init-eproject)
