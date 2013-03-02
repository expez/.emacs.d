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

(provide 'init-eproject)
