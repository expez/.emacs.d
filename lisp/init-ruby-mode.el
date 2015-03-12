(require-package 'yard-mode)
(require-package 'toml-mode)
(require-package 'rvm)
(require-package 'ruby-interpolation)
(require-package 'ruby-compilation)
(require-package 'rspec-mode)
(require-package 'robe)
(require-package 'rinari)
(require-package 'inf-ruby)
(require-package 'bundler)
(require-package 'projectile-rails)
(require-package 'feature-mode)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(after-load 'company
  (push 'company-robe company-backends))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(defun my-ruby-mode-hook ()
  (rvm-activate-corresponding-ruby)
  (robe-mode 1)
  (fill-keymap evil-normal-state-local-map
               "M-," 'pop-tag-mark
               "M-." 'robe-jump)
  (fill-keymap evil-insert-state-map
               (kbd "RET") 'reindent-then-newline-and-indent)
  (yard-mode 1)
  (eldoc-mode 1)
  (rinari-minor-mode 1)
  (setq webjump-api-sites '(("Ruby" . "http://apidock.com/ruby/")
                            ("Rails" . "http://apidock.com/rails/"))))

(add-hook 'ruby-mode-hook #'my-ruby-mode-hook)

(setq rspec-use-rvm 't
      rspec-use-bundler-when-possible 't)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "C-c , ,") 'ruby-open-spec-other-buffer))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)

(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")

(defun add-current-project-to-inf-ruby-load-path ()
  "This adds the lib folder--which is presumed to be an ancestor
of the currently active file--to the load path of the inf-ruby
process. "
  (interactive)
  (if (null inf-ruby-buffer)
      (inf-ruby)
    (let* ((lib-dir (concat
                     (car (split-string
                           (file-name-directory (buffer-file-name)) "lib"))
                     "lib"))
           (script (format "$:.unshift '%s'\n" lib-dir)))
      (comint-send-string inf-ruby-buffer script))))

(add-auto-mode 'ruby-mode "Rakefile" "\\.rake\\'" "\\.ru\\'" "\\.prawn\\'"
               "Gemfile\\'" "Capfile\\'" "Guardfile\\'" "\\.gemspec$")

(defadvice rspec-compile (around rspec-compile-around activate)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

(defadvice ruby-indent-line (after deep-indent-dwim activate)
  (let (c paren-column indent-column)
    (save-excursion
      (back-to-indentation)
      (save-excursion
        (let ((state (syntax-ppss)))
          (unless (zerop (car state))
            (goto-char (cadr state))
            (setq c (char-after))
            (setq paren-column (current-column))
            (when (memq c '(?{ ?\())
              (forward-char)
              (skip-syntax-forward " ")
              (unless (or (eolp) (eq (char-after) ?|))
                (setq indent-column (current-column)))))))
      (when (and indent-column
                 (eq (char-after) (matching-paren c)))
        (setq indent-column paren-column)))
    (when indent-column
      (let ((offset (- (current-column) (current-indentation))))
        (indent-line-to indent-column)
        (when (> offset 0) (forward-char offset))))))

(eval-after-load 'ruby-mode
  '(remf ruby-deep-indent-paren ?\())

(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

(defun ruby-open-spec-other-buffer ()
  (interactive)
  (when (featurep 'rspec-mode)
    (let ((source-buffer (current-buffer))
          (other-buffer (progn
                          (rspec-toggle-spec-and-target)
                          (current-buffer))))
      (switch-to-buffer source-buffer)
      (pop-to-buffer other-buffer))))

(defadvice beginning-of-defun (before fix-for-ruby-mode activate)
  (when (eq major-mode 'ruby-mode)
    (forward-char 1)))

(mapcar (lambda (keyword)
          (font-lock-add-keywords
           'ruby-mode
           `((,(concat ".\\(" keyword "\\)\\_>") 1 font-lock-keyword-face))))
        (list "each" "collect" "reject" "select" "inject" "include" "map" "reduce"))

(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(provide 'init-ruby-mode)
