(require-package 'company-tern)
(require-package 'tern)
(require-package 'json)
(require-package 'js2-refactor)
(require-package 'js2-mode)
(require 'flycheck)
(require-package 'web-beautify)

(defun web-beautify-dwim ()
  (interactive)
  (cond
   ((string= (file-name-extension (buffer-file-name)) "html") (web-beautify-html))
   ((string= (file-name-extension (buffer-file-name)) "css") (web-beautify-css))
   ((string= (file-name-extension (buffer-file-name)) "scss") (web-beautify-css))
   ((string= (file-name-extension (buffer-file-name)) "less") (web-beautify-css))
   ((string= (file-name-extension (buffer-file-name)) "sass") (web-beautify-css))
   ((string= (file-name-extension (buffer-file-name)) "js") (web-beautify-js))
   (t (message "Not sure how to beautify file ending with '%s'"
               (file-name-extension (buffer-file-name))))))

(add-auto-mode 'js2-mode "\\.js")

(defun my-js2-exit-snippet-hook ()
  (indent-region yas-snippet-beg yas-snippet-end))

(add-to-list 'company-backends 'company-tern)

(defun add-test-externs()
  (-when-let (name (buffer-file-name))
    (or (string-match "_spec.js" name)
        (string-match "_test.js" name))
    (setq js2-additional-externs
          '("describe" "it" "xit" "expect" "spyOn" "jasmine"
            "beforeEach" "runs" "waits" "waitsFor" "afterEach"
            "xdescribe" "element" "by" "browser" "yasmine" "inject"))))

(defun find-eslint ()
  (ignore-errors
    (let ((local-eslint (concat (file-name-directory
                                 (expand-file-name "package.json"
                                                   (locate-dominating-file
                                                    (buffer-file-name)
                                                    "package.json")))
                                "node_modules/.bin/eslint")))
      (if (file-exists-p local-eslint)
          local-eslint
        (path-to-executable "eslint")))))

(defun my-maybe-jsx-mode-hook ()
  (when (and (buffer-file-name)
             (string= (file-name-extension (buffer-file-name)) "jsx"))
    (modify-syntax-entry ?< "(>")
    (modify-syntax-entry ?> ")<")
    (sp-local-pair 'js2-mode "<" "/>")
    (setq-local flycheck-javascript-eslint-executable (find-eslint))
    (flycheck-select-checker 'javascript-eslint)))

(defun my-js2-mode-hook ()
  (setq-local yas-after-exit-snippet-hook #'my-js2-exit-snippet-hook)
  (js2-imenu-extras-setup)
  (add-test-externs)
  (setq mode-name "JS2")
  (when (find-eslint)
    (setq-local flycheck-javascript-eslint-executable (find-eslint)))
  (flycheck-mode t)
  (aggressive-indent-mode 0)
  (my-maybe-jsx-mode-hook)
  (fill-keymap evil-normal-state-local-map
               "M-." 'tern-find-definition
               "M-," 'tern-pop-find-definition
               "C-M-." 'tern-find-definition-by-name
               "M-p" 'flycheck-previous-error
               "M-n" 'flycheck-next-error
               "C-c b" 'web-beautify-js)
  (fill-keymap js2-mode-map
               "C-c C-a" 'jshint-annotate))

(add-hook 'js2-mode-hook #'my-js2-mode-hook)

(setq-default js2-use-font-lock-faces t
              js2-bounce-indent-p nil
              js2-concat-multiline-strings 'eol
              js2-idle-timer-delay 0.1
              js2-highlight-level 3
              js2-global-externs '("module" "require" "assert" "refute"
                                   "setTimeout" "clearTimeout" "setInterval"
                                   "clearInterval" "location" "__dirname"
                                   "console" "JSON" "angular")
              js2-mode-show-parse-errors nil
              js2-strict-missing-semi-warning nil
              js2-strict-inconsistent-return-warning nil
              js2-basic-offset 2
              js2-strict-trailing-comma-warning t)

(js2r-add-keybindings-with-prefix "C-c C-m")

;; js2-mode steals TAB, let's steal it back for yasnippet
(defun js2-tab-properly ()
  (interactive)
  (let ((yas/fallback-behavior 'return-nil))
    (unless (yas/expand)
      (indent-for-tab-command)
      (if (looking-back "^\s+")
          (back-to-indentation)))))

(define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)

(defun cjsp--eldoc-innards (beg)
  (save-excursion
    (goto-char beg)
    (search-forward "=")
    (let ((start (point)))
      (search-forward "*/")
      (forward-char -2)
      (buffer-substring-no-properties start (point)))))

(defun cjsp--indentation-of-html-line (html line-number)
  (with-temp-buffer
    (insert html)
    (html-mode)
    (indent-region (point-min) (point-max))
    (goto-line line-number)
    (back-to-indentation)
    (current-column)))

(defun cjsp--line-number-in-eldoc (p beg)
  (save-excursion
    (goto-char p)
    (let ((l (line-number-at-pos)))
      (goto-char beg)
      (- l (line-number-at-pos) -1))))

(defun js2-lineup-comment (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line (js2-same-line beg))
         (p (point))
         (offset (save-excursion
                   (goto-char beg)
                   (cond

                    ((looking-at "/\\*:DOC ")
                     (+ 2 (current-column)
                        (cjsp--indentation-of-html-line
                         (cjsp--eldoc-innards beg)
                         (cjsp--line-number-in-eldoc p beg))))

                    ((looking-at "/\\*")
                     (+ 1 (current-column)))

                    (:else 0)))))
    (unless first-line
      (indent-line-to offset))))

(defun jshint-annotate ()
  "Use completing read to choose among jshint annotations
to insert above current line"
  (interactive)
  (let* ((annotation (completing-read "Insert annotation: "
                                      (list "unused: false"
                                            "undef: false"
                                            "loopfunc: true"
                                            "falls through"
                                            "strict: true")))
         (prefix (if (string= annotation "falls through")
                     "/* "
                   "/*jshint "))
         (suffix " */"))
    (save-excursion
      (open-line-above)
      (previous-line)
      (insert prefix annotation suffix)
      (js2-indent-line))))

(defun js-insert-block-and-semi ()
  "Insert a block, and semicolon and line end. "
  (interactive)
  (insert "{};")
  (backward-char 2)
  (open-line 2)
  (save-excursion
    (forward-line 2)
    (indent-for-tab-command))
  (forward-line 1)
  (indent-for-tab-command))

(defun js-insert-block ()
  "Insert a block"
  (interactive)
  (insert "{}")
  (backward-char)
  (open-line 2)
  (save-excursion
    (forward-line 2)
    (indent-for-tab-command))
  (forward-line 1)
  (indent-for-tab-command))

;; Version with single quotes instead of doubles
(defun js2r-log-this ()
  "Log of the node at point, adding a 'console.log()' statement."
  (interactive)
  (js2r--guard)
  (let* ((log-info (js2r--figure-out-what-to-log-where))
         (stmt (car log-info))
         (pos (cdr log-info)))
    (save-excursion
      (goto-char pos)
      (when (looking-at "[;{]")
        (forward-char 1))
      (newline-and-indent)
      (insert "console.log('" stmt " = ', " stmt ");"))))

(provide 'init-javascript)
