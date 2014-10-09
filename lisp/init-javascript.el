(require-package 'company-tern)
(require-package 'tern)
(require-package 'json)
(require-package 'js2-refactor)
(require-package 'js2-mode)
(require-package 'skewer-mode)
(require 'js-lookup)

;;; bookmarklet to load skewer:
;;; javascript:(function(){var d=document;var s=d.createElement('script');s.src='http://localhost:8023/skewer';d.body.appendChild(s);})()

(defun skewer-start ()
  (interactive)
  (let ((httpd-port 8023)
        (browse-url-generic-program "chromium"))
    (httpd-start)
    (message "Ready to skewer the browser. Now jack in with the bookmarklet.")))

(defun skewer-demo ()
  (interactive)
  (let ((httpd-port 8024))
    (run-skewer)
    (skewer-repl)))

(when (eq system-type 'windows-nt)
  (setq tern-command
        (list "node"
              (concat (getenv "HOME")
                      "\\appdata\\roaming\\npm\\node_modules\\tern\\bin\\tern"))))
(add-auto-mode 'js2-mode "\\.js")

(defun my-js2-exit-snippet-hook ()
  (indent-region yas-snippet-beg yas-snippet-end))

(defun maybe-allow-tabs ()
  (when (and (string= system-name "NOLD0042")
             (eq (indentation-style) 'tabs)
         (allow-tabs)
         (setq tab-width 2))))

(add-to-list 'company-backends 'company-tern)

(defun my-js2-mode-hook ()
  (setq-local yas-after-exit-snippet-hook #'my-js2-exit-snippet-hook)
  (auto-complete-mode 0)
  (company-mode 1)
  (js2-imenu-extras-setup)
  (push 'ac-source-yasnippet ac-sources)
  (setq mode-name "JS2")
  (skewer-mode)
  (tern-mode t)
  (flycheck-mode t)
  (paredit-nonlisp)
  (evil-paredit-mode 1)
  (fill-keymap evil-normal-state-local-map
               "M-." 'tern-find-definition
               "M-," 'tern-pop-find-definition
               "C-M-." 'tern-find-definition-by-name
               "M-p" 'flycheck-previous-error
               "M-n" 'flycheck-next-error
               (kbd "<f1>") 'js-lookup)
  (fill-keymap evil-insert-state-local-map
               (kbd "C-m") 'js-insert-block-and-semi
               (kbd "M-m") 'js-insert-block
               (kbd "<return>") 'newline-and-indent)
  (fill-keymap js2-mode-map
               "C-c C-a" 'jshint-annotate)
  (maybe-allow-tabs))

(add-hook 'js2-mode-hook
          #'my-js2-mode-hook)

(setq-default js2-use-font-lock-faces t
              js2-bounce-indent-p nil
              js2-concat-multiline-strings 'eol
              js2-idle-timer-delay 0.1
              js2-highlight-level 3
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

(defun my-aget (key map)
  (cdr (assoc key map)))

(defun js2-fetch-autolint-externs (file)
  (let* ((settings (with-temp-buffer
                     (insert-file-literally file)
                     (javascript-mode)
                     (let (kill-ring) (kill-comment 1000))
                     (->> (buffer-substring (point-min) (point-max))
                       (s-trim)
                       (s-chop-prefix "module.exports = ")
                       (s-chop-suffix ";")
                       (json-read-from-string))))
         (predef (->> settings
                   (my-aget 'linterOptions)
                   (my-aget 'predef))))
    (--each (append predef nil)
      (add-to-list 'js2-additional-externs it))))

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                       " *, *" t))
                ))))

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
  "Use completing read among to choose among jshint annotations
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
  (save-excursion
    (goto-char (point-at-eol))
    (insert ";"))
  (insert "{}")
  (backward-char)
  (open-line 2)
  (save-excursion
    (forward-line 2)
    (indent-for-tab-command))
  (forward-line 1)
  (indent-for-tab-command))

(defun js-insert-block ()
  "Insert a block, and semicolon and line end. "
  (interactive)
  (insert "{}")
  (backward-char)
  (open-line 2)
  (save-excursion
    (forward-line 2)
    (indent-for-tab-command))
  (forward-line 1)
  (indent-for-tab-command))

(provide 'init-javascript)
