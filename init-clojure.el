(require-package 'javap-mode)
(require-package 'elein)
(require-package 'align-cljlet)
(require-package 'slamhound)
(require-package 'clojure-mode)
(require-package 'clojure-test-mode)
(require-package 'cider)
(require-package 'clj-refactor)
(require-package 'clojure-cheatsheet)
(require-package 'cider-decompile)
(require-package 'cider-tracing)
(require-package 'ac-nrepl)
(require 'cider-eldoc)

(add-auto-mode 'clojure-mode "\\.cljs\\'")

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice cider-load-current-buffer (before save-first activate)
  (save-buffer))

(defun my-cider-mode-hook ()
  (cider-turn-on-eldoc-mode)
  (ac-nrepl-setup))
(add-hook 'cider-mode-hook 'my-cider-mode-hook)

(defun my-cider-repl-mode-hook ()
  (setq show-trailing-whitespace nil)
  (ac-nrepl-setup)
  (paredit-mode 1)
                                        ;  (evil-paredit 1)
  )
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (clj-refactor-mode 1)
  (cider-mode 1)
  (clojure-test-mode 1)
  (paredit-mode 1)
  (evil-paredit-mode 1)
  (cljr-add-keybindings-with-prefix "C-c r")
  (fill-keymap clj-refactor-map "C")
  (local-set-key (kbd "RET") 'newline-and-indent)
  (fill-keymap evil-normal-state-local-map
               "M-." 'cider-jump
               "C->" 'cljr-thread
               "C-<" 'cljr-unwind
               "M-," 'cider-jump-back
               "C-c e" 'eval-and-insert))

(define-key clj-refactor-map (funcall key-fn "cc") 'live-cycle-clj-coll)
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces nil)
(setq cider-popup-stacktraces nil)
(setq cider-popup-on-error nil)
(setq cider-repl-history-file "~/.emacs.d/nrepl-history")

(add-hook 'nrepl-connected-hook
          (defun my-nrepl-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
            (nrepl-enable-on-existing-clojure-buffers)))

(put-clojure-indent 'match 1)
(put 'macrolet 'clojure-backtracking-indent '((2) 2))

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(defun eval-and-insert ()
  "Evals the expression at point and inserts the result on the line
  below."
  (interactive)
  (let ((res (cider-eval-and-get-value (cider-defun-at-point) (cider-current-ns))))
    (save-excursion
      (open-line-below)
      (forward-line)
      (beginning-of-line)
      (insert ";= " res))))

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))


(defun live-delete-and-extract-sexp ()
  "Delete the sexp and return it."
  (interactive)
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun toggle-clj-keyword-string ()
  "convert the string or keyword at (point) from string->keyword or keyword->string."
  (interactive)
  (let* ((original-point (point)))
    (while (and
            (> (point) 1)
            (not (equal "\"" (buffer-substring-no-properties (point) (+ 1 (point)))))
            (not (equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake."))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (live-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (live-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defun cycle-clj-coll ()
  "convert the coll at (point) from (x) -> {x} -> [x] -> -> #{x} -> (x) recur"
  (interactive)
  (let* ((original-point (point)))
    (while (and
            (> (point) 1)
            (not (equal "(" (buffer-substring-no-properties (point) (+ 1 (point)))))
            (not (equal "#{" (buffer-substring-no-properties (point) (+ 2 (point)))))
            (not (equal "{" (buffer-substring-no-properties (point) (+ 1 (point)))))
            (not (equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))))
      (backward-char))

    (cond
     ((equal "(" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))

     ((equal "#" (buffer-substring-no-properties (point) (+ 1 (point))))
      (progn
        (delete-char 1)
        (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")")))

     ((equal "{" (buffer-substring-no-properties (point) (+ 1 (point))))
      (if (equal ?# (char-before))
          (progn
            (backward-char)
            (delete-char 1)
            (insert "(" (substring (live-delete-and-extract-sexp) 1 -1) ")"))
        (insert "[" (substring (live-delete-and-extract-sexp) 1 -1) "]")))

     ((equal "[" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "#{" (substring (live-delete-and-extract-sexp) 1 -1) "}"))

     ((equal 1 (point))
      (message "beginning of file reached, this was probably a mistake.")))
    (goto-char original-point)))

(provide 'init-clojure)
