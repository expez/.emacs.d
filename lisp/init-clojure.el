(require-package 'javap-mode)
(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'clj-refactor)
(require-package 'clojure-cheatsheet)
(require-package 'refheap)
(require-package 'clojure-snippets)
(require-package 'flycheck-clojure)
(clojure-snippets-initialize)

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

(defadvice cider-load-current-buffer (before save-first activate)
  (save-buffer))

(after-load 'clj-refactor
  (setq cljr-magic-require-namespaces
        (-concat cljr-magic-require-namespaces
                 '(("component" . "com.stuartsierra.component")
                   ("s" . "schema.core")
                   ("log" . "taoensso.timbre")))))

(defun my-cider-mode-hook ()
  (eldoc-mode)
  (setq next-error-function #'flycheck-next-error-function))

(add-hook 'cider-mode-hook 'my-cider-mode-hook)

(defun my-cider-browse-ns-mode-hook ()
  (fill-keymap evil-normal-state-local-map (kbd "q")
               'cider-popup-buffer-quit-function))
(add-hook 'cider-browse-ns-mode-hook 'my-cider-browse-ns-mode-hook)

(defun my-cider-repl-mode-hook ()
  (setq show-trailing-whitespace nil)
  (smartparens-strict-mode 1)
  (company-mode 1)
  (eldoc-mode)
  (fill-keymaps '(evil-insert-state-local-map evil-normal-state-local-map)
                "M-." 'cider-jump-to-var
                "M-," 'cider-jump-back)
  (whitespace-mode 0)
  (evil-force-normal-state))
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(defun my-cider-connected-hook ()
  (cljr-update-artifact-cache)
  (cljr-warm-ast-cache))

(add-hook 'cider-connected-hook #'my-cider-connected-hook)

(defun cider-clear-errors ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'cider-note-p t))

(after-load 'flycheck
  (flycheck-clojure-setup))

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (flycheck-mode 1)
  (setq evil-symbol-word-search t)
  (clj-refactor-mode 1)
  (add-to-list 'flycheck-disabled-checkers 'clojure-cider-typed)
  (add-to-list 'flycheck-disabled-checkers 'clojure-cider-kibit)
  (cider-mode 1)
  (fill-keymap cider-mode-map
               "C-c C-e" 'cider-eval-defun-at-point
               "C-c C-m" nil
               "C-c h" 'clojure-cheatsheet
               "C-c M-b" 'cider-browse-ns-all
               "C-c m" 'cider-macroexpand-1
               "C-c c" 'cider-clear-errors
               "C-c M" 'cider-macroexpand-all)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (local-set-key (kbd "RET") 'newline-and-indent)
  (fill-keymap evil-normal-state-local-map
               "M-q" '(lambda () (interactive) (clojure-fill-paragraph))
               "M-." 'cider-jump-to-var
               "M-," 'cider-jump-back
               "M->" 'cljr-thread
               "M-<" 'cljr-unwind
               "M-n" 'flycheck-next-error
               "M-p" 'flycheck-previous-error
               "C-c s" 'toggle-spy
               "C-c R" 'cider-component-reset))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq nrepl-hide-special-buffers nil
      nrepl-auto-select-error-buffer t
      cider-repl-popup-stacktraces nil
      cider-popup-stacktraces t
      cider-repl-popup-stacktraces t
      cider-interactive-eval-result-prefix ";; => "
      cider-repl-history-file "~/.emacs.d/nrepl-history")

(defadvice cider-repl-return (before normal-mode activate)
  (evil-normal-state))

(put 'macrolet 'clojure-backtracking-indent '((2) 2))

(after-load 'clojure-mode
  (define-clojure-indent
    (apply 1)
    (are 'defun)
    (go-loop 1)
    (run* 1)
    (fresh 1)
    (match 1)
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)
    (for-all 1)))

(defun austin-connnect ()
  (interactive)
  "Call to use from the cider repl to connect to a bREPL"
  (cider-eval-sync "(cemerick.piggieback/cljs-repl :repl-env (cemerick.austin/exec-env))"))

(defun weasel-connect ()
  (interactive)
  "Connect the repl to weasel"
  (cider-eval-sync
   (s-join "\n" '("(require 'weasel.repl.websocket)"
                  "(cemerick.piggieback/cljs-repl :repl-env"
                  "(weasel.repl.websocket/repl-env"
                  ":ip \"0.0.0.0\" :port 9001))"))))

(setq repls-cljs-setup
"(require 'weasel.repl.websocket)
 (cemerick.piggieback/cljs-repl
   :repl-env
   (weasel.repl.websocket/repl-env :ip \"0.0.0.0\" :port 9001))")

(defun insert-weasel-and-fighweel-client-code ()
  (insert
   (s-join "\n"
           '("(enable-console-print!)"
             "(fw/watch-and-reload"
             ":jsload-callback (fn [] (print \"reloaded\")))"

             "(if-not (ws-repl/alive?)"
             "(ws-repl/connect \"ws://localhost:9001\""
             ":verbose true))"))))

(defun cider-component-reset ()
  "The reset to go along with Component."
  (interactive)
  (save-some-buffers :no-prompt
                     (lambda ()
                       (and (buffer-file-name)
                            (not (s-contains-p "jar:" (buffer-file-name)))
                            (s-ends-with-p "\.clj" (buffer-file-name)))))
  (with-current-buffer (cider-current-repl-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns "user")
      (goto-char (point-max))
      (insert "(reset)")
      (cider-repl-return)
      (cider-repl-set-ns ns))))

(defun toggle-spy (p)
  (interactive "P")
  (if current-prefix-arg
      (unspy)
    (insert "#spy/d ")))

(defun unspy ()
  (save-excursion
    (let ((start (progn (cljr--goto-toplevel) (point)))
          (end (progn (paredit-forward) (point))))
      (replace-regexp "#spy/d " "" nil start end))))

;;; modified to place s/def{,} into the imenu, when using Schemas
(defun clojure-match-next-def ()
  "Scans the buffer backwards for the next top-level definition.
Called by `imenu--generic-function'."
  (when (re-search-backward "^(\\(s/\\)?def\\sw*" nil t)
    (save-excursion
      (let (found?
            (start (point)))
        (down-list)
        (forward-sexp)
        (while (not found?)
          (forward-sexp)
          (or (if (char-equal ?[ (char-after (point)))
                              (backward-sexp))
                  (if (char-equal ?) (char-after (point)))
                (backward-sexp)))
          (destructuring-bind (def-beg . def-end) (bounds-of-thing-at-point 'sexp)
            (if (char-equal ?^ (char-after def-beg))
                (progn (forward-sexp) (backward-sexp))
              (setq found? t)
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

(defun nrepl-server-filter (process string)
  "Process server PROCESS output contained in STRING."
  (with-current-buffer (process-buffer process)
    (let ((moving (= (point) (process-mark process))))
      (save-excursion
        (goto-char (process-mark process))
        (insert string)
        (set-marker (process-mark process) (point)))
      (when moving
        (goto-char (process-mark process))
        (-when-let (win (get-buffer-window))
          (set-window-point win (point))))))
  (when (string-match "nREPL server started on port \\([0-9]+\\)" string)
    (let ((port (string-to-number (match-string 1 string))))
      (message (format "nREPL server started on %s" port))
      (with-current-buffer (process-buffer process)
        (let ((client-proc (nrepl-start-client-process nil port t process)))
          ;; FIXME: Bad connection tracking system. There can be multiple client
          ;; connections per server
          (setq nrepl-connection-buffer (buffer-name (process-buffer client-proc))))))))

(defun cljr--create-missing-test-file (oldfun &rest args)
  (condition-case nil
      (funcall oldfun)
    ('error (save-window-excursion (cljr-create-test-file)) (funcall oldfun))))

(advice-add 'projectile-toggle-between-implementation-and-test :around
            #'cljr--create-missing-test-file)

(defun cljr-create-test-file ()
  (interactive)
  (when (eq major-mode 'clojure-mode)
    (let* ((test-file (s-replace-all '(("/src/" . "/test/") (".clj" . "_test.clj"))
                                     (buffer-file-name)))
           (test-dir (file-name-directory test-file))
           (test-name (file-name-nondirectory test-file)))
      (make-directory test-dir :create-parents)
      (find-file-other-window test-file)
      (cljr--add-ns-if-blank-clj-file)
      (save-buffer))))

(provide 'init-clojure)
