(require-package 'javap-mode)
(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'clj-refactor)
(require-package 'clojure-cheatsheet)
(require-package 'clojure-snippets)
(require-package 'flycheck-clojure)

;; Error requiring init-clojure: (file-error Cannot open load file no such file or directory overlay)
(add-hook 'after-init-hook (lambda ()(require 'clj-refactor)))

(clojure-snippets-initialize)

(defadvice cider-load-buffer (before save-first activate)
  (save-buffer))

(after-load 'clj-refactor
  (setq cljr-magic-require-namespaces nil))

(defun my-cider-mode-hook ()
  (eldoc-mode)
  (setq next-error-function #'flycheck-next-error-function))

(add-hook 'cider-mode-hook 'my-cider-mode-hook)

(defun my-cider-browse-ns-mode-hook ()
  (fill-keymap evil-normal-state-local-map
               (kbd "q") #'cider-popup-buffer-quit-function
               (kbd "d") #'cider-browse-ns--doc-at-point
               (kbd "s") #'cider-browse-ns--var-at-point))

(add-hook 'cider-browse-ns-mode-hook 'my-cider-browse-ns-mode-hook)

(defun my-cider-repl-mode-hook ()
  (setq show-trailing-whitespace nil)
  (smartparens-strict-mode 1)
  (company-mode 1)
  (eldoc-mode)
  (fill-keymaps '(evil-insert-state-local-map evil-normal-state-local-map)
                "M-." 'cider-find-var
                "M-," 'cider-jump-back)
  (whitespace-mode 0)
  (evil-force-normal-state))
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(defun cider-clear-errors ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'cider-note-p t))

(after-load 'flycheck
  (flycheck-clojure-setup))

(after-load 'cider
  (fill-keymap cider-mode-map
               "C-c m" 'cider-macroexpand-1
               "C-c c" 'cider-clear-errors
               "C-c M" 'cider-macroexpand-all
               "C-c M-b" 'cider-browse-ns))

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (flycheck-mode 1)
  (setq evil-symbol-word-search t)
  (clj-refactor-mode 1)
  (add-to-list 'flycheck-disabled-checkers 'clojure-cider-typed)
  (add-to-list 'flycheck-disabled-checkers 'clojure-cider-kibit)
  (cider-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (local-set-key (kbd "RET") 'newline-and-indent)
  (fill-keymap evil-normal-state-local-map
               "M-q" '(lambda () (interactive) (clojure-fill-paragraph))
               "M-," 'cider-jump-back
               "M-." 'cider-find-var
               "M-n" 'flycheck-next-error
               "M-p" 'flycheck-previous-error
               "C-c s" 'toggle-spy
               "C-c f" 'toggle-print-foo
               "C-c R" 'cider-component-reset
               "C-c C-e" 'cider-eval-defun-at-point
               "C-c C-m" nil
               "C-c h" 'clojure-cheatsheet))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq nrepl-hide-special-buffers nil
      nrepl-log-messages t
      cider-popup-stacktraces t
      cider-repl-popup-stacktraces t
      cider-repl-use-pretty-printing t
      cider-prompt-save-file-on-load nil
      cider-repl-history-file "~/.emacs.d/nrepl-history"
      cljr-use-multiple-cursors nil
      cider-cljs-repl "(do (require 'cljs.repl.nashorn)
(cemerick.piggieback/cljs-repl (cljs.repl.nashorn/repl-env)))")

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

(defun weasel-connect ()
  (interactive)
  "Connect the repl to weasel"
  (cider-eval-sync
   (s-join "\n" '("(require 'weasel.repl.websocket)"
                  "(cemerick.piggieback/cljs-repl :repl-env"
                  "(weasel.repl.websocket/repl-env"
                  ":ip \"0.0.0.0\" :port 9001))"))))

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
    (cider-repl-set-ns "user")
    (goto-char (point-max))
    (insert "(reloaded.repl/reset)")
    (cider-repl-return)))

(defun toggle-print-foo ()
  "Insert a single print-foo, around point, or remove all existing print-foos.

With a prefix add print-foo throughout the function."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (let* ((foo-regexp
              (rx (and "print-" (or ">" ">>" "if" "let" "cond->"
                                    "cond->>""cond" "defn-" "defn"))))
             (replacement-regexp (rx (or "(->" "(->>" "(if" "(let" "(cond->"
                                         "(cond->>" "(cond" "(defn-" "(defn")))
             (found (save-excursion (goto-char (point-min))
                                    (re-search-forward foo-regexp nil :no-error))))
        (if found
            (progn
              (goto-char (point-min))
              (while (re-search-forward foo-regexp nil :no-error)
                (paredit-backward)
                (delete-char 6) ; delete print-
                (when (looking-at-p ">")
                  (insert "-"))))
          (if current-prefix-arg
              (progn
                (goto-char (point-min))
                (while (not (eobp))
                  (if (not (looking-at-p replacement-regexp))
                      (forward-char)
                    (if (looking-at-p "->")
                        (insert "print")
                      (insert "print-"))
                    (paredit-forward))))
            (re-search-backward replacement-regexp nil :no-error)
            (forward-char)
            (if (looking-at-p "->")
                (insert "print")
              (insert "print-")))))))
  (save-buffer))

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
        (let ((client-proc (nrepl-start-client-process nil port process)))
          ;; FIXME: Bad connection tracking system. There can be multiple client
          ;; connections per server
          (setq nrepl-connection-buffer (buffer-name (process-buffer client-proc))))))))

(defun cljr-guard-with-reader-conditional (cljs?)
  (interactive "P")
  (unless (looking-back "\\s-+")
    (paredit-backward))
  (paredit-wrap-round)
  (forward-char -1)
  (insert "#?")
  (paredit-forward-down)
  (if cljs?
      (insert ":cljs ")
    (insert ":clj ")))

(defun cljr--symbol->keyword (symbol)
  (intern (format ":%s" symbol)))

(defun cljr--plist-to-hash (plist)
  (let ((h (make-hash-table)))
    (dolist (k (-filter #'keywordp plist))
      (puthash k (plist-get plist k) h))
    h))

(defun nrepl-message-to-kill-ring (op)
  "Add the last nrepl messages issuing op OP to the kill ring."
  (interactive
   (list
    (completing-read "OP: " (-concat
                             cljr--nrepl-ops cider-required-nrepl-ops))))
  (with-current-buffer "*nrepl-messages*"
    (goto-char (point-max))
    (re-search-backward (format "op +\"%s\"" op))
    (paredit-backward-up)
    (let ((msg (cljr--extract-sexp)))
      (with-temp-buffer
        (insert msg)
        (goto-char (point-min))
        (paredit-forward-down)
        (delete-region (point) (point-at-eol))
        (goto-char (point-min))
        (->> (edn-read)
             (mapcar (lambda (e) (if (symbolp e) (cljr--symbol->keyword e) e)))
             cljr--plist-to-hash
             edn-print-string
             kill-new)))))

(provide 'init-clojure)
