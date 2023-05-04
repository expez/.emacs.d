(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'clj-refactor)
(require-package 'clojure-snippets)
(require-package 'flycheck-clj-kondo)
(require 'flycheck-clj-kondo)

(eval-after-load 'clojure-snippets
  '(clojure-snippets-initialize))

(defadvice cider-load-buffer (before save-first activate)
  (save-buffer))

(after-load 'clj-refactor
  (setq cljr-magic-require-namespaces nil
        cljr-clojure-test-declaration  "[clojure.test :refer :all]"
        cljr-favor-prefix-notation nil
        cljr-eagerly-build-asts-on-startup nil
        cljr-use-multiple-cursors nil
        cljr-warn-on-eval nil))

(defun my-cider-mode-hook ()
  (eldoc-mode)
  (setq-local company-dabbrev-char-regexp "\\sw\\|-")
  (cider-company-enable-fuzzy-completion)
  (setq-local next-error-function #'flycheck-next-error-function))

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
                "M-," 'cider-pop-back
                "C-c R" 'cider-component-reset
                "C-c m" 'cider-macroexpand-1)
  (whitespace-mode 0)
  (evil-force-normal-state)
  (cider-company-enable-fuzzy-completion))
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(defun cider-clear-errors ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'cider-note-p t))

(eval-after-load 'cider
  '(fill-keymap cider-mode-map
                "C-c m" 'cider-macroexpand-1
                "C-c R" 'cider-component-reset
                "C-c c" 'cider-clear-errors
                "C-c M" 'cider-macroexpand-all
                "C-c C-x" 'cider-ns-refresh
                "C-c M-b" 'cider-browse-ns))

(defun add-cider-quit-key ()
  (fill-keymap evil-normal-state-local-map "q" #'cider-popup-buffer-quit-function))

(add-hook 'cider-macroexpansion-mode-hook #'add-cider-quit-key)

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (flycheck-mode 1)
  (setq evil-symbol-word-search t)
  (clj-refactor-mode 1)
  (cider-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (change-whitespace-line-column 100)
  (setq-local fill-column 100
              clojure-docstring-fill-column 95)
  (fill-keymap evil-normal-state-local-map
               "M-q" '(lambda () (interactive) (clojure-fill-paragraph))
               "M-," 'cider-pop-back
               "M-." 'cider-find-var
               "M-n" 'flycheck-next-error
               "M-p" 'flycheck-previous-error
               "C-c s" 'toggle-spy
               "C-c f" 'toggle-print-foo
               "C-c C-e" 'cider-eval-defun-at-point
               "C-M-;" #'comment-or-uncomment-sexp
               "C-c C-m" nil))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq nrepl-hide-special-buffers nil
      nrepl-log-messages t
      cider-use-fringe-indicators nil
      cider-use-tooltips nil
      cider-repl-pop-to-buffer-on-connect 'display-only
      cider-refresh-before-fn "user/stop"
      cider-refresh-after-fn "user/start"
      cider-repl-display-help-banner nil
      cider-popup-stacktraces t
      cider-repl-popup-stacktraces t
      cider-repl-use-pretty-printing t
      cider-prompt-save-file-on-load nil
      cider-repl-history-file "~/.emacs.d/nrepl-history"
      cider-cljs-repl "(do (require 'cljs.repl.nashorn)
(cemerick.piggieback/cljs-repl (cljs.repl.nashorn/repl-env)))")

(defadvice cider-repl-return (before normal-mode activate)
  (evil-normal-state))

(defun my-recenter (&rest ignore)
  (recenter))

(advice-add 'cider-jump-to :after #'my-recenter)
(advice-add 'cider-pop-back :after #'my-recenter)

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
    (insert "(reset)")
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
    (insert "#spy/d "))
  (cider-eval-defun-at-point))

(defun unspy ()
  (save-excursion
    (let ((start (progn (cljr--goto-toplevel) (point)))
          (end (progn (paredit-forward) (point))))
      (replace-regexp "#spy/d " "" nil start end))))

(defun cljr-guard-with-reader-conditional (cljs?)
  (interactive "P")
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

(defun refresh-only-advice (oldfun mode)
  (if (< mode 0)
      (let ((cider-refresh-before-fn nil)
            (cider-refresh-after-fn nil))
        (funcall oldfun mode))
    (funcall oldfun mode)))

(advice-add 'cider-refresh :around #'refresh-only-advice)

(defun clojure-pp ()
  (interactive)
  (clojure-mode)
  (save-excursion
    (while (re-search-forward ",")
      (backward-char)
      (delete-char 1)
      (insert "\n")))
  (indent-buffer)
  (ethan-wspace-clean-all))


;;; indentation
(eval-after-load 'clojure-mode
  '(put-clojure-indent 'prop/for-all 1))

;;; debugger


(defun my-debugger-change-state (&rest _)
  (if cider--debug-mode
      (evil-emacs-state)
    (evil-normal-state)))

(advice-add 'cider--debug-mode :after #'my-debugger-change-state)

(provide 'init-clojure)
