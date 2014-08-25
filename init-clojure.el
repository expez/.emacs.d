(require-package 'javap-mode)
(require-package 'elein)
(require-package 'clojure-mode)
(require-package 'cider)
(require-package 'cider-browse-ns)
(require-package 'clj-refactor)
(require-package 'clojure-cheatsheet)
(require-package 'refheap)
(require 'cider-eldoc)
(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

(defadvice cider-load-current-buffer (before save-first activate)
  (save-buffer))

(after-load 'clj-refactor
  (setq cljr-magic-require-namespaces
        (-concat cljr-magic-require-namespaces
                 '(("component" . "com.stuartsierra.component")
                   ("s" . "schema.core")
                   ("log" . "taoensso.timbre")))
        cljr--magic-requires-re
        (concat "(\\("
                (regexp-opt (-map 'car cljr-magic-require-namespaces)) "\\)/")))

(defun my-cider-mode-hook ()
  (cider-turn-on-eldoc-mode))
(add-hook 'cider-mode-hook 'my-cider-mode-hook)

(defun my-cider-browse-ns-mode-hook ()
  (fill-keymap evil-normal-state-local-map (kbd "q")
               'cider-popup-buffer-quit-function))
(add-hook 'cider-browse-ns-mode-hook 'my-cider-browse-ns-mode-hook)

(defun my-cider-repl-mode-hook ()
  (setq show-trailing-whitespace nil)
  (company-mode 1)
  (cider-turn-on-eldoc-mode)
  (paredit-mode 1)
  (fill-keymaps '(evil-insert-state-local-map evil-normal-state-local-map)
                (kbd "C-a") 'cider-repl-bol
                "M-." 'cider-jump
                "M-," 'cider-jump-back
                (kbd "M-p") 'cider-repl-previous-input
                (kbd "M-n") 'cider-repl-next-input)
  (whitespace-mode 0)
  ;; (evil-paredit 1)
  )
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(defun cider-clear-errors ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'cider-note-p t))

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (auto-complete-mode 0)
  (company-mode 1)
  (setq-local evil-symbol-word-search t)
  (clj-refactor-mode 1)
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
  (paredit-mode 1)
  (evil-paredit-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (fill-keymap evil-normal-state-local-map
               "M-q" '(lambda () (interactive) (clojure-fill-paragraph))
               "M-." 'cider-jump
               "M-," 'cider-jump-back
               "C->" 'cljr-thread
               "C-<" 'cljr-unwind
               "C-c s" 'toggle-spy
               "C-c r" 'cider-repl-reset))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq nrepl-hide-special-buffers t
      nrepl-auto-select-error-buffer t
      cider-repl-popup-stacktraces nil
      cider-popup-stacktraces nil
      cider-popup-on-error t
      cider-repl-popup-stacktraces t
      cider-interactive-eval-result-prefix ";; => "
      cider-repl-history-file "~/.emacs.d/nrepl-history")

(put 'macrolet 'clojure-backtracking-indent '((2) 2))

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
  (context 2))

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

(defun insert-weasel-and-fighweel-client-code ()
  (insert
   (s-join "\n"
           '("(enable-console-print!)"
             "(fw/watch-and-reload"
             ":jsload-callback (fn [] (print \"reloaded\")))"

             "(if-not (ws-repl/alive?)"
             "(ws-repl/connect \"ws://localhost:9001\""
             ":verbose true))"))))

(defun cider-namespace-refresh ()
  (interactive)
  (cider-eval-sync
   "(require '[clojure.tools.namespace.repl :refer [refresh]]) (refresh)"
   (cider-current-ns)))

(defun cider-repl-reset ()
  "The reset to go along with Component."
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(user/reset)")
    (cider-repl-return)))

(defun toggle-spy ()
  (interactive)
  (save-excursion
    (if (spy-p)
        (unspy)
      (insert "#spy/d "))))

(defun spy-p ()
  (save-excursion
    (cljr--goto-toplevel)
    (let ((end (save-excursion (progn (paredit-forward) (point)))))
      (re-search-forward "#spy/d" end t))))

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

(provide 'init-clojure)
