(require-package 'javap-mode)
(require-package 'elein)
(require-package 'align-cljlet)
(require-package 'slamhound)
(require-package 'clojure-mode)
(require-package 'clojure-test-mode)
(require-package 'cider)
(require-package 'clj-refactor)
(require-package 'clojure-cheatsheet)
(require-package 'cider-tracing)
(require-package 'refheap)
(require 'cider-eldoc)
(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice cider-load-current-buffer (before save-first activate)
  (save-buffer))

(defun my-cider-mode-hook ()
  (cider-turn-on-eldoc-mode))
(add-hook 'cider-mode-hook 'my-cider-mode-hook)

(defun my-cider-repl-mode-hook ()
  (setq show-trailing-whitespace nil)
  (company-mode 1)
  (cider-turn-on-eldoc-mode)
  (paredit-mode 1)
  (fill-keymaps '(evil-insert-state-local-map evil-normal-state-local-map)
                (kbd "C-a") 'cider-repl-bol
                (kbd "M-p") 'cider-repl-previous-input
                (kbd "M-n") 'cider-repl-next-input)
  (whitespace-mode 0)
  ;; (evil-paredit 1)
  )
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (auto-complete-mode 0)
  (company-mode 1)
  (setq-local evil-symbol-word-search t)
  (clj-refactor-mode 1)
  (cider-mode 1)
  (fill-keymap cider-mode-map
               "C-c c-e" 'cider-eval-defun-at-point
               "C-c C-m" nil
               "C-c h" 'clojure-cheatsheet
               "C-c m" 'cider-macroexpand-1
               "C-c M" 'cider-macroexpand-all)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (clojure-test-mode 1)
  (paredit-mode 1)
  (evil-paredit-mode 1)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (fill-keymap evil-normal-state-local-map
               "M-." 'cider-jump
               "C->" 'cljr-thread
               "C-<" 'cljr-unwind
               "M-," 'cider-jump-back
               "C-c e" 'eval-and-insert))

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
   "(require 'weasel.repl.websocket) (cemerick.piggieback/cljs-repl :repl-env (weasel.repl.websocket/repl-env :ip \"0.0.0.0\" :port 9001))"))

(defun insert-weasel-and-fighweel-client-code ()
  (insert)
  "(enable-console-print!)
   (fw/watch-and-reload
 :jsload-callback (fn [] (print \"reloaded\")))

(if-not (ws-repl/alive?)
        (ws-repl/connect \"ws://localhost:9001\"
                         :verbose true))")

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
  (cider-eval-sync
   "(require '[clojure.tools.namespace.repl :refer [refresh]])
    (refresh)"
   (cider-current-ns)))

(provide 'init-clojure)
