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

(add-to-list 'auto-mode-alist '("\\.edn\\'" . clojure-mode))

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
  (cider-turn-on-eldoc-mode)
  (paredit-mode 1)
                                        ;  (evil-paredit 1)
  )
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (setq-local evil-symbol-word-search t)
  (clj-refactor-mode 1)
  (cider-mode 1)
  (fill-keymap cider-mode-map
               "C-c c-e" 'cider-eval-defun-at-point
               "C-c C-m" nil
               "C-c C-h" 'clojure-cheatsheet
               "C-c m" 'cider-macroexpand-1)
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
      cider-repl-popup-stacktraces nil
      cider-popup-stacktraces nil
      cider-popup-on-error nil
      cider-repl-history-file "~/.emacs.d/nrepl-history")

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
  (cider-eval-sync
   "(require '[clojure.tools.namespace.repl :refer [refresh]])
    (refresh)"
   (cider-current-ns)))

;; custom test locations instead of foo_test.c use test/foo.c
(defun my-clojure-test-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (before (subseq segments 0 1))
         (after (subseq segments 1))
         (test-segments (append before (list "test") after)))
    (format "%stest/%s.clj"
            (locate-dominating-file buffer-file-name "src/")
            (mapconcat 'identity test-segments "/"))))

(defun my-clojure-test-implementation-for (namespace)
  (let* ((namespace (clojure-underscores-for-hyphens namespace))
         (segments (split-string namespace "\\."))
         (before (subseq segments 0 1))
         (after (subseq segments 2))
         (impl-segments (append before after)))
    (format "%s/src/%s.clj"
            (locate-dominating-file buffer-file-name "src/")
            (mapconcat 'identity impl-segments "/"))))

(setq clojure-test-for-fn 'my-clojure-test-for
      clojure-test-implementation-for-fn 'my-clojure-test-implementation-for)

(provide 'init-clojure)
