(require 'clojure-mode)
(require 'nrepl)
(require 'ac-nrepl)
(require 'clj-refactor)
(require 'ac-nrepl)
(require 'align-cljlet)
(require 'clojure-test-mode)
(require 'elein)
(require 'slamhound)

(eval-after-load 'cider
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

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
  (paredit-mode 1))
(add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-hook)

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (clj-refactor-mode 1)
  (cider-mode 1)
  (paredit-mode 1)
  (cljr-add-keybindings-with-prefix "C-c")
  (fill-keymap evil-normal-state-local-map
               "M-." 'cider-jump
               "M-," 'cider-jump-back
               "M-TAB" 'complete-symbol))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(setq nrepl-hide-special-buffers t)
(setq cider-repl-popup-stacktraces t)
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

(provide 'init-clojure)
