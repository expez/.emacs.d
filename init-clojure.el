(require 'clojure-mode)
(require 'nrepl)
(require 'ac-nrepl)
(require 'clj-refactor)
(require 'ac-nrepl)
(require 'align-cljlet)
(require 'clojure-test-mode)
(require 'elein)
(require 'slamhound)

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook
          'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(add-auto-mode 'clojure-mode "\\.cljs\\'")

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(add-hook 'nrepl-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

(defadvice clojure-test-run-tests (before save-first activate)
  (save-buffer))

(defadvice nrepl-load-current-buffer (before save-first activate)
  (save-buffer))

(defun my-clojure-mode-hook ()
  (rainbow-delimiters-mode 0)
  (clj-refactor-mode 1)
  (cider-mode 1)
  (cljr-add-keybindings-with-prefix "C-c")
  (fill-keymap evil-normal-state-local-map
                          "M-." 'cider-jump
                          "M-," 'cider-jump-back
                          "M-TAB" 'complete-symbol))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(define-key clojure-mode-map (kbd "s-j") 'clj-jump-to-other-file)

(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")

(add-hook 'nrepl-connected-hook
          (defun my-nrepl-eldoc-hook ()
            (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
            (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
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
