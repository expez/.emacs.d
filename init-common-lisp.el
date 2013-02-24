(require 'slime)

(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (slime-setup '(slime-fancy))
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)
            (slime-mode 1)
            (pretty-lambdas)
            (turn-on-eldoc-mode)
            (define-key lisp-mode-map (kbd "C-c l") 'lispdoc)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)
            (rainbow-delimiters-mode 0)
            (evil-paredit-mode 1)
            (set-face-foreground 'paren-face "grey30")))

(setq inferior-lisp-program "/usr/bin/sbcl --noinform"
      lisp-lambda-list-keyword-alignment t
      lisp-lambda-list-keyword-parameter-alignment t)

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-mode-hook 'cliki:start-slime)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(let ((fasl-dir (expand-file-name "~/tmp/slime-fasls/")))
  (make-directory fasl-dir t)
  (setq slime-compile-file-options `(:fasl-directory ,fasl-dir)))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (paredit-mode +1)
            (evil-paredit-mode 1)))

(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(defslime-repl-shortcut slime-quickload ("quickload" "ql")
  (:handler #'cofi/slime-repl-quickload)
  (:one-liner "Load system from quickload distribution"))

(defun cofi/slime-repl-quickload ()
  (interactive)
  (let ((system-name
         (completing-read "System: "
                          (slime-eval '(cl:mapcar 'ql-dist:system-file-name
                                                  (ql:system-list)))
                          nil t)))
    (slime-eval-async
        `(cl:progn (ql:quickload ,system-name)
                   (cl:format t "; Loaded system \"~A\".~%" ,system-name)))))

(add-auto-mode 'lisp-mode "\.cl$" )

(defun lispdoc ()
  "Searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
         (symbol-at-point (symbol-at-point))
         (default (symbol-name symbol-at-point))
         (inp (read-from-minibuffer
               (if (or word-at-point symbol-at-point)
                   (concat "Symbol (default " default "): ")
		 "Symbol (no default): "))))
    (if (and (string= inp "") (not word-at-point) (not
						   symbol-at-point))
        (message "you didn't enter a symbol!")
      (let ((search-type (read-from-minibuffer
			  "full-text (f) or basic (b) search (default b)? ")))
	(browse-url (concat "http://lispdoc.com?q="
			    (if (string= inp "")
				default
			      inp)
			    "&search="
			    (if (string-equal search-type "f")
				"full+text+search"
			      "basic+search")))))))

(provide 'init-common-lisp)