(require 'paredit-ext)
(require 'paredit)
(require-package 'evil-paredit)
(require 'evil-paredit)

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(eval-after-load "evil"
  '(evil-define-key 'normal paredit-mode-map
     (kbd "C-t") 'transpose-sexps
     "(" 'paredit-wrap-round
     ")" 'paredit-close-round-and-newline))

(fill-keymap paredit-mode-map
             "M-;" 'paredit-comment-dwim
             "M-s" 'paredit-splice-sexp
             "M-S" 'paredit-split-sexp
             "M-j" 'paredit-join-sexps

             "M-o" 'paredit-forward-down
             "M-O" 'paredit-forward-up
             "M-u" 'paredit-backward-down
             "M-U" 'paredit-backward-up

             "M-l" 'paredit-forward
             "M-h" 'paredit-backward
             "M-k" 'paredit-kill
             "M-(" 'backward-barf-sexp
             "M-)" 'forward-barf-sexp
             "C-(" 'backward-slurp-sexp
             "C-)" 'forward-slurp-sexp)

(defun my-paredit-mode-hook ()
  (make-local-variable 'surround-operator-alist)
  (add-to-list 'surround-operator-alist
               '(evil-paredit-change . change))
  (add-to-list 'surround-operator-alist
               '(evil-paredit-delete . delete)))
(add-hook 'paredit-mode-hook #'my-paredit-mode-hook)

(defun forward-barf-sexp (prefix)
  "Calls `paredit-forward-barf-sexp', unless PREFIX is non nil.
  With prefix it calls `paredit-barf-all-the-way-forward'"
  (interactive "P")
  (if prefix
      (paredit-barf-all-the-way-forward)
    (paredit-forward-barf-sexp)))

(defun forward-slurp-sexp (prefix)
  "Calls `paredit-forward-slurp-sexp`, unless PREFIX is non nil.
  With prefix it calls `paredit-slurp-all-the-way-forward'"
  (interactive "P")
  (if prefix
      (paredit-slurp-all-the-way-forward)
    (paredit-forward-slurp-sexp)))

(defun backward-barf-sexp (prefix)
  "Calls `paredit-backward-barf-sexp', unless PREFIX is non nil.
  With prefix it calls `paredit-barf-all-the-way-backward'"
  (interactive "P")
  (if prefix
      (paredit-barf-all-the-way-backward)
    (paredit-backward-barf-sexp)))

(defun backward-slurp-sexp (prefix)
  "Calls `paredit-backward-slurp-sexp', unless PREFIX is non nil.
  With prefix it calls `paredit-slurp-all-the-way-backward'"
  (interactive "P")
  (if prefix
      (paredit-slurp-all-the-way-backward)
    (paredit-backward-slurp-sexp)))

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun conditionally-enable-paredit-mode ()
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (enable-paredit-mode)))

(evil-define-operator evil-paredit-change-line
  (beg end type register yank-handler)
  "Change to end of line respecting parenthesis."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-change beg end type register yank-handler)))

(defun evil-paredit-kill-end ()
  "Returns the position where paredit-kill would kill to"
  ;; (when (paredit-in-char-p)             ; Move past the \ and prefix.
  ;;   (backward-char 2))                  ; (# in Scheme/CL, ? in elisp)
  (let ((depth-at-point (first (paredit-current-parse-state)))
        (depth-at-at-line-beginning (save-excursion
                                      (goto-char (point-at-bol))
                                      (first (paredit-current-parse-state)))))
    (cond ((paredit-in-string-p)
           (save-excursion (progn (paredit-forward-up) (backward-char) (point))))
          ((= depth-at-point depth-at-at-line-beginning)
           (point-at-eol))
          ((paredit-in-comment-p)
           (paredit-check-forward-delete-in-comment)
           (evil-paredit-delete (point) (1+ (point-at-eol))))
          (t (save-excursion (paredit-forward-up) (1- (point)))))))

(evil-define-operator evil-paredit-delete-line
  (beg end type register yank-handler)
  "Delete to end of line respecting parenthesis."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-delete beg end
                         type register yank-handler)))

(evil-define-operator evil-paredit-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (evil-paredit-kill-end)))
    (evil-paredit-yank beg end type register)))

(defadvice evil-backward-paragraph (around wrap-with-prefix activate)
  (when current-prefix-arg
    (paredit-wrap-curly)))

(defadvice evil-forward-paragraph (around wrap-with-prefix activate)
  (when current-prefix-arg
    (paredit-wrap-square)))

(provide 'init-paredit)
