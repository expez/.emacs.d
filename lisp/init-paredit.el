(require 'paredit-ext)
(require 'paredit)
(require-package 'evil-paredit)
(require 'evil-paredit)

(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defun paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

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
             "M-k" 'paredit-splice-sexp-killing-backward
             "M-K" 'paredit-splice-sexp-killing-forward)

(defun my-paredit-mode-hook ()
  (make-local-variable 'evil-surround-operator-alist)
  (add-to-list 'evil-surround-operator-alist
               '(evil-paredit-change . change))
  (add-to-list 'evil-surround-operator-alist
               '(evil-paredit-delete . delete))
  (eval-after-load "evil"
    '(progn (evil-define-key 'normal paredit-mode-map
              (kbd "C-t") 'transpose-sexps
              "(" 'paredit-wrap-round
              "[" 'paredit-wrap-square
              "{" 'paredit-wrap-curly)
            (fill-keymap evil-normal-state-local-map
                         "C-9" 'backward-barf-sexp
                         "C-0" 'forward-barf-sexp
                         "M-9" 'backward-slurp-sexp
                         "M-0" 'forward-slurp-sexp))))
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
         (end (evil-paredit-kill-to)))
    (evil-paredit-change beg end type register yank-handler)))

(defun evil-paredit-kill-to ()
  "Returns the position where paredit-kill would kill to"
  ;; (when (paredit-in-char-p)             ; Move past the \ and prefix.
  ;;   (backward-char 2))                  ; (# in Scheme/CL, ? in elisp)
  (let ((depth-at-point (first (paredit-current-parse-state)))
        (depth-at-eol (save-excursion
                        (goto-char (point-at-eol))
                        (evil-paredit-depth-at-point))))
    (cond ((paredit-in-string-p)
           (min (point-at-eol)
                (cdr (paredit-string-start+end-points))))
          ((= depth-at-point depth-at-eol)
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
         (end (evil-paredit-kill-to)))
    (evil-paredit-delete beg end
                         type register yank-handler)))

(evil-define-operator evil-paredit-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (let* ((beg (point))
         (end (evil-paredit-kill-to)))
    (evil-paredit-yank beg end type register)))

(defadvice evil-backward-paragraph (around wrap-with-prefix activate)
  (when current-prefix-arg
    (paredit-wrap-curly)))

(defadvice evil-forward-paragraph (around wrap-with-prefix activate)
  (when current-prefix-arg
    (paredit-wrap-square)))

(defun evil-paredit-depth-at-point ()
  "Returns the depth in s-expressions, or strings, at point."
  (let ((depth (first (paredit-current-parse-state))))
    (if (paredit-in-string-p)
        (1+ depth)
      depth)))

(evil-define-operator evil-paredit-delete
  (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE respecting parenthesis.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  ;; dwim, don't just fail, delete as much as possible
  (when (or (eq type 'line)
            (eq type 'block))
    (save-excursion
      (let ((depth-beg (progn (goto-char beg)
                              (if (looking-at "(")
                                  (max 0 (1- (evil-paredit-depth-at-point)))
                                (evil-paredit-depth-at-point))))
            (depth-end (progn (goto-char (1- end)) (evil-paredit-depth-at-point))))
        (unless (eq depth-beg depth-end)
          (move-end-of-line nil)
          (paredit-backward-down (- depth-beg depth-end))
          (setq end (point))))))

  (evil-paredit-yank beg end type register yank-handler)
  (if (eq type 'block)
      (evil-apply-on-block #'delete-region beg end nil)
    (kill-region beg end))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank))
  (indent-for-tab-command))

(evil-define-operator evil-paredit-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (unless
      (and (eq type 'block)
           (-every?
            (lambda (s)
              (not
               (s-contains? "\\((\\)\\|\\()\\)" s))) (extract-rectangle beg end)))
    (if (fboundp 'paredit-check-region-state)
        (-evil-paredit-check-region beg end)
      (paredit-check-region-for-delete beg end)))
  (cond
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

;;; The original uses `backward-up-list`, so only works correctly when wrapping parens
(defun paredit-wrap-sexp (&optional argument open close)
  "Wrap the following S-expression.
If a `C-u' prefix argument is given, wrap all S-expressions following
  the point until the end of the buffer or of the enclosing list.
If a numeric prefix argument N is given, wrap N S-expressions.
Automatically indent the newly wrapped S-expression.
As a special case, if the point is at the end of a list, simply insert
  a parenthesis pair, rather than inserting a lone opening delimiter
  and then signalling an error, in the interest of preserving
  structure.
By default OPEN and CLOSE are round delimiters."
  (interactive "P")
  (paredit-lose-if-not-in-sexp 'paredit-wrap-sexp)
  (let ((open (or open ?\( ))
        (close (or close ?\) )))
    (paredit-handle-sexp-errors
        ((lambda (n) (paredit-insert-pair n open close 'goto-char))
         (cond ((integerp argument) argument)
               ((consp argument) (paredit-count-sexps-forward))
               ((paredit-region-active-p) nil)
               (t 1)))
      (insert close)
      (backward-char)))
  (save-excursion (if (not (eq open ?\{))
                      (paredit-backward-up)
                    (paredit-backward)
                    (backward-char))
                  (indent-sexp)))

(provide 'init-paredit)
