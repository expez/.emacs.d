(require 'paredit-ext)
(require 'paredit)

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(eval-after-load "evil"
  '(evil-define-key 'normal paredit-mode-map
     (kbd "C-t") 'transpose-sexps
     "(" 'paredit-wrap-round
     ")" 'paredit-close-round-and-newline))

(fill-keymap paredit-mode-map
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
  (let ((paren-count (count-matches "(" (line-beginning-position)
                                    (line-end-position)))
        (last-balanced-paren (evil-paredit-position-of
                              "\)"
                              (line-beginning-position)
                              (line-end-position))))
    (evil-paredit-change beg last-balanced-paren type register yank-handler)))

(defun evil-paredit-position-of (regexp start stop &optional nth)
  "Returns the buffer position of the `nth' occurrence of
  `regexp' between buffer positions `start' and `stop'"
  (save-excursion
    (goto-char start)
    (re-search-forward regexp stop (or nth 1))
    (point)))

(evil-define-operator evil-paredit-delete-line
  (beg end type register yank-handler)
  "Delete to end of line respecting parenthesis."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  ;; act linewise in Visual state
  (let* ((beg (or beg (point)))
         (end (or end beg))
         ;; NOTE the following count will be off when it encounters
         ;; parens in strings.
         (paren-count (count-matches "(" (line-beginning-position)
                                     (line-end-position)))
         (last-balanced-paren (evil-paredit-position-of
                               "\)"
                               (line-beginning-position)
                               (line-end-position))))
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond
     ((eq type 'block)
      ;; equivalent to $d, i.e., we use the block-to-eol selection and
      ;; call `evil-delete'. In this case we fake the call to
      ;; `evil-end-of-line' by setting `temporary-goal-column' and
      ;; `last-command' appropriately as `evil-end-of-line' would do.
      (let ((temporary-goal-column most-positive-fixnum)
            (last-command 'next-line))
        (evil-paredit-delete beg end 'block register yank-handler)))
     ((eq type 'line)
      (evil-paredit-delete beg end type register yank-handler))
     (t
      (evil-paredit-delete beg last-balanced-paren
                           type register yank-handler)))))

(evil-define-operator evil-paredit-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if (fboundp 'paredit-check-region-state)
      (-evil-paredit-check-region beg end)
    (paredit-check-region-for-delete beg end))
  (cond
   ((eq type 'block)
    (evil-yank-rectangle beg end register yank-handler))
   ((eq type 'line)
    (evil-yank-lines beg end register yank-handler))
   (t
    (evil-yank-characters beg end register yank-handler))))

(provide 'init-paredit)
