;;Minor mode map consist of all my bindings

(defvar ex-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x m") 'ido-hacks-execute-extended-command)
    (define-key map (kbd "C-x C-m") 'ido-hacks-execute-extended-command)
    (define-key map (kbd "C-x c") 'compile)
    (define-key map (kbd "C-x C-c") 'compile)
    (define-key map (kbd "C-x s") 'shell)
    (define-key map (kbd "C-x C-b") 'ibuffer)
    (define-key map (kbd "C-x r v") 'register-list)
    (define-key map (kbd "C-<tab>") 'hippie-expand)
    ;; easy spell check
    (define-key map (kbd "<f8>") 'ispell-word)
    ;;(global-set-key (kbd "C-S-<f8>") 'fd-switch-dictionary)
    (define-key map (kbd "M-S-<f8>") 'flyspell-buffer)
    (define-key map (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
    (define-key map (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

    (define-key map (kbd "C-x r q") 'save-buffers-kill-terminal)
    (define-key map (kbd "C-x v t") 'vc-create-tag)
    (define-key map (kbd "C-x v s") 'magit-status)

    (define-key map (kbd "C-x a r") 'align-regexp)
    (define-key map (kbd "C-x i") 'ido-goto-symbol)

    (define-key map (kbd "C-x C-r") 'ido-recentf-open)
    (define-key map (kbd "M-<backspace>") 'delete-indentation)

    map)
  "Keymap containing all my bindings. ")

(defun turn-on-ex-mode ()
  (interactive)
  "Enable 'ex-mode'"
  (ex-mode 1))

(define-minor-mode ex-mode
  "Turns on all my key-bindings and activate all functions."
  :global t
  :init-value nil
  :keymap ex-mode-keymap
  :lighter " Ex"

  (defalias 'rfb 'rename-file-and-buffer)
  (defalias 'mbf 'move-buffer-file)
  (defalias 'rb 'revert-buffer)
  (defalias 'oaf 'open-all-files-with-extension)
  (defalias 'dbf 'diff-buffer-with-file)

  (defalias 'gl 'gist-list)
  (defalias 'grb 'gist-region-or-buffer)

  ;;Evil mode
  (evil-ex-define-cmd "n[ew]" 'evil-window-new)
  (fill-keymap evil-normal-state-map
               "Y"     (kbd "y$")
               "<kp-add>" 'evil-numbers/inc-at-pt
               "<kp-subtract>" 'evil-numbers/dec-at-pt
                "C-SPC" 'evil-ace-jump-char-mode
                "SPC" 'evil-ace-jump-word-mode
                "S-SPC" 'evil-ace-jump-line-mode
                ":"     'evil-repeat-find-char-reverse
                "C-e" 'move-end-of-line
                "C-a" 'smart-line-beginning
                "go" 'goto-char
                "M-," 'pop-tag-mark)

  (fill-keymap evil-insert-state-map
               "C-h" 'backward-delete-char
               "C-[" 'evil-force-normal-state)

  (fill-keymaps (list evil-operator-state-map
                      evil-visual-state-map)
                "SPC" 'evil-ace-jump-char-to-mode ;; works like `t'
                "C-SPC" 'evil-ace-jump-char-mode ;; works like `f'
                "S-SPC" 'evil-ace-jump-line-mode)

  (fill-keymap evil-motion-state-map
                "C-e" 'move-end-of-line
                "C-a" 'smart-line-beginning)

  (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
    "K" 'magit-discard-item
    "L" 'magit-key-mode-popup-logging)

  (evil-add-hjkl-bindings magit-status-mode-map 'emacs
    "K" 'magit-discard-item
    "l" 'magit-key-mode-popup-logging
    "h" 'magit-toggle-diff-refine-hunk)

  (fill-keymap magit-log-mode-map
               "j" 'magit-goto-next-section
               "k" 'magit-goto-previous-section)

  (evil-add-hjkl-bindings rebase-mode 'emacs
    "h" 'describe-mode)

  (evil-leader/set-key
   "w" 'save-buffer
   "W" 'save-some-buffers
   "k" 'kill-buffer
   "K" 'kill-buffer-and-window
   "d" 'dired-jump
   "D" 'diff-buffer-with-file
   "c" 'compile
   "a" 'align-rexep
   "f" 'eproject-find-file
   "g" 'magit-status
   "." 'evil-ex
   "u" 'winner-undo)

  (evil-define-key 'normal paredit-mode-map
    (kbd "M-l") 'paredit-forward-slurp-sexp
    (kbd "M-h") 'paredit-backward-slurp-sexp
    (kbd "M-H") 'paredit-backward-barf-sexp
    (kbd "M-L") 'paredit-forward-barf-sexp
    (kbd "M-s") 'paredit-splice-sexp
    (kbd "M-S") 'paredit-split-sexp
    (kbd "M-j") 'paredit-join-sexps
    (kbd "M-k") 'paredit-kill
    (kbd "(") 'paredit-backward
    (kbd ")") 'paredit-forward)

  (define-key evil-normal-state-map (kbd "C-j") 'open-line-below)
  (define-key evil-normal-state-map (kbd "C-k") 'open-line-above)

  (define-key evil-normal-state-map (kbd "M-n") 'next-error)
  (define-key evil-normal-state-map (kbd "M-p") 'previous-errror)
  (define-key evil-normal-state-map (kbd "<left>") 'evil-prev-buffer)
  (define-key evil-normal-state-map (kbd "<right>") 'evil-next-buffer)

  (define-key evil-normal-state-map (kbd "<down>") 'move-text-down)
  (define-key evil-visual-state-map (kbd "<down>") 'move-text-down)
  (define-key evil-visual-state-map (kbd "<up>") 'move-text-up)
  (define-key evil-normal-state-map (kbd "<up>") 'move-text-up)

  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

  (define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)

  (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  (define-key winner-mode-map (kbd "C-x 7") 'winner-undo)
  (define-key winner-mode-map (kbd "C-x 9") 'winner-redo)

  (define-key lisp-mode-map (kbd "C-c l") 'lispdoc)

  (global-set-key (kbd "<end>") 'sr-speedbar-toggle)

  (define-key ac-completing-map
    (kbd "C-[") (lambda () (interactive "")
                  (ac-abort)
                  (evil-force-normal-state)))
  (fill-keymap ac-complete-mode-map
               "C-l" 'ac-expand-common
               "C-j" 'ac-next
               "C-k" 'ac-previous
               "ESC" 'ac-stop)
  (key-chord-define-global "qr" 'query-replace-regexp)
  (key-chord-define-global "qm" 'moccur))

(provide 'ex-mode)
