;; My function aliases.

(defalias 'rfb 'rename-file-and-buffer)
(defalias 'mbf 'move-buffer-file)
(defalias 'qr 'query-replace)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rb 'revert-buffer)

;;Ergomacs bindings for movement
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-i" ) 'previous-line)

(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)

(global-set-key (kbd "M-U") 'backward-paragraph)
(global-set-key (kbd "M-O") 'forward-paragraph)

(global-set-key (kbd "M--") 'toggle-letter-case)
;;Swap a bunch of bindings around.
(global-set-key [f5] 'call-last-kbd-macro)
(global-set-key (kbd "C-a" ) 'smart-line-beginning)
(global-set-key "\C-x\C-c" 'whole-line-or-region-kill-ring-save)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "M-w") 'backward-kill-line)
(global-set-key "\C-xm" 'smex)
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-xM" 'smex-major-mode-commands)
(global-set-key "\C-o" 'undo)
(global-set-key "\C-z" 'open-line)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "C-,") 'beginning-of-buffer)
(global-set-key (kbd "C-.") 'end-of-buffer)
(global-set-key "\C-a" 'back-to-indentation)
(global-set-key "\C-xg" 'goto-line)
(global-set-key (kbd "C-x G") 'goto-char)
(global-set-key "\C-xc" 'compile)
(global-set-key "\C-xs" 'shell)
(global-set-key "\C-cn" 'next-error)
(global-set-key (kbd "C-z") 'open-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x r v") 'register-list)
(global-set-key "\C-c\C-u" 'whole-line-or-region-comment-dwim)
(global-set-key (kbd "C-<tab>") 'hippie-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)
(global-set-key (kbd "M-S-k") 'kill-sentence)
;; easy spell check
(global-set-key (kbd "<f8>") 'ispell-word)
;;(global-set-key (kbd "C-S-<f8>") 'fd-switch-dictionary)
(global-set-key (kbd "M-S-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

(global-set-key "\C-cq" 'save-buffers-kill-terminal)
(global-set-key [insert]    'overwrite-mode) ; [Ins]
(global-set-key [kp-insert] 'overwrite-mode) ; [Ins]
(global-set-key "\C-xvt" 'vc-create-tag)
(global-set-key "\C-xvs" 'magit-status)
(global-set-key (kbd "C-�") 'push-mark-command)
(global-set-key (kbd "M-�") 'exchange-point-and-mark)
(global-set-key (kbd "C-�") 'ace-jump-mode)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "M-p") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-down-line)

(global-set-key (kbd "M-g c") 'goto-char)
(global-set-key (kbd "M-g M-c") 'goto-char)
(define-key yas/minor-mode-map (kbd "TAB") nil)
(define-key yas/minor-mode-map (kbd "<tab>") nil)
(global-set-key (kbd "M-m") 'er/expand-region)

;; Windmove

(global-set-key (kbd "<left>")  'windmove-left)
(global-set-key (kbd "<up>")    'windmove-up)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<down>")  'windmove-down)

;;Evil mode

(evil-ex-define-cmd "n[ew]" 'evil-window-new)
(key-chord-define-global "lj" 'evil-normal-state)
(key-chord-define-global "qr" 'query-replace-regexp)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
