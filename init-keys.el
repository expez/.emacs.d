(defkeymap misc-map
  "w" 'toggle-whitespace
  "g" 'gist-region-or-buffer
  "l" 'gist-list
  "r" 'rename-file-and-buffer
  "D" 'delete-current-file
  "d" 'diff-current-buffer-with-file
  "p" 'pretty-mode
  "d" 'diff-current-buffer-with-file
  "v" 'visual-line-mode)

(fill-keymap evil-normal-state-map
             "Y"     (kbd "y$")
             "U" 'universal-argument
             "<kp-add>" 'evil-numbers/inc-at-pt
             "<kp-subtract>" 'evil-numbers/dec-at-pt
             "C-SPC" 'evil-ace-jump-char-mode
             "SPC" 'evil-ace-jump-word-mode
             "S-SPC" 'evil-ace-jump-line-mode
             ":"     'evil-repeat-find-char-reverse
             "C-e" 'move-end-of-line
             "C-a" 'smart-line-beginning
             "go" 'goto-char
             "M-," 'pop-tag-mark
             "C-j" 'open-line-below
             "C-k" 'open-line-above
             "M-n" 'next-error
             "M-p" 'previous-errror
             "<down>" 'move-text-down
             "<up>" 'move-text-up
             "C-u" 'evil-scroll-up
             "K" misc-map)

(fill-keymap evil-insert-state-map
             "C-y" 'yank
             "C-v" 'quoted-insert
             "M-y" 'yank-pop
             "C-d" 'delete-char
             "C-e" 'end-of-line
             "C-h" 'backward-delete-char
             "C-[" 'evil-force-normal-state)

(fill-keymaps (list evil-operator-state-map
                    evil-visual-state-map)
              "SPC" 'evil-ace-jump-char-to-mode ; works like `t'
              "C-SPC" 'evil-ace-jump-char-mode ; works like `f'
              "S-SPC" 'evil-ace-jump-line-mode)

(fill-keymap evil-motion-state-map
             "C-e" 'move-end-of-line
             "C-a" 'smart-line-beginning)

(evil-define-key 'normal markdown-mode
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(evil-define-key 'normal tex-mode
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(evil-leader/set-key
  "." 'evil-ex
  "B" 'eproject-kill-project-buffers
  "K" 'kill-buffer-and-window
  "R" 'revert-all-buffers
  "W" 'save-some-buffers
  "a" 'align-rexep
  "b" 'eproject-switch-to-buffer
  "c" 'compile
  "d" 'dired-jump
  "f" 'eproject-find-file
  "g" 'magit-status
  "j" 'bookmark-jump
  "k" 'kill-buffer
  "p" 'eproject-revisit-project
  "r" 'revert-buffer
  "s" 'eshell-toggle
  "v" 'vc-annotate
  "w" 'save-buffer
  "u" 'winner-undo
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9)

(fill-keymap 'global
             "C-x m" 'execute-extended-command
             "C-x C-m" 'execute-extended-command
             "C-x c" 'compile
             "C-x C-c" 'compile
             "C-x s" 'eshell-toggle
             "C-x F" 'find-file-as-root
             "C-x C-b" 'ibuffer
             "C-x r v" 'register-list
             "C-<tab>" 'hippie-expand
             "<f8>" 'ispell-word
              "<end>" 'sr-speedbar-toggle
             "M-S-<f8>" 'flyspell-buffer
             "C-<f8>" 'flyspell-check-previous-highlighted-word
             "M-<f8>" 'flyspell-check-next-highlighted-word
             "C-x r q" 'save-buffers-kill-terminal
             "C-x v t" 'vc-create-tag
             "C-x a r" 'align-regexp
             "C-h C-f" 'find-function
             "C-x i" 'ido-goto-symbol
             "C-x C-r" 'ido-recentf-open
             "<f6>" 'toggle-deft-mode
             "M-<backspace>" 'delete-indentation)

(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(defalias 'rfb 'rename-file-and-buffer)
(defalias 'mbf 'move-buffer-file)
(defalias 'rb 'revert-buffer)
(defalias 'oaf 'open-all-files-with-extension)

(defalias 'gl 'gist-list)
(defalias 'grb 'gist-region-or-buffer)

(key-chord-define-global "qr" 'query-replace-regexp)
(key-chord-define-global "qm" 'moccur)

(provide 'init-keys)
