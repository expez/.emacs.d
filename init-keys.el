(require 'evil)
(require 'helm-descbinds)

(helm-descbinds-mode)

(defkeymap misc-map
  "c" 'ethan-wspace-clean-all
  "d" 'diff-current-buffer-with-file
  "D" 'delete-current-file
  "f" 'helm-find-files
  "g" 'gist-region-or-buffer
  "l" 'gist-list
  "i" 'indent-region-or-buffer
  "o" 'other-frame
  "p" 'pretty-mode
  "R" 'rainbow-mode
  "r" 'rename-file-and-buffer
  "t" (lambda () (interactive) (org-capture nil "t"))
  "T" 'org-todo-list
  "u" 'winner-undo
  "v" 'visual-line-mode
  "w" 'ethan-wspace-clean-all-modes
  "W" 'toggle-whitespace-mode)

(fill-keymap evil-normal-state-map
             "Y"     (kbd "y$")
             "Q" (lambda () (interactive) (evil-execute-macro 1 last-kbd-macro))
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
             "C-h" 'backward-delete-char)

(fill-keymap evil-visual-state-map
             "u" 'undo-tree-undo)

(fill-keymap evil-operator-state-map
             "SPC" 'evil-ace-jump-char-to-mode ; works like `t'
             "C-SPC" 'evil-ace-jump-char-mode ; works like `f'
             "S-SPC" 'evil-ace-jump-line-mode
             "K" misc-map)

(fill-keymap evil-visual-state-map "K" misc-map)
(fill-keymap evil-motion-state-map "K" misc-map)

(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(fill-keymap evil-motion-state-map
             "C-e" 'move-end-of-line
             "C-a" 'smart-line-beginning)

(define-key visual-line-mode-map
  [remap evil-next-line] 'evil-next-visual-line)
(define-key visual-line-mode-map
  [remap evil-previous-line] 'evil-previous-visual-line)

(evil-leader/set-key
  "." 'evil-ex
  "B" 'eproject-kill-project-buffers
  "K" 'kill-buffer-and-window
  "R" 'revert-all-buffers
  "W" 'save-some-buffers
  "A" 'align-regexp
  "a" 'ack
  "b" 'eproject-switch-to-buffer
  "c" 'compile
  "C" 'toggle-bury-compilation-buffer
  "d" 'dired-jump
  "f" 'eproject-find-file
  "g" 'magit-status
  "j" 'bookmark-jump
  "e" 'mc/edit-lines
  "/" 'mc/mark-all-like-this-dwim
  "k" 'kill-buffer
  "p" 'eproject-revisit-project
  "r" 'revert-buffer
  "s" 'eshell-toggle
  "v" 'vc-annotate
  "w" 'save-buffer
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9
  "!" 'wg-switch-to-index-1
  "@" 'wg-switch-to-index-2
  "#" 'wg-switch-to-index-3
  "$" 'wg-switch-to-index-4
  "%" 'wg-switch-to-index-5
  "^" 'wg-switch-to-index-6
  "&" 'wg-switch-to-index-7)

(fill-keymap 'global
             "C-x m" 'ido-hacks-execute-extended-command
             "C-x C-m" 'ido-hacks-execute-extended-command
             "C-x c" 'compile
             "C-x C-c" 'compile
             "C-x s" '(lambda ()(interactive)(ansi-term "/bin/zsh"))
             "C-x F" 'find-file-as-root
             "M->" 'mc/mark-next-like-this
             "M-<" 'mc/mark-previous-like-this
             "C-x C-b" 'ibuffer
             "C-x r v" 'register-list
             "C-c e" 'eval-and-replace
             "C-<tab>" 'hippie-expand
             "<f8>" 'ispell-word
             "<f5>" 'eshell-toggle
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

(eval-after-load "workgroups"
  '(fill-keymap 'global "C-x w" wg-map))

(eval-after-load "vc-annotate"
  '(fill-keymap vc-annotate-mode-map
                "j" 'next-line
                "J" 'vc-annotate-revision-at-line
                "k" 'previous-line))

(eval-after-load "org-agenda"
  '(fill-keymap org-agenda-mode-map
               "j" 'org-agenda-next-line
               "k" 'org-agenda-previous-line
              ;"J" 'org-agenda-goto-date
               "K" 'org-agenda-capture))

(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(evil-define-key 'normal compilation-minor-mode-map "RET" 'compile-goto-error)
(evil-define-key 'normal compilation-minor-mode-map "q" 'quit-window)

(defalias 'rfb 'rename-file-and-buffer)
(defalias 'mbf 'move-buffer-file)
(defalias 'rb 'revert-buffer)
(defalias 'oaf 'open-all-files-with-extension)

(defalias 'pl 'package-list-packages)

(defalias 'gl 'gist-list)
(defalias 'grb 'gist-region-or-buffer)

(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

(key-chord-define-global "qr" 'query-replace-regexp)
(key-chord-define-global "qm" 'moccur)

(provide 'init-keys)
