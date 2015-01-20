(require 'evil)
(require-package 'helm-descbinds)
(require-package 'key-chord)
(helm-descbinds-mode)

(defkeymap leader-map
  "." 'evil-ex
  "B" 'projectile-kill-buffers
  "K" 'kill-buffer-and-window
  "R" 'revert-all-buffers
  "W" 'save-some-buffers
  "A" 'align-regexp
  "a" 'ag-regexp
  "b" 'projectile-switch-to-buffer
  "c" 'compile
  "C" 'toggle-bury-compilation-buffer
  "d" 'dired-jump
  "f" 'projectile-find-file
  "g" 'magit-status
  "j" 'bookmark-jump
  "e" 'edit-user-config-file
  "h" 'change-whitespace-line-column
  "i" 'evil-iedit-state/iedit-mode
  "k" 'kill-buffer
  "SPC" 'clean-and-save-buffer
  "p" 'projectile-switch-project
  "r" 'revert-buffer
  "s" 'esk-eshell-in-dir
  "S" 'find-shell-init-file
  "u" 'run-term
  "t" 'test
  "v" 'vc-annotate
  "w" 'save-buffer
  "," 'yas-insert-snippet
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9
  "!" 'wg-switch-to-workgroup-at-index-0
  "@" 'wg-switch-to-workgroup-at-index-1
  "#" 'wg-switch-to-workgroup-at-index-2
  "$" 'wg-switch-to-workgroup-at-index-3
  "%" 'wg-switch-to-workgroup-at-index-4
  "^" 'wg-switch-to-workgroup-at-index-5
  "&" 'wg-switch-to-workgroup-at-index-6
  "*" 'wg-switch-to-workgroup-at-index-7
  "(" 'wg-switch-to-workgroup-at-index-8)

;;; quick hack because wg rebound it
(defun winner-undo- ()
  (interactive)
  (winner-undo))

(defkeymap misc-map
  "c" 'ethan-wspace-clean-all
  "d" 'diff-current-buffer-with-file
  "D" 'delete-current-file
  "e" 'buffer-to-utf8
  "f" 'helm-find-files
  "g" 'gist-region-or-buffer
  "l" 'gist-list
  "i" 'indent-region-or-buffer
  "o" 'other-frame
  "p" 'pretty-mode
  "s" 'flyspell-buffer
  "R" 'rainbow-mode
  "r" 'rename-file-and-buffer
  "t" 'new-todo
  "T" 'org-todo-list
  "u" 'winner-undo-
  "v" 'visual-line-mode
  "W" 'ethan-wspace-clean-all-modes
  "w" 'toggle-whitespace-mode)

(defun evil-execute-last-kbd-macro (count)
  "Calls the most recently defined kbd macro COUNT times"
  (interactive
   (let (count)
     (setq count (if current-prefix-arg
                     (if (numberp current-prefix-arg)
                         current-prefix-arg
                       0) 1))

     (list count)))
  (evil-execute-macro count last-kbd-macro))

(fill-keymap evil-normal-state-map
             "'" 'evil-use-register
             "\"" 'evil-goto-mark-line
             "Y"     (kbd "y$")
             "Q" 'evil-execute-last-kbd-macro
             "U" 'universal-argument
             "<kp-add>" 'evil-numbers/inc-at-pt
             "<kp-subtract>" 'evil-numbers/dec-at-pt
             "C-SPC" 'evil-ace-jump-char-mode
             "SPC" 'evil-ace-jump-word-mode
             "S-SPC" 'evil-ace-jump-line-mode
             ":" 'evil-repeat-find-char-reverse
             "C-e" 'move-end-of-line
             "C-a" 'smart-line-beginning
             "gc" 'goto-char
             "gf" 'ispell-word
             "gp" 'flyspell-check-previous-highlighted-word
             "gn" 'flyspell-check-next-highlighted-word
             "gu" 'evil-upcase
             "gU" 'evil-downcase
             "M-," 'pop-tag-mark
             "C-j" 'open-line-below
             "C-k" 'open-line-above
             "M-n" 'next-error
             "M-p" 'previous-error
             "C-u" 'evil-scroll-up
             "gs" 'just-one-space
             "gS" 'delete-blank-lines
             "K" misc-map
             "," leader-map)

(fill-keymap evil-window-map
             "M-h" 'buf-move-left
             "M-l" 'buf-move-right
             "M-j" 'buf-move-down
             "M-k" 'buf-move-up)

(defun add-c-e-to-local-map ()
  (define-key evil-insert-state-local-map (kbd "C-e") 'move-end-of-line))
(advice-add 'evil-initialize :after #'add-c-e-to-local-map)

(fill-keymap evil-insert-state-map
             "C-a" 'smart-line-beginning
             "C-y" 'yank
             "C-v" 'quoted-insert
             "M-y" 'yank-pop
             "C-å" 'evil-force-normal-state
             "C-d" 'delete-char
             "C-e" 'end-of-line
             "C-k" nil)

(fill-keymap evil-visual-state-map
             "'" 'evil-use-register
             "\"" 'evil-goto-mark-line
             "u" 'undo-tree-undo
             "," leader-map
             "K" misc-map)

(fill-keymap evil-operator-state-map
             "SPC" 'evil-ace-jump-char-to-mode ; works like `t'
             "C-SPC" 'evil-ace-jump-char-mode ; works like `f'
             "S-SPC" 'evil-ace-jump-line-mode
             "K" misc-map
             "," leader-map)

(fill-keymap evil-motion-state-map
             "K" misc-map
             "," leader-map)

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

(fill-keymap 'global
             "C-h h" nil
             "C-h g" nil
             "C-h c" nil
             "C-x v p" 'git-messenger:popup-message
             "C-x m" 'ido-hacks-execute-extended-command
             "C-x C-m" 'ido-hacks-execute-extended-command
             "C-x c" 'compile
             "C-x C-c" 'compile
             "C-x s" '(lambda ()(interactive)(ansi-term "/bin/zsh"))
             "C-x F" 'find-file-as-root
             "C-x C-b" 'ibuffer
             "C-x r v" 'register-list
             "C-x e" 'eval-and-replace
             "C-<tab>" 'hippie-expand
             "<f5>" 'eshell-toggle
             "<f2>" 'neotree-toggle
             "<pause>" 'toggle-debug
             "C-x r q" 'save-buffers-kill-terminal
             "C-x v t" 'vc-create-tag
             "C-x a r" 'align-regexp
             "C-h C-f" 'find-function
             "C-h C-k" 'find-function-on-key
             "C-h C-v" 'find-variable
             "C-h C-l" 'find-library
             "M-:" 'pp-eval-expression
             "C-x i" 'ido-goto-symbol
             "C-x C-r" 'ido-recentf-open
             "<f6>" 'toggle-deft-mode
             "C-w" 'evil-window-map
             "M-<backspace>" 'delete-indentation
             "<C-kp-subtract>" 'text-scale-decrease
             "<C-kp-add>" 'text-scale-increase
             "C-w" 'evil-window-map)

(evil-add-hjkl-bindings diff-mode-map 'emacs
  "K" 'diff-hunk-kill
  "C-x C-k" 'diff-file-kill
  "h" 'describe-mode
  "C-d" 'evil-scroll-down
  "C-u" 'evil-scroll-up
  "C-f" 'evil-scroll-page-down
  "C-b" 'evil-scroll-page-up
  "u" 'diff-undo
  "/" 'evil-search-forward
  "?" 'evil-search-backward
  "q" (lambda () (interactive) (kill-buffer)))

(evil-add-hjkl-bindings package-menu-mode-map 'emacs
  "h" 'package-menu-quick-help)

(after-load 'workgroups
  (fill-keymap 'global "C-x w" wg-map))

(after-load 'vc-annotate
  (fill-keymap vc-annotate-mode-map
               "?" 'evil-search-backward
               "/" 'evil-search-forward
               "C-d" 'evil-scroll-down
               "C-u" 'evil-scroll-up
               "C-f" 'evil-scroll-page-down
               "C-b" 'evil-scroll-page-up
               "j" 'next-line
               "J" 'vc-annotate-revision-at-line
               "k" 'previous-line))

(after-load 'org-agenda
  (fill-keymap org-agenda-mode-map
               "j" 'org-agenda-next-line
               "k" 'org-agenda-previous-line
                                        ;"J" 'org-agenda-goto-date
               "K" 'org-agenda-capture))

(after-load 'macrostep
  (evil-define-key 'normal macrostep-keymap
    (kbd "RET") 'macrostep-expand
    "e" 'macrostep-expand

    "u" 'macrostep-collapse
    "c" 'macrostep-collapse

    (kbd "TAB") 'macrostep-next-macro
    "n" 'macrostep-next-macro
    (kbd "S-TAB") 'macrostep-prev-macro
    "p" 'macrostep-prev-macro
    "q" 'macrostep-collapse-all
    (kbd "C-c C-c") 'macrostep-collapse-all))

(after-load 'undo-tree
  (fill-keymap undo-tree-visualizer-mode-map
               "l" 'undo-tree-visualize-switch-branch-right
               "h" 'undo-tree-visualize-switch-branch-left
               "H" 'describe-mode
               "k" 'undo-tree-visualize-undo
               "j" 'undo-tree-visualize-redo))

(after-load 'git-gutter
  (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk))

(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(evil-define-key 'normal compilation-minor-mode-map "RET" 'compile-goto-error)
(evil-define-key 'normal compilation-minor-mode-map "q" 'quit-window)

(defalias 'rfb 'rename-file-and-buffer)
(defalias 'mbf 'move-buffer-file)
(defalias 'rb 'revert-buffer)
(defalias 'oaf 'open-all-files-with-extension)

(defalias 'pl 'paradox-list-packages)

(defalias 'gl 'gist-list)
(defalias 'grb 'gist-region-or-buffer)

(key-chord-define-global "qr" 'query-replace-regexp)
(key-chord-define-global "qm" 'moccur)

(provide 'init-keys)
