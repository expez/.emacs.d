(fill-keymap 'global
             "C-x m" 'ido-hacks-execute-extended-command
             "C-x C-m" 'ido-hacks-execute-extended-command
             "C-x c" 'compile
             "C-x C-c" 'compile
             "C-x s" 'shell
             "C-x C-b" 'ibuffer
             "C-x r v" 'register-list
             "C-<tab>" 'hippie-expand
             ;; easy spell check
             "<f8>" 'ispell-word
              "<end>" 'sr-speedbar-toggle
             ;;"C-S-<f8>" 'fd-switch-dictionary
             "M-S-<f8>" 'flyspell-buffer
             "C-<f8>" 'flyspell-check-previous-highlighted-word
             "M-<f8>" 'flyspell-check-next-highlighted-word
             "C-x r q" 'save-buffers-kill-terminal
             "C-x v t" 'vc-create-tag
             "C-x a r" 'align-regexp
             "C-x i" 'ido-goto-symbol
             "C-x C-r" 'ido-recentf-open
             "M-<backspace>" 'delete-indentation)

(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

(defalias 'rfb 'rename-file-and-buffer)
(defalias 'mbf 'move-buffer-file)
(defalias 'rb 'revert-buffer)
(defalias 'oaf 'open-all-files-with-extension)
(defalias 'dbf 'diff-buffer-with-file)

(defalias 'gl 'gist-list)
(defalias 'grb 'gist-region-or-buffer)

(key-chord-define-global "qr" 'query-replace-regexp)
(key-chord-define-global "qm" 'moccur)

(provide 'init-keys)
