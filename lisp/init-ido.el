(require-package 'ido-vertical-mode)
(require-package 'ido-ubiquitous)
(require-package 'smex)
(require-package 'flx-ido)

(ido-mode 1)
(ido-mode 'both)
(ido-vertical-mode)
(ido-ubiquitous-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(smex-initialize)

(setq ido-enable-flex-matching t
      ido-use-faces nil ; disable ido faces to see flx highlights.
      ido-create-new-buffer 'always)

(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "~")
              (lambda ()
                (interactive)
                (if (looking-back "/")
                    (insert "~/")
                  (call-interactively 'self-insert-command))))))

(provide 'init-ido)
