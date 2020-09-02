(require-package 'restclient)
(require-package 'company-restclient)
(require 'restclient)

(add-to-list 'company-backends 'company-restclient)

(add-auto-mode 'restclient-mode "\\.restclient")

(defun my-restclient-response-loaded-hook ()
  (flycheck-mode 0)
  (when evil-normal-state-local-map
    (define-key evil-normal-state-local-map (kbd "q")
      (lambda () (interactive) (kill-buffer)))))

(defun my-restclient-mode-hook ()
  (fill-keymap evil-normal-state-local-map
               "M-n" 'restclient-jump-next
               "M-p" 'restclient-jump-prev)
  (flycheck-mode 0))

(add-hook 'restclient-response-loaded-hook #'my-restclient-response-loaded-hook)
(add-hook 'restclient-mode-hook #'my-restclient-mode-hook)

(provide 'init-restclient)
