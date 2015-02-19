(require-package 'restclient)
(require-package 'company-restclient)
(require 'restclient)

(add-to-list 'company-backends 'company-restclient)

(add-auto-mode 'restclient-mode "\\.restclient")

(defun my-restclient-response-loaded-hook ()
  (view-mode)
  (when evil-normal-state-local-map
    (define-key evil-normal-state-local-map (kbd "q")
      (lambda () (interactive) (view-mode-exit)))))

(defun my-restclient-mode-hook ()
  (flycheck-mode 0))

(add-hook 'restclient-response-loaded-hook #'my-restclient-response-loaded-hook)
(add-hook 'restclient-mode-hook #'my-restclient-mode-hook)

(fill-keymap restclient-mode-map
             "M-n" 'restclient-jump-next
             "M-p" 'restclient-jump-prev)

(defun override-proxy-settings-for-localhost (oldfun method url &rest args)
  (if (or (s-contains-p "localhost" url)
          (s-contains-p "127.0.0.1" url))
      (let ((url-proxy-services nil))
        (apply oldfun method url args))
    (apply oldfun method url args)))

(advice-add 'restclient-http-do :around #'override-proxy-settings-for-localhost)

(provide 'init-restclient)
