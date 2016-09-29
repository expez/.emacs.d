(defun my-python-mode-hook ()
  (setq evil-symbol-word-search t)
  (aggressive-indent-mode 0))

(add-hook 'python-mode-hook #'my-python-mode-hook)
(provide 'init-python)
