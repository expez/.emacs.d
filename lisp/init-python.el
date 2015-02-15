(require-package 'ein)
(require-package 'elpy)
(require 'elpy)

(setq elpy-rpc-backend "jedi"
      elpy-modules '(elpy-module-company
                     elpy-module-eldoc
                     elpy-module-pyvenv))

(defun my-ein-notebook-mode-hook ()
  (whitespace-mode 0)
  (define-key evil-insert-state-local-map
    (kbd "RET") 'newline-and-indent)
  (define-key ein:notebook-mode-map (kbd "C-c C-d")
    'ein:pytools-request-tooltip-or-help))
(add-hook 'ein:notebook-mode-hook #'my-ein-notebook-mode-hook)

(elpy-enable)

(defun absolute-dirname (path)
  "Return the directory name portion of a path.

If PATH is local, return it unaltered.
If PATH is remote, return the remote diretory portion of the path."
  (if (tramp-tramp-file-p path)
      (elt (tramp-dissect-file-name path) 3)
    path))

(defun run-virtualenv-python (&optional env)
  "Run Python in this virtualenv."
  (interactive)
  (let* ((env-root (locate-dominating-file
                    (or env default-directory) "bin/python"))
         (python-executable (if env-root
                                (concat (absolute-dirname env-root) "bin/python")
                              (concat (virtualenv-current) "/bin/python"))))
    (switch-to-buffer-other-window (run-python python-executable))))

(defun python-generate-repl-name (&optional buffer)
  "Generate a better name for a Python buffer."
  (let ((buffer (or buffer (window-buffer))))
    (with-current-buffer buffer
      (concat
       "*Python-"
       (file-name-nondirectory
        (substring default-directory 0
                   (when (equal (substring default-directory -1) "/") -1)))
       "@"
       (car (split-string (if (tramp-tramp-file-p default-directory)
                              (with-parsed-tramp-file-name default-directory py
                                py-host)
                            (system-name)) "\\."))
       "*"))))

(add-hook 'inferior-python-mode-hook #'my-inferior-python-mode-hook)

(defun my-inferior-python-mode-hook ()
  (rename-buffer (python-generate-repl-name)))

(provide 'init-python)
