(require-package 'python-mode)
(require-package 'company-jedi)
(require-package 'autopair)

(when (featurep 'python) (unload-feature 'python t))
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (pymacs-load "ropemacs" "rope-")

(add-to-list 'company-backends 'company-jedi)
(setq company-backends (delq 'company-ropemacs company-backends))

(setq ropemacs-enable-autoimport t
      py-set-fill-column-p t
      py-electric-colon-active t
      company-jedi-python-bin "python")

(defun my-python-mode-hook ()
  (auto-complete-mode 0)
  (py-autopair-mode-on)
  (company-jedi-start)
  (company-mode-on)
  (setq-local electric-pair-mode nil)
  (define-key evil-normal-state-local-map (kbd "M-.") 'company-jedi-goto-definition))

(add-hook 'python-mode-hook #'my-python-mode-hook)
(provide 'init-python)
