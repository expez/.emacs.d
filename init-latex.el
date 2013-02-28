(require 'tex)
(require 'auto-complete-latex)

(setq TeX-autosave t
      TeX-save-query nil
      TeX-parse-self t
      TeX-PDF-mode t
      reftex-plug-into-AUCTeX t
      TeX-view-program-selection '((output-pdf "Evince"))
      TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o"))
      TeX-source-correlate-start-server t
      TeX-newline-function #'reindent-then-newline-and-indent
      refTeX-plug-into-AUCTeX t)
(setq-default text-master 'dwim)

(defun my-LaTeX-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (turn-on-reftex)
  (auto-complete-mode 1)
  (setq-default TeX-command-default "Build")
  (TeX-source-correlate-mode 1)
  (orgtbl-mode))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

(dolist (cmd '("Print" "Queue" "Index" "File"))
  (setq TeX-command-list (remove* cmd TeX-command-list :test 'equal :key 'car)))

(provide 'init-latex)
