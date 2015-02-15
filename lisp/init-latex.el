(require-package 'auctex)
(require-package 'company-math)

(defun my-LaTeX-mode-hook ()
  (turn-on-orgtbl)
  (visual-line-mode 1)
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (turn-on-reftex)
  (setq fill-column 120)
  (setq-default TeX-command-default "Build"
                text-master 'dwim)
  (TeX-source-correlate-mode 1)
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
  (setq-local company-backends
              (append '(company-math-symbols-latex company-latex-commands)
                      company-backends)))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

(after-load 'tex
  (dolist (cmd '("Print" "Queue" "Index" "File"))
    (setq TeX-command-list (remove* cmd TeX-command-list
                                    :test #'equal :key #'car))))

(defun makeglossaries()
  "Runs the perl script makeglossaries on the current latex file."
  (interactive)
  (shell-command (concat "cd " (file-name-directory (buffer-file-name)) " && "
                         "makeglossaries " (file-name-sans-extension
                                            (buffer-name)))))

(defun check-item-entry ()
  "This function is meant to be used as advice for the
`LaTeX-insert-item' function. The purpose behind this is to delete
the extra blank line that is naively added by `LaTeX-insert-item'
when not already on an item line."
  (interactive)
  (save-excursion
    ;; Backward one line, check if it happened if the line we're
    ;; looking is empty, delete it
    (if (and (= (forward-line -1) 0)
             (looking-at "^\\s-*$"))
        (kill-line))))

(defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
  "This advice is meant to fix the issue where an extra blank
line is naively added by `LaTeX-insert-item' when not already on
an item line."
  (check-item-entry))

(provide 'init-latex)
