(setq prolog-system 'swi
      auto-mode-alist (append '(
                                ;("\\.pl$" . prolog-mode) ; fuck perl?
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

(defun specify-prolog-mode ()
  "Adds a file local variable indicating that this file should be
treated as a prolog file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "% -*- mode: prolog -*-")
    (newline-and-indent)))

(provide 'init-prolog)
