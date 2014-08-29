(setq prolog-system 'swi
      auto-mode-alist (append '(
                                        ;("\\.pl$" . prolog-mode) ; fuck perl?
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

(defun my-prolog-mode-hook ()
  (paredit-nonlisp)
  (evil-paredit-mode 1)
  (fill-keymap prolog-mode-map
               "C-c C-e" 'prolog-eval-line-or-region
               "C-c C-r" 'prolog-consult-line-or-region))

(defun prolog-consult-line-or-region ()
  "Consult region between BEG and END."
  (interactive)
  (save-window-excursion
    (if (region-active-p)
        (prolog-consult-region (region-beginning) (region-end))
      (prolog-consult-region (point-at-bol) (point-at-eol)))))

(defun prolog-eval-line-or-region ()
  "Sends the current region or line to the prolog buffer and
evals it."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (let ((code (buffer-substring (region-beginning) (region-end))))
          (prolog--insert-and-eval code))
      (prolog--insert-and-eval
       (buffer-substring (point-at-bol) (point-at-eol))))))

(defun prolog--insert-and-eval (code)
  (save-window-excursion
    (pop-to-buffer "*prolog*")
    (goto-char (point-max))
    (insert code)
    (goto-char (point-max))
    (comint-send-input)
    ;; TODO get this fucker to actually scroll so the new output is visible
    (with-selected-window (get-buffer-window "*prolog*")
      (goto-char (point-max)))))

(add-hook 'prolog-mode-hook #'my-prolog-mode-hook)
(add-hook 'prolog-inferior-mode #'my-prolog-mode-hook)

(defun specify-prolog-mode ()
  "Adds a file local variable indicating that this file should be
treated as a prolog file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "% -*- mode: prolog -*-")
    (newline-and-indent)))

(provide 'init-prolog)
