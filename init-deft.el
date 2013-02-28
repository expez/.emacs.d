(setq deft-extension "org"
      deft-directory "~/org/deft/"
      deft-text-mode 'org-mode)

(unless (file-exists-p deft-directory)
  (make-directory deft-directory :create-parents))

(define-minor-mode deft-note-mode "Deft notes" nil " Deft-Notes"
  (org-mode))
(setq deft-text-mode 'deft-note-mode)

(defun kill-all-deft-notes ()
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (not (eq nil deft-note-mode))
          (setq count (1+ count))
          (kill-buffer buffer))))))

(defun toggle-deft-mode () (interactive)
  (if (or
       (eq major-mode 'deft-mode)
       (not (eq nil deft-note-mode)))
      (progn (kill-all-deft-notes) (kill-buffer "*Deft*"))
    (deft)))

(provide 'init-deft)
