(require 'deft)

(define-derived-mode deft-note-mode org-mode "Deft note"
  (set (make-local-variable 'deft-note-mode) t))

(setq deft-extension "deft"
      deft-directory "~/org/deft/"
      deft-text-mode 'deft-note-mode
      deft-auto-save-interval 30.0)

(add-auto-mode 'deft-note-mode "\\.deft$")

(unless (file-exists-p deft-directory)
  (make-directory deft-directory :create-parents))

(defun kill-all-deft-notes ()
  (interactive)
  (save-excursion
    (dolist(buffer (buffer-list))
      (set-buffer buffer)
      (unless (null deft-note-mode)
        (kill-buffer buffer)))))

(defun toggle-deft-mode ()
  (interactive)
  (if (or
       (eq major-mode 'deft-mode)
       (eq major-mode 'deft-note-mode))
      (progn
        (kill-all-deft-notes)
        (kill-buffer "*Deft*"))
    (deft)))

(fill-keymap deft-mode-map
             "n" 'deft-new-file
             "N" 'deft-new-file-named
             "a" 'deft-archive-file
             "d" 'deft-delete-file
             "f" 'deft-find-file
             "g" 'deft-refresh
             "q" 'quit-window
             "r" 'deft-rename-file
             "t" 'deft-toggle-incremental-search)

(provide 'init-deft)
