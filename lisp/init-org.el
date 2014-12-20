(require-package 'deft)
(require 'deft)
(require 'org)

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-log-done t
      org-agenda-files (list "~/org/todo.org")
      org-agenda-skip-unavailable-files t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-start-on-weekday nil
      org-indirect-buffer-display 'current-window
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'other-window
      org-agenda-show-all-dates t
      org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")))
(add-to-list 'org-structure-template-alist
             '("n" "#+BEGIN_COMMENT\n?\n#+END_COMMENT"
               "<comment>\n?\n</comment>"))

(defun new-todo ()
  (interactive) (org-capture nil "t"))

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
      (unless (or (not (boundp 'deft-note-mode))
                  (null deft-note-mode))
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