(require 'evil-leader)
(require 'evil-numbers)
(require 'evil-paredit)

(evil-mode 1)

(global-surround-mode 1)

(setq evil-insert-state-cursor '("red" hbar)
      evil-normal-state-cursor '("white" box)
      evil-visual-state-cursor '("green" box)
      evil-default-cursor t
      evil-want-visual-char-semi-exclusive t
      evil-move-cursor-back nil
      evil-want-C-u-scroll t
      evil-leader/in-all-states t
      evil-ex-hl-update-delay 0.01)

(evil-leader/set-leader ",")

(evil-ex-define-cmd "n[ew]" 'evil-window-new)

(defmacro evil-enclose-ace-jump (&rest body)
  `(let ((old-mark (mark)))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (progn
           ,@body
           (recursive-edit))
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i))
   (ace-jump-mode 5)))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (evil-enclose-ace-jump
   (ace-jump-mode 9)))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 1)))

(evil-define-motion evil-ace-jump-char-to-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (setq ace-jump-mode-move-keys (loop for i from ?a to ?z collect i))
   (ace-jump-mode 5)
   (forward-char -1)))

(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

(defadvice evil-goto-definition (around evil-clever-goto-def activate)
  "Make use of emacs', slime's and etags possibilities for finding definitions."
  (case major-mode
    (lisp-mode (if slime-mode
                   (or (slime-find-definitions (symbol-name (symbol-at-point)))
                       ad-do-it)
                 ad-do-it))
    (emacs-lisp-mode (condition-case nil
                         (find-function (symbol-at-point))
                       (error (condition-case nil
                                  (find-variable (symbol-at-point))
                                (error ad-do-it)))))
    (otherwise
     (let ((tag (symbol-name (symbol-at-point))))
       (if (and (boundp 'gtags-mode) gtags-mode)
           (gtags-goto-tag tag nil)
         (if (and tags-file-name (find-tag-noselect tag))
             (find-tag tag)
           ad-do-it))))))

(defun set-mode-to-default-emacs (mode)
  (evil-set-initial-state mode 'emacs))

(mapcar 'set-mode-to-default-emacs
        '(dired
          shell-mode
          inferior-emacs-lisp-mode
          term-mode
          eshell-mode
          slime-repl-mode
          occur-mode
          inf-ruby-mode
          magit-branch-manager-mode
          magit-commit-mode
          magit-log-mode
          log-view-mode))

(provide 'init-evil)
