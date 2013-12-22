(require-package 'undo-tree)
(setq evil-search-module 'evil-search)

(require-package 'surround)
(require-package 'evil-visualstar)
(require-package 'evil-numbers)
(require-package 'evil)
(require-package 'ace-jump-mode)
(require-package 'goto-chg)

(evil-mode 1)

(global-surround-mode 1)

(setq-default surround-pairs-alist
              '((?\( . ("(" . ")"))
                (?\[ . ("[" . "]"))
                (?\{ . ("{" . "}"))

                (?\) . ("( " . " )"))
                (?\] . ("[ " . " ]"))
                (?\} . ("{ " . " }"))

                (?# . ("#{" . "}"))
                (?b . ("(" . ")"))
                (?B . ("{" . "}"))
                (?> . ("<" . ">"))
                (?t . surround-read-tag)
                (?< . surround-read-tag)
                (?f . surround-function)))

(setq evil-insert-state-cursor '("red" hbar)
      evil-normal-state-cursor '("white" box)
      evil-visual-state-cursor '("green" box)
      evil-default-cursor t
      evil-want-visual-char-semi-exclusive t
      evil-move-cursor-back nil
      evil-want-C-u-scroll t
      evil-ex-hl-update-delay 0.01)

(evil-ex-define-cmd "n[ew]" 'evil-window-new)

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
          deft-mode
          occur-mode
          term-mode
          eshell
          magit-branch-manager-mode
          magit-commit-mode
          magit-log-mode
          git-rebase-mode
          log-view-mode
          undo-tree-mode
          project-explorer-mode
          diff-mode))

(evil-set-initial-state 'man-mode 'motion)

(defadvice ido-hacks-execute-extended-command (before exit-insert-state
                                                      activate)
  (when (eql evil-state 'insert)
    (evil-normal-state)))

(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(defadvice evil-goto-mark (after center-mark activate)
  (evil-scroll-line-to-center nil))

(provide 'init-evil)
