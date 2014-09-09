(require-package 'undo-tree)
(require-package 'evil-surround)
(require 'evil-surround)
(require-package 'evil-visualstar)
(require 'evil-visualstar)
(require-package 'evil-paredit)
(require-package 'evil-numbers)
(require-package 'evil-jumpers)
(require 'evil-jumper)
(require-package 'evil)
(require-package 'ace-jump-mode)
(require 'ace-jump-mode)
(require-package 'goto-chg)
(require 'evil-numbers)
(setq evil-symbol-word-search t)
(evil-mode 1)

(global-evil-surround-mode 1)

(setq-default evil-surround-pairs-alist
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
                (?t . evil-surround-read-tag)
                (?< . evil-surround-read-tag)
                (?f . evil-surround-function)))

(add-hook 'evil-local-mode-hook (lambda ()
                                  (setq-local interprogram-cut-function nil)
                                  (setq-local interprogram-paste-function nil)))

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
          cider-stacktrace-mode
          cider-test-report-mode
          cider-doc-mode
          deft-mode
          occur-mode
          term-mode
          eshell
          magit-branch-manager-mode
          magit-commit-mode
          magit-log-mode
          git-rebase-mode
          log-view-mode
          cider-popup-buffer-mode
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

(defun comint-goto-end-and-insert ()
  (interactive)
  (if (not (comint-after-pmark-p))
      (progn (comint-goto-process-mark)
             (evil-append-line nil))
    (evil-insert 1)))

(evil-define-key 'normal comint-mode-map "i" 'comint-goto-end-and-insert)
(after-load 'inf-ruby-mode
  (evil-define-key 'normal inf-ruby-mode-map "i" 'comint-goto-end-and-insert))

(evil-define-key 'insert comint-mode-map
  (kbd "<up>") 'comint-previous-input
  (kbd "<down>") 'comint-next-input)

(provide 'init-evil)
