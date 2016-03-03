(require-package 'undo-tree)
(require-package 'evil-surround)
(require 'evil-surround)
(require-package 'evil-visualstar)
(require-package 'evil-numbers)
(require-package 'evil)
(require-package 'ace-jump-mode)
(require 'ace-jump-mode)
(require-package 'goto-chg)
(require 'evil-numbers)
(require-package 'evil-exchange)
(require-package 'evil-iedit-state)
(require-package 'evil-indent-textobject)
(require 'evil-indent-textobject)
(require-package 'evil-args)
(require 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

(after-load 'evil
  (evil-define-operator evil-yank-to-eol (beg end type register yank-handler)
    "Yank to end of line."
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (evil-yank beg end type register yank-handler))
  (define-key evil-normal-state-map "Y" #'evil-yank-to-eol)
  (define-key evil-motion-state-map "Y" #'evil-yank-to-eol)

  ;; Make "y" also work in motion state.
  (define-key evil-motion-state-map "y" #'evil-yank)

  (defun my-insert-line-below (count)
    (interactive "p")
    (dotimes (_ count)
      (evil-insert-newline-below)))
  (define-key evil-normal-state-map "go" #'my-insert-line-below)

  (defun my-insert-line-above (count)
    (interactive "p")
    (dotimes (_ count)
      (evil-insert-newline-above)))
  (define-key evil-normal-state-map "gO" #'my-insert-line-above))

(progn
  (evil-define-state iedit
    "`iedit state' interfacing iedit mode."
    :tag " <i> "
    :enable (normal)
    :cursor box
    :message "-- IEDIT --"
    ;; force iedit mode
    (if (evil-replace-state-p) (call-interactively 'iedit-mode)))
  (require 'evil-iedit-state))

(evil-mode 1)
(evil-exchange-install)

(global-evil-visualstar-mode)

(global-evil-surround-mode 1)
(setq evil-exchange-highlight-face 'region)

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
          cider-classpath-mode
          cider-doc-mode
          cider-docview-mode
          cider-inspector-mode
          cider-popup-buffer-mode
          cider-stacktrace-mode
          cider-test-report-mode
          cljr--change-signature-mode
          deft-mode
          occur-mode
          term-mode
          eshell
          magit-branch-manager-mode
          magit-commit-mode
          magit-log-mode
          magit-popup-mode
          magit-popup-sequence-mode
          git-rebase-mode
          log-view-mode
          project-explorer-mode
          paradox-menu-mode
          neotree-mode
          diff-mode))

(evil-set-initial-state 'man-mode 'motion)

;;; https://bitbucket.org/lyro/evil/issue/432/edebug-mode-map-cant-take-effect-for-the
(add-hook 'edebug-mode-hook 'evil-normalize-keymaps)

(defadvice ido-hacks-execute-extended-command (before exit-insert-state activate)
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
