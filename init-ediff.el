(setq ediff-window-setup-function 'ediff-setup-windows-plain ;;Don't want the control frame.
      diff-switches "-u" ;;Use unified format
      ediff-custom-diff-options "-U3" ;;3 lines of context.
      ediff-combination-pattern
      '("<<<<<<< A: HEAD" A "||||||| Ancestor" Ancestor "=======" B ">>>>>>> B: Incoming")
      ediff-show-clashes-only 't
      ediff-highlight-all-diffs nil)

;;Save window configuration prior to ediff, so we can jump to it from ediff if needed,
;;restore the previous window configuration when ediff terminates.
;;Taken from emacswiki/ediffmode
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
                  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
                  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)
(add-hook 'ediff-cleanup-hook (lambda () (ediff-janitor nil nil)))
(provide 'init-ediff)
