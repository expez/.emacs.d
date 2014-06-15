;;; WIP, not yet usable
(require 'company)
(require 'pos-tip)

(defun company-quickhelp-frontend (command)
  "`company-mode' front-end showing documentation in a
  `pos-tip' popup."
  (pcase command
    (`post-command (company-quickhelp-set-timer))
    (`hide (pos-tip-hide))))

(defun company-quickhelp-show ()
  (let ((selected (nth company-selection company-candidates)))
    (with-no-warnings
      (pos-tip-show (company-call-backend 'doc selected)
                    nil
                    nil
                    nil
                    300
                    80
                    nil
                    (* (frame-char-width)
                       (overlay-get company-pseudo-tooltip-overlay
                                    'company-width))
                    nil)))
  (company-quickhelp-cancel-timer))

(defvar company-quickhelp-timer nil
  "Quickhelp idle timer.")

(defvar company-quickhelp-delay 0.5
  "Delay, in seconds, before the quickhelp popup appears.")

(defadvice company-cancel (after remove-quickhelp-timer activate)
 (company-quickhelp-cancel-timer))

(defun company-quickhelp-set-timer ()
  (when (null company-quickhelp-timer)
    (setq company-quickhelp-timer
          (run-with-idle-timer company-quickhelp-delay nil
                               'company-quickhelp-show))))

(defun company-quickhelp-cancel-timer ()
  (when (timerp company-quickhelp-timer)
    (cancel-timer company-quickhelp-timer)
    (setq company-quickhelp-timer nil)))

(setq company-frontends (cons 'company-quickhelp-frontend company-frontends))

(provide 'company-quickhelp)
