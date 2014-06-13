;;; WIP, not yet usable
(require 'company)
(require 'pos-tip)

(defun company-quick-help--frontend (command)
  "`company-mode' front-end showing documentation in a
  `pos-tip' popup."
  (pcase command
    (`post-command (company-quickhelp-set-timer))
    (`hide (pos-tip-hide))))

(defun company-quickhelp-show ()
  (with-no-warnings
    (pos-tip-show (company-fetch-metadata)
                  nil
                  nil
                  nil
                  300
                  80
                  nil (+ 200
                         (overlay-get company-pseudo-tooltip-overlay 'company-width))
                  nil)))

(defadvice company-cancel (after remove-quickhelp-timer activate)
  (company-quickhelp-cancel-timer))

(defvar company-quickhelp-timer nil
  "Quick help idle timer.")

(defvar company-quickhelp-delay 0.5
  "Delay, in seconds, before the quickhelp popup appears.")

(defun company-quickhelp-set-timer ()
  (when (null company-quickhelp-timer)
    (setq company-quickhelp-timer (run-with-idle-timer company-quickhelp-delay nil 'company-quickhelp-show))))

(defun company-quickhelp-cancel-timer ()
  (when (timerp company-quick-help-timer)
    (cancel-timer company-quick-help-timer)
    (setq company-quick-help-timer nil)))

(setq company-frontends (cons 'company-quick-help-frontend company-frontends))

(provide 'company-quickhelp)
