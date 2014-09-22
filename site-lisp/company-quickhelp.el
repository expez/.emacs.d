;;; WIP, not yet usable
(require 'company)
(require 'pos-tip)

(defun company-quickhelp-frontend (command)
  "`company-mode' front-end showing documentation in a
  `pos-tip' popup."
  (pcase command
    (`post-command (company-quickhelp--set-timer))
    (`hide (pos-tip-hide))))

(defun company-quickhelp--show ()
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available")))
         (doc (with-current-buffer doc-buffer
                (buffer-substring (point-min) (point-max)))))
    (with-no-warnings
      (pos-tip-show doc
                    nil
                    company-point
                    nil
                    300
                    80
                    nil
                    (- (* (frame-char-width)
                          (overlay-get company-pseudo-tooltip-overlay
                                       'company-width))
                       (* (frame-char-width)
                          5))
                    nil)))
  (company-quickhelp--cancel-timer))

(defvar company-quickhelp--timer nil
  "Quickhelp idle timer.")

(defcustom company-quickhelp--delay 0.5
  "Delay, in seconds, before the quickhelp popup appears.")

(defadvice company-cancel (after remove-quickhelp-timer activate)
  (company-quickhelp--cancel-timer))

(defun company-quickhelp--set-timer ()
  (when (null company-quickhelp--timer)
    (setq company-quickhelp--timer
          (run-with-idle-timer company-quickhelp--delay nil
                               'company-quickhelp--show))))

(defun company-quickhelp--cancel-timer ()
  (when (timerp company-quickhelp--timer)
    (cancel-timer company-quickhelp--timer)
    (setq company-quickhelp--timer nil)))

;;;###autoload
(define-minor-mode company-quickhelp-mode nil nil nil nil
  "Provides documentation popups for `company-mode'."
  (if company-quickhelp-mode
      (progn (setq company-frontends
                   (delq 'company-quickhelp-frontend company-frontends))
             (company-quickhelp--cancel-timer))
    (setq company-frontends
          (cons 'company-quickhelp-frontend company-frontends))))

(provide 'company-quickhelp)
