(require 'company)
(require 'pos-tip)

(defun company-quickhelp-frontend (command)
  "`company-mode' front-end showing documentation in a
  `pos-tip' popup."
  (pcase command
    (`post-command (company-quickhelp--set-timer))
    (`hide
     (company-quickhelp--cancel-timer)
     (pos-tip-hide))))

(defun company-quickhelp--show ()
  (company-quickhelp--cancel-timer)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (company-call-backend 'doc-buffer selected))
         (ovl company-pseudo-tooltip-overlay)
         (x-gtk-use-system-tooltips nil))
    (when (and ovl doc-buffer)
      (with-no-warnings
        (let* ((width (overlay-get ovl 'company-width))
               (col (overlay-get ovl 'company-column))
               (extra (- (+ width col) (company--window-width))))
          (pos-tip-show (with-current-buffer doc-buffer (buffer-string))
                        nil
                        nil
                        nil
                        300
                        80
                        nil
                        (* (frame-char-width)
                           (- width (length company-prefix)
                              (if (< 0 extra) extra 1)))))))))

(defvar company-quickhelp--timer nil
  "Quickhelp idle timer.")

(defcustom company-quickhelp--delay 0.5
  "Delay, in seconds, before the quickhelp popup appears.")

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
(define-minor-mode company-quickhelp-mode
  "Provides documentation popups for `company-mode' using `pos-tip'."
  :global t
  (if company-quickhelp-mode
      (push 'company-quickhelp-frontend company-frontends)
    (setq company-frontends
          (delq 'company-quickhelp-frontend company-frontends))
    (company-quickhelp--cancel-timer)))

(provide 'company-quickhelp)
