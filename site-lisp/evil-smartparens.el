;;; evil-smartparens.el --- Evil support for smartparens

;; Copyright (C) 2015, Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/evil-smartparens
;; Keywords: evil smartparens
;; Version: 0.1
;; Package-Requires: ((evil "1.0") (cl-lib "0.3") (emacs "24.1") (diminish "0.44") (smartparens "1.6.3)

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Evil support for smartparens

;; Provide `evil-smartparens-mode' which enables evil support for smartparens.

;;; Code:

(require 'evil)
(require 'smartparens)
(require 'diminish)

(defcustom evil-smartparens-lighter " SP/e"
  "The lighter used for evil-smartparens without strict mode."
  :group 'evil-smartparens
  :type 'string)

(defcustom evil-smartparens-strict-lighter " SP/se"
  "The lighter used for evil-smartparens and strict mode."
  :group 'evil-smartparens
  :type 'string)

(defcustom evil-smartparens-threshold 2500
  "If the region being operated on is larger than this we cop out.

Quite a bit of work gets done to ensure the region being worked
is in an safe state, so this lets us sarifice safety for a snappy
editor on slower computers.

Even on a large computer you shouldn't set this too high or your
computer will freeze when copying large files out of Emacs."
  :group 'evil-smartparens
  :type 'string)

(defvar evil-sp--override nil)

(defun evil-sp--override ()
  (prog1 (or (not evil-smartparens-mode)
             (not smartparens-strict-mode)
             evil-sp--override
             (evil-sp--region-too-expensive-to-check))
    (setq evil-sp--override nil)))

;;; Evil-smartparens works by adding advice to regular functions.
;;; Unfortunately, the advice functionality is a global deal so in
;;; all the functions we advice we have to check if
;;; `evil-smartparens-mode' is active and if not just pass through to
;;; the old function.

(defun evil-sp--activate-advice ()
  "`evil-smartparens' is fully implemented in terms of advice to `evil'."
  (advice-add 'evil-delete :around #'evil-sp--modify-region)
  (advice-add 'evil-replace :around #'evil-sp--modify-region)
  (advice-add 'evil-yank :around #'evil-sp--modify-region)
  (advice-add 'evil-delete-line :around #'evil-sp--emulate-sp-kill-sexp)
  (advice-add 'evil-change-line :around #'evil-sp--emulate-sp-kill-sexp)
  (advice-add 'evil-delete-backward-char :around
              #'evil-sp--override-delete-backward-char)
  (advice-add 'evil-delete-backward-char-and-join :around
              #'evil-sp--override-delete-backward-char-and-join))

(defun evil-sp--deactivate-advice ()
  "Stop advising `evil' functions."
  (advice-remove 'evil-delete #'evil-sp--modify-region)
  (advice-remove 'evil-replace #'evil-sp--modify-region)
  (advice-remove 'evil-yank #'evil-sp--modify-region)
  (advice-remove 'evil-delete-line #'evil-sp--emulate-sp-kill-sexp)
  (advice-remove 'evil-change-line #'evil-sp--emulate-sp-kill-sexp)
  (advice-remove 'evil-delete-backward-char
                 #'evil-sp--override-delete-backward-char)
  (advice-remove 'evil-delete-backward-char
                 #'evil-sp--override-delete-backward-char-and-join))

(defun evil-sp--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun evil-sp--get-endpoint-for-killing ()
  "Return the endpoint from POINT upto which `sp-kill-sexp'would kill."
  (if (= (evil-sp--depth-at (point))
         (evil-sp--depth-at (point-at-eol)))
      ;; Act like kill line
      (point-at-eol)
    (max
     ;; Greedy killing
     (1- (evil-sp--point-after 'sp-up-sexp))
     (evil-sp--point-after 'sp-forward-sexp))))

(defun evil-sp--region-too-expensive-to-check ()
  "When it takes prohobitively long to check region we cop out."
  (when (region-active-p)
    (> (abs (- (region-beginning) (region-end)))
       evil-smartparens-threshold)))

(defun evil-sp-override ()
  (interactive)
  (setq evil-sp--override t))

(defun evil-sp--last-command-was-kill-p (type)
  (and type (listp type)))

(defun evil-sp--modify-region (oldfun beg end type &rest rest)
  "Wrapper around OLDFUN which shrinks or enlarges region until it's balanced."
  (if (and (evil-sp--override)
           (not (evil-sp--last-command-was-kill-p type)))
      (apply oldfun beg end type rest)
    (cl-letf (((symbol-function 'sp-message) (lambda (msg))))
      ;; hack for communicating through the advice that we're killing
      (if (evil-sp--last-command-was-kill-p type)
          ;; oldfun is evil-delete-line here, we cannot use that
          ;; because it doesn't use its END argument in all cases.
          (apply #'evil-delete beg
                 (evil-sp--get-endpoint-for-killing)
                 (second type) rest)
        (condition-case nil
            (apply oldfun (evil-sp--new-beginning beg end)
                   (evil-sp--new-ending beg end) type rest)
          ;; HACK: We might be deleting backwards and shrinking the
          ;; endpoint might never get us where we want.
          ('error (apply oldfun (evil-sp--new-beginning beg end :shrink)
                         end type rest)))))))

(defun evil-sp--no-sexp-between-point-and-eol? ()
  "Check if the region up to eol contains any opening or closing delimiters."
  (not (or (save-excursion
             (re-search-forward (sp--get-opening-regexp) (point-at-eol)
                                :noerror))
           (save-excursion
             (re-search-forward (sp--get-closing-regexp) (point-at-eol)
                                :noerror)))))

(defun evil-sp--emulate-sp-kill-sexp (oldfun beg end type &rest rest)
  "Enlarge the region bounded by BEG END until it matches `sp-kill-sexp' at BEG."
  (if evil-sp--override
      (apply oldfun beg end type rest)
    (if (evil-sp--no-sexp-between-point-and-eol?)
        (if (looking-at "\n")
            (evil-join (point) (1+ (point)))
          (apply oldfun beg end type rest))
      ;; We can't enlarge region here because `evil-delete-line' calls
      ;; `evil-delete' itself, overriding our work
      (apply oldfun (point) end (if (symbolp type) (list :kill-sexp type) type)
             rest))))

(defun evil-sp--override-delete-backward-char (oldfun beg end &rest rest)
  "This is done to ensure empty sexps are deleted."
  (if (and evil-smartparens-mode
           (save-excursion (forward-char) (sp-point-in-empty-sexp)))
      (apply #'evil-delete beg (incf end) rest)
    (apply oldfun beg end rest)))

(defun evil-sp--override-delete-backward-char-and-join (oldfun count)
  "This is done to ensure empty sexps are deleted."
  (when evil-smartparens-mode
    (if (evil-sp--override)
        (funcall oldfun count)
      (sp-backward-delete-char count))))

(defun evil-sp--lighter ()
  "Create the lighter for `evil-smartparens'.

We want a different lighter for `smartparens-mode' and
`smartparens-strict-mode'."
  (if smartparens-strict-mode
      evil-smartparens-strict-lighter
    evil-smartparens-lighter))

(defun evil-sp--disable ()
  "Deactive advice and restore modeline."
  (evil-sp--deactivate-advice)
  (diminish-undo 'smartparens-mode)
  (remove-hook #' smartparens-disabled-hook #'evil-sp--disable))

(defun evil-sp--enable ()
  "Activate advice and update modeline."
  (evil-sp--activate-advice)
  (diminish 'smartparens-mode)
  (when evil-visual-state-local-map
    (define-key evil-visual-state-local-map "o" 'evil-sp-override))
  (add-hook #' smartparens-disabled-hook #'evil-sp--disable))

;;;###autoload
(define-minor-mode evil-smartparens-mode
  "Toggle evil-smartparens."
  :lighter (:eval (evil-sp--lighter))
  :init-value nil
  (if evil-smartparens-mode
      (evil-sp--enable)
    (evil-sp--disable)))

(defun evil-sp--depth-at (&optional point)
  "Return the depth at POINT.

Strings affect depth."
  (push major-mode sp-navigate-consider-stringlike-sexp)
  (let ((depth 0))
    (save-excursion
      (when point
        (goto-char point))
      (unwind-protect
          (while (and (not (sp-point-in-comment))
                      (sp-backward-up-sexp))
            (incf depth))
        (pop sp-navigate-consider-stringlike-sexp)))
    depth))

(defun evil-sp--new-ending (beg end)
  "Find the largest safe region delimited by BEG END."
  (let ((region (s-trim (buffer-substring-no-properties beg end))))
    (unless (s-blank? region)
      (cond
       ((sp-point-in-empty-sexp)
        ;; expand region if we're in an empty sexp
        (setf end (save-excursion (sp-up-sexp) (point))))

       ;; reduce region if it's unbalanced due to selecting too much
       (t (while (not (or (sp-region-ok-p beg end)
                          (= beg end)))
            (cl-decf end))))))
  (when (= beg end)
    (evil-sp--fail))
  end)

(defun evil-sp--new-beginning (beg end &optional shrink)
  "Return a new value for BEG if POINT is inside an empty sexp.

If SHRINK is t we try to shrink the region until it is balanced
by decrementing BEG."
  (if (not shrink)
      (min beg
           (if (sp-point-in-empty-sexp)
               (evil-sp--point-after 'sp-backward-up-sexp)
             (point-max)))

    (let ((region (s-trim (buffer-substring-no-properties beg end))))
      (unless (s-blank? region)
        (cond
         ((sp-point-in-empty-sexp)
          ;; expand region if we're in an empty sexp
          (setf end (save-excursion (sp-backward-up-sexp) (point))))

         ;; reduce region if it's unbalanced due to selecting too much
         (t (while (not (or (sp-region-ok-p beg end)
                            (= beg end)))
              (cl-incf beg)))))
      (when (= beg end)
        (evil-sp--fail)))
    beg))

(defun evil-sp--fail ()
  "Error out with a friendly message."
  (error "Can't find a safe region to act on!"))

(provide 'evil-smartparens)
;;; evil-smartparens.el ends here
