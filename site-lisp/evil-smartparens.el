;;; evil-smartparens.el --- Evil support for smartparens

;; Copyright (C) 2015, Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/evil-smartparens
;; Keywords: evil smartparens
;; Version: 0.1
;; Package-Requires: ((evil "1.0") (cl-lib "0.3") (emacs "24.1") (diminish "0.44"))

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

(defun evil-sp--point-after (&rest actions)
  "Returns POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun evil-sp--new-beginning (beg)
  "Return a new value for BEG if POINT is inside an empty sexp."
  (min beg
       (or (when (sp-point-in-empty-sexp)
             (evil-sp--point-after 'sp-backward-up-sexp))
           (point-max))))

(defun evil-sp--modify-region (oldfun beg end &rest rest)
  (if (evil-sp--point-on-delimiter-in-unbalanced-sexp? beg end)
      (apply oldfun beg end rest)
    (apply oldfun (evil-sp--new-beginning beg)
           (evil-sp--new-ending beg end) rest)))

(defun evil-sp--no-sexp-between-point-and-eol? ()
  (and (not (save-excursion
              (re-search-forward (sp--get-opening-regexp) (point-at-eol)
                                 :noerror)))
       (not (save-excursion
              (re-search-forward (sp--get-closing-regexp) (point-at-eol)
                                 :noerror)))))

(defun evil-sp--emulate-sp-kill-sexp (oldfun beg end type &rest rest)
  "Enlarge the region bounded by BEG END until it matches
  `sp-kill-sexp' at BEG."
  (if (evil-sp--no-sexp-between-point-and-eol?)
      (if (looking-at "\n")
          (evil-join (point) (1+ (point)))
        (apply oldfun beg end type rest))
    (let ((end (max (evil-sp--point-after 'sp-up-sexp)
                    (evil-sp--point-after 'sp-forward-sexp)
                    (point-at-eol))))
      (apply oldfun (point) end type rest))))

(defun evil-sp--override-delete-backward-char (oldfun beg end &rest rest)
  (if (save-excursion (forward-char) (sp-point-in-empty-sexp))
      (apply #'evil-delete beg (incf end) rest)
    (apply oldfun beg end rest)))

(defun evil-sp--activate-advice ()
  (advice-add 'evil-delete :around #'evil-sp--modify-region)
  (advice-add 'evil-replace :around #'evil-sp--modify-region)
  (advice-add 'evil-yank :around #'evil-sp--modify-region)
  (advice-add 'evil-delete-line :around #'evil-sp--emulate-sp-kill-sexp)
  (advice-add 'evil-change-line :around #'evil-sp--emulate-sp-kill-sexp)
  (advice-add 'evil-delete-backward-char :around
              #'evil-sp--override-delete-backward-char))

(defun evil-sp--deactivate-advice ()
  (advice-remove 'evil-delete #'evil-sp--modify-region)
  (advice-remove 'evil-replace #'evil-sp--modify-region)
  (advice-remove 'evil-yank #'evil-sp--modify-region)
  (advice-remove 'evil-delete-line #'evil-sp--emulate-sp-kill-sexp)
  (advice-remove 'evil-change-line #'evil-sp--emulate-sp-kill-sexp)
  (advice-remove 'evil-delete-backward-char
                 #'evil-sp--override-delete-backward-char))

(defun evil-sp--lighter ()
  (if smartparens-strict-mode
      " SP/se"
    " SP/e"))

(defun evil-sp--disable ()
  (evil-sp--deactivate-advice)
  (diminish-undo 'smartparens-mode))

(defun evil-sp--enable ()
  (evil-sp--activate-advice)
  (diminish 'smartparens-mode))

;;;###autoload
(define-minor-mode evil-smartparens-mode
  "Toggle evil-smartparens."
  :lighter (:eval (evil-sp--lighter))
  :init-value nil
  (if evil-smartparens-mode
      (evil-sp--enable)
    (evil-sp--disable)))

(defun evil-sp--point-on-delimiter-in-unbalanced-sexp? (beg end)
  (unless (sp-region-ok-p (evil-sp--point-after 'sp-backward-up-sexp)
                          (evil-sp--point-after 'sp-up-sexp))
    (cl-letf (((symbol-function 'sp-message) (lambda (msg))))
      (and (= (abs (- end beg)) 1)
           (ignore-errors
             (save-excursion
               (sp-backward-up-sexp)
               (sp-backward-down-sexp)
               (not (sp-get-sexp))))))))

(defun evil-sp--new-ending (beg end)
  "Find the largest safe region delimited by BEG END"
  (cl-letf (((symbol-function 'sp-message) (lambda (msg))))
    (let ((region (s-trim (buffer-substring-no-properties beg end))))
      (unless (s-blank? region)
        (cond
         ((sp-point-in-empty-sexp)
          ;; expand region if we're in an empty sexp
          (setf end (save-excursion (sp-up-sexp) (point))))

         ((evil-sp--point-on-delimiter-in-unbalanced-sexp? beg end))

         ;; reduce region if it's unbalanced due to selecting too much
         (t (while (not (or (sp-region-ok-p beg end)
                            (= beg end)))
              (cl-decf end)))))))
  (when (= beg end)
    (evil-sp--fail))
  end)

(defun evil-sp--fail ()
  (error "Can't find a safe region to act on!"))

(provide 'evil-smartparens)
;;; evil-smartparens.el ends here
