;;; evil-macros --- a minor mode to make Evil macros more powerful

;; Copyright (C) 2013 Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; Created: 25 September 2013
;; Version: 0.1
;; Keywords: evil macro
;; X-URL: https://github.com/expez/emacs.d

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I'm missing a few things from Evil's macro support.  Specifically
;; the ability to easily edit macros.  I'd also like to make running
;; recursive macros easier.
;;
;; Usage:
;;
;; (require 'evil-macros)
;;

;;; Code:

(require 'evil)

(defcustom em-search-wrap nil
  "Non-nil if search should wrap around in macros."
  :type 'boolean
  :group 'evil-macros)

(define-minor-mode evil-macros
  "A minor mode to make Evil macros more powerful."
  :lighter " em"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "Q") 'em-call-last-macro)
            map))

(defun em-call-last-macro (count)
  "Call the last kebyoard macro that you defined.  A prefix
argument serves as the repeat count."
  (interactive "p")
  (let ((evil-search-wrap em-search-wrap))
    (evil-execute-macro count last-kbd-macro))
  (evil-force-normal-state))

(defun em-edit-macro (register)
  (interactive "sregister: ")
  "Pops open a buffer to edit the macro stored in `register'.  If
no register is provided the most recently defined macro will be
made available for edit."
  (popwin:popup-buffer "*Messages*"))

(provide 'evil-macros)
;;; evil-macros.el ends here
