;;; test-op-mode --- a minor mode to run common lisp tests.

;; Copyright (C) 2013 Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; Created: 20 March 2013
;; Version: 1.0
;; Keywords: test lisp
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

;; Always running your tests in the REPL is unsanitary.  This mode
;; lets you easily run the tests for your system in a fresh environment.
;;
;; This mode relies heavily on asdf.  The system under test is
;; loaded using asdf:load-system and the tests themselves are
;; executed using asdf:test-op.
;;
;; Usage:
;;
;; Configure asdf:test-op to run your tests.
;;
;; From some file within your project just call the function
;; `test-op' and all will be well.

;;; Code:

(define-minor-mode test-op-mode
  "A minor mode for running common lisp tests"
  :lighter " test-op"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-,") 'test-op)
            map))

(defun* test-op-find-asd (dir)
  (let ((dir (expand-file-name dir)))
    (if (< (length dir) 2)
        (error "Unable to locate system definition file")
      (progn (dolist (file (directory-files dir))
               (when (string= (file-name-extension file) "asd")
                 (return-from test-op-find-asd (expand-file-name file))))
             (test-op-find-asd (file-name-directory (chop dir)))))))


(defun test-op-test-cmd (system)
  (concat inferior-lisp-program " --non-interactive "
                   "--eval \"(unwind-protect (progn (asdf:load-system :" system
          ") (asdf:operate 'asdf:test-op :" system
          ")) (sb-ext:exit))\""))

(defun test-op ()
  (interactive)
  "Runs the tests for the current asdf system."
  (let* ((asd-file (test-op-find-asd (file-name-directory (buffer-file-name))))
        (system-name (file-name-sans-extension
                      (file-name-nondirectory asd-file)))
        (compilation-scroll-output t))
    (compile (test-op-test-cmd system-name))))

(provide 'test-op-mode)

;;; test-op.el ends here
