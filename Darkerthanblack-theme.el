;;; Darkerthanblack-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010 Joshua Jay Herman.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme Darkerthanblack
  "Another dark theme. :-)")

(custom-theme-set-faces
 'Darkerthanblack
 '(default ((t (:background "#06020a" :foreground "#ffffff"))))
 '(cursor ((t (:background "#ffffff" :foreground "#000000"))))
 '(region ((t (:background "#07d900" :weight bold))))
 '(mode-line ((t (:background "#ffffff" :foreground "#323232"))))
 '(mode-line-inactive ((t (:background "#fafeff" :foreground "#323232"))))
 '(fringe ((t (:background "#f2f2f2"))))
 '(minibuffer-prompt ((t (:foreground "#00CD59"))))
 '(font-lock-builtin-face ((t (:foreground "#ba55d3"))))
 '(font-lock-comment-face ((t (:foreground "#f2ffc3"))))
 '(font-lock-constant-face ((t (:foreground "#9ed6c7"))))
 '(font-lock-function-name-face ((t (:foreground "#82bdd1"))))
 '(font-lock-keyword-face ((t (:foreground "#99fd66"))))
 '(font-lock-string-face ((t (:foreground "#4b73de"))))
 '(font-lock-type-face ((t (:foreground "#e3d10a"))))
 '(font-lock-variable-name-face ((t (:foreground "#a0522d"))))
 '(font-lock-warning-face ((t (:foreground "#ff0000" :weight bold))))
 '(isearch ((t (:background "#cd00cd" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#dc54eb"))))
 '(link ((t (:foreground "#0000ff" :underline t))))
 '(link-visited ((t (:foreground "#8b008b" :underline t))))
 '(button ((t (:underline t))))
 '(header-line ((t (:background "#ffffff" :foreground "#323232")))))

(provide-theme 'Darkerthanblack)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; Darkerthanblack-theme.el  ends here
