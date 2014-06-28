(require-package 'company)
(require 'company)
(require 'company-dabbrev-code)

(setq company-idle-delay 0.2
      company-show-numbers t
      company-tooltip-limit 10
      company-minimum-prefix-length 0
      company-selection-wrap-around t
      company-selection-changed t
      company-require-match nil)

(add-to-list 'company-dabbrev-code-modes 'clojure-mode)

(fill-keymap company-active-map
             "C-h" 'company-show-doc-buffer
             "C-j" 'company-select-next
             "C-k" 'company-select-previous
             "C-l" 'company-complete-common)

(defun company-complete-dabbrev ()
  (interactive)
  (let ((company-backends '(company-dabbrev-code)))
    (company-complete)))

(after-load 'evil
  (fill-keymap evil-insert-state-map
               "C-n" 'company-complete-dabbrev))

(define-key global-map (kbd "C-l") 'company-complete)
