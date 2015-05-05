(require-package 'company)
(require 'company)
(require 'company-dabbrev-code)
(require-package 'company-quickhelp)
(require 'company-quickhelp)

(setq company-idle-delay 0.01
      company-show-numbers t
      company-tooltip-limit 10
      company-tooltip-align-annotations t
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-selection-changed t
      company-tooltip-flip-when-above nil
      company-require-match nil
      company-quickhelp-max-lines 60
      pos-tip-border-width 0)

(company-quickhelp-mode 1)
(global-company-mode 1)

(add-to-list 'company-dabbrev-code-modes 'clojure-mode)

(fill-keymap company-active-map
             "C-l" 'company-complete-common
             "C-k" 'company-complete-selection
             (kbd "TAB") 'yas-expand)

(defun company-complete-dabbrev ()
  (interactive)
  (let ((company-backends (cons 'company-dabbrev company-backends)))
    (company-complete)))

(after-load 'evil
  (fill-keymap evil-insert-state-map
               "C-n" 'company-complete-dabbrev))

(defadvice evil-normal-state (after cancel-company activate)
  (when company-candidates
    (company-abort)))

(provide 'init-company)
