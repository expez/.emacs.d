(require-package 'company)
(require 'company)

(setq company-idle-delay 0.2
      company-tooltip-limit 10
      company-minimum-prefix-length 2
      company-require-match nil)

(fill-keymap company-active-map
             "C-h" 'company-show-doc-buffer
             "C-j" 'company-select-next
             "C-k" 'company-select-previous
             "C-l" 'company-complete-common)

(define-key global-map (kbd "C-l") 'company-complete)
