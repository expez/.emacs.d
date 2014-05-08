(require-package 'company)
(require 'company)

(setq company-idle-delay t
      company-tooltip-limit 10
      company-minimum-prefix-length 2)

(fill-keymap company-active-map
             "C-h" 'company-show-doc-buffer
             "C-j" 'company-select-next
             "C-k" 'company-select-previous
             "C-l" 'company-complete-common)
