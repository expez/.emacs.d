(require-package 'company)
(require 'company)
(require 'company-dabbrev-code)

(setq company-idle-delay 0.2
      company-show-numbers t
      company-tooltip-limit 10
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-selection-changed t
      company-tooltip-flip-when-above t
      company-require-match nil)

(add-to-list 'company-dabbrev-code-modes 'clojure-mode)

(fill-keymap company-active-map "C-l" 'company-complete-common)

(defun company-complete-dabbrev ()
  (interactive)
  (let ((company-backends '(company-dabbrev-code)))
    (company-complete)))

(after-load 'evil
  (fill-keymap evil-insert-state-map
               "C-n" 'company-complete-dabbrev))

(defadvice evil-normal-state (after cancel-company activate)
  (when company-candidates
    (company-abort)))

(define-key global-map (kbd "C-l") 'company-complete)

(eval-after-load 'evil
  '(dotimes (i 10)
     (define-key evil-insert-state-map
       (vector (+ (aref (kbd "M-0") 0) i))
       `(lambda ()
          (interactive)
          (company-complete-number ,(if (zerop i) 10 i))))))
