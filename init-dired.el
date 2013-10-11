(require 'wdired)

(setq wdired-allow-to-change-permissions t
      wdired-confirm-overwrite t
      wdired-use-interactive-rename t)

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-copies 'always
      ddired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))


(toggle-diredp-find-file-reuse-dir 1)
(setq dired-dwim-target t)

(defun dired-2unix-eol-marked-files ()
  "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
  (interactive)
  (mapc
   (lambda (ff) (change-file-newline ff 'unix))
   (dired-get-marked-files)))

(defun dired-utf-8-unix-marked-files ()
  "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
  (interactive)
  (mapc
   (lambda (ff) (change-file-newline ff 'utf-8-unix))
   (dired-get-marked-files)))

(provide 'init-dired)
