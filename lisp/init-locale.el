(defun sanityinc/utf8-locale-p (v)
  "Return whether locale string V relates to a UTF-8 locale."
  (and v (string-match "UTF-8" v)))

(defun locale-is-utf8-p ()
  "Return t iff the \"locale\" command or environment variables prefer UTF-8."
  (or (sanityinc/utf8-locale-p (and (executable-find "locale")
                                    (shell-command-to-string "locale")))
      (sanityinc/utf8-locale-p (getenv "LC_ALL"))
      (sanityinc/utf8-locale-p (getenv "LC_CTYPE"))
      (sanityinc/utf8-locale-p (getenv "LANG"))))

(when (or window-system (locale-is-utf8-p))
  ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
  (setq utf-translate-cjk-mode nil)
  (set-language-environment 'utf-8)
  (setq locale-coding-system 'utf-8-unix)
  (set-default-coding-systems 'utf-8-unix)
  (setq buffer-file-coding-system 'utf-8-unix)
  (set-terminal-coding-system 'utf-8-unix)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8-unix))
  (prefer-coding-system 'utf-8-unix))

(provide 'init-locale)
