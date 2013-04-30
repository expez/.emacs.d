(let ((ag (path-to-executable "ag")))
  (when ag
      (setq ack-and-a-half-executable ag)))

(setq ack-and-a-half-prompt-for-directory t)

(defadvice ack-and-a-half-arguments-from-options (after
                                                  compatible-ag-options
                                                  activate)
  (setq ad-return-value (delete "--env" ad-return-value))
  (setq ad-return-value (delete "--noenv" ad-return-value))
  (setq ad-return-value (delete "--nocolor" ad-return-value)))

;;; until upstream merges my PR
(defcustom ack-and-a-half-use-ido t
  "Whether or not ack-and-a-half should use ido to provide
  completion suggestions when prompting for directory.")

(defun ack-and-a-half-read-dir ()
  (let ((dir (run-hook-with-args-until-success 'ack-and-a-half-root-directory-functions)))
    (if ack-and-a-half-prompt-for-directory
        (if (and dir (eq ack-and-a-half-prompt-for-directory 'unless-guessed))
            dir
          (if ack-and-a-half-use-ido
              (ido-read-directory-name "Directory: " dir dir t)
            (read-directory-name "Directory: " dir dir t)))
      (or dir
          (and buffer-file-name (file-name-directory buffer-file-name))
          default-directory))))

(provide 'init-ack-and-a-half)
