(let ((ag (path-to-executable "ag")))
  (when ag
      (setq ack-and-a-half-executable ag)))

(setq ack-and-a-half-prompt-for-directory t)

(defadvice ack-and-a-half-arguments-from-options (after
                                                  compatible-ag-options
                                                  activate)
  (setq ad-return-value (delete "--env" ad-return-value))
  (setq ad-return-value (delete "--noenv" ad-return-value))
  ;;(setq ad-return-value (delete "--nocolor" ad-return-value))
  )

(provide 'init-ack-and-a-half)
