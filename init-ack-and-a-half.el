(let (ag (path-to-executable "ag"))
  (if ag
      (setq ack-and-a-half-executable ag)
    (setq ack-and-a-half-executable "/usr/bin/vendor_perl/ack")))

(setq ack-and-a-half-prompt-for-directory t)

(defadvice ack-and-a-half-arguments-from-options (after
                                                  compatible-ag-options
                                                  activate)
  (delete "--env" ad-return-value)
  (delete "--noenv" ad-return-value))

(provide 'init-ack-and-a-half)
