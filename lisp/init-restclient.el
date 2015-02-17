(defun override-proxy-settings-for-localhost (oldfun method url &rest args)
  (if (or (s-contains-p "localhost" url)
          (s-contains-p "127.0.0.1" url))
      (let ((url-proxy-services nil))
        (apply oldfun method url args))
    (apply oldfun method url args)))

(advice-add 'restclient-http-do :around #'override-proxy-settings-for-localhost)

(provide 'init-restclient)
