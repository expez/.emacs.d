(require 'eproject)
(require 'eproject-extras)

(setf eproject-completing-read-function 'eproject--ido-completing-read)
(provide 'init-eproject)
