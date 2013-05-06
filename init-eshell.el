(require 'esh-opt)
(require 'eshell)
(require 'em-smart)
(require 'esh-toggle)
(require 'em-cmpl)
(require 'em-prompt)
(require 'em-term)

(setq eshell-where-to-jump 'begin
      eshell-history-size 1000
      eshell-save-history-on-exit t
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t
      eshell-stringify-t nil
      eshell-term-name "ansi"
       eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
      eshell-ls-dired-initial-args '("-h"))

(setq eshell-modules-list
      '(eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist
                    eshell-ls eshell-pred eshell-prompt eshell-rebind
                    eshell-script eshell-smart eshell-term eshell-unix
                    eshell-xtra))
(add-hook 'eshell-mode-hook
          '(lambda () (eshell/export "TERM" "dumb")))
(setenv "PAGER" "cat")
(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "tail")

(add-to-list 'eshell-command-completions-alist
             '("gunzip" "gz\\'"))
(add-to-list 'eshell-command-completions-alist
             '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

(add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)

(add-hook 'eshell-mode-hook
               '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-bol)))

(setq eshell-prompt-function
      (lambda nil
        (concat
         (abbreviate-file-name (eshell/pwd))
         (if (= (user-uid) 0) " # " " $ "))))

;;;###autoload
(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

;;;###autoload
(defun eshell/cds ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "src")))

;;;###autoload
(defun eshell/cdl ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory "lib")))

;;;###autoload
(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

;; these two haven't made it upstream yet
;;;###autoload
(when (not (functionp 'eshell/find))
  (defun eshell/find (dir &rest opts)
    (find-dired dir (mapconcat (lambda (arg)
                                 (if (get-text-property 0 'escaped arg)
                                     (concat "\"" arg "\"")
                                   arg))
                               opts " "))))

;;;###autoload
(when (not (functionp 'eshell/rgrep))
  (defun eshell/rgrep (&rest args)
    "Use Emacs grep facility instead of calling external grep."
    (eshell-grep "rgrep" args t)))

;;;###autoload
(defun eshell/extract (file)
  (let ((command (some (lambda (x)
                         (if (string-match-p (car x) file)
                             (cadr x)))
                       '((".*\.tar.bz2" "tar xjf")
                         (".*\.tar.gz" "tar xzf")
                         (".*\.bz2" "bunzip2")
                         (".*\.rar" "unrar x")
                         (".*\.gz" "gunzip")
                         (".*\.tar" "tar xf")
                         (".*\.tbz2" "tar xjf")
                         (".*\.tgz" "tar xzf")
                         (".*\.zip" "unzip")
                         (".*\.Z" "uncompress")
                         (".*" "echo 'Could not extract the file:'")))))
    (eshell-command-result (concat command " " file))))

(defface esk-eshell-error-prompt-face
  '((((class color) (background dark)) (:foreground "red" :bold t))
    (((class color) (background light)) (:foreground "red" :bold t)))
  "Face for nonzero prompt results"
  :group 'eshell-prompt)

(add-hook 'eshell-after-prompt-hook
          (defun esk-eshell-exit-code-prompt-face ()
            (when (and eshell-last-command-status
                       (not (zerop eshell-last-command-status)))
              (let ((inhibit-read-only t))
                (add-text-properties
                 (save-excursion (beginning-of-line) (point)) (point-max)
                 '(face esk-eshell-error-prompt-face))))))

(defun esk-eshell-in-dir (&optional prompt)
  "Change the directory of an existing eshell to the directory of the file in
the current buffer or launch a new eshell if one isn't running. If the
current buffer does not have a file (e.g., a *scratch* buffer) launch or raise
eshell, as appropriate. Given a prefix arg, prompt for the destination
directory."
  (interactive "P")
  (let* ((name (buffer-file-name))
         (dir (cond (prompt (read-directory-name "Directory: " nil nil t))
                    (name (file-name-directory name))
                    (t nil)))
         (buffers (delq nil (mapcar (lambda (buf)
                                      (with-current-buffer buf
                                        (when (eq 'eshell-mode major-mode)
                                          (buffer-name))))
                                    (buffer-list))))
         (buffer (cond ((eq 1 (length buffers)) (first buffers))
                       ((< 1 (length buffers)) (ido-completing-read
                                                "Eshell buffer: " buffers nil t
                                                nil nil (first buffers)))
                       (t (eshell)))))
    (with-current-buffer buffer
      (when dir
        (eshell/cd (list dir))
        (eshell-send-input))
      (end-of-buffer)
      (pop-to-buffer buffer))))

(provide 'init-eshell)
