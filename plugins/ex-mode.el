;;Minor mode map consist of all my bindings and functions

;; Update the buffer list for uniquify when needed
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))

(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1)
               (rename-buffer new-name) (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))


(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR."
 (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn
   (copy-file filename newname 1)
   (delete-file filename)
   (set-visited-file-name newname)
   (set-buffer-modified-p nil) t))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(defun ant-compile ()
  "Traveling up the path, find build.xml file and run compile."
  (interactive)
  (with-temp-buffer
    (while (and (not (file-exists-p "build.xml"))
                (not (equal "/" default-directory)))
      (cd ".."))
    (call-interactively 'compile)))

;; Move line functions
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; Convert endlines
(defun dos2unix ()
  "Convert a buffer from dos ^M end of lines to unix end of lines"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  "Convert a buffer from unix end of line to dos ^M end of lines."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))


(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun launch (command)
  "Launch an application from Emacs, with its own output
buffer. This is like asynch-shell-command but allows for any
number of processes at a time, rather than just one. If given a
prefix argument, the process's buffer is displayed."
  (interactive (list (read-shell-command (concat default-directory "$ "))))
  (let* ((name (car (split-string-and-unquote command)))
         (buffer (generate-new-buffer (concat "*" name "*"))))
    (set-process-sentinel (start-process-shell-command name buffer command)
                          'launch-sentinel)
    (if (eq (car current-prefix-arg) 4)
        (display-buffer buffer))))

(defun launch-sentinel (proc event)
  "Reports on changes in `launch'ed applications."
  (message (format "%s: %s" proc event)))

(defun diff-buffer-with-file ()
  "Diff the current buffer with the content saved in the file."
  (lambda nil (interactive) (diff-buffer-with-file (current-buffer))))

;; Guesses which file is the master file by looking for \include statements.
(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (if (equal filename nil)
      nil
    (let ((candidate nil)
          (filename (file-name-nondirectory filename)))
      (save-excursion
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (let ((name (buffer-name))
                  (file buffer-file-name))
              (if (and file (string-match "\\.tex$" file))
                  (progn
                    (goto-char (point-min))
                    (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                        (setq candidate file))
                    (if (re-search-forward (concat "\\\\include{"
                                                   (file-name-sans-extension filename) "}") nil t)
                        (setq candidate file))))))))
      (if candidate
          (message "TeX master document: %s" (file-name-nondirectory candidate)))
      candidate)))

(defun delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too.
If no file is associated, just close buffer without prompt for save."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Delete file?: " currentFile))
      (kill-buffer (current-buffer))
      (when (not (equal currentFile nil))
        (delete-file currentFile) ) ) ) )

(defun make-backup ()
  "Make a backup copy of current buffer's file.
Create a backup of current buffer's file.
The new file name is the old file name with “~” added at the end, in the same dir.
If such a file already exist, append more “~”.
If the current buffer is not associated with a file, its a error."
  (interactive)
  (let (cfile bfilename)
    (setq cfile (buffer-file-name))
    (setq bfilename (concat cfile "~"))

    (while (file-exists-p bfilename)
      (setq bfilename (concat bfilename "~"))
      )

    (copy-file cfile bfilename t)
    (message (concat "Backup saved as: " (file-name-nondirectory bfilename)))
    )
  )

(defun open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.
 (This command always run the saved version.)

If the file is emacs lisp, run the byte compiled version if appropriate."
  (interactive)
  (let (suffixMap fName fSuffix progName cmdStr)

    ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "js")
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")))

    (setq fName (buffer-file-name))
    (setq fSuffix (file-name-extension fName))
    (setq progName (cdr (assoc fSuffix suffixMap)))
    (setq cmdStr (concat progName " \""   fName "\""))

    (when (buffer-modified-p)
      (progn
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (progn
          (load (file-name-sans-extension fName))
          )
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        )
      )
    ))

(defun run-current-java-file ()
  "Execute the current file's class with Java.
For example, if the current buffer is the file x.java,
then it'll call “java x” in a shell."
  (interactive)
  (let (fnm prog-name cmd-str)
    (setq fnm (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    (setq prog-name "java")
    (setq cmd-str (concat prog-name " " fnm " &"))
    (shell-command cmd-str))
  )

(defun change-file-newline (fpath eol-system)
  "Change file's line ending to unix convention.
FPATH is full path to file.
eol-system is one of “'unix”, “'dos”, “'mac”.
The “'dos” means Windows's convention.
The “'mac” means Mac OS Classic's convention.
For Mac OS X, use “'unix”."
  (let (mybuffer)
    (setq mybuffer (find-file fpath))
    (set-buffer-file-coding-system eol-system)
    (save-buffer)
    (kill-buffer mybuffer)
    )
  )

(defun dired-2unix-eol-marked-files ()
  "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
  (interactive)
  (mapc
   (lambda (ff) (change-file-newline ff 'unix))
   (dired-get-marked-files))
  )

(defun dired-utf-8-unix-marked-files ()
  "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
  (interactive)
  (mapc
   (lambda (ff) (change-file-newline ff 'utf-8-unix))
   (dired-get-marked-files))
  )

(defun delete-current-file ()
  "Delete the file associated with the current buffer.
Delete the current buffer too.
If no file is associated, just close buffer without prompt for save."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Delete file?: " currentFile))
      (kill-buffer (current-buffer))
      (when (not (equal currentFile nil))
        (delete-file currentFile) ) ) ) )

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ) )

 (defun how-many-region (begin end regexp &optional interactive)
   "Print number of non-trivial matches for REGEXP in region.
 Non-interactive arguments are Begin End Regexp"
   (interactive "r\nsHow many matches for (regexp): \np")
   (let ((count 0) opoint)
     (save-excursion
       (setq end (or end (point-max)))
       (goto-char (or begin (point)))
       (while (and (< (setq opoint (point)) end)
                   (re-search-forward regexp end t))
         (if (= opoint (point))
             (forward-char 1)
           (setq count (1+ count))))
       (if interactive (message "%d occurrences" count))
       count)))

 (defun infer-indentation-style ()
   ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
   ;; neither, we use the current indent-tabs-mode
   (let ((space-count (how-many-region (point-min) (point-max) "^  "))
         (tab-count (how-many-region (point-min) (point-max) "^\t")))
     (if (> space-count tab-count) (setq indent-tabs-mode nil))
     (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )

(defun fd-switckh-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "norwegian") "english" "norwegian")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))


(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (not (file-remote-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun revert-all-buffers ()
  "Revert all non-modified buffers associated with a file.
This is to update existing buffers after a Git pull of their underlying files."
  (interactive)
  (save-current-buffer
    (mapc (lambda (b)
            (set-buffer b)
            (unless (or (null (buffer-file-name)) (buffer-modified-p))
              (revert-buffer t t)
              (message "Reverted %s\n" (buffer-file-name))))
          (buffer-list))))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

(defcustom elisp-programming-major-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    ielm-mode)
  "Mode that are used to do Elisp programming.")

(dolist (mode elisp-programming-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   (lambda ()
     (turn-on-eldoc-mode)
     (rainbow-delimiters-mode 0)
     (set-face-foreground 'paren-face "grey30"))))

(defcustom programming-language-major-modes
  '(prog-mode     ; This is the mode perl, makefile, lisp-mode, scheme-mode,
                  ; emacs-lisp-mode, sh-mode, java-mode, c-mode, c++-mode,
                  ; python-mode inherits from.
    lua-mode
    cmake-mode
    tex-mode                            ; LaTeX inherits
    sgml-mode                           ; HTML inherits
    css-mode
    nxml-mode
    diff-mode
    haskell-mode
    rst-mode)
  "What to consider as programming languages.")

(dolist (mode programming-language-major-modes)
  (add-hook
   (intern (concat (symbol-name mode) "-hook"))
   (lambda ()
     (font-lock-add-keywords
      nil
      '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\|NOTE\\)"
         1
         '(:box (:color "grey10" :line-width 2) :background "red" :bold t :foreground "yellow")
         prepend)))
     (rainbow-mode 1)
     (rainbow-delimiters-mode 1)
     (setq show-trailing-whitespace t)
     (flyspell-prog-mode)
     (setq whitespace-style '(face lines-tail))
     (whitespace-mode 1)
     (glasses-mode 1)
     (set (make-local-variable 'compile-command)
            (format "make -f %s" (get-closest-pathname))))))

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defun string/ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
    (string= (substring s (- 0 elength)) ending)))

(defun open-all-files-with-extension (dir extension)
  "Open all files below dir with the given extension."
  (interactive "DBase directory: \nsExtension: ")
  (let* ((list (directory-files dir t "^[^.]"))
         (files (remove-if 'file-directory-p list))
         (dirs (remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (if (string/ends-with file extension)
          (find-file-noselect file)))
    (dolist (dir dirs)
       (find-file-noselect dir)
       (open-all-files-with-extension dir extension))))

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the
 current directory towards root. This may not do the correct thing in presence
 of links. If it does not find FILE, then it shall return the name of FILE in
 the current directory, suitable for creation"
  (let ((root (expand-file-name "/")))
    (expand-file-name file
                      (loop
                       for d = default-directory then (expand-file-name ".." d)
                       if (file-exists-p (expand-file-name file d))
                       return d
                       if (equal d root)
                       return nil))))

  (defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))
             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))
             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(defvar ex-mode-keymap
  (let ((map (make-sparse-keymap)))

    ;;Ergomacs bindings for movement
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "M-j") 'backward-char)
    (define-key map (kbd "M-k") 'next-line)
    (define-key map (kbd "M-i") 'previous-line)

    (define-key map (kbd "M-u") 'backward-word)
    (define-key map (kbd "M-o") 'forward-word)

    (define-key map (kbd "M-U") 'backward-paragraph)
    (define-key map (kbd "M-O") 'forward-paragraph)

    (define-key map (kbd "M--") 'toggle-letter-case)
    ;;Swap a bunch of bindings around.
    (define-key map (kbd "C-a") 'smart-line-beginning)
    (define-key map (kbd "C-x C-c") 'whole-line-or-region-kill-ring-save)
    (define-key map (kbd "C-w") 'backward-kill-word)
    (define-key map (kbd "M-w") 'backward-kill-line)
    (define-key map (kbd "C-x m") 'ido-hacks-execute-extended-command)
    (define-key map (kbd "C-x C-m") 'ido-hacks-execute-extended-command)
    (define-key map (kbd "C-o") 'undo)
    (define-key map (kbd "C-z") 'open-line)
    (define-key map (kbd "C-x C-k") 'kill-region)
    (define-key map (kbd "C-,") 'beginning-of-buffer)
    (define-key map (kbd "C-.") 'end-of-buffer)
    (define-key map (kbd "C-x g") 'goto-line)
    (define-key map (kbd "C-x G") 'goto-char)
    (define-key map (kbd "C-x c") 'compile)
    (define-key map (kbd "C-x s") 'shell)
    (define-key map (kbd "M-n") 'next-error)
    (define-key map (kbd "M-p") 'previous-error)
    (define-key map (kbd "C-z") 'open-line)
    (define-key map (kbd "C-x C-b") 'ibuffer)
    (define-key map (kbd "C-x r v") 'register-list)
    (define-key map (kbd "C-<tab>") 'hippie-expand)
    (define-key map (kbd "M-S-k") 'kill-sentence)
    ;; easy spell check
    (define-key map (kbd "<f8>") 'ispell-word)
    ;;(global-set-key (kbd "C-S-<f8>") 'fd-switch-dictionary)
    (define-key map (kbd "M-S-<f8>") 'flyspell-buffer)
    (define-key map (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
    (define-key map (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)

    (define-key map (kbd "C-c q") 'save-buffers-kill-terminal)
    (define-key map [insert] 'overwrite-mode)
    (define-key map [kp-insert] 'overwrite-mode)
    (define-key map (kbd "C-x v t") 'vc-create-tag)
    (define-key map (kbd "C-x v s") 'magit-status)
    (define-key map (kbd "C-'") 'push-mark-command)
    (define-key map (kbd "M-'") 'pop-global-mark)
    (define-key map (kbd "C-;") 'ace-jump-mode)
    (define-key map (kbd "C-ø") 'push-mark-command)
    (define-key map (kbd "M-ø") 'pop-global-mark)
    (define-key map (kbd "C-æ") 'ace-jump-mode)
    (define-key map (kbd "C-x a r") 'align-regexp)
    (define-key map (kbd "M-N") 'scroll-up-line)
    (define-key map (kbd "M-P") 'scroll-down-line)
    (define-key map (kbd "M-g c") 'goto-char)
    (define-key map (kbd "M-g M-c") 'goto-char)
    (define-key map (kbd "M-m") 'er/expand-region)
    (define-key map (kbd "<f5>") 'call-last-kbd-macro)
    (define-key map (kbd "C-c i") 'ido-goto-symbol)

    ;; Windmove
    (define-key map (kbd "<left>") 'windmove-left)
    (define-key map (kbd "<up>") 'windmove-up)
    (define-key map (kbd "<right>") 'windmove-right)
    (define-key map (kbd "<down>") 'windmove-down)

    (define-key map (kbd "C-x C-r")'ido-recentf-open)
    (define-key map (kbd "M-<up>") 'move-text-up)
    (define-key map (kbd "M-<down>") 'move-text-down)
    (define-key map (kbd "M-<backspace>") 'delete-indentation)

    map)
  "Keymap containing all my bindings. ")

(defun turn-on-ex-mode ()
  (interactive)
  "Enable 'ex-mode'"
  (ex-mode 1))


(define-minor-mode ex-mode
  "Turns on all my key-bindings and activate all functions."
  :global t
  :init-value nil
  :keymap ex-mode-keymap
  :lighter " Ex"

  ;; Minor mode body
  (defalias 'rfb 'rename-file-and-buffer)
  (defalias 'mbf 'move-buffer-file)
  (defalias 'rb 'revert-buffer)
  (defalias 'oaf 'open-all-files-with-extension)

  ;;Evil mode
  (evil-ex-define-cmd "n[ew]" 'evil-window-new)

  (define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)
  ;;(key-chord-define-global "lj" 'evil-normal-state)
  (key-chord-define-global "qr" 'query-replace-regexp)
  (key-chord-define-global "qm" 'moccur))

(defun eclim-ac-complete ()
  "Query eclim for available completions at point."
  (interactive)
  (auto-complete (list 'ac-source-emacs-eclim
                       'ac-source-emacs-eclim-c-dot)))

(provide 'ex-mode)
