
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

;;This has been replaced by whole-line-or-region.el
;;The following will paste a copied line above the current line regardless of the position of point:
;; (defadvice kill-ring-save (around slick-copy activate)
;;   "When called interactively with no active region, copy a single line instead."
;;   (if (or (use-region-p) (not (called-interactively-p)))
;;       ad-do-it
;;     (kill-new (buffer-substring (line-beginning-position)
;; 				(line-beginning-position 2))
;; 	      nil '(yank-line))
;;     (message "Copied line")))

;; (defadvice kill-region (around slick-copy activate)
;;   "When called interactively with no active region, kill a single line instead."
;;   (if (or (use-region-p) (not (called-interactively-p)))
;;       ad-do-it
;;     (kill-new (filter-buffer-substring (line-beginning-position)
;; 				       (line-beginning-position 2) t)
;; 	      nil '(yank-line))))

;; (defun yank-line (string)
;;   "Insert STRING above the current line."
;;   (beginning-of-line)
;;   (unless (= (elt string (1- (length string))) ?\n)
;;     (save-excursion (insert "\n")))
;;   (insert string))
;;thus, a line can be copied by M-w C-y.

(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;


(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) (set-buffer-modified-p nil) 	t))))

(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
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

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

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
		    (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
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
            ("vbs" . "cscript")
                                        ;            ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            )
          )

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

(defun fd-switch-dictionary()
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
