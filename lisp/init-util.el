(require 'cl)
(require-package 's)
(require 's)

(defun add-auto-mode (mode &rest patterns)
  "Associate every pattern in `PATTERNS' with `MODE'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defmacro add-lambda (hook &rest body)
  "Wrap `BODY' in a lambda, and add it to `HOOK'."
  (declare (indent 1))
  `(add-hook ,hook (lambda () ,@body)))

(defvar webjump-api-sites nil)

(make-variable-buffer-local 'webjump-api-sites)

(defun webjump-api ()
  (interactive)
  (require 'webjump)
  (let* ((completion-ignore-case t)
         (default (caar webjump-api-sites))
         (url (cdr (assoc-string
                    (completing-read "Search API: " webjump-api-sites nil t
                                     nil nil default)
                    webjump-api-sites t)))
         (name (completing-read "Name: " nil nil nil (thing-at-point 'symbol))))
    (browse-url (if (webjump-null-or-blank-string-p name)
                    url
                  (concat url (webjump-url-encode name))))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

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

(defun diff-current-buffer-with-file ()
  (interactive)
  "Diff the current buffer with the content saved in the file."
  (diff-buffer-with-file (current-buffer)))

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
        (delete-file currentFile)))))

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
      (setq bfilename (concat bfilename "~")))
    (copy-file cfile bfilename t)
    (message (concat "Backup saved as: " (file-name-nondirectory bfilename)))))

(defun open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

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
    (kill-buffer mybuffer)))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%F" (current-time))))

(defun how-many-in-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
 Non-interactive arguments are BEGIN END REGEXP"
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

(defun indentation-style ()
  "Returns 'tabs if tabs, 'spaces if spaces."
  (let ((space-count (how-many-in-region (point-min) (point-max) "^  "))
        (tab-count (how-many-in-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count)
        'spaces
      'tabs)))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

(defun fd-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "norwegian") "english" "norwegian")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)))

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

(defun copy-file-name-to-clipboard ()
  "Copy name of the file the buffer is visiting to the clipboard as a string.

With a prefix the file name is copied to the clipboard without
being turned into a string."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (if current-prefix-arg
          (kill-new filename)
        (kill-new (format "\"%s\"" filename)))
      (message "Copied file name '%s' to the clipboard." filename))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (eq pt (point))
      (beginning-of-line))))

(defun open-all-files-with-extension (dir extension)
  "Open all files below dir with the given extension."
  (interactive "DBase directory: \nsExtension: ")
  (let* ((list (directory-files dir t "^[^.]"))
         (files (remove-if 'file-directory-p list))
         (dirs (remove-if-not 'file-directory-p list)))
    (dolist (file files)
      (if (s-suffix-p file extension)
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
        (goto-char position))))
    (evil-scroll-line-to-center nil))
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

(defun mediawiki-insert-sub-header ()
  "Insert subheader  via  === (e.g. === FOO ===.)"
  (interactive)
  (mediawiki-insert "===" "==="))

(defun kill-region-or-backward-kill-word ()
  "Call kill region, if region is active, otherwise backward-kill-word"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word 1)))

(defun fill-keymap (keymap &rest mappings)
  "Fill `KEYMAP' with `MAPPINGS'.
See `pour-mappings-to'."
  (pour-mappings-to keymap mappings))

(defun fill-keymaps (keymaps &rest mappings)
  "Fill `KEYMAPS' with `MAPPINGS'.
See `pour-mappings-to'."
  (dolist (keymap keymaps keymaps)
    (let ((map (if (symbolp keymap)
                   (symbol-value keymap)
                 keymap)))
      (pour-mappings-to map mappings))))

(defun cofi/set-key (map spec cmd)
  "Set in `map' `spec' to `cmd'.

`Map' may be `'global' `'local' or a keymap.
A `spec' can be a `read-kbd-macro'-readable string or a vector."
  (let ((setter-fun (case map
                      (global #'global-set-key)
                      (local #'local-set-key)
                      (t (lambda (key def) (define-key map key def)))))
        (key (typecase spec
               (vector spec)
               (string (read-kbd-macro spec))
               (t (error "wrong argument")))))
    (funcall setter-fun key cmd)))

(defun take (n lst)
  "Return atmost the first `N' items of `LST'."
  (let (acc '())
    (while (and lst (> n 0))
      (decf n)
      (push (car lst) acc)
      (setq lst (cdr lst)))
    (nreverse acc)))

(defun group (lst n)
  "Group `LST' into portions of `N'."
  (let (groups)
    (while lst
      (push (take n lst) groups)
      (setq lst (nthcdr n lst)))
    (nreverse groups)))

(defun pour-mappings-to (map mappings)
  "Calls `cofi/set-key' with `map' on every key-fun pair in `MAPPINGS'.
`MAPPINGS' is a list of string-fun pairs, with a `READ-KBD-MACRO'-readable string and a interactive-fun."
  (dolist (mapping (group mappings 2))
    (cofi/set-key map (car mapping) (cadr mapping)))
  map)

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented region"))
      (progn
        (indent-buffer)
        (message "Indented buffer")))))

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
          (save-buffer))))

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (progn
          (load (file-name-sans-extension fName)))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))

(defun load-if-exists (file)
  "Calls LOAD on FILE if FILE exists."
  (let ((file-exists (file-exists-p (expand-file-name file))))
    (when file-exists
      (load (expand-file-name file)))
    file-exists))

(defun directory-dirs (dir)
  "Find all directories in DIR."
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t)))
    (dolist (file files)
      (unless (member file '("." ".."))
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (directory-dirs file))
                               dirs))))))
    dirs))

(defun report-init-results (errors)
  (if (not errors)
      (message "Initialization successful!")
    (message "Errors during init, see buffer *init-errors*")
    (switch-to-buffer "*init-errors*")
    (insert (mapconcat #'identity errors "\n"))))

(defun file-to-feature (file)
  (intern (file-name-nondirectory (s-chop-suffix "\.el" file))))

(defun safe-load-init-files (dir &optional regexp)
  "Require all elisp files in DIR.  When REGEXP is provided match
only those files with name of form REGEXP.el.

REGEXP defaults to ^init-.*\.el$"
  (let* ((regexp (if regexp regexp "^init-.\*\.el\$"))
         (files (directory-files dir t regexp))
         (features (mapcar #'file-to-feature files))
         (init-errors nil))
    (loop for feature in features
          do (condition-case err
                 (load (symbol-name feature))
               (error (push (format "Error requiring %s: %s" feature err)
                            init-errors))))
    (report-init-results init-errors)))

(defmacro defkeymap (symbol &rest mappings)
  "Define keymap bound to `symbol'.
See `pour-mappings-to'"
  `(progn (unless (boundp ',symbol)
            (defvar ,symbol (make-sparse-keymap)))
          (fill-keymap ,symbol ,@mappings)))

(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun toggle-bury-compilation-buffer ()
  (interactive)
  (if compilation-finish-function
      (progn
        (setq compilation-finish-function nil)
        (message "Bury buffer: OFF."))
    (setq compilation-finish-function
          (lambda (buf str)
            (if (string-match "exited abnormally" str)
                (message "Compilation errors, press M-n to visit")

              (run-at-time 1.3 nil #'switch-to-prev-buffer
                           (get-buffer-window buf) 'append)

              (message "Compilation succesful!"))))
    (message "Bury buffer: ON.")))

(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun path-to-executable (program)
  "Returns the path to `program' or NIL.  Relies on which, so
only works on *nix."
  (let* ((which (s-chomp (shell-command-to-string (concat "which " program))))
         (executable (if (or (string-match "not found" which)
                             (string-match "no \\w+ in" which))
                         nil
                       which)))
    executable))

(defun inside-string-p ()
  "T if point is inside a string, NIL otherwise."
  (nth 3 (syntax-ppss)))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun debug-config (test-fn)
  "Repeatedly calls `test-fn' to verify that everything is OK
while loading init files.  `test-fn' should return NIL to
indicate failure."
  (when test-fn
    (find-file "/tmp/debug-config")
    (loop for file in (directory-files user-emacs-directory t "^init-.*\.el\$")
          always (funcall test-fn)
          do (progn
               (set-buffer "debug-config")
               (insert (format "Test OK.  Loading file %s..." file))
               (save-buffer)
               (load file))
          finally (delete-file "/tmp/debug-config"))))

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defun edit-user-config-file ()
  "Use completing read to open one of the configuration files for
  emacs for editing."
  (interactive)
  (ido-find-file-in-dir my-config-dir))

(defun find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (s-split "/" (getenv "SHELL")))))
         (shell-init-file (cond
                           ((s-equals? "zsh" shell) ".zshrc")
                           ((s-equals? "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file (expand-file-name shell-init-file (getenv "HOME")))))

(defun make-current-buffer-executable ()
  "Make `buffer-file-name' executable."
  (interactive)
  (when (not (file-executable-p buffer-file-name))
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message "Made %s executable" buffer-file-name)))

(defun clean-and-save-buffer ()
  "Reindents the buffer and cleans up any whitespace errors.
Then saves the buffer."
  (interactive)
  (indent-buffer)
  (ethan-wspace-clean-all)
  (indent-for-tab-command)
  (save-buffer))

(defun what-face (pos)
  "Returns the face at `POS'"
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun buffer-to-unix ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix))

(defun buffer-to-dos()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun buffer-to-mac ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

(defun buffer-to-utf8 ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t)
  (save-buffer))

(defun run-term (&optional arg)
  "Spawns an urxvt client in the buffer directory"
  (interactive "P")
  (let ((default-directory default-directory))
    (when arg
      (when (string-match "^.*/src/$" default-directory)
        (cd "../")
        (when (file-directory-p "build")
          (cd "build"))))
    (start-process "my-urxvt" nil "urxvtc")))

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(defun toggle-debug (_)
  (interactive "P")
  (if current-prefix-arg
      (toggle-debug-on-quit)
    (toggle-debug-on-error)))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(provide 'init-util)
