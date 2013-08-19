(require 'ac-emacs-eclim-source)
(require 'eclim)
(require 'eclimd)
(require 'java-mode-indent-annotations)

(setq eclimd-default-workspace "~/workspace"
      eclim-executable "/usr/share/eclipse/plugins/org.eclim_2.2.7.4-ge53b9ed/bin/eclim"
      eclim-eclipse-dirs '("/usr/share/eclipse")
      eclim-auto-save t
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      eclimd-wait-for-process nil
      eclim-print-debug-messages nil)

(global-eclim-mode)

(add-lambda 'java-mode-hook
  (setq c-basic-offset 4
        c-label-offset 0)

  ;; fix indentation for anonymous classes
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'inexpr-class 0)

  (ac-emacs-eclim-java-setup)
  (help-at-pt-set-timer)
  (java-mode-indent-annotations-setup)
  (define-key evil-normal-state-local-map (kbd "M-.")
    'eclim-java-find-declaration)
  (when eclim-mode
    (ac-emacs-eclim-java-setup)))

(fill-keymap eclim-mode-map
             "C-c C-e C-d" 'eclim-java-show-documentation-for-current-element
             "C-c d" 'eclim-java-show-documentation-for-current-element
             "C-c t" 'eclim-java-find-type
             "C-c f" 'eclim-java-find-generic
             "C-c r" 'eclim-java-find-references
             "C-c ," 'eclim-run-test
             "C-c c" 'eclim-problems-correct
             "C-c s" 'eclim-goto-super)

(defun eclim-goto-super ()
  "Jump to superclass."
  (interactive)
  (save-excursion
    (re-search-backward "extends \\(\\w+\\)" nil t)
    (if (match-string 1)
        (eclim-java-find-type (match-string 1))
      (re-search-forward "extends \\(\\w+\\)" nil t)
      (when (match-string 1)
        (eclim-java-find-type (match-string 1))))))

;;; This version does not visit file in another buffer
(defun eclim--visit-declaration (line)
  (ring-insert find-tag-marker-ring (point-marker))
  (find-file (assoc-default 'filename line))
  (goto-line (assoc-default 'line line))
  (move-to-column (- (assoc-default 'column line) 1)))

(defun eclim-run-test ()
  (interactive)
  (if (not (string= major-mode "java-mode"))
      (message "Sorry cannot run current buffer."))
  (compile (concat eclim-executable " -command java_junit -p
                   " eclim--project-name " -t " (eclim-package-and-class))))

(add-lambda 'eclim-problems-mode-hook
  (define-key evil-normal-state-local-map (kbd "a") 'eclim-problems-show-all)
  (define-key evil-normal-state-local-map (kbd "e") 'eclim-problems-show-errors)
  (define-key evil-normal-state-local-map (kbd "g") 'eclim-problems-buffer-refresh)
  (define-key evil-normal-state-local-map (kbd "q") 'eclim-quit-window)
  (define-key evil-normal-state-local-map (kbd "w") 'eclim-problems-show-warnings)
  (define-key evil-normal-state-local-map (kbd "f") 'eclim-problems-toggle-filefilter)
  (define-key evil-normal-state-local-map (kbd "c") 'eclim-problems-correct)
  (define-key evil-normal-state-local-map (kbd "RET") 'eclim-problems-open-current))

(defun ant-compile ()
  "Traveling up the path, find build.xml file and run compile."
  (interactive)
  (with-temp-buffer
    (while (and (not (file-exists-p "build.xml"))
                (not (equal "/" default-directory)))
      (cd ".."))
    (call-interactively 'compile)))

(defun run-current-java-file ()
  "Execute the current file's class with Java.
For example, if the current buffer is the file x.java,
then it'll call “java x” in a shell."
  (interactive)
  (let (fnm prog-name cmd-str)
    (setq fnm (file-name-sans-extension
               (file-name-nondirectory (buffer-file-name))))
    (setq prog-name "java")
    (setq cmd-str (concat prog-name " " fnm " &"))
    (shell-command cmd-str)))

(defun eclim-ac-complete ()
  "Query eclim for available completions at point."
  (interactive)
  (auto-complete (list ac-source-emacs-eclim)))

(defadvice ethan-wspace-clean-before-save-hook (around eclim-compat activate)
  "Fix spaces being removed while typing because eclim autosaves
  to get completions and ethan-wspace removes 'trailing
  whitespace'"
  (unless (and (eql major-mode 'java-mode)
               (eql evil-state 'insert))
    ad-do-it))

(defconst custom-java-style
  `((c-recognize-knr-p . nil)
    (c-basic-offset . 4)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . (
                        (func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        (innamespace . 0)
                        (arglist-intro . ++)
                        (arglist-cont-nonempty . ++)
                        (annotation-var-cont . 0)))
    (c-block-comment-prefix . "*"))
  "Custom Java Programming Style")

(defun custom-set-java-style ()
  "Set the current buffer's java-style to my Custom Programming Style. Meant to be added to `java-mode-hook'."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-toggle-auto-newline 1)
  (c-add-style "custom-java-style" custom-java-style t))

(defun eclim-problems-correct ()
  (interactive)
  (let ((p (eclim--problems-get-current-problem)))
    (if (not (string-match "\.java$" (cdr (assoc 'filename p))))
        (error "Not a Java file. Corrections are currently supported only for Java.")
      (eclim-java-correct (cdr (assoc 'line p)) (eclim--byte-offset))
      (message (buffer-name)))))

(defun eclim-java-correct (line offset)
  "Must be called with the problematic file opened in the current buffer."
  (message "Getting corrections...")
  (eclim/with-results correction-info ("java_correct" "-p" "-f" ("-l" line) ("-o" offset))
    (let ((window-config (current-window-configuration))
          (corrections (cdr (assoc 'corrections correction-info)))
          (project (eclim--project-name))) ;; store project name before buffer change
      (pop-to-buffer "*corrections*")
      (erase-buffer)
      (use-local-map eclim-java-correct-map)

      (insert "Problem: " (cdr (assoc 'message correction-info)) "\n\n")
      (if (eq (length corrections) 0)
          (insert "No automatic corrections found. Sorry.")
        (insert (substitute-command-keys
                 (concat
                  "Choose a correction by pressing \\[eclim-java-correct-choose]"
                  " on it or typing its index. Press \\[eclim-java-correct-quit] to quit"))))
      (insert "\n\n")

      (dotimes (i (length corrections))
        (let ((correction (aref corrections i)))
          (insert "------------------------------------------------------------\n"
                  "Correction "
                  (int-to-string (cdr (assoc 'index correction)))
                  ": " (cdr (assoc 'description correction)) "\n\n"
                  "Preview:\n\n"
                  (cdr (assoc 'preview correction))
                  "\n\n")))
      (goto-char (point-min))
      (setq eclim-corrections-previous-window-config window-config)
      (make-local-variable 'eclim-correction-command-info)
      (setq eclim-correction-command-info (list 'project project
                                                'line line
                                                'offset offset)))))


(provide 'init-java)
