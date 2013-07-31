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

(provide 'init-java)
