(require 'ac-emacs-eclim-source)
(require 'eclim)
(require 'eclimd)
(require 'java-mode-indent-annotations)

(setq eclimd-default-workspace "~/workspace"
      eclim-executable "/usr/share/eclipse/eclim"
      eclim-auto-save t
      help-at-pt-display-when-idle t
      help-at-pt-timer-delay 0.1
      eclimd-wait-for-process nil
      eclim-print-debug-messages t)

(add-hook 'java-mode-hook
          '(lambda ()
             (setq c-basic-offset 4
                   c-label-offset 0)

             ;; fix indentation for anonymous classes
             (c-set-offset 'substatement-open 0)
             (c-set-offset 'inexpr-class 0)

             (eclim-mode 1)
             (ac-emacs-eclim-config)
             (help-at-pt-set-timer)
             (java-mode-indent-annotations-setup)
             (custom-set-variables
              '(eclim-eclipse-dirs '("/usr/share/eclipse")))
             (setq c-basic-offset 4)))

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
    (setq fnm (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    (setq prog-name "java")
    (setq cmd-str (concat prog-name " " fnm " &"))
    (shell-command cmd-str)))

(defun eclim-ac-complete ()
  "Query eclim for available completions at point."
  (interactive)
  (auto-complete (list 'ac-source-emacs-eclim
                       'ac-source-emacs-eclim-c-dot)))

(provide 'init-java)
