--- /home/expez/.emacs.d/plugins/efuncs.el.LOCAL.7778.el	2012-07-29 11:50:15.001638888 +0200
+++ /home/expez/.emacs.d/plugins/efuncs.el.REMOTE.7778.el	2012-07-29 11:50:15.009638934 +0200
@@ -454,61 +454,12 @@
   (when buffer-file-name
     (kill-new (file-truename buffer-file-name))))
 
-(defun ido-recentf-open ()
-  "Use `ido-completing-read' to \\[find-file] a recent file"
-  (interactive)
-  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
-      (message "Opening file...")
-    (message "Aborting")))
-
-(defun ielm-auto-complete ()
-  "Enables `auto-complete' support in \\[ielm]."
-  (setq ac-sources '(ac-source-functions
-                     ac-source-variables
-                     ac-source-features
-                     ac-source-symbols
-                     ac-source-words-in-same-mode-buffers))
-  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
-  (auto-complete-mode 1))
-(add-hook 'ielm-mode-hook 'ielm-auto-complete)
-
-
-;;Following two sections stolen from Julien Danjou
-;; elisp
-(defcustom elisp-programming-major-modes
-  '(emacs-lisp-mode
-    lisp-interaction-mode
-    ielm-mode)
-  "Mode that are used to do Elisp programming.")
 
-(dolist (mode elisp-programming-major-modes)
-  (add-hook
-   (intern (concat (symbol-name mode) "-hook")) 'turn-on-eldoc-mode))
-
-(defcustom programming-language-major-modes
-  '(prog-mode     ; This is the mode perl, makefile, lisp-mode, scheme-mode,
-                                        ; emacs-lisp-mode, sh-mode, java-mode, c-mode, c++-mode,
-                                        ; python-mode inherits from.
-    lua-mode
-    cmake-mode
-    tex-mode                            ; LaTeX inherits
-    sgml-mode                           ; HTML inherits
-    css-mode
-    nxml-mode
-    diff-mode
-    haskell-mode
-    rst-mode)
-  "What to consider as programming languages.")
-
-(dolist (mode programming-language-major-modes)
-  (add-hook
-   (intern (concat (symbol-name mode) "-hook"))
-   (lambda ()
-     (font-lock-add-keywords
-      nil
-      '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\|NOTE\\)"
-         1
-         '(:box (:color "grey10" :line-width 2) :background "red" :bold t :foreground "yellow")
-         prepend)))
-     (setq show-trailing-whitespace t)
-     (flyspell-prog-mode))))
+(defun smart-line-beginning ()
+  "Move point to the beginning of text on the current line; if that is already
+the current position of point, then move it to the beginning of the line."
+  (interactive)
+  (let ((pt (point)))
+    (beginning-of-line-text)
+    (when (eq pt (point))
+      (beginning-of-line))))
