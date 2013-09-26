(require 'auto-complete-nxml)
(require 'nxml-mode)

(setq auto-complete-nxml-popup-help-key "C-,")

(add-lambda 'nxml-mode-hook
  (make-local-variable whitespace-line-column)
  (setq whitespace-line-column 120))

(add-auto-mode
 'nxml-mode
 (concat "\\."
         (regexp-opt
          '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
            "gpx" "tcx"))
         "\\'"))
(setq nxml-slash-auto-complete-flag t)

(push (concat user-emacs-directory "schemas/schemas.xml")
      rng-schema-locating-files)

;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun pp-xml-region (begin end)
  "Pretty format XML markup in region. The function inserts
linebreaks to separate tags that have nothing but whitespace
between them.  It then indents the markup by using nxml's
indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(defun nxml-where ()
      "Display the hierarchy of XML elements the point is on as a path."
      (interactive)
      (let ((path nil))
        (save-excursion
          (save-restriction
            (widen)
            (while (condition-case nil
                       (progn
                         (nxml-backward-up-element) ; always returns nil
                         t)
                     (error nil))
              (setq path (cons (xmltok-start-tag-local-name) path)))
            (message "/%s" (mapconcat 'identity path "/"))))))

(provide 'init-nxml)
