;;; js-lookup --- quickly look up JavaScript documentation

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Use `js-lookup' to prompt for a topic to look up and open in the
;; browser (`browse-url').

;;; Code:

(eval-when-compile (require 'cl))
(require 'ido)

(defvar js-lookup-root (file-name-directory load-file-name)
  "Data root directory for JavaScript lookup.")

(defvar js-lookup-db (make-hash-table :test 'equal)
  "Lookup database mapping items to look up against their URLs.")

(defvar js-lookup-path ()
  "Current documentation path while loading the database (dynamically bound).")

(defvar js-lookup-category-seperator "."
  "Separator between category and item name in the selection list.")

(defvar js-lookup-path-seperator "/"
  "Separator between category and item name in the path.")

;;;###autoload
(defun js-lookup (select)
  "Lookup something related to JavaScript. If called
interactively, prompts the user for an item to look up."
  (interactive
   (let ((keys (loop for key being the hash-keys of js-lookup-db collect key)))
     (list (ido-completing-read "Describe JavaScript: " keys nil t))))
  (browse-url (apply #'concat (reverse (gethash select js-lookup-db)))))

;; Database DSL macros

(defmacro js-lookup/entry (item path)
  "Add an entry ITEM to the database, appending PATH to the
current documentation path."
  (let ((name (format "%s" item)))
    `(puthash ,name (cons ,path js-lookup-path) js-lookup-db)))

(defmacro js-lookup/root (dir &rest body)
  "Append DIR to the currently established documentation
path. This path, `js-lookup-path' is initially the empty string."
  (declare (indent defun))
  `(let ((js-lookup-path (cons ,dir js-lookup-path))) ,@body))

(defmacro js-lookup/entries (&rest items)
  "Add a number of flat entries to the database on top of the current path."
  (declare (indent defun))
  (cons 'progn
        (loop for item in items
              for name = (format "%s" item)
              collect `(js-lookup/entry ,name ,name))))

(defmacro js-lookup/category (category &rest items)
  "Register CATEGORY in the database along with each of its ITEMS."
  (declare (indent defun))
  (let ((name (format "%s" category)))
    `(progn
       (js-lookup/entry ,name ,name)
       (js-lookup/root ,(concat name js-lookup-path-seperator)
         ,@(loop for item in items
                 for key = (format "%s%s%s" name
                                   js-lookup-category-seperator item)
                 for value = (remove ?_ (format "%s" item))
                 collect `(js-lookup/entry ,key ,value))))))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("(\\<\\(js-lookup/category\\)\\> +\\([^ ()]+\\)"
     (1 'font-lock-keyword-face)
     (2 'font-lock-variable-name-face))
    ("(\\<\\(js-lookup/root\\)\\>"
     (1 'font-lock-keyword-face))
    ("(\\<\\(js-lookup/entry\\)\\>"
     (1 'font-lock-keyword-face))
    ("(\\<\\(js-lookup/entries\\)\\>"
     (1 'font-lock-keyword-face))))

(provide 'js-lookup)

(eval-when (load eval)
  (load-file (expand-file-name "js-lookup-database.el" js-lookup-root)))

;;; js-lookup.el ends here
