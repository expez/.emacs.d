(require 'ert)
(require 's)
(require 'evil-smartparens)
(require 'evil-tests)

(defun evil-sp--enable (&rest _)
  (when evil-smartparens-mode
    (evil-smartparens-mode)))

(defun evil-sp--fail ()
  ;; don't error out during tests
  )

(advice-add 'evil-delete :before #'evil-sp--enable)
(advice-add 'evil-yank :before #'evil-sp--enable)
(advice-add 'evil-delete-line :before #'evil-sp--enable)
(advice-add 'evil-change-line :before #'evil-sp--enable)

(ert-deftest evil-sp-test-delete-word ()
  "Test `evil-delete-word'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([T]his)"
    ("dw" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-2-word ()
  "Test `evil-delete-word' with repeat count"
  :tags '(evil-sp count)
  (evil-test-buffer
    "([T]his)"
    ("2dw" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-WORD ()
  "Test `evil-delete-WORD'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([T]his)"
    ("dW" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-2-WORD ()
  "Test `evil-delete-WORD' with repeat count"
  :tags '(evil-sp count)
  (evil-test-buffer
    "([T]his)"
    ("2dW" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-2-WORD-again ()
  "Test `evil-delete-WORD' with repeat count"
  :tags '(evil-sp count)
  (evil-test-buffer
    "([T]his that)"
    ("2dW" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-char ()
  "Test `evil-delete-char'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([x])"
    ("x" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-char-on-paren ()
  "Test `evil-delete-char' on paren"
  :tags '(evil-sp)
  (evil-test-buffer
    "[(])"
    ("x" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-backward-char ()
  "Test `evil-delete-backward-char'."
  :tags '(evil-sp)
  (evil-test-buffer
    "([x])"
    ("X" [escape])
    "(x)"))

(ert-deftest evil-sp-test-delete-backward-char-on-paren ()
  "Test `evil-delete-backward-char' on paren"
  :tags '(evil-sp)
  (evil-test-buffer
    "([)]"
    ("X" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-unbalanced-region-dwim ()
  "Test `evil-delete' with an unbalanced visual selection"
  :tags '(evil-sp)
  (evil-test-buffer
    "(<funcall with some args)>"
    ("d" [escape])
    "()"))

(ert-deftest evil-sp-test-delete-unbalanced-region-dwim-on-let ()
  "Test `evil-delete' with an unbalanced visual selection"
  :tags '(evil-sp)
  (evil-test-buffer
    "(let [foo (bar baz)
           <qux 1
           quux (+ 1 2)]>
       (dwim foo qux quux))"
    ("d" [escape])
    "(let [foo (bar baz)
           ]>
       (dwim foo qux quux))"))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-kills-garb ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(foo bar)[]     ; Useless comment"
    ("D" [escape])
    "(foo bar)"))

(ert-deftest evil-sp-test-delete-line-is-greedy ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "[(]let [foo (bar baz)
           qux 1
           quux (+ 1 2)]
       (dwim foo qux quux))"
    ("D" [escape])
    ""))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-preserves-comments ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "([f]oo bar)     ; Useless comment"
    ("D" [escape])
    "()     ; Useless comment"))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-deletes-useless-line ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "[(]foo bar)     ; Useless line"
    ("D" [escape])
    ""))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-works-in-string ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(foo \"bar baz\"
           quux)"
    ("D" [escape])
    "(foo \"\"
           quux)"))

(ert-deftest evil-sp-test-delete-line-is-sp-kill-sexp-works-in-latex-tags ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "foo ${bar[ ]baz} quux"
    ("D" [escape])
    "foo ${bar} quux"))

(ert-deftest evil-sp-test-delete-whole-line-is-greedy ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(let ((foo bar)
       (f[r]obnicate bar)))"
    ("dd" [escape])
    "(let ((foo bar)
))"))

(ert-deftest evil-sp-test-delete-whole-line-fails-when-greed-is-futile ()
  "Test `evil-delete-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(let [(](foo bar)
       (frobnicate bar)))"
    ("dd" [escape])
    "(let ((foo bar)
       (frobnicate bar)))"))

(ert-deftest evil-sp-test-change-whole-line-is-greedy ()
  "Test `evil-change-line'"
  :tags '(evil-sp)
  (evil-test-buffer
    "(let ((foo bar)
       (f[r]obnicate bar)))"
    ("dd" [escape])
    "(let ((foo bar)
))"))