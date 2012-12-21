;;(Load-theme 'Darkerthanblack t)
;(load-theme 'tomorrow-night-bright t)
(add-hook 'after-make-frame-functions
          '(lambda (f)
             (with-selected-frame f
               (when (window-system f) (color-theme-solarized-dark)))))
;(load-theme 'solarized-dark t)

(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0               ;; ... are very ...
  scroll-down-aggressively 0             ;; ... annoying
  scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v

;;Settings for uniquify
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;; re-builder
(setq reb-re-syntax 'string)

;; Treat 'y' or <CR> as yes, 'n' as no.
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)
(define-key query-replace-map [?\C-m] 'act)

;; Line numbering
(global-linum-mode 1)

(setq compilation-ask-about-save nil)
;;compile window smaller:
(setq compilation-window-height 30) ;;Not entirely sure I like this.
;;Close compilation window if compile was succesful.
(setq compilation-finish-function
      (lambda (buf str)

        (if (string-match "exited abnormally" str)
            ;;there were errors
            (message "Compilation errors, press C-c n to visit")

          ;;no errors, make the compilation window go away in 2 second
          (run-at-time 1 nil 'delete-windows-on buf)
          ;;(run-at-time 1 nil 'kill-buffer buf)

          (message "Compilation succesful!"))))

;;Trailing whitespace is unnecessary.
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(setq require-final-newline 'visit-save);;Add newline at the end of files.

;;Enable trash-can
(setq delete-by-moving-to-trash t)

;; Windmove, real hackers don't use arrows to navigate with, but might use them
;; to move between buffers!
(windmove-default-keybindings 'shift) ;; use shift-arrows to move between buffers.

(show-paren-mode 1) ;; highlight parenthesis.
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis) ;; highlight parenthesis, 'expression would only highlight entire exp within paren.

;; Set parenface
(set-face-foreground 'paren-face "grey30")

;; Initial message for scratch buffer.
(setq initial-scratch-message
  ";; scratch buffer created -- happy hacking\n")

;; Cua mode is only used for the great rectangle support.
(setq cua-enable-cua-keys nil)
(cua-mode 1)

(global-font-lock-mode 1) ;;Syntax highlighting.

;;Iswitchb mode
(iswitchb-mode 1)
(setq iswitchb-default-method 'samewindow) ;;Avoid switching to another frame.

(put 'set-goal-column 'disabled nil) ;; Enable set-goal-column

;; Enable IDO mode for buffers and files, enable flex matching.
(ido-mode 'both)
(setq ido-enable-flex-matching t)
(ido-everywhere 1)

;; No startup message.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Remove menu bar, tool bar and scroll bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;see what you are typing
(setq echo-keystrokes 0.1)

;;Set the font I like, Inconsolata. If on windows set Consolas.

(if (eq system-type 'windows-nt)
    (set-default-font
     "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

(if (eq system-type 'gnu/linux)
    (set-default-font "Inconsolata-12"))

;; Would call Windows command interpreter. Change it.

;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)

;; Remove C-m (^M) characters that appear in output

(add-hook 'comint-output-filter-functions
'comint-strip-ctrl-m)

;;Upcase region is occasionally useful!
(put 'upcase-region 'disabled nil)

;; Whole line or region mode
(whole-line-or-region-mode 1)

;;enable another previous disabled function.
(put 'downcase-region 'disabled nil)

;; Turn on debugging to get stacktrace if something goes wrong.
(setq debug-on-error t)

;; Store auto-save files to system's temp directory.
(setq backup-directory-alist
     `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
     `((".*" ,temporary-file-directory t)))

(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; Paredit mode
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))

(defadvice he-substitute-string (after he-paredit-fix)
"remove extra paren when expanding line in paredit"
(if (and paredit-mode (equal (substring str -1) ")"))
    (progn (backward-delete-char 1) (forward-char))))

;;Turn on the undo tree.
(global-undo-tree-mode 1)

;;Use Emacs-w3m
;; (setq w3m-use-cookies t)
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (setq w3m-default-display-inline-image t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "conkeror")

;;Yasnippet
(yas-global-mode 1)
(setq yas-trigger-key nil)
(yas/reload-all) ;; Needed to disable trigger key
(setq yas/root-directory "~/.emacs.d/mysnippets")
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

;;Enable images
(auto-image-file-mode 1)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
"Prevent annoying \"Active processes exist\" query when you quit Emacs."
(flet ((process-list ())) ad-do-it))

;;Prevent flyspell from spamming.
(setq flyspell-issue-message-flag nil)

(setq ispell-dictionary "english")
;;Make sure ispell process is started in home directory to use personal dictionaries.
(setq ispell-process-directory (expand-file-name "~/"))
(setq ispell-program-name "aspell") ;;Use aspell dictionaries.
(setq ispell-list-command "list") ;;Faster region checking, "list" for aspell, "-1" for ispell.

;;Ultra twice as slow as ispell, fast twice as slow as ultra, normal 10x slower than fast.
;;(setq ispell-extra-args '("--sug-mode=ultra"))

;;Ediff setup
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ;;Don't want the control frame.
(setq diff-switches "-u") ;;Use unified format
(setq ediff-custom-diff-options "-U3") ;;3 lines of context.

;;Save window configuration prior to ediff, so we can jump to it from ediff if needed,
;;restore the previous window configuration when ediff terminates.
;;Taken from emacswiki/ediffmode
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
"*Register to be set up to hold `my-ediff-bwin-config'
configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
"*Register to be used to hold `my-ediff-awin-config' window
configuration.")

(defun my-ediff-bsh ()
"Function to be called before any buffers or window setup for
ediff."
(setq my-ediff-bwin-config (current-window-configuration))
(when (characterp my-ediff-bwin-reg)
(set-register my-ediff-bwin-reg
(list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
"Function to be called after buffers and window setup for ediff."
(setq my-ediff-awin-config (current-window-configuration))
(when (characterp my-ediff-awin-reg)
(set-register my-ediff-awin-reg
(list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
"Function to be called when ediff quits."
(when my-ediff-bwin-config
(set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)
(add-hook 'ediff-cleanup-hook (lambda () (ediff-janitor nil nil)))

;; Workgroups
(workgroups-mode 1)
(setq wg-prefix-key  (kbd "C-x w"))

;;Don't open a new buffer for each opened directory in Dired.
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-copies 'always);;always recursively copy.

(electric-pair-mode 1)

(defun my-make-CR-do-indent ()
(define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

;;Org-mode

(setq org-src-fontify-natively t) ;; syntax hightlighting when inserting code.)


(defun my-org-mode-hook ()
  (local-set-key (kbd "C-,") 'beginning-of-buffer))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;;AucTeX config

(setq TeX-autosave t
TeX-parse-self t
TeX-PDF-mode t
refTeX-plug-into-AUCTeX t)
(setq-default text-master nil)
;;(add-hook 'LaTeX-mode-hook (setq TeX-master (guess-tex-master (buffer-file-name)))) ;;Guess which  file is the master.


(defun my-LaTeX-mode-hook ()
  (visual-line-mode 1)
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (TeX-source-correlate-mode 1)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (local-set-key (kbd "C-x c") 'TeX-command-master)
  (local-set-key (kbd "C-c s") 'LaTeX-section)
  (local-set-key (kbd "C-c e") 'LaTeX-environment)
  (local-set-key (kbd "C-c i") 'LaTeX-insert-item)
  (local-set-key (kbd "C-c f") 'TeX-font)
  (local-set-key (kbd "C-,") 'beginning-of-buffer))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

(add-hook 'ace-jump-mode-before-jump-hook
          (lambda () (push-mark (point) t))) ;until it's fixed in Maramalade

;;Haskell mode
(defun haskell-style ()
  "Sets the current buffer to use Haskell Style. Meant to be
  added to `haskell-mode-hook'"
  (interactive)
  (setq tab-width 4
        haskell-indentation-layout-offset 4
        haskell-indentation-left-offset 4
        haskell-indentation-ifte-offset 4
        haskell-hoogle-command nil))
;; haskell-hoogle-command to "hoogle" uses local command line hoogle (cabal install hoogle)
;; setting this variable to "nil" would use haskell.org/hoogle in browser.

(defun my-haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-simple-indent)
  (turn-on-haskell-font-lock)
  (haskell-style)
  (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
  (define-key haskell-mode-map "\C-cai" 'haskell-align-imports)

  (define-key haskell-mode-map (kbd "C-<left>")
    (lambda ()
      (interactive)
      (haskell-move-nested -1)))

  (define-key haskell-mode-map (kbd "C-<right>")
    (lambda ()
      (interactive)
      (haskell-move-nested 1)))

  (setq default-hpaste-nick "Expez"))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun hasktags ()
  "regenerate TAGS file using hasktags in the project root (found by TAGS file)"
  (if (eq major-mode 'haskell-mode)
      (start-process "*generate-hasktags*" "*generate-hasktags*" "generate-hasktags.sh")))

(add-hook 'after-save-hook 'hasktags)
(setq tags-revert-without-query 1)

(add-hook 'haskell-mode-hook
	  (lambda ()
         (auto-complete-mode 1)
	     (make-local-variable 'ac-sources)
	     (setq ac-sources '(ac-source-abbrev
                            ac-source-words-in-buffer
                            my/ac-source-haskell))
	     nil))

(setq electric-pair-pairs '(
                (?\" . ?\")
                (?\{ . ?\})
                (?\[ . ?\])))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Auto-complete
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(defun ac-c-mode-setup ()
  (setq clang-complete-executable "~/.emacs.d/plugins/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (launch-completion-proc))

(defun my-ac-config ()
  (add-hook 'c-mode-hook 'ac-c-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t)
  (setq ac-auto-start 2)
  (setq ac-quick-help-delay 0.5)
  (setq ac-auto-show-menu 0.2)
  (setq ac-fuzzy-enable t)
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (ac-flyspell-workaround))

(my-ac-config)

;; Following AC hack is taken from http://madscientist.jp/~ikegami/diary/20090215.html#p01
(defconst my/haskell-reserved-keywords
  (sort
   (list "case" "class" "data" "default" "deriving" "do" "else" "if" "import" "in" "infix"
   "infixl" "infixr" "instance" "let" "module" "newtype" "of" "then" "type" "where" "as"
   "qualified" "hiding")
   #'(lambda (a b) (> (length a) (length b))))
  "Reserved keywords in Haskell.")

(defconst my/haskell-defined-types
  (sort
   (list "Bool" "False" "True" "Char" "String" "IO" "IOError" "Maybe" "Just" "Nothing"
   "Either" "Right" "Left" "Ordering" "LT" "EQ" "GT" "Integer" "Int" "Ratio" "Float"
   "Double" "Complex")
   #'(lambda (a b) (> (length a) (length b))))
  "Defined types in Haskell.")

(defconst my/haskell-defined-classes
  (sort
   (list "Eq" "==" "/=" "Ord" "compare" "max" "min" "<=" ">=" "ReadS" "ShowS" "Read"
   "read" "readsPrec" "readList" "showsPrec" "show" "showList" "Enum" "succ" "toEnum"
   "fromEnum" "enumFrom" "enumFromThen" "enumFromTo" "enumFromThenTo" "Functor" "fmap"
   "Monad" ">>=" ">>" "return" "fail" "Bounded" "minBound" "maxBound" "Num" "negate" "abs"
   "signum" "fromInteger" "Real" "toRational" "Integral" "quot" "rem" "div" "mod"
   "quotRem" "divMod" "toInteger" "Fractional" "recip" "fromRational" "Floating" "pi"
   "exp" "log" "sqrt" "**" "logBase" "sin" "cos" "tan" "asin" "acos" "atan" "sinh" "cosh"
   "tanh" "asinh" "acosh" "atanh" "RealFrac" "properFraction" "truncate" "ceiling" "floor"
   "RealFloat" "floatRadix" "floatDigits" "floatRange" "decodeFloat" "encodeFloat"
   "exponent" "significand" "scaleFloat" "isNan" "isInfinite" "isDenormalized"
   "isNegativeZero" "isIEEE" "atan2" "gcd" "lcm" "^^" "fromIntegral" "realtoFrac")
   #'(lambda (a b) (> (length a) (length b))))
  "Defined classes in Haskell.")

(defconst my/haskell-prelude-functions
  (sort
   (list ; "&&" "||"
         "not" "otherwise" "maybe" "either" "fst" "snd" "curry" "uncurry" "pred"
	 "round" "subtract" "odd" "mapM" "mapM_" "sequence" "sequence_" "=<<" "id" "const"
	 "flip" "until" "asTypeOf" "error" "undefined" "$!" "seq" "map" "++" "filter"
	 "head" "last" "tail" "init" "null" "length" "!!" "reverse" "fold" "fold1" "foldr"
	 "foldr1" "and" "or" "any" "all" "sum" "product" "concat" "concatMap" "maximum"
	 "minimum" "scanl" "scanl1" "scanr" "scanr1" "iterate" "repeat" "replicate"
	 "cycle" "take" "drop" "splitAt" "takeWhile" "dropWhile" "span" "break" "elem"
	 "notElem" "lookup" "zip" "zip3" "zipWith" "zipWith3" "unzip" "unzip3" "lines"
	 "words" "unlines" "unwords" "shows" "showChar" "showString" "showParen" "reads"
	 "readParen" "lex" "putChar" "putStr" "putStrLn" "print" "getChar" "getLine"
	 "getContents" "intract" "FilePath" "readFile" "writeFile" "appendFile" "readIO"
	 "readLn" "IOException" "ioError" "userError" "catch")
   #'(lambda (a b) (> (length a) (length b))))
  "Defined functions in GHC Prelude.")

(defconst my/haskell-ghc-modules
  (sort
   (list
   "Control.Applicative" "Control.Arrow" "Control.Category" "Control.Concurrent"
   "Control.Concurrent.MVar" "Control.Concurrent.QSem" "Control.Concurrent.QSemN"
   "Control.Concurrent.STM" "Control.Concurrent.STM.TArray" "Control.Concurrent.STM.TChan"
   "Control.Concurrent.STM.TMVar" "Control.Concurrent.STM.TVar"
   "Control.Concurrent.SampleVar" "Control.Exception" "Control.Exception.Base"
   "Control.Monad" "Control.Monad.Cont" "Control.Monad.Cont.Class" "Control.Monad.Error"
   "Control.Monad.Error.Class" "Control.Monad.Fix" "Control.Monad.Identity"
   "Control.Monad.Instances" "Control.Monad.List" "Control.Monad.RWS"
   "Control.Monad.RWS.Class" "Control.Monad.RWS.Lazy" "Control.Monad.RWS.Strict"
   "Control.Monad.Reader" "Control.Monad.Reader.Class" "Control.Monad.ST"
   "Control.Monad.ST.Lazy" "Control.Monad.ST.Strict" "Control.Monad.STM"
   "Control.Monad.State" "Control.Monad.State.Class" "Control.Monad.State.Lazy"
   "Control.Monad.State.Strict" "Control.Monad.Trans" "Control.Monad.Writer"
   "Control.Monad.Writer.Class" "Control.Monad.Writer.Lazy" "Control.Monad.Writer.Strict"
   "Control.OldException" "Control.Parallel" "Control.Parallel.Strategies"
   "Data.Array" "Data.Array.Diff" "Data.Array.IArray" "Data.Array.IO"
   "Data.Array.IO.Internals" "Data.Array.MArray" "Data.Array.Paralell"
   "Data.Array.Paralell.Arr" "Data.Array.Paralell.Base" "Data.Array.Paralell.Lifted"
   "Data.Array.Paralell.PArray" "Data.Array.Paralell.Prelude"
   "Data.Array.Paralell.Prelude.Double" "Data.Array.Paralell.Int"
   "Data.Array.Paralell.Word8" "Data.Array.Paralell.Stream" "Data.Array.Paralell.Unlifted"
   "Data.Array.Paralell.Unlifted.Distributed" "Data.Array.Paralell.Unlifted.Paralell"
   "Data.Array.Paralell.Unlifted.Sqeuential" "Data.Array.ST" "Data.Array.Storable"
   "Data.Array.Unboxed" "Data.Bits" "Data.Bool" "Data.ByteString" "Data.ByteString.Char8"
   "Data.ByteString.Fusion" "Data.ByteString.Internal" "Data.ByteString.Lazy"
   "Data.ByteString.Lazy.Char8" "Data.ByteString.Lazy.Fusion"
   "Data.ByteString.Lazy.Internal" "Data.ByteString.Unsafe" "Data.Char" "Data.Complex"
   "Data.Data" "Data.Dynamic" "Data.Either" "Data.Eq" "Data.Fixed" "Data.Foldable"
   "Data.Function" "Data.Generics" "Data.Generics.Aliases" "Data.Generics.Basics"
   "Data.Generics.Instances" "Data.Generics.Schemes" "Data.Generics.Text"
   "Data.Generics.Twins" "Data.Graph" "Data.HashTable" "Data.IORef" "Data.Int"
   "Data.IntMap" "Data.IntSet" "Data.Ix" "Data.List" "Data.Map" "Data.Maybe" "Data.Monoid"
   "Data.Ord" "Data.Ratio" "Data.STRef" "Data.STRef.Lazy" "Data.STRef.Strict"
   "Data.Sequence" "Data.Set" "Data.String" "Data.Time" "Data.Time.Calendar"
   "Data.Time.Calendar.Easter" "Data.Time.Calendar.Julian" "Data.Time.Calendar.MonthDay"
   "Data.Time.Calendar.OrdinalDate" "Data.Time.Calendar.WeekDate" "Data.Time.Clock"
   "Data.Time.Clock.POSIX" "Data.Time.Clock.TAI" "Data.Time.Format" "Data.Time.LocalTime"
   "Data.Traversable" "Data.Tree" "Data.Tuple" "Data.Typeable" "Data.Unique"
   "Data.Version" "Data.Word" "Debug.Trace"
   "Distribution.Compat.ReadP" "Distribution.Compiler" "Distribution.InstalledPackageInfo"
   "Distribution.License" "Distribution.Make" "Distribution.ModuleName"
   "Distribution.Package" "Distribution.PackageDescription"
   "Distribution.PackageDescription.Check" "Distribution.PackageDescription.Configuration"
   "Distribution.PackageDescription.Parse" "Distribution.ParseUtils" "Distribution.ReadE"
   "Distribution.Simple" "Distribution.Simple.Build" "Distribution.Simple.Build.Macros"
   "Distribution.Simple.Build.PathsModule" "Distribution.Simple.BuildPaths"
   "Distribution.Simple.Command" "Distribution.Simple.Compiler"
   "Distribution.Simple.Configure" "Distribution.Simple.GHC" "Distribution.Simple.Haddock"
   "Distribution.Simple.Hugs" "Distribution.Simple.Install"
   "Distribution.Simple.InstallDirs" "Distribution.Simple.JHC"
   "Distribution.Simple.LocalBuildInfo" "Distribution.Simple.NHC"
   "Distribution.Simple.PackageIndex" "Distribution.Simple.PreProcess"
   "Distribution.Simple.PreProcess.Unlit" "Distribution.Simple.Program"
   "Distribution.Simple.Register" "Distribution.Simple.Setup"
   "Distribution.Simple.SrcDist" "Distribution.Simple.UserHooks"
   "Distribution.Simple.Utils" "Distribution.System" "Distribution.Text"
   "Distribution.Verbosity" "Distribution.Version"
   "Foreign" "Foreign.C" "Foreign.C.Error" "Foreign.C.String" "Foreign.C.Types"
   "Foreign.Concurrent" "Foreign.ForeignPtr" "Foreign.Marshal" "Foreign.Marshal.Alloc"
   "Foreign.Marshal.Array" "Foreign.Marshal.Error" "Foreign.Marshal.Pool"
   "Foreign.Marshal.Utils" "Foreign.Ptr" "Foreign.StablePtr" "Foreign.Storable"
   "GHC.Arr" "GHC.Bool" "GHC.Conc" "GHC.ConsoleHandler" "GHC.Desugar" "GHC.Environment"
   "GHC.Err" "GHC.Exts" "GHC.Generics" "GHC.Handle" "GHC.Ordering" "GHC.PArr" "GHC.Prim"
   "GHC.PrimopWrappers" "GHC.Tuple" "GHC.Types" "GHC.Unicode" "GHC.Unit"
   "Language.Haskell.Extension" "Language.Haskell.Lexer" "Language.Haskell.ParseMonad"
   "Language.Haskell.ParseUtils" "Language.Haskell.Parser" "Language.Haskell.Pretty"
   "Language.Haskell.Syntax" "Language.Haskell.TH" "Language.Haskell.TH.Lib"
   "Language.Haskell.TH.Ppr" "Language.Haskell.TH.PprLib" "Language.Haskell.TH.Quote"
   "Language.Haskell.TH.Syntax"
   "Network" "Network.BSD" "Network.Socket" "Network.URI" "Numeric"
   "Prelude"
   "System.CPUTime" "System.Cmd" "System.Console.Editline" "System.Console.Readline"
   "System.Console.GetOpt" "System.Directory" "System.Environment" "System.Exit"
   "System.FilePath" "System.FilePath.Posix" "System.FilePath.Windows" "System.IO"
   "System.IO.Error" "System.IO.Unsafe" "System.Info" "System.Locale" "System.Mem"
   "System.Mem.StableName" "System.Mem.Weak" "System.Posix" "System.Posix.Directory"
   "System.Posix.DynamicLinker" "System.Posix.DynamicLinker.Module"
   "System.Posix.DynamicLinker.Prim" "System.Posix.Env" "System.Posix.Error"
   "System.Posix.Files" "System.Posix.IO" "System.Posix.Process"
   "System.Posix.Process.Internals" "System.Posix.Resource" "System.Posix.Semaphore"
   "System.Posix.SharedMem" "System.Posix.Signals" "System.Posix.Signals.Exts"
   "System.Posix.Temp" "System.Posix.Terminal" "System.Posix.Time" "System.Posix.Types"
   "System.Posix.Unistd" "System.Posix.User" "System.Process" "System.Random"
   "System.Time" "System.Timeout"
   "Test.HUnit" "Test.HUnit.Base" "Test.HUnit.Lang" "Test.HUnit.Terminal"
   "Test.HUnit.Text" "Test.QuickCheck" "Test.QuickCheck.Batch" "Test.QuickCheck.Poly"
   "Test.QuickCheck.Utils" "Text.Html" "Text.Html.BlockTable"
   "Text.ParserCombinators.Parsec" "Text.ParserCombinators.Parsec.Char"
   "Text.ParserCombinators.Parsec.Combinator" "Text.ParserCombinators.Parsec.Error"
   "Text.ParserCombinators.Parsec.Expr" "Text.ParserCombinators.Parsec.Language"
   "Text.ParserCombinators.Parsec.Perm" "Text.ParserCombinators.Parsec.Pos"
   "Text.ParserCombinators.Parsec.Prim" "Text.ParserCombinators.Parsec.Token"
   "Text.ParserCombinators.ReadP" "Text.ParserCombinators.ReadPrec" "Text.PrettyPrint"
   "Text.PrettyPrint.HughesPJ" "Text.Printf" "Text.Read" "Text.Read.Lex" "Text.Regex.Base"
   "Text.Regex.Base.Context" "Text.Regex.Base.Impl" "Text.Regex.Base.RegexLike"
   "Text.Regex.Posix" "Text.Regex.Posix.ByteString" "Text.Regex.Posix.String"
   "Text.Regex.Posix.Wrap" "Text.Show" "Text.Show.Functions" "Text.XHtml"
   "Text.XHtml.Debug" "Text.XHtml.Frameset" "Text.XHtml.Strict" "Text.XHtml.Table"
   "Text.XHtml.Transitional" "Trace.Hpc.Mix" "Trace.Hpc.Reflect" "Trace.Hpc.Tix"
   "Trace.Hpc.Util"
   "Unsafe.Coerce") #'(lambda (a b) (> (length a) (length b))))
  "GHC modules.")

;; see also the latest GHC manual
;; http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html
(defconst my/haskell-ghc-pragmas
  (sort
   (list "LANGUAGE" "OPTIONS_GHC" "INCLUDE" "WARNING" "DEPRECATED" "INLINE" "NOINLINE"
   "LINE" "RULES" "SPECIALIZE" "UNPACK" "SOURCE")
   #'(lambda (a b) (> (length a) (length b))))
  "GHC pragmas.")

;; see also the latest GHC manual
;; http://www.haskell.org/ghc/docs/latest/html/users_guide/flag-reference.html#id2631364
(defvar my/haskell-ghc-options
  (list "OverlappingInstances" "IncoherentInstances" "UndecidableInstances" "Arrows"
	"ForeignFunctionInterface" "Generics" "ImplicitParams" "ImplicitPrelude"
	"MonomorphismRestriction" "MonoPatBinds" "RelaxedPolyRec" "ExtendedDefaultRules"
	"OverloadedStrings" "GADTs" "TypeFamilies" "ScopedTypeVariables" "TemplateHaskell"
	"QuasiQuotes" "BangPatterns" "CPP" "PatternGuards" "ViewPatterns" "UnicodeSyntax"
	"MagicHash" "NewQualifiedOperators" "PolymorphicComponents" "Rank2Types"
	"RankNTypes" "ImpredicativeTypes" "ExistentialQuantification" "KindSignatures"
	"EmptyDataDecls" "ParallelListComp" "TransformListComp" "UnliftedFFITypes"
	"LiberalTypeSynonyms" "TypeOperators" "RecursiveDo" "PArr" "RecordWildCards"
	"NamedFieldPuns" "DisambiguateRecordFields" "UnboxedTuples" "StandaloneDeriving"
	"DeriveDataTypeable" "GeneralizedNewtypeDeriving" "TypeSynonymInstances"
	"FlexibleContexts" "FlexibleInstances" "ConstrainedClassMethods"
	"MultiParamTypeClasses" "FunctionnalDependencies" "PackageImports"))

(defvar my/haskell-ghc-no-options
      (mapcar '(lambda (n) (concat "No" n)) my/haskell-ghc-options))

(defvar my/haskell-ghc-language-options
  (sort (append nil my/haskell-ghc-options my/haskell-ghc-no-options)
	#'(lambda (a b) (> (length a) (length b))))
  "GHC Language options.")

(defvar my/ac-source-haskell
  '((candidates
     . (lambda ()
	 (all-completions ac-target
			  (append nil
				  my/haskell-defined-types
				  my/haskell-defined-classes
				  my/haskell-reserved-keywords
				  my/haskell-prelude-functions
				  my/haskell-ghc-modules
				  my/haskell-ghc-pragmas
				  my/haskell-ghc-language-options
				  '("-fglasgow-exts")
				  )))))
  "Sources for Haskell keywords.")

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

(column-number-mode 1)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(add-to-list 'auto-mode-alist '("\\.cmd\\'" . ntcmd-mode))

;;Evil mode
;;(evil-mode 1)
;;(setq evil-default-cursor t)
;;(setq evil-want-visual-char-semi-exclusive t)
;;(global-surround-mode 1)
;;(setq evil-normal-state-cursor 'hollow)
;;(setq evil-insert-state-cursor '("red" hbar))

(key-chord-mode 1)
 ;;(setq recentf-auto-cleanup 'never) ;; disable before we start recentf! If using Tramp a lot.
(recentf-mode t)
(setq recentf-max-saved-items 100)

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(ido-hacks-mode 1)

(turn-on-ex-mode)

(defun give-my-keybindings-priority ()
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'ex-mode))
      (let ((mykeys (assq 'ex-mode minor-mode-map-alist)))
        (assq-delete-all 'ex-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'buffer-list-update-hook 'give-my-keybindings-priority)

(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'server-done-hook (lambda nil (kill-buffer nil)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; cc-mode
(c-add-style "ex"
             '("linux"
               (c-offsets-alist
                (c-basic-offset . 2))))

;; Eclim for java development
(add-hook 'java-mode-hook
	  '(lambda ()
         (eclim-mode 1)
         (ac-emacs-eclim-config)
         ;; Eclim uses help to display errors
         (setq help-at-pt-display-when-idle t)
         (setq eclimd-default-workspace "~/workspace")
         (setq eclim-executable "/usr/share/eclipse/eclim")
         (setq eclim-auto-save t)
         (setq eclim-print-debug-messages t)
         (local-set-key (kbd "M-/") 'eclim-ac-complete)
         (setq help-at-pt-timer-delay 0.1)
         (help-at-pt-set-timer)
         (java-mode-indent-annotations-setup)
         (custom-set-variables
          '(eclim-eclipse-dirs '("/usr/share/eclipse")))
         (setq c-basic-offset 4)))

(setq shift-select-mode nil)
;; Cua mode re-actives shift selection, this function fixes that issue.
(ergoemacs-fix-cua--pre-command-handler-1)

(setq tramp-default-method "ssh")

(setq mediawiki-mode-hook
      (lambda ()
        (visual-line-mode 1)
        (turn-off-auto-fill)
        (define-key mediawiki-mode-map (kbd "C-c o") 'mediawiki-browse)
        (define-key mediawiki-mode-map (kbd "C-c g") 'mediawiki-reload)
        (define-key mediawiki-mode-map (kbd "C-c <ret>") 'mediawiki-open-page-at-point)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-h") 'mediawiki-insert-header)
        (define-key mediawiki-mode-map (kbd "C-c C-f C-e") 'mediawiki-insert-sub-header)))

(setq enable-recursive-minibuffers t)

(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode +1)
            (slime-setup '(slime-fancy))
            (slime-mode 1)
            (turn-on-eldoc-mode)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)
            (rainbow-delimiters-mode 0)
            (local-set-key (kbd "C-w") 'paredit-backward-kill-word)
            (local-set-key (kbd "M-J") 'paredit-backward)
            (local-set-key (kbd "M-L") 'paredit-forward)
            (local-set-key (kbd "M-H") 'paredit-splice-sexp)
            (set-face-foreground 'paren-face "grey30")))

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-mode-hook 'cliki:start-slime)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
'(add-to-list 'ac-modes 'slime-repl-mode))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; The SBCL binary and command-line arguments
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))
