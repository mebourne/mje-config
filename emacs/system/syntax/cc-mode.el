;; Emacs configuration file
;; Syntax colouring regular expressions for cc-mode
;; Written by Martin Ebourne

;; C/C++/Java
(defvar c-font-lock-keywords-local nil)
(defvar c++-font-lock-keywords-local nil)
(defvar java-font-lock-keywords-local nil)
(defvar c-font-lock-keywords-local-slower nil)
(defvar c++-font-lock-keywords-local-slower nil)
(let ((c-keywords
       (eval-when-compile (regexp-opt
			   '("auto" "break" "case" "char" "const" "continue"
			     "default" "do" "double" "else" "enum" "extern"
			     "float" "for" "goto" "if" "int" "long" "register"
			     "return" "short" "signed" "sizeof" "static"
			     "struct" "switch" "typedef" "union" "unsigned"
			     "void" "volatile" "while"
			     ))))
      (c++-keywords
       (eval-when-compile (regexp-opt
			   '("and" "and_eq" "asm" "auto" "bitand" "bitor"
			     "bool" "break" "case" "catch" "char" "class"
			     "compl" "const" "const_cast" "continue" "default"
			     "delete" "do" "double" "dynamic_cast" "else"
			     "enum" "explicit" "export" "extern" "false" "false" "float"
			     "for" "friend" "goto" "if" "inline" "int" "long"
			     "mutable" "namespace" "new" "not" "not_eq"
			     "operator" "or" "or_eq" "private" "protected"
			     "public" "register" "reinterpret_cast" "return"
			     "short" "signed" "sizeof" "static" "static_cast" "struct"
			     "switch" "template" "this" "throw" "true" "try"
			     "typedef" "typeid" "typename" "union" "unsigned"
			     "using" "virtual" "void" "volatile" "wchar_t"
			     "while" "xor" "xor_eq"
			     ))))
      (java-keywords
       (eval-when-compile (regexp-opt
			   '("abstract" "break" "case" "catch"
			     "class" "const" "continue" "default" "do"
			     "else" "extends" "false" "final" "finally"
			     "for" "goto" "if" "implements" "import" "instanceof"
			     "interface" "native" "new" "null" "package"
			     "private" "protected" "public" "return" "static" "strictfp"
			     "super" "switch" "synchronized" "this" "throw" "throws"
			     "transient" "true" "try" "volatile" "while"
			     ))))
      (java-type-names
       (mapconcat 'identity
		  (cons 
		   (eval-when-compile
		     (regexp-opt '("boolean" "byte" "char" "double" "float" "int"
				   "long" "short" "void")))
		   java-font-lock-extra-types)
		  "\\|"))
      (ctoken "[a-zA-Z_][a-zA-Z0-9_]*")
      (cendwordl "\\(^\\|[^a-zA-Z0-9_]\\)")
      (cendwordr "\\($\\|[^a-zA-Z0-9_]\\)")
      )

;;;
;;; This is less accurate but faster than the later alternative. Active
;;;

  (setq c-font-lock-keywords-local
	(list
	 ;;
	 ;; Fontify filenames in #include <...> preprocessor directives.
	 '("^[ \t]*#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
	 ;;
	 ;; Fontify function macro names.
	 (cons (concat "^[ \t]*#[ \t]*define[ \t]+\\(" ctoken "\\)(") '(1 font-lock-function-name-face))
	 ;;
	 ;; Fontify otherwise as symbol names, and the preprocessor directive names.
	 (list (concat "^[ \t]*\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(" ctoken "\\)?")
	       '(1 font-lock-constant-face) '(2 font-lock-variable-name-face nil t))
	 ;;
	 ;; Fontify all builtin keywords (except case, default and goto; see below).
	 (cons (concat "\\<\\(" c-keywords "\\)\\>") 'font-lock-keyword-face)
	 ;;
	 ;; Fontify numbers
	 (list (concat "\\<\\([0-9][0-9A-Fa-fXxUuLlEe]*\\)\\>") 1 'font-lock-number-face)
	 ;;
	 ;; Fontify function names
	 (list (concat "\\<\\(" ctoken "\\)[ \t]*(") 1 'font-lock-function-name-face)
	 ;;
	 ;; Fontify variable names
	 (list (concat "\\<\\(" ctoken "\\)\\>") 1 'font-lock-variable-name-face)
	 ))
  (setq c++-font-lock-keywords-local
	(list
	 ;;
	 ;; Fontify Rose comments darker.
	 ;'("\\(//\\#\\#.*$\\)" (1 font-lock-rose-comment-face t))
	 ;;
	 ;; Fontify filenames in #include <...> preprocessor directives.
	 '("^[ \t]*#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
	 ;;
	 ;; Fontify function macro names.
	 (cons (concat "^[ \t]*#[ \t]*define[ \t]+\\(" ctoken "\\)(") '(1 font-lock-function-name-face))
	 ;;
	 ;; Fontify otherwise as symbol names, and the preprocessor directive names.
	 (list (concat "^[ \t]*\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(" ctoken "\\)?")
	       '(1 font-lock-constant-face) '(2 font-lock-variable-name-face nil t))
	 ;;
	 ;; Fontify all builtin keywords (except case, default and goto; see below).
	 (cons (concat "\\<\\(" c++-keywords "\\)\\>") 'font-lock-keyword-face)
	 ;;
	 ;; Fontify numbers
	 (list (concat "\\<\\([0-9][0-9A-Fa-fXxUuLlEe]*\\)\\>") 1 'font-lock-number-face)
	 ;;
	 ;; Fontify function names
	 (list (concat "\\<\\(" ctoken "\\)[ \t]*(") 1 'font-lock-function-name-face)
	 ;;
	 ;; Fontify variable names
	 (list (concat "\\<\\(" ctoken "\\)\\>") 1 'font-lock-variable-name-face)
	 ))
  (setq java-font-lock-keywords-local
	(list
	 ;;
	 ;; Fontify Rose comments darker.
	 ;'("\\(//\\#\\#.*$\\)" (1 font-lock-rose-comment-face t))
	 ;;
	 ;; Fontify filenames in #include <...> preprocessor directives.
	 (list "\\<\\(import\\|package\\)\\>[ \t]*\\(\\sw+\\(\\.\\(\\*\\|\\sw+\\)\\)*\\)?"
	       '(1 font-lock-keyword-face) '(2 font-lock-constant-face nil t))
	 ;;
	 ;; Fontify all builtin keywords
	 (cons (concat "\\<\\(" java-keywords "\\)\\>") 'font-lock-keyword-face)
	 ;;
	 ;; Fontify all types
	 (cons (concat "\\<\\(" java-type-names "\\)\\>") 'font-lock-type-face)
	 ;;
	 ;; Fontify numbers
	 (list (concat "\\<\\([0-9][0-9A-Fa-fXxUuLlEe]*\\)\\>") 1 'font-lock-number-face)
	 ;;
	 ;; Fontify function names
	 (list (concat "\\<\\(" ctoken "\\)[ \t]*(") 1 'font-lock-function-name-face)
	 ;;
	 ;; Fontify variable names
	 (list (concat "\\<\\(" ctoken "\\)\\>") 1 'font-lock-variable-name-face)
	 ))

;;;
;;; This is more accurate than the above but too slow. Disabled
;;;

  (setq c-font-lock-keywords-local-slower
	(list
	 ;;
	 ;; Fontify filenames in #include <...> preprocessor directives.
	 '("^[ \t]*#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
	 ;;
	 ;; Fontify function macro names.
	 (cons (concat "^[ \t]*#[ \t]*define[ \t]+\\(" ctoken "\\)(") '(1 font-lock-function-name-face))
	 ;;
	 ;; Fontify otherwise as symbol names, and the preprocessor directive names.
	 (list (concat "^[ \t]*\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(" ctoken "\\)?")
	       '(1 font-lock-constant-face) '(2 font-lock-variable-name-face nil t))
	 ;;
	 ;; Fontify all builtin keywords
	 (cons (concat cendwordl "\\(" c-keywords "\\)" cendwordr) '(2 font-lock-keyword-face))
	 ;;
	 ;; Fontify numbers
	 (cons (concat cendwordl "\\([0-9][0-9A-Fa-fXxUuLlEe]*\\)" cendwordr) '(2 font-lock-number-face))
	 ;;
	 ;; Fontify function names
	 (cons (concat cendwordl "\\(" ctoken "\\)[ \t]*(") '(2 font-lock-function-name-face))
	 ;;
	 ;; Fontify variable names
	 (cons (concat cendwordl "\\(" ctoken "\\)") '(2 font-lock-variable-name-face))
	 ))
  (setq c++-font-lock-keywords-local-slower
	(list
	 ;;
	 ;; Fontify Rose comments darker.
	 '("\\(//\\#\\#.*$\\)" (1 font-lock-rose-comment-face t))
	 ;;
	 ;; Fontify filenames in #include <...> preprocessor directives.
	 '("^[ \t]*#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
	 ;;
	 ;; Fontify function macro names.
	 (cons (concat "^[ \t]*#[ \t]*define[ \t]+\\(" ctoken "\\)(") '(1 font-lock-function-name-face))
	 ;;
	 ;; Fontify otherwise as symbol names, and the preprocessor directive names.
	 (list (concat "^[ \t]*\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\(" ctoken "\\)?")
	       '(1 font-lock-constant-face) '(2 font-lock-variable-name-face nil t))
	 ;;
	 ;; Fontify function names that end with a keyword
	 (cons (concat "\\([a-zA-Z0-9_]+\\(" c++-keywords "\\)\\)[ \t]*(") '(1 font-lock-function-name-face))
	 ;;
	 ;; Fontify variable names that end with a keyword
	 (cons (concat "\\([a-zA-Z0-9_]+\\(" c++-keywords "\\)\\)" cendwordr) '(1 font-lock-variable-name-face))
	 ;;
	 ;; Fontify all builtin keywords
	 (cons (concat "\\(" c++-keywords "\\)" cendwordr) '(1 font-lock-keyword-face))
	 ;;
	 ;; Fontify function names
	 (cons (concat "\\(" ctoken "\\)[ \t]*(") '(1 font-lock-function-name-face))
	 ;;
	 ;; Fontify variable names
	 (cons (concat "\\(" ctoken "\\)") '(1 font-lock-variable-name-face))
	 ;;
	 ;; Fontify numbers
	 (cons (concat "\\([0-9][0-9A-Fa-fXxUuLlEe]*\\)") '(1 font-lock-number-face))
	 ))
  )

;; Setup font-lock mode
(add-hook 'c-mode-common-hook
	  (function
	   (lambda ()
	     (let ((keywords (c-mode-symbol "font-lock-keywords-local")))
	       (if (not (eq nil keywords))
		   (setcar font-lock-defaults keywords))))
	   ))
