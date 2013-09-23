;; Emacs configuration file
;; Syntax colouring regular expressions for asm-mode
;; Written by Martin Ebourne

(eval-after-load
 "asm-mode"
 '(font-lock-add-keywords
   'asm-mode
   (list
    ;; Assembler style comments
    ;;   '("^\#" . font-lock-comment-face)
    ;; Fontify filenames in #include <...> preprocessor directives.
    '("^[ \t]*#[ \t]*include[ \t]+\\(<[^>\"\n]+>\\)" 1 font-lock-string-face)
    ;; Fontify function macro names.
    '("^[ \t]*#[ \t]*define[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)(" 1
      font-lock-function-name-face)
    ;; Fontify otherwise as symbol names, and the preprocessor directive names.
    '("^[ \t]*\\(#[ \t]*[a-z]+\\)\\>[ \t]*\\([a-zA-Z_][a-zA-Z0-9_]*\\)?"
      (1 font-lock-constant-face) (2 font-lock-variable-name-face nil t))
    ;; C++ style comments (for when using C++ preprocessor)
    '("//.*" . font-lock-comment-face)
    ;; Fontify expression assignments
    '("\\(^\\|;\\|:\\)[ \t]*\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)[ \t]*=" 2
      font-lock-variable-name-face)
    ;; Instructions
    '("\\(^\\|;\\|:\\)[ \t]*\\([a-zA-Z_.][a-zA-Z0-9_.]*\\)[^:a-zA-Z0-9_$.]" 2
      font-lock-keyword-face)
    ;; Registers
    '("\\<\\(\\%[a-zA-Z0-9]+\\)\\>" . font-lock-function-name-face)
    ;; Fontify variable names
    '("\\<\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" . font-lock-variable-name-face)
    ;; Fontify numbers
    '("\\<\\([0-9][0-9A-Fa-fXxh]*\\)\\>" . font-lock-number-face)
    )
   'set))
