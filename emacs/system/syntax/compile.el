;; Emacs configuration file
;; Syntax colouring regular expressions for compile
;; Written by Martin Ebourne

;; Compilation
(eval-after-load
 "compile"
 '(font-lock-add-keywords
   'compilation-mode
   '(("\\`.*\n.*$" . font-lock-comment-face)
     ("^make:.*\\*\\*\\*.*$" . font-lock-warning-face)
     ("^make:.*$" . font-lock-keyword-face)
     ("^Compilation finished.*$" . font-lock-comment-face)
     ("^\\(WARNING:\\)\\(.*\\)$" (1 font-lock-function-name-face)
      (2 font-lock-variable-name-face))
     ("^\\([^:]+:\\([0-9]+:\\)+\\)\\(.*\\)$" (1 font-lock-function-name-face)
      (3 font-lock-variable-name-face))
     ("\\<[0-9][0-9]+\\>" . font-lock-number-face)
     ;("^.+$" . font-lock-function-name-face)
     )
   'set))
