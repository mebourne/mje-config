;; Emacs configuration file
;; Syntax colouring regular expressions for html-mode
;; Written by Martin Ebourne

;; HTML
(font-lock-add-keywords
 'html-mode
 '(("<\\([!?][a-z0-9]+\\)" 1 font-lock-keyword-face)
   ("<\\(/?[a-z0-9]+\\)" 1 font-lock-function-name-face)
   ("[&%][-.A-Za-z0-9]+;?" . font-lock-variable-name-face)
   )
 'set)
