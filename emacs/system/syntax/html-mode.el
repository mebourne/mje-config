;; Emacs configuration file
;; Syntax colouring regular expressions for html-mode
;; Written by Martin Ebourne
;; $Id: html-mode.el 792 2003-09-22 11:47:18Z martin $

;; HTML
(defconst html-font-lock-keywords-local
  '(("<\\([!?][a-z0-9]+\\)" 1 font-lock-keyword-face)
    ("<\\(/?[a-z0-9]+\\)" 1 font-lock-function-name-face)
    ("[&%][-.A-Za-z0-9]+;?" . font-lock-variable-name-face)
    )
  "Additional expressions to highlight in HTML mode.")
