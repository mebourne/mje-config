;; Emacs configuration file
;; Syntax colouring regular expressions pcl-cvs
;; Written by Martin Ebourne

;; PCL-CVS
(font-lock-add-keywords
 'cvs-mode
 '(("^In.*$" . font-lock-function-name-face)
   ("^.* ci .*$" . font-lock-variable-name-face)
   ("^[ *]*\\(Updated\\|Modified\\|Merged\\|Added\\|Removed\\|Patched\\|Same\\).*$" .
    font-lock-keyword-face)
   ("^[ *]*Conflict.*$" . font-lock-string-face)
   ("^[ *]*Unknown.*$" . font-lock-string-face)
   ("^[ *]*Move.*$" . font-lock-warning-face)
   ("^[ *]*Internal.*$" . font-lock-warning-face)
   ("^\\(PCL-CVS\\|  This\\|---\\).*$" . font-lock-comment-face)
   )
 'set)
