;; Emacs configuration file
;; Syntax colouring regular expressions for sendmail and message
;; Written by Martin Ebourne

;; Mail editing modes
(defconst mail-keywords-local
  '(;; Fontify quoting
    ("^> > >.*$" . font-lock-constant-face)
    ;; Fontify quoting
    ("^> >.*$" . font-lock-keyword-face)
    ;; Fontify quoting
    ("^>.*$" . font-lock-string-face)
    ;; Fontify headers
    ("^[A-Z][A-Za-z-]+:" . font-lock-comment-face)
    ;; Fontify signature
    ("^-- \\(.\\|\n\\)*\\'" . font-lock-comment-face)
    ;; Fontify mail addresses
    ("<?[A-Za-z0-9.$_-]+@\\([A-Za-z0-9_-]+\\.\\)*[A-Za-z0-9_-]+>?" .
     font-lock-function-name-face)
    ;; Fontify URLs
    ("\\(\\(<\\(URL:\\)?\\)?ftp://[A-Za-z0-9./~_-]*>?\\)" 1
     font-lock-variable-name-face t)
    ("\\(\\(<\\(URL:\\)?\\)?https?://[A-Za-z0-9./~_-]*>?\\)" 1
     font-lock-variable-name-face t)
    ("\\(\\(<\\(URL:\\)?\\)?gopher://[A-Za-z0-9./~_-]*>?\\)" 1
     font-lock-variable-name-face t)
    ("\\(\\(<\\(URL:\\)?\\)?wais://[A-Za-z0-9./~_-]*>?\\)" 1
     font-lock-variable-name-face t)
    )
  "Additional expressions to highlight in mail editing modes.")
(eval-after-load
 "sendmail"
 '(setq mail-font-lock-keywords mail-keywords-local))
(eval-after-load
 "message"
 '(setq message-font-lock-keywords mail-keywords-local))
