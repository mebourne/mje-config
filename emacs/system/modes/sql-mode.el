;; Emacs configuration file
;; Setup for sql-mode
;; Written by Martin Ebourne
;; $Id: sql-mode.el 792 2003-09-22 11:47:18Z martin $

(setq auto-mode-alist
      (append '(("\\.sql\\'" . sql-mode)
		("\\.sp\\'"  . sql-mode)
		("\\.idx\\'" . sql-mode)
		("\\.key\\'" . sql-mode)
		) auto-mode-alist))


(defun sql-highlight-sybase-keywords ()
  "Highlight Sybase keywords.
Basically, this just sets `font-lock-keywords' appropriately."
  (interactive)
  (setq font-lock-keywords sql-mode-sybase-font-lock-keywords)
  (font-lock-fontify-buffer))


(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (setq comment-column 45)
		      (setq fill-column 78)
		      (setq paragraph-start "[ 	]*$")
		      (setq font-lock-defaults '(sql-mode-sybase-font-lock-keywords
						 nil t ((?_ . "w"))))
		      )))


;(eval-after-load "sql"
;  '(progn
;     ; Map _ as a word character
;     (modify-syntax-entry ?_ "w" sql-mode-syntax-table)
;     ))

;   '(
;     ("^\\(#.*\\)$" (1 font-lock-constant-face))
;     ("[^A-Za-z0-9_@]\\(@@?[A-Za-z0-9_]+\\)\\>" (1 font-lock-variable-name-face))
;     ("\\<[0-9.][0-9Ee+-.]*\\>" . font-lock-number-face)
;     )
;   (list "\\.sql\\'" "\\.sp\\'")
;	     (modify-syntax-entry ?_ "w" font-lock-syntax-table)
