;; Emacs configuration file
;; Setup for sql-mode
;; Written by Martin Ebourne
;; $Id: sql-mode.el 792 2003-09-22 11:47:18Z martin $

(setq auto-mode-alist
      (append '(("\\.constr\\'" . sql-mode)
		("\\.vw\\'" . sql-mode)
		("\\.bind\\'" . sql-mode)
		("\\.lock\\'" . sql-mode)
		) auto-mode-alist))
