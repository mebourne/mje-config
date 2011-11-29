;; Emacs configuration file
;; Setup for sql-mode
;; Written by Martin Ebourne

(setq auto-mode-alist
      (append '(("\\.constr\\'" . sql-mode)
		("\\.vw\\'" . sql-mode)
		("\\.bind\\'" . sql-mode)
		("\\.lock\\'" . sql-mode)
		) auto-mode-alist))
