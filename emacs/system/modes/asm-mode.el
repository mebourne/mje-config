;; Emacs configuration file
;; Setup for asm-mode
;; Written by Martin Ebourne
;; $Id: asm-mode.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

(setq auto-mode-alist
      (append '(("\\.asm"   . asm-mode)
		) auto-mode-alist))
(add-hook 'asm-mode-hook
	  (function (lambda ()
		      (local-set-key ";"    'self-insert-command)
		      (local-set-key ":"    'self-insert-command)
		      (local-set-key "\C-m" 'newline)
		      (set (make-local-variable 'dabbrev-case-fold-search) nil)
		      (set (make-local-variable 'dabbrev-case-replace) nil)
		      (add-hook 'write-contents-hooks 'eliminate-trailing-spaces)
		      )))
