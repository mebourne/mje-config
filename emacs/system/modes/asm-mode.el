;; Emacs configuration file
;; Setup for asm-mode
;; Written by Martin Ebourne
;; $Id: asm-mode.el,v 1.2 2001/05/16 14:22:45 mebourne Exp $

(add-hook 'asm-mode-hook
	  (function (lambda ()
		      (local-set-key ";"    'self-insert-command)
		      (local-set-key ":"    'self-insert-command)
		      (local-set-key "\C-m" 'newline)
		      (set (make-local-variable 'dabbrev-case-fold-search) nil)
		      (set (make-local-variable 'dabbrev-case-replace) nil)
		      (add-hook 'write-contents-hooks 'eliminate-trailing-spaces)
		      )))
