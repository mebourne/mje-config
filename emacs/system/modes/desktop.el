;; Emacs configuration file
;; Setup for desktop session saving
;; Written by Martin Ebourne
;; $Id: desktop.el,v 1.3 2002/11/25 15:42:17 mebourne Exp $

;; Session saver
(cond (window-system
       (load "desktop")

       (setq desktop-enable (not secondary))

       (setq desktop-globals-to-save
	     (append
	      '(kill-ring coding-system-history
			  compile-command
			  command-history
			  compile-history
			  dired-regexp-history
			  dired-shell-command-history
			  extended-command-history
			  filename-history
			  file-name-history
			  input-method-history
			  iswitchb-history
			  minibuffer-history
			  query-replace-history
			  read-expression-history
			  regexp-history
			  set-variable-value-history
			  shell-command-history
			  gud-gdb-history
			  gud-sdb-history
			  gud-dbx-history
			  gud-xdb-history
			  gud-perldb-history
			  gud-pdb-history
			  gud-jdb-history
			  )
	      desktop-globals-to-save))
       (add-hook 'kill-emacs-hook
		 (function (lambda ()
			     (desktop-truncate search-ring 10)
			     (desktop-truncate regexp-search-ring 10)
			     (desktop-truncate kill-ring 10)
			     (desktop-truncate kill-ring-yank-pointer 10)
			     )))
       ))
