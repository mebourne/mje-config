;; Emacs configuration file
;; Setup for desktop session saving
;; Written by Martin Ebourne
;; $Id$

(eval-when-compile
  (require 'desktop))

;; Session saver
(cond (window-system
       (require 'desktop)

       (if (fboundp 'desktop-save-mode)
	   (desktop-save-mode (not secondary))
	 (setq desktop-enable (not secondary))
	 )

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
			  find-file-root-history
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
