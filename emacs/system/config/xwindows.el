;; Emacs configuration file
;; Xwindows specific setup
;; Written by Martin Ebourne
;; $Id$

(cond (window-system

       ;; Sort out kill-emacs to confirm first and C-x C-z not to iconify if in X
       (add-hook 'kill-emacs-query-functions
		 (function (lambda ()
			     (y-or-n-p "Are you MAD? "))))
       (global-unset-key "\C-x\C-z")

       ;; Set up defaults for server
       (if (eq system-type 'windows-nt)
	   (progn
	     (require 'gnuserv)
	     (setq gnuserv-frame (selected-frame))
	     (setq ange-ftp-tmp-name-template
		   (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))
	     (setq ange-ftp-gateway-tmp-name-template
		   (concat (expand-file-name (getenv "TEMP")) "/ange-ftp"))
	     )
	 (require 'server)
	 (setq server-temp-file-regexp "/tmp/\\|/draft$")
	 )
       ))
