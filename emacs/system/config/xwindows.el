;; Emacs configuration file
;; Xwindows specific setup
;; Written by Martin Ebourne
;; $Id: xwindows.el,v 1.2 2002/03/18 20:04:48 mebourne Exp $

;; Syntax colouring. X only
(cond (window-system

       ;; Setup font-lock mode
       (eval-after-load
        "font-lock"
        '(progn
           (setq font-lock-defaults-alist
		 (append
		  '((c-mode             . (c-font-lock-keywords-local))
		    (c++-mode           . (c++-font-lock-keywords-local))
		    (java-mode          . (java-font-lock-keywords-local))
		    (jde-mode           . (java-font-lock-keywords-local))
		    (cvs-mode           . (cvs-font-lock-keywords-local))
		    (html-mode          . (html-font-lock-keywords-local))
		    )
		  font-lock-defaults-alist))))

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

       ;; Keep the mouse pointer out of the way
;       (mouse-avoidance-mode 'banish)

       ;; Enable strokes
       (strokes-mode)
       ))
