;; Emacs configuration file
;; Setup for shell
;; Written by Martin Ebourne
;; $Id: shell.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;; Shell mode
(setq executable-insert nil)
(setq auto-mode-alist
      ;; matches files
      ;;	- that have a suffix .sh, .csh or .shar (shell archive)
      ;;	- that contain ressources for the various shells
      ;;	- startup files for X11
      (cons (cons
	     (concat "\\.c?sh\\'\\|\\.shar\\'\\|/\\.\\(z?profile\\|bash_profile\\|z?login\\|"
		     "bash_login\\|z?logout\\|bash_logout\\|[kz]shrc\\|bashrc\\|t?cshrc\\|"
		     "esrc\\|rcrc\\|[kz]shenv\\|xinitrc\\|startxrc\\|xsession\\)\\'\\|"
		     "/z\\(profile\\|login\\|logout\\|shrc\\)'")
	     'sh-mode)
	    auto-mode-alist)
      interpreter-mode-alist
      (nconc '(("ash" . sh-mode)
	       ("bash" . sh-mode)
	       ("csh" . sh-mode)
	       ("dtksh" . sh-mode)
	       ("es" . sh-mode)
	       ("itcsh" . sh-mode)
	       ("jsh" . sh-mode)
	       ("ksh" . sh-mode)
	       ("oash" . sh-mode)
	       ("pdksh" . sh-mode)
	       ("rc" . sh-mode)
	       ("sh" . sh-mode)
	       ("sh5" . sh-mode)
	       ("tcsh" . sh-mode)
	       ("wksh" . sh-mode)
	       ("wsh" . sh-mode)
	       ("zsh" . sh-mode))
	     interpreter-mode-alist))
