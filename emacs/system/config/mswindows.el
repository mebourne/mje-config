;; Emacs configuration file
;; MS Windows specific setup
;; Written by Martin Ebourne
;; $Id: mswindows.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

(if (eq system-type 'windows-nt)
    (progn

      ;; For the interactive shell
      (setq w32-quote-process-args t)
      (setq shell-command-switch "-c")

      (custom-set-variables
       '(ange-ftp-ftp-program-name "c:/gnuwin32/emacs-20.2.1/bin/ftp.exe")
       '(shell-file-name "c:/gnuwin32/emacs-20.2.1/bin/cmdproxy.exe")
       '(explicit-shell-file-name "c:/gnuwin32/b18/H-i386-cygwin32/bin/bash.exe")
       )

      ;; Set basic colours correctly
      (defun set-frame-colours (frame)
	(if inverse-video
	    (modify-frame-parameters frame (list '(background-color . "black")
						 '(foreground-color . "white")
						 '(background-mode  . dark))))
	)
      (add-hook 'after-make-frame-functions 'set-frame-colours)
      (if inverse-video
	  (progn
	    (set-frame-colours (selected-frame))
	    (setq frame-background-mode 'dark)
	    (set-face-foreground 'default "white")
	    (set-face-foreground 'bold "white")
	    (set-face-foreground 'bold-italic "white")
	    (set-face-foreground 'italic "white")
	    (set-face-foreground 'underline "white")
	    (set-face-foreground 'modeline "black")
	    (set-face-background 'modeline "white")
	    (eval-after-load "info"
	      '(progn
		 (set-face-foreground 'info-menu-5 "white")
		 (set-face-foreground 'info-node "white")
		 (set-face-foreground 'info-xref "white")
		 ))
	    ))

      (setq w32-grab-focus-on-raise nil)

      ))
