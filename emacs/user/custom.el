;; Emacs configuration file
;; Custom definitions
;; Written by Martin Ebourne
;; $Id: custom.el 792 2003-09-22 11:47:18Z martin $

(custom-set-variables

 ;; No message in scratch buffer
 '(initial-scratch-message nil)

 ;; No beeps
 '(visible-bell t)

 ;; Speedbar size/position
 '(speedbar-frame-parameters (quote ((minibuffer . nil)
				     (unsplittable . t)
				     (menu-bar-lines . 0)
				     (border-width . 0)
				     (top . 20)
				     (left . 762)
				     (width . 55)
				     (user-position . t))))

 ;; Set up mail addresses
 '(mail-user-agent 'message-user-agent)
 '(mail-host-address "csfb.com")
 '(user-mail-address "Martin.Ebourne@csfb.com")
 '(message-signature
   "Martin Ebourne
Martin.Ebourne@csfb.com
")
 )
