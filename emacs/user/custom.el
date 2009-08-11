;; Emacs configuration file
;; Custom definitions
;; Written by Martin Ebourne
;; $Id$

(custom-set-variables

 ;; Disable the useless toolbar
 '(tool-bar-mode nil)

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
 '(mail-host-address "zepler.org")
 '(user-mail-address "martin@zepler.org")
 '(message-signature
   "Martin Ebourne
martin@zepler.org
")
 )
