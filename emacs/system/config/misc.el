;; Emacs configuration file
;; Miscellaneous stuff
;; Written by Martin Ebourne
;; $Id: misc.el,v 1.8 2002/03/25 17:42:10 mebourne Exp $

(setq message-log-max 1000)

;; Load site-lisp files
(load "clearcase")

;; Load filecache
(load "filecache")

;; Display european characters
(standard-display-european t)

;; Get the time & column number on the info bar. Disable mail flag though
;(display-time)
(column-number-mode 1)

;; Enable BBC-B style cursor copying
(setq vcursor-key-bindings t)
(require 'vcursor)

;; My really wonderful cursor flashing - the biz!
(require 'blink)

;; My just as wonderful cursor constraining - top!
(require 'constrain)

 ;; Give buffers sensible unique filenames
(require 'uniquify)

;; Fix behaviour of minibuffer history
(require 'fix-hist)

;; Well good buffer switching
(iswitchb-mode t)
(iswitchb-default-keybindings)
(setq iswitchb-case nil)

;; Save places in files for when reloading them
(require 'saveplace)

;; Enable lots of little file editing modes
(require 'generic-x)

;; Enable auto (de-)compression
(auto-compression-mode t)

;; Enable global auto revert
(global-auto-revert-mode t)

;; Display image files as pictures
(auto-image-file-mode t)

;; Default all files in the shell configuration directories to be shell script mode
(if (getenv "ZCONFIGDIR")
    (setq auto-mode-alist
	  (append auto-mode-alist
		  (list (cons (concat "^" (getenv "ZCONFIGDIR") "/") 'sh-mode)
			(cons (concat "^" (getenv "ZUSERCONFIGDIR") "/") 'sh-mode))
		  ))
  )
