;; Emacs configuration file
;; Miscellaneous stuff
;; Written by Martin Ebourne
;; $Id$

(setq message-log-max 1000)

;; Load clearcase integration
(load "clearcase")

;; Load filecache
(load "filecache")

;; Load semantic
;(load "semantic-load")
; Disable auto-parse for now since it give errors during startup:
; Symbol's function definition is void: semantic-map-buffers
;(global-semantic-auto-parse-mode 1)
;;(global-semanticdb-minor-mode 1)
;(add-hook 'semantic-init-hooks
;	  (lambda ()
;	    (imenu-add-to-menubar "Tokens")))

;; Display european characters
(set-language-environment "Latin-1")

;; Get the time & column number on the info bar. Disable mail flag though
;(display-time)
(column-number-mode t)

;; Enable BBC-B style cursor copying
(setq vcursor-key-bindings t)
(require 'vcursor)

;; My really wonderful cursor flashing - the biz!
(blink-cursor-mode 0)
(blink-enable)

;; My just as wonderful cursor constraining - top!
(constrain-enable)

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
