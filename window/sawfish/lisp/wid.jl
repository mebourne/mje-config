
;; wid.jl -- simple window identifiers
;; $Id$

;; Copyright (C) 2001 Nikita Danilov <NikitaDanilov.Yahoo.COM>

;; This file is part of wid.

;; Wid is free software; you can redistribute  it and/or modify it under
;; the terms of the GNU General Public  License as published by the Free
;; Software Foundation; either version 2, or  (at your option) any later
;; version.

;; Wid is distributed in  the hope that it will  be useful,  but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You  should have received  a copy  of the  GNU General Public License
;; along with sawmill; see the file COPYING.  If not,  write to the Free
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Assigns to windows short ids (wids) (from the sequence
;; 0,1,2,..,9,a,b,...,z,36,37,...)  and provides shortcuts to switch to
;; particular window by one key-stroke.

;; INSTALLATION:
;; Put this file into load-path and add (load "wid") to .sawfishrc
;;
;; After installation window titles will be pefixed with string of the form
;; "<wid>| ". To switch to the window with id <wid>, press Hyper-<wid>.
;;
;; CONFIGURATION:
;; By default, this module binds its short-cuts to the Hyper-prefixed
;; keystrokes. If you want to use Hyper and Super modifiers on PC keyboard
;; with Microsoft keys, add something like this to your .xmodmap file:
;; 
;; ! for pc-keyboard with `Windows' keys
;; ! left key with MSWindows logo
;; keycode 115 = Hyper_L
;; ! right key with MSWindows logo
;; keycode 116 = Hyper_R
;; ! local menu
;; keycode 117 = Super_R
;;
;; wid-widability-hook is used to select which windows should be decorated
;; with wids. Use something like this
;; 
;; (add-hook 'wid-widability-hook 
;;		  (lambda (win)
;;			(and (not (equal (aref (get-x-text-property win 'WM_CLASS) 1) 
;;							 "Panel"))
;;				 (not (equal (aref (get-x-text-property win 'WM_CLASS) 1) 
;;							 "Xrus")))))
;;
;; to skip Gnome panel, keyboard switcher, etc.
;; 

(defvar wid-used-wids nil "List of used wids")
(defvar wid-wid-property nil "Symbol used to store wids as window properties")
(defvar wid-widability-hook nil 
  "Hook called at window creation. Functions in this hook are called with single argiment: window. They should return t if window should be decorated with wid.")

(defun wid-wid-used-? (id)
  (member id wid-used-wids))

(defun wid-use-wid (id)
  (if (not (wid-wid-used-? id))
		(setq wid-used-wids (cons id wid-used-wids))))

(defun wid-free-wid (id)
  (if (wid-wid-used-? id)
	  (setq wid-used-wids (delete id wid-used-wids))))

(defun wid-fresh-wid ()
  (let ((scan 0)) (while (wid-wid-used-? scan) (setq scan (1+ scan))) scan))

(defun wid-get-window-wid (window)
  (window-get window 'wid-wid-property))

(defun wid-set-window-wid (window wid)
  (window-put window 'wid-wid-property wid))

(defun wid-widable-window-? (window)
  (or (not wid-widability-hook) 
	  (call-hook wid-widability-hook (list window) 'and)))

(defun wid-add-wid-to-window (window)
  (if (wid-widable-window-? window)
	  (let ((id (wid-fresh-wid)))
		(wid-set-window-wid window id)
		(wid-use-wid id)
		(wid-add-wid-to-title window)
		(wid-add-wid-to-icon-name window)
		)))

(defun wid-del-wid-from-window (window)
  (wid-free-wid (wid-get-window-wid window)))

(defun wid-find-window-by-wid (id)
  (car (filter (lambda (window) 
				 (equal (wid-get-window-wid window) id)) 
			   (managed-windows))))

(add-hook 'add-window-hook wid-add-wid-to-window)
(add-hook 'destroy-notify-hook wid-del-wid-from-window)

(defun wid-wid-to-string (id)
  (cond 
   ((<= 10 id 36) (format nil "%c" (+ ?a (- id 10))))
   (t (format nil "%d" id))))

(defun wid-start-with-wid-? (name id)
  (string-match (concat "^" (wid-wid-to-string id) "\\| ") name))

(defun wid-add-wid-to-title (window)
  (let* ((id (wid-get-window-wid window))
		(id-string (wid-wid-to-string id))
		(title (or (nth 2 (get-x-property window 'WM_NAME)) "")))
	(if (and id (not (wid-start-with-wid-? title id)))
	    (progn
	      (if (string-match "^(.\\| )+" title)
		  (setq title (substring title (match-end 0)))
		  )
	      (set-x-property window 'WM_NAME (concat id-string "| " title) 
			      'STRING 8)))))

(defun wid-add-wid-to-icon-name (window)
  (let* ((id (wid-get-window-wid window))
		(id-string (wid-wid-to-string id))
		(iconname (or (nth 2 (get-x-property window 'WM_ICON_NAME)) "")))
	(if (and id (not (wid-start-with-wid-? iconname id)))
	    (progn
	      (if (string-match "^(.\\| )+" iconname)
		  (setq iconname (substring iconname (match-end 0)))
		  )
	      (set-x-property window 'WM_ICON_NAME (concat id-string "| " iconname) 
			      'STRING 8)
	      (set-x-property window '_NET_WM_ICON_NAME (concat id-string "| " iconname) 
			      'UTF8_STRING 8)
	      ))))

(add-hook 'property-notify-hook (lambda (window prop type)
				  (if (wid-widable-window-? window)
				      (case prop
					((WM_NAME)
					 (wid-add-wid-to-title window))
					((WM_ICON_NAME)
					 (wid-add-wid-to-icon-name window))))))

(add-hook 'wid-widability-hook (lambda (window)
				 (and window (not (window-transient-p window))
				      (window-wants-input-p window))))

(bind-keys global-keymap "H-0" '(display-window (wid-find-window-by-wid 0)))
(bind-keys global-keymap "H-1" '(display-window (wid-find-window-by-wid 1)))
(bind-keys global-keymap "H-2" '(display-window (wid-find-window-by-wid 2)))
(bind-keys global-keymap "H-3" '(display-window (wid-find-window-by-wid 3)))
(bind-keys global-keymap "H-4" '(display-window (wid-find-window-by-wid 4)))
(bind-keys global-keymap "H-5" '(display-window (wid-find-window-by-wid 5)))
(bind-keys global-keymap "H-6" '(display-window (wid-find-window-by-wid 6)))
(bind-keys global-keymap "H-7" '(display-window (wid-find-window-by-wid 7)))
(bind-keys global-keymap "H-8" '(display-window (wid-find-window-by-wid 8)))
(bind-keys global-keymap "H-9" '(display-window (wid-find-window-by-wid 9)))
(bind-keys global-keymap "H-a" '(display-window (wid-find-window-by-wid 10)))
(bind-keys global-keymap "H-b" '(display-window (wid-find-window-by-wid 11)))
(bind-keys global-keymap "H-c" '(display-window (wid-find-window-by-wid 12)))
(bind-keys global-keymap "H-d" '(display-window (wid-find-window-by-wid 13)))
(bind-keys global-keymap "H-e" '(display-window (wid-find-window-by-wid 14)))
(bind-keys global-keymap "H-f" '(display-window (wid-find-window-by-wid 15)))
(bind-keys global-keymap "H-g" '(display-window (wid-find-window-by-wid 16)))
(bind-keys global-keymap "H-h" '(display-window (wid-find-window-by-wid 17)))
(bind-keys global-keymap "H-i" '(display-window (wid-find-window-by-wid 18)))
(bind-keys global-keymap "H-j" '(display-window (wid-find-window-by-wid 19)))
(bind-keys global-keymap "H-k" '(display-window (wid-find-window-by-wid 20)))
(bind-keys global-keymap "H-l" '(display-window (wid-find-window-by-wid 21)))
(bind-keys global-keymap "H-m" '(display-window (wid-find-window-by-wid 22)))
(bind-keys global-keymap "H-n" '(display-window (wid-find-window-by-wid 23)))
(bind-keys global-keymap "H-o" '(display-window (wid-find-window-by-wid 24)))
(bind-keys global-keymap "H-p" '(display-window (wid-find-window-by-wid 25)))
(bind-keys global-keymap "H-q" '(display-window (wid-find-window-by-wid 26)))
(bind-keys global-keymap "H-r" '(display-window (wid-find-window-by-wid 27)))
(bind-keys global-keymap "H-s" '(display-window (wid-find-window-by-wid 28)))
(bind-keys global-keymap "H-t" '(display-window (wid-find-window-by-wid 29)))
(bind-keys global-keymap "H-u" '(display-window (wid-find-window-by-wid 30)))
(bind-keys global-keymap "H-v" '(display-window (wid-find-window-by-wid 31)))
(bind-keys global-keymap "H-w" '(display-window (wid-find-window-by-wid 32)))
(bind-keys global-keymap "H-x" '(display-window (wid-find-window-by-wid 33)))
(bind-keys global-keymap "H-y" '(display-window (wid-find-window-by-wid 34)))
(bind-keys global-keymap "H-z" '(display-window (wid-find-window-by-wid 35)))

(provide 'wid)

;;
;; $Log$
;;

