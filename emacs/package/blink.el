;;; blink.el --- Flash cursor

;; Copyright (C) 1997 Martin Ebourne, martin@galaxy.tcp.co.uk
;; All rights reserved
;; 29-01-97 - First version
;; 30-01-97 - Renamed from cursor to blink
;; 30-01-97 - Added blink-enable and blink-disable
;;
;; $Id: blink.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;;; Commentary:

;; Load this and from then on the cursor will flash in order to help make
;; it more visible.

;;; Code:


;; User configurable variables

(defvar blink-wait-time 3
  "*Time in seconds to wait after user input before starting to flash the
cursor.")

(defvar blink-show-time 1.5
  "*Time in seconds to flash the cursor on for.")

(defvar blink-hide-time 0.5
  "*Time in seconds to flash the cursor off for.")

(defvar blink-show-colour "green"
  "*Colour to use when showing the cursor.")

(defvar blink-hide-colour "rgb:05/05/05"
  "*Colour to use when hiding the cursor. Black will not work because
Emacs mutates it into white - use a dark grey instead.")


;; User interface functions

(defun blink-enable ()
  "Enable cursor flashing."
  (interactive)
  (add-hook 'post-command-hook 'blink-reset)
  )
  
(defun blink-disable ()
  "Disable cursor flashing."
  (interactive)
  (remove-hook 'post-command-hook 'blink-reset)
  (blink-cancel-timers)
  )


;; Internal variables

(defvar blink-show-handle nil)
(defvar blink-hide-handle nil)


;; Internal functions

(defun blink-show ()
  "Show the cursor."
  (set-cursor-color blink-show-colour)
  )

(defun blink-hide ()
  "Hide the cursor."
  (set-cursor-color blink-hide-colour)
  
  ;; Start up a timeout to show it again
  (setq blink-show-handle (run-with-timer blink-hide-time nil 'blink-show))
  )

(defun blink-cancel-timers ()
  "Cancel any blink timers and make sure the cursor is visible."
  (if blink-show-handle
      (cancel-timer blink-show-handle))
  (if blink-hide-handle
      (cancel-timer blink-hide-handle))
  (blink-show)
  )

(defun blink-reset ()
  "Restart cursor flashing."
  (blink-cancel-timers)

  ;; Start up a new repeating timer
  (setq blink-hide-handle (run-with-timer blink-wait-time (+ blink-hide-time blink-show-time)
					    'blink-hide))
  )

(blink-enable)

(provide 'blink)
