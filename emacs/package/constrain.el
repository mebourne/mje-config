;;; constrain.el --- Restrict cursor to middle area of window

;; Copyright (C) 1997-1999 Martin Ebourne, martin@zzz.co.uk
;; All rights reserved
;; 30-01-97 - First version
;; 31-01-97 - changed from constrain to middle line to constrain
;;            to middle area of window. Added ability to make
;;            scroll by less than the window height, Simon Liddington
;; 31-01-97 - Fixed page-up and page-down to keep cursor column, MJE
;; 18-02-97 - Fix to check in current buffer, SJL
;; 10-06-99 - Fixed to work properly with the mouse, MJE
;;
;; $Id: constrain.el 792 2003-09-22 11:47:18Z martin $

;;; Commentary:

;; Load this and from then on the cursor will be constrained to
;; stay above a certain percentage of the window height from
;; the bottom and top of the window. This both makes it easier
;; to find, and ensures useful amounts of visible source above
;; and below. Page up and down are also re-defined to be more
;; sensible.

;;; Code:


;; User configurable variables

(defvar constrain-percentage 20
  "*Percentage of buffer window at top and bottom which the cursor is not
allowed in.")

(defvar constrain-scroll-percentage 100
  "*Percentage of buffer window size by which scroll up and down move point.")

(defvar constrain-period 0.25
  "*Time in seconds between constrains of the cursor.")

(defvar constrain-except-modes '(speedbar-mode)
  "*Modes which constrain should not operate in.")


;; User interface functions

(defvar constrain-timer nil)

(defun constrain-enable ()
  "Force cursor to stay above a certain percentage of the window height
 from the bottom and top of the window. The percentage is set by the
 variable constrain-percentage"
  (interactive)
;;  (add-hook 'post-command-hook 'constrain-cursor)
  (if constrain-timer
      (cancel-timer constrain-timer))
  (setq scroll-conservatively 50)
  (setq constrain-timer (run-with-timer 0 constrain-period 'constrain-cursor))
  (global-set-key [prior] 'constrain-page-up)
  (global-set-key "\M-v" 'constrain-page-up)
  (global-set-key [next] 'constrain-page-down)
  (global-set-key "\C-v" 'constrain-page-down)
  )

(defun constrain-disable ()
  "Allow the cursor to go where it normally does."
  (interactive)
;;  (remove-hook 'post-command-hook 'constrain-cursor)
  (if constrain-timer
      (cancel-timer constrain-timer))
  (setq constrain-timer nil)
  (global-set-key [prior] 'scroll-down)
  (global-set-key "\M-v" 'scroll-down)
  (global-set-key [next] 'scroll-up)
  (global-set-key "\C-v" 'scroll-up)
  )


;; Internal functions

(defun constrain-current-line nil
  "Return the vertical position of point in the selected window.
Top line is 0.  Counts each text line only once, even if it wraps."
  (+ (count-lines (window-start) (window-point)) (if (= (current-column) 0) 1 0) -1))

(defun constrain-highest-line nil
  "Return the highest line in the window allowed by constrain-percentage."
  (if (= constrain-percentage 50)
      (/ (window-height) 2)
    (/ (* (- (window-height) 2) constrain-percentage) 100))
  )

(defun constrain-lowest-line nil
  "Return the lowest line in the window allowed by constrain-percentage."
  (if (= constrain-percentage 50)
      (/ (window-height) 2)
    (- (- (window-height) 2) (constrain-highest-line)))
  )

(defvar constrain-last-event-type nil)

(defun constrain-cursor ()
  "Re-position cursor within lines of window defined by constrain-percentage."

  ;; Only ever constrain current buffer
  (if (eq (current-buffer) (window-buffer))
      (if (not (memq major-mode constrain-except-modes))
	  (let ((wasmouse nil)
		(type (car-safe last-input-event))
		(basic-type (event-basic-type last-input-event)))


	    ;; Don't constrain if last event involved one of the mouse buttons or mouse dragging.
	    ;; The only exception to this is a single press of button 1 which is to place cursor
	    ;; and therefore should constrain. However, delay this case until the next time to
	    ;; ensure we haven't caught the start of a double-click
	    (if (or (and (eq basic-type 'mouse-1)
			 (not (and (eq type 'mouse-1)
				   (eq type constrain-last-event-type))))
		    (eq basic-type 'mouse-2)
		    (eq basic-type 'mouse-3)
		    (mouse-movement-p last-input-event))
		(setq wasmouse t)
	      )

	    ;; Remember type for button 1 single click checks
	    (setq constrain-last-event-type type)

	    ;; Only constrain if not mouse operation from above and cursor is out of bounds
	    (if (not wasmouse)
		(cond ((>= (constrain-current-line) (constrain-lowest-line))
		       (recenter (constrain-lowest-line)))
		      ((< (constrain-current-line) (constrain-highest-line))
		       (recenter (constrain-highest-line))))
	      )
	    )
	)
    )
  )

(defun constrain-page-up (&optional arg)
  "Move cursor vertically up by ARG pages."
  (interactive "p")
  (let ((start-line (constrain-current-line)))
    (previous-line (* arg (/ (* (- (window-height) 2) constrain-scroll-percentage) 100)))
    (setq this-command 'previous-line)
    (recenter start-line))
  )

(defun constrain-page-down (&optional arg)
  "Move cursor vertically down by ARG pages."
  (interactive "p")
  (let ((start-line (constrain-current-line)))
    (next-line (* arg (/ (* (- (window-height) 2) constrain-scroll-percentage) 100)))
    (setq this-command 'next-line)
    (recenter start-line))
  )

;; Make sure if we scroll to fast we at least don't recenter
(constrain-enable)

(provide 'constrain)
