; -*- sawfish -*-
;Time-stamp: <01/11/20 01:04:01 friedel>
;; --- Full window maximization as in WindowMaker --- ;;
;; by Friedrich Delgado Friedrichs <friedel@nomaden.org>

;; Installation: Put this into ~/.sawfish/lisp/friedel and
;; (require 'friedel.fullmax) in your ~/.sawfishrc
;;
;; Afterwards bind a keystroke to (maximize-full-toggle)
;;
;; This is different from the deframation lisp snippet by Greg Merchan
;; (see http://adraken.themes.org/map.php?keywords=frames) in that
;; respect, that it deframes the window and resizes it over the whole
;; area of the screen (i sometimes like this for browsers and/or
;; emacs), obstructing anything else, as WindowMaker is able to do it.
;;
;; Changes:
;; Wed Apr 18 18:25:29 MEST 2001, Andreas Büsching <crunchy@tzi.de>
;;   * added optional parameters to maximize-full-toggle
;;     to adjust width, height and vertical and horizontal offset
;;   * added to functions maximize-full-vertical-right-toggle, 
;;     maximize-full-vertical-left-toggle which help to create a
;;     split screen (on the left my emacs and next to it a rxvt
;;     showing the RFC I have to implement ;-)

(define *fullmaxed-windows* '())
(define *fullmaxed-window-dimensions* '())
(define *fullmaxed-window-positions* '())

(define maxfull-hooks '(window-state-change-hook destroy-notify-hook))

(defun maxfull-restore-dimensions (w)
  (let ((wd (assoc w *fullmaxed-window-dimensions*)))
    ;;Window w has changed dimensions
    (when wd
      (resize-window-to w (cadr wd) (cddr wd))
      (setq *fullmaxed-window-dimensions*
	    (remove (assoc w *fullmaxed-window-dimensions*)
		    *fullmaxed-window-dimensions*)))))

(defun maxfull-restore-positions (w)
  (let ((wp (assoc w *fullmaxed-window-positions*)))
    ;;Window w has changed position
    (when wp
      (move-window-to w (cadr wp) (cddr wp))
            (setq *fullmaxed-window-positions*
	    (remove (assoc w *fullmaxed-window-positions*)
		    *fullmaxed-window-positions*)))))

(defun maxfull-restore-frame (w)
  (let ((wi (assoc w *fullmaxed-windows*)))
    ;;Window w is fullmaxed
    (when wi
      (set-window-frame w (cdr wi))
      (setq *fullmaxed-windows* (remove wi *fullmaxed-windows*)))))

(defun maxfull-enable-hooks ()
  (mapcar (lambda (x)
	    (add-hook x maxfull-restore-frame)) maxfull-hooks))

(defun maxfull-disable-hooks ()
  (mapcar (lambda (x)
	    (remove-hook x maxfull-restore-frame)) maxfull-hooks))

(maxfull-enable-hooks)

(defun maximize-full-vertical-right-toggle (w)
  "Maximize window to half screen size (without frame)"
  (interactive "%W")
  (maximize-full-toggle w 2 1 (/ (screen-width) 2) 0))

(defun maximize-full-vertical-left-toggle (w)
  "Maximize window to half screen size (without frame)"
  (interactive "%W")
  (maximize-full-toggle w 2))

(defun maximize-full-toggle (w #!optional (width 1) (height 1) (x 0) (y 0))
  "Maximize window to full screen size (without frame)"
  (interactive "%W")
  (maxfull-disable-hooks)
  (if (not (assoc w *fullmaxed-windows*))
      (progn
	(setq *fullmaxed-windows*
	      (cons (cons w (window-frame w)) *fullmaxed-windows*))
	(setq *fullmaxed-window-dimensions*
	      (cons (cons w (window-dimensions w))
		    *fullmaxed-window-dimensions*))
	(setq *fullmaxed-window-positions*
	      (cons (cons w (window-position w))
		    *fullmaxed-window-positions*))
	(set-window-frame w '())
	(move-window-to w x y)
	(resize-window-to w 
			  (/ (screen-width) width)
			  (/ (screen-height) height))
	(maxfull-enable-hooks))
    (progn 
      (maxfull-restore-frame w)
      (maxfull-restore-dimensions w)
      (maxfull-restore-positions w))))

;(define-command 'maximize-full-toggle maximize-full #:spec "%W")

(provide 'fullmax)
;; -------------------------------------------------- ;;
