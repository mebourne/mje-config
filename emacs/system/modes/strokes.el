;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne
;; $Id: strokes.el,v 1.2 2002/03/21 10:54:15 mebourne Exp $

(setq strokes-global-map '((((0 . 5)
			     (1 . 5)
			     (2 . 5)
			     (3 . 5)
			     (4 . 5)
			     (5 . 5)
			     (6 . 5)
			     (7 . 5)
			     (8 . 5)
			     (9 . 5)
			     (10 . 5))
			    . switch-to-next-buffer-other-window)
			   (((0 . 2)
			     (1 . 2)
			     (2 . 3)
			     (3 . 3)
			     (4 . 4)
			     (5 . 4)
			     (5 . 5)
			     (6 . 5)
			     (6 . 6)
			     (7 . 6)
			     (7 . 7)
			     (7 . 8)
			     (6 . 8)
			     (5 . 8)
			     (4 . 8)
			     (3 . 8)
			     (2 . 8)
			     (1 . 8)
			     (1 . 7)
			     (1 . 6)
			     (1 . 5)
			     (2 . 5)
			     (3 . 4)
			     (4 . 4)
			     (5 . 4)
			     (6 . 4)
			     (7 . 3)
			     (8 . 3)
			     (9 . 2)
			     (10 . 2))
			    . kill-this-buffer)
			   (((7 . 0)
			     (6 . 0)
			     (5 . 0)
			     (4 . 0)
			     (3 . 0)
			     (2 . 1)
			     (1 . 1)
			     (0 . 2)
			     (0 . 3)
			     (1 . 4)
			     (2 . 4)
			     (3 . 5)
			     (4 . 5)
			     (5 . 6)
			     (6 . 6)
			     (7 . 7)
			     (8 . 7)
			     (9 . 7)
			     (10 . 7)
			     (10 . 8)
			     (10 . 9)
			     (9 . 9)
			     (8 . 9)
			     (7 . 9)
			     (6 . 9)
			     (5 . 9)
			     (4 . 9)
			     (3 . 9)
			     (2 . 9)
			     (1 . 9)
			     (0 . 9))
			    . save-buffer))
      )


;; Strokes prompts to save strokes file on Emacs exit. But user may not have
;; one, using config defaults instead, so extra check in here
(defun strokes-prompt-user-save-strokes-if-present ()
  "As for strokes-prompt-user-save-strokes, but does nothing
if user strokes file does not exist."
  (if (file-exists-p strokes-file)
      (strokes-prompt-user-save-strokes))
  t
  )

;; Need to advise the function since otherwise it repeatedly undoes our
;; changes in Emacs 21 or later
(defadvice strokes-mode (after strokes-mode-remove-prompt activate preactivate)
  "Modify the quit emacs prompt that strokes-mode installs to
work with the emacs config system."
  (remove-hook 'kill-emacs-query-functions
	       'strokes-prompt-user-save-strokes
	       )
  (add-hook 'kill-emacs-query-functions
	    'strokes-prompt-user-save-strokes-if-present
	    ))
