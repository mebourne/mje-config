;; Emacs function file
;; Modified yank to make it actually work like the text claims it does
;; Written by Martin Ebourne
;; $Id: yank.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

(defun yank (&optional arg)
  "Reinsert the last stretch of killed text.
More precisely, reinsert the stretch of killed text most recently
killed OR yanked.  Put point at end, and set mark at beginning.
With just C-u as argument, same but put point at beginning (and mark at end).
With argument N, reinsert the Nth most recently killed stretch of killed
text.
See also the command \\[yank-pop]."
  (interactive "*P")
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (push-mark (point))

  (cond ((listp arg) 0)
	((eq arg '-) -1)
	(t (setq kill-ring-yank-pointer kill-ring)))
  
  (insert (current-kill (cond
			 ((listp arg) 0)
			 ((eq arg '-) -1)
			 (t (1- arg)))))
  (if (consp arg)
      ;; This is like exchange-point-and-mark, but doesn't activate the mark.
      ;; It is cleaner to avoid activation, even though the command
      ;; loop would deactivate the mark because we inserted text.
      (goto-char (prog1 (mark t)
		   (set-marker (mark-marker) (point) (current-buffer)))))
  ;; If we do get all the way thru, make this-command indicate that.
  (setq this-command 'yank)
  nil)