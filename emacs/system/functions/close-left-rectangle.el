;; Emacs function file
;; Close rectangle to the left, opposite of close-rectangle
;; Written by Martin Ebourne

;;;###autoload
(defun close-left-rectangle (start end)
  "Delete all whitespace preceeding a specified column in each line.
The left edge of the rectangle specifies the position in each line
at which whitespace deletion should end.  On each line in the
rectangle, all continuous whitespace finishing at that column is
deleted."
  (interactive "r")
  (operate-on-rectangle '(lambda (startpos begextra endextra)
			   (save-excursion
			     (goto-char startpos)
			     (delete-region (point)
					    (progn
					      (skip-syntax-backward " ")
					      (point)))))
			start end t))
