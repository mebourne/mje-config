;; Emacs function file
;; Eliminate trailing spaces at ends of lines
;; Written by Martin Ebourne

;;;###autoload
(defun eliminate-trailing-spaces ()
  "Eliminate whitespace at ends of lines. Doesn't eliminate
space on current line to ensure the cursor doesn't move."
  (interactive)
  (save-excursion
    (let ((oldline (count-lines (point-min) (point))))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
	(if (/= oldline (count-lines (point-min) (point)))
	    (delete-region (match-beginning 0) (point))))
      ))
  )

;;;###autoload
(defun eliminate-trailing-spaces-ignore-comments ()
  "Eliminate whitespace at ends of lines. Doesn't eliminate
space on current line to ensure the cursor doesn't move,
nor on C++ comment lines. (Rational Rose puts extraneous space
 on comments and removing it causes problems.)"
  (interactive)
  (save-excursion
    (let ((oldline (count-lines (point-min) (point))))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
	(if (/= oldline (count-lines (point-min) (point)))
	    (let ((from (match-beginning 0))
		  (to (point)))
	      (beginning-of-line)
	      (if (re-search-forward "//" from t)
		  (beginning-of-line 2)
		(delete-region from to)
	      ))))
      ))
  )
