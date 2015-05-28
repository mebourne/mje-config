;; Emacs function file
;; Cut-n-paste coding with style
;; Written by Martin Ebourne

;;;###autoload
(defun yank-with-replace (regexp to-string)
  "Yank from the kill ring and search-replace in the inserted text."
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Replace"
		   (if current-prefix-arg " word" "")
		   " regexp"
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common))))
  (yank)
  (let ((start (region-beginning))
        (end (make-marker)))
    (set-marker end (region-end))
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp end t)
        (replace-match to-string))
      (set-marker end nil)
      )))
