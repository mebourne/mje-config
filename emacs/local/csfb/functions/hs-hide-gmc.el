;; Emacs function file
;; Hide GMC logging code
;; Written by Martin Ebourne

;;;###autoload
(defun hs-hide-gmc ()
  "Hide GMC logging code to make source easier to read.
Can be re-displayed using hs-show-all."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "if *( *m_pclTrace")
      (let ((start (match-beginning 0))
	    (end (re-search-forward ";"))
	    )
	(overlay-put (hs-flag-region start end 'code) 'hs-ofs 0)
	)
      ))
  )
