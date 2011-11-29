;; Emacs function file
;; Build the CRUISE source tree
;; Written by Martin Ebourne

;;;###autoload
(defun cruise-build (&optional arg)
  "Build the current CRUISE source tree,
and send the output to a compile window."
  (interactive)
  (if (not (string-match "^\\(.*/com/gs/fw/reg/cruise/\\)" default-directory))
      (error "Not in a CRUISE source tree"))
  (let ((default-directory (substring default-directory (match-beginning 0) (match-end 0))))
    (compile (concat "rsh lnrrdsa01 \"(cd "
		     default-directory
		     "; /usr/local/gnu/bin/make -k)\""))))
