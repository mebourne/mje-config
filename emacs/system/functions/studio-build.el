;; Emacs function file
;; Build the current Visual Studio source tree
;; Written by Martin Ebourne
;; $Id: studio-build.el,v 1.1 2001/05/11 17:31:38 mebourne Exp $

;;;###autoload
(defun studio-build (&optional arg)
  "Build the current Visual Studio source tree,
and send the output to a compile window."
  (interactive)
  (if (not (string-match "^\\(.*/Source/\\)" default-directory))
      (error "Not in a Visual Studio source tree"))
  (let ((default-directory (substring default-directory (match-beginning 0) (match-end 0))))
    (compile (concat "nmake -cs"))))
