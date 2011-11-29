;; Emacs configuration file
;; Setup for compile
;; Written by Martin Ebourne

;; Modify compilation to understand MSM build script output
(eval-after-load "compile"
  '(progn
     (setq compilation-enter-directory-regexp-alist
	   (append compilation-enter-directory-regexp-alist
		   '(
		     ;; Matches lines printed by the MSM build script
		     ("^Base directory \\(.*\\)$" 1)
		     ("^Entering directory \\(.*\\)$" 1)
		     )))
     (setq compilation-leave-directory-regexp-alist
	   (append compilation-leave-directory-regexp-alist
		   '(
		     ;; Matches lines printed by the MSM build script
		     ("^Leaving directory \\(.*\\)$" 1)
		     )))
     ))
