;; Emacs function file
;; Build a BPTA source tree
;; Written by Martin Ebourne
;; $Id: bpta-build.el,v 1.2 2002/04/24 10:16:13 mebourne Exp $

;;;###autoload
(defun bpta-build (command)
  "Build the current BPTA source tree, and send the output to a compile
window.

It will run the command in the Source directory of the current component,
using compile-command by default. It will prompt for compile-command
with prefix arg.

The ClearCase view of the current file will be set automatically."
  (interactive
   (if current-prefix-arg
       (list (read-from-minibuffer "Compile command: "
                                 (eval compile-command) nil nil
                                 '(compile-history . 1)))
     (list (eval compile-command))))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (let ((view (clearcase-fprop-viewtag default-directory)))
    (if (not (and view
		  (string-match "^\\(.*/BPTA[^/]*/\\)" default-directory)))
	(error "Not in a BPTA source tree"))
    (let ((default-directory (concat (substring default-directory (match-beginning 0) (match-end 0))
				     "Source/")))
      (save-some-buffers (not compilation-ask-about-save) nil)
      (compile-internal (concat "cleartool setview -exec '" command "' " view)
			"No more errors")
      (save-excursion
	(set-buffer compilation-last-buffer)
	(make-local-variable 'directory-abbrev-alist)
	(setq directory-abbrev-alist (append directory-abbrev-alist
					     (list (cons "^/vobs/"
							 (concat "/view/" view "/vobs/")))))
      ))))
