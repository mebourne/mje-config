;; Emacs function file
;; Build an STS source tree
;; Written by Martin Ebourne
;; $Id: sts-build.el 725 2004-03-04 22:10:48Z martin $

(require 'compile)

;;;###autoload
(defun build (command)
  "Build the current source tree, and send the output to a compile
window.

It will run the command in the first directory found with a Makefile,
starting from the current directory and going up to each parent,
using compile-command by default. It will prompt for compile-command
with prefix arg."
  (interactive
   (if current-prefix-arg
       (list (read-from-minibuffer "Compile command: "
                                 (eval compile-command) nil nil
                                 '(compile-history . 1)))
     (list (eval compile-command))))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (let ((dir default-directory))
    (while (and dir (not (file-exists-p (concat dir "Makefile"))))
      (setq dir (file-name-directory (directory-file-name dir))))
    (if dir
	(let ((default-directory dir))
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (compilation-start command))
      (error "Not in a source tree")
      )))
