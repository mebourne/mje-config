;; Emacs configuration file
;; Advised functions
;; Written by Martin Ebourne
;; $Id: advice.el,v 1.2 2001/05/22 17:12:31 mebourne Exp $

(defadvice switch-to-buffer (before existing-buffers-only activate preactivate)
  "When called interactively switch to existing buffers only, unless 
when called with a prefix argument."
  (interactive 
   (list (read-buffer "Switch to buffer: " (other-buffer) 
                      (null current-prefix-arg)))))

(defadvice switch-to-buffer-other-window (before existing-buffers-only-other-window activate
						 preactivate)
  "When called interactively switch to existing buffers only, unless 
when called with a prefix argument."
  (interactive 
   (list (read-buffer "Switch to buffer in other window: " (other-buffer) 
                      (null current-prefix-arg)))))

(defadvice switch-to-buffer-other-frame (before existing-buffers-only-other-frame activate
						preactivate)
  "When called interactively switch to existing buffers only, unless 
when called with a prefix argument."
  (interactive 
   (list (read-buffer "Switch to buffer in other frame: " (other-buffer) 
                      (null current-prefix-arg)))))

(defadvice find-file (before existing-files-only activate preactivate)
  "When called interactively find existing files only, unless when
called with a prefix argument."
  (interactive
   (list (read-file-name "Find file: " nil nil (null current-prefix-arg)))))

(defadvice find-file-other-window (before existing-files-only-other-window activate preactivate)
  "When called interactively find existing files only, unless when
called with a prefix argument."
  (interactive
   (list (read-file-name "Find file in other window: " nil nil (null current-prefix-arg)))))

(defadvice find-file-other-frame (before existing-files-only-other-frame activate preactivate)
  "When called interactively find existing files only, unless when
called with a prefix argument."
  (interactive
   (list (read-file-name "Find file in other frame: " nil nil (null current-prefix-arg)))))

(defadvice find-alternate-file (before existing-alternate-files-only activate preactivate)
  "When called interactively find existing files only, unless when
called with a prefix argument."
  (interactive
   (list (read-file-name "Find alternate file: " nil nil (null current-prefix-arg)
			 (and buffer-file-name (file-name-nondirectory buffer-file-name))))))

(defadvice dired (before existing-dirs-only activate preactivate)
  "When called interactively find existing files only."
  (interactive
   (reverse
    (list (if current-prefix-arg
	      (read-string "Dired listing switches: "
			   dired-listing-switches)
	    dired-listing-switches)
	  (read-file-name "Dired (directory): " nil default-directory t)))))

(defadvice dired-other-window (before existing-dirs-only-other-window activate preactivate)
  "When called interactively find existing files only."
  (interactive
   (reverse
    (list (if current-prefix-arg
	      (read-string "Dired listing switches: "
			   dired-listing-switches)
	    dired-listing-switches)
	  (read-file-name "Dired in other window (directory): " nil default-directory t)))))

(defadvice dired-other-frame (before existing-dirs-only-other-frame activate preactivate)
  "When called interactively find existing files only."
  (interactive
   (reverse
    (list (if current-prefix-arg
	      (read-string "Dired listing switches: "
			   dired-listing-switches)
	    dired-listing-switches)
	  (read-file-name "Dired in other frame (directory): " nil default-directory t)))))

(defadvice info (before info-any-file activate preactivate)
  "Allow entering of leaf file names which are searched for
on the info path."
  (interactive 
   (list (read-file-name "Info file name (path search on): "))))
