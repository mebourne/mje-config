;; Emacs configuration file
;; Customised variables
;; Written by Martin Ebourne

(setq custom-file (concat install-user-base-dir "/custom.el"))

;; Change customisation defaults
(custom-set-variables

 ;; Use clipboard for cutting & pasting
 '(x-select-enable-clipboard t)

 ;; Stop Emacs from changing the major mode when you rename a file
 '(change-major-mode-with-file-name nil)

 ;; Stop Emacs auto extending the file when you press down at eof
 '(next-line-add-newlines nil)

 ;; Revert to traditional move by logical lines behaviour
 '(line-move-visual nil)

 ;; Stop completing onto object files
 '(completion-ignored-extensions (append completion-ignored-extensions
					 (list ".d" ".zwc")))

 ;; Try new indenting
 '(c-tab-always-indent nil)
 '(c-insert-tab-function 'indent-relative)

 ;; Get the time & column number on the info bar. Disable mail flag though
 '(display-time-24hr-format t)
 '(display-time-mail-file t)

 ;; Put scroll bars back on the right side
 '(scroll-bar-mode (quote right))

 ;; Mouse yanks at point, not where mouse is
 '(mouse-yank-at-point t)

 ;; Cursor is stretched to size of underlying character (eg. tab)
 '(x-stretch-cursor t)

 ;; Mark lines past buffer in margin
 '(indicate-empty-lines t)

 ;; Display trailing white space
 '(show-trailing-whitespace t)

 ;; Prefer windows to be split vertically
 '(split-width-threshold nil)

 ;; Give buffers sensible unique filenames
 '(uniquify-buffer-name-style (quote reverse))
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-separator "|")

 ;; Stop hide-show hiding the initial comment block
 '(hs-minor-mode-hook nil)

 ;; UK date style (D/M/Y)
 '(european-calendar-style t)
 '(calendar-time-display-form '(24-hours ":" minutes))

 ;; Desktop saving
 '(desktop-base-file-name (concat ".emacs.desktop." (user-real-login-name)))
 '(desktop-missing-file-warning nil)
 '(desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log$\\|(ftp)\\|^tags\\|^TAGS\\|^pcl-cvs\\.[A-Za-z0-9]+$\\|^\\.newsrc-dribble$\\)")
 '(desktop-load-locked-desktop t)
 '(desktop-restore-eager 2)
 '(desktop-lazy-idle-delay 2)
 '(desktop-save 'if-exists)
 '(desktop-save-mode t)

 ;; Save places in files for when reloading them
 '(save-place-limit 1000)
 '(save-place-version-control (quote never))
 '(save-place t)

 ;; Buffers sync with disk for interworking with rose and studio
 '(auto-revert-interval 5)

 ;; Increase history lengths
 '(history-length 100)

 ;; Show garbage collection
 '(garbage-collection-messages t)

 ;; Increase this to prevent errors from it
 '(max-specpdl-size 3000)

 ;; Single space at end of sentence in fill mode
 '(sentence-end-double-space nil)

 ;; Word wrap width
 '(fill-column 78)

 ;; Dired mode
 '(dired-guess-shell-gnutar "tar")
 '(dired-omit-files "^#\\|^\\.\\|^\\(CVS\\|TAGS\\)$")
 '(dired-omit-files-p t)

 ;; Settings for clearcase version control
 '(clearcase-diff-on-checkin t)
 '(clearcase-make-backup-files t)
 '(clearcase-suppress-checkout-comments t)
 '(clearcase-use-normal-diff t)
 '(clearcase-normal-diff-switches "-uw")

 ;; Ange-ftp default to anonymous ftp and binary xfer
 '(ange-ftp-default-user "ftp")
 '(ange-ftp-binary-file-name-regexp "")

 ;; Tramp default to ssh /bin/sh and su
 '(tramp-default-method "sshx")
 '(tramp-default-method-alist '(("\\`localhost\\'" "\\`root\\'" "su")))

 ;; Get the bracket matching to stay on
 '(blink-matching-paren nil)
 '(show-paren-mode t nil (paren))

 ;; Settings for version control
 '(vc-checkin-on-register t)
 '(vc-directory-exclusion-list (quote ("SCCS" "RCS" "CVS" "lost+found")))
 '(vc-diff-on-checkin t)
 '(vc-cc-use-normal-diff t)
 '(vc-cc-display-branch nil)
 '(vc-default-comment "")
 '(vc-make-backup-files t)

 ;; Nicer form of context diff
 '(diff-switches "-u")

 ;; Dynamic abbreviation
 '(dabbrev-abbrev-skip-leading-regexp "[$@%&]")

 ;; Speedbar. Wider. Sorted.
 '(speedbar-frame-parameters '((minibuffer . nil)
			       (unsplittable . t)
			       (menu-bar-lines . 0)
			       (border-width . 0)
			       (width . 40)))
 '(speedbar-obj-do-check nil)
 '(speedbar-vc-do-check nil)  ; This one due to icons accumulating over time in current version
 '(speedbar-tag-hierarchy-method '(speedbar-trim-words-tag-hierarchy
				   speedbar-prefix-group-tag-hierarchy
				   speedbar-sort-tag-hierarchy))
 '(speedbar-supported-extension-expressions
   (append '(".[ch]\\(\\+\\+\\|pp\\|c\\|h\\|xx\\)?" ".[CH]" ".tex\\(i\\(nfo\\)?\\)?"
	     ".el" ".emacs" ".l" ".lsp" ".p" ".java" ".f\\(90\\|77\\|or\\)?")
	   (if speedbar-use-imenu-flag
	       '(".ada" ".p[lm]" ".tcl" ".m" ".scm" ".pm" ".py" ".g"
		 ;; html is not supported by default, but an imenu tags package
		 ;; is available.  Also, html files are nice to be able to see.
		 ".s?html"
		 "[Mm]akefile\\(\\.in\\)?" ".mk"
		 ".sh"
		 ".xml" ".xsd" ".xslt"
		 ".sql"))))

 ;; JDE
 '(jde-use-font-lock nil)

 ;; Perl
 '(perl-indent-level 2)

 ;; Shell script
 '(sh-indentation 2)
 '(sh-basic-offset 2)
 '(sh-indent-for-do 0)
 '(sh-indent-after-do '+)
 '(sh-indent-comment nil)

 ;; Disable inserting of #! line at top of scripts
 '(executable-insert nil)

 ;; Setup syntax colouring
 '(global-font-lock-mode t nil (font-lock))
 '(font-lock-support-mode (quote jit-lock-mode))
 '(jit-lock-stealth-time 1)

 ;; More descriptive title bar
 '(frame-title-format '("" invocation-name "   " user-login-name "@" system-name
			   "   [%b]"))

 ;; Include Clearcase view in modeline
 '(mode-line-format '("-" mode-line-mule-info mode-line-modified
		      mode-line-frame-identification mode-line-buffer-identification
		      "   " global-mode-string
		      "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"
		      (which-func-mode ("" which-func-format "--"))
		      (line-number-mode "L%l--")
		      (column-number-mode "C%c--")
		      (-3 . "%p")
		      (:eval (let ((view (clearcase-fprop-viewtag default-directory)))
			       (if view
				   (concat "--[" view "]"))))
		      "-%-"))
 )

(custom-set-faces
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "dark green"))
			    (((class color) (background dark)) (:foreground "green"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "blue"))
			      (((class color) (background dark)) (:foreground "dodger blue"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "dodger blue"))
			   (((class color) (background dark)) (:foreground "cyan"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "coral"))
			    (((class color) (background dark)) (:foreground "pale goldenrod"))))
 '(font-lock-warning-face ((((class color) (background light)) (:foreground "red"))
			    (((class color) (background dark)) (:foreground "red"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "dark slate gray"))
			 (((class color) (background dark)) (:foreground "light salmon"))))
 '(font-lock-number-face ((((class color) (background light)) (:foreground "orange"))
			  (((class color) (background dark)) (:foreground "orange"))))
 '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "blue violet"))
				  (((class color) (background dark)) (:foreground "yellow"))))
 '(font-lock-function-name-face ((((class color) (background light)) (:foreground "firebrick"))
				  (((class color) (background dark)) (:foreground "light pink"))))
 '(font-lock-rose-comment-face ((((class color) (background light)) (:foreground "gray"))
				 (((class color) (background dark)) (:foreground "dark green"))))
 '(trailing-whitespace ((t (:background "grey20")))))
; orchid, indian red, dark khaki
