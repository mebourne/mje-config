;; Emacs configuration file
;; Customised variables
;; Written by Martin Ebourne
;; $Id: custom.el,v 1.3 2001/05/16 17:14:26 mebourne Exp $

(setq custom-file (concat install-user-base-dir "/custom.el"))

;; Change customisation defaults
(custom-set-variables

 ;; Stop Emacs from changing the major mode when you rename a file
 '(change-major-mode-with-file-name nil)

 ;; Stop Emacs auto extending the file when you press down at eof
 '(next-line-add-newlines nil)

 ;; Stop completing onto object files
 '(completion-ignored-extensions (append completion-ignored-extensions
					 (list ".d")))

 ;; Try new indenting
 '(c-tab-always-indent nil)
 '(c-insert-tab-function 'indent-relative)

 ;; Get the time & column number on the info bar. Disable mail flag though
 '(display-time-24hr-format t)
 '(display-time-mail-file t)

 ;; Put scroll bars back on the right side
 '(scroll-bar-mode (quote right))

 ;; Resize the minibuffer to show all contents as required. Doesn't work in customize
 ;'(resize-minibuffer-mode t)
 '(resize-minibuffer-window-max-height 3)

 ;; Mouse yanks at point, not where mouse is
 '(mouse-yank-at-point t)

 ;; Give buffers sensible unique filenames
 '(uniquify-buffer-name-style (quote reverse))
 '(uniquify-after-kill-buffer-p nil)
 '(uniquify-separator "|")

 ;; Stop hide-show hiding the initial comment block
 '(hs-minor-mode-hook nil)

 ;; UK date style (D/M/Y)
 '(european-calendar-style t)
 '(calendar-time-display-form '(24-hours ":" minutes))

 ;; Desktop
 '(desktop-missing-file-warning t)
 '(desktop-buffers-not-to-save "\\(^nn\\.a[0-9]+\\|\\.log$\\|(ftp)\\|^tags\\|^TAGS\\|^pcl-cvs\\.[A-Za-z0-9]+$\\|^\\.newsrc-dribble$\\)")

 ;; Save places in files for when reloading them
 '(save-place-limit 1000)
 '(save-place-version-control (quote never))
 '(save-place t)

 ;; Buffers sync with disk for interworking with rose and studio
 '(global-auto-revert-mode t)
 '(auto-revert-interval 30)

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

 ;; Ange-ftp default to anonymous ftp and binary xfer
 '(ange-ftp-default-user "ftp")
 '(ange-ftp-binary-file-name-regexp "")

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

 ;; Strokes settings
 '(strokes-modeline-string "")
 '(strokes-minimum-match-score 1000)
 '(strokes-grid-resolution 11)
 '(strokes-use-strokes-buffer nil)
 '(strokes-file (concat install-user-base-dir "/strokes.el"))

 ;; Speedbar. Wider. Sorted.
 '(speedbar-frame-parameters '((minibuffer)
			       (width . 40)
			       (border-width . 0)
			       (menu-bar-lines . 0)
			       (unsplittable . t)))
; '(speedbar-tag-hierarchy-method '(prefix-group trim-words sort))

 ;; JDE
 '(jde-use-font-lock nil)

 ;; Perl
 '(perl-indent-level 2)

 ;; Disable inserting of #! line at top of scripts
 '(executable-insert nil)

 ;; Setup syntax colouring
 '(global-font-lock-mode t nil (font-lock))
 '(font-lock-support-mode (quote lazy-lock-mode))
 '(lazy-lock-stealth-nice 0.25)
 '(lazy-lock-stealth-lines 50)
 '(lazy-lock-stealth-time 1)
 '(lazy-lock-minimum-size 5120)
 '(lazy-lock-defer-time 0)
 '(lazy-lock-stealth-load 75)

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
		      (vc-cc-pwv ("--[" vc-cc-pwv "]")) "-%-"))
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
 )
; orchid, indian red, dark khaki
