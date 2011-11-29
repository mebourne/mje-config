;; Emacs configuration file
;; Gnus setup file
;; Written by Martin Ebourne

(setq gnus-select-method
      '(nnml "private"))
(setq nnmail-split-methods
      '(("sysinfo" "^Subject: \\(SMP Totals\\|Outstanding SMPs\\|Checked out files\\|check_all_merges\\)")
	("sysinfo" "^From: <lheso>")
	("fixes" "^Subject: SMP:.*_fixed_")
	("resets" "^Subject: SMP:.*_reset_")
	("committed" "^Subject: .*build committed")
	("humour" "^To: .*Jokes_LN")
	("humour" "^To: .*[Ll]iffe.com")  ;; Some random person on andyg's list
	("humour" "^Cc: .*[Ll]iffe.com")  ;; Some random person on andyg's list
	("offsick" "^To: .*OFFSICK")
	("offsick" "^Subject: [^Rr].*ttendance$")
	("traces" "^Subject: Shared.*has died")
	("traces" "^Apparently-To: lh")
	("personal" "Ebourne")
	("personal" "martine")
	("misc" "")))
(setq nnmail-crosspost nil)
(setq gnus-message-archive-group
      '((if (message-news-p)
	    "sent-news"
	  (concat "sent-mail-" (format-time-string "%Y-%m" (current-time))))))
(setq gnus-move-split-methods '(("" "reference")))
(setq gnus-suppress-duplicates t)
