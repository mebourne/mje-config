;; Emacs configuration file
;; Setup for cc-mode
;; Written by Martin Ebourne
;; $Id$

;; Map _ as a word character to make faster font lock regexps work correctly. Not needed for
;; slow font lock regexps
(add-hook 'c-initialization-hook
	  (function (lambda ()
		      (modify-syntax-entry ?_ "w" c-mode-syntax-table)
		      (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
		      (modify-syntax-entry ?_ "w" java-mode-syntax-table)
		      )))

;; C & C++
(setq auto-mode-alist
      (append '(("\\.sq[cC]\\'" . c++-mode)
		("\\.inl\\'"   . c++-mode)
		("\\.ipp\\'"   . c++-mode)
		("\\.idl\\'"   . c++-mode)
		) auto-mode-alist))

(add-hook 'c-mode-common-hook
	  (function (lambda ()
		      (set (make-local-variable 'dabbrev-case-fold-search) nil)
		      (set (make-local-variable 'dabbrev-case-replace) nil)
		      (hs-setup)
		      (c-toggle-auto-state t)
		      (c-toggle-hungry-state t)
		      )))

;; Add our styles
(c-add-style "martin"
	     '((c-basic-offset . 2)
	       (c-offsets-alist . ((access-label      . -1)
				   (case-label        . +)
				   (statement-cont    . c-lineup-math)
				   (substatement-open . 0)
				   ))
	       (c-hanging-braces-alist . ((defun-open before)
					  (defun-close before)
					  (class-open before)
					  (class-close before)
					  (inline-open)
					  (inline-close)
					  (block-open before)
					  (block-close before)
					  (substatement-open before)
					  (statement-case-open before)
					  (extern-lang-open before)
					  (extern-lang-close before)
					  (brace-list-open)
					  (brace-list-close before)
					  (brace-list-intro)
					  (brace-list-entry)
					  ))
	       (c-hanging-semi&comma-criteria . nil)
	       ;;(c-cleanup-list . (empty-defun-braces))
	       (c-electric-pound-behavior . 'alignleft)
	       (comment-column . 45)
	       (fill-column . 78)
	       (indent-tabs-mode . nil)
	       ))
(c-add-style "martin-java"
	     '("java"
	       (c-basic-offset . 2)
	       (c-offsets-alist . ((case-label          . +)
				   (substatement-open   . 0)
				   (statement-case-open . 0)
				   (statement-cont      . c-lineup-math)
				   ))
	       (c-hanging-braces-alist . ((defun-open before)
					  (defun-close before)
					  (class-open before)
					  (class-close before)
					  (inline-open before)
					  (inline-close before)
					  (block-open before)
					  (block-close before)
					  (substatement-open before)
					  (statement-case-open before)
					  (extern-lang-open before)
					  (extern-lang-close before)
					  (brace-list-open)
					  (brace-list-close before)
					  (brace-list-intro)
					  (brace-list-entry)
					  ))
	       (c-hanging-semi&comma-criteria . nil)
	       (c-electric-pound-behavior . 'alignleft)
	       (comment-column . 45)
	       (fill-column . 78)
	       (indent-tabs-mode . nil)
	       ))
(c-add-style "pear"
	     '((c-basic-offset . 4)
	       (c-offsets-alist . ((access-label      . -1)
				   (block-open        . 0)
				   (case-label        . 0)
				   (statement-cont    . c-lineup-math)
				   (substatement-open . 0)
				   ))
	       (c-hanging-braces-alist . ((defun-open before)
					  (defun-close before)
					  (class-open)
					  (class-close before)
					  (inline-open before)
					  (inline-close before)
					  (block-open before)
					  (block-close before)
					  (substatement-open)
					  (statement-case-open before)
					  (extern-lang-open before)
					  (extern-lang-close before)
					  (brace-list-open)
					  (brace-list-close before)
					  (brace-list-intro)
					  (brace-list-entry)
					  ))
	       (c-hanging-semi&comma-criteria . nil)
	       (c-electric-pound-behavior . 'alignleft)
	       (comment-column . 45)
	       (fill-column . 78)
	       (indent-tabs-mode . nil)
	       (tab-width . 4)
	       ))
(c-add-style "boxbackup"
	     '("martin"
	       (tab-width . 2)
	       (indent-tabs-mode . t)
	       ))

;; Set up our C style
(custom-set-variables
 '(c-default-style '((java-mode . "martin-java")
		     (php-mode  . "pear")
		     (other     . "martin")
		     ))
 )
