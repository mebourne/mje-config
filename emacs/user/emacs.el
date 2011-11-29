;; Emacs configuration file
;; .emacs.el, main user startup file
;; Written by Martin Ebourne

(if (file-readable-p "/etc/config/emacs/startup.el")
    (load "/etc/config/emacs/startup")
  (load "~/config/emacs/startup"))
(load custom-file)

;; Enable useful commands which are disabled by default
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Keep cursor on middle line
(setq constrain-percentage 50)

;(setq gud-dbx-directories
;      (list
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/com"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/dom"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/framework"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/idom"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/internal"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/parsers"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/sax"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/sax2"
;       "/home/mebourne/dloads/xml/xerces-c-src1_7_0/src/xercesc/util"
;       ))
