;; Emacs configuration file
;; Extra font lock faces
;; Written by Martin Ebourne
;; $Id: faces.el 792 2003-09-22 11:47:18Z martin $

(defvar font-lock-number-face	'font-lock-number-face
  "Face name to use for numbers.")

(defface font-lock-number-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "OrangeRed"))
    (t (:bold t :italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'font-lock-highlighting-faces)

(defvar font-lock-rose-comment-face	'font-lock-rose-comment-face
  "Face name to use for Rational Rose comments.")

(defface font-lock-rose-comment-face
  '((((class grayscale) (background light))
     (:foreground "LightGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :bold t :italic t))
    (((class color) (background light)) (:foreground "Firebrick"))
    (((class color) (background dark)) (:foreground "OrangeRed"))
    (t (:bold t :italic t)))
  "Font Lock mode face used to highlight Rational Rose comments."
  :group 'font-lock-highlighting-faces)
