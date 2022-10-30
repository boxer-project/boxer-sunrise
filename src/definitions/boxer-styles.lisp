;;;;
;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;        Boxer Styles, centrally collected set of fonts and styles to allow for redefintion
;;;;        and future theming support.  When possible going forward, we try to use existing names from
;;;;        HTML CSS, whose boxes, borders, and fonts actually match up fairly closely to what we need
;;;;        in Boxer.
;;;;
;;;;        Currently, just a centrally collected set of defvar's, but will have more "style sheet" type
;;;;        functionality in the future.
(in-package :boxer)

(defvar *mouse-shrink-corner--background-color* #(:RGB 1.0 0.9 0.0))


(defvar *solarized-base03* #(:rgb-hex "#002b36"))
(defvar *solarized-base02* #(:rgb-hex "#073642"))
(defvar *solarized-base01* #(:rgb-hex "#586e75"))
(defvar *solarized-base00* #(:rgb-hex "#657b83"))
(defvar *solarized-base0* #(:rgb-hex "#839496"))
(defvar *solarized-base1* #(:rgb-hex "#93a1a1"))
(defvar *solarized-base2* #(:rgb-hex "#eee8d5"))
(defvar *solarized-base3* #(:rgb-hex "#fdf6e3"))
(defvar *solarized-yellow* #(:rgb-hex "#b58900"))
(defvar *solarized-orange* #(:rgb-hex "#cb4b16"))
(defvar *solarized-red* #(:rgb-hex "#dc322f"))
(defvar *solarized-magenta* #(:rgb-hex "#d33682"))
(defvar *solarized-violet* #(:rgb-hex "#6c71c4"))
(defvar *solarized-blue* #(:rgb-hex "#268bd2"))
(defvar *solarized-cyan* #(:rgb-hex "#2aa198"))
(defvar *solarized-green* #(:rgb-hex "#859900"))

(defun rgb-hex->rgb (color)
  (let ((hexstring (aref color 1)))
    `#(:rgb ,(/ (parse-integer (subseq hexstring 1 3) :radix 16) 255)
            ,(/ (parse-integer (subseq hexstring 3 5) :radix 16) 255)
            ,(/ (parse-integer (subseq hexstring 5 7) :radix 16) 255))))

(defun rgb->rgb-hex (rgb-color)
  (let* ((red (floor (* 255 (aref rgb-color 1))))
        (green (floor (* 255 (aref rgb-color 2))))
        (blue (floor (* 255 (aref rgb-color 3))))
        (hex-string (format nil "#~2,'0x~2,'0x~2,'0x" red green blue)))
  `#(:rgb-hex ,hex-string)))

(defun rgb-hex->ogl (color)
  (bw::ogl-convert-color (rgb-hex->rgb color)))

(defun default-light-theme ()
  (setf *background-color* (rgb-hex->ogl #(:rgb-hex "#FFFFFF"))
        *foreground-color* (rgb-hex->ogl #(:rgb-hex "#000000"))
        *default-border-color* (rgb-hex->ogl #(:rgb-hex "#000000")))
)

(defun solarized-dark-theme ()
  (setf *background-color* (rgb-hex->ogl *solarized-base03*)
        *foreground-color* (rgb-hex->ogl *solarized-base0*)
        *default-border-color* (rgb-hex->ogl *solarized-base1*))
)

(defun solarized-light-theme ()
  (setf *background-color* (rgb-hex->ogl *solarized-base3*)
        *foreground-color* (rgb-hex->ogl *solarized-base00*)
        *default-border-color* (rgb-hex->ogl *solarized-base02*)
  )
)

;;;
;;; Storing styles in plists.  We are going to store our CSS inspired styles in a plist
;;; on boxes, and we'll just call them css-styles even though they aren't real cascading
;;; styles sheets. This will be a plist for now, but we may eventually change to a hashmap
;;; when there are more of them. If that happens, we will still need to dump them as plists
;;; as the current binary boxer-dump-thing doesn't handle hashmaps.
;;;
;;; Current styles and their values:
;;;   - background-color
;;;     Should be stored as #(:rgb-hex "#000000")
;;;   - border-color
;;;     Should be stored as #(:rgb-hex "#000000")
;;;
;;; There will be 2 methods for getting and setting these from boxes.

(defmethod set-css-style ((box plist-subclass) style-name style-value)
  "Sets the css style value by it's name on a box, row, or other plist-subclass"
  (let ((css-styles (getprop box :css-styles '())))
    (setf (getf css-styles style-name) style-value)
    (putprop box css-styles :css-styles)))

(defmethod get-css-style (obj style-name)
  nil)

(defmethod get-css-style ((box plist-subclass) style-name)
  "Sets the css style value by it's name on a box, row, or other plist-subclass"
  (let ((css-styles (getprop box :css-styles '())))
    (getf css-styles style-name)))

(defmethod remove-css-style ((box plist-subclass) style-name)
  "Removes the css style value by it's name on a box, row, or other plist-subclass"
  (let ((css-styles (getprop box :css-styles '())))
    (remf css-styles style-name)
    (putprop box css-styles :css-styles)))

;;;
;;; From looking at the primary other example, which is xref, there appear to be 6 total
;;; hooks that may need to be implemented for this to work. 2 of them are for the dumper
;;; and loader.
;;;

;; The following two are registered in dumper.lisp

(defun css-styles-dump-plist-length (box)
  "If the box plist has a :css-styles entry then the added length will be 2. One for the
  keyword name, and the second for the plist value."
  (if (getprop box :css-styles)
    2
    0))

(defun css-styles-dump-plist-internal (box stream)
  "If the box plist has a :css-styles entry, that will be the keyword, and the value will be
  the corresponding plist, which should be dumpable by dump-boxer-thing."
  (when (getprop box :css-styles)
    (dump-boxer-thing :css-styles stream)
    (dump-boxer-thing (getprop box :css-styles) stream)))

;; The next 4 are currently registered in xfile.lisp

(defun copy-css-styles-special-property (from-box to-box)
  (when (getprop from-box :css-styles)
    (putprop to-box (getprop from-box :css-styles) :css-styles)))

(defun edvc-css-styles-hook (eb vc)
  (let ((css-styles (getf (plist eb) :css-styles)))
    (unless (null css-styles)
      (setf (getf (vc-graphics vc) 'css-styles) (copy-tree css-styles)))))

(defun vcvc-css-styles-hook (ovc vc)
  (let ((css-styles (getf (vc-graphics ovc) 'css-styles)))
    (unless (null css-styles)
      (setf (getf (vc-graphics vc) 'css-styles) (copy-tree css-styles)))))

(defun print-vc-css-styles-hook (vc eb)
  (let ((css-styles (getf (vc-graphics vc) 'css-styles)))
    (unless (null css-styles)
      (putprop eb css-styles :css-styles))))
