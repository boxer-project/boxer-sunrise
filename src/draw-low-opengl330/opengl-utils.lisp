;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER-WINDOW; -*-
#|

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+


 Low level prims for dealing with fonts & colors

Modification History (most recent at the top)

 8/12/14 charcode->oglfont-index
 4/23/14 added ogl-font-ascent
 2/23/14 %ogl-decache-font should clear to *opengl-font-cache-end* instead of *opengl-font-end*
 2/20/14 ogl-debug and related defvars....
 2/14/14 added  %print-opengl-font as :print-funtion to opengl-font's
10/19/13 moved *use-capogi-fonts* here, %ogl-use-font checks it to dispatch to capogi caching
 7/12/13 re-enabled ogl- fun
11/27/12 fill-oglfont-parameters: change widths array to be of floats
 8/29/12 new layer of type checking prims: ogl-draw-line, ogl-draw-rect, ogl-set-pen-size
         multiline2
 8/22/12 full conversion to floats, all x,y coords, widths & heights
12/10/11 added d-font (describe) for use in debugging
 7/05/11 %ogl-cache-font - break out special case for null %drawing-array
         because OpenGL internals need to run inside of a rendering-on
 6/23/11 %ogl-cache-font (or ... *boxer-pane*)
 *opengl-font-cache-end*, do-ofont-chars, ogl-char-width, fill-ogl-font-parameters
 2/02/11 break out ensure-oglfont-parameters from ogl-set-font
 4/28/10 added debug-opengl-print and friends
 4/17/10 removed make-opengl-font-no-cache
11/25/09 ogl-reshape coerces to double floats instead of just calls to (float ...)
11/05/09 pixel->color
11/03/09 changed og-draw-string to use :external-format :unicode
 4/22/09 *unicode-window-1252*, charcode->oglfont-index, unicode->oglfont-index,
          make-opengl-font-from-native-font, %ogl-cache-font hack unicode translation to window-1252

|#

(in-package :boxer-opengl)



;;;; FONTS

;;; External Interface
;; ogl-set-font
;; ogl-font-height
;; ogl-char-width,height
;; ogl-draw-character
;; ogl-draw-string
;; ogl-string-width, height  (font string)

;; Note: last value is "leading" which is the recommended space between lines

(defun ogl-char-width (cha &optional (font *current-opengl-font*))
  (let* ((glyph (find-box-glyph cha font boxer::*font-size-baseline*))
         (advance (box-glyph-advance glyph)))
    ;; A number of our fixnum operations will fail if this isn't an integer.
    (floor advance)))

;; the same for both char,string-height
;; (defun ogl-font-height (font)
;;   (* (cadr (boxer::opengl-font-fontspec font)) boxer::*font-size-baseline*))

;; (defun ogl-font-ascent (font)
;;   "sgithens TODO: temporary hack see ogl-font-height, the math for this should be even more different"
;;   (ogl-font-height font))

;; returns ascent, height and leading (space between rows)
;; maybe shopuld return width info ?
;; need to figure out what width info is used...
(defun ogl-font-info (font)
  (values (ogl-font-ascent font) (ogl-font-height font) 1))

(defun ogl-string-width (string &optional (font *current-opengl-font*))
  (let ((total 0))
    (for:for ((i over string))
      (setf total (+ total (ogl-char-width i))))
    total))

(defun ogl-reshape (width height)
  (setf (boxgl-device-projection-matrix bw::*boxgl-device*)
        (create-ortho-matrix width height))
  (gl:viewport 0 0 width height))
