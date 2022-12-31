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

(in-package :boxer-window)

(defun ogl-init (width height)
  (opengl:gl-matrix-mode opengl:*gl-projection*)
  (opengl:gl-load-identity)
  ;; orthographic projection, 0,0 = top,left
  ;; Note:GL-Ortho wants double-floats as args (and insists on the mac)
  (opengl:gl-ortho (coerce 0.0 'double-float)            (coerce (float width) 'double-float)
            (coerce (float height) 'double-float) (coerce 0.0 'double-float)
            (coerce -1.0 'double-float)           (coerce 1.0 'double-float)))

;;; State management (see Red Book appendix B for state vars)
;; Note, type can be either an atomic type of a list of type and length
;; valid types are: :SIGNED-8 :SIGNED-16 :SIGNED-32 :UNSIGNED-8 :UNSIGNED-16
;;                  :UNSIGNED-32 (:DOUBLE :DOUBLE-FLOAT) (:FLOAT :SINGLE-FLOAT)
(defmacro get-opengl-state (pname type &optional return-vector)
  (let ((gl-vect-var (gensym)))
    (flet ((canonicalize-type (raw-type)
                              (if (eq raw-type :boolean) :unsigned-8 raw-type)))
          `(let ((,gl-vect-var (cond ((and (not (null ,return-vector))
                                           (listp ',type)
                                           ; sgithens 2021-03-21 Removing this check now to get rid of
                                           ; fli dependency, all uses of get-opengl-state seem safe in
                                           ; the codebase. Will be refactored as we set up the libre
                                           ; opengl rendering.
                                           ;(fli::pointerp ,return-vector)
                                           )
                                      ,return-vector)
                                 (t
                                  (opengl::make-gl-vector ,(canonicalize-type
                                                           (if (listp type) (car type) type))
                                                          ,(if (listp type) (cadr type) 1))))))
             (unwind-protect
              (progn
               (,(ecase (if (listp type) (car type) type)
                        (:boolean 'opengl::gl-get-booleanv)
                        ((:signed-32 :integer) 'opengl::gl-get-integerv)
                        (:float 'opengl::gl-get-floatv))
                ,pname ,gl-vect-var)
               ,(cond ((and (listp type) (not (null return-vector)))
                       `,gl-vect-var)
                  ((listp type)
                   `(values ,@(let ((value-forms nil))
                                (dotimes (i (cadr type))
                                  (push `(opengl::gl-vector-aref ,gl-vect-var ,i)
                                        value-forms))
                                (nreverse value-forms))))
                  ((eq type :boolean)
                   `(let ((raw (opengl::gl-vector-aref ,gl-vect-var 0)))
                      (cond ((zerop raw) nil)
                        ((= raw 1) t)
                        (t (error "~D is not a valid boolean value" raw)))))
                  (t
                   `(opengl::gl-vector-aref ,gl-vect-var 0))))
              (when (null ,return-vector)
                (opengl::free-gl-vector ,gl-vect-var)))))))

;; for debugging
(eval-when (compile)
  (defvar *include-opengl-debugging?* nil)
)

(defmacro debug-opengl-print (format-string &rest args)
  (when *include-opengl-debugging?*
    `(format *error-output* ,format-string . ,args)))

;;;; for testing flags
(defun gl-enabled? (flag) (if (zerop (opengl::gl-is-enabled flag)) nil t))

;;;;
(eval-when (compile)
  (defvar *opengl-type-checking-included?* t)
)

(defvar *opengl-type-checking-action* :coerce)

(defmacro ogl-type (arg type)
  (cond ((null *opengl-type-checking-included?*) `,arg)
    (t
     `(cond ((eq *opengl-type-checking-action* :error)
             (cond ((typep ,arg ,type) ,arg)
               (t (error "The arg, ~S, is not of type ~S" ',arg ,type))))
        ((eq *opengl-type-checking-action* :coerce)
         (coerce ,arg ,type))
        (t (error "*opengl-type-checking-action*,~S, should be :COERCE or :ERROR"
                  *opengl-type-checking-action*))))))

(defun ogl-flush-draw-line (c-buffer buffer-pos) ; color-buffer)
  "Using the provided C buffer and raw position in the buffer that is filled, interprets
  it as an array of paired vertices for drawing a line.

  Because the buffer-pos is assumed to be the raw position in the buffer, the number of points
  for glDrawArrays is determined by dividing it by 2.
  "
  (opengl::gl-enable-client-state opengl::*gl-vertex-array*)
  ; (opengl::gl-enable-client-state opengl::*gl-color-array*)
  (opengl::gl-disable-client-state opengl::*gl-color-array*)
  (opengl::gl-vertex-pointer 2 opengl::*gl-int* 0 c-buffer)
  ; (opengl::gl-color-pointer 4 opengl::*gl-float* 0 color-buffer)
  (opengl::gl-draw-arrays opengl::*gl-lines* 0 (/ buffer-pos 2)))

(defun ogl-buffer-draw-line (x0 y0 x1 y1 c-buffer buffer-pos) ; color-buffer color-pos)
  "Takes 2 vertices, a C buffer and the current position in the buffer, and copies the
  points in to the buffer to prepare for an glDrawArray call.
  "

  (setf (cffi:mem-aref c-buffer :int buffer-pos) x0)
  (setf (cffi:mem-aref c-buffer :int (+ buffer-pos 1)) y0)
  (setf (cffi:mem-aref c-buffer :int (+ buffer-pos 2)) x1)
  (setf (cffi:mem-aref c-buffer :int (+ buffer-pos 3)) y1)

  ;; 2022-06-23 The current color scheme is very slow still so we are not including it yet.
  ;; It may be the old opengl functions being called, or maybe the number of mem-aref operations...
  ;; the below could probably be done in one bulk call of somesort I think.
  ;; color
  ; (setf (cffi:mem-aref color-buffer :float color-pos)
  ;   (bw::ogl-color-red boxer::*graphics-state-current-pen-color*))
  ; (setf (cffi:mem-aref color-buffer :float (+ color-pos 1))
  ;   (bw::ogl-color-green boxer::*graphics-state-current-pen-color*))
  ; (setf (cffi:mem-aref color-buffer :float (+ color-pos 2))
  ;   (bw::ogl-color-blue boxer::*graphics-state-current-pen-color*))
  ; (setf (cffi:mem-aref color-buffer :float (+ color-pos 3))
  ;   (bw::ogl-color-alpha boxer::*graphics-state-current-pen-color*))

  ; ;; We have to set the color twice, once for each vertice of each line segment
  ; (setf (cffi:mem-aref color-buffer :float (+ color-pos 4))
  ;   (bw::ogl-color-red boxer::*graphics-state-current-pen-color*))
  ; (setf (cffi:mem-aref color-buffer :float (+ color-pos 5))
  ;   (bw::ogl-color-green boxer::*graphics-state-current-pen-color*))
  ; (setf (cffi:mem-aref color-buffer :float (+ color-pos 6))
  ;   (bw::ogl-color-blue boxer::*graphics-state-current-pen-color*))
  ; (setf (cffi:mem-aref color-buffer :float (+ color-pos 7))
  ;   (bw::ogl-color-alpha boxer::*graphics-state-current-pen-color*))
)

(defun ogl-set-pen-size (new)
  (opengl:gl-point-size (ogl-type new 'float))
  (opengl:gl-line-width (ogl-type new 'float)))


;;; note that gl-begin can also be
;; modes can be: *gl-points*, *gl-lines*, *GL-LINE-LOOP*, *GL-LINE-STRIP*,
;; *GL-TRIANGLES*, *GL-TRIANGLE-STRIP*, *GL-TRIANGLE-FAN*, *GL-QUADS*,
;; *GL-QUAD-STRIP*, or *gl-polygon*

(defun ogl-draw-poly (points)
  (opengl:gl-begin opengl:*gl-polygon*)
  (dolist (v points) (opengl:gl-vertex2-f (ogl-type (car v) 'float) (ogl-type (cadr v) 'float)))
  (opengl:gl-end))

;; used directly
(defun boxer::multiline2 (&rest x-and-y-s)
  ;; sgithens TODO 2022-12-30 Set this up properly to be a single draw arrays call rather
  ;;                          than a series of single line draws.
  (let ((prev-x nil)
        (prev-y nil))
    (do* ((vertices x-and-y-s (cddr vertices))
          (x (car vertices)  (car vertices))
          (y (cadr vertices) (cadr vertices)))
      ((null y)
      (unless (null x) ; both run out @ same time
        (error "Unpaired vertex in ~A" x-and-y-s)))
      (when prev-x
        (boxer::gl-add-line *boxgl-device* prev-x prev-y x y))
      (setf prev-x x prev-y y))))

(defun ogl-draw-point (x y)
  (opengl:gl-begin *gl-points*)
  (opengl:gl-vertex2-f (ogl-type x 'float) (ogl-type y 'float))
  (opengl:gl-end))

;;;; FONTS


(defstruct (ogl-graphics-state (:constructor %make-ogl-graphics-state))
  (color nil)
  (font  nil))

;; this handles font parameter filling in the editor
;; font parameter filling in sprite graphics is handled by change-graphics-font
;; I believe this is only being used in ogl-set-font... below and can probably be removed
(defun ogl-set-font (font)
  (setq *current-opengl-font* font))

(defun set-font-info (x)
  (let* ((font-no (if x x *normal-font-no*))
         (system-font (boxer::find-cached-font font-no)))
    (ogl-set-font system-font)
    (multiple-value-bind (ascent height  leading)
                         (ogl-font-info system-font)
                         (setq %drawing-font-cha-ascent ascent
                               %drawing-font-cha-hei (+ height leading)))
    font-no))

;;; External Interface
;; ogl-set-font
;; ogl-font-height
;; ogl-char-width,height
;; ogl-draw-character
;; ogl-draw-string
;; ogl-string-width, height  (font string)

(defmacro with-ogl-font ((font) &body body)
  (let ((oldfont (gensym)))
    `(let ((,oldfont *current-opengl-font*))
       (unwind-protect
        (progn
         (let ((*current-opengl-font* ,font))
           . ,body))))))

(eval-when (compile)
           (defvar *include-font-debugging* nil)
           )

(defvar *debug-font-caching* nil)

(defmacro ogl-debug (&body forms)
  (when *include-font-debugging*
    `(when *debug-font-caching*
       . ,forms)))

;; Note: last value is "leading" which is the recommended space between lines

;;; sizes, this can be all done on the CPU side
(defun ogl-char-width (cha &optional (font *current-opengl-font*))
  (let* ((cha-pixmap (boxer::find-freetype-pixmap cha font *ogl-current-color-vector* boxer::*font-size-baseline*))
        (width (opengl::ogl-pixmap-width cha-pixmap)))
    ;; TODO This could potentially be the glyph advance value, but so far the value of
    ;; that and the pixmap widths seem the same.
    width))

;; the same for both char,string-height
(defun ogl-font-height (font)
  "sgithens TODO: temporary hack until we draw the proper values from freetype"
  (let* ((cha-pixmap (boxer::find-freetype-pixmap "A" font *ogl-current-color-vector* boxer::*font-size-baseline*))
        (height (opengl::ogl-pixmap-height cha-pixmap)))
    height))

(defun ogl-font-ascent (font)
  "sgithens TODO: temporary hack see ogl-font-height, the math for this should be even more different"
  (ogl-font-height font))

;; returns ascent, height and leading (space between rows)
;; maybe shopuld return width info ?
;; need to figure out what width info is used...
(defun ogl-font-info (font)
  (values (ogl-font-ascent font) (ogl-font-height font) 1))

(defun ogl-string-width (string &optional (font *current-opengl-font*))
  (let* ((cha-pixmap (boxer::find-freetype-pixmap string font *ogl-current-color-vector* boxer::*font-size-baseline*))
        (width (opengl::ogl-pixmap-width cha-pixmap)))
    ;; TODO This should actually be the glyph advance value
    width))

(defun ogl-draw-char (char x y)
  (boxer::freetype-draw-char char x y *current-opengl-font* *ogl-current-color-vector* boxer::*font-size-baseline* t))

(defun ogl-draw-string (text x y)
  (boxer::freetype-draw-char text (floor x) (floor y) *current-opengl-font* *ogl-current-color-vector* boxer::*font-size-baseline*))

;;;; COLORS

;; we'll use opengl vectors as the primary color object, this allows us to
;; bind and pass them around as they need to be in the upper level boxer code

(defvar *ogl-current-color-vector*) ; init'd in start-boxer (boxwin-opengl.lisp)

(defvar *ogl-color-counter* 0)
(defvar *ogl-color-freed* 0)

(defun make-ogl-color (r g b &optional (alpha 1.0))
  (incf *ogl-color-counter*)
  (box::with-stack-list (color (coerce r 'single-float)
                               (coerce g 'single-float)
                               (coerce b 'single-float)
                               (coerce alpha 'single-float))
                        (opengl::make-gl-vector :float 4 :contents color)))

(defun %make-ogl-color ()
  (incf *ogl-color-counter*)
  (opengl::make-gl-vector :float 4))

(defun free-ogl-color (color)
  (incf *ogl-color-freed*)
  (opengl::free-gl-vector color))

(defun ogl-convert-color (colorspec)
  "Takes an RGB spec vector and returns an opengl 4f vector with RGBA values, with alpha at 1.0
  Colorspec is an RGB vector like this (for orange): #(:RGB 1.0 0.6470585 0.0)"
  (make-ogl-color (svref colorspec 1)
                  (svref colorspec 2)
                  (svref colorspec 3)))

(defun ogl-color-red   (color) (opengl:gl-vector-aref color 0))
(defun ogl-color-green (color) (opengl:gl-vector-aref color 1))
(defun ogl-color-blue  (color) (opengl:gl-vector-aref color 2))
(defun ogl-color-alpha (color) (opengl:gl-vector-aref color 3))

(defun ogl-set-color (color) (opengl:gl-color4-fv color))

(defun float-precision= (a b &optional (precision 0.001))
  (if (zerop a) (zerop b)  (< (abs (/ (- a b) a)) precision)))

(defun ogl-color= (c1 c2)
  "Compares an ogl-color to one another to see if they are the same.
  For all practical purposes we're using the usual color range of 0 - 255 (a bit larger than that),
  so only that portion of a float will be considered, regardless of how
  many trailing decimal points each RGB float component as it has."
  (and (float-precision= (opengl:gl-vector-aref c1 0) (opengl:gl-vector-aref c2 0))
       (float-precision= (opengl:gl-vector-aref c1 1) (opengl:gl-vector-aref c2 1))
       (float-precision= (opengl:gl-vector-aref c1 2) (opengl:gl-vector-aref c2 2))
       ;; compare alpha values ?
       ))


;; resource pool for gl-colors used in maintaining-ogl-color
(defvar *ogl-color-pool* nil)

(defvar *initial-ogl-color-pool-size* 20)

(defun initialize-ogl-color-pool ()
  (dotimes (i *initial-ogl-color-pool-size*) (push (%make-ogl-color) *ogl-color-pool*)))

(defun allocate-ogl-color () (or (pop *ogl-color-pool*) (%make-ogl-color)))

(defun ogl-current-color (&optional (vector *ogl-current-color-vector*))
  (get-opengl-state opengl:*gl-current-color* (:float 4) vector))

(defun deallocate-ogl-color (color) (push color *ogl-color-pool*))

(defmacro maintaining-ogl-color (&body body)
  (let ((old-color (gensym)))
    `(let ((,old-color (ogl-current-color (allocate-ogl-color))))
       (unwind-protect
        (progn . ,body)
        (unless (ogl-color= ,old-color (ogl-current-color))
          (ogl-set-color ,old-color))
        (deallocate-ogl-color ,old-color)))))

(defun print-color (ogl-color)
  (format t "<OGL-Color R:~3F G:~3F B:~3F alpha:~3F>" (ogl-color-red ogl-color)
          (ogl-color-green ogl-color) (ogl-color-blue ogl-color)
          (ogl-color-alpha ogl-color)))

(defun ogl-report (&optional clear?)
  (format t "~&~D OGL colors allocated, ~D freed, ~D leaked"
          bw::*ogl-color-counter* bw::*ogl-color-freed*
          (- bw::*ogl-color-counter* bw::*ogl-color-freed*))
  (when clear? (setq bw::*ogl-color-counter* 0 bw::*ogl-color-freed* 0)))

; the gl-viewport is conditionalized with:
;  (when #+Win32 (win32:is-window-visible
;                 (win32:pane-hwnd (capi-internals:representation canvas)))
;	#-Win32 T
; in example code

(defun ogl-reshape (width height)
  (opengl:gl-viewport 0 0 width height) ; x y width height (ints)
  (debug-opengl-print "~%OGL Reshape (~D, ~D)" width height)
  (opengl:gl-matrix-mode opengl:*gl-projection*)
  (opengl:gl-load-identity)
  ;; orthographic projection, 0,0 = top,left
  ;; Note:GL-Ortho wants double-floats as args (and insists on the mac)
  (opengl:gl-ortho (coerce 0.0 'double-float)            (coerce (float width) 'double-float)
            (coerce (float height) 'double-float) (coerce 0.0 'double-float)
            (coerce -1.0 'double-float)           (coerce 1.0 'double-float)))


;;; pixel conversion
;; color values are floats between 0.0 and 1.0
(defun float-color-to-byte-value (value)
  (round (* 255 (/ value 1.0))))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun opengl::color->pixel (color)
  (dpb (float-color-to-byte-value (ogl-color-alpha color))
       opengl::*gl-rgba-rev-alpha-byte*
       (dpb (float-color-to-byte-value (ogl-color-blue color))
            opengl::*gl-rgba-rev-blue-byte*
            (dpb (float-color-to-byte-value (ogl-color-green color))
                 opengl::*gl-rgba-rev-green-byte*
                 (float-color-to-byte-value (ogl-color-red color))))))

(defun opengl::pixel->color (pixel)
  (make-ogl-color (/ (ldb opengl::*gl-rgba-rev-red-byte* pixel)   255.0)
                  (/ (ldb opengl::*gl-rgba-rev-green-byte* pixel) 255.0)
                  (/ (ldb opengl::*gl-rgba-rev-blue-byte* pixel)  255.0)
                  (/ (ldb opengl::*gl-rgba-rev-alpha-byte* pixel) 255.0)))

;;; circle, (eventually) arcs, ellipses
;;; lisp crib of http://slabode.exofire.net/circle_draw.shtml
;;; should be moved to opengl directory

(defun num-slices (radius) (round (* 10 (sqrt radius))))

(defun opengl-draw-arc (cx cy radius start-angle arc-angle &optional filled?)
  (let* ((num-slices (round (* (num-slices radius) (/ arc-angle (* 2 pi)))))
         (theta (/ arc-angle (1- num-slices)))
         (tangent-factor (tan theta))
         (radial-factor (cos theta))
         (x (* radius (cos start-angle)))
         (y (* radius (sin start-angle))))
    (opengl:gl-begin (if filled? opengl:*gl-polygon* opengl:*gl-line-strip*))
    (when filled? (opengl:gl-vertex2-f (coerce cx 'single-float) (coerce cy 'single-float)))
    (dotimes (i num-slices)
      (opengl:gl-vertex2-f (coerce (+ x cx) 'single-float) (coerce (+ y cy) 'single-float))
      (let ((tx (- y)) (ty x))
        (setq x (+ x (* tx tangent-factor))
              y (+ y (* ty tangent-factor)))
        (setq x (* x radial-factor)
              y (* y radial-factor))))
    (opengl:gl-end)))

#|

(drawing-on-window (*boxer-pane*)
    (%draw-rectangle 30 20 100 100)
    (bw::swap-buffers *boxer-pane*))

|#
