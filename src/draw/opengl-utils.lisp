;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER-WINDOW; -*-
#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

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
                                           (fli::pointerp ,return-vector))
                                      ,return-vector)
                                 (t
                                  (opengl:make-gl-vector ,(canonicalize-type
                                                           (if (listp type) (car type) type))
                                                          ,(if (listp type) (cadr type) 1))))))
             (unwind-protect
              (progn
               (,(ecase (if (listp type) (car type) type)
                        (:boolean 'opengl:gl-get-booleanv)
                        ((:signed-32 :integer) 'opengl:gl-get-integerv)
                        (:float 'opengl::gl-get-floatv))
                ,pname ,gl-vect-var)
               ,(cond ((and (listp type) (not (null return-vector)))
                       `,gl-vect-var)
                  ((listp type)
                   `(values ,@(let ((value-forms nil))
                                (dotimes (i (cadr type))
                                  (push `(opengl:gl-vector-aref ,gl-vect-var ,i)
                                        value-forms))
                                (nreverse value-forms))))
                  ((eq type :boolean)
                   `(let ((raw (opengl:gl-vector-aref ,gl-vect-var 0)))
                      (cond ((zerop raw) nil)
                        ((= raw 1) t)
                        (t (error "~D is not a valid boolean value" raw)))))
                  (t
                   `(opengl:gl-vector-aref ,gl-vect-var 0))))
              (when (null ,return-vector)
                (opengl::free-gl-vector ,gl-vect-var)))))))

;; for debugging
(eval-when (compile)
  (defvar *include-opengl-debugging?* t)
)

(defmacro debug-opengl-print (format-string &rest args)
  (when *include-opengl-debugging?*
    `(when (member "-debug" sys:*line-arguments-list* :test #'string-equal)
       (format *error-output* ,format-string . ,args))))

;;;; for testing flags
(defun gl-enabled? (flag) (if (zerop (opengl:gl-is-enabled flag)) nil t))

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



;;; new layer of opengl drawing primitives which (optional) check & coerce parameter type

(defun ogl-draw-line (x0 y0 x1 y1)
  (opengl:gl-begin opengl:*gl-lines*)
  (opengl:gl-vertex2-f (ogl-type x0 'float) (ogl-type y0 'float))
  (opengl:gl-vertex2-f (ogl-type x1 'float) (ogl-type y1 'float))
  (opengl:gl-end))

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
  (opengl:gl-begin opengl:*gl-line-strip*)
  (do* ((vertices x-and-y-s (cddr vertices))
        (x (car vertices)  (car vertices))
        (y (cadr vertices) (cadr vertices)))
    ((null y)
     (unless (null x) ; both run out @ same time
       (error "Unpaired vertex in ~A" x-and-y-s)))
    (opengl:gl-vertex2-f (ogl-type x 'float) (ogl-type y 'float)))
  (opengl:gl-end))

(defun ogl-draw-rect (x0 y0 x1 y1)
  (opengl:gl-rectf (ogl-type x0 'float) (ogl-type y0 'float)
            (ogl-type x1 'float) (ogl-type y1 'float)))

(defun ogl-draw-point (x y)
  (opengl:gl-begin *gl-points*)
  (opengl:gl-vertex2-f (ogl-type x 'float) (ogl-type y 'float))
  (opengl:gl-end))

;;;; FONTS

(defvar *current-opengl-font* nil
  "set-font sets, char drawing uses this value")

(defvar *current-opengl-font-base-addr* nil)

;; control how much of font to cache on GPU
;; we can tune these on startup if GPU memory is low
;; useful alternative values would be from 32 to 128
;; these values MUST be finalized BEFORE any opengl fonts are created !!!

;; the current font caching scheme is as follows:
;; We will cache for chars based on ISO-8859-1 with the exception that we'll
;; pack the space from #x80 to #x9F with the Windows-1252 glyphs
;; if we need to save GPU memory we can skip charcodes from #x0 to #x20 since those
;; are non printing chars (That's the purpose of *opengl-font-start*)
;; *** TrueType fonts require exsitence of glyph 0 (it's the "missing char glyph") ***
;; Since Lispworks uses unicode strings (in particular, the result of cut & paste
;; from other apps) we have to intercept the unicode char codes for the glyphs
;; in Windows-1252 between #x80 and #x9F
;; That is the job of the function charcode->oglfont-index

(defconstant *opengl-font-start* 0)
(defparameter *opengl-font-end*  #x2123)
(defparameter *opengl-font-cache-end* 255)

(defvar *opengl-font-outline-p* nil)

(defvar *default-char-code* 32) ; should print as a "box" , 32 is SPACE for now

(defvar *unicode-font?* t)

(defun charcode->oglfont-index (charcode)
  (cond ((>= charcode *opengl-font-cache-end*) ; instead of *opengl-font-end*
         ;; most likely unicode, see if there is a translation...
                                               (unicode->oglfont-index charcode))
    (t (-& charcode *opengl-font-start*))))

(defvar *unicode-window-1252*
  '((#x20AC 128)  ; euro
    (#x201A 130)  ; &sbquo low single curved quote, left
    (#x0192 131)  ; f
    (#x201E 132)  ; &bdquo low double curved quote
    (#x2026 133)  ; "..." ellipsis
    (#x2020 134)  ; dagger
    (#x2021 135)  ; double dagger
    (#x02C6 136)  ; circumflex
    (#x2030 137)  ; permille
    (#x0160 138)  ;
    (#x2039 139)  ; &lsaquo single angle quote, left
    (#x0152 140)  ; OE (dipthong)
    (#x017D 142)  ; Z+hacek
    (#x2018 145)  ; &lsquo single curved quote, left
    (#x2019 146)  ; &rsquo single curved quote, right
    (#x201C 147)  ; &ldquo double curved quote, left
    (#x201D 148)  ; &rdquo double curved quote, right
    (#x2022 149)  ; &bull  bullet
    (#x2013 150)  ; &ndash
    (#x2014 151)  ; &mdash
    (#x02DC 152)  ; ~  tilde
    (#x2122 153)  ; TM  trademark
    (#x0161 154)  ; S
    (#x203A 155)  ; &rsaquo single angle quote, right
    (#x0153 156)  ; oe dipthong
    (#x017E 158)  ; z+hacek
    (#x0178 159))); Y diaresis

(defun unicode->oglfont-index (unicode)
  (let ((trans (cadr (assoc unicode *unicode-window-1252* :test #'=))))
    (cond ((null trans) *default-char-code*)
      (t (-& trans *opengl-font-start*)))))

(defmacro do-ofont-chars ((char-code-var &key (end '*opengl-font-end*)) &body body)
  `(do ((,char-code-var *opengl-font-start* (1+& ,char-code-var)))
     ((>=& ,char-code-var ,end))
     . ,body))

(defstruct (opengl-font (:constructor %make-opengl-font)
                        (:print-function %print-opengl-font))
  (native-font nil)   ;
  (dl-base-addr nil)  ; NIL means font is not in GPU cache
  (width nil) ;
  (height 0)
  ;; since opengl char drawing is baseline based, this is a useful
  ;; parameter to have available...
  (ascent 0)
  (widths-array nil)
  )

(defun %print-opengl-font (font stream level)
  (declare (ignore level))
  (cond ((capogi-font? (opengl-font-native-font font))
         (format stream "#<OGLFont ")
         (print-capogi-font-internal (opengl-font-native-font font) stream)
         (format stream "[~A]>" (opengl-font-dl-base-addr font)))
    (t
     (format stream "#<OGLFont ~A [~A]>"
             (opengl-font-native-font font) (opengl-font-dl-base-addr font)))))

(defstruct (ogl-graphics-state (:constructor %make-ogl-graphics-state))
  (color nil)
  (font  nil)
  )

;;; It's taking too long to cache all the fonts on startup so the
;;; new scheme is to resolve the native font into an OpenGL font but
;;; fill in the font info on demand (in particular, the widths array)
;;;
;;; Note: 5/28/2011
;;; Filling the entire unicode space of a font still introduces a perceptable
;;; stutter so the new paradigm will be to fill the ASCII space of the widths
;;; array and fill in the rest character by character on demand
(defun fill-oglfont-parameters (ofont &optional (pane *boxer-pane*))
  (let* ((native-font (opengl-font-native-font ofont))
         (ascent (gp::get-font-ascent pane native-font)))
    (setf (opengl-font-ascent ofont) ascent
          (opengl-font-height ofont) (+ ascent (gp::get-font-descent pane
                                                                     native-font)))
    (cond ((gp::font-fixed-width-p pane native-font)
           (setf (opengl-font-width ofont)
                 (gp::get-char-width pane #\a native-font)))
      (t (let ((widths-array (make-array (- *opengl-font-end*
                                            *opengl-font-start*)
                                         :element-type 'float
                                         ; :element-type 'fixnum
                                         ))
               (maxwid 0))
           (do-ofont-chars (char-code :end *opengl-font-cache-end*)
             (let ((trans-idx (-& char-code *opengl-font-start*))
                   (cw (gp::get-char-width pane (code-char char-code)
                                           native-font)))
               (setf (aref widths-array trans-idx) cw)
               (setq maxwid (max maxwid cw))))
           (setf (opengl-font-width ofont) maxwid)
           (setf (opengl-font-widths-array ofont) widths-array))))
    ofont))


;; should do smarter error handling here
;; for now, allow wgl-use-font to signal the error (:errorp t)
(defun %ogl-cache-font (ofont)
  (let ((ba (cond ((null box::%drawing-array)
                   (opengl:rendering-on (*boxer-pane*) (cache-capogi-font (opengl-font-native-font ofont))))
              (t
               (cache-capogi-font (opengl-font-native-font ofont))))))
    (setf (opengl-font-dl-base-addr ofont) ba))
  ofont)

(defun %ogl-decache-font (ofont)
  (unless (null (opengl-font-dl-base-addr ofont))
    ;; it is possible for a font to be in the cache, but unfilled because of the
    ;; new lazy caching scheme
    (opengl:gl-delete-lists (opengl-font-dl-base-addr ofont) *opengl-font-cache-end*)  ;was *opengl-font-end*
    (setf (opengl-font-dl-base-addr ofont) nil)))

;;; External Interface
;; ogl-set-font
;; ogl-font-height
;; ogl-char-width,height
;; ogl-draw-character
;; ogl-draw-string
;; ogl-string-width, height  (font string)
;;
;; it is now possible for an OpenGL font to not have all its parameters
;; precalculated @ startup so check for, and handle this here

(defun ensure-oglfont-parameters (font)
  (when (and (null (opengl-font-width font))
             (null (opengl-font-widths-array font)))
    ;; font does not have precalculated width(s) info
    (fill-oglfont-parameters font))
  (when (null (opengl-font-dl-base-addr font)) (ogl-cache-font font)))

;; this handles font parameter filling in the editor
;; font parameter filling in sprite graphics is handled by change-graphics-font
(defun ogl-set-font (font)
  (ensure-oglfont-parameters font)
  (setq *current-opengl-font* font
        *current-opengl-font-base-addr* (opengl-font-dl-base-addr font)))

(defmacro with-ogl-font ((font) &body body)
  (let ((oldfont (gensym)))
    `(let ((,oldfont *current-opengl-font*))
       (unwind-protect
        (progn
         (when (null (opengl-font-dl-base-addr ,font)) (ogl-cache-font ,font))
         (let ((*current-opengl-font* ,font)
               (*current-opengl-font-base-addr*
                (opengl-font-dl-base-addr ,font)))
           . ,body))
        (when (null (opengl-font-dl-base-addr ,oldfont))
          (ogl-cache-font ,oldfont))))))

;;; Font cache is a FIFO list of font structs
;;; we should query the OPENGL implemtation and tune some of these numbers
;;; during startup

(defvar *font-cache-size* 32)

(defvar *cached-fonts* nil)

(eval-when (compile)
           (defvar *include-font-debugging* nil)
           )

(defvar *debug-font-caching* nil)

(defmacro ogl-debug (&body forms)
  (when *include-font-debugging*
    `(when *debug-font-caching*
       . ,forms)))

(defun ogl-cache-font (font-struct)
  (ogl-debug (format t "~&===> Caching ~A" font-struct))
  (ogl-debug (format t "  cache= ~A" *cached-fonts*))
  (cond ((>= (length *cached-fonts*) *font-cache-size*)
         ;; decache from GPU...
         (ogl-debug (format t "~& %decaching ~A" (car (last *cached-fonts*))))
         (%ogl-decache-font (car (last *cached-fonts*)))
         ;; remove from font list
         (setq *cached-fonts* (subseq *cached-fonts* 0 (1-& *font-cache-size*)))
         ;; add new
         (push (%ogl-cache-font font-struct) *cached-fonts*))
    (t (push (%ogl-cache-font font-struct) *cached-fonts*)))
  (ogl-debug (format t "~& cache= ~A~&" *cached-fonts*)))

;; useful during debugging....
(defun clear-ogl-font-cache ()
  (dolist (f *cached-fonts*) (%ogl-decache-font f))
  (setq *cached-fonts* nil))

;; returns ascent, height and leading (space between rows)
;; maybe shopuld return width info ?
;; need to figure out what width info is used...
(defun ogl-font-info (font)
  (values (bw::opengl-font-ascent font) (bw::opengl-font-height font)
          (bw::opengl-font-width font) 1))

;; Note: last value is "leading" which is the recommended space between lines

;;; sizes, this can be all done on the CPU side
(defun ogl-char-width (cha &optional (font *current-opengl-font*))
  (let ((wa (opengl-font-widths-array font)))
    (cond ((not (null wa))
           (let ((ccw (aref wa (charcode->oglfont-index (char-code cha)))))
             (cond ((null ccw)
                    (let ((w (gp:get-char-width *boxer-pane* (char-code cha)
                                                (opengl-font-native-font font))))
                      (setf (aref wa (charcode->oglfont-index (char-code cha))) w)
                      w))
               (t ccw))))
      (t (opengl-font-width font)))))

;; the same for both char,string-height
(defun ogl-font-height (font) (opengl-font-height font))
(defun ogl-font-ascent (font) (opengl-font-ascent font))

(defun ogl-string-width (string &optional (font *current-opengl-font*))
  (let ((wa (opengl-font-widths-array font)))
    (cond ((not (null wa))
           (let ((acc 0))
             (dotimes (i (length string))
               (incf acc (aref wa (charcode->oglfont-index (char-code (char string i))))))
             acc))
      (t (* (opengl-font-width font) (length string))))))

(defun ogl-draw-char (char x y)
  (if (member :freetype-fonts *features*)
    (boxer::freetype-draw-char char x y *current-opengl-font* *ogl-current-color-vector* t)
    (progn
     (opengl:gl-raster-pos2-f (ogl-type x 'float) (ogl-type y 'float)) ; (+ y (opengl-font-ascent *current-opengl-font*))
     (opengl:gl-call-list (+ *current-opengl-font-base-addr*
                             (charcode->oglfont-index (char-code char)))))))

(defun ogl-draw-string (text x y)
  (if (member :freetype-fonts *features*)
    (boxer::freetype-draw-char text x y *current-opengl-font* *ogl-current-color-vector*)
    (progn
     (opengl:gl-raster-pos2-f (ogl-type x 'float) (ogl-type (+ y (opengl-font-ascent *current-opengl-font*)) 'float))
     (let* ((base (-& *current-opengl-font-base-addr*
                      ;; this is pretty flaky and will break if there ever is a
                      ;; char-code which is less than *opengl-font-start*
                      *opengl-font-start*)))
       ;; Set up for a string-drawing display list call.
       (opengl:gl-list-base base)
       ;; Draw a string using font display lists.
       (fli:with-foreign-string (ptr elts bytes
                                     :external-format :unicode
                                     :null-terminated-p nil)
                                text
                                (declare (ignore bytes))
                                (opengl:gl-call-lists elts
                                                      opengl:*gl-unsigned-short* ; to match :unicode
                                                      ;opengl:*gl-unsigned-byte*
                                                      ptr))))))

;; useful for debugging as in (dolist (f *cached-fonts*) (d-font f))
(defun d-font (ofont)
  (let* ((nf (bw::opengl-font-native-font ofont))
         (attr (gp:font-description-attributes
                (gp::font-description nf))))
    (format t "~%~A  ~D  ~A,~A: base addr = ~D" (getf attr :name) (getf attr :size)
            (getf attr :weight) (getf attr :slant) (bw::opengl-font-dl-base-addr ofont))))






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
                        (opengl:make-gl-vector :float 4 :contents color)))

(defun %make-ogl-color ()
  (incf *ogl-color-counter*)
  (opengl:make-gl-vector :float 4))

(defun free-ogl-color (color)
  (incf *ogl-color-freed*)
  (opengl::free-gl-vector color))

(defun ogl-convert-color (colorspec)
  (make-ogl-color (color:color-red colorspec)
                  (color:color-green colorspec)
                  (color:color-blue colorspec)))

(defun ogl-color-red   (color) (opengl:gl-vector-aref color 0))
(defun ogl-color-green (color) (opengl:gl-vector-aref color 1))
(defun ogl-color-blue  (color) (opengl:gl-vector-aref color 2))
(defun ogl-color-alpha (color) (opengl:gl-vector-aref color 3))

(defun ogl-set-color (color) (opengl:gl-color4-fv color))

(defun ogl-color= (c1 c2)
  (and (= (opengl:gl-vector-aref c1 0) (opengl:gl-vector-aref c2 0))
       (= (opengl:gl-vector-aref c1 1) (opengl:gl-vector-aref c2 1))
       (= (opengl:gl-vector-aref c1 2) (opengl:gl-vector-aref c2 2))
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

(defun num-slices (radius) (round (* 500 (sqrt radius))))

(defun opengl-draw-circle (cx cy radius &optional filled?)
  (let* ((num-slices (num-slices radius))
         (theta (/ (* 2 pi) num-slices))
         (tangent-factor (tan theta))
         (radial-factor (cos theta))
         (x radius) ; start 3 O'clock
         (y 0))
    (opengl:gl-begin (if filled? opengl:*gl-polygon* opengl:*gl-line-loop*))
    (dotimes (i num-slices)
      (opengl:gl-vertex2-f (coerce (+ x cx) 'single-float) (coerce (+ y cy) 'single-float))
      (let ((tx (- y)) (ty x))
        (setq x (+ x (* tx tangent-factor))
              y (+ y (* ty tangent-factor)))
        (setq x (* x radial-factor)
              y (* y radial-factor))))
    (opengl:gl-end)))

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
    (%draw-rectangle 30 20 100 100 alu-seta *boxer-pane*)
    (bw::swap-buffers *boxer-pane*))

|#
