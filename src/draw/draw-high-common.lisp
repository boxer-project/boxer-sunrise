;;;;; -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER; Base:8.-*-
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
;;;;   This file contains the low level drawing primitives which are
;;;;   machine independent.  This file is meant to coexist with various
;;;;   "xxx-draw-low" files which are the machine specific primitives.
;;;;
(in-package :boxer)

(defclass boxgl-device ()
  ((pen-size  :accessor boxgl-device-pen-size)
   (pen-color :accessor boxgl-device-pen-color) ; current color in #(:rgb 1.0 1.0 1.0 1.0) format
   (line-stipple :accessor line-stipple :initform nil
     :documentation "Boolean to indicate if we're currently drawing dashed lines.")

   (projection-matrix :accessor boxgl-device-projection-matrix)
   (transform-matrix :accessor boxgl-device-transform-matrix
                     :initform (3d-matrices:meye 4))
   (model-matrix :accessor boxgl-device-model-matrix
                 :initform (3d-matrices:meye 4))))

;;;
;;; Graphics-canvas is for drawing on turtle/graphics boxes
;;;

(defclass graphics-canvas ()
  ((pixmap :initform nil :accessor graphics-canvas-pixmap :initarg :pixmap
    :documentation "The ogl-pixmap with the backing texture-id and dimensions.")
   (op-count :initform 0 :accessor op-count
    :documentation "A counter that can be used to document the number of graphics
                    command operations have been applied to this texture. In a graphics command
                    display list, we can use this to pick back up painting to the texture without
                    repeating previous operations.")
   (pen-color-cmd :initform nil :accessor graphics-canvas-pen-color-cmd
    :documentation "Vector of the last run gdispl command for changing the color, so it can be turned back
                    on when picking up in the list without starting from the beginning.")
   (pen-size-cmd  :initform nil :accessor graphics-canvas-pen-size-cmd
    :documentation "Vector of the last run gdispl command for changing the pen size, so it can be turned back
                    on when picking up in the list without starting from the beginning.")
   (pen-font-cmd  :initform nil :accessor graphics-canvas-pen-font-cmd
    :documentation "Vector of the last run gdispl command for changing the font, so it can be turned back
                    on when picking up in the list without starting from the beginning.")))

(defmethod enable ((self graphics-canvas) &key (device bw::*boxgl-device*))
  ""
  (error "graphics-canvas enable needs an toolkit specific implementation"))

(defmethod disable ((self graphics-canvas) &key (device bw::*boxgl-device*))
  (error "graphics-canvas disable needs an toolkit specific implementation"))

(defmethod get-canvas-mesh (obj)
  (error "graphics-canvas disable needs an toolkit specific implementation"))

(defun buffer-canvas-mesh (device mesh pixmap wid hei)
  (boxer-opengl::%buffer-canvas-mesh device mesh pixmap wid hei))

(defun draw-canvas-mesh (mesh pixmap)
  (boxer-opengl::%draw-canvas-mesh mesh pixmap))

(defmethod clear-graphics-canvas (obj)
  ""
  (error "graphics-canvas clear-graphics-canvas needs an toolkit specific implementation"))

(defmethod clear (obj)
  ""
  (error "graphics-canvas clear needs an toolkit specific implementation"))

(defmethod resize (obj wid hei)
  (error "graphics-canvas resize needs an toolkit specific implementation"))

;;;
;;; DRAWING-ON-WINDOW macros
;;;

(defmacro drawing-on-window ((window) &body body)
  "DRAWING-ON-WINDOW is an &body macro which all the drawing macros in this
must be called inside of. It basically prepares the window to be drawn on
and binds all the magic variables that the drawing macros need including
the bootstrapping of the clipping and coordinate scaling variables."
  #+lispworks `(opengl::rendering-on (,window)
    . ,body)
  #+glfw-engine `(progn
    . ,body))

(defmacro drawing-on-bitmap ((bitmap) &body body)
  "Used instead of DRAWING-ON-WINDOW for bitmaps."
  (let ((canvas-var (gensym)))
    `(drawing-on-window (*boxer-pane*)
       (let ((,canvas-var (boxer-opengl::make-graphics-canvas (ogl-pixmap-width  ,bitmap) (ogl-pixmap-height ,bitmap) ,bitmap)))
         (unwind-protect
           (progn
             (enable ,canvas-var)
             (progn . ,body)
             (pixblt-from-screen ,bitmap 0 0 (ogl-pixmap-width ,bitmap) (ogl-pixmap-height ,bitmap)))
           (disable ,canvas-var))))))

;;;
;;; Drawing, Colors, Width, and Geometry layout type Macros
;;;

(defun set-pen-color (color)
  (%set-pen-color color))

(defmacro maintaining-pen-color (&body body)
  (let ((old-color (gensym)))
    `(let ((,old-color (boxgl-device-pen-color bw::*boxgl-device*)))
       (unwind-protect
        (progn . ,body)
          (unless (equalp ,old-color (boxgl-device-pen-color bw::*boxgl-device*))
            (setf (boxgl-device-pen-color bw::*boxgl-device*) ,old-color))))))

(defmacro with-pen-color ((color) &body body)
  `(maintaining-pen-color
    (set-pen-color ,color)
    . ,body))

;;; see BU::COLOR-AT in grprim3.lisp
(defun window-pixel-color (x y &optional (view *boxer-pane*))
  (pixel->color (boxer-opengl::%get-pixel view (floor x) (floor y))))

(defmacro with-pen-size ((newsize) &body body)
  (let ((oldpsvar (gensym)) (nochangevar (gensym)) (newsizevar (gensym)))
    `(let ((,oldpsvar (boxgl-device-pen-size bw::*boxgl-device*))
           (,newsizevar (float ,newsize))
           (,nochangevar nil))
       (unwind-protect
        (progn
         (cond ((= ,newsizevar ,oldpsvar) (setq ,nochangevar t))
           (t (%set-pen-size ,newsize)))
         . ,body)
        (unless ,nochangevar (%set-pen-size ,oldpsvar))))))

(defun %set-pen-size (v)
  (setf (boxgl-device-pen-size bw::*boxgl-device*) v))

(defmacro with-line-stippling ((pattern factor) &body body)
  (let ((stipplevar (gensym)))
    `(let ((,stipplevar (line-stipple bw::*boxgl-device*)))
       (unwind-protect
        (progn
          (setf (line-stipple bw::*boxgl-device*) t)
         . ,body)
        (unless ,stipplevar
          (setf (line-stipple bw::*boxgl-device*) nil))))))

(defmacro maintaining-drawing-font (&body body)
  (let ((font-var (gensym)))
    `(let ((,font-var *current-opengl-font*))
       (unwind-protect
        (progn . ,body)
        ;; NOTE: fonts aren't necessarily EQ
        (unless (eql *current-opengl-font* ,font-var)
          (setq *current-opengl-font* ,font-var))))))

(defmacro rebind-font-info ((font-no) &body body)
  `(let ((%drawing-font-cha-hei %drawing-font-cha-hei)
         (%drawing-font-cha-ascent %drawing-font-cha-ascent))
     (unless (null ,font-no)
       (maintaining-drawing-font
        (set-font-info ,font-no)
        ,@body))))

;;;
;;; Scaling and Clipping Macros
;;;

(defun get-model-matrix ()
  (boxgl-device-model-matrix bw::*boxgl-device*))

(defun (setf get-model-matrix) (new-val)
  (setf (boxgl-device-model-matrix bw::*boxgl-device*) new-val))

(defmacro with-model-matrix ((model-matrix) &body body)
  (let ((cur-model-matrix (gensym)))
    `(let* ((,cur-model-matrix (boxgl-device-model-matrix bw::*boxgl-device*)))
       (unwind-protect
           (progn
             (setf (boxgl-device-model-matrix bw::*boxgl-device*) ,model-matrix)
             (refresh-gpu-model-matrix)
             . ,body)
         (progn
           (setf (boxgl-device-model-matrix bw::*boxgl-device*) ,cur-model-matrix)
           (refresh-gpu-model-matrix))))))

(defun refresh-gpu-model-matrix ()
  (%refresh-gpu-model-matrix))

(defun calculate-clip-rectangle (stack)
  "Takes a stack of lists each with 4 members: x1, y1, x2, y2 and finds the final
   rectangle to be clipped on screen.

   Assumes that each point x2,y2 is greater than x1,y1."
  (destructuring-bind (x1 y1 x2 y2) (car stack)
    (dolist (next (cdr stack))
      (destructuring-bind (next-x1 next-y1 next-x2 next-y2) next
        (setf x1 (max x1 next-x1) y1 (max y1 next-y1)
              x2 (min x2 next-x2) y2 (min y2 next-y2))))
    (list x1 y1 x2 y2)))

(defun clip-stencil-rectangle (clip-stack)
    (boxer-opengl::write-to-stencil)
    (with-model-matrix ((3d-matrices:meye 4))
      (with-pen-color (*transparent*)
        (destructuring-bind (x1 y1 x2 y2) (calculate-clip-rectangle clip-stack)
          (draw-rectangle (- x2 x1) (- y2 y1) x1 y1)))
      (boxer-opengl::render-inside-stencil)))

(defmacro with-clipping-inside ((x y wid hei) &body body)
  `(unwind-protect
    (progn
      (push (list ,x ,y (+ ,x ,wid) (+ ,y ,hei)) *clipping-stack*)
      (clip-stencil-rectangle *clipping-stack*)
      . ,body)
    ;; reset the old clip region
    (progn
      (pop *clipping-stack*)
      (if (car *clipping-stack*)
        (clip-stencil-rectangle *clipping-stack*)
        (boxer-opengl::ignore-stencil)))))

(defun update-gpu-matrices ()
  (boxer-opengl::update-matrices-ubo bw::*boxgl-device*))

(defun create-transform-matrix (x y &optional (z 0))
  "Create a transform matrix offset from the origin by x and y."
  (3d-matrices:mtranslation (3d-vectors:vec x y z)))

(defmethod set-transform ((self boxgl-device) h v)
  "Sets the absolute values of the current transform matrix and updates the ubo."
    (setf (boxgl-device-transform-matrix self)
          (create-transform-matrix h v))
    (boxer-opengl::update-transform-matrix-ubo self))

(defun create-ortho-matrix (wid hei &key (zoom (zoom-level *boxer-pane*)))
  "Create an orthogonal projection matrix for use in our shaders with the given width and height."
  (let* ((ortho (3d-matrices:mortho 0 wid hei 0 -1000 1000))
         (origin (content-origin *boxer-pane*))
         (trans (3d-matrices:mtranslation (3d-vectors:vec (first origin) (second origin) 0))))
    (3d-matrices:nmscale trans (3d-vectors:vec zoom zoom 1.0))
    (setf ortho (3d-matrices:m*  ortho trans))
    ortho))

;;;
;;; Fonts
;;;
(defun init-freetype-fonts  ()
  (boxer-opengl::load-freetype-faces)
  (setf boxer-opengl::*freetype-glyph-atlas* (boxer-opengl::make-glyph-atlas)))

(defun find-glyph (spec)
  (%find-glyph spec))

(defun string-hei (font-no)
  (%string-hei font-no))

(defun string-wid (font-no string)
  (%string-wid font-no string))

(defun string-ascent (font-no)
  (%string-ascent font-no))

(defun cha-hei () %drawing-font-cha-hei)

(defun cha-ascent () %drawing-font-cha-ascent)

(defun cha-wid (char)
  (%cha-wid char))

(defun clear-window (color)
  (%clear-window color))

;;;
;;; Drawing functions
;;;

;; TODO for anything not in the texture map, just use the regular draw-cha for now
(defun draw-cha (char x y &key (gl-model nil))
  "Draw-cha needs to draw at the char's baseline rather than the top left corner.  In a
multifont row, the common reference point will be the baseline instead of the top edge"
  (%draw-cha char x y :gl-model gl-model))

(defun draw-circle (x y radius &optional filled?)
  (%draw-circle x y radius filled?))

(defun draw-ellipse (x y width height &optional filled?)
  (%draw-ellipse x y width height filled?))

(defun draw-line (x0 y0 x1 y1)
  (%draw-line x0 y0 x1 y1))

(defun draw-point (x y)
  (%draw-point x y))

(defun draw-poly (points)
  (%draw-poly points))

(defun draw-rectangle (w h x y)
  (%draw-rectangle w h x y))

(defun draw-c-arc (x y radius start-angle sweep-angle &optional filled?)
  (boxer-opengl::%draw-c-arc x y radius start-angle sweep-angle filled?))

(defun erase-rectangle (w h x y)
  (with-pen-color (*background-color*)
     (draw-rectangle w h x y)))

;; used directly
(defun multiline2 (&rest x-and-y-s)
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
        (draw-line prev-x prev-y x y))
      (setf prev-x x prev-y y))))

(defun draw-string (font-no string region-x region-y)
  (%draw-string font-no string region-x region-y))


(defun bitblt-to-screen (wid hei from-array from-x from-y to-x to-y)
  (boxer-opengl::%bitblt-to-screen wid hei from-array from-x from-y to-x to-y))

(defun bitblt-from-screen (wid hei to-array from-x from-y to-x to-y)
  (%bitblt-from-screen wid hei to-array from-x from-y to-x to-y))


(defun swap-graphics-buffers (&optional (pane *boxer-pane*))
  (boxer-opengl::%flush-port-buffer pane))

(defun gl-reshape (window-width window-height &optional (pane *boxer-pane*))
  #+lispworks (opengl::%resize-opengl-context (capi-internals:representation *boxer-pane*)
                (opengl::context *boxer-pane*)
                window-width window-height)
  (boxer-opengl::ogl-reshape window-width window-height))

;;;
;;; Drawing operations to wrap Graphics List Playback to allow for varying performance
;;; optimizations
;;;
;;; For instance, in OpenGL we can put a bunch of lines on a C buffer and then draw them
;;; at once, and this requires keeping track of some state variables. The methods below
;;; allow you to perform these initializations, drawing, finalizations before and after
;;; iterating through a list of graphics commands, and before each graphic command is processed.
;;;

(defun make-boxer-gl-model ()
  (%make-boxer-gl-model))

(defmethod get-boxgl-model-for-screen-obj ((self screen-obj))
  "Gets an instance of `boxer-gl-model` from the plist on the screen-obj, creating it if it doensn't exist
  yet. Also updates the cur-tick and needs-update properties of the model so we know if the screen-obj has
  changed and requires rebuffering of vertices."
  (let ((tick (get-updated-tick self))
        (model (getprop self  :gl-model)))

    (unless model
      (putprop self (make-boxer-gl-model) :gl-model)
      (setf model (getprop self  :gl-model)))

    (if (equal tick (slot-value model 'cur-tick))
      (setf (needs-update model) nil)
      (progn
        (reset-meshes model)
        (setf (slot-value model 'cur-tick) tick)))
    model))

;; TODO We need to make a proper flexible Boxer Mesh class
;; (defmethod needs-update (obj)
;;   (boxer-opengl::needs-update obj))

(defmethod get-updated-tick ((self screen-obj))
  (actual-obj-tick (screen-obj-actual-obj self)))

(defmethod get-updated-tick ((self screen-row))
  "For checking if a screen row needs rebuffered, we are looking at the dimensions, offsets, and
   cliping. It's tick updates on nearly every repaint, so that's not usable here."
  (with-slots (actual-obj wid hei x-offset y-offset x-got-clipped? y-got-clipped?)
              self
    (list (actual-obj-tick actual-obj) wid hei x-offset y-offset x-got-clipped? y-got-clipped?)))

(defmethod get-updated-tick ((self screen-box))
  "Currently we're just using the screen-box model for drawing borders."
  (with-slots (wid hei actual-obj box-type) self
    (list wid hei (name actual-obj) box-type (get-css-style actual-obj :border-color)
          (display-style-border-style (display-style-list actual-obj)))))

(defmethod get-graphics-canvas-for-screen-obj ((self plist-subclass) &optional wid hei)
  (unless (slot-boundp self 'plist)
    (setf (plist self) nil))

  (let ((paint-tex (getprop self :graphics-canvas)))
    (cond
      ((null paint-tex)
       (putprop self (boxer-opengl::make-graphics-canvas wid hei) :graphics-canvas)
       (setf paint-tex (getprop self :graphics-canvas))
       (boxer-opengl::enable paint-tex)
       (clear-window *transparent*)
       (boxer-opengl::opengl-enables)
       (boxer-opengl::disable paint-tex))
      ((or (not (equal wid (ogl-pixmap-width (boxer-opengl::graphics-canvas-pixmap paint-tex))))
           (not (equal hei (ogl-pixmap-height (boxer-opengl::graphics-canvas-pixmap paint-tex)))))
       (boxer-opengl::enable paint-tex)
       (resize paint-tex wid hei)
       (boxer-opengl::disable paint-tex))
      (t nil))
    paint-tex))

(defparameter *cur-gl-model-screen-obj* nil
  "If this global object is set, then supported draw-xyzed commands will buffer vertices to this gl model
  rather immediately buffer and draw them.")

(defun start-drawing-screen-obj-model (screen-obj)
  "This set the gl-model to being drawing to for all supported drawing commands until stop-drawing-screen-obj-model
  is called.
  Currently used to set the model for drawing box borders, including name and type labels."
  (setf *cur-gl-model-screen-obj* (get-boxgl-model-for-screen-obj screen-obj)))

(defun stop-drawing-screen-obj-model ()
  "This will stop drawing to the screen-obj's model and draw it to the active buffer."
  (when *cur-gl-model-screen-obj*
    (draw *cur-gl-model-screen-obj*))
  (setf *cur-gl-model-screen-obj* nil))

;;; Things for measuring repaint times
(defvar *last-framerate-time* (get-universal-time))
(defvar *number-of-frames* 0)
(defvar *current-framerate* 0)

(defun update-framerate ()
  "Register a repaint to update the statistics for the repaint rate."
  ;; https://www.opengl-tutorial.org/miscellaneous/an-fps-counter
  (let* ((current-time (get-universal-time))
        (diff (- current-time *last-framerate-time*)))
    (setq *number-of-frames* (1+ *number-of-frames*))
    (when (>= diff 1)
      (setq *current-framerate* (/ 1000.0 *number-of-frames*))
      (setq *number-of-frames* 0)
      (setq *last-framerate-time* (get-universal-time)))
  ))
