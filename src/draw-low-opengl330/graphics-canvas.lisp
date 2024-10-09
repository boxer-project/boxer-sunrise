;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;        These file contains utilities for drawing operations on a Boxer Graphics Canvas,
;;;;        interfacing with a graphics-sheet, graphics-box, and graphics-screen-box objects.
;;;;
;;;;        This provides a means of drawing graphics primitives on to a texture that is used
;;;;        to display all the drawing, rather than drawing the raw primitive operations in GL
;;;;        on each repaint. This is used so that we can update the screen reasonably after a
;;;;        turtle has painted a few million lines... or similar use cases.
;;;;
(in-package :boxer)

(defclass graphics-canvas ()
  ((pixmap :initform nil :accessor graphics-canvas-pixmap :initarg :pixmap
    :documentation "The ogl-pixmap with the backing texture-id and dimensions.")
   (renderbuffer :initform nil :accessor graphics-canvas-renderbuffer :initarg :renderbuffer
    :documentation "")
   (framebuffer :initform nil :accessor graphics-canvas-framebuffer :initarg :framebuffer
    :documentation "Integer ID of the openGL framebuffer for this canvas.")
   (op-count :initform 0 :accessor op-count
    :documentation "A counter that can be used to document the number of graphics
                    command operations have been applied to this texture. In a graphics command
                    display list, we can use this to pick back up painting to the texture without
                    repeating previous operations.")
   (cached-projection-matrix :initform nil :accessor cached-projection-matrix
    :documentation "We have to reset the drawing matrices for these canvases. This is what
                    we'll set it back to when we're done.")
   (cached-transform-matrix :initform nil :accessor cached-transform-matrix
    :documentation "See the comments on cached-projection-matrix.")
   (cached-model-matrix :initform nil :accessor cached-model-matrix
    :documentation "See the comments on cached-projection-matrix.")

   (pen-color-cmd :initform nil :accessor graphics-canvas-pen-color-cmd
    :documentation "Vector of the last run gdispl command for changing the color, so it can be turned back
                    on when picking up in the list without starting from the beginning.")
   (pen-size-cmd  :initform nil :accessor graphics-canvas-pen-size-cmd
    :documentation "Vector of the last run gdispl command for changing the pen size, so it can be turned back
                    on when picking up in the list without starting from the beginning.")
   (pen-font-cmd  :initform nil :accessor graphics-canvas-pen-font-cmd
    :documentation "Vector of the last run gdispl command for changing the font, so it can be turned back
                    on when picking up in the list without starting from the beginning.")))

(defun make-graphics-canvas (wid hei)
  "Creates a new graphics-canvas, and all it's backing openGL resources  such a framebuffer, textures, etc.
   Should be called inside an openGL context"
  (let* ((framebuffer (gl:gen-framebuffer))
         (renderbuffer (gl:gen-renderbuffer))
         (texture (gl:gen-texture))
         (pixmap (make-ogl-pixmap wid hei :texture texture))
         (togo (make-instance 'graphics-canvas :framebuffer framebuffer :pixmap pixmap :renderbuffer renderbuffer)))

    (gl:bind-framebuffer :framebuffer (graphics-canvas-framebuffer togo))

    ;; Color texture for the framebuffer
    (gl:bind-texture :texture-2d texture)
    (gl:tex-image-2d :texture-2d 0 :rgba wid hei 0 :rgba :unsigned-byte (cffi:null-pointer))
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:bind-texture :texture-2d 0)
    (gl:framebuffer-texture-2d :framebuffer :color-attachment0 :texture-2d texture 0)

    ;; Depth and Stencil renderbuffer for the framebuffer
    ;; It's not clear that these are necessary for our current graphics operations.
    ; (gl:bind-renderbuffer :renderbuffer renderbuffer)
    ; (gl:renderbuffer-storage :renderbuffer :depth24-stencil8 wid hei)
    ; (gl:bind-renderbuffer :renderbuffer 0)
    ; (gl:framebuffer-renderbuffer :framebuffer :depth-stencil-attachment :renderbuffer renderbuffer)

    (gl:clear-color 0.0 0.0 0.0 0.0)
    (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)

    (gl:bind-framebuffer :framebuffer 0)
    togo))

(defmethod clear ((self graphics-canvas))
  "Clears the contents of the canvas setting it back to transparent."
  (setf (op-count self) 0)
  (setf (graphics-canvas-pen-color-cmd self) nil)
  (setf (graphics-canvas-pen-size-cmd self) nil)
  (setf (graphics-canvas-pen-font-cmd self) nil)
  ;; Wrapping this in a rendering-on because sometimes clear is called by clearscreen
  ;; outside of a repaint.
  #+lispworks (opengl:rendering-on (*boxer-pane*)
    (enable self)
    (gl:clear-color 0.0 0.0 0.0 0.0) ;; transparent
    (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)
    (disable self)))

(defmethod clear-graphics-canvas ((self box))
  "If this box has a graphics-canvas on any screen-boxes rendering it, clears them."
  (dolist (item (get-visible-screen-objs self))
    (let ((canvas (getprop item :graphics-canvas)))
      (when canvas
        (clear canvas)))))

(defmethod resize ((self graphics-canvas) wid hei)
  "Resizes the contents, by regenerating the backing texture. Existing contents will be lost.

   Respects the global *update-bitmap?* to avoid intensive work during manual box resizing.
  "
  (when *update-bitmap?*
    (let* ((pixmap (graphics-canvas-pixmap self))
          (texture (ogl-pixmap-texture pixmap)))
      (gl:bind-texture :texture-2d texture)
      (gl:tex-image-2d :texture-2d 0 :rgba wid hei 0 :rgba :unsigned-byte (cffi:null-pointer))
      (gl:bind-texture :texture-2d 0)
      (setf (ogl-pixmap-width pixmap) wid)
      (setf (ogl-pixmap-height pixmap) hei)
      (setf (op-count self) 0)
      (gl:clear-color 0.0 0.0 0.0 0.0) ;; transparent
      (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))))

(defmethod enable ((self graphics-canvas) &key (device bw::*boxgl-device*))
  "Enables the framebuffer backing this graphics-canvas, such that any GL operations will
   take place on it, until calling disable (or manually setting another framebuffer with GL
   calls).
   This takes care of updating our projection, transform, and other matrices for the shaders,
   as well as resetting the viewport for the framebuffer."
  (gl:bind-framebuffer :framebuffer (graphics-canvas-framebuffer self))
  (setf (cached-projection-matrix self) (boxgl-device-projection-matrix device))
  (setf (cached-transform-matrix self) (boxgl-device-transform-matrix device))
  (setf (cached-model-matrix self) (boxgl-device-model-matrix device))
  (let* ((pixmap (graphics-canvas-pixmap self))
         (wid (ogl-pixmap-width pixmap))
         (hei (ogl-pixmap-height pixmap))
         (new-projection-matrix (create-ortho-matrix wid hei)))
    (setf (boxgl-device-projection-matrix device) new-projection-matrix)
    (setf (boxgl-device-transform-matrix device) (create-transform-matrix 0 0))
    (setf (boxgl-device-model-matrix device) (3d-matrices:meye 4))
    #+lispworks (opengl:gl-viewport 0 0 wid hei)
  (update-matrices-ubo device)))

(defmethod disable ((self graphics-canvas) &key (device bw::*boxgl-device*))
  "See comments for enable. Resets the current framebuffer back to the default front/back buffers."
  (gl:bind-framebuffer :framebuffer 0)
  (setf (boxgl-device-projection-matrix device) (cached-projection-matrix self))
  (setf (boxgl-device-transform-matrix device) (cached-transform-matrix self))
  (setf (boxgl-device-model-matrix device) (cached-model-matrix self))
  #+lispworks (opengl:gl-viewport 0 0 (aref (resolution) 0) (aref (resolution) 1))
  (update-matrices-ubo device))

;;;
;;; Routines for storing/filling/drawing a mesh for an above graphics-canvas
;;;
(defmethod get-canvas-mesh ((self plist-subclass))
  (unless (slot-boundp self 'plist)
    (setf (plist self) nil))

  (let ((mesh (getprop self :mesh)))
    (cond
      ((null mesh)
       (setf mesh (make-boxer-glyphs-xyz-txty-rgba-mesh))
       (setup-xyz-txty-mesh (mesh-vao mesh) (mesh-vbo mesh) (* 5 6))
       (putprop self mesh :mesh))
      (t
       nil))
    mesh))

(defun buffer-canvas-mesh (device mesh pixmap wid hei)
  (enable-gl-objects device :shader (pixmap-shader device)
                            :program (shader-program (pixmap-shader device))
                            :vao     (mesh-vao mesh)
                            :buffer  (mesh-vbo mesh))

  (gl:pixel-store :unpack-alignment 4)

  (create-pixmap-texture pixmap ) ;;pwid phei data)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (ogl-pixmap-texture pixmap))

  ;; The openGL text-coordinates go from 0 to 1 and originate in the bottom left corner.
  ;; The boxer pixmap coordinates start from 0 and go to the width/heigh in pixels and
  ;; originate in the top left corner.
  (let* ((tex-coord-x1 (/ 0 (ogl-pixmap-width pixmap)))
         (tex-coord-y1 (- 1 (/ (+ 0 hei) (ogl-pixmap-height pixmap))))
         (tex-coord-x2 (/ (+ 0 wid) (ogl-pixmap-width pixmap)))
         (tex-coord-y2 (- 1 (/ 0 (ogl-pixmap-height pixmap))))
         (vertices (float-vector 0          0          0.0 tex-coord-x1 tex-coord-y2
                                 0          (+ 0 hei)  0.0 tex-coord-x1 tex-coord-y1
                                 (+ 0 wid)  0          0.0 tex-coord-x2 tex-coord-y2
                                 (+ 0 wid)  (+ 0 hei)  0.0 tex-coord-x2 tex-coord-y1
                                 0          (+ 0 hei)  0.0 tex-coord-x1 tex-coord-y1
                                 (+ 0 wid)  0          0.0 tex-coord-x2 tex-coord-y2))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))
  (unenable-shader-programs device)
  (setf (mesh-pos mesh) (+ (mesh-pos mesh) (* 5 6))))

(defun draw-canvas-mesh (mesh pixmap)
    (enable-gl-objects bw::*boxgl-device* :shader (pixmap-shader bw::*boxgl-device*)
                                          :program (shader-program (pixmap-shader bw::*boxgl-device*))
                                          :vao     (mesh-vao mesh)
                                          :buffer  (mesh-vbo mesh))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (ogl-pixmap-texture pixmap))

    (gl:draw-arrays :triangles 0 6)
    (gl:bind-texture :texture-2d 0)
    (unenable-shader-programs bw::*boxgl-device*))
