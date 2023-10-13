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
   (cached-ortho-matrix :initform nil :accessor cached-ortho-matrix
    :documentation "We have to reset the drawing matrices for these canvases. This is what
                    we'll set it back to when we're done.")
   (cached-transform-matrix :initform nil :accessor cached-transform-matrix
    :documentation "See the comments on cached-ortho-matrix.")

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
  (opengl:rendering-on (*boxer-pane*)
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
   This takes care of updating our ortho, transform, and other matrices for the shaders,
   as well as resetting the viewport for the framebuffer."
  (gl:bind-framebuffer :framebuffer (graphics-canvas-framebuffer self))
  (setf (cached-ortho-matrix self) (boxgl-device-ortho-matrix device))
  (setf (cached-transform-matrix self) (boxgl-device-transform-matrix device))
  (let* ((pixmap (graphics-canvas-pixmap self))
         (wid (ogl-pixmap-width pixmap))
         (hei (ogl-pixmap-height pixmap))
         (new-ortho-matrix (create-ortho-matrix wid hei)))
    (setf (boxgl-device-ortho-matrix device) new-ortho-matrix)
    (setf (boxgl-device-transform-matrix device) (create-transform-matrix 0 0))
    (opengl:gl-viewport 0 0 wid hei)
    (gl:disable :scissor-test)
  (update-matrices-ubo device)))

(defmethod disable ((self graphics-canvas) &key (device bw::*boxgl-device*))
  "See comments for enable. Resets the current framebuffer back to the default front/back buffers."
  (gl:bind-framebuffer :framebuffer 0)
  (setf (boxgl-device-ortho-matrix device) (cached-ortho-matrix self))
  (setf (boxgl-device-transform-matrix device) (cached-transform-matrix self))
  (opengl:gl-viewport 0 0 (aref (resolution) 0) (aref (resolution) 1))
  (gl:enable :scissor-test)
  (update-matrices-ubo device))
