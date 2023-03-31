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
;;;;
;;;;                                        +-Data--+
;;;;               This file is part of the | BOXER | system
;;;;                                        +-------+
;;;;
;;;;
;;;;   OpenGL Drawing and Shader Routines
;;;;
;;;;   This is an implemention of the draw-defs drawing operations using modern OpenGL 3.30 core
;;;;   operations with proper vertex and fragment shaders. Outside of any context operations such
;;;;   as swapping buffers and wrapping the calls in contexts, this will use only generic openGL
;;;;   and math calls from libraries like cl-opengl and 3d-matrices. This should make it usable
;;;;   in other openGL environments without too much trouble.
;;;;
;;;;   Notes
;;;;     - gl:get-uniform-location is slow, so we store the uniform ids for each shader
(in-package :boxer)

(defvar *cffi-float-size* nil
  "Looking up the float size does take some time, performance wise.")

(defvar *shaders-dir* nil
  "Path to the folder on disc where we can find the shaders. On distributed apps on macOS or Windows this should be in
   the read-only section of it's resource files. On macOS we should put them in boxer.app/Contents/PlugIns/shaders
   since they constitute code.")

(defstruct box-glyph
  ch
  width
  rows
  bearing-x
  bearing-y
  advance
  ;; If this glyph has a standalone texture this is the GL id
  texture-id
  ;; If this glyph is part of a texture atlas, these are it's
  ;; offsets in texture coordinates (ie. [0, 1])
  tx
  ty)

(defun line-by-width-corners (x0 y0 x1 y1 width)
  "Given two points for a line and a width for the line, calculate the 4 points necessary for the (most likely)
  long and skinny rectangle that delineates the line."

  ;; We will find the normals.    dx = x1 - x0    dy = y1 - y0
  ;;                           (-dy, dx) and (dy, -dx)
  ;; Normalize them by v = sqrt(dx^2 + dy^2)
  ;; Divide them by v
  ;; Add both normals to each point to get the 4 corners.
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (v  (sqrt (+ (expt dx 2) (expt dy 2))))
         (half-wid (/ width 2))
         (sx0 (* (/ (* -1 dy) v) half-wid)) ;; scaled normal transforms
         (sy0 (* (/ dx v) half-wid))
         (sx1 (* (/ dy v) half-wid))
         (sy1 (* (/ (* -1 dx) v) half-wid)))
    (make-array '(8) :initial-contents (list (+ x0 sx0) (+ y0 sy0) (+ x0 sx1) (+ y0 sy1)
                                             (+ x1 sx0) (+ y1 sy0) (+ x1 sx1) (+ y1 sy1)))))

(defclass boxgl-shader-program ()
  ((program :initarg :program :accessor shader-program)
   (vao     :initarg :vao     :accessor shader-vao)
   (buffer  :initarg :buffer  :accessor shader-buffer)))

(defclass boxgl-device ()
  ((lines-shader        :accessor lines-shader)

   (ft-glyph-shader     :accessor ft-glyph-shader)
   (text-color-uni      :accessor text-color-uni)
   (ft-glyph-gl-array   :accessor ft-glyph-gl-array)

   (glyph-atlas-shader  :accessor glyph-atlas-shader)

   (pixmap-shader       :accessor pixmap-shader)
   (dashed-lines-shader :accessor dashed-lines-shader)

   (canvas-shader       :accessor canvas-shader)
   (circle-xyrad-uni    :accessor circle-xyrad-uni)

   (circle-shader       :accessor circle-shader)

   ;; Simple Shader
   ;; Uses a color uniform, starting out using for box borders
   ;; to draw them all in one go
   (simple-shader       :accessor simple-shader)
   (borders-arr         :accessor borders-arr :initform (gl:alloc-gl-array :float (* 6 1024))) ; A gl-array specifically for drawing box borders
   (borders-count       :accessor borders-count :initform 0) ; Number of vertices

   (matrices-ubo :accessor matrices-ubo)
   (ortho-matrix :accessor boxgl-device-ortho-matrix)
   (transform-matrix :accessor boxgl-device-transform-matrix)

   (pen-color :accessor boxgl-device-pen-color) ; current color in #(:rgb 1.0 1.0 1.0 1.0) format
   (pen-size  :accessor boxgl-device-pen-size)
   (line-stipple :accessor line-stipple :initform nil
     :documentation "Boolean to indicate if we're currently drawing dashed lines.")

   (null-gl-elements-array :accessor null-gl-elements-array)

   ;; Currently bound Shader Program, vao, etc
   (cur-program :accessor cur-program :initform 0)
   (cur-vao :accessor cur-vao :initform 0)
   (cur-buffer :accessor cur-buffer :initform 0)))

(defvar *gl-pixmap-texture-count* 0
  "The number of openGL textures we've created for ogl-pixmaps.")

(defun create-pixmap-texture (pixmap)
  "Creates a texture for the pixmap if it doesn't exist yet. If the texture has already been
  generated, does nothing. (Which means the texture still needs to be bound if you're about to
  do something."
    (when (= 0 (ogl-pixmap-texture pixmap))
      (let ((texture-id (gl:gen-texture))
            (wid (ogl-pixmap-width pixmap))
            (hei (ogl-pixmap-height pixmap))
            (data (ogl-pixmap-data pixmap)))
        (setf (ogl-pixmap-texture pixmap) texture-id)
        (gl:bind-texture :texture-2d texture-id)
        (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
        (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
        (gl:tex-parameter :texture-2d :texture-min-filter :linear)
        (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

        (gl:tex-image-2d :texture-2d 0 :rgba wid hei 0 :rgba :unsigned-byte data)
        (gl:generate-mipmap :texture-2d)
        (incf *gl-pixmap-texture-count*))))

(defun gl-add-pixmap (device from-array tx ty wid hei fx fy)
  (enable-gl-shader-program device (pixmap-shader device))

  (gl:pixel-store :unpack-alignment 4)

  (create-pixmap-texture from-array ) ;;pwid phei data)

  ;; Remember to bind the texture before trying to set an active texture, else
  ;; we'll render a black square trying to make an active texture when none is bound.
  (gl:bind-texture :texture-2d (ogl-pixmap-texture from-array))
  (gl:active-texture :texture1)

  (let* ((vertices `#(,(coerce tx 'single-float)         ,(coerce ty 'single-float)         0.0 0.0 1.0
                      ,(coerce tx 'single-float)         ,(coerce (+ ty hei) 'single-float) 0.0 0.0 0.0
                      ,(coerce (+ tx wid) 'single-float) ,(coerce ty 'single-float)         0.0 1.0 1.0
                      ,(coerce (+ tx wid) 'single-float) ,(coerce (+ ty hei) 'single-float) 0.0 1.0 0.0
                      ,(coerce tx 'single-float)         ,(coerce (+ ty hei) 'single-float) 0.0 0.0 0.0
                      ,(coerce (+ tx wid) 'single-float) ,(coerce ty 'single-float)         0.0 1.0 1.0
                      ))
          (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)
    (gl:draw-arrays :triangles 0 6)))

(defun gl-add-atlas-char2 (device x y ch &key (rgb (boxgl-device-pen-color device))
                                       (baseline-bot nil)
                                       (font *current-opengl-font*)
                                       (atlas *freetype-glyph-atlas*))
  "Test version of draw-char that uses a glyph from a texture atlas and the atlas shader which
  takes the color values on the vao."
  (let* ((font-face (current-freetype-font font))
         (glyph (get-glyph atlas `(("Arial" 12) ,ch 1.0)))
         (bearing-x (box-glyph-bearing-x glyph))
         (bearing-y (box-glyph-bearing-y glyph))
         (width (box-glyph-width glyph))
         (w width)
         (h (box-glyph-rows glyph))
         (x (coerce x 'float))
         (y (coerce y 'float))
         (xpos (+ x bearing-x))
         (font-hei (bw::ogl-font-height font))
         (ypos (if baseline-bot
                 (- (- (+ y font-hei) bearing-y) (* font-hei 0.2)) ;; This scaling by 0.2 of the font height is some
                 (- (- y bearing-y) (* font-hei 0.2))))            ;; pixel pushing for the current repaint layout.
         (ypos+h (+ ypos h))
         (xpos+w (+ xpos w))
         (tx (coerce (box-glyph-tx glyph) 'single-float))
         (ty (coerce (box-glyph-ty glyph) 'single-float)))
    (enable-gl-shader-program device (glyph-atlas-shader device))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (glyph-atlas-texture-id atlas))

    ;; make the vertices array
    (let* (
           (vertices `#(,xpos   ,ypos       0.0 ,tx                                     ,ty                                       ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ;; 0.0 0.0
                        ,xpos   ,ypos+h     0.0 ,tx                                     ,(+ ty (/ h (glyph-atlas-height atlas)))  ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ;;0.0 1.0
                        ,xpos+w ,ypos+h     0.0 ,(+ tx (/ w (glyph-atlas-width atlas))) ,(+ ty (/ h (glyph-atlas-height atlas)))  ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)

                        ; These 2 can be removed when using the element buffer
                        ,xpos   ,ypos       0.0 ,tx                                     ,ty                                       ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ;; 0.0 0.0
                        ,xpos+w ,ypos+h     0.0 ,(+ tx (/ w (glyph-atlas-width atlas))) ,(+ ty (/ h (glyph-atlas-height atlas)))  ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)

                        ,xpos+w ,ypos       0.0 ,(+ tx (/ w (glyph-atlas-width atlas))) ,ty                                       ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ;; 1.0 0.0
                        ))
           (arr (gl:alloc-gl-array :float (length vertices)))
          ;  (arr (ft-glyph-gl-array device))
           )
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:draw-arrays :triangles 0 6)
      (gl:bind-texture :texture-2d 0))
      (unenable-shader-programs bw::*boxgl-device*)
    glyph))

(defun gl-add-atlas-char (device x y ch &key (rgb (boxgl-device-pen-color device))
                                       (baseline-bot nil)
                                       (font *current-opengl-font*)
                                       (atlas *freetype-glyph-atlas*))
  "Test version of draw-char that uses a glyph from a texture atlas and the glyph shader which
  takes the color from a uniform."
  (let* ((color-uniform (text-color-uni device))
         (font-face (current-freetype-font font))
         (glyph (get-glyph atlas `(("Arial" 12) ,ch 1.0)))
         (bearing-x (box-glyph-bearing-x glyph))
         (bearing-y (box-glyph-bearing-y glyph))
         (width (box-glyph-width glyph))
         (w width)
         (h (box-glyph-rows glyph))
         (x (coerce x 'float))
         (y (coerce y 'float))
         (xpos (+ x bearing-x))
         (font-hei (bw::ogl-font-height font))
         (ypos (if baseline-bot
                 (- (- (+ y font-hei) bearing-y) (* font-hei 0.2)) ;; This scaling by 0.2 of the font height is some
                 (- (- y bearing-y) (* font-hei 0.2))))            ;; pixel pushing for the current repaint layout.
         (ypos+h (+ ypos h))
         (xpos+w (+ xpos w))
         (tx (coerce (box-glyph-tx glyph) 'single-float))
         (ty (coerce (box-glyph-ty glyph) 'single-float)))
    (enable-gl-shader-program device (ft-glyph-shader device))
    (gl:uniformf color-uniform (aref rgb 1) (aref rgb 2) (aref rgb 3))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (glyph-atlas-texture-id atlas))

    ;; make the vertices array
    (let* (
           (vertices `#(,xpos   ,ypos       ,tx                                     ,ty ;; 0.0 0.0
                        ,xpos   ,ypos+h     ,tx                                     ,(+ ty (/ h (glyph-atlas-height atlas)))  ;;0.0 1.0
                        ,xpos+w ,ypos+h     ,(+ tx (/ w (glyph-atlas-width atlas))) ,(+ ty (/ h (glyph-atlas-height atlas)))
                        ; ,xpos   ,ypos     0.0 0.0
                        ; ,xpos+w ,ypos+h   1.0 1.0
                        ,xpos+w ,ypos       ,(+ tx (/ w (glyph-atlas-width atlas))) ,ty ;; 1.0 0.0
                        ))
           (arr (gl:alloc-gl-array :float (length vertices)))
          ;  (arr (ft-glyph-gl-array device))
           )
      (format t "~%gl-add-atlas-char ~A ~A ~A ~A" x y ch vertices)
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (%gl::buffer-sub-data :array-buffer 0
                           (* 16 *cffi-float-size*)
                           (gl::gl-array-pointer arr))
      (gl:draw-elements :triangles (null-gl-elements-array device) :count 6)
      (gl:bind-texture :texture-2d 0))
    glyph))

(defun gl-add-char (device x y ch &key (rgb (boxgl-device-pen-color device))
                                       (baseline-bot nil)
                                       (font *current-opengl-font*))
  (let* ((color-uniform (text-color-uni device))
         (font-face (current-freetype-font font))
         (glyph (find-box-glyph ch font boxer::*font-size-baseline*))
         (bearing-x (box-glyph-bearing-x glyph))
         (bearing-y (box-glyph-bearing-y glyph))
         (width (box-glyph-width glyph))
         (w width)
         (h (box-glyph-rows glyph))
         (x (coerce x 'float))
         (y (coerce y 'float))
         (xpos (+ x bearing-x))
         (font-hei (bw::ogl-font-height font))
         (ypos (if baseline-bot
                 (- (- (+ y font-hei) bearing-y) (* font-hei 0.2)) ;; This scaling by 0.2 of the font height is some
                 (- (- y bearing-y) (* font-hei 0.2))))            ;; pixel pushing for the current repaint layout.
         (ypos+h (+ ypos h))
         (xpos+w (+ xpos w))
         (arr (ft-glyph-gl-array device)))
    (enable-gl-shader-program device (ft-glyph-shader device))
    (gl:uniformf color-uniform (aref rgb 1) (aref rgb 2) (aref rgb 3))
    (gl:active-texture :texture0)

    (setf (gl:glaref arr 0) xpos)
    (setf (gl:glaref arr 1) ypos)
    (setf (gl:glaref arr 4) xpos)
    (setf (gl:glaref arr 5) ypos+h)
    (setf (gl:glaref arr 8) xpos+w)
    (setf (gl:glaref arr 9) ypos+h)
    (setf (gl:glaref arr 12) xpos+w)
    (setf (gl:glaref arr 13) ypos)

    (unless (box-glyph-texture-id glyph)
      (setf (box-glyph-texture-id glyph)
            (create-glyph-texture font-face ch glyph)))
    (gl:bind-texture :texture-2d (box-glyph-texture-id glyph))
    (%gl::buffer-sub-data :array-buffer 0
                          (* 16 *cffi-float-size*)
                          (gl::gl-array-pointer arr))
    (gl:draw-elements :triangles (null-gl-elements-array device) :count 6)
    (gl:bind-texture :texture-2d 0)
    (unenable-shader-programs bw::*boxgl-device*)
    glyph))

(defun gl-add-string (device font string x y)
  (let ((cur-x x)
        (prev-glyph nil))
      (for:for ((c over string))
        (setf prev-glyph (gl-add-char device cur-x y c :baseline-bot t :font font))
        (setf cur-x (+ cur-x (box-glyph-advance prev-glyph)))
        )))

(defun gl-add-point (device x0 y0 &key (rgb (boxgl-device-pen-color device)))
  (enable-gl-shader-program device (lines-shader device))

  (let* ((vertices `#(,(coerce x0 'float) ,(coerce y0 'float) 0.0 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  (gl:draw-arrays :points 0 1))

(defun enable-gl-objects (device &key (program nil) (vao nil) (buffer nil))
  "Enable the Shader program with program-id. Checks the currently enabled program first to
  see if the operation is necessary. If it's already the current program, does nothing."
  (when  (and program
              (not (equal program (cur-program device))))
    (gl:use-program program)
    (setf (cur-program device) program))
  (when  (and vao
              (not (equal vao (cur-vao device))))
    (gl:bind-vertex-array vao)
    (setf (cur-vao device) vao))
  (when  (and buffer
              (not (equal buffer (cur-buffer device))))
    (gl:bind-buffer :array-buffer buffer)
    (setf (cur-buffer device) buffer)))

(defun enable-gl-shader-program (device shader)
  (enable-gl-objects device :program (shader-program shader) :vao (shader-vao shader) :buffer (shader-buffer shader)))

(defun unenable-shader-programs (device)
  (enable-gl-objects device :program 0 :vao 0 :buffer 0))

(defun gl-add-line-skip-lines (device x0 y0 x1 y1 &key (rgb (boxgl-device-pen-color device))
                                            (pen-size (boxgl-device-pen-size bw::*boxgl-device*)))
  (cond ((line-stipple device)
         (enable-gl-shader-program device (dashed-lines-shader device))
         (let ((res (resolution)))
           (gl:uniformf (gl:get-uniform-location (shader-program (dashed-lines-shader device)) "u_resolution")
                        (coerce (aref res 0) 'single-float) (coerce (aref res 1) 'single-float))))
        (t
         (enable-gl-shader-program device (lines-shader device))))


  (let* ((vertices `#(,(coerce x0 'float) ,(coerce y0 'float), (aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce x1 'float) ,(coerce y1 'float), (aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  ; (gl:draw-arrays :triangles 0 3)
  (gl:draw-arrays :lines 0 2)
  )

(defun gl-buffer-rect (device x y wid hei c-buffer buffer-pos &key (rgb (boxer::boxgl-device-pen-color device)))
  (let ((red   (aref rgb 1))
        (green (aref rgb 2))
        (blue  (aref rgb 3))
        (alpha (aref rgb 4)))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 0)) (coerce x 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 1)) (coerce y 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 2)) 0.0)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 3)) red)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 4)) green)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 5)) blue)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 6)) alpha)

    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 7)) (coerce (+ x wid) 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 8)) (coerce (+ y hei) 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 9)) 0.0)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 10)) red)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 11)) green)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 12)) blue)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 13)) alpha)

    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 14)) (coerce x 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 15)) (coerce (+ y hei) 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 16)) 0.0)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 17)) red)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 18)) green)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 19)) blue)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 20)) alpha)

    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 21)) (coerce x 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 22)) (coerce y 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 23)) 0.0)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 24)) red)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 25)) green)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 26)) blue)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 27)) alpha)

    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 28)) (coerce (+ x wid) 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 29)) (coerce y 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 30)) 0.0)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 31)) red)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 32)) green)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 33)) blue)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 34)) alpha)

    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 35)) (coerce (+ x wid) 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 36)) (coerce (+ y hei) 'float))
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 37)) 0.0)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 38)) red)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 39)) green)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 40)) blue)
    (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 41)) alpha)
    )
)

(defun gl-add-box-border-line (device x0 y0 x1 y1 &key (rgb (boxgl-device-pen-color device))
                                                       (pen-size (boxgl-device-pen-size bw::*boxgl-device*)))

  (when (refresh? *cur-border-cache*)
    (unless (and (equal (coerce x0 'single-float) (coerce x1 'single-float)) (equal (coerce y0 'single-float) (coerce y1 'single-float)))
      ; (cond ((line-stipple device)
      ;     (enable-gl-shader-program device (dashed-lines-shader device))
      ;     (let ((res (resolution)))
      ;       (gl:uniformf (gl:get-uniform-location (shader-program (dashed-lines-shader device)) "u_resolution")
      ;                     (coerce (aref res 0) 'single-float) (coerce (aref res 1) 'single-float))))
      ;     (t
      ;     (enable-gl-shader-program device (lines-shader device))))

    (let* (
           (corners (line-by-width-corners x0 y0 x1 y1 pen-size))
           (vertices `#(,(coerce (aref corners 0) 'single-float) ,(coerce (aref corners 1) 'single-float)
                       ,(coerce (aref corners 2) 'single-float) ,(coerce (aref corners 3) 'single-float)
                       ,(coerce (aref corners 4) 'single-float) ,(coerce (aref corners 5) 'single-float)

                       ,(coerce (aref corners 2) 'single-float) ,(coerce (aref corners 3) 'single-float)
                       ,(coerce (aref corners 4) 'single-float) ,(coerce (aref corners 5) 'single-float)
                        ,(coerce (aref corners 6) 'single-float) ,(coerce (aref corners 7) 'single-float)
                        ))
          ; (arr (gl:alloc-gl-array :float (length vertices)))
           (arr (arr *cur-border-cache*))
          )
      (dotimes (i (length vertices))
        (setf (gl:glaref arr (border-vert-count *cur-border-cache*)) (aref vertices i))
        (incf (border-vert-count *cur-border-cache*)))
      ; (gl:buffer-data :array-buffer :static-draw arr)
      ; (gl:free-gl-array arr))

    ; (gl:draw-arrays :triangles 0 6)
    ))))

(defvar *max-border-verts* 0)
(defvar *cur-border-cache* nil)

(defclass gl-box-border-cache ()
  ((cached-dslist :accessor cached-dslist :initform nil) ;; display-style-list
   (refresh?      :accessor refresh? :initform t) ;; Whether we are updating the arr this run
   (arr           :accessor arr :initform (gl:alloc-gl-array :float 400))
   (count         :accessor border-vert-count :initform 0) ;; current number of vertices
   (buffer        :accessor buffer :initform (gl:gen-buffer))  ;; The opengl buffer id
  ))

(defun make-gl-box-border-cache (dslist)
  (let ((togo (make-instance 'gl-box-border-cache)))
    (setf (cached-dslist togo) dslist)
    (gl:bind-buffer :array-buffer (buffer togo))
    (%gl:buffer-data :array-buffer (* 4 6 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)
    ; (gl:bind-buffer :array-buffer 0)
        (unenable-shader-programs bw::*boxgl-device*)

    togo))

(defun update-box-border-cache (cache new-dslist)
  (cond ((equalp new-dslist (cached-dslist cache))
         (setf (refresh? cache) nil)
        )
        (t
         (setf (refresh? cache) t)
         (setf (border-vert-count cache) 0)
         (setf (cached-dslist cache) new-dslist)
        )
  )
  )

(defun gl-draw-rects (device c-buffer pos)
  (enable-gl-shader-program device (lines-shader device))

  (%gl:buffer-data :array-buffer (* *cffi-float-size* pos) c-buffer :dynamic-draw)
  (gl:draw-arrays :triangles 0 (/ pos 7))
)

(defun gl-buffer-line (device x0 y0 x1 y1 c-buffer buffer-pos &key (rgb (boxer::boxgl-device-pen-color device)))
  "Takes 2 vertices, a C buffer and the current position in the buffer, and copies the
  points in to the buffer to prepare for a bulk draw-arrays call.
  "
  ;; Each Vertice has 7 points, x, y, z, r, g, b, a
  (setf (cffi:mem-aref c-buffer :float buffer-pos) (coerce x0 'float))        ; x
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 1)) (coerce y0 'float))  ; y
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 2)) 0.0)                 ; z
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 3)) (aref rgb 1))        ; r
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 4)) (aref rgb 2))        ; g
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 5)) (aref rgb 3))        ; b
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 6)) (aref rgb 4))        ; a

  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 7)) (coerce x1 'float))  ; x
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 8)) (coerce y1 'float))  ; y
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 9)) 0.0)                 ; z
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 10)) (aref rgb 1))       ; r
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 11)) (aref rgb 2))       ; g
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 12)) (aref rgb 3))       ; b
  (setf (cffi:mem-aref c-buffer :float (+ buffer-pos 13)) (aref rgb 4))       ; a
)

(defun gl-draw-lines (device c-buffer pos)
  (enable-gl-shader-program device (lines-shader device))
  (%gl:buffer-data :array-buffer (* *cffi-float-size* pos) c-buffer :dynamic-draw)
  (gl:draw-arrays :lines 0 (/ pos 7))
                            )

(defun create-shader (glsl-filename shader-type)
  "Creates and return the int id for a new shader. `glsl-filename` should be the relative
  filename, such as 'boxgl-stuff.vs'. Shader type should be the symbol type accepted by cl-opengl
  such as :vertex-shader or :fragment-shader"
  (let ((togo (gl:create-shader shader-type)))
    (gl:shader-source togo (read-shader-source glsl-filename))
    (gl:compile-shader togo)
    (log:debug "~%shader: ~A infolog: ~A" glsl-filename (gl:get-shader-info-log togo))
    togo))

(defun setup-xyz-txty-rgba-vao (vao vbo num-entries)
  ;; This VAO will have 3 locations
  ;; - vec3 position xyz
  ;; - vec2 texture coords tx ty
  ;; - vec4 color rgba
  ;;
  ;; total stride: 9 floats
  (gl:bind-vertex-array vao)
  (gl:bind-buffer :array-buffer vbo)
  (%gl:buffer-data :array-buffer (* num-entries 9 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)

  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil (* 9 *cffi-float-size*) (cffi:null-pointer))
  (gl:enable-vertex-attrib-array 1)
  (gl:vertex-attrib-pointer 1 2 :float nil (* 9 *cffi-float-size*) (* 3 *cffi-float-size*))
  (gl:enable-vertex-attrib-array 2)
  (gl:vertex-attrib-pointer 2 4 :float nil (* 9 *cffi-float-size*) (* 5 *cffi-float-size*))

  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0)
)

(defun setup-glyph-atlas-program (device)
  (let* ((glyph-program       (gl:create-program))
         (glyph-vao           (gl:gen-vertex-array))
         (glyph-buffer        (gl:gen-buffer))
         (glyph-index-buffer  (gl:gen-buffer)))

    (gl:attach-shader glyph-program (create-shader "boxgl-atlas-glyph.vs" :vertex-shader))
    (gl:attach-shader glyph-program (create-shader "boxgl-atlas-glyph.fs" :fragment-shader))
    (gl:link-program glyph-program)
    (log:debug "~%atlas-glyph-program infolog: ~A" (gl:get-program-info-log glyph-program))

    (gl:pixel-store :unpack-alignment 1)

    (setup-xyz-txty-rgba-vao glyph-vao glyph-buffer 6)

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-texture :texture-2d 0)

    (setf (boxer::glyph-atlas-shader device) (make-instance 'boxgl-shader-program :program glyph-program
                                                            :vao glyph-vao       :buffer  glyph-buffer))

    (%gl:uniform-block-binding glyph-program (gl:get-uniform-block-index glyph-program "Matrices") 0)

    (gl:bind-vertex-array 0)))

(defun setup-freetype-program (device)
  (let* ((glyph-program       (gl:create-program))
         (glyph-vao           (gl:gen-vertex-array))
         (glyph-buffer        (gl:gen-buffer))
         (glyph-index-buffer  (gl:gen-buffer)))

    (gl:attach-shader glyph-program (create-shader "boxgl-freetype-glyph.vs" :vertex-shader))
    (gl:attach-shader glyph-program (create-shader "boxgl-freetype-glyph.fs" :fragment-shader))
    (gl:link-program glyph-program)
    (log:debug "~%glyph-program infolog: ~A" (gl:get-program-info-log glyph-program))

    (gl:pixel-store :unpack-alignment 1)

    (gl:bind-buffer :element-array-buffer glyph-index-buffer)
      (let ((arr (gl:alloc-gl-array :unsigned-short 6))
            (indexes #(0 1 3
                       1 2 3)))
        (dotimes (i (length indexes))
          (setf (gl:glaref arr i) (aref indexes i)))
        (gl:buffer-data :element-array-buffer :static-draw arr)
        (gl:free-gl-array arr))
      (gl:bind-buffer :element-array-buffer 0)

    (gl:bind-vertex-array glyph-vao)
    (gl:bind-buffer :array-buffer glyph-buffer)
    (%gl:buffer-data :array-buffer (* 4 6 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 4 :float :false (* 4 *cffi-float-size*) 0)
    (gl:bind-buffer :element-array-buffer glyph-index-buffer)

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-texture :texture-2d 0)

    (setf (boxer::ft-glyph-shader device) (make-instance 'boxgl-shader-program :program glyph-program
                                                          :vao glyph-vao       :buffer  glyph-buffer))

    (%gl:uniform-block-binding glyph-program (gl:get-uniform-block-index glyph-program "Matrices") 0)

    (gl:bind-vertex-array 0))

    (let* ((vertices `#(0.0 0.0   0.0 0.0
                        0.0 0.0   0.0 1.0
                        0.0 0.0   1.0 1.0
                        0.0 0.0   1.0 0.0))
           (arr (gl:alloc-gl-array :float (length vertices))))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (setf (ft-glyph-gl-array device) arr)
    )
)

(defun setup-pixmap-program (device)
  (let* ((pixmap-program   (gl:create-program))
         (pixmap-vao       (gl:gen-vertex-array))
         (pixmap-buffer    (gl:gen-buffer)))
    (gl:attach-shader pixmap-program (create-shader "boxgl-pixmap.vs" :vertex-shader))
    (gl:attach-shader pixmap-program (create-shader "boxgl-pixmap.fs" :fragment-shader))
    (gl:link-program pixmap-program)
    (log:debug "~%pixmap-program infolog: ~A" (gl:get-program-info-log pixmap-program))

    ;; Vertex attribute data for the pixmaps will be in the following format:
    ;;    layout 0       layout 1
    ;;      vertice        texture coordinate
    ;;      x    y    z     tx  ty
    ;; #( 0.5  0.5  0.0    1.0 1.0 ...
    ;;
    ;; Total stride is 5

    (gl:bind-vertex-array pixmap-vao)
    (gl:bind-buffer :array-buffer pixmap-buffer)
    ; (%gl:buffer-data :array-buffer (* 4 6 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)
    (gl:enable-vertex-attrib-array 0)
    ;; layout 0 is the x, y, z
    (gl:vertex-attrib-pointer 0 3 :float :false (* 5 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    ;; layout 1 is the texture coords tx, ty
    (gl:vertex-attrib-pointer 1 2 :float :false (* 5 *cffi-float-size*) (* 3 *cffi-float-size*))
    ; (gl:bind-buffer :array-buffer 0)
    ; (gl:bind-vertex-array 0)
    (gl:bind-texture :texture-2d 0)

    (setf (boxer::pixmap-shader device) (make-instance 'boxgl-shader-program :program pixmap-program
                                                       :vao pixmap-vao       :buffer  pixmap-buffer))

    (%gl:uniform-block-binding pixmap-program (gl:get-uniform-block-index pixmap-program "Matrices") 0)))

(defun read-shader-source (filename &key (shaders-dir *shaders-dir*))
  "Reads the relative filename of the shader without any directory, and loads the source into a
  string. Configured to use the Boxer file location for shaders. Returns a string with the source"
  (uiop:read-file-string
    (cl-fad:merge-pathnames-as-file shaders-dir filename)))

(defun create-ortho-matrix (wid hei)
  "Create an orthogonal projection matrix for use in our shaders with the given width and height."
  (let ((ortho (3d-matrices:mortho 0 wid hei 0 -1.0 1.0)))
    (3d-matrices:marr4 ortho)))

(defun create-transform-matrix (x y)
  "Create a transform matrix offset from the origin by x and y."
  (3d-matrices:marr4 (3d-matrices:mtranslation (3d-vectors:vec x y 0))))

(defun update-transform-ubo (device)
  "Update just the transform matrix in the matrices ubo"

  (gl:bind-buffer :uniform-buffer (matrices-ubo device))
  (let* ((arr (gl:alloc-gl-array :float (* 4 4)))
         (i 0))
    (for:for ((item over (3d-matrices:marr4 (3d-matrices:mtranspose (3d-matrices:mat4  (boxgl-device-transform-matrix device))))))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (gl:buffer-sub-data :uniform-buffer arr :offset 0 :buffer-offset (* *cffi-float-size* 16) :size (* *cffi-float-size* (* 4 4)))
    (gl:free-gl-array arr)
         )
  (gl:bind-buffer :uniform-buffer 0))

(defun update-matrices-ubo (device)
  (gl:bind-buffer :uniform-buffer (matrices-ubo device))
  ;; Currently we have two mat4 and one vec4
  (let* ((rgb (boxer::boxgl-device-pen-color device))
         (arr (gl:alloc-gl-array :float (+ 4 2 (* 2 (* 4 4)))))
          (i 0))
    (for:for ((item over (3d-matrices:marr4 (3d-matrices:mtranspose (3d-matrices:mat4  (boxgl-device-ortho-matrix device))))))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (for:for ((item over (3d-matrices:marr4 (3d-matrices:mtranspose (3d-matrices:mat4  (boxgl-device-transform-matrix device))))))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (for:for ((item over (resolution)))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (setf (gl:glaref arr i) (coerce (aref rgb 1) 'single-float))
    (incf i)
    (setf (gl:glaref arr i) (coerce (aref rgb 2) 'single-float))
    (incf i)
    (setf (gl:glaref arr i) (coerce (aref rgb 3) 'single-float))
    (incf i)
    (setf (gl:glaref arr i) (coerce (aref rgb 4) 'single-float))
    (incf i)

    (gl:buffer-sub-data :uniform-buffer arr)
    (gl:free-gl-array arr)
  )
  (gl:bind-buffer :uniform-buffer 0)
)

(defun make-boxgl-device (wid hei)
  "Constructor to create a new boxgl-device renderer. Sets up initial shaders, buffers, etc.
Needs to be run inside an active openGL context. Using the Lispworks OpenGL impl that means
inside an opengl:rendering-on macro invocation."
  (let ((togo (make-instance 'boxgl-device)))
    (setf *cffi-float-size* (cffi:foreign-type-size :float))

    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (setup-freetype-program togo)
    (setup-glyph-atlas-program togo)
    (setup-pixmap-program togo)
    (setf (lines-shader togo)
          (setup-lines-program)

          (dashed-lines-shader togo)
          (setup-lines-program :vertex-shader "boxgl-dashed-lines.vs" :fragment-shader "boxgl-dashed-lines.fs")

          (canvas-shader togo)
          (setup-canvas-program)

          (circle-shader togo)
          (setup-circle-program)

          (simple-shader togo)
          (setup-simple-shader)

          (matrices-ubo togo)
          (gl:gen-buffer)

          (boxgl-device-ortho-matrix togo)
          (create-ortho-matrix wid hei)

          (boxgl-device-transform-matrix togo)
          (create-transform-matrix 0 0)

          (boxer::boxgl-device-pen-color togo)
          #(:rgb 0.0 0.0 0.0 1.0)

          (boxer::boxgl-device-pen-size togo)
          1)

          (setf (circle-xyrad-uni togo)
                (gl:get-uniform-location (shader-program (canvas-shader togo)) "circle_xyrad")

                (text-color-uni togo)
                (gl:get-uniform-location (shader-program (ft-glyph-shader togo)) "textColor")

                (null-gl-elements-array togo)
                (gl:make-null-gl-array :unsigned-short)
                )

    ;; Set up uniform buffer object for projection and transform matrices
    (gl:bind-buffer :uniform-buffer (matrices-ubo togo))
    ;; number of floats: vec2 u_res, mat4 ortho, mat4 transform
    (%gl:buffer-data :uniform-buffer (* *cffi-float-size* (+ 4 2 (* 2 (* 4 4)))) (cffi:null-pointer) :static-draw)
    (gl:bind-buffer :uniform-buffer 0)

    (%gl:bind-buffer-range :uniform-buffer 0 (matrices-ubo togo) 0 (* *cffi-float-size* (+ 4 2 (* 2 (* 4 4)))))
    (update-matrices-ubo togo)
  togo))
