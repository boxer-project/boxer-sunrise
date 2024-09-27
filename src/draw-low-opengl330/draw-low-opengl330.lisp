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

(defun float-vector (&rest items)
  "Takes an arbitrary number of numbers, and returns them as a vector, ensuring
   they are all single floats that will play nicely on an openGL vbo."
  (map 'vector (lambda (item) (coerce item 'single-float)) items))

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
  ;; offsets in texture coordinates (ie. [0, 1]).
  ;;
  ;; In many cases, especially when we're not zoomed at all, t-width and t-rows
  ;; will be the same as width and rows. However, when we are using a larger font
  ;; size texture to downscale over a smaller one that isn't on the atlas, these
  ;; will be different, as the texture will be larger than the quad we are going to
  ;; render it on.
  tx
  ty
  t-width
  t-rows)

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

   (absolute-lines-shader :accessor absolute-lines-shader)

   (canvas-shader       :accessor canvas-shader)
   (circle-xyrad-uni    :accessor circle-xyrad-uni)

   (circle-shader       :accessor circle-shader)
   (arc-shader          :accessor arc-shader)
   (ellipse-shader      :accessor ellipse-shader)

   ;; Simple Shader
   ;; Uses a color uniform, starting out using for box borders
   ;; to draw them all in one go
   (simple-shader       :accessor simple-shader)
   (borders-arr         :accessor borders-arr :initform (gl:alloc-gl-array :float (* 6 1024))) ; A gl-array specifically for drawing box borders
   (borders-count       :accessor borders-count :initform 0) ; Number of vertices

   (matrices-ubo :accessor matrices-ubo
    :documentation "Integer describing the gl uniform buffer object we are storing our transformation
                    matrices and other information in.")
   (projection-matrix :accessor boxgl-device-projection-matrix)
   (transform-matrix :accessor boxgl-device-transform-matrix
                     :initform (3d-matrices:meye 4))
   (model-matrix :accessor boxgl-device-model-matrix
                 :initform (3d-matrices:meye 4))

   (pen-color :accessor boxgl-device-pen-color) ; current color in #(:rgb 1.0 1.0 1.0 1.0) format
   (pen-size  :accessor boxgl-device-pen-size)
   (line-stipple :accessor line-stipple :initform nil
     :documentation "Boolean to indicate if we're currently drawing dashed lines.")

   (null-gl-elements-array :accessor null-gl-elements-array)

   ;; Currently bound Shader Program, vao, etc
   (cur-program :accessor cur-program :initform 0)
   (cur-vao :accessor cur-vao :initform 0)
   (cur-buffer :accessor cur-buffer :initform 0)))

(defmethod set-transform ((self boxgl-device) h v)
  "Sets the absolute values of the current transform matrix and updates the ubo."
    (setf (boxer::boxgl-device-transform-matrix self)
          (boxer::create-transform-matrix h v))
    (update-transform-matrix-ubo self))

;; (defmethod adjust-transform ((self boxgl-device) h v)
;;   "Translates the current transform matrix by an additional h and v distance.
;;   Does not replace it from scratch. In the repaint code using this macro we typically
;;   see the origin adjusted by some amount, and then un-adjusted by it with the negative
;;   amounts to put it back."
;;   (let* ((current-transform (boxer::boxgl-device-transform-matrix self))
;;          (adjust-matrix (boxer::create-transform-matrix h v))
;;          (new-transform (3d-matrices:m* current-transform adjust-matrix)))
;;     (setf (boxer::boxgl-device-transform-matrix self) new-transform)
;;     (update-transform-matrix-ubo self)))

(defvar *gl-pixmap-texture-count* 0
  "The number of openGL textures we've created for ogl-pixmaps.")

(defun create-pixmap-texture (pixmap)
  "Creates a texture for the pixmap if it doesn't exist yet. If the texture has already been
  generated, does nothing. (Which means the texture still needs to be bound if you're about to
  do something.

  Checks the update-texture-p accessor on the pixmap. If t, it will recopy the data to the
  existing texture."
    (let ((texture-id nil)
          (wid (ogl-pixmap-width pixmap))
          (hei (ogl-pixmap-height pixmap))
          (data (ogl-pixmap-data pixmap)))
      (cond ((= 0 (ogl-pixmap-texture pixmap))
             (setf texture-id (gl:gen-texture))
             (setf (ogl-pixmap-texture pixmap) texture-id)
             (gl:bind-texture :texture-2d texture-id)
             (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
             (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
             (gl:tex-parameter :texture-2d :texture-min-filter :linear)
             (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

             (gl:tex-image-2d :texture-2d 0 :rgba wid hei 0 :rgba :unsigned-byte data)
             (gl:generate-mipmap :texture-2d)
             (setf (ogl-pixmap-update-texture-p pixmap) nil)
             (incf *gl-pixmap-texture-count*))
            ((ogl-pixmap-update-texture-p pixmap)
             (setf texture-id (ogl-pixmap-texture pixmap))
             (gl:bind-texture :texture-2d texture-id)
             (gl:tex-image-2d :texture-2d 0 :rgba wid hei 0 :rgba :unsigned-byte data)
             (gl:bind-texture :texture-2d 0)
             (setf (ogl-pixmap-update-texture-p pixmap) nil))
            (t nil))))

(defun gl-add-pixmap (device from-array to-x to-y wid hei from-x from-y)
  (enable-gl-shader-program device (pixmap-shader device))

  (gl:pixel-store :unpack-alignment 4)

  (create-pixmap-texture from-array ) ;;pwid phei data)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (ogl-pixmap-texture from-array))

  ;; The openGL text-coordinates go from 0 to 1 and originate in the bottom left corner.
  ;; The boxer pixmap coordinates start from 0 and go to the width/heigh in pixels and
  ;; originate in the top left corner.
  (let* ((tex-coord-x1 (/ from-x (ogl-pixmap-width from-array)))
         (tex-coord-y1 (- 1 (/ (+ from-y hei) (ogl-pixmap-height from-array))))
         (tex-coord-x2 (/ (+ from-x wid) (ogl-pixmap-width from-array)))
         (tex-coord-y2 (- 1 (/ from-y (ogl-pixmap-height from-array))))
         (vertices (float-vector to-x          to-y          0.0 tex-coord-x1 tex-coord-y2
                                 to-x          (+ to-y hei)  0.0 tex-coord-x1 tex-coord-y1
                                 (+ to-x wid)  to-y          0.0 tex-coord-x2 tex-coord-y2
                                 (+ to-x wid)  (+ to-y hei)  0.0 tex-coord-x2 tex-coord-y1
                                 to-x          (+ to-y hei)  0.0 tex-coord-x1 tex-coord-y1
                                 (+ to-x wid)  to-y          0.0 tex-coord-x2 tex-coord-y2))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)
    (gl:draw-arrays :triangles 0 6))
  (unenable-shader-programs device))

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
         (xpos (+ x bearing-x))
         (font-hei (ogl-font-height font))
         (ypos (if baseline-bot
                 (- (- (+ y font-hei) bearing-y) (* font-hei 0.2)) ;; This scaling by 0.2 of the font height is some
                 (- (- y bearing-y) (* font-hei 0.2))))            ;; pixel pushing for the current repaint layout.
         (ypos+h (+ ypos h))
         (xpos+w (+ xpos w))
         (tx (box-glyph-tx glyph))
         (ty (box-glyph-ty glyph)))
    (enable-gl-shader-program device (glyph-atlas-shader device))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (glyph-atlas-texture-id atlas))

    ;; make the vertices array
    (let* ((vertices (float-vector xpos   ypos       0.0 tx                                     ty                                       (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;; 0.0 0.0
                                   xpos   ypos+h     0.0 tx                                     (+ ty (/ h (glyph-atlas-height atlas)))  (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;;0.0 1.0
                                   xpos+w ypos+h     0.0 (+ tx (/ w (glyph-atlas-width atlas))) (+ ty (/ h (glyph-atlas-height atlas)))  (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4)

                                   ; These 2 can be removed when using the element buffer
                                   xpos   ypos       0.0 tx                                     ty                                       (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;; 0.0 0.0
                                   xpos+w ypos+h     0.0 (+ tx (/ w (glyph-atlas-width atlas))) (+ ty (/ h (glyph-atlas-height atlas)))  (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4)

                                   xpos+w ypos       0.0 (+ tx (/ w (glyph-atlas-width atlas))) ty                                       (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4) ;; 1.0 0.0
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
         (xpos (+ x bearing-x))
         (font-hei (ogl-font-height font))
         (ypos (if baseline-bot
                 (- (- (+ y font-hei) bearing-y) (* font-hei 0.2)) ;; This scaling by 0.2 of the font height is some
                 (- (- y bearing-y) (* font-hei 0.2))))            ;; pixel pushing for the current repaint layout.
         (ypos+h (+ ypos h))
         (xpos+w (+ xpos w))
         (tx (box-glyph-tx glyph))
         (ty (box-glyph-ty glyph)))
    (enable-gl-shader-program device (ft-glyph-shader device))
    (gl:uniformf color-uniform (aref rgb 1) (aref rgb 2) (aref rgb 3))
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d (glyph-atlas-texture-id atlas))

    ;; make the vertices array
    (let* ((vertices (float-vector xpos   ypos       tx                                     ty ;; 0.0 0.0
                                   xpos   ypos+h     tx                                     (+ ty (/ h (glyph-atlas-height atlas)))  ;;0.0 1.0
                                   xpos+w ypos+h     (+ tx (/ w (glyph-atlas-width atlas))) (+ ty (/ h (glyph-atlas-height atlas)))
                                   ; xpos   ypos     0.0 0.0
                                   ; xpos+w ypos+h   1.0 1.0
                                   xpos+w ypos       (+ tx (/ w (glyph-atlas-width atlas))) ty ;; 1.0 0.0
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
    (unenable-shader-programs device)
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
         (x (coerce x 'single-float))
         (y (coerce y 'single-float))
         (xpos (+ x bearing-x))
         (font-hei (ogl-font-height font))
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
        (cond
          ;; If there is an active gl model that needs update
          ((and *cur-gl-model-screen-obj* (needs-update *cur-gl-model-screen-obj*))
           (progn
             (setf prev-glyph (add-char *cur-gl-model-screen-obj* device cur-x y c :baseline-bot t :font font))
             ;; If the above glyph isn't on the texture atlas, we immediately render it and get the glyph
             ;; so we can keep accumulating the length of the string from it's dimensions
             (unless prev-glyph
               (setf prev-glyph (gl-add-char device cur-x y c :baseline-bot t :font font)))))
          (*cur-gl-model-screen-obj*
           ;; We still have to go through the glyphs to immediate print any chars that aren't on the texture atlas
           (setf prev-glyph
                 (get-glyph *freetype-glyph-atlas* `(,(opengl-font-fontspec font) ,c ,(coerce *font-size-baseline* 'float))))
           (unless prev-glyph
               (setf prev-glyph (gl-add-char device cur-x y c :baseline-bot t :font font))))
          (t
           (setf prev-glyph (gl-add-char device cur-x y c :baseline-bot t :font font))))
        (setf cur-x (+ cur-x (box-glyph-advance prev-glyph))))))

(defun gl-add-point (device x0 y0 &key (rgb (boxgl-device-pen-color device)))
  (enable-gl-shader-program device (lines-shader device))

  (let* ((vertices (float-vector x0 y0 0.0 (aref rgb 1) (aref rgb 2) (aref rgb 3) (aref rgb 4)))
         (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr))

  (gl:draw-arrays :points 0 1)
  (unenable-shader-programs device))

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

(defun create-shader (glsl-filename shader-type)
  "Creates and return the int id for a new shader. `glsl-filename` should be the relative
  filename, such as 'boxgl-stuff.vs'. Shader type should be the symbol type accepted by cl-opengl
  such as :vertex-shader or :fragment-shader"
  (let ((togo (gl:create-shader shader-type)))
    (gl:shader-source togo (read-shader-source glsl-filename))
    (gl:compile-shader togo)
    (log:info "~%shader: ~A infolog: ~A" glsl-filename (gl:get-shader-info-log togo))
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

    (let* ((vertices  #(0.0 0.0   0.0 0.0
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
  (let* ((ortho (3d-matrices:mortho 0 wid hei 0 -1.0 1.0))
         (zoom  (zoom-level *boxer-pane*))
         (origin (content-origin *boxer-pane*))
         (trans (3d-matrices:mtranslation (3d-vectors:vec (first origin) (second origin) 0))))
    (3d-matrices:nmscale trans (3d-vectors:vec zoom zoom 1.0))
    (setf ortho (3d-matrices:m*  ortho trans))

    ortho))

(defun create-transform-matrix (x y)
  "Create a transform matrix offset from the origin by x and y."
  (3d-matrices:mtranslation (3d-vectors:vec x y 0)))

(defun update-transform-matrix-ubo (device)
  "Update just the transform matrix in the matrices ubo. Slightly more efficient than updating everything in our
   UBO array."
  (gl:bind-buffer :uniform-buffer (matrices-ubo device))
  (let ((arr (gl:alloc-gl-array :float (* 4 4)))
        (i 0))
    (for:for ((item over (3d-matrices:marr4 (3d-matrices:mtranspose (boxgl-device-transform-matrix device)))))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (gl:buffer-sub-data :uniform-buffer arr :offset 0 :buffer-offset (* *cffi-float-size* (* 2 (* 4 4))) :size (* *cffi-float-size* (* 4 4)))
    (gl:free-gl-array arr))
  (gl:bind-buffer :uniform-buffer 0))

(defun update-matrices-ubo (device)
  (gl:bind-buffer :uniform-buffer (matrices-ubo device))
  ;; Currently we have two mat4 and one vec4
  (let* ((rgb (boxer::boxgl-device-pen-color device))
         ;; Resolution is 2 floats and we have 3 4x4 matrices
         (arr (gl:alloc-gl-array :float (+ 2 (* 3 (* 4 4)))))
          (i 0))
    (for:for ((item over (3d-matrices:marr4 (3d-matrices:mtranspose (boxgl-device-model-matrix device)))))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (for:for ((item over (3d-matrices:marr4 (3d-matrices:mtranspose (boxgl-device-projection-matrix device)))))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (for:for ((item over (3d-matrices:marr4 (3d-matrices:mtranspose (boxgl-device-transform-matrix device)))))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))
    (for:for ((item over (resolution)))
      (setf (gl:glaref arr i) (coerce item 'single-float))
      (incf i))

    (gl:buffer-sub-data :uniform-buffer arr)
    (gl:free-gl-array arr)
  )
  (gl:bind-buffer :uniform-buffer 0)
)

(defun update-boxgl-programs (&key (device bw::*boxgl-device*))
  "Creates and sets all our GL shader programs. Can be used at runtime to reload programs when we're working on
   shaders."
  (setf (lines-shader device)
        (setup-lines-program)

        (dashed-lines-shader device)
        (setup-lines-program :vertex-shader "boxgl-dashed-lines.vs" :fragment-shader "boxgl-dashed-lines.fs")

        (absolute-lines-shader device)
        (setup-lines-program :vertex-shader "boxgl-absolute-lines.vs" :fragment-shader "boxgl-lines.fs")

        (canvas-shader device)
        (setup-canvas-program)

        (circle-shader device)
        (setup-circle-program)

        (arc-shader device)
        (setup-arc-program)

        (ellipse-shader device)
        (setup-ellipse-program)

        (simple-shader device)
        (setup-simple-shader)
))

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
    (update-boxgl-programs :device togo)
    (setf (matrices-ubo togo)
          (gl:gen-buffer)

          (boxgl-device-projection-matrix togo)
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
    ;; number of floats: mat4 model, mat4 projection, mat4 transform, vec2 u_res
    (%gl:buffer-data :uniform-buffer (* *cffi-float-size* (+ 2 (* 3 (* 4 4)))) (cffi:null-pointer) :static-draw)
    (gl:bind-buffer :uniform-buffer 0)

    (%gl:bind-buffer-range :uniform-buffer 0 (matrices-ubo togo) 0 (* *cffi-float-size* (+ 4 2 (* 3 (* 4 4)))))
    (update-matrices-ubo togo)
  togo))
