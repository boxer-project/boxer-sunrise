;; -*- Mode: Lisp -*-


#|



             COPYRIGHT 2007 - 2011 PyxiSystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    Support for OpenGL based pixmaps


Modification History (most recent at top)

 2/ 6/11   %pixblt-{from,to}-screen added &optional buffer arg

|#



;;
;; minimal implementation assuming pixmaps to be 32 bits per pixel
;; in this scheme, pixles will be unsigned ints woth RGB, alpha data packed together
;; in 8 bit quantities

;; we'll use the opengl data formats that mimic win32 pixels to make our porting life easier

(in-package "OPENGL")

(defvar *pixmap-data-type* *gl-rgba*)
(defvar *pixmap-data-format* *gl-unsigned-byte*)

(defconstant *gl-rgba-rev-alpha-byte* (byte 8 24))
(defconstant *gl-rgba-rev-blue-byte* (byte 8 16))
(defconstant *gl-rgba-rev-green-byte* (byte 8 8))
(defconstant *gl-rgba-rev-red-byte* (byte 8 0))

(defstruct (ogl-pixmap (:constructor %make-ogl-pixmap))
  (width 0)
  (height 0)
  (data nil))

(defun make-ogl-pixmap (width height)
  (cond ((and (integerp width)  (not (minusp width))
              (integerp height) (not (minusp height)))
         (%make-ogl-pixmap :width width :height height
                           :data (fli:allocate-foreign-object :type :unsigned-int
                                                              :initial-element 0
                                                              :nelems (* width height))))
        (t (error "Pixmap dimensions, (~S, ~S) must be non-negative integers"
                  width height))))

(defun ogl-free-pixmap (pixmap)
  (when (ogl-pixmap-p pixmap)
    (fli::free-foreign-object (ogl-pixmap-data pixmap))))

;; gak !
(defun ogl-pixmap-depth (pm) (declare (ignore pm)) 32)

(defun %pixblt-to-screen (from-array tx ty wid hei fx fy &optional (buffer *gl-back*))
  (let* ((data (ogl-pixmap-data from-array))
         (pwid (ogl-pixmap-width from-array))
         (phei (ogl-pixmap-height from-array))
         ;; remember that OpenGL coords, starts from lower left
         ;; these are the converted fy value for the array
         (ofy (- phei (+ fy hei))))
    ;; always set the pixel storage params as they may have been munged by previous
    ;; operations
    (gl-pixel-storei *gl-unpack-row-length* (if (= wid pwid) 0 pwid))
    (gl-pixel-storei *gl-unpack-image-height* (if (= hei phei) 0 phei))
    (gl-pixel-storei *gl-unpack-skip-pixels* fx)
    (gl-pixel-storei *gl-unpack-skip-rows* ofy)
    ;; draw into the back buffer
    (gl-draw-buffer buffer)
    ;; position, remeber y coord reversal...
    (gl-raster-pos2-i tx (+ ty hei))
    ;; finally, actuall move the pixels
    (gl-draw-pixels wid hei *pixmap-data-type* *pixmap-data-format* data)))

(defun %pixblt-from-screen (to-array fx fy wid hei tx ty &optional (buffer *gl-front*))
  (let* ((data (ogl-pixmap-data to-array))
         (pwid (ogl-pixmap-width to-array))
         (phei (ogl-pixmap-height to-array))
         ;; remember that OpenGL coords, starts from lower left
         ;; these are the converted fy value for the array
         (oty (- phei (+ ty hei))))
    ;; set all the pixel storage modes...
    (gl-pixel-storei *gl-pack-row-length* (if (= wid pwid) 0 pwid))
    (gl-pixel-storei *gl-pack-image-height* (if (= hei phei) 0 phei))
    (gl-pixel-storei *gl-pack-skip-pixels* tx)
    (gl-pixel-storei *gl-pack-skip-rows* oty)
    ;; read from the (visible) front buffer
    (gl-read-buffer buffer)
    ;; move the pixels....
    (gl-read-pixels fx fy wid hei *pixmap-data-type* *pixmap-data-format* data)))

(defun %pixblt-in-screen (wid hei fx fy tx ty)
  (gl-raster-pos2-i tx (+ ty hei))
  (gl-copy-pixels fx (+ fy hei) wid hei *gl-color*))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun make-offscreen-pixel (red green blue &optional (alpha 255))
  (dpb alpha *gl-rgba-rev-alpha-byte*
       (dpb blue *gl-rgba-rev-blue-byte*
            (dpb green *gl-rgba-rev-green-byte* red))))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun pixel= (p1 p2) (= p1 p2))

;; x and y are in the boxer (top,left = 0,0) sense
(defun pixmap-pixel (pixmap x y)
  (let* ((data (opengl::ogl-pixmap-data pixmap))
         (pwid (opengl::ogl-pixmap-width pixmap))
         (phei (opengl::ogl-pixmap-height pixmap))
         (ogl-y (- phei y 1)))
    (fli::dereference data :index (+ x (* ogl-y pwid)))))

(defun set-pixmap-pixel (newpixel pixmap x y)
  (let* ((data (opengl::ogl-pixmap-data pixmap))
         (pwid (opengl::ogl-pixmap-width pixmap))
         (phei (opengl::ogl-pixmap-height pixmap))
         (ogl-y (- phei y 1)))
    (setf (fli::dereference data :index (+ x (* ogl-y pwid))) newpixel)))

;; to "clear" a pixmap means to write the particular pixel in entire data field
(defun clear-ogl-pixmap (pixmap pixel-value)
  (when (ogl-pixmap-p pixmap)
    (let ((w (ogl-pixmap-width pixmap))
          (h (ogl-pixmap-height pixmap))
          (data (ogl-pixmap-data pixmap)))
      (declare (fixnum w h))
      (dotimes (i (* w h))
        (setf (fli::dereference data :index i) pixel-value)))))

;; basic basic, probably a good candidate form optimization but currently only used by copy-graphics-sheet
(defun %copy-pixmap-data (wid hei from-pixmap from-x from-y to-pixmap to-x to-y)
  (when (and (ogl-pixmap-p from-pixmap)(ogl-pixmap-p to-pixmap))
    (let* ((fw (ogl-pixmap-width from-pixmap))
           (fh (ogl-pixmap-height from-pixmap))
           (fdata (ogl-pixmap-data from-pixmap))
           (tw (ogl-pixmap-width to-pixmap))
           (th (ogl-pixmap-height to-pixmap))
           (tdata (ogl-pixmap-data to-pixmap))
           (actual-wid (min wid (- tw to-x) (- fw from-x)))
           (actual-hei (min hei (- th to-y) (- fh from-y))))
      (dotimes (y actual-hei)
        (dotimes (x actual-wid)
          (setf (fli::dereference tdata :index (+ (+ x to-x)   (* (- th (+ y to-y)   1) tw)))
                (fli::dereference fdata :index (+ (+ x from-x) (* (- fh (+ y from-y) 1) fw)))))))))
