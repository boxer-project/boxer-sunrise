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

(in-package :boxer)

(defvar *pixmap-data-type* :rgba)
(defvar *pixmap-data-format* :unsigned-byte)
(defvar *pixmap-ffi-type* :unsigned-int
  "The pointer type for foreign-alloc and reference of pixmap data")

(defvar *gl-rgba-rev-alpha-byte* (byte 8 24))
(defvar *gl-rgba-rev-blue-byte* (byte 8 16))
(defvar *gl-rgba-rev-green-byte* (byte 8 8))
(defvar *gl-rgba-rev-red-byte* (byte 8 0))

(defclass ogl-pixmap ()
  ((width             :initarg :width             :initform 0   :accessor ogl-pixmap-width)
   (height            :initarg :height            :initform 0   :accessor ogl-pixmap-height)
   (texture           :initarg :texture           :initform 0   :accessor ogl-pixmap-texture)
   (update-texture-p  :initarg :update-texture-p  :initform nil :accessor ogl-pixmap-update-texture-p
    :documentation "If this is true, then changes have been made to the pixel data and should be
                    redrawn to the GL texture.")
   (data              :initarg :data              :initform nil :accessor ogl-pixmap-data)
   (depth             :initarg :depth             :initform 32  :accessor ogl-pixmap-depth)))

(defun make-ogl-pixmap (width height &key (texture 0))
  (cond ((and (integerp width)  (not (minusp width))
              (integerp height) (not (minusp height)))
         (make-instance 'ogl-pixmap :width width :height height :texture texture
                           :data (cffi:foreign-alloc *pixmap-ffi-type*
                                                     :initial-element 0
                                                     :count (* width height))))
        (t (error "Pixmap dimensions, (~S, ~S) must be non-negative integers"
                  width height))))

(defgeneric ogl-pixmap-p (x) (:method (x) nil) (:method ((x ogl-pixmap)) t))

(defun ogl-free-pixmap (pixmap)
  (when (ogl-pixmap-p pixmap)
    (cffi:foreign-free (ogl-pixmap-data pixmap))
    ))

(defun pixblt-from-screen (pixmap fx fy wid hei)
  (boxer-opengl::%pixblt-from-screen pixmap fx fy wid hei))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun make-offscreen-pixel (red green blue &optional (alpha 255))
  (dpb alpha *gl-rgba-rev-alpha-byte*
       (dpb blue *gl-rgba-rev-blue-byte*
            (dpb green *gl-rgba-rev-green-byte* red))))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun pixel= (p1 p2) (= p1 p2))

;; x and y are in the boxer (top,left = 0,0) sense
(defun pixmap-pixel (pixmap x y)
  (let* ((data (ogl-pixmap-data pixmap))
         (pwid (ogl-pixmap-width pixmap))
         (phei (ogl-pixmap-height pixmap))
         (ogl-y (- phei y 1)))
    (cffi:mem-aref data *pixmap-ffi-type* (+ x (* ogl-y pwid)))))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun pixel->color (pixel)
  `#(:rgb ,(/ (ldb *gl-rgba-rev-red-byte* pixel)   255.0)
          ,(/ (ldb *gl-rgba-rev-green-byte* pixel) 255.0)
          ,(/ (ldb *gl-rgba-rev-blue-byte* pixel)  255.0)
          ,(/ (ldb *gl-rgba-rev-alpha-byte* pixel) 255.0)))

(defun pixmap-pixel-color (pixmap x y)
  (pixel->color (pixmap-pixel pixmap x y)))

(defun set-pixmap-pixel (pixmap x y newpixel)
  (let* ((data (ogl-pixmap-data pixmap))
         (pwid (ogl-pixmap-width pixmap))
         (phei (ogl-pixmap-height pixmap))
         (ogl-y (- phei y 1)))
    (setf (cffi:mem-aref data *pixmap-ffi-type* (+ x (* ogl-y pwid))) newpixel))
  (setf (ogl-pixmap-update-texture-p pixmap) t))

(defsetf pixmap-pixel set-pixmap-pixel)

;; to "clear" a pixmap means to write the particular pixel in entire data field
(defun clear-ogl-pixmap (pixmap pixel-value)
  (when (ogl-pixmap-p pixmap)
    (let ((w (ogl-pixmap-width pixmap))
          (h (ogl-pixmap-height pixmap))
          (data (ogl-pixmap-data pixmap)))
      (declare (fixnum w h))
      (dotimes (i (* w h))
        (setf (cffi:mem-aref data *pixmap-ffi-type* i) pixel-value)))
    (setf (ogl-pixmap-update-texture-p pixmap) t)))

;; basic basic, probably a good candidate form optimization but currently only used by copy-graphics-sheet
(defun copy-pixmap-data (wid hei from-pixmap from-x from-y to-pixmap to-x to-y)
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
          (setf
              (cffi:mem-aref tdata *pixmap-ffi-type* (+ (+ x to-x)   (* (- th (+ y to-y)   1) tw)))
              (cffi:mem-aref fdata *pixmap-ffi-type* (+ (+ x from-x) (* (- fh (+ y from-y) 1) fw)))))))
    (setf (ogl-pixmap-update-texture-p to-pixmap) t)))

(defun copy-pixmap (pixmap)
  (let* ((w (ogl-pixmap-width pixmap))
         (h (ogl-pixmap-height pixmap))
         (new-pixmap (make-ogl-pixmap w h)))
    (copy-pixmap-data w h pixmap 0 0 new-pixmap 0 0)
    new-pixmap))

(defun deallocate-system-dependent-structures (box)
  (let ((gi (graphics-info box)))
    (when (graphics-sheet? gi)
      (let ((bm (graphics-sheet-bit-array gi)))
        (unless (null bm) (ogl-free-pixmap bm))))))
