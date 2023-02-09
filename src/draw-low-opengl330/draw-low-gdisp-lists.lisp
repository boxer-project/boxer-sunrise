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
;;;;         This file contains all of the high level code that the redisplay uses
;;;;         in the OpenGL port
;;;;
;;;;         New work for buffering graphics display lists, very much experimental and in-progress
(in-package :boxer)

(defvar *gl-cur-gdisp-list-cache* nil
  "The current cache we are writing to globally.")

(defclass gl-gdisp-list-cache ()
  ((gdispl-com-count :accessor gdispl-com-count :initform 0) ;; Number of graphics commands in the graphics sheet list. If this stays the same
                                                            ;; we can skip the whole process of looping in gdispl.lisp:redisplay-graphics-sheet
                                                            ;; unless there has been a de-caching change like canvas resize, etc.
   (rect-arr    :accessor rect-arr :initform (gl:alloc-gl-array :float (* 2 (* 6 3)))) ;; 2 triangles, 6 points per vertice

   (buf-count   :accessor buf-count :initform 0) ;; current location in the opengl buffer
   (buf-max     :accessor buf-max   :initform 0) ;; how much we've buffered so far. If we're below this value in a gdisp
                                                  ;; list, we don't need to re-buffer it.

   (vao         :accessor vao    :initform (gl:gen-vertex-array))
   (buffer      :accessor buffer :initform (gl:gen-buffer)))
)

(defun clear-gdisp-list-cache (screen-box)
  (let ((cache (getprop screen-box :gl-gdisp-cache)))
    (when cache
      (setf (buf-count cache) 0
            (buf-max   cache) 0
            (gdispl-com-count cache) 0)
    )))

(defun make-gl-graphics-box-cache ()
  (let ((togo (make-instance 'gl-gdisp-list-cache)))

    (gl:bind-vertex-array (vao togo))
    (gl:bind-buffer :array-buffer (buffer togo))
    (%gl:buffer-data :array-buffer (* 2000000 36 *cffi-float-size*) (cffi:null-pointer) :dynamic-draw)

    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 2 :float nil (* 6 *cffi-float-size*) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 4 :float nil (* 6 *cffi-float-size*) (* 2 *cffi-float-size*))

    (unenable-shader-programs bw::*boxgl-device*)
    togo))

(defun gl-gdisp-list-rect (cache device x y wid hei &key (rgb (boxgl-device-pen-color device))
                                                                 (pen-size (boxgl-device-pen-size bw::*boxgl-device*))
                                                                 (buffered-size (* *cffi-float-size* (* 6 6))))
;   (unless (and (equal (coerce x0 'single-float) (coerce x1 'single-float)) (equal (coerce y0 'single-float) (coerce y1 'single-float)))
    ;; We don't need to buffer this again if it was already added in a previous repaint of the gdisp list
    (when (> (+ (buf-count cache) buffered-size) (buf-max cache))

    (let* ((vertices `#(,(coerce x 'float) ,(coerce y 'float)                  ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce (+ x wid) 'float) ,(coerce (+ y hei) 'float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce x 'float) ,(coerce (+ y hei) 'float)         ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)

                      ,(coerce x 'float) ,(coerce y 'float)                 ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce (+ x wid) 'float) ,(coerce y 'float)         ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ,(coerce (+ x wid) 'float) ,(coerce (+ y hei) 'float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4)
                      ))
          (arr (rect-arr cache)))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))

      (gl:bind-buffer :array-buffer (buffer cache))

      (%gl::buffer-sub-data :array-buffer (buf-count cache)
                           buffered-size
                           (gl::gl-array-pointer arr))

    ; (gl:bind-buffer :array-buffer 0)
     (unenable-shader-programs device)

    ))
    (setf (buf-count cache) (+ (buf-count cache) buffered-size))
; )
)

(defun gl-gdisp-list-line-segment (cache device x0 y0 x1 y1 &key (rgb (boxgl-device-pen-color device))
                                                                 (pen-size (boxgl-device-pen-size bw::*boxgl-device*))
                                                                 (buffered-size (* *cffi-float-size* (* 6 6))))
  (unless (and (equal (coerce x0 'single-float) (coerce x1 'single-float)) (equal (coerce y0 'single-float) (coerce y1 'single-float)))
    ;; We don't need to buffer this again if it was already added in a previous repaint of the gdisp list
    (when (> (+ (buf-count cache) buffered-size) (buf-max cache))

    (let* ((corners (line-by-width-corners x0 y0 x1 y1 pen-size))
          (vertices `#(,(coerce (aref corners 0) 'single-float) ,(coerce (aref corners 1) 'single-float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 0
                       ,(coerce (aref corners 2) 'single-float) ,(coerce (aref corners 3) 'single-float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 1
                       ,(coerce (aref corners 4) 'single-float) ,(coerce (aref corners 5) 'single-float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 2

                       ,(coerce (aref corners 2) 'single-float) ,(coerce (aref corners 3) 'single-float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 1
                       ,(coerce (aref corners 4) 'single-float) ,(coerce (aref corners 5) 'single-float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 2
                       ,(coerce (aref corners 6) 'single-float) ,(coerce (aref corners 7) 'single-float) ,(aref rgb 1) ,(aref rgb 2) ,(aref rgb 3) ,(aref rgb 4) ; point 3
                        ))
          (arr (rect-arr cache)))
      (dotimes (i (length vertices))
        (setf (gl:glaref arr i) (aref vertices i)))
      (gl:bind-buffer :array-buffer (buffer cache))
      (%gl::buffer-sub-data :array-buffer (buf-count cache)
                           buffered-size
                           (gl::gl-array-pointer arr))

     (unenable-shader-programs device)))
    (setf (buf-count cache) (+ (buf-count cache) buffered-size))
))

(defun gl-gdisp-list-draw (cache device)
  (format *standard-output* "~%gl-gdisp-list-draw ~A" (buf-count cache))

  (enable-gl-objects device :program (shader-program (lines-shader device))
                            :vao     (vao cache)
                            :buffer  (buffer cache))

  (gl:draw-arrays :triangles 0 (/ (buf-max cache) 6))
  (setf (buf-max cache) (max (buf-max cache) (buf-count cache)))
  (setf (buf-count cache) 0))
