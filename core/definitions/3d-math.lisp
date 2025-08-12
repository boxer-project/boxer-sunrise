(defpackage :3d-vectors
  (:use :cl)
  (:export
   :vec3
   :vec
   :vx3
   :vy3
   :vz3
   :+vx+
   :+vy+
   :+vz+
   :nv+
   :nv*
   :nvrot))

(in-package :3d-vectors)

(declaim (inline %vec3))
(defstruct (vec3 (:conc-name NIL)
                 (:constructor %vec3 (%vx3 %vy3 %vz3))
                 (:copier vcopy3)
                 (:predicate vec3-p))
  (%vx3 (ensure-float 0) :type single-float)
  (%vy3 (ensure-float 0) :type single-float)
  (%vz3 (ensure-float 0) :type single-float))

(defun vec (x y z)
  (%vec3 (coerce x 'single-float) (coerce y 'single-float) (coerce z 'single-float)))

(defun vx3 (v)
  (%vx3 v))

(defun vy3 (v)
  (%vy3 v))

(defun vz3 (v)
  (%vz3 v))


(defparameter +vx+ (vec 1.0 0.0 0.0))
(defparameter +vy+ (vec 0.0 1.0 0.0))
(defparameter +vz+ (vec 0.0 0.0 1.0))

(defun nv+ (vec1 vec2)
  (setf (vx3 vec1) (+ (vx3 vec1) (vx3 vec2))
        (vy3 vec1) (+ (vy3 vec1) (vy3 vec2))
        (vz3 vec1) (+ (vz3 vec1) (vz3 vec2))))

(defun nv* (vec1 scale)
  (setf (vx3 vec1) (* (vx3 vec1) scale)
        (vy3 vec1) (* (vy3 vec1) scale)
        (vz3 vec1) (* (vz3 vec1) scale)))

(defun nvrot (v axi phi)
  "STUB"
  v)

(defpackage :3d-matrices
  (:use :cl)
  (:export
   :mat4
   :marr4
   :meye
   :m*
   :mortho
   :mperspective
   :mtranslation
   :mtranspose
   :nmrotate
   :nmscale
   :nmtranslate))

(in-package :3d-matrices)

(declaim (inline %mat4))
(defstruct (mat4 (:conc-name NIL)
                 (:constructor %mat4 (marr4))
                 (:copier NIL)
                 (:predicate mat4-p))
  (marr4 NIL )) ; :type (simple-array single-float (16))

(defun mat4 (v)
  (%mat4 (copy-seq v)))

(defun meye (n)
  (%mat4  #(1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0)))

(defun m* (mat1 mat2)
  "STUB"
  mat1)

(defun mortho (left right bottom top near far)
  "STUB"
  (meye 4))

(defun mperspective (fovy aspect near far)
  "STUB"
  (meye 4))

(defun mtranslation (v)
  "STUB"
  (meye 4))

(defun mtranspose (m)
  "STUB"
  (meye 4))

(defun nmrotate (m v angle)
  "STUB"
  (meye 4))

(defun nmscale (m v)
  "STUB"
  (meye 4))

(defun nmtranslate (m v)
  "STUB"
  (meye 4))
