;;;;
;;;;     Boxer
;;;;     Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;     Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;     used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;     Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;     https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                          +-Data--+
;;;;                 This file is part of the | BOXER | system
;;;;                                          +-------+
;;;;

(in-package :boxer)

;; this should eventually use /etc/magic
(defun file-type (filename)
  (if (boxer-file-contents? filename) :boxer :text))

(defvar *error-on-unknown-file-type* nil)

(defvar *special-file-readers* nil)

;; TYPE is a keyword returned by the file-type function
;; FUNCTION is a function that takes 1 arg, a filename, and should return a box
(defmacro deffile-type-reader (type function)
  `(progn
    (unless (fast-memq ',type *special-file-readers*)
      (push ',type *special-file-readers*))
    (setf (get ',type 'file-type-reader-function) ',function)))

(defun get-special-file-reader (type)  (get type 'file-type-reader-function))

;; the basic file readers...
(deffile-type-reader :boxer load-binary-box-internal)

(deffile-type-reader :text   read-text-file-internal)
