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

;;;; special file readers
;; these can be platform specific (i.e. :pict for the mac) or
;; they can work across platforms for defined standards like GIF
;; EVERYTHING must support the types :boxer and :text

;; this should eventually use /etc/magic
(defun file-type (filename)
  "Returns the symbol for the file type that determines which reader/write will be used by
  box"
  (cond ((and (probe-file filename) (boxer-file-contents? filename))
         :application/box)
        ((equal (pathname-type filename) "box")
         :application/box)
        ((equal (pathname-type filename) "boxer")
         :application/boxer.document)
        (t
         :text)
    ))

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
(deffile-type-reader :boxer load-binary-box-internal)  ;; historical boxes may need this
(deffile-type-reader :application/box load-binary-box-internal)
(deffile-type-reader :application/boxer.document load-boxer-document-zipped)

(deffile-type-reader :text   read-text-file-internal)  ;; historical boxes may need this
(deffile-type-reader :text/plain   read-text-file-internal)


;;;; special file writers

(defvar *special-file-writers* nil)

;; TYPE is a keyword returned by the file-type function
;; FUNCTION is a function that takes 1 arg, a filename, and should return a box
(defmacro deffile-type-writer (type function)
  `(progn
    (unless (fast-memq ',type *special-file-writers*)
      (push ',type *special-file-writers*))
    (setf (get ',type 'file-type-writer-function) ',function)))

(defun get-special-file-writer (type)  (get type 'file-type-writer-function))

;; the basic file writers...
(deffile-type-writer :boxer load-binary-box-internal)
(deffile-type-writer :application/box load-binary-box-internal)

(deffile-type-writer :text   read-text-file-internal)
(deffile-type-writer :text/plain   read-text-file-internal)

(defun untitled-filename (directory)
  "Creates an untitled filename used to save a new box file. Checks to make sure
   it doens't exist already."
  (do ((i 1 (1+ i))
       (filename nil)
       (togo nil))
      ((not (null togo))
       togo)
    (cond ((= i 1)
           (setf filename "Untitled.box"))
          (t
           (setf filename (format nil "Untitled ~A.box" i))))
    #-emscripten
    (unless (probe-file (cl-fad:merge-pathnames-as-file directory filename))
      (setf togo (cl-fad:merge-pathnames-as-file directory filename)))))
