;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/arrows.lisp,v 1.5.15.2 2021/11/22 20:41:02 martin Exp $" -*-

;; Copyright (c) 1987--2021 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;; Load up the required generic images use #. reader syntax to embed the
;;; images within the fasl.
;;; ----------------------------------------------------------------------


(eval-when (compile eval)
  (defmacro register-button-image (pathname)
    (gp:read-external-image (merge-pathnames pathname (or #+LUCID *compile-file-pathname*
                                                          (current-pathname))))))


(defvar *down-arrow* #.(register-button-image #p"./images/down-arrow.bmp"))
(defvar *up-arrow* #.(register-button-image #p"./images/up-arrow.bmp"))
(defvar *up-disabled* #.(register-button-image #p"./images/up-disabled.bmp"))
(defvar *down-disabled* #.(register-button-image #p"./images/down-disabled.bmp"))

(setf (gp:external-image-transparent-color-index *down-arrow*) 0
      (gp:external-image-transparent-color-index *up-arrow*) 0
      (gp:external-image-transparent-color-index *up-disabled*) 0
      (gp:external-image-transparent-color-index *down-disabled*) 0)
