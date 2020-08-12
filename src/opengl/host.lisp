;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/host.lisp,v 1.4.15.1 2017/01/19 11:51:03 martin Exp $" -*-

;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.


(in-package "USER")

(setf (logical-pathname-translations "OPENGL")
      `(("**;*" ,(merge-pathnames "**/*" (pathname-location *load-truename*)))))

