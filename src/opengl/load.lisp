;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/load.lisp,v 1.11.14.2 2017/11/10 12:59:26 martin Exp $" -*-

;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(load (current-pathname "defsys"))

(compile-system "OPENGL" :load t :target-directory (get-temp-directory))


