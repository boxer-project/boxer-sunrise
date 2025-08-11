;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/load.lisp,v 1.12.1.1 2021/07/12 15:10:01 martin Exp $" -*-

;; Copyright (c) 1987--2021 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(load (current-pathname "defsys"))

(compile-system "OPENGL" :load t :target-directory (get-temp-directory))


