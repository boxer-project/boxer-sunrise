;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/load.lisp,v 1.7.14.2 2017/11/10 13:00:26 martin Exp $" -*-

;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(load (current-pathname "../load"))

(load (current-pathname "defsys"))

(compile-system "OPENGL-EXAMPLES" :load t :target-directory (get-temp-directory))


