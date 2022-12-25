;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/load.lisp,v 1.8.1.1 2021/07/12 15:10:02 martin Exp $" -*-

;; Copyright (c) 1987--2021 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(load (current-pathname "../load"))

(load (current-pathname "defsys"))

(compile-system "OPENGL-EXAMPLES" :load t :target-directory (get-temp-directory))


