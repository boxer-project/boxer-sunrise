;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/defsys.lisp,v 1.15.15.1 2021/07/12 15:10:02 martin Exp $" -*-

;; Copyright (c) 1987--2021 LispWorks Ltd. All rights reserved.


(in-package "CL-USER")

(defsystem "OPENGL-EXAMPLES"
  ()
  :members
  (("OPENGL" :type :system :root-module nil)
   "arrows"
   "icosahedron"
   "texture"
   "3d-text")
  :rules
  ((:in-order-to :compile :all (:requires (:load "OPENGL")))
   (:in-order-to :compile "icosahedron" (:requires (:load "arrows")))
   ))

