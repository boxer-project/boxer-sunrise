;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/defsys.lisp,v 1.15.14.1 2017/01/19 11:51:04 martin Exp $" -*-

;; Copyright (c) 1987--2017 LispWorks Ltd. All rights reserved.


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

