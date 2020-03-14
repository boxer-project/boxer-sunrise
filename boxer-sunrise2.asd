#|
  This file is a part of boxer-sunrise2 project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise2"
  :version "0.1.0"
  :author "Steven Githens"
  :license ""
  :depends-on ("uiop")
  :components ((:module "src"
                :components
                ((:file "boxer-sunrise2")
                 ;; Beginning of `DEFINITIONS` module
                 (:file "macros")
                 (:file "boxdef")
                 (:file "storage")
                 (:file "vrtdef")
                 ;; This is the beginning of the redisplay module from original
                 ;; boxer, when I get a chance to split this up in to modules.
                 (:file "disdcl")
                 ;; Beginning of `DRAW` module
                 (:file "opengl-utils")
                 (:file "capogi")
                ;;  (:file "opengl-utils-glut")
                 (:file "draw-low-opengl")
                 (:file "draw-low-stubs")
                 (:file "draw-high-common")
                 (:file "draw-high-hardware-clip")
                 ;; Beginning of `DISPLAYDEFS` module
                 ;; "Some more Definitions for Drawing things"
                 (:file "disdef")
                 ;; Beginning of `RELATIONSHIPS` module
                 ;; "The basic underlying structure of the Editor is defined here"
                 (:file "infsup")
                 ;; Beginning of `evalvars` module
                 ;; "Definitions necessary for describing evaluator state variables"
                 (:file "varmacs")
                 (:file "vars")



                 ;; Beginning of `EVALDEFS` module
                 ;; "Basic Defs and Macros used by the evaluator including how variables work."
                 (:file "evalmacs")
                 (:file "fdeval")
                 (:file "bind")
                 (:file "funs")
                 (:file "stacks")
                ;;  (:file "eval")
                 ;; Beginning of `EVALPRIMS` module
                 ;; "Evaluator Primitives"
                 ;;(:file "prims")
                 ;; Beginning of `PROCESSES` module
                 ;; "Macros and functions for creating processes"
                 ;;(:file "process")
                 ;; Beginning of `BOXWIN` module
                 ;; "The interface between the Boxer Editor and the window system"
                 ;; these are also window system specific
                 #+lispworks (:file "boxwin-opengl")
                 ;; (:file "hardcopy-lw")
                 ;; (:file "mousedoc")
                 ;; (:file "oglmacreg")
                 ;; (:file "winreg")
                 ;; Beginning of `BOXNET` module
                 ;; operating-system and window-system and lisp-system dependent
                 ;; (:file "boxnet")
                 ;; Beginning of `EDITOR` module
                 ;; "The Basic Structure of the Boxer Editor. "
                 ;; (:file "editor")
                 ;; Beginning of `REDISPLAY` module
                 ;; "The redisplay for the Editor"
                 ;; (:file "lodisp")
                 ;; (:file "repaint")
                 ;; (:file "new-borders")
                 ;; (:file "disply")
                 ;; Beginning of `GRFDEFS` module
                 ;; "Definitions for Sprite Graphics"
                 ;; (:file "grobjs")
                 ;; (:file "grfdfs")
                 ;; (:file "gdispl")
                 ;; Beginning of `EDITOR-HIGH` module
                 ;; "Higher level Editor Utilities"
                 ;; (:file "mouse")
                 ;; (:file "simple-stream")
                 ;; (:file "makcpy")
                 ;; (:file  "xfile")
                 ;; (:file "region")
                 ;; (:file "oglscroll")
                 (:file "mode")
                 ;; (:file "search")
                 (:file "comdef")
                 ;; Beginning of `IMPEXP` module
                 ;; "This imports and exports boxer structure to/from other common file types"
                 ;; (:file "impexp")
                 ;; Beginning of `CHNKPR` module
                 ;; "This is the interface between the Editor and the Evaluator"
                 ;; (:file "chunker")
                 ;; (:file "realprinter")
                 ;; Beginning of `VIRTCOPY` module
                 ;; "The virtual Copy mechanism used by the evaluator"
                 ;; (:file "edvc")
                 ;; (:file "virtcopy")
                 ;; (:file "vcgc")
                 ;; Beginning of `EVALUTILS` module
                 ;; "Utilities for the Evaluator"
                 ;; (:file "transparent")
                 ;; (:file "eval-utils")
                 ;; (:file "errors")
                 ;; Beginning of `EVALUATOR` module
                 ;; "The Boxer Evaluator"
                 ;; (:file "eval-eval")
                 ;; Beginning of `EVAL-INTERFACE` module
                 ;; "Interface to the evaluator"
                 ;; (:file "ev-int")
                 ;; Beginning of `COMPILER` module
                 ;; ""
                 ;; (:file "comp")
                 ;; Beginning of `STEPPER` module
                 ;; "The Movie Stepper"
                 ;; (:file "stepper")
                 ;; Beginning of `FILESYSTEM` module
                 ;; "Saving and Restoring Boxes To/From Files"
                 ;; (:file "fildfs")
                 ;; (:file "dumper")
                 ;; (:file "loader")
                 ;; (:file "clientmacros")
                 ;; (:file "bfslocal")
                 ;; (:file "bfsforeign")
                 ;; (:file "client")
                 ;; (:file "base64")
                 ;; (:file "binhex")
                 ;; (:file "applefile")
                 ;; (:file "surf")
                 ;; (:file "http")
                 ;; (:file "mail")
                 ;; (:file "mailfile")
                 ;; Beginning of `TRIGGER` module
                 ;; "Box triggers for Constraint Propagation and Other Fun Stuff"
                 ;; (:file "trigger")
                 ;; Beginning of `GRAPHICS` module
                 ;; (:file "grmeth")
                 ;; (:file "gcmeth")
                 ;; (:file "turtle")
                 ;; (:file "sprite")
                 ;; Beginning of `SCRIPT-SUPPORT` module
                 ;; (:file "applescript")
                 ;; Beginning of `RPRIMS` module
                 ;; 	   "Recursive primititives (only Update-Shape for now)"
                 ;; (:file "recursive-prims")
                 ;; Beginning of `MATHPRIMS` module
                 ;; "Math primitives"
                 ;; (:file "math-prims")
                 ;; Beginning of `GRPRIMS` module
                 ;; "Graphics Primitives"
                 ;; (:file "grupfn")
                 ;; (:file "grprim1")
                 ;; (:file "grprim2")
                 ;; (:file "grprim3")
                 ;; Beginning of `FILEPRIMS` module
                 ;; 	    "File System Primitives"
                 ;; (:file "file-prims")
                 ;; Beginning of `NETPRIMS` module
                 ;; "Network Primitives"
                 ;; (:file "net-prims")
                 ;; Beginning of `PROCESSPRIMS` module
                 ;; "Process Primitives"
                 ;; (:file "process-prims")
                 ;; Beginning of `primitives` module
                 ;; "Data Manipulators"
                 ;; (:file "dataprims")
                 ;; (:file "build")
                 ;; (:file "misc-prims")
                 ;; Beginning of `KEYDEF`
                 ;; "How Boxer handles Input"
                 ;; (:file "keydef-lwm")
                 ;; (:file "keydef-high")
                 ;; (:file "dribble")
                 ;; Beginning of `COMS` module
                 ;; "Editor Commands Definitions"
                 ;; (:file "comsa")
                 ;; (:file "comsb")
                 ;; (:file "comse")
                 ;; (:file "comsf")
                 ;; (:file "coms-oglmouse")
                 ;; (:file "comss")
                 ;; (:file "coms-fs")
                 ;; (:file "popup")
                 ;; Beginning of `keys` module
                 ;; "Install Commands on Specific Keys"
                 ;; (:file "keys-new")
                 ;; Beginning of `UTILITIES` module
                 ;; 	   "Various useful tools for Boxer system hacking"
                 ;; (:file "mcl-utils")
                 ;; Beginning of `printer` module
                 ;; 	   "Utilities for printing out boxes in various ways"
                 ;; (:file "ps")
                 ;; (:file "deep-print")
                 ;; Beginning of `site` module
                 ;; 	   "Utilities for Site specific customizations"
                 ;; (:file "site")
                 ;; (:file "xten")
                 ;; Beginning of `menu` module
                 ;;    "Menus for Harlequin Lispworks on the PC"
                 (:file "lw-menu")
                 ;; Beginning of `SYSPRIMS` module
                 ;; 	   "Primitives to tweak default system parameters"
                 ;; (:file "sysprims")
                )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "boxer-sunrise2-test"))))
