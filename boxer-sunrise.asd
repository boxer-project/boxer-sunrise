#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise"
  :version "3.4.23"
  :author "Steven Githens"
  :license ""
  :depends-on (:alexandria
               (:feature (:not :emscripten) :cl-fad)
               (:feature (:not :emscripten) :drakma)
               (:feature (:not :emscripten) :external-program)
               (:feature (:not :emscripten) :log4cl)
               :quri
               (:feature (:not :emscripten) :zip)
               :boxer-sunrise-core
               #+(or glfw-engine lispworks)
               :boxer-sunrise-opengl
               )
  :components ((:module "src"
                :components
                (
                 ;; temporary workaround until we get rid of the extra boxer-opengl:: references
                 #+(or text-repl-engine emscripten)
                 (:file "draw-low-opengl330/package")
                 #+text-repl-engine
                 (:module "draw-low-empty"
                  :components ((:file "empty-draw-bridge")))

                 (:file "definitions/boxer-styles")

                 ;; Beginning of `DISPLAYDEFS` module
                 ;; "Some more Definitions for Drawing things"
                 (:file "definitions/disdef")

                 ;; Beginning of `RELATIONSHIPS` module
                 ;; "The basic underlying structure of the Editor is defined here"
                 (:file "relationships/infsup")

                 ;; Beginning of `evalvars` module
                 ;; "Definitions necessary for describing evaluator state variables"
                 (:file "evaluator/varmacs")
                 (:file "evaluator/vars")

                 ;; Beginning of `EVALDEFS` module
                 ;; "Basic Defs and Macros used by the evaluator including how variables work."
                 (:file "evaluator/evalmacs")
                 (:file "evaluator/funs")
                 (:file "evaluator/fdeval")
                 (:file "evaluator/stacks")
                 (:file "evaluator/bind")
                 (:file "evaluator/eval")

                 ;; Beginning of `EVALPRIMS` module
                 ;; "Evaluator Primitives"
                 (:file "primitives/prims")

                 ;; Beginning of `PROCESSES` module
                 ;; "Macros and functions for creating processes"
                 (:file "evaluator/process")

                 ;; Beginning of `BOXWIN` module
                 ;; "The interface between the Boxer Editor and the window system"
                 ;; these are also window system specific
                 (:file "boxwin/eval-command-loop")

                 (:file "boxwin/mousedoc")
                 (:file "boxwin/boxapp-data")

                 ;; Beginning of `EDITOR` module
                 ;; "The Basic Structure of the Boxer Editor. "
                 (:file "editor/editor")

                 ;; Beginning of `GRFDEFS` module
                 ;; "Definitions for Sprite Graphics"
                 (:file "filesystem/fildfs")
                 (:file "grfdefs/grobjs")
                 (:file "grfdefs/grfdfs")
                 (:file "grfdefs/gdispl")
                 (:file "grfdefs/boxer-graphics-commands")
                 (:file "grfdefs/graphics-clear")

                 ;; Beginning of `EDITOR-HIGH` module
                 ;; "Higher level Editor Utilities"
                 (:file "editor-high/copy-paste-buffers")
                 (:file "editor-high/mouse-tracking")
                 (:file "editor-high/mouse")
                 (:file "editor-high/simple-stream")
                 (:file "editor-high/makcpy")
                 (:file "editor-high/xfile" :if-feature (:not :emscripten))
                 (:file "editor-high/region")
                 (:file "editor-high/oglscroll")
                 (:file "editor-high/mode")
                 (:file "editor-high/search")
                 (:file "editor-high/comdef")
                 ;; Beginning of `IMPEXP` module
                 ;; "This imports and exports boxer structure to/from other common file types"
                 (:file "impexp/impexp")
                 (:file "impexp/opml-export")
                 (:file "impexp/json-export")
                 ;; Beginning of `CHNKPR` module
                 ;; "This is the interface between the Editor and the Evaluator"
                 (:file "chnkpr/chunker")
                 (:file "chnkpr/realprinter")
                 ;; Beginning of `VIRTCOPY` module
                 ;; "The virtual Copy mechanism used by the evaluator"
                 (:file "virtcopy/virtcopy")
                 (:file "virtcopy/edvc")
                 (:file "virtcopy/vcgc")
                 ;; Beginning of `EVALUTILS` module
                 ;; "Utilities for the Evaluator"
                 (:file "evalutils/transparent")
                 (:file "evalutils/eval-utils")
                 (:file "evalutils/errors")
                 ;; Beginning of `EVALUATOR` module
                 ;; "The Boxer Evaluator"
                 (:file "evaluator/eval-eval")
                 ;; Beginning of `EVAL-INTERFACE` module
                 ;; "Interface to the evaluator"
                 (:file "evaluator/ev-int")
                 ;; Beginning of `COMPILER` module
                 ;; ""
                 (:file "compiler/comp")
                 ;; Beginning of `STEPPER` module
                 ;; "The Movie Stepper"
                 #+lispworks (:file "stepper/stepper")
                 #+lispworks (:file "stepper/stepper-eval")

                 ;; Beginning of `FILESYSTEM` module
                 ;; "Saving and Restoring Boxes To/From Files"
                 (:file "filesystem/dumper")
                 (:file "filesystem/loader")
                 #-emscripten (:file "filesystem/boxer-document-format")
                 (:file "filesystem/surf")
                 (:file "filesystem/datasources/url")
                 (:file "filesystem/datasources/net-url")
                 (:file "filesystem/datasources/local-url")
                 (:file "filesystem/datasources/http-url" :if-feature (:not :emscripten))
                 (:file "filesystem/datasources/helpers")
                 (:file "filesystem/formats" :if-feature (:not :emscripten))
                 ;; Beginning of `TRIGGER` module
                 ;; "Box triggers for Constraint Propagation and Other Fun Stuff"
                 (:file "trigger/trigger")
                 ;; Beginning of `GRAPHICS` module
                 (:file "graphics/grmeth")
                 (:file "graphics/gcmeth")
                 (:file "graphics/turtle")
                 (:file "graphics/sprite")
                 ;; Beginning of `RPRIMS` module
                 ;; 	   "Recursive primititives (only Update-Shape for now)"
                 (:file "primitives/recursive-prims")
                 ;; Beginning of `MATHPRIMS` module
                 ;; "Math primitives"
                 (:file "primitives/math-prims")
                 ;; Beginning of `GRPRIMS` module
                 ;; "Graphics Primitives"
                 (:file "primitives/grupfn")
                 (:file "primitives/grprim1")
                 (:file "primitives/grprim2")
                 (:file "primitives/grprim3")
                 ;; Beginning of `FILEPRIMS` module
                 ;; 	    "File System Primitives"
                 (:file "primitives/file-prims")

                 ;; Beginning of `PROCESSPRIMS` module
                 ;; "Process Primitives"
                 (:file "primitives/process-prims")
                 ;; Beginning of `primitives` module
                 ;; "Data Manipulators"
                 (:file "primitives/dataprims")
                 (:file "primitives/build")
                 (:file "primitives/misc-prims")

                 (:file "primitives/obsolete")

                 ;; Beginning of `KEYDEF`
                 ;; "How Boxer handles Input"
                 (:file "keydef/keydef-high")
                 (:file "keydef/alternate-names")

                 ;; Beginning of `COMS` module
                 ;; "Editor Commands Definitions"
                 (:file "coms/comsa")
                 (:file "coms/comsb")
                 (:file "coms/comse")
                 (:file "coms/comsf")
                 (:file "coms/coms-oglmouse")
                 (:file "coms/comss")
                 (:file "coms/coms-fs")
                 (:file "coms/popup")
                 ;; Beginning of `keys` module
                 ;; "Install Commands on Specific Keys"
                 (:file "keydef/keys-new")
                 ;; Beginning of `UTILITIES` module
                 ;; 	   "Various useful tools for Boxer system hacking"
                 (:file "utilities/mcl-utils")
                 ;; Beginning of `site` modules
                 ;; 	   "Utilities for Site specific customizations"
                 (:file "site/site")

                 ;; Beginning of `SYSPRIMS` module
                 ;;    "Primitives to tweak default system parameters"
                 (:file "primitives/sysprims")

                )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "boxer-sunrise-test"))))
