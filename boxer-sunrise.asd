#|
  This file is a part of boxer-sunrise project.
  Copyright (c) 2019 Steven Githens (steve@githens.org)
|#

#|
  Author: Steven Githens (steve@githens.org)
|#

(defsystem "boxer-sunrise"
  :version "3.4.17"
  :author "Steven Githens"
  :license ""
  :depends-on (:alexandria
               :cffi
               :cl-fad
               :cl-glu
               :cl-json
               :cl-opengl
               :drakma
               :for
               :html-entities
               :iterate
               :log4cl
               :pngload
               :qbase64
               :quri
               :serapeum
               :trivial-garbage
               :uiop
               :zip
               :zpng
               :3d-matrices
               :3d-vectors
               :boxer-sunrise-core
               )
  :components ((:module "src"
                :components
                (
                 (:module "draw-low-opengl330"
                  :depends-on ()
                  :components ((:file "platform-utils")
                               (:file "pixmap")
                               (:file "graphics-canvas")
                               (:file "simple-line-shapes")
                               (:file "line-shapes")
                               (:file "shader-shapes")
                               (:file "box-models-meshes")
                               (:file "draw-low-opengl330")
                               (:file "opengl-utils")
                               #+(and freetype-fonts (not delivering))
                               (:file "freetype-fonts")
                               (:file "draw-low-opengl")))

                 #+lispworks (:file "boxwin/lw-capi/boxwin-330gl")

                 ;; Beginning of `DRAW` module
                 (:file "draw/draw-high-common")

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
                 #+lispworks (:file "boxwin/lw-capi/error-dialogs")
                 #+lispworks (:file "boxwin/lw-capi/color-picker-menu")
                 #+lispworks (:file "boxwin/lw-capi/lw-toolbar")
                 #+lispworks (:file "boxwin/lw-capi/click-handlers")
                 #+lispworks (:file "boxwin/lw-capi/pane-callbacks")
                 #+lispworks (:file "boxwin/lw-capi/boxwin-opengl")
                 #+lispworks (:file "boxwin/lw-capi/clipboard")

                ;  #+lispworks (:file "boxwin/lw-capi/modern-opengl")

                ;;  (:file "boxwin/lw-capi/hardcopy-lw")
                 (:file "boxwin/mousedoc")
                 (:file "boxwin/boxapp-data")

                 ;; Beginning of `EDITOR` module
                 ;; "The Basic Structure of the Boxer Editor. "
                 (:file "editor/editor")

                 ;; Beginning of `REDISPLAY` module
                 ;; "The redisplay for the Editor"
                 (:file "redisplay/blinkers")
                 (:file "redisplay/disply")
                 (:file "redisplay/lodisp")
                 (:file "redisplay/dev-overlay")
                ;;  (:file "redisplay/repaint-pass-1")
                 (:file "redisplay/repaint-2024")
                 (:file "redisplay/repaint-pass-2")
                 (:file "redisplay/repaint")
                 (:file "redisplay/new-borders")

                 ;; Beginning of `GRFDEFS` module
                 ;; "Definitions for Sprite Graphics"
                 (:file "filesystem/fildfs")
                 (:file "grfdefs/wrap-line")
                 (:file "grfdefs/grobjs")
                 (:file "grfdefs/grfdfs")
                 (:file "grfdefs/gdispl")
                 (:file "grfdefs/boxer-graphics-commands")
                 (:file "grfdefs/graphics-commands")
                 (:file "grfdefs/graphics-clear")

                 ;; Boxtops use some macros from gdispl.lisp, this needs to be a
                 ;; bit reorged.
                 (:file "redisplay/boxtops")

                 ;; Beginning of `EDITOR-HIGH` module
                 ;; "Higher level Editor Utilities"
                 (:file "editor-high/mouse")
                 (:file "editor-high/simple-stream")
                 (:file "editor-high/makcpy")
                 (:file "editor-high/xfile")
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
                 (:file "impexp/full-html-export")
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
                 (:file "filesystem/boxer-document-format")
                 (:file "filesystem/surf")
                 (:file "filesystem/datasources/url")
                 (:file "filesystem/datasources/net-url")
                 (:file "filesystem/datasources/local-url")
                 (:file "filesystem/datasources/http-url")
                 (:file "filesystem/datasources/helpers")
                 (:file "filesystem/formats")
                 ;; Beginning of `TRIGGER` module
                 ;; "Box triggers for Constraint Propagation and Other Fun Stuff"
                 (:file "trigger/trigger")
                 ;; Beginning of `GRAPHICS` module
                 (:file "graphics/grmeth")
                 (:file "graphics/gcmeth")
                 (:file "graphics/turtle")
                 (:file "graphics/sprite")
                 ;; Beginning of `SCRIPT-SUPPORT` module
                 (:file "script-support/applescript")
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
                 (:file "keydef/keydef-lwm")
                 (:file "keydef/keydef-high")
                 (:file "keydef/alternate-names")
                 (:file "keydef/dribble")

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
                 #+lispworks (:file "keydef/keys-new")
                 ;; Beginning of `UTILITIES` module
                 ;; 	   "Various useful tools for Boxer system hacking"
                 (:file "utilities/mcl-utils")
                 ;; Beginning of `printer` module
                 ;; 	   "Utilities for printing out boxes in various ways"
                 ;;  (:file "printer/ps")
                 #+lispworks (:file "printer/deep-print")
                 ;; Beginning of `site` modules
                 ;; 	   "Utilities for Site specific customizations"
                 (:file "site/site")
                 (:file "site/xten")
                 ;; Beginning of `menu` module
                 ;;    "Menus for Harlequin Lispworks on the PC"
                 #+lispworks (:file "boxwin/lw-capi/file-dialogs")
                 #+lispworks (:file "boxwin/lw-capi/lw-menu")
                 #+lispworks (:file "boxwin/lw-capi/preferences-dialog")
                 #+lispworks (:file "boxwin/lw-capi/confirm-quit-dialogs")


                 ;; Beginning of `av` module
                 ;;    "Primitives and support code for controlling VCR's from Mac Boxer"
                 (:file "av/av-stubs")
                 ;; Beginning of `SYSPRIMS` module
                 ;;    "Primitives to tweak default system parameters"
                 (:file "primitives/sysprims")
                 ;; Bootstrap methods to startup boxer
                 (:file "start-boxer")
                )))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "boxer-sunrise-test"))))
