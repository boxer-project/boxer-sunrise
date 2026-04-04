
;; This may be redundant since we have to manually add these to asdf-cc.lisp for now
(setf *features* (cons :embedboxer (cons :embedded-boxer (cons :emscripten (cons :shim-3d-math (cons :text-repl-engine *features*))))))
(require :asdf)
(pushnew "./" ql:*local-project-directories* )
(ql:register-local-projects)
(setf *wasm* t)
(load "./scripts/asdf-cc.lisp")

(trace uiop/pathname:ensure-absolute-pathname)

(when *wasm*
  (setf c::*user-linker-flags*  "-sSHARED_MEMORY=1 -sWASM_WORKERS=1 -pthread" )
  (setf c::*user-cc-flags*  "-sSHARED_MEMORY=1 -sWASM_WORKERS=1 -pthread" ))

(print "Compiler Messages: ")
(print c::*suppress-compiler-messages*)
(setf c::*suppress-compiler-messages* nil)

(print "Features: ")
(print *features*)

(when *wasm*
  ;; (:ASDF-ECL/CC::cross-compile-plan "libboxercore" :ASDF-ECL/CC::*wasm-target*)
  (:ASDF-ECL/CC::cross-compile "libboxercore" :target :ASDF-ECL/CC::*wasm-target*))

(unless *wasm*
  (:ASDF-ECL/CC::cross-compile-plan "libboxercore" :ASDF-ECL/CC::*host-target*)
  (:ASDF-ECL/CC::cross-compile "libboxercore" :target :ASDF-ECL/CC::*host-target*))

(quit)
