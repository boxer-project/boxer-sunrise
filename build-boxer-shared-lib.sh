#/bin/sh

# Currently, I'm running this twice with different options to :move-here, because I need the local copy to build
# against, and the original copy in the obscure common-lisp cache area to actually load against at runtime as I
# still need to solve the issue with gettting the resulting shared library to be relocatable in our loading.
function build_boxer_sunrise_core_dylib () {
ecl \
    -eval '(setf *features* (cons :embedded-boxer (cons :emscripten (cons :shim-3d-math (cons :text-repl-engine *features*)))))' \
    -eval '(require :asdf)' \
    -eval '(pushnew "./" ql:*local-project-directories* )' \
    -eval '(ql:register-local-projects)' \
    -eval '(asdf:make-build :libboxercore :type :shared-library :monolithic t :move-here "./" :init-name "init_lib_BOXER_CORE_LISP")' \
    -eval '(quit)'

ecl \
    -eval '(setf *features* (cons :embedded-boxer (cons :emscripten (cons :shim-3d-math (cons :text-repl-engine *features*)))))' \
    -eval '(require :asdf)' \
    -eval '(pushnew "./" ql:*local-project-directories* )' \
    -eval '(ql:register-local-projects)' \
    -eval '(asdf:make-build :libboxercore :type :shared-library :monolithic t :init-name "init_lib_BOXER_CORE_LISP")' \
    -eval '(quit)'

}

build_boxer_sunrise_core_dylib
