#|
    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


 This file contains the `start-boxer` function for use in the MacOS .app bundle.

|#
(in-package :boxer)

(defun start-boxer ()
    ;; Here we are adding the Resources/libs directory of the application bundle
    ;; which will libfreetype.6.dylib, as well as any other *.dylib and *.dll files.
    (pushnew
        (make-pathname :directory
            (append (butlast (pathname-directory (lw:lisp-image-name)))
            '("Resources" "libs")))
        cffi:*foreign-library-directories* :test #'equal)

    ;; This adds the directory where we are keeping the compiled version of freetype2,
    ;; such that it can be included eventually using asdf:load-system.
    (setf asdf:*central-registry*
               (list* '*default-pathname-defaults*
                      (make-pathname :directory
                        (append (butlast (pathname-directory (lw:lisp-image-name)))
                        '("Resources" "cl-deps" "cl-freetype2")))
                asdf:*central-registry*))


    ;; sgithens 2020-12-09 This requires some explanation. We are currently manually loading
    ;; the entire cl-freetype2 system, rather than using asdf because:
    ;;  - cffi grovel is invoked on loading :cl-freetype2
    ;;  - this uses compile-file
    ;;  - compile-file is not available on delivered lispworks applications
    ;;  - currently on MacOS unloading of C modules is not supported, so we can't add it as
    ;;    part of the image during delivery
    ;;
    ;;  We are working on isolating the location where cl-freetype2 calls compile-file, or
    ;;  other possible fixes. Hopefully this will be removed in the near future and the
    ;;  asdf:load-system call used instead.
    ;; (asdf:load-system :cl-freetype2)
    (let ((freetype-deps-dir (make-pathname :directory
            (append (butlast (pathname-directory (lw:lisp-image-name)))
            '("Resources" "cl-deps" "cl-freetype2" "src")))))
        (load (merge-pathnames "package.64xfasl" freetype-deps-dir))

        (load (merge-pathnames "ffi/grovel/grovel-freetype2.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/cffi-cwrap.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/cffi-defs.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-lib.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-init.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-basic-types.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-face.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-glyph.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-size.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-outline.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "ffi/ft2-bitmap.64xfasl" freetype-deps-dir))

        (load (merge-pathnames "init.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "face.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "bitmap.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "glyph.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "render.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "outline.64xfasl" freetype-deps-dir))
        (load (merge-pathnames "toy.64xfasl" freetype-deps-dir)))

    ;; Adding the fonts directory based on whereever this MacOS application happens to
    ;; be running from.
    (setf boxer-window::*capogi-font-directory* (make-pathname :directory
                                                    (append (butlast (pathname-directory (lw:lisp-image-name)))
                                                    '("Resources" "Fonts"))) )

    ;; See the above comments on freetype, but we are manually loading our code for it
    ;; here, because the freetype package is not yet declared when we build our system.
    (load (make-pathname :directory
                        (append (butlast (pathname-directory (lw:lisp-image-name)))
                        '("Resources" "cl-deps"))
                   :name "freetype-fonts" :type "lisp"))

    (boxer-window::window-system-specific-make-boxer)
    (boxer-window::window-system-specific-start-boxer)
    ;; Set the appropriate default font we'd like to have when the system starts.
    (boxer-window::font-size-menu-action 3 nil))
