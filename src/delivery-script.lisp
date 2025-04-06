#|
    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


 This file contains the delivery script for building boxer executables.

|#

; http://www.lispworks.com/documentation/lw71/DV/html/delivery-198.htm

;; sunrise-109 This should likely go in .lispworks, but I don't want to depend
;; on folks having that set up properly.
;; https://www.lispworks.com/documentation/lw70/LW/html/lw-684.htm
(lw:set-default-character-element-type 'character)

;; TODO The Application Builder tool doesn't seem to be loading the initialization file...
#+win32 (load "Z:/quicklisp/setup.lisp")

(in-package "CL-USER")
(load-all-patches)
(require :asdf)
(require :uiop)

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defvar *boxer-project-dir* (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(setf asdf:*central-registry*
               (list* '*default-pathname-defaults*
                      *boxer-project-dir*
                      #+win32 #P"z:/code/boxer-sunrise/" ; TODO Sorting out path functions on win32...
                      asdf:*central-registry*))

; (load (example-file "opengl/examples/load"))
(ql:quickload :cl-fad)
#+(and lispworks x64 macosx) (load (cl-fad:merge-pathnames-as-file *boxer-project-dir* "src/opengl-lw-8/examples/load.lisp"))
#+(and lispworks x64 win32) (load #P"z:/code/boxer-sunrise/src/opengl-lw-8/examples/load.lisp")

(setf *features* (cons :delivering *features*))

(ql:quickload :log4cl)
(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :zpng)
(ql:quickload :qbase64)
(ql:quickload :html-entities)
(ql:quickload :quri)

;; Loading these freetype2 dependencies so they are available when we manually
;; load the freetype compiles filed during startup.
(ql:quickload :alexandria)
(ql:quickload :trivial-garbage)
(ql:quickload :cffi)
(ql:quickload :zip)

(ql:quickload :cl-opengl)
(ql:quickload :pngload)
(ql:quickload :3d-vectors)
(ql:quickload :3d-matrices)
(ql:quickload :iterate)
(ql:quickload :for)

;; (ql:quickload :cl-freetype2)

(asdf:load-system :boxer-sunrise)

(deliver 'boxer::start-boxer
        ;; We are currently using a custom tailored application folder template, that was
        ;; originally created using this method, but we've made customizations to. At some
        ;; point we could refine this to create the final app bundle with all our customizations
        ;; on each run, rather than having to maintain our own copy. We do have to add our
        ;; current set of fonts, and icons to it. Other than that we have some random property
        ;; changes in the Info.plist.
        ;;  #+:cocoa
        ;;  (create-macos-application-bundle
        ;;   (merge-pathnames "./data/boxersunrise.app" (uiop:getcwd))
        ;;   ;; Do not copy file associations...
        ;;   :document-types nil
        ;;   ;; ...or CFBundleIdentifier from the LispWorks bundle
        ;;   :identifier "org.boxer.BoxerSunrise"
        ;;   )

        ;; TODO still working on getting these paths sorted out on win32, unfortunately hardcoded at the moment.
        #+win32 #P"z:/code/boxer-sunrise/data/boxer-sunrise/boxersunrise.exe"
        #+mac (cl-fad:merge-pathnames-as-file *boxer-project-dir* "data/boxersunrise.app/Contents/MacOS/boxersunrise")
        0 :interface :capi
        :keep-pretty-printer t
        :startup-bitmap-file nil
        :split t
        )
