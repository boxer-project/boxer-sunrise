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


 This file contains the delivery script for building boxer executables.

|#

; http://www.lispworks.com/documentation/lw71/DV/html/delivery-198.htm

;; TODO The Application Builder tool doesn't seem to be loading the initialization file...
#+win32 (load "Z:/quicklisp/setup.lisp")

(in-package "CL-USER")
(load-all-patches)
(require :asdf)
(require :uiop)
(setf asdf:*central-registry*
               (list* '*default-pathname-defaults*
                      (uiop:getcwd)
                      #+win32 #P"z:/code/boxer-sunrise2/" ; TODO Sorting out path functions on win32...
                      asdf:*central-registry*))

(load (example-file "opengl/examples/load"))
(setf *features* (cons :opengl *features*))
(setf *features* (cons :freetype-fonts *features*))
(setf *features* (cons :delivering *features*))

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :drakma)
(ql:quickload :cl-json)
(ql:quickload :zpng)
(ql:quickload :qbase64)
(ql:quickload :html-entities)
;; Loading these freetype2 dependencies so they are available when we manually
;; load the freetype compiles filed during startup.
(ql:quickload :alexandria)
(ql:quickload :trivial-garbage)
(ql:quickload :cffi)
;; (ql:quickload :cl-freetype2)

(asdf:load-system :boxer-sunrise2)

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
        #+win32 #P"z:/code/boxer-sunrise2/data/boxer-sunrise/boxersunrise.exe"
        #+mac (merge-pathnames "./data/boxersunrise.app/Contents/MacOS/boxersunrise" (uiop:getcwd))
        0 :interface :capi
        :keep-pretty-printer t
        :startup-bitmap-file nil
        :split t
        )
