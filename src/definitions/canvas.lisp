;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;         This file contains classes and structures to store the data needed for each boxer canvas, which could be
;;;;         on a separate window or tab (or in a separate div in a webGL future).  Historically a lot of these pieces
;;;;         of structure where in global variables, making it difficult to separate out to support multiple running
;;;;         Boxer canvases.
;;;;
(in-package :boxer)

(defstruct (blinker (:conc-name blinker-))
  (x 0)
  (y 0)
  (wid 0)
  (hei 0))

(defmethod update-blinker((self blinker) x y wid hei)
  (setf (blinker-x self) x (blinker-y self) y (blinker-wid self) wid (blinker-hei self) hei))

(defstruct (region-row-blinker (:include blinker))
  "Region Row Blinkers. For selected regions in the document. The uid will typically be set to
  the screen-row the region is highlighting."
  (uid nil))

(defclass boxer-canvas
  ()
  ((point-blinker :accessor point-blinker
    :documentation "Contains a `blinker` struct for the current location of the cursor.")
   (outermost-screen-box :initform nil ; :accessor outermost-screen-box
    :documentation "The screen-box instance which is currently fullscreened for the canvas.")

   ;; The following variables track the current location of the content. We keep track of the zoom level and
   ;; the amount the content is scrolled at the global level if it exceeds the window size. We also keep track of
   ;; where the content is anchored. Historically it's been anchored at the top/left corner, as in MS Word's web view,
   ;; but if we want it to be centered and perhaps have a gray background outside the Boxer canvas, then it may be
   ;; shifted, to be centered and down a bit.
   (content-wid :accessor content-wid
    :documentation "The width of all the content (may be larger than the window).")
   (content-hei :accessor content-hei
    :documentation "The height of all the content (may be larger than the window).")
   (horizontal-scroll :accessor horizontal-scroll :initform 0
    :documentation "Horizontal distance scrolled from the left in the window.")
   (vertical-scroll :accessor vertical-scroll :initform 0
    :documentation "Vertical distance scrolled from the top in the window.")
   (content-origin :accessor content-origin :initform '(0 0)
    :documentation "The distance from the top-left that the content starts. Useful in the case of centering the boxer view.")
   (zoom-level :accessor zoom-level :initform 1.0
    :documentation "Current zoom level of the canvas window. As a percentage, reasonable ranges are usually something
                    like 0.25 to 4.0")

   ;; Viewing modes
   (view-layout :accessor view-layout :initform :microworld-view
    :documentation "Layout of the boxer-canvas. Can be:
     :canvas-view Similar to Word's Page View
     :microworld-view Similar to Word's Web or Draft View")
   (page-size :accessor page-size :initform '(800 1100)
    :documentation "")

   (active :accessor active :initform t
    :documentation "Boolean that determines whether or not the window containing this canvas
                    is currently active. Primarily this is so we can make a decision to stop
                    rendering repeatedly when our window isn't the active window in the OS.")

   ;; Current Colors
   (backdrop-color :accessor backdrop-color :initform *white*)
    ))

(defmethod port-width ((self boxer-canvas))
  "Give the width of the port or widget container the Boxer documents canvas. Calculation will be different
   depending on the GL and Widget toolkit used. Needed for scrollbars, panning, and other types of interactions."
  (error "boxer-canvas port-width needs an toolkit specific implementation"))

(defmethod port-height ((self boxer-canvas))
  "See port-width for description."
  (error "boxer-canvas port-height needs an toolkit specific implementation"))

(defmethod activate-canvas-view ((self boxer-canvas))
  (setf ;*backdrop-color* #(:rgb 0.89 0.89 0.89)
        (content-origin self) (list 120 20)
        (view-layout self) :canvas-view
        (backdrop-color self) #(:rgb 0.89 0.89 0.89)))

(defmethod activate-microworld-view ((self boxer-canvas))
  (setf ;*backdrop-color* *white*
        (content-origin self) (list 0 0)
        (view-layout self) :microworld-view
        (backdrop-color self) *white*))

(defmethod viewport-to-document-x ((self boxer-canvas) x)
  "Takes the actual x location on the Boxer documents viewport, and translates it to be the X value for the
  outermost screen box.  If the document is panned in the window, or if it is zoomed, this adjusts the
  value so it can be used for internal document interactions."
  (- (* x (/ 1 (zoom-level self))) (horizontal-scroll self)))

(defmethod viewport-to-document-y ((self boxer-canvas) y)
  "see port-to-document-x"
  (- (* y (/ 1 (zoom-level self))) (vertical-scroll self)))

(defmethod adjust-global-scroll ((self boxer-canvas))
  "Updates the global transform matrix based on the current values of the top level global scrollbars."
  (setf (boxer::boxgl-device-transform-matrix bw::*boxgl-device*)
        (boxer::create-transform-matrix (horizontal-scroll self) (vertical-scroll self))))

(defmethod v-scrollable? ((self boxer-canvas))
 (> (content-hei self) (port-height self)))

(defmethod h-scrollable? ((self boxer-canvas))
 (> (content-wid self) (port-width self)))

(defun reset-global-scrolling ()
  "Move the global os scrollbars back to 0,0 along with the GL transform."
  (setf (horizontal-scroll *boxer-pane*) 0)
  (setf (vertical-scroll *boxer-pane*) 0)
  (boxer::set-transform bw::*boxgl-device* 0 0))
