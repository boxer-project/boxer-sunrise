;;;;
;;;; For this renderer research, I want to test rendering a repaint to something like
;;;; WebGL with the more friendly pixi.js library on top of it.
;;;;

;;;; In order to do this, we'll create a seperate file output for each repaint, and for now
;;;; label it in a file with the tick out.  To avoid the huge pile of files for the many
;;;; frame repaints, we'll have a global variable to toggle it, and then we can export a repaint
;;;; then toggle it off. For now, we'll export to a file to a simple line per draw format. In the
;;;; future this could be json over websockets or something.





(in-package :boxer)

(defvar *agnostic-render-on* nil)
(defvar *cur-agnostic-stream* nil)

(defun start-agnostic-render ()
  (when *agnostic-render-on*
    (if *cur-agnostic-stream* (stop-agnostic-render))
    (setf *cur-agnostic-stream*
      (open (format nil "/Users/sgithens/Desktop/boxer-repaint-~a" boxer::*tick*) :direction :output))))

(defun stop-agnostic-render ()
  (close *cur-agnostic-stream*)
  (setf *cur-agnostic-stream* nil) ; is this necessary?
  )

(defun agnostic-render (renderstr)
  (when (and *agnostic-render-on* *cur-agnostic-stream*)
    (format *cur-agnostic-stream* renderstr)
    (format *cur-agnostic-stream* "~%")))
