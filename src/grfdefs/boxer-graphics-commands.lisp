(in-package :boxer)

;;;; 32   BOXER-CHANGE-ALU                             (NEW-ALU)
;;;; 33   BOXER-CHANGE-PEN-WIDTH                       (NEW-WIDTH)
;;;; 34   BOXER-CHANGE-GRAPHICS-FONT                   (NEW-FONT-NO)
;;;; 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
;;;; 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
;;;; 39   BOXER-CENTERED-STRING                        (X Y STRING)
;;;; 40   BOXER-LEFT-STRING                            (X Y STRING)
;;;; 41   BOXER-RIGHT-STRING                           (X Y STRING)
;;;; 42   BOXER-CENTERED-RECTANGLE                     (X Y WIDTH HEIGHT)
;;;; 43   BOXER-DOT                                    (X Y)
;;;; 47   BOXER-CENTERED-BITMAP                        (BITMAP X Y WIDTH HEIGHT)
;;;; 60   BOXER-FILLED-ELLIPSE                         (X Y WIDTH HEIGHT)
;;;; 61   BOXER-ELLIPSE                                (X Y WIDTH HEIGHT)
;;;; 62   BOXER-FILLED-CIRCLE                          (X Y RADIUS)
;;;; 63   BOXER-CIRCLE                                 (X Y RADIUS)

(defmacro boxer-playback-graphics-list (gl &key (start 0) (graphics-canvas nil))
  "In progress work to move all the boxer graphics to turtle coordinates. Will get trued
   up with the defboxer-graphics-handler macros soon."
  `(with-graphics-state (,gl t)
         (do-vector-contents (command ,gl :start ,start)
           (let ((com (aref command 0)))
             (cond ((eq com 33)
                    (draw-boxer-change-pen-width command))
                   ((eq com 35)
                    (draw-boxer-line-segment command))
                   ((eq com 36)
                    (draw-boxer-change-graphics-color command))
                   ((eq com 39)
                    (draw-boxer-centered-string command))
                   ((eq com 40)
                    (draw-boxer-left-string command))
                   ((eq com 41)
                    (draw-boxer-right-string command))
                   ((eq com 42)
                    (draw-boxer-centered-rectangle command))
                   ((eq com 43)
                    (draw-boxer-dot command))
                   ((eq com 47)
                    (draw-boxer-centered-bitmap command))
                   ((eq com 60)
                    (draw-boxer-filled-ellipse command))
                   ((eq com 61)
                    (draw-boxer-ellipse command))
                   ((eq com 62)
                    (draw-boxer-filled-circle command))
                   ((eq com 63)
                    (draw-boxer-circle command))
                   (t
                    nil))))
        ;;  (process-graphics-command-marker command)
))

;; 33   BOXER-CHANGE-PEN-WIDTH              (NEW-WIDTH)

(defun draw-boxer-change-pen-width (com)
  (%set-pen-size (aref com 1)))

;; 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
(defun draw-boxer-line-segment (com)
  "Takes a boxer command starting with 35:
    ex #(35 -0.5 -0.5 0.0 0.5)"
  ;; The y-axis needs to be flipped
  (draw-line (aref com 1) (* -1 (aref com 2)) (aref com 3) (* -1 (aref com 4))))

;; 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
(defun draw-boxer-change-graphics-color (com)
  (%set-pen-color (aref com 1)))

;; 39   BOXER-CENTERED-STRING             (X Y STRING)
;; TODO all these string commands need to check for newlines, see graphics-commands.lisp
(defun draw-boxer-centered-string (com)
  (let* ((wid (string-wid *graphics-state-current-font-no* (aref com 3)))
         (hei (string-hei *graphics-state-current-font-no*))
         (x (- (aref com 1) (/ wid 2)))
         (y (- (aref com 2) (/ hei 2))))
    (draw-string *graphics-state-current-font-no* (aref com 3)
               x y)))

;;;; 40   BOXER-LEFT-STRING          (X Y STRING)
(defun draw-boxer-left-string (com)
  ;; TODO
  (draw-boxer-centered-string com))

;;;; 41   BOXER-RIGHT-STRING         (X Y STRING)
(defun draw-boxer-right-string (com)
  ;; TODO
  (draw-boxer-centered-string com))

;; 42   BOXER-CENTERED-RECTANGLE       (X Y WIDTH HEIGHT)
(defun draw-boxer-centered-rectangle (com)
  (let ((x (aref com 1)) (y (aref com 2)) (w (aref com 3)) (h (aref com 4)))
    (draw-rectangle w h
                   (- x (/ w 2))
                   ;; The y axis is flipped
                   (* -1 (+ y (/ h 2))))))

;; 43   BOXER-DOT                      (X Y)
(defun draw-boxer-dot (com)
  (let ((x (aref com 1)) (y (aref com 2)))
    (draw-rectangle *graphics-state-current-pen-width* *graphics-state-current-pen-width*
                 (- x (/ *graphics-state-current-pen-width* 2))
                 (* -1 (+ y (/ *graphics-state-current-pen-width* 2))))))

;; 47   BOXER-CENTERED-BITMAP          (BITMAP X Y WIDTH HEIGHT)
(defun draw-boxer-centered-bitmap (com)
  (let ((bitmap (aref com 1)) (x (aref com 2)) (y (aref com 3)) (wid (aref com 4)) (hei (aref com 5)))
    (bitblt-to-screen wid hei bitmap 0 0
                      (- x (floor wid 2))
                      (* -1 (+ y (floor hei 2))))))

;; 60   BOXER-FILLED-ELLIPSE        (X Y WIDTH HEIGHT)
(defun draw-boxer-filled-ellipse (com)
  (let ((x (aref com 1)) (y (aref com 2)) (w (aref com 3)) (h (aref com 4)))
    (draw-ellipse x ;(- x (floor w 2))
                  y ;(* -1 (+ y (floor h 2)))
                  w h t)))

;; 61   BOXER-ELLIPSE               (X Y WIDTH HEIGHT)
(defun draw-boxer-ellipse (com)
  (let ((x (aref com 1)) (y (aref com 2)) (w (aref com 3)) (h (aref com 4)))
    (draw-ellipse x ;(- x (floor w 2))
                  y ;(* -1 (+ y (floor h 2)))
                  w h nil)))

;; 62   BOXER-FILLED-CIRCLE        (X Y RADIUS)
(defun draw-boxer-filled-circle (com)
  (let ((x (aref com 1)) (y (aref com 2)) (radius (aref com 3)))
    (draw-circle x y radius t)))

;; 63   BOXER-CIRCLE               (X Y RADIUS)
(defun draw-boxer-circle (com)
  (let ((x (aref com 1)) (y (aref com 2)) (radius (aref com 3)))
    (draw-circle x y radius nil)))
