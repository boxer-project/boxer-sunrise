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
;;;; 58   BOXER-WEDGE                                  (X Y RADIUS START-ANGLE SWEEP-ANGLE)
;;;; 59   BOXER-ARC                                    (X Y RADIUS START-ANGLE SWEEP-ANGLE)
;;;; 60   BOXER-FILLED-ELLIPSE                         (X Y WIDTH HEIGHT)
;;;; 61   BOXER-ELLIPSE                                (X Y WIDTH HEIGHT)
;;;; 62   BOXER-FILLED-CIRCLE                          (X Y RADIUS)
;;;; 63   BOXER-CIRCLE                                 (X Y RADIUS)


(defun boxer-playback-graphics-list (gl &key (start 0) (graphics-canvas nil) (translate? nil))
  "In progress work to move all the boxer graphics to turtle coordinates. Will get trued
   up with the defboxer-graphics-handler macros soon.

  If translate? is t, we will move the origin according to the values of %drawing-half-width and
  %drawing-half-height. The default is nil, as some usages of this will want to perform their own
  translation, scaling, rotation, etc."
  (let ((prev-model nil)
        (trans-mat nil))

    (when translate?
      (setf prev-model (boxgl-device-model-matrix bw::*boxgl-device*)
            trans-mat  (3d-matrices:mtranslation
                         (3d-vectors:vec %drawing-half-width %drawing-half-height 0.0)))

      (setf (boxgl-device-model-matrix bw::*boxgl-device*) (3d-matrices:marr4 trans-mat))
      (update-matrices-ubo bw::*boxgl-device*))

    (when (and *use-opengl-framebuffers* graphics-canvas)
      (when (graphics-canvas-pen-color-cmd graphics-canvas)
        (process-graphics-command-marker (graphics-canvas-pen-color-cmd graphics-canvas)))
      (when (graphics-canvas-pen-size-cmd graphics-canvas)
        (process-graphics-command-marker (graphics-canvas-pen-size-cmd graphics-canvas)))
      (when (graphics-canvas-pen-font-cmd graphics-canvas)
        (process-graphics-command-marker (graphics-canvas-pen-font-cmd graphics-canvas))))

    (with-graphics-state (gl t)
         (do-vector-contents (command gl :start start)
;                     (format t
; "~%matmatMAT: ~A
;      trns: ~A
;      com: ~A
;      gl: ~A" (boxgl-device-model-matrix bw::*boxgl-device*)
;              (boxgl-device-transform-matrix bw::*boxgl-device*) command gl)
           (let ((com (aref command 0)))
             (when (< com 32)
              ;  (format t "~%~%About to convert: ~A" command)
              ;; sgithens TODO is this actually updating the contents of the graphics list for next time and saving?
               (setf command (allocate-window->boxer-command command))
               (setf com (aref command 0))
              ;  (format t "~%converted       : ~A" command)
               )
             (cond ((eq com 32)
                    (draw-boxer-change-alu command))
                   ((eq com 33)
                    (draw-boxer-change-pen-width command))
                   ((eq com 34)
                    (draw-boxer-change-graphics-font command))
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
                   ((eq com 58)
                    (draw-boxer-wedge command))
                   ((eq com 59)
                    (draw-boxer-arc command))
                   ((eq com 60)
                    (draw-boxer-filled-ellipse command))
                   ((eq com 61)
                    (draw-boxer-ellipse command))
                   ((eq com 62)
                    (draw-boxer-filled-circle command))
                   ((eq com 63)
                    (draw-boxer-circle command))
                   (t
                    (break "Unhandled boxer graphics playback: ~A" command))))

             (when (and *use-opengl-framebuffers* graphics-canvas)
               (cond ((equal 4 (aref command 0))
                      (setf (graphics-canvas-pen-color-cmd graphics-canvas) command))
                     ((equal 1 (aref command 0))
                      (setf (graphics-canvas-pen-size-cmd graphics-canvas) command))
                     ((equal 2 (aref command 0))
                      (setf (graphics-canvas-pen-font-cmd graphics-canvas) command))))
            )
                   ;;  (process-graphics-command-marker command)
                   )
    (when translate?
      (setf (boxgl-device-model-matrix bw::*boxgl-device*) prev-model)
      (update-matrices-ubo bw::*boxgl-device*))))

;; 32   BOXER-CHANGE-ALU                             (NEW-ALU)
(defun draw-boxer-change-alu (com)
  (setq *graphics-state-current-alu* (aref com 1)))

;; 33   BOXER-CHANGE-PEN-WIDTH              (NEW-WIDTH)

(defun draw-boxer-change-pen-width (com)
  (%set-pen-size (aref com 1)))

;; 34   BOXER-CHANGE-GRAPHICS-FONT          (NEW-FONT-NO)
(defun draw-boxer-change-graphics-font (com)
  (setq *graphics-state-current-font-no* (aref com 1)))

;; 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
(defun draw-boxer-line-segment (com)
  "Takes a boxer command starting with 35:
    ex #(35 -0.5 -0.5 0.0 0.5)"
  ;; The y-axis needs to be flipped
  (ck-mode-draw-line (aref com 1) (* -1 (aref com 2)) (aref com 3) (* -1 (aref com 4))
                     *graphics-state-current-alu*))

;; 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
(defun draw-boxer-change-graphics-color (com)
  (%set-pen-color (aref com 1)))

;; 39   BOXER-CENTERED-STRING             (X Y STRING)
(defun draw-boxer-centered-string (com)
  (let* ((text (aref com 3))
         (x (aref com 1))
         (y (- (aref com 2))))
    (do* ((height (1+ (string-hei *graphics-state-current-font-no*)))
          (s text (subseq s (let ((p (position #\newline s)))
                              (if (null p) 0 (1+ p)))))
          (trimmed-string (subseq s 0 (position #\newline s))
                          (subseq s 0 (position #\newline s)))
          (width (string-wid *graphics-state-current-font-no* trimmed-string)
                 (string-wid *graphics-state-current-font-no* trimmed-string))
          (wx (- x (/ width 2)) (- x (/ width 2)))
          (wy y (+ wy height)))
      ((not (position #\newline s))
       (draw-string *graphics-state-current-font-no* trimmed-string wx wy))
      (draw-string *graphics-state-current-font-no* trimmed-string wx wy))))

;;;; 40   BOXER-LEFT-STRING          (X Y STRING)
(defun draw-boxer-left-string (com)
  (let* ((text (aref com 3))
         (x (aref com 1))
         (y (- (aref com 2))))
    (loop
      (draw-string *graphics-state-current-font-no*
                    (subseq text 0 (position #\newline text)) x y)
      ;; If we have drawn the last line (the current line has no CR's)
      (if (not (position #\newline text))
        (return)
        (setq text (subseq text (let ((p (position #\newline text)))
                            (if (null p) 0 (1+ p))))
              y (+ y 1 (string-hei *graphics-state-current-font-no*)))))))

;;;; 41   BOXER-RIGHT-STRING         (X Y STRING)
(defun draw-boxer-right-string (com)
  (let ((x (aref com 1))
        (y (- (aref com 2)))
        (text (aref com 3))
        (width 0)
        (trimmed-string ""))
    (loop
      (setq trimmed-string (subseq text 0 (position #\newline text))
            width (string-wid *graphics-state-current-font-no* trimmed-string))
      (draw-string *graphics-state-current-font-no* trimmed-string (- x width) y)
      ;; If we have drawn the last line (the current line has no CR's)
      (if (not (position #\newline text))
        (return)
        (setq text (subseq text (let ((p (position #\newline text)))
                                  (if (null p) 0 (1+ p))))
              y (+ y (1+ (string-hei *graphics-state-current-font-no*))))))))

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

;; 58   BOXER-WEDGE                    (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defun draw-boxer-wedge (com)
  (let ((x (aref com 1)) (y (- (aref com 2))) (radius (aref com 3)) (start-angle (aref com 4)) (sweep-angle (aref com 5)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle t))))

;; 59   BOXER-ARC                      (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defun draw-boxer-arc (com)
  (let ((x (aref com 1)) (y (- (aref com 2))) (radius (aref com 3)) (start-angle (aref com 4)) (sweep-angle (aref com 5)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle nil))))

;; 60   BOXER-FILLED-ELLIPSE        (X Y WIDTH HEIGHT)
(defun draw-boxer-filled-ellipse (com)
  (let ((x (aref com 1)) (y (- (aref com 2))) (w (aref com 3)) (h (aref com 4)))
    (draw-ellipse x ;(- x (floor w 2))
                  y ;(* -1 (+ y (floor h 2)))
                  w h t)))

;; 61   BOXER-ELLIPSE               (X Y WIDTH HEIGHT)
(defun draw-boxer-ellipse (com)
  (let ((x (aref com 1)) (y (- (aref com 2))) (w (aref com 3)) (h (aref com 4)))
    (draw-ellipse x ;(- x (floor w 2))
                  y ;(* -1 (+ y (floor h 2)))
                  w h nil)))

;; 62   BOXER-FILLED-CIRCLE        (X Y RADIUS)
(defun draw-boxer-filled-circle (com)
  (let ((x (aref com 1)) (y (- (aref com 2))) (radius (aref com 3)))
    (draw-circle x y radius t)))

;; 63   BOXER-CIRCLE               (X Y RADIUS)
(defun draw-boxer-circle (com)
  (let ((x (aref com 1)) (y (- (aref com 2))) (radius (aref com 3)))
    (draw-circle x y radius nil)))
