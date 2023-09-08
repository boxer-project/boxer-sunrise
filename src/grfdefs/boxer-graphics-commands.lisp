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

(defun apply-gdispl-com (fun vec)
  (apply fun (cdr (coerce vec 'list))))

(defun process-graphics-command (command)
  (let ((com (aref command 0)))
    (cond ((eq com 32)
           (apply-gdispl-com #'draw-boxer-change-alu command))
          ((eq com 33)
           (apply-gdispl-com #'draw-boxer-change-pen-width command))
          ((eq com 34)
           (apply-gdispl-com #'draw-boxer-change-graphics-font command))
          ((eq com 35)
           (apply-gdispl-com #'draw-boxer-line-segment command))
          ((eq com 36)
           (apply-gdispl-com #'draw-boxer-change-graphics-color command))
          ((eq com 37)
           (apply-gdispl-com #'draw-boxer-transform-matrix command)
          )
          ((eq com 39)
           (apply-gdispl-com #'draw-boxer-centered-string command))
          ((eq com 40)
           (apply-gdispl-com #'draw-boxer-left-string command))
          ((eq com 41)
           (apply-gdispl-com #'draw-boxer-right-string command))
          ((eq com 42)
           (apply-gdispl-com #'draw-boxer-centered-rectangle command))
          ((eq com 43)
           (apply-gdispl-com #'draw-boxer-dot command))
          ((eq com 47)
           (apply-gdispl-com #'draw-boxer-centered-bitmap command))
          ((eq com 58)
           (apply-gdispl-com #'draw-boxer-wedge command))
          ((eq com 59)
           (apply-gdispl-com #'draw-boxer-arc command))
          ((eq com 60)
           (apply-gdispl-com #'draw-boxer-filled-ellipse command))
          ((eq com 61)
           (apply-gdispl-com #'draw-boxer-ellipse command))
          ((eq com 62)
           (apply-gdispl-com #'draw-boxer-filled-circle command))
          ((eq com 63)
           (apply-gdispl-com #'draw-boxer-circle command))
          (t
          (break "Unhandled boxer graphics playback: ~A" command)))))

(defun boxer-playback-graphics-list (gl &key (start 0) (graphics-canvas nil) (translate? nil))
  "In progress work to move all the boxer graphics to turtle coordinates. Will get trued
   up with the defboxer-graphics-handler macros soon.

  If translate? is t, we will move the origin according to the values of %drawing-half-width and
  %drawing-half-height. The default is nil, as some usages of this will want to perform their own
  translation, scaling, rotation, etc."
  (let ((prev-model nil)
        (trans-mat nil))

    (with-graphics-state (gl t)
      (when translate?
        (setf prev-model (boxgl-device-model-matrix bw::*boxgl-device*)
              trans-mat  (3d-matrices:mtranslation
                          (3d-vectors:vec %drawing-half-width %drawing-half-height 0.0)))

        (setf (boxgl-device-model-matrix bw::*boxgl-device*) (3d-matrices:marr4 trans-mat))
        (update-matrices-ubo bw::*boxgl-device*))

      (when (and *use-opengl-framebuffers* graphics-canvas)
        (when (graphics-canvas-pen-color-cmd graphics-canvas)
          (process-graphics-command (graphics-canvas-pen-color-cmd graphics-canvas)))
        (when (graphics-canvas-pen-size-cmd graphics-canvas)
          (process-graphics-command (graphics-canvas-pen-size-cmd graphics-canvas)))
        (when (graphics-canvas-pen-font-cmd graphics-canvas)
          (process-graphics-command (graphics-canvas-pen-font-cmd graphics-canvas))))

      (do-vector-contents (command gl :start start)
        (process-graphics-command command)
        (when (and *use-opengl-framebuffers* graphics-canvas)
          (cond ((member (aref command 0) '(4 36))
                (setf (graphics-canvas-pen-color-cmd graphics-canvas) command))
                ((member (aref command 0) '(1 33))
                (setf (graphics-canvas-pen-size-cmd graphics-canvas) command))
                ((member (aref command 0) '(2 34))
                (setf (graphics-canvas-pen-font-cmd graphics-canvas) command)))))
      (when translate?
        (setf (boxgl-device-model-matrix bw::*boxgl-device*) prev-model)
        (update-matrices-ubo bw::*boxgl-device*)))))

;; 32   BOXER-CHANGE-ALU                             (NEW-ALU)
(defun draw-boxer-change-alu (new-alu)
  (setq *graphics-state-current-alu* new-alu))

;; 33   BOXER-CHANGE-PEN-WIDTH              (NEW-WIDTH)

(defun draw-boxer-change-pen-width (new-width)
  (%set-pen-size new-width))

;; 34   BOXER-CHANGE-GRAPHICS-FONT          (NEW-FONT-NO)
(defun draw-boxer-change-graphics-font (new-font-no)
  (setq *graphics-state-current-font-no* new-font-no))

;; 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
(defun draw-boxer-line-segment (x0 y0 x1 y1)
  "Takes a boxer command starting with 35:
    ex #(35 -0.5 -0.5 0.0 0.5)"
  ;; The y-axis needs to be flipped
  (ck-mode-draw-line x0 (- y0) x1 (- y1)
                     *graphics-state-current-alu*))

;; 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
(defun draw-boxer-change-graphics-color (new-color)
  (setf *graphics-state-current-pen-color* new-color)
  (%set-pen-color new-color))

;; 37   BOXER-TRANSFORM-MATRIX                       (. . .)

(defun draw-boxer-transform-matrix (matrix)
  (setf (boxgl-device-model-matrix bw::*boxgl-device*) matrix)
  (update-matrices-ubo bw::*boxgl-device*))

;; 39   BOXER-CENTERED-STRING             (X Y STRING)
(defun draw-boxer-centered-string (x y text)
  (let ((y (- y)))
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
(defun draw-boxer-left-string (x y text)
  (let ((y (- y)))
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
(defun draw-boxer-right-string (x y text)
  (let ((y (- y))
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
(defun draw-boxer-centered-rectangle (x y w h)
    (draw-rectangle w h
                   (- x (/ w 2))
                   ;; The y axis is flipped
                   (- (+ y (/ h 2)))))

;; 43   BOXER-DOT                      (X Y)
(defun draw-boxer-dot (x y)
    (draw-rectangle *graphics-state-current-pen-width* *graphics-state-current-pen-width*
                 (- x (/ *graphics-state-current-pen-width* 2))
                 (- (+ y (/ *graphics-state-current-pen-width* 2)))))

;; 47   BOXER-CENTERED-BITMAP          (BITMAP X Y WIDTH HEIGHT)
(defun draw-boxer-centered-bitmap (bitmap x y wid hei)
    (bitblt-to-screen wid hei bitmap 0 0
                      (- x (floor wid 2))
                      (- (+ y (floor hei 2)))))

;; 58   BOXER-WEDGE                    (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defun draw-boxer-wedge (x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle t))))

;; 59   BOXER-ARC                      (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defun draw-boxer-arc (x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle nil))))

;; 60   BOXER-FILLED-ELLIPSE        (X Y WIDTH HEIGHT)
(defun draw-boxer-filled-ellipse (x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h t)))

;; 61   BOXER-ELLIPSE               (X Y WIDTH HEIGHT)
(defun draw-boxer-ellipse (x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h nil)))

;; 62   BOXER-FILLED-CIRCLE        (X Y RADIUS)
(defun draw-boxer-filled-circle (x y radius)
  (let ((y (- y)))
    (draw-circle x y radius t)))

;; 63   BOXER-CIRCLE               (X Y RADIUS)
(defun draw-boxer-circle (x y radius)
  (let ((y (- y)))
    (draw-circle x y radius nil)))
