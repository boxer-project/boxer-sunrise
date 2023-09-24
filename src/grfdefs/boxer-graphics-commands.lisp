(in-package :boxer)

;; TODO!!! Hollow Rectangle!!

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

(defun apply-gdispl-com (gc vec)
  (apply #'draw-gc (cons gc (list (cdr (coerce vec 'list))))))

(defun process-graphics-command (command)
  (let* ((com (aref command 0))
         (handler (gethash com *graphics-commands*)))
    (apply-gdispl-com handler command)))

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

(defclass graphics-command () ())

(defmethod draw-gc ((self graphics-command) com)
  (error "No draw method defined for this command"))

(defmethod copy ((self graphics-command) com)
  (copy-seq com))

(defmethod extents ((self graphics-command) com)
  (values 0 0 0 0))

(defmacro defdraw-graphics-command ((gc-class &rest method-args) &body body)
  `(defmethod draw-gc ((self ,gc-class) com)
     (let ,(loop for i
                 from 0
                 to (1- (length method-args))
                 collect (list (nth i method-args) (list 'nth i 'com)))
       . ,body)))

(defmacro defextents-graphics-command ((gc-class &rest method-args) &body body)
  `(defmethod extents ((self ,gc-class) com)
     (let ,(loop for i
                 from 0
                 to (1- (length method-args))
                 collect (list (nth i method-args) (list 'nth i 'com)))
       . ,body)))

;; 32   BOXER-CHANGE-ALU                             (NEW-ALU)
(defclass boxer-change-alu (graphics-command)
  ())

(defdraw-graphics-command (boxer-change-alu new-alu)
  (setq *graphics-state-current-alu* new-alu))

;; 33   BOXER-CHANGE-PEN-WIDTH              (NEW-WIDTH)
(defclass boxer-change-pen-width (graphics-command)
  ())

(defdraw-graphics-command (boxer-change-pen-width new-width)
  (%set-pen-size new-width))

(defextents-graphics-command (boxer-change-pen-width new-width)
  (setq *graphics-state-current-pen-width* new-width)
  ;; need to update because other graphics command
  ;; extent forms rely on an accurate value for pen-width
  (values 0 0 0 0 t))

;; 34   BOXER-CHANGE-GRAPHICS-FONT          (NEW-FONT-NO)
(defclass boxer-change-graphics-font (graphics-command)
  ())

(defdraw-graphics-command (boxer-change-graphics-font new-font-no)
  (setq *graphics-state-current-font-no* new-font-no))

(defextents-graphics-command (boxer-change-graphics-font new-font-no)
  (setq *graphics-state-current-font-no* new-font-no)
  ;; need to update because other graphics command
  ;; extent forms rely on an accurate value for pen-width
  (values 0 0 0 0 t))

;; 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
(defclass boxer-line-segment (graphics-command)
  ())

(defdraw-graphics-command (boxer-line-segment x0 y0 x1 y1)
  "Takes a boxer command starting with 35:
    ex #(35 -0.5 -0.5 0.0 0.5)"
  ;; The y-axis needs to be flipped
  (ck-mode-draw-line x0 (- y0) x1 (- y1)
                     *graphics-state-current-alu*))

(defextents-graphics-command (boxer-line-segment x0 y0 x1 y1)
  (let ((delta #-mcl (ceiling *graphics-state-current-pen-width* 2)
               ;; this has to stay until the non centered thick line bug in
               ;; the mac implementation gets fixed
               #+mcl *graphics-state-current-pen-width*))
    (values (- (min x0 x1) delta) (- (min y0 y1) delta)
            (+ (max x0 x1) delta) (+ (max y0 y1) delta))))

;; 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
(defclass boxer-change-graphics-color (graphics-command)
  ())

(defdraw-graphics-command (boxer-change-graphics-color new-color)
  (setf *graphics-state-current-pen-color* new-color)
  (%set-pen-color new-color))

;; 37   BOXER-TRANSFORM-MATRIX                       (. . .)
(defclass boxer-transform-matrix (graphics-command)
  ())

(defdraw-graphics-command (boxer-transform-matrix matrix)
  (setf (boxgl-device-model-matrix bw::*boxgl-device*) matrix)
  (update-matrices-ubo bw::*boxgl-device*))

;; 39   BOXER-CENTERED-STRING             (X Y STRING)
(defclass boxer-centered-string (graphics-command)
  ())

(defdraw-graphics-command (boxer-centered-string x y text)
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

(defextents-graphics-command (boxer-centered-string x y text)
  (let ((height 0) (s text) (width 0) (wx x))
    (loop
      (setq height
            (+ height (1+(string-hei *graphics-state-current-font-no*)))
            width (max (string-wid *graphics-state-current-font-no*
                                  (subseq s 0 (position #\newline s)))
                      width)
            wx (min wx (- x (/ width 2))))
      ;; If we have handled the last line (the current line has no CR's)
      (if (not (position #\newline s))
        (return (values (coerce wx 'boxer-float) (coerce y 'boxer-float)
                        (coerce (+ wx width) 'boxer-float)
                        (coerce (+ y height) 'boxer-float)))
        (setq s (subseq s (let ((p (position #\newline s)))
                            (if (null p) 0 (1+& p)))))))))

;;;; 40   BOXER-LEFT-STRING          (X Y STRING)
(defclass boxer-left-string (graphics-command)
  ())

(defdraw-graphics-command (boxer-left-string x y text)
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
(defclass boxer-right-string (graphics-command)
  ())

(defdraw-graphics-command (boxer-right-string x y text)
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
(defclass boxer-centered-rectangle (graphics-command)
  ())

(defdraw-graphics-command (boxer-centered-rectangle x y w h)
    (draw-rectangle w h
                   (- x (/ w 2))
                   ;; The y axis is flipped
                   (- (+ y (/ h 2)))))

(defextents-graphics-command (boxer-centered-rectangle x y width height)
  (let ((half-width (values (/ width 2.0)))
        (half-height (values (/ height 2.0))))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height))))

;; 43   BOXER-DOT                      (X Y)
(defclass boxer-dot (graphics-command)
  ())

(defdraw-graphics-command (boxer-dot x y)
    (draw-rectangle *graphics-state-current-pen-width* *graphics-state-current-pen-width*
                 (- x (/ *graphics-state-current-pen-width* 2))
                 (- (+ y (/ *graphics-state-current-pen-width* 2)))))

(defextents-graphics-command (boxer-dot x y)
  (let ((half-size (/ *graphics-state-current-pen-width* 2.0)))
    (declare (type boxer-float half-size))
    (values (float-minus x half-size) (float-minus y half-size)
            (float-plus x half-size) (float-plus y half-size))))

;; 47   BOXER-CENTERED-BITMAP          (BITMAP X Y WIDTH HEIGHT)
(defclass boxer-centered-bitmap (graphics-command)
  ())

(defmethod copy ((self boxer-centered-bitmap) command)
  (let ((togo (copy-seq command)))
    (setf (aref togo 1) (copy-pixmap (aref command 1)))
    togo))

(defdraw-graphics-command (boxer-centered-bitmap bitmap x y wid hei)
    (bitblt-to-screen wid hei bitmap 0 0
                      (- x (floor wid 2))
                      (- (+ y (floor hei 2)))))

(defextents-graphics-command (boxer-centered-bitmap bitmap x y width height)
  (let ((half-width (/ width 2.0))
        (half-height (/ height 2.0)))
    (declare (type boxer-float half-width half-height))
    ;; removed float-plus/minus because x & y are not floats
    (values (- x half-width) (- y half-height)
            (+ x half-width) (+ y half-height))))

;; 58   BOXER-WEDGE                    (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defclass boxer-wedge (graphics-command)
  ())

(defdraw-graphics-command (boxer-wedge x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle t))))

(defextents-graphics-command (boxer-wedge x y radius start-angle sweep-angle)
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius)))

;; 59   BOXER-ARC                      (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defclass boxer-arc (graphics-command)
  ())

(defdraw-graphics-command (boxer-arc x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle nil))))

(defextents-graphics-command (boxer-arc x y radius start-angle sweep-angle)
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius)))

;; 60   BOXER-FILLED-ELLIPSE        (X Y WIDTH HEIGHT)
(defclass boxer-filled-ellipse (graphics-command)
  ())

(defdraw-graphics-command (boxer-filled-ellipse x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h t)))

(defextents-graphics-command (boxer-filled-ellipse x y width height)
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height))))

;; 61   BOXER-ELLIPSE               (X Y WIDTH HEIGHT)
(defclass boxer-ellipse (graphics-command)
  ())

(defdraw-graphics-command (boxer-ellipse x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h nil)))

(defextents-graphics-command (boxer-ellipse x y width height)
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height))))

;; 62   BOXER-FILLED-CIRCLE        (X Y RADIUS)
(defclass boxer-filled-circle (graphics-command)
  ())

(defdraw-graphics-command (boxer-filled-circle x y radius)
  (let ((y (- y)))
    (draw-circle x y radius t)))

(defextents-graphics-command (boxer-filled-circle x y radius)
  (values (- x radius) (- y radius) (+ x radius) (+ y radius)))

;; 63   BOXER-CIRCLE               (X Y RADIUS)
(defclass boxer-circle (graphics-command)
  ())

(defdraw-graphics-command (boxer-circle x y radius)
  (let ((y (- y)))
    (draw-circle x y radius nil)))

(defextents-graphics-command (boxer-circle x y radius)
  (values (- x radius) (- y radius) (+ x radius) (+ y radius)))

(defparameter *graphics-commands*
  (serapeum:dict 32 (make-instance 'boxer-change-alu)
                 33 (make-instance 'boxer-change-pen-width)
                 34 (make-instance 'boxer-change-graphics-font)
                 35 (make-instance 'boxer-line-segment)
                 36 (make-instance 'boxer-change-graphics-color)
                 37 (make-instance 'boxer-transform-matrix)
                 39 (make-instance 'boxer-centered-string)
                 40 (make-instance 'boxer-left-string)
                 41 (make-instance 'boxer-right-string)
                 42 (make-instance 'boxer-centered-rectangle)
                 43 (make-instance 'boxer-dot)
                 47 (make-instance 'boxer-centered-bitmap)
                 58 (make-instance 'boxer-wedge)
                 59 (make-instance 'boxer-arc)
                 60 (make-instance 'boxer-filled-ellipse)
                 61 (make-instance 'boxer-ellipse)
                 62 (make-instance 'boxer-filled-circle)
                 63 (make-instance 'boxer-circle)))
