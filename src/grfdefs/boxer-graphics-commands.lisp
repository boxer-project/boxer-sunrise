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
        ;; Sometimes there may still be old window graphics command lurking around, like in
        ;; the :cached-boxtop entry in a boxes plist
        (setf command (allocate-window->boxer-command command))

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

(defclass graphics-command ()
  ((name :initform nil)
   (command-args :initform nil)
   (transformation-template :initform nil)))

(defmethod draw-gc ((self graphics-command) com)
  (error "No draw method defined for this command"))

(defmethod copy ((self graphics-command) com)
  (copy-seq com))

(defmethod extents ((self graphics-command) com)
  (values 0 0 0 0))

(defmethod load-gc ((self graphics-command) com)
  nil)

(defmethod dump-gc ((self graphics-command) command stream)
  (dump-boxer-thing command stream))

(defmethod deallocate-gc ((self graphics-command) command)
  nil)

(defmethod sprite-gc ((self graphics-command) command)
  '())

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

(defmacro defsprite-graphics-command ((gc-class &rest method-args) &body body)
  `(defmethod sprite-gc ((self ,gc-class) com)
     (let ,(loop for i
                 from 0
                 to (1- (length method-args))
                 collect (list (nth i method-args) (list 'nth i 'com)))
       . ,body)))

(defmacro defrecord-graphics-command (gc-class &rest method-args)
  ;; TODO sgithens 2023-09-29 Unfinished, currently these are manually defined throughout the
  ;; file.
  ; (let* ((gc (make-instance gc-class))
  ;        (opcode (slot-value gc 'opcode))
  ;        (record-name (intern (format nil "record-graphics-command-~A" (slot-value gc 'name))))
  ;        (args (loop for i
  ;                   from 0
  ;                   to (1- (length method-args))
  ;                   collect (nth i method-args))))
  ;   `(defun ,record-name ,args
  ;      (sv-append %graphics-list (coerce (list ,opcode ,@args) 'vector))))
  )

;; 32   BOXER-CHANGE-ALU                             (NEW-ALU)
(defclass boxer-change-alu (graphics-command)
  ((opcode :initform 32)
   (name :initform "boxer-change-alu")
   (command-args :initform nil)
   (transformation-template :initform nil)))

(defdraw-graphics-command (boxer-change-alu new-alu)
  (setq *graphics-state-current-alu* new-alu))

(defmethod load-gc ((self boxer-change-alu) command)
  (setf (aref command 1) (reallocate-file-alu (aref command 1))))

(defmethod dump-gc ((self boxer-change-alu) command stream)
  (let ((existing-alu (aref command 1)))
    (unwind-protect
    (progn (setf (aref command 1) (canonicalize-file-alu existing-alu))
            (dump-boxer-thing command stream))
    (setf (aref command 1) existing-alu))))

(defsprite-graphics-command (boxer-change-alu new-alu)
  (list (case new-alu
          (#.alu-xor 'bu::penreverse)
          ((#.alu-ior #.alu-seta) 'bu::pendown)
          ((#.alu-andca #.alu-setz) 'bu::penerase)
          (t (warn "Untranslatable alu ~A, assuming PENDOWN" new-alu)
            'bu::pendown))))

(defun record-boxer-graphics-command-change-alu (new-alu)
  (sv-append %graphics-list (coerce (list 32 new-alu) 'vector)))

;; 33   BOXER-CHANGE-PEN-WIDTH              (NEW-WIDTH)
(defclass boxer-change-pen-width (graphics-command)
  ((opcode :initform 33)
   (name :initform "boxer-change-pen-width")
   (command-args :initform nil)
   (transformation-template :initform nil)))

(defdraw-graphics-command (boxer-change-pen-width new-width)
  (%set-pen-size new-width))

(defextents-graphics-command (boxer-change-pen-width new-width)
  (setq *graphics-state-current-pen-width* new-width)
  ;; need to update because other graphics command
  ;; extent forms rely on an accurate value for pen-width
  (values 0 0 0 0 t))

(defsprite-graphics-command (boxer-change-pen-width new-width)
  (list 'bu::set-pen-width new-width))

(defun record-boxer-graphics-command-change-pen-width (new-width)
  (sv-append %graphics-list (coerce (list 33 new-width) 'vector)))

;; 34   BOXER-CHANGE-GRAPHICS-FONT          (NEW-FONT-NO)
(defclass boxer-change-graphics-font (graphics-command)
  ((opcode :initform 34)
   (name :initform "boxer-change-graphics-font")
   (command-args :initform nil)
   (transformation-template :initform nil)))

(defdraw-graphics-command (boxer-change-graphics-font new-font-no)
  (setq *graphics-state-current-font-no* new-font-no))

(defextents-graphics-command (boxer-change-graphics-font new-font-no)
  (setq *graphics-state-current-font-no* new-font-no)
  ;; need to update because other graphics command
  ;; extent forms rely on an accurate value for pen-width
  (values 0 0 0 0 t))

(defsprite-graphics-command (boxer-change-graphics-font new-font-no)
  (list 'bu::set-type-font new-font-no))

(defmethod load-gc ((self boxer-change-graphics-font) command)
  (when (>=& *version-number* 12)
    (setf (svref& command 1)
    (make-font-from-file-value (svref& command 1)))))

(defmethod dump-gc ((self boxer-change-graphics-font) command stream)
  (cond ((>=& *version-number* 12)
         ;; guts of a dump-array
         (enter-table 'fake-array)
         (write-file-word bin-op-initialize-and-return-array stream)
         (dump-array-1 stream 2 nil) (dump-boxer-thing 2 stream)
         (dump-boxer-thing (svref& command 0) stream)
         (dump-font (svref& command 1) stream))
    (t (dump-boxer-thing command stream))))

(defun record-boxer-graphics-command-change-graphics-font (new-font-no)
  (sv-append %graphics-list (coerce (list 34 new-font-no) 'vector)))

;; 35   BOXER-LINE-SEGMENT                           (X0 Y0 X1 Y1)
(defclass boxer-line-segment (graphics-command)
  ((opcode :initform 35)
   (name :initform "boxer-line-segment")
   (command-args :initform '(x0 y0 x1 y1))
   (transformation-template :initform '(:x-transform :y-transform :x-transform :y-transform))))

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

(defsprite-graphics-command (boxer-line-segment x0 y0 x1 y1)
  (cond ((and (= x0 last-x) (= y0 last-y))
        (setq last-x x1 last-y y1)
        (list 'bu::setxy x1 y1))
    (t
    (setq last-x x1 last-y y1)
    (append (sprite-commands-for-new-position x0 y0)
            (list 'bu::setxy x1 y1)))))

(defun record-boxer-graphics-command-line-segment (x0 y0 x1 y1)
  (sv-append %graphics-list (coerce (list 35 x0 y0 x1 y1) 'vector)))

;; 36   BOXER-CHANGE-GRAPHICS-COLOR                  (NEW-COLOR)
(defclass boxer-change-graphics-color (graphics-command)
  ((opcode :initform 36)
   (name :initform "boxer-change-graphics-color")
   (command-args :initform nil)
   (transformation-template :initform nil)))

(defdraw-graphics-command (boxer-change-graphics-color new-color)
  (setf *graphics-state-current-pen-color* new-color)
  (%set-pen-color new-color))

(defmethod load-gc ((self boxer-change-graphics-color) command)
  (setf (aref command 1)
        (reallocate-pixel-color (aref command 1))))

(defmethod dump-gc ((self boxer-change-graphics-color) command stream)
  (let* ((outgoing-command (copy-seq command))
         (existing-pixel (aref outgoing-command 1)))
    (unless (or (typep existing-pixel 'fixnum)
                (listp existing-pixel))
             ;; If this is a float to an openGL float vector, convert it
             ;; to an integer representation of the color
            (setf (aref outgoing-command 1)
              (if (>= *version-number* 12)
                  (pixel-dump-value existing-pixel)
                  (canonicalize-pixel-color existing-pixel))))
    (dump-boxer-thing outgoing-command stream)))

(defsprite-graphics-command (boxer-change-graphics-color new-color)
  (list 'bu::set-pen-color new-color))

(defun record-boxer-graphics-command-change-graphics-color (new-color)
  (sv-append %graphics-list (coerce (list 36 new-color) 'vector)))

;; 37   BOXER-TRANSFORM-MATRIX                       (. . .)
(defclass boxer-transform-matrix (graphics-command)
  ((opcode :initform 37)
   (name :initform "boxer-transform-matrix")
   (command-args :initform nil)
   (transformation-template :initform nil)))

(defdraw-graphics-command (boxer-transform-matrix matrix)
  (setf (boxgl-device-model-matrix bw::*boxgl-device*) matrix)
  (update-matrices-ubo bw::*boxgl-device*))

;; 39   BOXER-CENTERED-STRING             (X Y STRING)
(defclass boxer-centered-string (graphics-command)
  ((opcode :initform 39)
   (name :initform "boxer-centered-string")
   (command-args :initform '(x y string))
   (transformation-template :initform '(:x-transform :y-transform nil))))

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

(defsprite-graphics-command (boxer-centered-string x y text)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::type (make-box (list (list (coerce string
                                                      'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::type
                  (make-box (list (list (coerce string
                                                'simple-string)))))))))

(defun record-boxer-graphics-command-centered-string (x y text)
  (sv-append %graphics-list (coerce (list 39 x y text) 'vector)))

;;;; 40   BOXER-LEFT-STRING          (X Y STRING)
(defclass boxer-left-string (graphics-command)
  ((opcode :initform 40)
   (name :initform "boxer-left-string")
   (command-args :initform '(x y string))
   (transformation-template :initform '(:x-transform :y-transform nil))))

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

(defsprite-graphics-command (boxer-left-string x y text)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::ltype (make-box (list (list (coerce string
                                                        'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::ltype
                  (make-box (list (list (coerce string
                                                'simple-string)))))))))

(defun record-boxer-graphics-command-left-string (x y text)
  (sv-append %graphics-list (coerce (list 40 x y text) 'vector)))

;;;; 41   BOXER-RIGHT-STRING         (X Y STRING)
(defclass boxer-right-string (graphics-command)
  ((opcode :initform 41)
   (name :initform "boxer-right-string")
   (command-args :initform '(x y string))
   (transformation-template :initform '(:x-transform :y-transform nil))))

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

(defsprite-graphics-command (boxer-right-string x y text)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::rtype (make-box (list (list (coerce string
                                                        'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::rtype
                  (make-box (list (list (coerce string
                                                'simple-string)))))))))

(defun record-boxer-graphics-command-right-string (x y text)
  (sv-append %graphics-list (coerce (list 41 x y text) 'vector)))

;; 42   BOXER-CENTERED-RECTANGLE       (X Y WIDTH HEIGHT)
(defclass boxer-centered-rectangle (graphics-command)
  ((opcode :initform 42)
   (name :initform "boxer-centered-rectangle")
   (command-args :initform '(x y width height))
   (transformation-template :initform '(:x-transform :y-transform :coerce :coerce))))

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

(defsprite-graphics-command (boxer-centered-rectangle x y width height)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-rect width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-rect width height)))))

(defun record-boxer-graphics-command-centered-rectangle (x y width height)
  (sv-append %graphics-list (coerce (list 42 x y width height) 'vector)))

;; 43   BOXER-DOT                      (X Y)
(defclass boxer-dot (graphics-command)
  ((opcode :initform 43)
   (name :initform "boxer-dot")
   (command-args :initform '(x y))
   (transformation-template :initform '(:x-transform :y-transform))))

(defdraw-graphics-command (boxer-dot x y)
    (draw-rectangle *graphics-state-current-pen-width* *graphics-state-current-pen-width*
                 (- x (/ *graphics-state-current-pen-width* 2))
                 (- (+ y (/ *graphics-state-current-pen-width* 2)))))

(defextents-graphics-command (boxer-dot x y)
  (let ((half-size (/ *graphics-state-current-pen-width* 2.0)))
    (declare (type boxer-float half-size))
    (values (float-minus x half-size) (float-minus y half-size)
            (float-plus x half-size) (float-plus y half-size))))

(defsprite-graphics-command (boxer-dot x y)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::dot))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::dot)))))

(defun record-boxer-graphics-command-dot (x y)
  (sv-append %graphics-list (coerce (list 43 x y) 'vector)))

;; 44 TODO Hollow Rectangle??

  ; :SPRITE-COMMAND
  ; (cond ((and (= x last-x) (= y last-y))
  ;       (list 'bu::stamp-hollow-rect width height))
  ;   (t
  ;   (setq last-x x last-y y)
  ;   (append (sprite-commands-for-new-position x y)
  ;           (list 'bu::stamp-hollow-rect width height))))


;; 47   BOXER-CENTERED-BITMAP          (BITMAP X Y WIDTH HEIGHT)
(defclass boxer-centered-bitmap (graphics-command)
  ((opcode :initform 47)
   (name :initform "boxer-centered-bitmap")
   (command-args :initform '(bitmap x y width height))
   (transformation-template :initform '(nil :x-transform :y-transform :coerce :coerce))))

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

(defmethod dump-gc ((self boxer-centered-bitmap) command stream)
  (with-graphics-command-slots-bound command (bitmap x y width height)
    (progn
    ;; faking a dump of a simple array (which is the command)
    (enter-table command) ; we need to do this so load table index is correct
    (write-file-word bin-op-initialize-and-return-array stream)
    (multiple-value-bind (dims opts) (decode-array command)
                          (dump-array-1 stream dims opts))
    (dump-boxer-thing (length command) stream)
    ;; now the contents
    (dump-boxer-thing (svref& command 0) stream) ; opcode
    (dump-pixmap bitmap stream)
    (dump-boxer-thing x stream)
    (dump-boxer-thing y stream)
    (dump-boxer-thing width stream)
    (dump-boxer-thing height stream))))

(defmethod deallocate-gc ((self boxer-centered-bitmap) command)
  (ogl-free-pixmap (aref command 1)))

(defun record-boxer-graphics-command-centered-bitmap (bitmap x y width height)
  (sv-append %graphics-list (coerce (list 47 bitmap x y width height) 'vector)))

;; 58   BOXER-WEDGE                    (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defclass boxer-wedge (graphics-command)
  ((opcode :initform 58)
   (name :initform "boxer-wedge")
   (command-args :initform '(x y radius start-angle sweep-angle))
   (transformation-template :initform '(:x-transform :y-transform nil nil nil))))

(defdraw-graphics-command (boxer-wedge x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle t))))

(defextents-graphics-command (boxer-wedge x y radius start-angle sweep-angle)
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius)))

(defsprite-graphics-command (boxer-wedge x y radius start-angle sweep-angle)
  ; !!!what abut synching HEADING ???
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-wedge radius sweep-angle))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-wedge radius sweep-angle)))))

(defun record-boxer-graphics-command-wedge (x y radius start-angle sweep-angle)
  (sv-append %graphics-list (coerce (list 58 x y radius start-angle sweep-angle) 'vector)))


;; 59   BOXER-ARC                      (X Y RADIUS START-ANGLE SWEEP-ANGLE)
(defclass boxer-arc (graphics-command)
  ((opcode :initform 59)
   (name :initform "boxer-arc")
   (command-args :initform '(x y radius start-angle sweep-angle))
   (transformation-template :initform '(:x-transform :y-transform nil nil nil))))

(defdraw-graphics-command (boxer-arc x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (%draw-c-arc x y radius start-angle sweep-angle nil))))

(defextents-graphics-command (boxer-arc x y radius start-angle sweep-angle)
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius)))

(defsprite-graphics-command (boxer-arc x y radius start-angle sweep-angle)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-arc radius sweep-angle))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-arc radius sweep-angle)))))

(defun record-boxer-graphics-command-arc (x y radius start-angle sweep-angle)
  (sv-append %graphics-list (coerce (list 59 x y radius start-angle sweep-angle) 'vector)))

;; 60   BOXER-FILLED-ELLIPSE        (X Y WIDTH HEIGHT)
(defclass boxer-filled-ellipse (graphics-command)
  ((opcode :initform 60)
   (name :initform "boxer-filled-ellipse")
   (command-args :initform '(x y width height))
   (transformation-template :initform '(:x-transform :y-transform :coerce :coerce))))

(defdraw-graphics-command (boxer-filled-ellipse x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h t)))

(defextents-graphics-command (boxer-filled-ellipse x y width height)
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height))))

(defsprite-graphics-command (boxer-filled-ellipse x y w h)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-ellipse width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-ellipse width height)))))

(defun record-boxer-graphics-command-filled-ellipse (x y width height)
  (sv-append %graphics-list (coerce (list 60 x y width height) 'vector)))

;; 61   BOXER-ELLIPSE               (X Y WIDTH HEIGHT)
(defclass boxer-ellipse (graphics-command)
  ((opcode :initform 61)
   (name :initform "boxer-ellipse")
   (command-args :initform '(x y width height))
   (transformation-template :initform '(:x-transform :y-transform :coerce :coerce))))

(defdraw-graphics-command (boxer-ellipse x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h nil)))

(defextents-graphics-command (boxer-ellipse x y width height)
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height))))

(defsprite-graphics-command (boxer-ellipse x y width height)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-ellipse width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-ellipse width height)))))

(defun record-boxer-graphics-command-ellipse (x y width height)
  (sv-append %graphics-list (coerce (list 61 x y width height) 'vector)))

;; 62   BOXER-FILLED-CIRCLE        (X Y RADIUS)
(defclass boxer-filled-circle (graphics-command)
  ((opcode :initform 62)
   (name :initform "boxer-filled-circle")
   (command-args :initform '(x y radius))
   (transformation-template :initform '(:x-transform :y-transform nil))))

(defdraw-graphics-command (boxer-filled-circle x y radius)
  (let ((y (- y)))
    (draw-circle x y radius t)))

(defextents-graphics-command (boxer-filled-circle x y radius)
  (values (- x radius) (- y radius) (+ x radius) (+ y radius)))

(defsprite-graphics-command (boxer-filled-circle x y radius)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-circle radius))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-circle radius)))))

(defun record-boxer-graphics-command-filled-circle (x y radius)
  (sv-append %graphics-list (coerce (list 62 x y radius) 'vector)))

;; 63   BOXER-CIRCLE               (X Y RADIUS)
(defclass boxer-circle (graphics-command)
  ((opcode :initform 63)
   (name :initform "boxer-circle")
   (command-args :initform '(x y radius))
   (transformation-template :initform '(:x-transform :y-transform nil))))

(defdraw-graphics-command (boxer-circle x y radius)
  (let ((y (- y)))
    (draw-circle x y radius nil)))

(defextents-graphics-command (boxer-circle x y radius)
  (values (- x radius) (- y radius) (+ x radius) (+ y radius)))

(defsprite-graphics-command (boxer-circle x y radius)
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-circle radius))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-circle radius)))))

(defun record-boxer-graphics-command-circle (x y radius)
  (sv-append %graphics-list (coerce (list 63 x y radius) 'vector)))

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
