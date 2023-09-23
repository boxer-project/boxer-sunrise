;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those ;;;;  portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is ;;;;  retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with ;;;;  this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;        All of the graphics display list command declarations using defstandard-graphics-handlers.
;;;;
(in-package :boxer)

;;; 0 Change ALU

(defgraphics-state-change (change-alu 0) (new-alu)
  :dump-form
  (let ((existing-alu (svref& command 1)))
    (unwind-protect
    (progn (setf (svref& command 1) (canonicalize-file-alu existing-alu))
            (dump-boxer-thing command stream))
    (setf (svref& command 1) existing-alu)))
  :load-form
  (setf (svref& command 1) (reallocate-file-alu (svref& command 1)))
  :sprite-command
  (list (case new-alu
          (#.alu-xor 'bu::penreverse)
          ((#.alu-ior #.alu-seta) 'bu::pendown)
          ((#.alu-andca #.alu-setz) 'bu::penerase)
          (t (warn "Untranslatable alu ~A, assuming PENDOWN" new-alu)
            'bu::pendown)))
  :body
  (unless (=& new-alu *graphics-state-current-alu*)
    (setq *graphics-state-current-alu* new-alu)))

;;; 1 Change Pen Width

(defgraphics-state-change (change-pen-width 1) (new-width)
  :boxer-extents-form (progn (setq *graphics-state-current-pen-width* new-width)
                       ;; need to update because other graphics command
                       ;; extent forms rely on an accurate value for pen-width
                       (values 0 0 0 0 t))
  :sprite-command
  (list 'bu::set-pen-width new-width)
  :body
  (unless (=& new-width *graphics-state-current-pen-width*)
    (setq *graphics-state-current-pen-width* new-width)
    (%set-pen-size new-width)))

;;; 2 Change Graphics Font

(defgraphics-state-change (change-graphics-font 2) (new-font-no)
  :boxer-extents-form (progn (setq *graphics-state-current-font-no* new-font-no)
                      ;; need to update because other graphics command
                      ;; extent forms rely on an accurate value for pen-width
                      (values 0 0 0 0 t))
  :dump-form (cond ((>=& *version-number* 12)
                    ;; guts of a dump-array
                    (enter-table 'fake-array)
                    (write-file-word bin-op-initialize-and-return-array stream)
                    (dump-array-1 stream 2 nil) (dump-boxer-thing 2 stream)
                    (dump-boxer-thing (svref& command 0) stream)
                    (dump-font (svref& command 1) stream))
              (t (dump-boxer-thing command stream)))
  :load-form (when (>=& *version-number* 12)
              (setf (svref& command 1)
                    (make-font-from-file-value (svref& command 1))))
  :sprite-command
  (list 'bu::set-type-font new-font-no)
  :body
  (unless (=& new-font-no *graphics-state-current-font-no*)
    ;; have to check for possible font
    (setq *graphics-state-current-font-no* new-font-no)))

;;; 3 Line Segment

(defstandard-graphics-handlers (line-segment 3)
  :COMMAND-ARGS (x0 y0 x1 y1)
  :BOXER-EXTENTS-FORM
  (let ((delta #-mcl (ceiling *graphics-state-current-pen-width* 2)
              ;; this has to stay until the non centered thick line bug in
              ;; the mac implementation gets fixed
              #+mcl *graphics-state-current-pen-width*))
    (values (- (min x0 x1) delta) (- (min y0 y1) delta)
            (+ (max x0 x1) delta) (+ (max y0 y1) delta)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :x-transform :y-transform)
  :sprite-command
  (cond ((and (= x0 last-x) (= y0 last-y))
        (setq last-x x1 last-y y1)
        (list 'bu::setxy x1 y1))
    (t
    (setq last-x x1 last-y y1)
    (append (sprite-commands-for-new-position x0 y0)
            (list 'bu::setxy x1 y1)))))

;;; 4 Change Graphics Color

(defgraphics-state-change (change-graphics-color 4) (new-color)
             :dump-form
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
                (dump-boxer-thing outgoing-command stream))
             :load-form
             (setf (aref command 1)
                   (reallocate-pixel-color (aref command 1)))
             :sprite-command
             (list 'bu::set-pen-color new-color)
             :body
             (unless (color= new-color *graphics-state-current-pen-color*)
               (setq *graphics-state-current-pen-color* new-color)
               (%set-pen-color new-color)))

(defgraphics-state-change (transform-matrix 5) (matrix)
             :body
             nil)

;;; 7 Centered String

(defstandard-graphics-handlers (centered-string 7)
  :COMMAND-ARGS (x y string)
  :BOXER-EXTENTS-FORM
  (let ((height 0) (s string) (width 0) (wx x))
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
                            (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::type (make-box (list (list (coerce string
                                                      'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::type
                  (make-box (list (list (coerce string
                                                'simple-string)))))))))

;;; 8 Left String

(defstandard-graphics-handlers (left-string 8)
  :COMMAND-ARGS (x y string)
  ; :EXTENTS-FORM
  ; (let ((height 0) (s string) (width 0))
  ;   (loop
  ;     (setq height (+ height 1(string-hei *graphics-state-current-font-no*))
  ;           width (max (string-wid *graphics-state-current-font-no*
  ;                                 (subseq s 0 (position #\newline s)))
  ;                     width))
  ;     ;; If we have handled the last line (the current line has no CR's)
  ;     (if (not (position #\newline s))
  ;       (return (values x y (+ x width) (+ y height)))
  ;       (setq s (subseq s (let ((p (position #\newline s)))
  ;                           (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::ltype (make-box (list (list (coerce string
                                                        'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::ltype
                  (make-box (list (list (coerce string
                                                'simple-string)))))))))

;;; 9 Right String

(defstandard-graphics-handlers (right-string 9)
  :COMMAND-ARGS (x y string)
  ; :EXTENTS-FORM
  ; (let ((height 0) (s string) (width 0))
  ;   (loop
  ;     (setq height (+ height 1 (string-hei *graphics-state-current-font-no*))
  ;           width (max (string-wid *graphics-state-current-font-no*
  ;                                 (subseq s 0 (position #\newline s)))
  ;                     width))
  ;     ;; If we have handled the last line (the current line has no CR's)
  ;     (if (not (position #\newline s))
  ;       (return (values (- x width) y x (+ y height)))
  ;       (setq s (subseq s (let ((p (position #\newline s)))
  ;                           (if (null p) 0 (1+& p))))))))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::rtype (make-box (list (list (coerce string
                                                        'simple-string))))))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::rtype
                  (make-box (list (list (coerce string
                                                'simple-string)))))))))

;;; 10 Centered Rectangle

(defstandard-graphics-handlers (centered-rectangle 10)
  :COMMAND-ARGS (x y width height)
  :BOXER-EXTENTS-FORM
  (let ((half-width (values (/ width 2.0)))
        (half-height (values (/ height 2.0))))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-rect width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-rect width height)))))

;;; 11 Dot

;; maybe this should be a circle ? Need to check relative speeds
;; also, should probably use the pen-width ?
(defstandard-graphics-handlers (dot 11)
  :COMMAND-ARGS (x y)
  :BOXER-EXTENTS-FORM
  (let ((half-size (/ *graphics-state-current-pen-width* 2.0)))
    (declare (type boxer-float half-size))
    (values (float-minus x half-size) (float-minus y half-size)
            (float-plus x half-size) (float-plus y half-size)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::dot))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::dot)))))

;;; 12 Hollow Rectangle

(defstandard-graphics-handlers (hollow-rectangle 12)
  :COMMAND-ARGS (x y width height)
  :BOXER-EXTENTS-FORM
  (let ((half-width (values (/ width 2.0)))
        (half-height (values (/ height 2.0))))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-rect width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-rect width height)))))

;;; 15 Centered Bitmap

(defstandard-graphics-handlers (centered-bitmap 15)
  :COMMAND-ARGS (bitmap x y width height)
  :BOXER-EXTENTS-FORM
  (let ((half-width (/ width 2.0))
        (half-height (/ height 2.0)))
    (declare (type boxer-float half-width half-height))
    ;; removed float-plus/minus because x & y are not floats
    (values (- x half-width) (- y half-height)
            (+ x half-width) (+ y half-height)))
  :TRANSFORMATION-TEMPLATE
  (nil :x-transform :y-transform :coerce :coerce)
  :deallocate-args (graphics-command)
  :deallocate-form (ogl-free-pixmap bitmap)
  :DUMP-FORM ;; need special handling for the bitmap...
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

;;; 26 Wedge

(defstandard-graphics-handlers (wedge 26)
  :COMMAND-ARGS (x y radius start-angle sweep-angle)
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil nil nil)
  :SPRITE-COMMAND ; !!!what abut synching HEADING ???
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-wedge radius sweep-angle))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-wedge radius sweep-angle)))))

;;; 27 Arc

(defstandard-graphics-handlers (arc 27)
  :COMMAND-ARGS (x y radius start-angle sweep-angle)
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius)
          (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil nil nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-arc radius sweep-angle))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-arc radius sweep-angle)))))

;;; 28 Filled Ellipse

(defstandard-graphics-handlers (filled-ellipse 28)
  :COMMAND-ARGS (x y width height)
  :BOXER-EXTENTS-FORM
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-ellipse width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-ellipse width height)))))


;;; 29 Ellipse

(defstandard-graphics-handlers (ellipse 29)
  :COMMAND-ARGS (x y width height)
  :BOXER-EXTENTS-FORM
  (let ((half-width (/ width 2.0))
        (half-height (/  height 2.0)))
    (declare (type boxer-float half-width half-height))
    (values (float-minus x half-width) (float-minus y half-height)
            (float-plus x half-width) (float-plus y half-height)))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform :coerce :coerce)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-ellipse width height))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-ellipse width height)))))

;;; 30 Filled Circle

(defstandard-graphics-handlers (filled-circle 30)
  :COMMAND-ARGS (x y radius)
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius) (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-circle radius))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-circle radius)))))

;;; 31 Circle

(defstandard-graphics-handlers (circle 31)
  :COMMAND-ARGS (x y radius)
  :BOXER-EXTENTS-FORM
  (values (- x radius) (- y radius) (+ x radius) (+ y radius))
  :TRANSFORMATION-TEMPLATE
  (:x-transform :y-transform nil)
  :SPRITE-COMMAND
  (cond ((and (= x last-x) (= y last-y))
        (list 'bu::stamp-hollow-circle radius))
    (t
    (setq last-x x last-y y)
    (append (sprite-commands-for-new-position x y)
            (list 'bu::stamp-hollow-circle radius)))))
