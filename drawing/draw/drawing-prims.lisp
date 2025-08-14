(in-package :boxer)

(boxer-eval::defboxer-primitive bu::snip ((bu::Port-to box)
                              (boxer-eval::numberize x) (boxer-eval::numberize y)
                              (boxer-eval::numberize width) (boxer-eval::numberize height))
  ;; some args checking...
  (cond ((or (not (numberp width)) (< width 1))
         (boxer-eval::primitive-signal-error
          :graphics "Width(" width ") should be a positive integer"))
        ((not (typep width 'fixnum)) (setq width (round width))))
  (cond ((or (not (numberp height)) (< height 1))
         (boxer-eval::primitive-signal-error
          :graphics "Height(" height ") should be a positive integer"))
        ((not (typep height 'fixnum)) (setq height (round height))))
  ;; check X and Y
  (let* ((gb (box-or-port-target box))
         (graphics-sheet (if (box? gb)
                             (graphics-info gb)
                             (graphics-info-graphics-sheet (vc-graphics gb))))
         (new-gs (make-graphics-sheet-with-graphics-list (fixr width)
                                                         (fixr height)))
         (newvc (make-empty-vc)))
    (cond ((null graphics-sheet)
           (boxer-eval::primitive-signal-error :graphics box
                                         "does not have any graphics"))
          (t
           ;; fix up the new graphics sheet
           (setf (graphics-sheet-draw-mode new-gs) :clip)
           (setf (vc-graphics newvc) (list 'graphics-sheet new-gs))
           (unless (null (graphics-sheet-background graphics-sheet))
             (setf (graphics-sheet-background new-gs)
                   (graphics-sheet-background graphics-sheet)))
           (with-graphics-vars-bound-internal graphics-sheet
             (let ((orig-x (fix-array-coordinate-x (- (float x) (/ width 2.0))))
                   (orig-y (fix-array-coordinate-y (float (+ y (/ height 2.0))))))
               ;(when (not (and (<& -1 orig-x %drawing-width)
               ;                (<& -1 orig-y %drawing-height)))
               ; (boxer-eval::primitive-signal-error :graphics
               ;			       "Point is not in graphics box"))
               (unless (null (graphics-sheet-bit-array graphics-sheet))
                 ;; NOTE: put this here instead of for ALL new-gs's
                 ;; since only bitmaps are non lisp structures
                 (queue-non-lisp-structure-for-deallocation new-gs)
                 (let ((new-bm (make-ogl-pixmap
                                                      width height)))
                   (setf (graphics-sheet-bit-array new-gs) new-bm)
                   ;; copy the relevant part of the bitmap
                   (drawing-on-bitmap (new-bm)
                     ;; first, initialize the bitmap in the background color
                     ;; in case pieces of it will get snipped from OUTSIDE
                     ;; the original
                     (with-pen-color ((or (graphics-sheet-background graphics-sheet)
                                          *background-color*))
                       (draw-rectangle width height 0 0))
                     (unless (or (>=& orig-x %drawing-width)
                                 (>=& orig-y %drawing-height))
                       (bitblt-to-screen
                                         (if (minusp& orig-x)
                                             (max& 0
                                                   (min& %drawing-width
                                                         (-& width (-& orig-x))))
                                             (min& width
                                                   (-& %drawing-width orig-x)))
                                         (if (minusp& orig-y)
                                             (max& 0
                                                   (min& %drawing-height
                                                         (-& height (-& orig-y))))
                                             (min& height
                                                   (-& %drawing-height orig-y)))
                                         (graphics-sheet-bit-array graphics-sheet)
                                         (max& orig-x 0) (max& orig-y 0) 0 0)))))
               ;; if there is a graphics list, it needs to be
               ;; translated to look right
               (let ((gl (graphics-sheet-graphics-list graphics-sheet))
                     (new-gl (graphics-sheet-graphics-list new-gs)))
                 (when (and gl (not (zerop&(storage-vector-active-length gl))))
                   (copy-graphics-command-list-state gl new-gl)
                   (do-vector-contents (command gl)
                     (let ((command-copy (copy-graphics-command command)))
                       (translate-graphics-command
                        command-copy
                        (floor (- (- (/ width 2) x) (/ %drawing-width 2)))
                        (floor (- (/ height 2) (/ %drawing-height 2) (- y)))
                        )
                       (sv-append new-gl command-copy)))))))))
    newvc))

(boxer-eval::defboxer-primitive bu::freeze ()
  (let ((gb (get-relevant-graphics-box)))
    (if (or (null gb) (eq gb :no-graphics))
        (boxer-eval::primitive-signal-error :graphics "No graphics to FREEZE")
      (let* ((graphics-sheet
              (if (box? gb)
                  (graphics-info gb)
                (graphics-info-graphics-sheet (vc-graphics gb))))
             (wid (graphics-sheet-draw-wid graphics-sheet))
             (hei (graphics-sheet-draw-hei graphics-sheet))
             (display-list (graphics-sheet-graphics-list graphics-sheet))
             (bitmap (graphics-sheet-bit-array graphics-sheet)))
        (when (null bitmap)
          ;; make sure there IS a backing store
          (let ((new (make-ogl-pixmap wid hei)))
            ;; don't clear the bitmap because the background will be handled in the next step
            (setf (graphics-sheet-bit-array graphics-sheet) new)))
        ;; use the back buffer as a scratch buffer, it will get drawn over in the next repaint
        ;; anyway.
        (drawing-on-bitmap ((graphics-sheet-bit-array graphics-sheet))
          ;; use the upper left corner of the back buffer
          ;; start with the "background" color
          (cond ((null bitmap) ;; note that bitmap refers to a pre-existing bit-array
                 (with-pen-color ((or (graphics-sheet-background graphics-sheet) *background-color*))
                   (draw-rectangle wid hei 0 0)))
                (t (bitblt-to-screen wid hei bitmap 0 0 0 0)))
          ;; now play the graphics list into the same area
          (with-graphics-vars-bound-internal graphics-sheet
            (boxer-playback-graphics-list display-list :translate? t)))
        ;; now clear the display list and graphics-canvas if in use
        (clear-graphics-list display-list)
        (clear-graphics-canvas gb)
        (setf (ogl-pixmap-update-texture-p (graphics-sheet-bit-array graphics-sheet)) t)
        (modified-graphics gb)
        boxer-eval::*novalue*))))
