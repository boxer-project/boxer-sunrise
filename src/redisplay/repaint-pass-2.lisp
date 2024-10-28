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
;;;;         This file contains the second pass of the repaint mechanism, which paints the box structure
;;;;         to the screen using the screen-obj calculations that have already been calculated (most likely
;;;;         by repaint-pass-1).
;;;;
(in-package :boxer)

;;;; Pass 2

(DEFUN DRAW-PORT-BOX-ELLIPSIS? (SCREEN-BOX)
       (AND (PORT-BOX? (SLOT-VALUE SCREEN-BOX 'ACTUAL-OBJ))
            (BOX-ELLIPSIS-STYLE? (SLOT-VALUE SCREEN-BOX 'SCREEN-ROWS))))

(DEFUN DRAW-PORT-BOX-ELLIPSIS (SCREEN-BOX X Y)
       (FUNCALL (GET (SLOT-VALUE SCREEN-BOX 'SCREEN-ROWS) 'DRAW-SELF) X Y))

(defmethod gray-body ((self screen-box))
  (multiple-value-bind (il it ir ib)
                       (box-borders-widths (class-name (class-of (screen-obj-actual-obj self))) self)
                       (let ((inside-wid (- (screen-obj-wid self) (+ ir il)))
                             (inside-hei (- (screen-obj-hei self) (+ ib it))))
                         (with-pen-color (*gray*)
                           (draw-rectangle inside-wid inside-hei il it)))))

(defmethod got-repainted ((self screen-obj))
  (setf (slot-value self 'tick) (tick)))

(defmethod got-repainted ((self screen-row))
  (call-next-method)
  (setf (screen-chas-array-fds (slot-value self 'screen-chas))
        ;; we need copies so that FD's which have been slid around by the editor
        ;; don't change the FD cha-no's in screen structure, if the CONSing
        ;; gets too bad, might need to consider modifying existing screen FD's
        (mapcar #'copy-boxer-font-descriptor
                (chas-array-fds (chas-array (slot-value self 'actual-obj))))))

(defun within-boxer-pane (item)
  "Checks if the screen-obj item is on the screen, taking in to affect and global scrolling
   on the canvas using the top level operating sysytem scroll bars."
  (let ((y-pos (second (multiple-value-list (xy-position item))))
        (vert-scroll (vertical-scroll *boxer-pane*)))
      (and (> (+ y-pos vert-scroll (screen-obj-hei item)) 0)
           (< (+ y-pos vert-scroll) (* (/ 1 (zoom-level *boxer-pane*)) (viewport-height *boxer-pane*))))))

(defmethod region-in-screen-box? ((self screen-box) &optional (region-list *region-list*))
  (when (and (car region-list)
             (car (screen-objs (bp-row (interval-start-bp (car *region-list*))))))  ;; sunrise-102
    (let ((region-screen-box (screen-box (car (screen-objs (bp-row (interval-start-bp (car *region-list*))))))))
      (eq self region-screen-box))))

(defmethod repaint-cursors-regions ((self screen-box))
  (when (or (bps self) (region-in-screen-box? self))
    (with-model-matrix ((3d-matrices:meye 4))
      (update-model-matrix-ubo bw::*boxgl-device*)
      (when (bps self)
        (repaint-cursor *point*))
      (when (region-in-screen-box? self)
        (dolist (region *region-list*)
          (when (not (null region)) (interval-update-repaint-all-rows region)))))))

(defmethod repaint-inferiors-pass-2-sb ((self screen-box))
  (with-slots (wid hei box-type screen-rows x-offset y-offset scroll-x-offset scroll-y-offset x-got-clipped? y-got-clipped? actual-obj bps)
    self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths box-type self)
      ;; sgithens TODO 2024-04-16 Get rid of this clear
      (clear-stencil-buffer)

      (cond ((draw-port-box-ellipsis? self)
             (draw-port-box-ellipsis self il it))
            ((or x-got-clipped? y-got-clipped?)
              ;; TODO sgithens this x,y calculation should really be fixed up in the scene graph.
              ;; I'm subtracing the scroll offsets because the world offset is shifted during scrolling, but
              ;; that pinned top left corner should really be saved somewhere that doens't get changed even
              ;; during scrolling.
              (with-clipping-inside ((+ il (- (world-x-offset self) scroll-x-offset))
                                     (+ it (- (world-y-offset self) scroll-y-offset))
                                     (- wid ir il)
                                     (- hei it ib))
             (with-model-matrix ((world-internal-matrix self))
               (do-vector-contents (inf-screen-obj screen-rows :index-var-name row-no)
                 ;; if row.y-pos > box.y-pos AND row.y-pos < box.y-pos + box.hei
                 ;;   then render the row
                 (let ((row-y-pos (second (multiple-value-list (xy-position inf-screen-obj))))
                       (row-y-hei (screen-obj-hei inf-screen-obj))
                       (box-y-pos (second (multiple-value-list (xy-position self)))))
                   (when (and (> (+ row-y-hei row-y-pos) box-y-pos)
                             (< row-y-pos (+ box-y-pos hei))
                             (within-boxer-pane inf-screen-obj))
                     (repaint-pass-2-sr inf-screen-obj))))
               (repaint-cursors-regions self))))
            (t
              (do-vector-contents (inf-screen-obj screen-rows :index-var-name row-no)
                (when (within-boxer-pane inf-screen-obj)
                  (repaint-pass-2-sr inf-screen-obj)))
              (repaint-cursors-regions self))))))

(defmethod repaint-inferiors-pass-2-sr ((self screen-row))
  (let* ((inf-x-offset 0)
         (inf-y-offset 0)
         (row-baseline (slot-value self 'baseline))
         (row-fds (row-fds (slot-value self 'actual-obj)))
         (gl-model (get-boxgl-model-for-screen-obj self)))
    (do-screen-chas-with-font-info (inf-screen-obj (slot-value self 'screen-chas)
                                                   :index-var-name cha-no
                                                   :font-descriptors row-fds
                                                   :cha-drawing? t)
      (cond ((screen-cha? inf-screen-obj)
             ;; draw the char
             (if (get-glyph *freetype-glyph-atlas* `(,(opengl-font-fontspec *current-opengl-font*) ,inf-screen-obj ,(coerce *font-size-baseline* 'float)))
              (when (needs-update gl-model)
                (draw-cha inf-screen-obj
                          inf-x-offset (+ row-baseline inf-y-offset)
                          :gl-model gl-model))
              (draw-cha inf-screen-obj inf-x-offset (+ row-baseline inf-y-offset)))
             ;; update the inf-x-offset
             (incf inf-x-offset (cha-wid inf-screen-obj)))
        (t
         ;; make sure the box knows where it should be
         ;; this SHOULD be correct as a result of pass-1
         (unless (and (= (screen-obj-x-offset inf-screen-obj) inf-x-offset)
                      ;; should we bother to checks y-offsets ?
                      )
           ;; sgithens 2024-06-13 This should no longer happen. If this error isn't triggered, remove this entire
           ;;                     'unless' block in another release or so.
           ;;
           (error "repaint-inferiors-pass-2-sr adjusting items that should have been handled in pass-1
  inf-screen-obj: ~A
  (screen-obj-x-offset inf-screen-obj): ~A
                          inf-x-offset: ~A
  (screen-obj-y-offset inf-screen-obj): ~A
                          inf-y-offset: ~A"
             inf-screen-obj
             (screen-obj-x-offset inf-screen-obj) inf-x-offset
             (screen-obj-y-offset inf-screen-obj) inf-y-offset)
           (setf (screen-obj-x-offset inf-screen-obj) inf-x-offset)
           (setf (screen-obj-y-offset inf-screen-obj) inf-y-offset))
         (repaint-pass-2-sb inf-screen-obj)
         ;; finally update inf-x-offset, screen-box style
         (incf inf-x-offset (screen-obj-wid inf-screen-obj)))))
    (draw gl-model)))

(defmethod repaint-pass-2-sr ((self screen-row))
  (with-slots (x-offset y-offset wid hei actual-obj)
    self
    (when actual-obj
      (with-model-matrix ((world-matrix self))
        (when (closet-row? actual-obj (superior-box actual-obj))
          ;; this should get the inner width from the superior box
          (with-pen-color (*closet-color*) (draw-rectangle wid hei 0 0)))
        (repaint-inferiors-pass-2-sr self)
        (got-repainted self)))))

(defmethod repaint-pass-2-sb ((self screen-box))
  ;; (multiple-value-bind (x-pos y-pos) (xy-position self)
  (with-slots (x-offset y-offset wid hei actual-obj box-type)
    self
    (with-model-matrix ((world-matrix self))
      (maintaining-pen-color
       ;; need this because we may be in the middle of a colored font run
       (%set-pen-color *foreground-color*)
       ;; During redisplay-pass-2 the only part of the screen the redisplay
       ;; methods are allowed to draw in is the max of the region currently
       ;; occupied by the screen obj, and the space that will be occupied by
       ;; the screen obj when redisplay-pass-2 is complete.
       (case (display-style self)
         (:supershrunk (draw-super-shrunk-box actual-obj 0 0 box-type))
         (:shrunk (let ((boxtop (boxtop actual-obj)))
                    (cond ((null boxtop)
                           (gray-body self)
                           ;; Now deal with the Borders,
                           (box-borders-draw box-type self))
                      (t
                       ;; keep track of the boxtop we are drawing so
                       ;; we can tell in pass-1 if it changes (and, therefore,
                       ;; need to erase the old one...
                       (setf (display-style-border-style
                              (slot-value self 'display-style-list))
                             boxtop)
                       (with-clipping-inside ((- (world-x-offset self) 1)
                                              (- (world-y-offset self) 1)
                                              wid
                                              hei)
                         (draw-boxtop self boxtop actual-obj 0 0 wid hei))))))
         (t ;; have to draw any background BEFORE inferiors
            (multiple-value-bind (lef top rig bot)
                                  (box-borders-widths box-type self)
                                 (when (not (null (get-background-color actual-obj)))
                                   (with-pen-color ((get-background-color actual-obj))
                                     (draw-rectangle (- wid rig lef) (- hei bot top) lef top))))
            (repaint-inferiors-pass-2-sb self)
            ;; draw any scroll info no
            (draw-scroll-info self)
            ;; Now deal with the Borders,
            (box-borders-draw box-type self))))))
  ;; Make a note of the fact that this screen box has
  ;; been redisplayed (pass-1 and pass-2 complete).
  (got-repainted self))

(defmethod repaint-inferiors-pass-2-sb ((SELF GRAPHICS-SCREEN-BOX))
  (LET ((GRAPHICS-SHEET (screen-obj-actual-obj (SCREEN-SHEET SELF))))
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths (slot-value self 'box-type) self)
                         (let ((inner-width  (min (- (screen-obj-wid self) il ir)
                                                 (graphics-sheet-draw-wid graphics-sheet)))
                               (inner-height (min (- (screen-obj-hei self) it ib)
                                                 (graphics-sheet-draw-hei graphics-sheet))))
                           (with-model-matrix ((world-matrix (screen-sheet self)))
                             (with-clipping-inside ((+ ir (world-x-offset self))
                                                    (+ it (world-y-offset self))
                                                    inner-width
                                                    inner-height)
                               (when (not (null (graphics-sheet-background graphics-sheet)))
                                 (with-pen-color ((graphics-sheet-background graphics-sheet))
                                   (draw-rectangle inner-width inner-height 0 0)))
                               (let ((ba (graphics-sheet-bit-array graphics-sheet)))
                                 (unless (null ba)
                                   (bitblt-to-screen
                                     (min inner-width  (ogl-pixmap-width  ba))
                                     (min inner-height (ogl-pixmap-height ba))
                                     ba 0 0 0 0)))
                               ;; First draw the graphics list operations, then display the
                               ;; framebuffer they just got painted to, assuming framebuffers
                               ;; are in use.
                               (unless (null (graphics-sheet-graphics-list graphics-sheet))
                                 (redisplay-graphics-sheet-graphics-list graphics-sheet self))
                               ;; Draw the gl-model canvas / framebuffer
                               (when *use-opengl-framebuffers*
                               (let* ((wid (graphics-sheet-draw-wid graphics-sheet))
                                       (hei (graphics-sheet-draw-hei graphics-sheet))
                                       (canvas (get-graphics-canvas-for-screen-obj self wid hei))
                                       (pixmap (graphics-canvas-pixmap canvas)))
                                 (bitblt-to-screen (min inner-width  (ogl-pixmap-width  pixmap))
                                                   (min inner-height (ogl-pixmap-height pixmap))
                                                   pixmap 0 0 0 0)))
                               ;; then handle any sprite graphics...
                               (unless (null (graphics-sheet-graphics-list graphics-sheet))
                                 (redisplay-graphics-sheet-sprites graphics-sheet self))))))))

;;;; Screen Sprites....
(defmethod repaint-pass-2-sb ((self sprite-screen-box))
  )


(defun top-level-repaint-pass-2 ()
  ;; Currently this is above the repaint-pass-2 even though it should be below it forcing a second repaint. If we
  ;; added support for a z-index then we could just paint it below after the dimensions are calculated.
  (with-pen-color (*background-color*)
    (draw-rectangle (+ 6 (screen-obj-wid *outermost-screen-box*)) (+ 6 (screen-obj-hei *outermost-screen-box*)) 6 6))
  (repaint-pass-2-sb *outermost-screen-box*))
