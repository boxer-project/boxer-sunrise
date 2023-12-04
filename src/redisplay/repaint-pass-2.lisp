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

(defvar *tutti-frutti* nil)

;; useful for testing
(defun colorit (w h x y)
  (with-pen-color ((bw::make-ogl-color (+ .3 (random .7))
                                       (random 1.0)
                                       (+ .3 (random .7)) .2))
    (draw-rectangle w h x y)))


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

(defmethod repaint-inferiors-pass-2-sb ((self screen-box))
  (with-slots (wid hei box-type screen-rows scroll-x-offset scroll-y-offset actual-obj)
    self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths box-type self)
                         (format t
"~%with-clipping-inside name: ~A x: ~A y: ~A wid: ~A hei: ~A
                        h-scroll-amt: ~A v-scroll-amt: ~A
                        new-x: ~A new-y: ~A "
                           (name actual-obj) il it (- wid il ir) (- hei it ib)
                           (horizontal-scroll *boxer-pane*) (vertical-scroll *boxer-pane*)
                           (+ it (vertical-scroll *boxer-pane*))
                           (+ il (horizontal-scroll *boxer-pane*)))
                        ;;  (with-clipping-inside (il it (- wid il ir) (- hei it ib))
                         (with-clipping-inside ((+ il (horizontal-scroll *boxer-pane*)) (+ it (vertical-scroll *boxer-pane*)) (- wid il ir) (- hei it ib))
                           ;; The clipping and rescaling for these methods is similar to
                           ;; the clipping and rescaling for the other redisplay-pass-1
                           ;; and redisplay-pass-2 methods, except that here the region
                           ;; of the screen of that is being draw inside is
                           ;; a subpart of the screen obj, the screen-box's screen-rows.
                           (if (draw-port-box-ellipsis? self)
                             (draw-port-box-ellipsis self il it)
                             (with-scrolling-origin (scroll-x-offset scroll-y-offset)
                               (do-vector-contents (inf-screen-obj screen-rows :index-var-name row-no)
                                 (repaint-pass-2-sr inf-screen-obj))))))))

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
    ;; (with-drawing-inside-region (x-offset y-offset wid hei)
    (with-origin-at (x-offset y-offset)
      (when (closet-row? actual-obj (superior-box actual-obj))
        ;; this should get the inner width from the superior box
        (with-pen-color (*closet-color*) (draw-rectangle wid hei 0 0)))
      (repaint-inferiors-pass-2-sr self)
      (got-repainted self)
      )
      )))

(defmethod repaint-pass-2-sb ((self screen-box))
  (multiple-value-bind (x-pos y-pos) (xy-position self)
    (format t "~%2repain-pass-2-sb xy-position: x: ~A y: ~A" x-pos y-pos)
  (with-slots (x-offset y-offset wid hei actual-obj box-type)
    self
    (with-origin-at (x-offset y-offset)
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
                       (draw-boxtop boxtop actual-obj 0 0 wid hei)
                       ;			    (with-turtle-clipping (wid hei)
                       ;			      (draw-boxtop boxtop actual-obj 0 0 wid hei))
                       ))))
         (t ;; have to draw any background BEFORE inferiors
            (unless nil ;(null background)
              (multiple-value-bind (lef top rig bot)
                                   (box-borders-widths box-type self)
                                   (cond ((not (null *tutti-frutti*))
                                          (colorit  (- wid rig lef) (- hei bot top) lef top))
                                         ((not (null (get-background-color actual-obj)))
                                          (with-pen-color ((get-background-color actual-obj))
                                            (draw-rectangle (- wid rig lef) (- hei bot top) lef top))) )))
            (repaint-inferiors-pass-2-sb self)
            ;; draw any scroll info no
            (draw-scroll-info self)
            ;; Now deal with the Borders,
            (box-borders-draw box-type self))))))) ;)
  ;; Make a note of the fact that this screen box has
  ;; been redisplayed (pass-1 and pass-2 complete).
  (got-repainted self))

;;;redisplay for graphics boxes


(defmethod repaint-inferiors-pass-2-sb ((SELF GRAPHICS-SCREEN-BOX))
  (LET ((GRAPHICS-SHEET (GRAPHICS-SCREEN-SHEET-ACTUAL-OBJ
                          (SCREEN-SHEET SELF)))
        (av-info (av-info (slot-value self 'actual-obj))))
         (multiple-value-bind (x y)
                              (graphics-screen-sheet-offsets (screen-sheet self))
                              (multiple-value-bind (il it ir ib)
                                                   (box-borders-widths (slot-value self 'box-type) self)
                                                   (let ((inner-width  (min (- (screen-obj-wid self) il ir)
                                                                            (graphics-sheet-draw-wid graphics-sheet)))
                                                         (inner-height (min (- (screen-obj-hei self) it ib)
                                                                            (graphics-sheet-draw-hei graphics-sheet))))
                                                    ;;  (with-drawing-inside-region (x y inner-width inner-height)
                                                     (with-origin-at (x y)
                                                      ;;  (with-turtle-clipping (inner-width inner-height)
                                                         ;; first handle the background or video if there is any
                                                         (cond ((not (null av-info))
                                                                (draw-current-av-info av-info 0 0
                                                                                      inner-width inner-height))
                                                           ((not (null (graphics-sheet-background graphics-sheet)))
                                                            (with-pen-color ((graphics-sheet-background graphics-sheet))
                                                              (draw-rectangle inner-width inner-height 0 0))))
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
                                                           (redisplay-graphics-sheet-sprites graphics-sheet self)))))))) ;)

;;;; Screen Sprites....

(defmethod repaint-pass-2-sb ((self sprite-screen-box))
  )

;;
(defun top-level-repaint-pass-2 ()
  ;; Currently this is above the repaint-pass-2 even though it should be below it forcing a second repaint. If we
  ;; added support for a z-index then we could just paint it below after the dimensions are calculated.
  (with-pen-color (*background-color*)
    (draw-rectangle (+ 6 (screen-obj-wid *outermost-screen-box*)) (+ 6 (screen-obj-hei *outermost-screen-box*)) 6 6))
  (repaint-pass-2-sb *outermost-screen-box*))
