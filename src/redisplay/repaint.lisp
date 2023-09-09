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
;;;;         This file contains all of the high level code that the redisplay uses
;;;;         in the OpenGL port
;;;;
;;;;  Modification History (most recent at the top)
;;;;
;;;;   4/22/14 rewrote cursor display to hack fonts cursor-info & repaint-cursor-internal
;;;;   9/ 1/12 Removed fixnum arithmetic from:
;;;;           needs-repaint-pass-1? (screen-obj), repaint-inferiors-pass-1-sb (screen-box)
;;;;           rp1-sr-increment-superior-parameters (screen-row)
;;;;           repaint-inferiors-pass-1-sr (screen-row)
;;;;           rp1-sb-increment-superior-parameters (screen-box)
;;;;           repaint-pass-1-sr (screen-row), repaint-pass-1-sb (screen-box)
;;;;           repaint-inferiors-pass-2-sb (screen-box), repaint-inferiors-pass-2-sr (screen-row)
;;;;           gray-body (screen-box)
;;;;           repaint-inferiors-pass-{1,2}-sb (graphics-screen-box)
;;;;           repaint-cursor-internal
;;;;   8/23/11 flush-port-buffer removed from repaint-window and repaint-cursor,
;;;;           added to repaint-internal, repaint-in-eval
;;;;           removed force-repaint-window
;;;;   8/12/11 repaint
;;;;  11/09/10 repaint-window: flush-port-buffer instead of bw::swap-buffers
;;;;  11/07/10 use capi:apply-in-pane-process in repaint
;;;;   9/26/10 added repaint-mouse-docs to repaint-window
;;;;   2/05/10 extra reality checking added to repaint
;;;;  12/09/09 repaint-guts checks for null *outermost-screen-box*
;;;;  11/27/09 repaint-pass-2-sr closet highlighting added
;;;;  11/24/09 repaint-in-eval checks bw::*suppressed-actions*
;;;;  11/20/09 added force? arg to repaint-in-eval
;;;;   9/02/09 fixed bug in unseen-box-width
;;;;   2/26/09 repaint-inferiors-pass-1-sb resets max-scroll-wid @ beginning of pass
;;;;   2/24/09 now compiler warning free
;;;;   2/23/09 repaint-pass-1-sr, repaint-inferiors-pass-1-sr, update-scroll-wid, unseen-box-width
;;;;           now with horizontal scrolling action
;;;;   2/12/09 got-repainted for SB's no longer needed after removal of inf-shift slot
;;;;  02/07/09 repaint-pass-2-sb now calls draw-scroll-info
;;;;  11/15/08 repaint-in-eval checks system variable *repaint-during-eval?*
;;;;  05/15/06 started file
;;;;

(in-package :boxer)

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





;; get an approximate width of the box for scrolling purposes (DO NOT RECURSE !!)
(defvar *box-borders-horizontal-extent* 15)
(defvar *average-cha-wid* 6)

(defmethod unseen-box-width ((self box))
  (max *minimum-box-wid*
       (+ *box-borders-horizontal-extent*
          (let ((max-row-wid 0))
            (do-box-rows ((row self))
              (setq max-row-wid (max max-row-wid
                                     (* (length-in-chas row) *average-cha-wid*))))
            max-row-wid))))







(defmethod set-scroll-to-actual-row ((self screen-box) new-value)
  (unless (eq new-value (slot-value self 'scroll-to-actual-row))
    (let ((actual-obj (screen-obj-actual-obj self)))
      (when (not-null actual-obj)
        (setf (slot-value self 'scroll-to-actual-row) new-value)
        (record-scroll-info actual-obj self new-value)
        (setf (slot-value self 'scroll-y-offset) 0)
        T))))

;; need to keep track of scrolling info on the editor box so that when we
;; come back to a previously scrolled box (for which the screen structure
;; may have, in the interim, been deallocated) we can then restore the
;; scrolled state
(defmethod record-scroll-info ((self box) screen-box row)
  (let* ((current-scroll-info (getf (slot-value self 'plist) 'scroll-info))
         (port-chain (get-port-chain screen-box))
         (match (assoc port-chain current-scroll-info :test #'equal)))
    (cond ((not (null match)) (setf (cdr match) row))
      (t (push (cons port-chain row)
               (getf (slot-value self 'plist) 'scroll-info))))))


;; walk up the screen hierarchy looking for ports.  This provides the most
;; reliable way of identifying unique screen structure without actually
;; using screen structure.  We can't use screen objs because they are
;; ephemeral, and worse, can become either deallocated or allocated to
;; a totally different editor object
(defun get-port-chain (screen-box)
  (let ((ports nil))
    (do ((sb screen-box (superior-screen-box sb)))
      ((not (screen-box? sb))  ; not NIL check cause screen top is *boxer-pane*
                               (nreverse ports))
      (let ((actual-obj (screen-obj-actual-obj sb)))
        (when (port-box? actual-obj) (push actual-obj ports))))))

;; tall 1st lines
(defmethod scroll-dn-one-screen-box ((self screen-box))
  (let ((last-screen-row (last-screen-row self)))
    (unless (null last-screen-row)
      (let* ((last-row (screen-obj-actual-obj last-screen-row))
             (new-scroll-row (cond ((and (= (screen-rows-length self) 1)
                                         (not (null (next-row last-row))))
                                    (next-row last-row))
                               (t last-row))))
        (set-scroll-to-actual-row self new-scroll-row)
        new-scroll-row))))

;; zeroes Y scrolling offset.  this fixes non pageUp for (tall) partially scrolled
;; 1st lines
(defmethod scroll-up-one-screen-box ((self screen-box))
  (unless (or (null (screen-obj-actual-obj self))
              (zerop& (screen-rows-length self)))
    (setf (slot-value self 'scroll-y-offset) 0)
    (ensure-row-is-displayed (screen-obj-actual-obj (first-screen-row self))
                             self -1 t)))

;; shadow these methods out for graphics-screen-boxes
(defmethod set-scroll-to-actual-row ((self graphics-screen-box) new-value)
  ;  (declare (ignore self))
  (declare (ignore new-value))
  nil)

(defmethod scroll-dn-one-screen-box ((self graphics-screen-box))
  ;  (declare (ignore self))
  nil)

(defmethod scroll-up-one-screen-box ((self graphics-screen-box))
  ;  (declare (ignore self))
  nil)



;;;; Pass 2

(DEFUN DRAW-PORT-BOX-ELLIPSIS? (SCREEN-BOX)
       (AND (PORT-BOX? (SLOT-VALUE SCREEN-BOX 'ACTUAL-OBJ))
            (BOX-ELLIPSIS-STYLE? (SLOT-VALUE SCREEN-BOX 'SCREEN-ROWS))))

(DEFUN DRAW-PORT-BOX-ELLIPSIS (SCREEN-BOX X Y)
       (FUNCALL (GET (SLOT-VALUE SCREEN-BOX 'SCREEN-ROWS) 'DRAW-SELF) X Y))

(defmethod repaint-inferiors-pass-2-sb ((self screen-box))
  (with-slots (wid hei box-type screen-rows scroll-x-offset scroll-y-offset)
    self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths box-type self)
                         (with-clipping-inside (il it (- wid il ir) (- hei it ib))
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
    (with-drawing-inside-region (x-offset y-offset wid hei)
      (when (closet-row? actual-obj (superior-box actual-obj))
        ;; this should get the inner width from the superior box
        (with-pen-color (*closet-color*) (draw-rectangle wid hei 0 0)))
      (repaint-inferiors-pass-2-sr self)
      (got-repainted self))))

(defvar *tutti-frutti* nil)

(defmethod repaint-pass-2-sb ((self screen-box))
  (with-slots (x-offset y-offset wid hei actual-obj box-type)
    self
    (with-drawing-inside-region (x-offset y-offset wid hei)
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
            (box-borders-draw box-type self))))))
  ;; Make a note of the fact that this screen box has
  ;; been redisplayed (pass-1 and pass-2 complete).
  (got-repainted self))

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
                                                     (with-drawing-inside-region (x y inner-width inner-height)
                                                       (with-turtle-clipping (inner-width inner-height)
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
                                                           (redisplay-graphics-sheet-sprites graphics-sheet self)))))))))



;;;; Screen Sprites....

(defmethod repaint-pass-2-sb ((self sprite-screen-box))
  )

(defun repaint-window (&OPTIONAL (WINDOW *BOXER-PANE*) (flush-buffer? t) &KEY (process-state-label "stopped"))
  (opengl:rendering-on (window)
    (bw::check-for-window-resize)
    (REDISPLAYING-WINDOW (WINDOW)
                         (clear-window window)
                         (repaint-guts)
                         (repaint-mouse-docs)
                         (repaint-dev-overlay process-state-label)
                         (when flush-buffer? (swap-graphics-buffers window)))))

;;; called also by printing routines.
(defun repaint-guts ()
  (unless (null *outermost-screen-box*)
    ;; can happen asynch, during window startup if window systems tries to update before
    ;; all the boxer innards are created
    (top-level-repaint-pass-1)
    (top-level-repaint-pass-2)))

;;
(defun top-level-repaint-pass-2 ()
  (repaint-pass-2-sb *outermost-screen-box*))

(defun repaint-internal (&optional just-windows?)
  (redisplaying-unit
    (dolist (redisplayable-window *redisplayable-windows*)
      (repaint-window redisplayable-window (not (eq redisplayable-window
                                                    *boxer-pane*))))
    (dolist (region *region-list*)
      (when (not (null region)) (interval-update-repaint-all-rows region)))
    ;; comment out next line for outermost box save document, updates will
    ;; occur inside of set-outermost-box instead...
    (when (bp? *point*)
      ; (set-window-name (current-file-status (point-box)))
      ;; repaint-cursor can now cause horizontal scrolling of the box
      ;; neccessitating an additional repaint, if so, it will throw
      ;; to 'scroll-x-changed TAG
      (unless just-windows?
        (repaint-cursor *point* nil)))
    ;; swap buffers here, after all drawing is complete
    (swap-graphics-buffers *boxer-pane*)))

(defun repaint (&optional just-windows?)
  (opengl:rendering-on (*boxer-pane*)
    (when *reload-shaders*
      (update-boxgl-programs)
      (setf *reload-shaders* nil))
    (repaint-internal just-windows?)))

(defun repaint-with-cursor-relocation ()
  (let ((*allow-redisplay-encore? t))
    (repaint)))

;;;; Ephemera: cursors, regions
(defun repaint-cursor (&optional (cursor *point*)(flush-buffer? T))
  (drawing-on-window (*boxer-pane*)
                     ;; this updates the position & size
                     (repaint-cursor-internal cursor)
                     ;; now draw it
                     (draw-blinker (point-blinker *boxer-pane*))
                     (when flush-buffer? (swap-graphics-buffers *boxer-pane*))))

(defun repaint-cursor-internal (&optional (cursor *point*))
  (ignore-errors  ;; without this, errors here will generate endless beeping
                  (and (bp? cursor)
                       (multiple-value-bind (cursor-x cursor-y size-hint)
                                            (bp-coordinates cursor)
                                            (declare (ignore size-hint))
                                            (let* ((psb (point-screen-box))
                                                   ;; 1/9/03 under certain conditions, point-screen-box can be NIL
                                                   ;; usually because  of programatic scrolling
                                                   (actual-box (or (and psb (screen-obj-actual-obj psb))
                                                                   (bp-box cursor)))
                                                   (scroll-row (and psb (scroll-to-actual-row psb)))
                                                   (scroll-y-offset (or (and psb (slot-value psb 'scroll-y-offset))
                                                                        0)))
                                              ;; check the BP coords here, we may have to adjust the scroll-x-offset of the psb
                                              ;; check to see if cursor-x falls inside of PSB, if not, change the scroll-x-offset of psb
                                              (unless (or (null psb) (null *allow-redisplay-encore?))
                                                (multiple-value-bind (lef top rig bot)
                                                                     (box-borders-widths (box-type psb) psb) (declare (ignore top bot))
                                                                     (let* ((psb-x (- (screen-obj-absolute-coordinates psb) (slot-value psb 'scroll-x-offset)))
                                                                            (psb-min-x (+ psb-x lef))
                                                                            (psb-max-x (+ psb-x (- (screen-obj-wid psb) rig)))
                                                                            (move-distance (floor (slot-value psb 'wid) 2)))
                                                                       (cond ((<= psb-min-x cursor-x psb-max-x)) ; everything is ok, cursor is already visible
                                                                         ((< cursor-x psb-min-x) ; cursor is to left of the visible part of the box
                                                                                                 (setf (slot-value psb 'scroll-x-offset)
                                                                                                       (min 0 (+ (slot-value psb 'scroll-x-offset) (- psb-min-x cursor-x) move-distance)))
                                                                                                 (setq *redisplay-encore?* t)
                                                                                                 (throw 'scroll-x-changed t))
                                                                         ((> cursor-x psb-max-x) ; cursor is to the right of the visible part of the box
                                                                                                 (setf (slot-value psb 'scroll-x-offset)
                                                                                                       (min 0 (+ (slot-value psb 'scroll-x-offset) (- psb-max-x cursor-x move-distance))))
                                                                                                 (setq *redisplay-encore?* t)
                                                                                                 (throw 'scroll-x-changed nil))))))
                                              ;; continue...
                                              (multiple-value-bind (c-width c-height offset-from-top)
                                                                   (cursor-info cursor)
                                                                   (let ((on-scroll-row-offset (if (or (eq (bp-row cursor) scroll-row)
                                                                                                       (and (null scroll-row) (box? actual-box)
                                                                                                            (eq (bp-row cursor) (first-inferior-row actual-box))))
                                                                                                 scroll-y-offset
                                                                                                 0)))
                                                                     (update-blinker (point-blinker *boxer-pane*)
                                                                                     cursor-x
                                                                                     (- (+ cursor-y offset-from-top) on-scroll-row-offset)
                                                                                     c-width
                                                                                     (+ c-height on-scroll-row-offset))))
                                              ;            (cond ((and (or (eq (bp-row cursor) scroll-row)
                                              ;                            (and (null scroll-row)
                                              ;                                 (box? actual-box)
                                              ;                                 (eq (bp-row cursor)
                                              ;                                     (first-inferior-row actual-box))))
                                              ;                        (not (zerop scroll-y-offset)))
                                              ;                   (set-cursorpos *boxer-pane* cursor-x (- cursor-y scroll-y-offset))
                                              ;                   (SET-CURSOR-SIZE *POINT-BLINKER* 3 (+ (get-cursor-height cha)
                                              ;                                                         scroll-y-offset)))
                                              ;                  (t
                                              ;                   (set-cursorpos *boxer-pane* cursor-x cursor-y)
                                              ;                   (SET-CURSOR-SIZE *POINT-BLINKER* 3
                                              ;                                    (get-cursor-height cha))))
                                              )))))


(defvar *default-cursor-width* 3)
(defvar *unseen-box-cursor-height* 17)

;; returns width, height & offset-from-top
;; cursor in front of a box should be displayed from the top of the row, for chars it should
;; be anchored on the baseline
(defun cursor-info (cursor)
  (let* ((row (bp-row cursor))
         (cha-no (bp-cha-no cursor))
         (sb (bp-screen-box cursor))
         (font-no (bfd-font-no (closest-bfd row cha-no)))
         (screen-row (cdr (assoc sb (slot-value row 'screen-objs))))
         (cha (cha-at-cha-no row cha-no)))
    (cond ((box? cha)
           (cond ((null (displayed-screen-objs cha)) ; box is not visible
                                                     (values *default-cursor-width* *unseen-box-cursor-height* 0))
             (t (let ((sb (inf-current-screen-box cha)))
                  (if (null sb)
                    (values *default-cursor-width* *unseen-box-cursor-height* 0)
                    (values *default-cursor-width* (screen-obj-hei sb) 0))))))
      ((name-row? row) (values *default-cursor-width* (string-hei *border-name-font*) 0))
      (t (values *default-cursor-width*
                 (string-ascent font-no)
                 (if (null screen-row) 0
                   (- (slot-value screen-row 'baseline) (string-ascent font-no))))))))

(defun get-cursor-height (cha)
  (COND ((NULL CHA) 12)
        ((CHA? CHA) (CHA-HEI))
        ((and (box? cha) (null (displayed-screen-objs cha)))
         17)
        ((EQ ':SHRUNK
             (DISPLAY-STYLE
              (BP-SCREEN-BOX *POINT*)))
         (- (SCREEN-OBJ-HEI (BP-SCREEN-BOX *POINT*))
            17))
        ((name-row? (SLOT-VALUE cha 'name))
         (+ ;(box-borders-name-tab-height (car (displayed-screen-objs cha))) 7
            12))
        (T
         (let ((sb (INF-CURRENT-SCREEN-BOX CHA)))
           (if (null sb) 17 (SCREEN-OBJ-HEI sb))))))




;;; This is called periodically during eval
;;; Try to avoid excessive repaints by:
;;;  1) Don't repaint more often than *eval-repaint-quantum* (default 50
;;;     milliseconds)
;;;  2) Make sure we wait at least *eval-repaint-ration* times the length of the
;;;     last repaint to keep repaint time from dominating the processors time
;;;     (default is 2, i.e. repaint will never take up more than 1/2 the
;;;     processor time)
;;;
(defun repaint-in-eval (&optional force?)
  ;; if fast enuff...
  ; (bw::update-toolbar-font-buttons)
  (let ((now (get-internal-real-time)))
    (when (or force?
              (and (>& now (+ *last-eval-repaint* *eval-repaint-quantum*))
                  (>& now (+ *last-eval-repaint* (* *eval-repaint-ratio*
                                                    *last-repaint-duration*)))))
        #+lispworks (capi::apply-in-pane-process *boxer-pane* #'bw::update-toolbar-font-buttons)
        (setq *last-eval-repaint* now)
        (process-editor-mutation-queue-within-eval)
        (unless (null bw::*suppressed-actions*)
          (funcall (pop bw::*suppressed-actions*)))
        (repaint-window *boxer-pane* t :process-state-label "eval")
        (setq *last-repaint-duration* (- (get-internal-real-time) now))))
)
