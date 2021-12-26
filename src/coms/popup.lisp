;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    Portable Popup Menus

Modification History (most recent at top)

 9/14/12 de-fixnum arithmetic: item-size, item-height, menu-size, draw-item, track-item,
                               menu-select, draw-doc, doc-{width,height}
                               popup-doc-{offset-coords,shrink,expand,view-flip,
                                          resize,toggle-type,graphics}
                               com-mouse-{tl,tr,bl,br,type-tag}-pop-up
 8/15/12 handle float return from string-wid in: draw-doc
 7/20/11 #+opengl menu-select added draw-menu for initial selection case
12/04/10 #-opengl draw-doc
 9/25/10 initial FULL OpenGL implementation
 6/15/05 added new popup docs, :bottom-right-off        = "Manual Sizing"
                               :top-left-initial-box    = "Box Menu"
                               :top-right-outermost-box = "Box Menu"
         used by popup-(un)doc-{shrink,expand,resize} to be more relevant to particular situations
 6/14/05 popup-doc-resize, popup clause moved to the end of function, it was causing
         a popup delay before any of the other mouse documentation could run
 6/12/05 for some reason, we had to add a window-system-dependent-set-origin
         call to popup-doc-resize even though we are in a drawing-on-window env
         and none of the other corner docs needs to
12/20/04 menu-select needed to have the window origin reset after the mouse loop
         for #+carbon-compat, also had to do the same for draw-doc
         changed *popup-mouse-documentation?* back to :unroll for  #+mcl now that it seems
         to be working again...
 8/25/04 update-{tr,tl}-menu smarter about disabling items in certain contexts
 6/17/04 added flush-port-buffers to {draw,erase}-doc
10/28/03 menu-select: removed flush-port-buffer that was inside with-mouse-tracking
         changed other flush-port-buffer to force-graphics-output
 9/02/03 added #+carbon-compat (flush-port-buffer) to menu-select
         changed sleep to snooze in selection confirm flashing loop
 4/21/03 merged current LW and MCL files
10/16/01 draw,erase-doc use *popup-doc-on?*
         popup-doc-xxx functions take extra popup-only? arg and wait for popup
 5/24/01 added *popup-mouse-documentation?* to control popup documentation
 4/25/01 popup-(un)doc-xxx helper functions done
 4/10/01 popup-doc done
 4/03/01 changed popup-doc methods to used shared backing store
 3/15/01 started popup-doc implementation
 2/17/01 merged current LW and MCL files
 5/29/00 #+lispworks in *tt-menu*
 4/12/00 calls to enter methods in hostspot coms now check for entry from below
         functions are: com-hotspot-shrink/expand/supershrink/full-screen-box
12/07/99 LWW: change font in menu-item class

5/11/98 changed draw-item to use new convention for draw-cha and ccl::*gray-color*
5/11/98 started logging changes: source = boxer version 2.3

|#

(in-package :boxer)

(defclass menu-item
  ()
  ((title :initarg :title :initform "" :accessor menu-item-title)
   (action :initarg :action :initform nil)
   ;; font is an index into *boxer-pane*'s font map
   (font  :initarg :font
          :initform *default-font*
          :accessor menu-item-font)
   (enabled? :initform t)
   (checked? :initform nil))
  (:metaclass block-compile-class))

(defvar *menu-item-margin* 8)

(defclass popup-menu
  ()
  ((items :initarg :items :initform nil)
   (default-item-no :initarg :default-item-no :initform 0))
  (:metaclass block-compile-class))

;; "bubble help" used mostly for mouse documentation

(defclass popup-doc
  ()
  ((string :initarg :string :initform "" :accessor popup-doc-string))
  (:metaclass block-compile-class))



;;; menu item methods...
(defmethod set-menu-item-title ((menu menu-item) new-title)
  (setf (slot-value menu 'title) new-title))

(defmethod set-menu-item-action ((menu menu-item) new-action)
  (setf (slot-value menu 'action) new-action))

(defmethod get-menu-item-font ((menu menu-item))
  (or (slot-value menu 'font)
      *default-font*))

(defmethod item-size ((item menu-item))
  (values
   (+ (* *menu-item-margin* 2)
       (string-wid (get-menu-item-font item) (slot-value item 'title)))
   ;; leave a pixel above and below
   (+ (string-hei (get-menu-item-font item)) 2)))

(defmethod item-height ((item menu-item))
  (+ (string-hei (get-menu-item-font item)) 2))

(defvar *default-check-char* #+mcl (code-char 195) #-mcl #\o) ; mcl bullet is 165

(defmethod draw-item ((item menu-item) x y)
  (if (null (slot-value item 'enabled?))
      ;; grey it, if we can
      (with-pen-color (*gray*)
        ;; yuck why isn't gray bound somewhere
        (let ((check (slot-value item 'checked?)))
          (rebind-font-info ((get-menu-item-font item))
            (cond ((null check))
                  ((characterp check)
                   (draw-cha check (+ x 2) (+ y (cha-ascent))))
                  (t (draw-cha *default-check-char*
                               (+ x 2)  (+ y (cha-ascent)))))))
        (draw-string (get-menu-item-font item) (slot-value item 'title)
                     (+ x *menu-item-margin*) (1+ y)))
      (progn
        (with-pen-color (*black*)
          (draw-string (get-menu-item-font item) (slot-value item 'title)
                       (+ x *menu-item-margin*) (1+ y))
          (let ((check (slot-value item 'checked?)))
            (rebind-font-info ((get-menu-item-font item))
              (cond ((null check))
                    ((characterp check)
                     (draw-cha check (+ x 2)  (+ y (cha-ascent))))
                    (t (draw-cha *default-check-char*
                                 (+ x 2)  (+ y (cha-ascent))))))))))
  ;; return height used up
  (+ (string-hei (get-menu-item-font item)) 2))

(defmethod set-menu-item-check-mark ((item menu-item) check-mark)
  (setf (slot-value item 'checked?) check-mark))

(defmethod menu-item-enable ((item menu-item))
  (setf (slot-value item 'enabled?) T))

(defmethod menu-item-disable ((item menu-item))
  (setf (slot-value item 'enabled?) nil))



;;; menu methods...
(defmethod menu-items ((menu popup-menu)) (slot-value menu 'items))

(defmethod find-menu-item ((menu popup-menu) title)
  (car (member title (slot-value menu 'items)
               :test #'(lambda (a b) (string-equal a (slot-value b 'title))))))

(defmethod menu-size ((menu popup-menu))
  (let ((wid 0) (hei 0))
    (dolist (item (slot-value menu 'items))
      (multiple-value-bind (iw ih)
          (item-size item)
        (setq wid (max wid iw)
              hei (+ hei ih))))
    ;; add 2 pixels to leave room for the shadowing
    ;; and another 1 for the border
    (values (+ wid 3) (+ hei 3))))

(defmethod draw-menu ((menu popup-menu) x y)
  (multiple-value-bind (mwid mhei) (menu-size menu)
    (erase-rectangle mwid mhei x y)
    (let ((acc-y (1+ y)))
      (dolist (item (slot-value menu 'items))
        (setq acc-y (+ acc-y (draw-item item x acc-y)))))
    ;; draw the top & left borders
    (draw-rectangle (- mwid 2) 1 x y)
    (draw-rectangle 1 (- mhei 2) x y)
    ;; and the bottom & right stubs
    (draw-rectangle 4 1 x (+ y (- mhei 2)))
    (draw-rectangle 1 4 (+ x (- mwid 2)) y)
    ;; draw the shadow rects
    (draw-rectangle (- mwid 4) 2 (+ x 4) (+ y (- mhei 2)))
    (draw-rectangle 2 (- mhei 4) (+ x (- mwid 2)) (+ y 4))))

;; the check for valid local coords should have already been made
(defmethod track-item ((menu popup-menu) local-y)
  (let ((acc-y 1))
    (dolist (item (slot-value menu 'items)
                  (let* ((last (car (last (slot-value menu 'items))))
                         (last-height (item-height last)))
                    (values last (- acc-y last-height) last-height)))
      (let ((item-height (item-height item)))
        (if (<= (- local-y acc-y) item-height) ; in the item
            (return (values item acc-y item-height))
            ;; otherwise, increment and continue
            (incf acc-y item-height))))))

(defvar *select-1st-item-on-popup* t)

;; this is the main method, it pops up at (x,y) then tracks the mouse
;; funcalling the selected menu-item-action
(defmethod menu-select ((menu popup-menu) x y)
  (multiple-value-bind (mwid mhei) (menu-size menu)
    (let* ((window-width (sheet-inside-width *boxer-pane*)); what about %clip-rig?
           (window-height (sheet-inside-height *boxer-pane*)) ; %clip-bot?
           (fit-x (- (+ x mwid) window-width))
           (fit-y (- (+ y mhei) window-height))
           ;; if either fit-? vars are positive, we will be off the screen
           (real-x (if (plusp fit-x) (- window-width mwid) x))
           (real-y (if (plusp fit-y) (- window-height mhei) y))
           ;; current-item is bound out here because we'll need it after the
           ;; tracking loop exits...
           (current-item nil))
      (unless (zerop (mouse-button-state))
        ;; make sure the mouse is still down
        ;; if the original x and y aren't good, warp the mouse to the new location
        ;; a more fine tuned way is to use fit-x and fit-y to shift the current
        ;; mouse pos by the amount the menu is shifted (later...)
        (drawing-on-window (*boxer-pane*)
          (when (or *select-1st-item-on-popup* (plusp fit-x) (plusp fit-y))
            (warp-pointer *boxer-pane* (+ real-x 5) (+ real-y 5)))
          ;; now draw the menu and loop
          (unwind-protect
            (progn
              ;; draw menu
              (draw-menu menu real-x real-y)
              (force-graphics-output)
              ;; loop
              (let ((current-y 0) (current-height 0))
                (boxer-window::with-mouse-tracking ((mouse-x real-x) (mouse-y real-y))
                  (let ((local-x (- mouse-x real-x)) (local-y (- mouse-y real-y)))
                    (if (and (< 0 local-x mwid) (< 0 local-y mhei))
                      ;; this means we are IN the popup
                      (multiple-value-bind (ti iy ih)
                                           (track-item menu local-y)
                        (cond ((and (null current-item)
                                    (not (slot-value ti 'enabled?)))
                               ;; no current, selected item is disabled...
                               )
                              ((null current-item)
                               ;; 1st time into the loop, set vars and then
                               (setq current-item ti current-y iy current-height ih)
                               ;; highlight
                               (draw-menu menu real-x real-y)
                               (with-pen-color (bw::*blinker-color*)
                                 (with-blending-on
                                   (draw-rectangle (- mwid 3) ih
                                                   (1+ real-x) (+ real-y iy))))
                               (force-graphics-output))
                              ((eq ti current-item)) ; no change, do nothing
                              ((not (slot-value ti 'enabled?))
                               ;; redraw menu with nothing selected
                               (draw-menu menu real-x real-y)
                               (force-graphics-output)
                               (setq current-item nil))
                              (t ; must be a new item selected,
                               ;; redraw menu
                               (draw-menu menu real-x real-y)
                               ;; highlight selected item...
                               (with-pen-color (bw::*blinker-color*)
                                 (with-blending-on
                                   (draw-rectangle (- mwid 3) ih
                                                   (1+ real-x) (+ real-y iy))))
                               (force-graphics-output)
                               ;; set vars
                               (setq current-item ti current-y iy current-height ih))))
                      ;; we are OUT of the popup
                      (cond ((null current-item)) ; nothing already selected
                            (t ; redraw menu with nothing selected
                             (draw-menu menu real-x real-y)
                             (force-graphics-output)
                             (setq current-item nil))))))
                ;; loop is done, either we are in and item or not
                (unless (null current-item)
                  ;; if we are in an item, flash and erase the highlighting
                  (dotimes (i 5)
                    (draw-menu menu real-x real-y)
                    (force-graphics-output) (snooze .05)
                    (with-pen-color (bw::*blinker-color*)
                      (with-blending-on
                        (draw-rectangle (- mwid 3) current-height
                                        (1+ real-x) (+ real-y current-y))))
                    (force-graphics-output) (snooze .05)))))))
        ;; funcall the action (note we are OUTSIDE of the drawing-on-window
        (unless (null current-item)
          (let ((action (slot-value current-item 'action)))
            (unless (null action) (funcall action))))))))


;;; popup doc methods

(defvar *popup-mouse-documentation?* #+lwwin :unroll #+mcl T #+opengl :unroll
  "Can be NIL (no documentation), :unroll or T")


;;  Practically speaking, this means def-redisplay-initialization
;; this also implies that we need to keep track of all popup docs because if we
;; change the font, we'll have to recalculate the string widths and possibly
;; reallocate new backing stores.

;; Since there can be only one popup-doc displyed at a time, we can use a shared
;; backing store.  It just has to be as large as the biggest doc.  We also need
;; to keep track of the biggest doc, in case the font is changed, then we
;; need to reallocate a larger doc based on the new font.

;(defmethod initialize-instance ((self popup-doc) &rest initargs) )

(defmethod draw-doc ((self popup-doc) x y)
  (let* ((swid (ceiling (string-wid *popup-doc-font* (slot-value self 'string))))
         (shei (string-hei *popup-doc-font*))
         (pad (+ *popup-doc-border-width* *popup-doc-padding*))
         (full-pad (* pad 2))
         (full-wid (+ swid full-pad))
         (full-hei (+ shei full-pad)))
    ;; frame (left, top, right and bottom)
    (draw-rectangle *popup-doc-border-width* full-hei x y)
    (draw-rectangle full-wid *popup-doc-border-width* x y)
    (draw-rectangle *popup-doc-border-width* full-hei
                    (- (+ x full-wid) *popup-doc-border-width*) y)
    (draw-rectangle full-wid *popup-doc-border-width*
                    x (- (+ y full-hei) *popup-doc-border-width*))
    ;; background
    (with-pen-color (*popup-doc-color*)
      (draw-rectangle
                      (+ swid (* *popup-doc-padding* 2))
                      (+ shei (* *popup-doc-padding* 2))
                      (+ x *popup-doc-border-width*)
                      (+ y *popup-doc-border-width*)))
    ;; doc string
    (draw-string *popup-doc-font* (slot-value self 'string)
                 (+ x pad) (+ y pad)))
  ;; set the flag
  (setq *popup-doc-on?* t))

(defmethod erase-doc ((self popup-doc) x y)
  (declare (ignore x y))
  (repaint-window)
  (setq *popup-doc-on?* nil))

(defmethod doc-width ((self popup-doc))
  (+ (ceiling (string-wid *popup-doc-font* (slot-value self 'string)))
      (* (+ *popup-doc-border-width* *popup-doc-padding*) 2)))

(defmethod doc-height ((self popup-doc))
  (+ (string-hei *popup-doc-font*)
      (* (+ *popup-doc-border-width* *popup-doc-padding*) 2)))

(define-popup-doc :top-left "Shrink")
(define-popup-doc :shrunken-top-left "Supershrink")

(define-popup-doc :top-right "Expand")
(define-popup-doc :fullscreen "Fullscreen")

(define-popup-doc :bottom-right "Resize")
(define-popup-doc :bottom-right-off "Manual Sizing")

(define-popup-doc :bottom-left-g "Graphics View")
(define-popup-doc :bottom-left-t "Text View")

(define-popup-doc :type "Toggle Type")

(define-popup-doc :name "Edit Name")

(define-popup-doc :name-handle "Name")

;; unused ?
(define-popup-doc :port-target-name "Target Name")

(define-popup-doc :port-target-type "Target Type")

(define-popup-doc :scroll-bar "Scroll")

;; for shrunken boxes?
(define-popup-doc :inside "Expand")

(define-popup-doc :graphics "Graphics Click")

(define-popup-doc :sprite "Sprite Click")

;; special cases
(define-popup-doc :top-left-initial-box "Box Menu")

(define-popup-doc :top-right-outermost-box "Box Menu")


;;; Popup doc Interface routines
;; these are what get called to do the actual documentation by mouse tracking
;; in the boxwin files
;; these functions must:
;; 1) decide if we should document (e.g. toggling not always available)
;; 2) if so, then:
;;       a) popup-doc (properly offset, i.e. not obscuring mouse or hotspot)
;;       b) highlighting hotspot if appropriate
;;       c) status-line doc
;; 3) undo doc

;; should be about doc size + mouse cursor size
(defparameter *popup-doc-edge-padding* 30)

;; for popup-doc offset, prefer below and to the right of the mouse
;; exceptions being if the mouse is near the right or bottom edges of the window
(defun popup-doc-offset-coords (doc x y)
  (multiple-value-bind (w h)
      (boxer::window-inside-size *boxer-pane*)
    (cond ((> (+ *popup-doc-edge-padding* y) h)
           (values (min x (- w (doc-width doc))) (- y (doc-height doc) 10)))
          (t (values (min x (- w (doc-width doc))) (+ y 26))))))

;;;; The main utilities (called from mousedoc.lisp via {un}document-mouse-dispatch)
;; the opengl undoc dispatch erases by redrawing the screen, no
;; need for specific versions

;; top left
(defun popup-doc-shrink (screen-box &optional supershrink? popup-only?)
  (let* ((box-type (box-type screen-box))
         (doc (get-popup-doc-for-area
               (cond ((eq (screen-obj-actual-obj screen-box) *initial-box*)
                      :top-left-initial-box)
                     ((not (null  supershrink?))
                      :shrunken-top-left)
                     (t :top-left)))))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
                           (box-borders-offsets box-type screen-box)
        (let ((corner-x (+ x delta-x))
              (corner-y (+ y delta-y
                            (box-top-y (name-string-or-null
                                        (screen-obj-actual-obj screen-box))
                                        0))))
          (unless popup-only?
            (bw::set-mouse-doc-status-xy corner-x corner-y))
          ;; then (possibly) the popup doc
          (unless (or (null *popup-mouse-documentation?*)
                      (bw::popup-doc-delay))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (bw::set-mouse-doc-popup-info doc doc-x doc-y))))))))


;; top right
(defun popup-doc-expand (screen-box &optional fullscreen? popup-only?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (cond ((eq screen-box (outermost-screen-box))
                     :top-right-outermost-box)
                    ((not (null fullscreen?))
                     :fullscreen)
                    (t :top-right)))))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (declare (ignore delta-x))
        (multiple-value-bind (lef top rig bot)
            (box-borders-widths box-type screen-box)
          (declare (ignore lef top bot))
          (let ((corner-x (+ x (screen-obj-wid screen-box) (- rig)))
                (corner-y (+ y delta-y
                              ;; #+opengl
                              (box-top-y (name-string-or-null
                                          (screen-obj-actual-obj screen-box))
                                         0))))
            (unless popup-only?
              (bw::set-mouse-doc-status-xy corner-x corner-y))
            ;;
            (unless (or (null *popup-mouse-documentation?*)
                        (bw::popup-doc-delay))
              (multiple-value-bind (doc-x doc-y)
                  (popup-doc-offset-coords doc corner-x corner-y)
                (bw::set-mouse-doc-popup-info doc doc-x doc-y)))))))))


;; bottom left
(defun popup-doc-view-flip (screen-box &optional to-graphics? popup-only?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (if to-graphics? :bottom-left-g :bottom-left-t))))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (declare (ignore delta-y))
        (multiple-value-bind (lef top rig bot)
            (box-borders-widths box-type screen-box)
          (declare (ignore lef top rig))
          (let ((corner-x (+ x delta-x))
                (corner-y (+ y (screen-obj-hei screen-box) (- bot))))
            (unless popup-only?
              (bw::set-mouse-doc-status-xy corner-x corner-y))
            ;;
            (unless (or (null *popup-mouse-documentation?*)
                        (bw::popup-doc-delay))
              (multiple-value-bind (doc-x doc-y)
                  (popup-doc-offset-coords doc corner-x corner-y)
                (bw::set-mouse-doc-popup-info doc doc-x doc-y)))
            ))))))

;; bottom right

(defun popup-doc-resize (screen-box popup-only? &optional is-off?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area (if is-off? :bottom-right-off :bottom-right))))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (lef top rig bot)
          (box-borders-widths box-type screen-box)
        (declare (ignore lef top))
        (let ((corner-x (+ x (screen-obj-wid screen-box) (- rig)))
              (corner-y (+ y (screen-obj-hei screen-box) (- bot))))
          (unless popup-only?
            (bw::set-mouse-doc-status-xy corner-x corner-y))
          (unless (or (null *popup-mouse-documentation?*)
                      (bw::popup-doc-delay))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (bw::set-mouse-doc-popup-info doc doc-x doc-y))))))))

;; type tab
;; we don't light up the hotspot because it would look like the box has
;; already been toggled
(defun popup-doc-toggle-type (screen-box popup-only?)
  ;; no non popup doc but leave this here for future
  (declare (ignore popup-only?))
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area :type)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (lef top rig bot)
          (box-borders-widths box-type screen-box)
        (declare (ignore rig top))
        (let ((corner-x (+ x lef))
              (corner-y (+ y (screen-obj-hei screen-box) (- bot))))
          (unless (or (null *popup-mouse-documentation?*)
                      (bw::popup-doc-delay))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (bw::set-mouse-doc-popup-info doc doc-x doc-y))))))))

(defun popup-undoc-toggle-type (screen-box)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area :type)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (lef top rig bot)
          (box-borders-widths box-type screen-box)
        (declare (ignore rig top))
        (let ((corner-x (+& x lef))
              (corner-y (+& y (screen-obj-hei screen-box) (-& bot))))
          (unless (or (null *popup-mouse-documentation?*)
                      (null *popup-doc-on?*))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (erase-doc doc doc-x doc-y))))))))

(defun popup-doc-graphics (screen-box popup-only?)
  ;; no non popup doc but leave this here for future
  (declare (ignore popup-only?))
  (unless (or (null *popup-mouse-documentation?*)
              (bw::popup-doc-delay))
    (let ((doc (get-popup-doc-for-area :graphics)))
      (multiple-value-bind (x y)
          (xy-position screen-box)
        (multiple-value-bind (doc-x doc-y)
            (popup-doc-offset-coords doc (+ x 10) (+ y 10))
          (bw::set-mouse-doc-popup-info doc doc-x doc-y))))))

(defun popup-undoc-graphics (screen-box)
  (unless (or (null *popup-mouse-documentation?*)
              (null *popup-doc-on?*))
    (let ((doc (get-popup-doc-for-area :graphics)))
      (multiple-value-bind (x y)
          (xy-position screen-box)
        (multiple-value-bind (doc-x doc-y)
            (popup-doc-offset-coords doc (+ x 10) (+ y 10))
          (erase-doc doc doc-x doc-y))))))

;;; Hotspots
(defvar *global-hotspot-control?* t)

;; bound by popup menu tracker for the benefit of the action COMs
(defvar *hotspot-mouse-box* nil)
(defvar *hotspot-mouse-screen-box* nil)

;; flags for global control
;; sgithens: Moved to boxdef.lisp 2020-03-27
;; (defvar *top-left-hotspots-on?*     t)
;; (defvar *top-right-hotspots-on?*    t)
;; (defvar *bottom-left-hotspots-on?*  t)
;; (defvar *bottom-right-hotspots-on?* t)

(defboxer-command com-mouse-toggle-tl-hotspot (&optional (box *hotspot-mouse-box*))
  "Enables or Disables the top left Hotspot"
  (if *global-hotspot-control?*
      (setq *top-left-hotspots-on?* (not *top-left-hotspots-on?*))
      (set-top-left-hotspot-active? box (not (top-left-hotspot-active? box))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-toggle-tr-hotspot (&optional (box *hotspot-mouse-box*))
  "Enables or Disables the top right Hotspot"
  (if *global-hotspot-control?*
      (setq *top-right-hotspots-on?* (not *top-right-hotspots-on?*))
      (set-top-right-hotspot-active? box (not (top-right-hotspot-active? box))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-toggle-bl-hotspot (&optional (box *hotspot-mouse-box*))
  "Enables or Disables the bottom left Hotspot"
  (if *global-hotspot-control?*
      (setq *bottom-left-hotspots-on?* (not *bottom-left-hotspots-on?*))
      (set-bottom-left-hotspot-active? box (not (bottom-left-hotspot-active? box))))
  boxer-eval::*novalue*)

;; br hotspot ignores global flag
(defboxer-command com-mouse-toggle-br-hotspot (&optional (box *hotspot-mouse-box*))
  "Enables or Disables the Bottom Right Hotspot"
  (set-bottom-right-hotspot-active? box (not (bottom-right-hotspot-active? box)))
  boxer-eval::*novalue*)

;; shrink, expand and toggle closet
;; these are different from the vanilla mouse versions because the point is
;; not guaranteed to be in the box (although it may be, so we may need to
;; exit some boxes).
(defboxer-command com-hotspot-shrink-box (&optional (box *hotspot-mouse-box*)
                                                    (screen-box
                                                     *hotspot-mouse-screen-box*))
  "Shrink the box using the shrink hotspot"
  (let ((old-box (point-box)))
    (unless (eq old-box box)
      (send-exit-messages box screen-box)
      (enter box (not (superior? old-box box)))
      (move-point (box-first-bp-values box))
      (set-point-screen-box screen-box))
    (com-shrink-box box)))

(defboxer-command com-hotspot-supershrink-box (&optional (box *hotspot-mouse-box*)
                                                         (screen-box
                                                          *hotspot-mouse-screen-box*))
  "Supershrink the box using the shrink hotspot"
  (let ((old-box (point-box)))
    (unless (eq old-box box)
      (send-exit-messages box screen-box)
      (enter box (not (superior? old-box box)))
      (move-point (box-first-bp-values box))
      (set-point-screen-box screen-box))
    (com-super-shrink-box box)))

(defboxer-command com-hotspot-expand-box (&optional (box *hotspot-mouse-box*)
                                                    (screen-box
                                                     *hotspot-mouse-screen-box*))
  "Expand the Box using the Expand hotspot"
  (let ((old-box (point-box)))
    (unless (eq old-box box)
      (send-exit-messages box screen-box)
      (enter box (not (superior? old-box box)))
      (move-point (box-first-bp-values box))
      (set-point-screen-box screen-box))
    (com-expand-box)))

(defboxer-command com-hotspot-full-screen-box (&optional (box *hotspot-mouse-box*)
                                                         (screen-box
                                                          *hotspot-mouse-screen-box*))
  "Expand the Box to Full Screen via the Expand hotspot"
  (let ((old-box (point-box)))
    (unless (eq old-box box)
      (send-exit-messages box screen-box)
      (enter box (not (superior? old-box box)))
      (move-point (box-first-bp-values box))
      (set-point-screen-box screen-box))
    (if (and (graphics-box? box)
             (display-style-graphics-mode? (display-style-list box)))
        (com-expand-box)
        (com-set-outermost-box))))

(defboxer-command com-hotspot-normal-size-box (&optional (box *hotspot-mouse-box*)
                                                         (screen-box
                                                          *hotspot-mouse-screen-box*))
  "Set the box to it's normal size via the hotspot context menu."
  (cond ((eq box *initial-box*) t )      ; Do nothing if this is the initial box
        ((eq box (outermost-box)) ; If it's the outermost box, collapse it
          (com-collapse-box))
        ((eq (display-style box) ':shrunk) ; If it's shrunk, expand it
          (com-hotspot-expand-box box screen-box))
        ((eq (display-style box) ':supershrunk)          ; If it's supershrunk, expand it twice?
          (com-hotspot-expand-box box screen-box)  ; I don't think this state is possible at the moment
          (com-hotspot-expand-box box screen-box)) ; supershrunk boxes don't have the menu... yet! 2021-03-01
        (t t) ; Otherwise it's normal, or some other un as yet expected state
      )
  )

(defboxer-command com-hotspot-toggle-closet (&optional (box *hotspot-mouse-box*)
                                                       (screen-box
                                                        *hotspot-mouse-screen-box*))
  "Open/Close the closet from a hotspot"
  (com-toggle-closets box screen-box))

(defboxer-command com-hotspot-toggle-graphics (&optional (box *hotspot-mouse-box*))
  "Flip from/to graphics presentation via the bottom left hotspot"
  (com-toggle-box-view box))

(defun top-left-hotspot-on? (edbox)
  (or (and *global-hotspot-control?* *top-left-hotspots-on?*)
      (and (not *global-hotspot-control?*)
           (top-left-hotspot-active? edbox))))

(defboxer-command com-change-box-to-doit (&optional (box *hotspot-mouse-box*))
  "Changes the box to a Doit box. If it's already a doit box it should remain so."
  (set-type box 'doit-box))

(defboxer-command com-change-box-to-data (&optional (box *hotspot-mouse-box*))
  "Changes the box to a Doit box. If it's already a doit box it should remain so."
  (set-type box 'data-box))

;;
;;  Re-usable menu with:
;;    Fullscreen
;;    Normal
;;    Shrunk
;;    Supershrunk
;;    -----------
;;    Open Closet
;;    Properties
;;

(defvar *boxsize-closet-properties-popup-menu*
  (make-instance 'popup-menu
                    :items (list  (make-instance 'menu-item
                                    :title "Fullscreen"
                                    :action 'com-hotspot-full-screen-box)
                                  (make-instance 'menu-item
                                    :title "Normal"
                                    :action 'com-hotspot-normal-size-box )    ;   'com-hotspot-expand-box)
                                  (make-instance 'menu-item
                                    :title "Shrunk"
                                    :action 'com-hotspot-shrink-box)
                                  (make-instance 'menu-item
                                    :title "Supershrunk"
                                    :action 'com-hotspot-supershrink-box)
                                  (make-instance 'menu-item
                                    :title "-----------")
                                  (make-instance 'menu-item
                                    :title "Open Closet"
                                    :action 'com-hotspot-toggle-closet)
                                  (make-instance 'menu-item
                                    :title "Properties"
                                    :action 'com-edit-box-properties)
                                    )))

(defun update-boxsize-closet-properties-menu (box)
  (let ((fullscreen-item  (car    (menu-items *boxsize-closet-properties-popup-menu*)))
        (normal-item      (cadr   (menu-items *boxsize-closet-properties-popup-menu*)))
        (shrink-item      (caddr  (menu-items *boxsize-closet-properties-popup-menu*)))
        (supershrink-item (cadddr (menu-items *boxsize-closet-properties-popup-menu*)))
        (closet-item      (nth 5  (menu-items *boxsize-closet-properties-popup-menu*)))
        (properties-item  (nth 6  (menu-items *boxsize-closet-properties-popup-menu*))))
    (set-menu-item-title fullscreen-item "Fullscreen")
    (menu-item-enable fullscreen-item)
    (set-menu-item-title normal-item "Normal")
    (set-menu-item-title shrink-item "Shrunk")

    (if (and (graphics-box? box) (display-style-graphics-mode? (display-style-list box)))
      (menu-item-disable fullscreen-item))

    (cond ((eq box (outermost-box))
          ;;  (menu-item-enable fullscreen-item)
           (set-menu-item-title fullscreen-item "- Fullscreen"))
          ((eq (display-style box) ':normal)
           (set-menu-item-title normal-item "- Normal"))
          ((eq (display-style box) ':shrunk)
           (set-menu-item-title shrink-item "- Shrunk"))
          (t
           nil))

    (let ((closet-row (slot-value box 'closets)))
          (if (or (null closet-row) (null (row-row-no box closet-row)))
              ;; either there's no closet or it's currently closed...
              (set-menu-item-title closet-item "Open Closet")
              (set-menu-item-title closet-item "Close Closet")))
  ))

;;
;; Re-usable menu with:
;;   Text Side
;;   Graphics Side
;;   -------------
;;   Data
;;   Doit

(defvar *box-types-popup-menu*
  (make-instance 'popup-menu
                   :items (list (make-instance 'menu-item
                                    :title "Text Side"
                                    :action nil) ; 'com-hotspot-toggle-graphics)
                                (make-instance 'menu-item
                                    :title "Graphics Side"
                                    :action nil) ;'com-hotspot-toggle-graphics)
                                (make-instance 'menu-item
                                    :title "-------------"
                                    :action nil)
                                (make-instance 'menu-item
                                    :title "Data"
                                    :action 'com-change-box-to-data)
                                (make-instance 'menu-item
                                    :title "Doit"
                                    :action 'com-change-box-to-doit)
                                    )))

(defun update-box-types-menu (box)
  (let ((text-side-item      (car   (menu-items *box-types-popup-menu*)))
        (graphics-side-item  (cadr   (menu-items *box-types-popup-menu*)))
        (data-item  (nth 3   (menu-items *box-types-popup-menu*)))
        (doit-item  (nth 4   (menu-items *box-types-popup-menu*)))
        )
    (set-menu-item-title text-side-item "Text Side")
    (set-menu-item-action text-side-item nil)
    (set-menu-item-title graphics-side-item "Graphics Side")
    (set-menu-item-action graphics-side-item nil)
    (menu-item-enable graphics-side-item)
    (set-menu-item-title data-item "Data")
    (set-menu-item-title doit-item "Doit")

    (cond ((data-box? box)
           (set-menu-item-title data-item "- Data")
           )
          ((doit-box? box)
           (set-menu-item-title doit-item "- Doit")
           ))

    ;; Currently, the only command for this is to 'toggle', not explicity set the state.
    ;; So we're setting the toggle action on the state not currently in vogue, and setting
    ;; the menu item in the state to nil.
    (cond ((not (graphics-box? box))
           ;; nothing for now, perhaps later we can add a dialog ADD graphics
           (menu-item-disable graphics-side-item)
           (set-menu-item-title text-side-item "- Text Side"))
          ((display-style-graphics-mode? (display-style-list box))
           (set-menu-item-title graphics-side-item "- Graphics Side")
           (set-menu-item-action text-side-item 'com-hotspot-toggle-graphics))
          (t (set-menu-item-title text-side-item "- Text Side")
             (set-menu-item-action graphics-side-item 'com-hotspot-toggle-graphics)))
  )
)

(defun top-right-hotspot-on? (edbox)
  (or (and *global-hotspot-control?* *top-right-hotspots-on?*)
      (and (not *global-hotspot-control?*) (top-right-hotspot-active? edbox))))

(defboxer-command com-mouse-boxsize-closet-properties-pop-up (&optional (window *boxer-pane*)
                                       (x (bw::boxer-pane-mouse-x))
                                       (y (bw::boxer-pane-mouse-y))
                                       (mouse-bp
                                        (mouse-position-values x y))
                                       (click-only? t))
  "Pop up a box attribute menu"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region) (reset-editor-numeric-arg)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box))
         (swid (screen-obj-wid screen-box))
         (edbox (screen-obj-actual-obj screen-box))
         (*hotspot-mouse-box* edbox)
         (*hotspot-mouse-screen-box* screen-box))
    (if (and (not click-only?)
             (mouse-still-down-after-pause? 0)) ; maybe *mouse-action-pause-time* ?
        (multiple-value-bind (left top right)
            (box-borders-widths box-type screen-box)
          (declare (ignore left))
          ;; will probably have to fudge this for type tags near the edges of
          ;; the screen-especially the bottom and right edges
          (multiple-value-bind (abs-x abs-y) (xy-position screen-box)
            (update-boxsize-closet-properties-menu edbox)
            ;; the coms in the pop up rely on this variable
            ;; (menu-select *tr-popup* (- (+ abs-x swid) right) (+ abs-y top))
            (menu-select *boxsize-closet-properties-popup-menu* x y)
            ))
        ))
  boxer-eval::*novalue*)

(defun bottom-left-hotspot-on? (edbox)
  (or (and *global-hotspot-control?* *bottom-left-hotspots-on?*)
      (and (not *global-hotspot-control?*) (bottom-left-hotspot-active? edbox))))

(defboxer-command com-mouse-box-types-pop-up (&optional (window *boxer-pane*)
                                       (x (bw::boxer-pane-mouse-x))
                                       (y (bw::boxer-pane-mouse-y))
                                       (mouse-bp
                                        (mouse-position-values x y))
                                       (click-only? t))
  "Pop up a box attribute menu"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region) (reset-editor-numeric-arg)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box))
         (shei (screen-obj-hei screen-box))
         (edbox (screen-obj-actual-obj screen-box))
         (*hotspot-mouse-box* edbox)
         (*hotspot-mouse-screen-box* screen-box))
    (if (and (not click-only?)
             (mouse-still-down-after-pause? 0)) ; maybe *mouse-action-pause-time* ?
        (multiple-value-bind (left top right bottom)
            (box-borders-widths box-type screen-box)
          (declare (ignore top right))
          ;; will probably have to fudge this for type tags near the edges of
          ;; the screen-especially the bottom and right edges
          (multiple-value-bind (abs-x abs-y) (xy-position screen-box)
            (update-box-types-menu edbox)
            ;; the coms in the pop up rely on this variable
            (menu-select *box-types-popup-menu* x y
                        ;;  (+ abs-x left) (- (+ abs-y shei) bottom)
                         )
                         ))
        ;; for simple clicks we do the action (unless it is disabled)
        (when (bottom-left-hotspot-on? edbox)
          (com-hotspot-toggle-graphics edbox)))))

(defun com-hotspot-unfix-box-size (&optional (box *hotspot-mouse-box*))
  (com-unfix-box-size box))

;; THings are setup so that this menu appears ONLY if the hotspot is enabled
;; indicating that auto box sizing is active.  If manual box sizing is active
;; we go straight to com-mouse-resize-box
(defvar *br-popup* (make-instance 'popup-menu
                     :items (list (make-instance 'menu-item
                                    :title "Automatic Box Size"
                                    :action 'com-hotspot-unfix-box-size)
                                  (make-instance 'menu-item
                                    :title "Manual Box Size"
                                    :action 'com-mouse-toggle-br-hotspot))))

(defun update-br-menu (box)
  (declare (ignore box))
  (let ((auto-item    (car  (menu-items *br-popup*)))
        (manual-item  (cadr (menu-items *br-popup*))))
    ;; We only see the menu if the spot is "off", an "on" spot means resize
    ;; make sure "auto" is greyed out
    (menu-item-disable  auto-item)
    (menu-item-enable manual-item)))

;; NOTE: bottom right corner NEVER (currently) checks the global var, only
;; the local box flag is used...
;; active hotspot is interpreted to mean resizable
(defboxer-command com-mouse-br-pop-up (&optional (window *boxer-pane*)
                                       (x (bw::boxer-pane-mouse-x))
                                       (y (bw::boxer-pane-mouse-y))
                                       (mouse-bp
                                       (mouse-position-values x y))
                                       (click-only? t))
  "Pop up a box attribute menu"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region) (reset-editor-numeric-arg)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box))
         (swid (screen-obj-wid screen-box))
         (shei (screen-obj-hei screen-box))
         (edbox (screen-obj-actual-obj screen-box)))
    (cond ((bottom-right-hotspot-active? edbox)
           (if (or click-only? (not (mouse-still-down-after-pause? 0)))
               ;; hotspot is on, but we only got a click, shortcut to restore menu
               (progn
                 (set-fixed-size edbox nil nil)
                 (set-scroll-to-actual-row screen-box nil)
                 (modified edbox)
                 ;; turn the hotspot off so the menu will pop up next time
                 (set-bottom-right-hotspot-active? edbox nil))
               ;; otherwise, do the normal resize stuff
               (com-mouse-resize-box window x y mouse-bp click-only?)))
          (t ;; otherwise, update and present a menu
           (multiple-value-bind (left top right bottom)
               (box-borders-widths box-type screen-box)
             (declare (ignore left top))
             ;; will probably have to fudge this for type tags near the edges of
             ;; the screen-especially the bottom and right edges
             (multiple-value-bind (abs-x abs-y) (xy-position screen-box)
               (update-br-menu edbox)
               ;; the coms in the pop up rely on this variable
               (let ((*hotspot-mouse-box* edbox)
                     (*hotspot-mouse-screen-box* screen-box))
                 (menu-select *br-popup*
                              (- (+ abs-x swid) right)
                              (- (+ abs-y shei) bottom))))))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-br-resize-box (&optional (window *boxer-pane*)
                                       (x (bw::boxer-pane-mouse-x))
                                       (y (bw::boxer-pane-mouse-y))
                                       (mouse-bp
                                       (mouse-position-values x y))
                                       (click-only? t))
  "Pop up a box attribute menu"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region) (reset-editor-numeric-arg)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box))
         (swid (screen-obj-wid screen-box))
         (shei (screen-obj-hei screen-box))
         (edbox (screen-obj-actual-obj screen-box)))
    (cond (t (bottom-right-hotspot-active? edbox)
           (if (or click-only? (not (mouse-still-down-after-pause? 0)))
               ;; hotspot is on, but we only got a click, shortcut to restore menu
               (progn
                 (set-fixed-size edbox nil nil)
                 (set-scroll-to-actual-row screen-box nil)
                 (modified edbox)
                 ;; turn the hotspot off so the menu will pop up next time
                 (set-bottom-right-hotspot-active? edbox nil))
               ;; otherwise, do the normal resize stuff
               (com-mouse-resize-box window x y mouse-bp click-only?)))
          (t ;; otherwise, update and present a menu
            nil
          ;;  (multiple-value-bind (left top right bottom)
          ;;      (box-borders-widths box-type screen-box)
          ;;    (declare (ignore left top))
          ;;    ;; will probably have to fudge this for type tags near the edges of
          ;;    ;; the screen-especially the bottom and right edges
          ;;    (multiple-value-bind (abs-x abs-y) (xy-position screen-box)
          ;;      (update-br-menu edbox)
          ;;      ;; the coms in the pop up rely on this variable
          ;;      (let ((*hotspot-mouse-box* edbox)
          ;;            (*hotspot-mouse-screen-box* screen-box))
          ;;        (menu-select *br-popup*
          ;;                     (- (+ abs-x swid) right)
          ;;                     (- (+ abs-y shei) bottom)))))
                              )))
  boxer-eval::*novalue*)
