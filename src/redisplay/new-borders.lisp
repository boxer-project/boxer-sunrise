;;;;  ;-*- Mode:Lisp; Package:boxer; Syntax: Common-Lisp; -*-
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
;;;;
;;;;                                           +-Data--+
;;;;                  This file is part of the | Boxer | System
;;;;                                           +-------+
;;;;
;;;;
;;;;   The (new opengl port) Code for handling the box borders is here.
;;;;
;;;;  box-borders-widths
;;;;  box-borders-minimum-size
;;;;  box-borders-draw
;;;;
;;;;  super-shrunk-size
;;;;  draw-super-shrunk-box
;;;;
;;;;
;;;;  ToDo:name truncation
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   4/19/14 draw-borders-label, border-label-protrusion =>moved label string up 1 pixel
;;;;  11/22/12 draw-super-shrunk-box, fine tuned data box case
;;;;   8/29/12 multiline2i => multiline2, undo all the special fixnum arithmetic in:
;;;;           border-label-protrusion,  border-name-{width,height,protrusion},
;;;;           port-name-protrusion, draw-borders-{name,label,closet-info},  draw-port-name,
;;;;           type-tab-tracking-info,  border-thickness,
;;;;           plain-borders-{minimum-size-1,widths-1,draw-1,name-tab-values}
;;;;           port-borders-{widths,minimum-size}
;;;;           port-frame-widths,  box-top-y,
;;;;           draw-{round-corners,slant-corners,super-shrunk-box,port-frame}, port-borders-draw,
;;;;           visible-corner-size, {tl,tr,bl,br}-corner-tracking-info,  name-tab-tracking-info,
;;;;           box-borders-name-tab-position, get-position-in-border,
;;;;           draw-mouse-{shrink,expand,toggle,resize}-corner,
;;;;           All of the mouse border handlers because screen-obj x,y,wid,hei can now be floats
;;;;
;;;;   8/11/12 handle string-wid float returned value: border-{label,name}-width
;;;;   6/23/23 handle string-wid float returned value: plain-borders-minimum-size-1,
;;;;                                                   draw-borders-name, draw-borders-label
;;;;   3/ 7/11 #+opengl version of default-character-height & tuned
;;;;           outline in draw-supershrunk-box
;;;;  12/ 5/10 popup doc support added to repaint-mouse-docs
;;;;  11/28/09 draw-borders-closet-info added
;;;;   5/11/09 get-position-in-border now returns :scroll-bar for horizontal scroll bar clicks as well
;;;;   8/11/08 get-position-in-border uses *no-label-mouse-tracking-width* to track click on label space when
;;;;           type labels are hidden
;;;;

(in-package :boxer)

;; Constants (defvar's for now, change to defconstant when we are happy)
(defvar *border-outside-space* 1  ; or 2 or 3
  "Amount of space from the border to the limit of the box")

(defvar *border-inside-space* 5 ; 7 (should be an odd number)
  "Amount of space from the border to the inside of the box")

(defvar *basic-border-width* 1) ; +1 for file boxes...

(defvar *border-left-margin* 1) ; *border-inside-space*

(defvar *border-label-font* nil)
(defvar *border-name-font*  nil)

(defvar *border-name-padding* 1 "whitespace between name string & name border")

(defvar *border-name-slant-offset* 3)

(defvar *noname-extension* 1)
(defvar *noname-blank-width* 7)

(defvar *port-box-gap* 2) ; distance between the port frame and the target frame

(defvar *port-name-indent* 5)

(defvar *transparent-border-stipple-pattern* #xAAAA)
(defvar *transparent-border-stipple-factor* 3)


(defvar *no-label-mouse-tracking-width* 10
  "Amount of space to allocate for mouse tracking of border type labels when the lable is hidden")

(defun border-thickness (border-style)
  (if (fast-memq border-style '(:thick :thick-dashed))
      (+ *basic-border-width* 1)
    *basic-border-width*))

(defun border-label-width (label)
 (if *show-border-type-labels*
     (string-wid *border-label-font* label)
   0))

(defun border-label-height (box-type)
  (declare (ignore box-type))
  (if *show-border-type-labels*
      (string-hei *border-label-font*)
    0))

;; "protrusion" describes the extension beyond where the border would
;; otherwise be
(defun border-label-protrusion ()
  (if *show-border-type-labels*
      (1- (ffloor (string-hei *border-label-font*) 2))
    0))

(defun border-name-width (name)
  (cond ((null name)
         (+ *noname-blank-width*
             (* 2 *basic-border-width*)
             (* 2 *border-name-padding*)
             (* 2 *border-name-slant-offset*)))
        (t
         (+ (string-wid *box-border-name-font-no* name)
            (* 2 *basic-border-width*)
            (* 2 *border-name-padding*)
            (* 2 *border-name-slant-offset*)))))

(defun border-name-height (name)
  (cond ((null name) (+ *noname-extension* *border-inside-space*))
        (t (+ (string-hei *box-border-name-font-no*)
               (* *basic-border-width* 2)
               ;(*& *border-name-padding* 2)
               ))))

;; "protrusion" describes the extension beyond where the border would
;; otherwise be
(defun border-name-protrusion (name)
  (cond ((null name) *noname-extension*)
        (t (- (border-name-height name)
               ;; name is inset by this amount...
               *border-inside-space*))))

(defun port-name-protrusion (name)
  (cond ((null name) *noname-extension*)
        (t (+ (string-hei *box-border-name-font-no*)
               (* *basic-border-width* 2)
               (* *border-name-padding* 2)
               ;; name is inset by this amount...
               (- *port-box-gap*)))))

;;;; Minimum Size

;;; stub version => 40,25 or 52,37 for ports
;(defun box-borders-minimum-size (box-type screen-box)
;  (declare (ignore screen-box))
;  (case box-type
;    (port-box (values 52 37))
;    (t (values 40 25))))

(defun box-borders-minimum-size (box-type screen-box)
  (case box-type
    (port-box (port-borders-minimum-size screen-box))
    (t (plain-borders-minimum-size screen-box))))

(defun plain-borders-minimum-size (screen-box)
  (let ((box (screen-obj-actual-obj screen-box)))
    (plain-borders-minimum-size-1 (name-string-or-null box)
                                  (box-type-label box)
                                  (display-style-border-style (display-style-list
                                                               screen-box)))))

(defun plain-borders-minimum-size-1 (name type-label border-style)
  (let ((border-thickness (border-thickness border-style)))
    (values (max *minimum-box-wid*
                 (+ (* 2 (+ *border-outside-space* *border-inside-space*
                               border-thickness))
                     (ceiling (border-name-width name)))
                 (+ (* 2 (+ *border-outside-space* *border-inside-space*
                               border-thickness))
                     (border-label-width type-label)))
            (max *minimum-box-hei*
                 (+ (+ *border-outside-space* (border-name-protrusion name)
                         border-thickness *border-inside-space*)
                     ;; the top border's thickness and
                     ;; the bottom border's thickness..
                     (+ *border-outside-space* (border-label-protrusion)
                         *border-inside-space*))))))

(defun port-borders-minimum-size (screen-box)
  ;; 1st get the inside dimensions
  (let* ((port (screen-obj-actual-obj screen-box))
         (target (ports port))
         (p-border-style (display-style-border-style (display-style-list port)))
         (p-border-thickness (border-thickness p-border-style)))
    (multiple-value-bind (t-min-wid t-min-hei)
        (cond ((null target) (values 0 0))
              (t (plain-borders-minimum-size-1 (name-string-or-null target)
                                               (class-name (class-of target))
                                               (display-style-border-style
                                                (display-style-list target)))))
      (values (max *minimum-box-wid*
                   (+ t-min-wid
                      (* 2 (+ *port-box-gap* *border-outside-space*
                              p-border-thickness))))
              (max *minimum-box-hei*
                   (+ t-min-hei
                      (+ *border-outside-space*
                         (border-name-protrusion (name-string-or-null port))
                         p-border-thickness *port-box-gap*)
                      (+ *border-outside-space*
                         (border-label-protrusion)
                         *port-box-gap*)))))))

;;;; Border widths

;;; stub version ==> 10,10,10,10 or 16,16,16,16 for ports
;(defun box-borders-widths (box-type screen-box)
;  (declare (ignore screen-box))
;  (case box-type
;    (port-box (values 16 16 16 16))
;    (t (values 10 10 10 10))))

(defun box-borders-widths (box-type screen-box)
  (case box-type
    (port-box (port-borders-widths screen-box))
    (t (plain-borders-widths screen-box))))

(defun plain-borders-widths (screen-box)
  (let ((box (screen-obj-actual-obj screen-box)))
    (plain-borders-widths-1 (name-string-or-null box) (class-name (class-of box))
                            (display-style-border-style (display-style-list
                                                         screen-box)))))

;; values left, top, right bottom
(defun plain-borders-widths-1 (name type border-style)
  (let* ((border-thickness (border-thickness border-style))
         (vert-width (+ *border-outside-space* *border-inside-space*
                        border-thickness)))
    (values vert-width ; left
            (+ *border-outside-space* (border-name-protrusion name) ; top
               border-thickness *border-inside-space*)
            vert-width ; right
            (floor (+ *border-outside-space* (border-label-protrusion) ;; sgithens TODO boxer-sunrise-22 debugging fixnum issues
               border-thickness *border-inside-space*)))))

(defun port-borders-widths (screen-box)
  (if (screen-obj-actual-obj screen-box)
    (let* ((port (screen-obj-actual-obj screen-box))
         (target (ports port))
         (p-border-style (display-style-border-style (display-style-list port)))
         (p-border-thickness (border-thickness p-border-style)))
    (multiple-value-bind (t-lef t-top t-rig t-bot)
        (cond ((null target) (values 0 0 0 0))
              (t (plain-borders-widths-1 (name-string-or-null target)
                                         (class-name (class-of target))
                                         (display-style-border-style
                                          (display-style-list target)))))
      (values (+ t-lef *port-box-gap* p-border-thickness *border-outside-space*)
              (+ t-top *port-box-gap* (port-name-protrusion
                                       (name-string-or-null port))
                 p-border-thickness *border-outside-space*)
              (+ t-rig *port-box-gap* p-border-thickness *border-outside-space*)
              (+ t-bot *port-box-gap* p-border-thickness
                 (border-label-protrusion) *border-outside-space*))))
    0))

(defun port-frame-widths (port)
  (let* ((p-border-style (display-style-border-style (display-style-list port)))
         (p-border-thickness (border-thickness p-border-style)))
    (values (+ *port-box-gap* p-border-thickness *border-outside-space*)
            (+ *port-box-gap* (port-name-protrusion (name-string-or-null port))
               p-border-thickness *border-outside-space*)
            (+ *port-box-gap* p-border-thickness *border-outside-space*)
            (+ *port-box-gap* p-border-thickness
               (border-label-protrusion) *border-outside-space*))))



;;;; Box Borders Draw
;;; this is now just a plain drawing of the entire border - no more box borders
;;; caching
;;; we will also draw the scroll info here (perhaps &optional scrollpos args to
;;; handle interactive mouse scrolling ?)
;;; Drawing Borders are divided into:
;;;  1) walls (thick, thin, dashed, colored?)
;;;  2) type info - corners, label
;;;  3) name
;;;  4) scrolling - hor, vert arrows & slides

;; look in plist for now, probably not a good idea to use display-style-lists
;; unless colored borders become commonplace

(defun get-border-color (box) (getprop box 'border-color *default-border-color*))

;; hook for colored borders (fold transparency in here too ? (via line stipple))
(defmacro with-border-drawing-styles ((actual-obj) &body body)
  `(with-pen-color ((get-border-color ,actual-obj))
     . ,body))

(defun box-borders-draw (box-type screen-box)
  (case box-type
    (port-box (port-borders-draw screen-box))
    (t (plain-borders-draw screen-box))))

;; this is drawn in a transformed coord system...
(defun plain-borders-draw (screen-box)
  (plain-borders-draw-1 (screen-obj-actual-obj screen-box)
                        0 ;(screen-obj-x-offset screen-box)
                        0 ;(screen-obj-y-offset screen-box)
                        (screen-obj-wid screen-box)
                        (screen-obj-hei screen-box)))

(defun box-top-y (name y)
  (if (null name)
      (+ y *border-outside-space* *noname-extension*)
    (+ y (border-name-protrusion name) *border-outside-space*)))

(defvar *show-empty-name-rows* t
  "Determines whether or not we render the name tabs on boxes when their names are zero length.
  (ie. They don't have a name.)")

(defun plain-borders-draw-1 (actual-obj x y outer-wid outer-hei)
  (let* ((name (name-string-or-null actual-obj) )
         (box-type (class-name (class-of actual-obj)))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         ;; important info that is not in an instance var
         (name-top (+ y *border-outside-space*))
         ;; where the name tab begins vertically
         (box-top (box-top-y name y))
         ;; where the box walls begins vertically
         (box-left (+ x *border-outside-space*))
         ;; where the box walls proper begin horizontally
         (corner-size (+ border-thickness *border-inside-space*))
         ;; space that a corner takes up
         (box-right (- (+ x outer-wid) border-thickness *border-outside-space*))
         (box-bottom (- (+ y outer-hei)
                        border-thickness (border-label-protrusion)
                        *border-outside-space*))
         (inner-left (+ box-left corner-size))
         (inner-top (+ box-top corner-size))
         (inner-right (- box-right *border-inside-space*))
         (inner-bottom (- box-bottom *border-inside-space*))
         (name-end-x 0) ; this will get set later
         (label-end-x 0)) ; this 2
    (with-border-drawing-styles (actual-obj)
      ;; We only render the name rows if the preference for rendering them is true, or they aren't empty,
      ;; or the cursor is currently in the name row.
      (if (or *show-empty-name-rows*
              (> (length name) 0)
              (and (eq actual-obj (screen-obj-actual-obj (cadddr *point*)))
                   (name-row? (cadr *point*))))
        (setq name-end-x (draw-borders-name name inner-left box-top name-top))
        (setq name-end-x inner-left))
      (setq label-end-x (draw-borders-label (box-type-label actual-obj) inner-left box-bottom))
      (draw-borders-walls box-left box-right inner-top inner-bottom box-top
                          box-bottom name-end-x label-end-x inner-right
                          border-style border-thickness)
      ;(draw-borders-scrolling-info inner-top inner-right inner-bottom label-end-x)
      ;; draw-corners form is last so it can return offset coords for use by
      ;; port strut drawing routines
      (draw-borders-closet-info actual-obj name-end-x inner-right box-top inner-top border-thickness)
      (draw-borders-corners box-type border-thickness box-left inner-left box-top
                            inner-top box-right inner-right box-bottom inner-bottom)
      )))

(defun draw-borders-closet-info (actual-obj name-end-x inner-right
                                            box-top inner-top border-thickness)
  ;; should adjust if there is a visible closet-row
  (let ((cr (slot-value actual-obj 'closets))
        (top-y (+ box-top border-thickness)))
    (unless (null cr)
      (with-pen-color (*closet-color*)
        (draw-rectangle (- inner-right name-end-x) (- inner-top top-y 1)
                        name-end-x top-y)))))

(defun draw-borders-name (name name-x name-mid-y name-top-y)
  (let ((current-x name-x)
        (inside-name-width (if (null name)
                               *noname-blank-width*
                             (string-wid *border-name-font* name)))
        (lower-slant-y (+ name-mid-y *border-inside-space*)))
    (let ((end-x (+ current-x *border-name-slant-offset*)))
      ;; the left "<" of the name
      (draw-line current-x name-mid-y end-x name-top-y)
      (draw-line current-x name-mid-y end-x lower-slant-y)
      (setq current-x end-x))
    (let ((end-x (+ current-x inside-name-width (* *border-name-padding* 2))))
      ;; the name string itself
      (unless (null name)
        (draw-string *border-name-font* name current-x
                     (+ name-top-y *border-name-padding* *basic-border-width*)))
      ;; the top & bottom
      (draw-line current-x name-top-y end-x name-top-y)
      (draw-line current-x lower-slant-y end-x lower-slant-y)
      (setq current-x end-x))
    (let ((end-x (+ current-x *border-name-slant-offset*)))
      ;; the right ">"
      (draw-line current-x name-top-y end-x name-mid-y)
      (draw-line current-x lower-slant-y end-x name-mid-y)
      end-x)))

(defun draw-borders-label (label inner-left box-bottom)
  (cond ((null *show-border-type-labels*)
         inner-left)
        (t
           (draw-string *border-label-font* label
                        inner-left (- box-bottom
                                       (1+ (ffloor (string-hei *border-label-font*) 2))))
           (+ inner-left (string-wid *border-label-font* label)))))

;; dashed and thick has to be handled @ this level because we
;; don't want the name border to be dashed or thick...
(defun draw-borders-walls (box-left box-right inner-top inner-bottom ; 4 vert sides
                          box-top box-bottom name-end-x label-end-x inner-right
                          border-style border-thickness)
  (with-pen-size (border-thickness)
    (cond ((fast-memq border-style '(:dashed :thick-dashed))
           (with-line-stippling (*transparent-border-stipple-pattern*
                                 *transparent-border-stipple-factor*)
             (draw-borders-walls-1 box-left box-right inner-top inner-bottom
                                   box-top box-bottom name-end-x label-end-x
                                   inner-right)))
          (t
           (draw-borders-walls-1 box-left box-right inner-top inner-bottom box-top
                                 box-bottom name-end-x label-end-x inner-right)))))

;; basic wall drawing function, thickness and stippling are handled via
;; various "with-" macros
(defun draw-borders-walls-1 (box-left box-right inner-top inner-bottom ; 4 vert sides
                                      box-top box-bottom name-end-x label-end-x
                                      inner-right)
  ;; left
  (draw-line box-left inner-top box-left inner-bottom)
  ;; top
  (draw-line name-end-x box-top inner-right box-top)
  ;; right
  (draw-line box-right inner-top box-right inner-bottom)
  ;; bottom
  (draw-line label-end-x box-bottom inner-right box-bottom))

(defun draw-borders-corners (box-type border-thickness
                                      box-left inner-left box-top inner-top
                                      box-right inner-right box-bottom inner-bottom)
  (ecase box-type
    (doit-box (draw-square-corners box-left inner-left box-top inner-top
                                   box-right inner-right box-bottom inner-bottom
                                   border-thickness))
    (data-box (draw-round-corners box-left inner-left box-top inner-top
                                  box-right inner-right box-bottom inner-bottom
                                  border-thickness))))

;; all of the corner drawing routines should return 4 values to help
;; port drawing routines to connect the struts...
(defun draw-square-corners (box-left inner-left box-top inner-top
                                     box-right inner-right box-bottom inner-bottom
                                     border-thickness)
  (with-pen-size (border-thickness)
    (multiline2 box-left inner-top box-left box-top inner-left box-top) ; top right
    (multiline2 inner-right box-top box-right box-top box-right inner-top) ;top left
    (multiline2 inner-right box-bottom box-right box-bottom box-right inner-bottom)
    (multiline2 box-left inner-bottom box-left box-bottom inner-left box-bottom))
  (values box-left box-top box-right box-bottom))

;;; we approximate quarter round with 3 line segments...
(defun draw-round-corners (box-left inner-left box-top inner-top
                                    box-right inner-right box-bottom inner-bottom
                                    border-thickness)
  (let ((bl+2 (+ box-left 2))   (il-3 (- inner-left 3))
        (bt+2 (+ box-top 2))    (it-3 (- inner-top 3))
        (br-2 (- box-right 2))  (ir+3 (+ inner-right 3))
        (bb-2 (- box-bottom 2)) (ib+3 (+ inner-bottom 3)))
    (with-pen-size (border-thickness)
      (multiline2 box-left inner-top bl+2 it-3 il-3 bt+2 inner-left box-top) ; TL
      (multiline2 inner-right box-top ir+3 bt+2 br-2 it-3 box-right inner-top) ;TR
      (multiline2 inner-right box-bottom
                  ir+3 bb-2 br-2 ib+3 box-right inner-bottom) ; Bottom Right
      (multiline2 box-left inner-bottom
                  bl+2 ib+3 il-3 bb-2 inner-left box-bottom))) ; Bottom Left
  (values (+ box-left 3) (+ box-top 3) (- box-right 3) (- box-bottom 3)))

(defun draw-slant-corners (box-left inner-left box-top inner-top
                                    box-right inner-right box-bottom inner-bottom
                                    border-thickness)
  (with-pen-size (border-thickness)
    (draw-line box-left inner-top inner-left box-top) ; top left
    (draw-line inner-right box-top box-right inner-top) ; top right
    (draw-line inner-right box-bottom box-right inner-bottom);bottom right
    (draw-line box-left inner-bottom inner-left box-bottom)) ; bot left
  (values (+ box-left 3) (+ box-top 3) (- box-right 3) (- box-bottom 3)))

(defun port-borders-draw (screen-box)
  (let* ((port (screen-obj-actual-obj screen-box))
         (target (ports port))
         (px 0) ;(screen-obj-x-offset screen-box))
         (py 0) ;(screen-obj-y-offset screen-box))
         (pw (screen-obj-wid screen-box))
         (ph (screen-obj-hei screen-box)))
    (multiple-value-bind (pfleft pftop pfright pfbottom)
        (port-frame-widths port)
      (multiple-value-bind (i-strut-left i-strut-top i-strut-right i-strut-bottom)
          (plain-borders-draw-1 target pfleft pftop
                                (- pw pfleft pfright) (- ph pftop pfbottom))
        ;; then draw the port frame, then
        (draw-port-frame port px py pw ph
                         i-strut-left i-strut-top i-strut-right i-strut-bottom)))))

(defun draw-port-frame (port x y wid hei
                             i-strut-left i-strut-top i-strut-right i-strut-bottom)
  (let* ((name (name-string-or-null port))
         (border-style (display-style-border-style (display-style-list port)))
         (border-thickness (border-thickness border-style))
         ;; important info that is not in an instance var
         (name-top (+ y *border-outside-space*))
         ;; where the name tab begins vertically
         (box-top (if (null name)
                      (+ name-top *noname-extension*)
                    (+ y (port-name-protrusion name) *border-outside-space*)))
         ;; where the box walls begins vertically
         (box-left (+ *border-outside-space*))
         (box-right (- (+ x wid) border-thickness *border-outside-space*))
         (box-bottom (- (+ y hei) border-thickness *border-outside-space*))
         (name-start-x (+ box-left *port-name-indent*))
         (name-end-x 0)) ; this will get set later
    (with-border-drawing-styles (port)
      ;; name
      (setq name-end-x
            (draw-port-name name name-start-x box-top name-top))
      ;; walls
      (with-pen-size (border-thickness)
        (draw-line box-left box-top box-left box-bottom) ; left
        (draw-line box-left box-top name-start-x box-top) ; top
        (draw-line name-end-x box-top box-right box-top) ; top, after name
        (draw-line box-right box-top box-right box-bottom) ; right
        (draw-line box-left box-bottom box-right box-bottom)) ; bottom
      ;; struts
      (draw-line box-left box-top i-strut-left i-strut-top)
      (draw-line box-right box-top i-strut-right i-strut-top)
      (draw-line box-right box-bottom i-strut-right i-strut-bottom)
      (draw-line box-left box-bottom i-strut-left i-strut-bottom))))

(defun draw-port-name (name name-x name-mid-y name-top-y)
  (let ((current-x name-x)
        (inside-name-width (if (null name)
                               *noname-blank-width*
                             (string-wid *border-name-font* name)))
        (lower-slant-y (+ name-mid-y *border-inside-space*)))
    (let ((end-x (+ current-x *border-name-slant-offset*)))
      ;; the left "<" of the name
      (draw-line current-x name-mid-y end-x name-top-y)
      (draw-line current-x name-mid-y end-x lower-slant-y)
      (setq current-x end-x))
    (let ((end-x (+ current-x inside-name-width (* *border-name-padding* 2))))
      ;; the name string itself
      (unless (null name)
        (draw-string *border-name-font* name current-x
                     (+ name-top-y *border-name-padding* *basic-border-width*)))
      ;; the top & bottom
      (draw-line current-x name-top-y end-x name-top-y)
      (draw-line current-x lower-slant-y end-x lower-slant-y)
      (setq current-x end-x))
    (let ((end-x (+ current-x *border-name-slant-offset*)))
      ;; the right ">"
      (draw-line current-x name-top-y end-x name-mid-y)
      (draw-line current-x lower-slant-y end-x name-mid-y)
      end-x)))


;; this is only used for mouse tracking in the border so we are deliberately
;; liberal in the values returned...
;; returns (values n-min-x n-min-y n-max-x n-max-y is-there-a-name?)
(defun box-borders-name-tab-values (box-type screen-box)
  (case box-type
    (port-box (plain-borders-name-tab-values screen-box)) ; close enuff for now...
    (t (plain-borders-name-tab-values screen-box))))

(defun plain-borders-name-tab-values (screen-box)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         (vert-width (+ *border-outside-space* *border-inside-space*
                        border-thickness))
         (name (name-string-or-null actual-obj))
         (name-width (border-name-width name)))
    (values vert-width
            *border-outside-space*
            (+ vert-width name-width)
            (+ *border-outside-space* (border-name-height name))
            name)))

;; odds and ends used in mouse-corner-tracking in coms-mouse.lisp &
(defun box-borders-offsets (box-type screen-box)
  (declare (ignore box-type screen-box))
  (values *border-outside-space* *border-outside-space*))

(defun visible-corner-size (screen-box)
  (+ (border-thickness (display-style-border-style
                        (screen-obj-actual-obj screen-box)))
     *border-inside-space*))

;; Tracking-Info functions....
;; these return x, y, width, height for use with track-mouse-area
;; x, y is relative to the TL corner of the box
;; there should be a tracking-info function for every mouseable part of a box's borders

(defun tl-corner-tracking-info (screen-box)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (name (name-string-or-null actual-obj))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         ;; important info that is not in an instance var
         ;(name-top *border-outside-space*)
         ;; where the name tab begins vertically
         (box-top (box-top-y name 0))
         ;; where the box walls begins vertically
         (box-left *border-outside-space*)
         ;; where the box walls proper begin horizontally
         (corner-size (+ border-thickness *border-inside-space*)))
    (values box-left box-top corner-size corner-size)))

(defun tr-corner-tracking-info (screen-box)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (name (name-string-or-null actual-obj))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         ;; important info that is not in an instance var
         ;(name-top *border-outside-space*)
         ;; where the name tab begins vertically
         (box-top (box-top-y name 0))
         (box-right (- (screen-obj-wid screen-box)
                        border-thickness *border-outside-space*))
         ;; where the box walls proper begin horizontally
         (corner-size (+ border-thickness *border-inside-space*))
         (inner-right (- box-right *border-inside-space*)))
    (values inner-right box-top corner-size corner-size)))

(defun br-corner-tracking-info (screen-box)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (box-type (class-name (class-of actual-obj)))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         (box-right (- (screen-obj-wid screen-box)
                       border-thickness *border-outside-space*))
         (box-bottom (- (screen-obj-hei screen-box)
                        border-thickness (border-label-protrusion)
                        *border-outside-space*))
         (corner-size (+ border-thickness *border-inside-space*))
         (inner-right (- box-right *border-inside-space*))
         (inner-bottom (- box-bottom *border-inside-space*)))
    (values inner-right inner-bottom corner-size corner-size)))

(defun bl-corner-tracking-info (screen-box)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (box-type (class-name (class-of actual-obj)))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         (box-left *border-outside-space*)
         (box-bottom (- (screen-obj-hei screen-box)
                        border-thickness (border-label-protrusion)
                        *border-outside-space*))
         (corner-size (+ border-thickness *border-inside-space*))
         (inner-bottom (- box-bottom *border-inside-space*)))
    (values box-left inner-bottom corner-size corner-size)))

(defun type-tab-tracking-info (screen-box)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         (label (box-type-label actual-obj))
         (box-bottom (- (screen-obj-hei screen-box)
                          border-thickness (border-label-protrusion)
                          *border-outside-space*))
         (corner-size (+ border-thickness *border-inside-space*))
         (inner-left (+ *border-outside-space* corner-size)))
    (values inner-left (- box-bottom (ffloor (string-hei *border-label-font*) 2))
            (string-wid *border-label-font* label)
            (string-hei *border-label-font*))))

(defun name-tab-tracking-info (screen-box)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         (corner-size (+ border-thickness *border-inside-space*))
         (inner-left (+ *border-outside-space* corner-size)))
    (values inner-left *border-outside-space*
            (+ *noname-blank-width* (* 2 *border-name-slant-offset*)
               (* 2 *basic-border-width*))
            (+ corner-size *noname-extension*))))


;;; GUI interface
;;; these draw the particular GUI element when the mouse is over the corner
;;; drawing opens the possibility of supersizing interface elements for novices
;;; how about interface color ?
(defun toggle-corner-fun (x y wid hei)
  (let ((rx (- (+ x wid) 2)) (by (- (+ y hei) 2)))
    (with-pen-color (*border-gui-color*)
      (draw-line (+ x 2) y rx y) ; top arrow horizontal
      (draw-line rx y rx by) ; top arrow right
      (draw-line (+ x 1) (+ by 1) (- rx 1) (+ by 1)) ; bottom arrow horizontal
      (draw-line (+ x 1) (+ y 2) (+ x 1) (+ by 1)) ; bottom arrow left
      (draw-line x (+ y 3) (+ x 3) (+ y 3)) ; cross the bottom arrow head
      (draw-line (- rx 1) (- by 2) (+ rx 2) (- by 2))))) ; cross the top arrow head

(defun shrink-corner-fun (x y wid hei)
  (let ((fx (1- x)) (fy (1- y)) (lx (+ x wid)) (ly (+ y hei))
        (tsize (floor (min wid hei) 2)))
    (with-pen-color (*border-gui-color*)
      (draw-poly (list (cons fx (+ fy tsize)) (cons (+ fx tsize) fy)  ;; top left
                                (cons (+ fx tsize) (+ fy tsize))))
      (draw-poly (list (cons (+ fx tsize 1) fy) (cons (+ fx tsize 1) (+ fy tsize)) ; top right
                                (cons lx (+ fy tsize))))
      (draw-poly (list (cons fx (+ fy tsize 1)) (cons (+ fx tsize) (+ fy tsize 1)) ; bottom left
                                (cons (+ fx tsize) ly)))
      (draw-poly (list (cons (+ fx tsize 1) (+ fy tsize 1)) (cons lx (+ fy tsize 1)) ; bottom right
                                (cons (+ fx tsize 1) (1+ ly)))))))

(defun expand-corner-fun (x y wid hei)
  (let ((lx (+ x wid)) (ly (+ y hei))
        (tside (floor (min wid hei) 2)))
    (with-pen-color (*border-gui-color*)
      (draw-poly (list (cons x y) (cons (+ x tside) y) (cons x (+ y tside)))) ; top left
      (draw-poly (list (cons (- lx tside) y) (cons lx y) (cons lx (+ y tside)))) ; top right
      (draw-poly (list (cons x ly) (cons x (- ly tside)) (cons (+ x tside) ly))) ; bottom left
      (draw-poly (list (cons (- lx tside) ly) (cons lx ly) (cons lx (- ly tside))))))) ; bottom R


(defun resize-corner-fun (x y wid hei)
  (default-gui-fun x y wid hei))

(defun default-gui-fun (x y wid hei)
  (with-pen-color (*border-gui-color*)
    (draw-rectangle wid hei x y)))



;;; supershrunk stuff
;;; just draw the 4 corners & surround with a box?

(defun default-character-height ()
  (rebind-font-info (*default-font*) (cha-hei)))

(defun super-shrunk-size ()
  (let ((size (default-character-height)))
    (values size size)))

(defun draw-super-shrunk-box (x y box-type)
  (multiple-value-bind (wid hei)
      (super-shrunk-size)
    (let ((startx (+ x 1)) (starty (+ y 1))(endx (+ x wid -1)) (endy (+ y hei -2)))
      (flet ((outer-box () (multiline2 startx starty endx starty endx endy
                                       startx endy startx starty)))
        (ecase box-type
          (doit-box (outer-box)
                    (let ((insx (+ startx 2)) (insy (+ starty 2))
                          (inex (- endx 2)) (iney (- endy 2)))
                      (multiline2 insx insy inex insy inex iney insx iney insx insy)))
          (data-box (outer-box)
                    (draw-rectangle (- wid 7) (- hei 7)(+ startx 2) (+ starty 2)))
          (port-box (outer-box)
                    (draw-line startx starty endx endy)
                    (draw-line startx endy endx starty)))))))




;; used by the redisplay when the *point* is in a name tab
(defun box-borders-name-tab-position (screen-box x y cha-no)
  (let* ((actual-obj (screen-obj-actual-obj screen-box))
         (name (name-string-or-null actual-obj))
         (border-style (display-style-border-style (display-style-list actual-obj)))
         (border-thickness (border-thickness border-style))
         (corner-size (+ border-thickness *border-inside-space*))
         (x-pos (+ x *border-outside-space* corner-size
                   *border-name-slant-offset* *basic-border-width*)))
    (rebind-font-info (*border-name-font*)
      (dotimes (i cha-no) (incf x-pos (cha-wid (char name i)))))
    (values x-pos
            (+ y *border-outside-space* *border-name-padding* *basic-border-width*))))

;;;; Mousing around
;;;; Mouse support

(defvar *mouse-border-areas* nil
  "A list of keywords describing mouse-sensitive portions of the box border")

;; this is used in keydef-high
(defun defined-mouse-border-areas ()
  *mouse-border-areas*)

(defmacro define-border-mouse-handler (name bp-values-handler)
  `(progn
     (unless (member ',name *mouse-border-areas*)
       (push ',name *mouse-border-areas*))
     (setf (get ',name 'mouse-bp-values-handler) ,bp-values-handler)))


;; mouse clicks on empty file boxes use this to trigger a read of the box
(defun insure-box-contents-for-click (box)
  (cond ((and (port-box? box)
              (box? (ports box))
              (null (slot-value (ports box) 'first-inferior-row)))
         ;; fill the port target
         (fill-box-from-server (ports box)))
        ((and (not (port-box? box)) (null (slot-value box 'first-inferior-row)))
         ;; fill the vanilla box
         (fill-box-from-server box))))


;;; mouse-bp-values-handlers

;;; ":inside" means that the mouse has been tracked inside a box but that
;;; a specific row, cha-no position could not be determined
(define-border-mouse-handler :inside
  #'(lambda (actual-obj x y screen-obj)
      (declare (ignore ignore))
      (insure-box-contents-for-click actual-obj)
      (if (null (first-inferior-row actual-obj))
          (multiple-value-bind (row cha-no)
              (box-self-bp-values actual-obj)
            (let* ((ssr (screen-row screen-obj))
                   (ssb (unless (null ssr) (screen-box ssr))))
              (if (screen-box? ssb)
                  (if (> x (/ (screen-obj-wid screen-obj) 2))
                      (values row (1+ cha-no) ssb
                              (+ (screen-obj-x-offset ssr)
                                 (screen-obj-x-offset ssb)
                                 (screen-obj-wid screen-obj) x)
                              (+ (screen-obj-y-offset ssr)
                                 (screen-obj-y-offset ssb) y))
                    (values row cha-no ssb (+ (screen-obj-x-offset ssr)
                                              (screen-obj-x-offset ssb) x)
                            (+ (screen-obj-y-offset ssr)
                               (screen-obj-y-offset ssb) y)))
                (box-first-bp-values actual-obj))))
        (box-first-bp-values actual-obj))))

(define-border-mouse-handler :outside
    #'(lambda (actual-obj x y screen-obj)
        (if (eq actual-obj *initial-box*)
            (box-first-bp-values *initial-box*)
          (multiple-value-bind (row cha-no)
              (box-self-bp-values actual-obj)
            (let* ((ssr (screen-row screen-obj))
                   (ssb (unless (null ssr) (screen-box ssr))))
              (if (screen-box? ssb)
      (if (> x (/ (screen-obj-wid screen-obj) 2))
                      (values row (1+ cha-no) ssb
                              (+ (screen-obj-x-offset ssr)
                                 (screen-obj-x-offset ssb)
                                 (screen-obj-wid screen-obj) x)
                              (+ (screen-obj-y-offset ssr)
                                 (screen-obj-y-offset ssb) y))
        (values row cha-no ssb (+ (screen-obj-x-offset ssr)
                                              (screen-obj-x-offset ssb) x)
          (+ (screen-obj-y-offset ssr)
                               (screen-obj-y-offset ssb) y)))
                (box-first-bp-values actual-obj)))))))

;; if the *point* is already in the box being moused, return *point*'s
;; values otherwise, the begining of the box
(defun mouse-box-values (mouse-box mouse-screen-box)
  (if (and (eq mouse-screen-box (point-screen-box)) (eq mouse-box (point-box)))
      (bp-values *point*)
      (box-first-bp-values mouse-box)))

(define-border-mouse-handler :top-left
    #'(lambda (actual-obj x y screen-obj)
  (declare (ignore x y))
        (insure-box-contents-for-click actual-obj)
  (mouse-box-values actual-obj screen-obj)))

(define-border-mouse-handler :top-right
    #'(lambda (actual-obj x y screen-obj)
  (declare (ignore x y))
        (insure-box-contents-for-click actual-obj)
        (mouse-box-values actual-obj screen-obj)))

(define-border-mouse-handler :bottom-left
    #'(lambda (actual-obj x y screen-obj)
  (declare (ignore x y))
        (insure-box-contents-for-click actual-obj)
        (mouse-box-values actual-obj screen-obj)))

(define-border-mouse-handler :bottom-right
    #'(lambda (actual-obj x y screen-obj)
  (declare (ignore x y))
        (insure-box-contents-for-click actual-obj)
        (mouse-box-values actual-obj screen-obj)))

(define-border-mouse-handler :type
    #'(lambda (actual-obj x y screen-obj)
  (declare (ignore x y))
        (insure-box-contents-for-click actual-obj)
        (mouse-box-values actual-obj screen-obj)))

(define-border-mouse-handler :name
  #'(lambda (actual-obj x y screen-box)
            ;;(insure-box-contents-for-click actual-obj)
            (let ((nr (name-row actual-obj)))
              (cond ((null nr)
                     ;; If the nr is nil, it almost certainly means that it's the name-row
                     ;; for the top level WORLD box, which users cannot rename.
                     (boxer-editor-error "You cannot name the outermost box")
                     (funcall (get :inside 'mouse-bp-values-handler) actual-obj x y screen-box))
                (t (values nr
                           (get-cha-no-in-name-row
                            (- x (box-borders-name-tab-values (box-type screen-box)
                                                              screen-box))
                            (chas nr))))))))

;;; in OpenGL, the name "handles" refer to concrete space
(define-border-mouse-handler :name-handle
  #'(lambda (actual-obj &rest ignore)
      (declare (ignore ignore))
      (insure-box-contents-for-click actual-obj)
      (box-first-bp-values actual-obj)))

(define-border-mouse-handler :port-target-name
  #'(lambda (actual-obj &rest ignore)
      (declare (ignore ignore))
      (insure-box-contents-for-click actual-obj)
      (box-first-bp-values (ports actual-obj))))

(define-border-mouse-handler :port-target-type
  #'(lambda (actual-obj &rest ignore)
      (declare(ignore ignore))
      (insure-box-contents-for-click actual-obj)
      (box-first-bp-values (ports actual-obj))))

(define-border-mouse-handler :scroll-bar
    #'(lambda (actual-obj x y screen-box)
  (declare (ignore x y))
  (values (or (scroll-to-actual-row screen-box)
        (first-inferior-row actual-obj))
    0)))


;; this is called when the mouse code determines that the x,y position
;; is NOT within the inferiors of the box.  It should return either NIL,
;; indicating that it doesn't seem to be in the borders either or else a
;; symbol with a defined MOUSE-BP-VALUES-HANDLER, or else ':try-again
;; which allows the calling function more leeway in searching for screen-rows

(defvar *border-retry-margin* 3)

(defmethod get-position-in-border ((self screen-box) x y)
  (with-slots (wid hei actual-obj) self
    (let ((box-type (class-name (class-of actual-obj))))
      (multiple-value-bind (left top right bottom)
          (box-borders-widths box-type self)
        (let ((inner-right  (- wid right)) (inner-bottom (- hei bottom)))
          (cond ((<= 0.0 y top)
                 ;; somewhere in the top border....
                 (multiple-value-bind (n-min-x n-min-y n-max-x n-max-y name?)
                     (box-borders-name-tab-values box-type self)
                   (cond ((<= x left) :top-left)
                         ((>= x inner-right) :top-right)
                         ((and (<= n-min-y y n-max-y)
                               (<= n-min-x x n-max-x))
                          (if (null name?) ':name-handle ':name))
                         ((> y *border-retry-margin*) :try-again)
                         (t ':outside))))
                ((> y inner-bottom)
                 ;; somewhere in teh bottom border....
                 (cond ((<= x left) :bottom-left)
                       ((>= x inner-right) :bottom-right)
                       ((<= left x
                            ;; this perhaps ought to call string-wid of the
                            ;; type label instead of using a variable
                            (+ left (max (border-label-width (box-type-label actual-obj))
                                         *no-label-mouse-tracking-width*)))
                        :type)
                       ((and (h-scrollable? self) ; horizontal scroll bar area
                             (not (dont-show-scroll-buttons? self)))
                        :scroll-bar)
                       ((< y (- hei *border-retry-margin*)) :try-again)
                       (t ':inside)))
                ;; This tracks both the (possible) vertical & horizontal scroll bars
                ;; more fine grained tracking is done in the editor command, this is done
                ;; avoid a proliferation of mouse scrolll tracking command names
                ;; e.g. mouse-click-on-HORIZONTAL-scroll-bar-LEFT-BUTTON
                ((and (> x inner-right)
                      ;; somewhere in the right border (corners have alrady been checked)
                      (v-scrollable? self)
                      (not (dont-show-scroll-buttons? self)))
                 :scroll-bar)
                ((or (<= x 2) (>= x (- wid 2))
                     (>= y (- hei 2)))
                 ':outside)
                (t ':try-again)))))))

(defmethod get-position-in-border ((self graphics-screen-box) x y)
  (let ((1st-try (call-next-method)))
    (if (eq 1st-try :try-again)
  ;; graphics boxes shouldn't try again, if you don't get it
  ;; the first time, then assume it is outside
  ':outside
  1st-try)))

;;; not very modular, need to fix this
(defun get-cha-no-in-name-row (x list-of-chas)
  (let ((cha-no 0)
  (acc-wid 0))
    (rebind-font-info (*border-name-font*)
      (dolist (screen-cha list-of-chas (length list-of-chas))
  (incf acc-wid (cha-wid screen-cha))
  (incf cha-no)
  (when (>= acc-wid x)
    (return (1- cha-no)))))))

(defvar *zoom-step-pause-time* 0
  "Seconds to pause between each step in zooming a box.
   Default is 0 meaning zoom as fast as the machine will go.")

(defvar *show-border-type-labels* t)



;;; mouse doc utilities
;;; here because the original non OpenGL versions were in caching-box-corners.lisp

;; do not have to be limited by the actual size of the corner
(defvar *mouse-corner-highlight-size* 10)

;; elevator style
(defun draw-mouse-shrink-corner (x-in y-in)
  "Currently two arrows facing each other, on a yellow circle with black border."
  (let* ((x (- x-in (/ *mouse-corner-highlight-size* 3)))
         (y (- y-in (/ *mouse-corner-highlight-size* 3)))
         (half-x (+ x (floor *mouse-corner-highlight-size* 2)))
         (half-y (+ y (floor *mouse-corner-highlight-size* 2)))
         (full-x (+ x *mouse-corner-highlight-size*))
         (full-y (+ y *mouse-corner-highlight-size*)))
    (with-pen-color (*mouse-shrink-corner--background-color*)
      (draw-circle half-x half-y (* *mouse-corner-highlight-size* 0.7)  t))
    (with-pen-color (*mouse-doc-highlight-color*)
      (draw-circle half-x half-y (* *mouse-corner-highlight-size* 0.7)  nil)
      (draw-poly (list (cons x y) (cons half-x half-y)
                                (cons x full-y) (cons x y)))
      (draw-poly (list (cons full-x y) (cons full-x full-y)
                                (cons half-x half-y) (cons full-x y)))
      )))

(defun draw-mouse-expand-corner (x-in y-in)
  "Currently two arrows facing away from each other, on a green circle with a black border."
  (let* ((x (- x-in (/ *mouse-corner-highlight-size* 3)))
         (y (- y-in (/ *mouse-corner-highlight-size* 3)))
         (half-x (+ x (/ *mouse-corner-highlight-size* 2)))
         (half-y (+ y (/ *mouse-corner-highlight-size* 2)))
         (full-x (+ x *mouse-corner-highlight-size*))
         (full-y (+ y *mouse-corner-highlight-size*)))
    (with-pen-color (*green*)
      (draw-circle half-x half-y (* *mouse-corner-highlight-size* 0.7)  t))
    (with-pen-color (*mouse-doc-highlight-color*)
      (draw-circle half-x half-y (* *mouse-corner-highlight-size* 0.7)  nil)
      (draw-poly (list (cons x half-y) (cons (- half-x 1) y)
                                (cons (- half-x 1) full-y) (cons x half-y)))
      (draw-poly (list (cons (+ half-x 1) y) (cons full-x half-y)
                                (cons (+ half-x 1) full-y) (cons (+ half-x 1) y))))))

(defun draw-mouse-toggle-corner (x y)
  (with-pen-color (*mouse-doc-highlight-color*)
    ;; top arrow
    (multiline2 (+ x 1) y
                 (- (+ x *mouse-corner-highlight-size*) 2) y
                 (- (+ x *mouse-corner-highlight-size*) 2)
                 (- (+ y *mouse-corner-highlight-size*) 3))
    ;; bottom arrow
    (multiline2 (+ x 1) (+ y 2)
                 (+ x 1) (- (+ y *mouse-corner-highlight-size*) 2)
                 (- (+ x *mouse-corner-highlight-size*) 3)
                 (- (+ y *mouse-corner-highlight-size*) 2))
    ;; bottom arrow head
    (draw-line x (+ y 4) (+ x 3) (+ y 4))
    ;; top arrow head
    (draw-line (+ x *mouse-corner-highlight-size*)
               (- (+ y *mouse-corner-highlight-size*) 4)
               (- (+ x *mouse-corner-highlight-size*) 3)
               (- (+ y *mouse-corner-highlight-size*) 4))))

(defun draw-mouse-resize-corner (x-in y-in)
  "Currently a set of arrows pointing NW to SE on a white circle with black border."
  (let* ((x (- x-in (/ *mouse-corner-highlight-size* 3)))
        (y (- y-in (/ *mouse-corner-highlight-size* 3)))
        (half-x (+ x (/ *mouse-corner-highlight-size* 2)))
        (half-y (+ y (/ *mouse-corner-highlight-size* 2)))
        (full-x (+ x *mouse-corner-highlight-size*))
        (full-y (+ y *mouse-corner-highlight-size*))
        (half (/ *mouse-corner-highlight-size* 2)))
    (with-pen-color (*white*)
      (draw-circle half-x half-y (* *mouse-corner-highlight-size* 0.7)  t))
    (with-pen-color (*mouse-doc-highlight-color*)
      (draw-circle half-x half-y (* *mouse-corner-highlight-size* 0.7)  nil)
      ;; TL arrowhead
      (draw-poly (list (cons x y) (cons (+ x half) y)
                                (cons x (+ y half)) (cons x y)))
      ;; BR arrowhead
      (draw-poly (list (cons full-x full-y) (cons (- full-x half) full-y)
                                (cons full-x (- full-y half)) (cons full-x full-y)))
      (draw-line x y full-x full-y))))

;;; OpenGL just adds things to be redrawn during regular redisplay

;; called from boxer::repaint-window
(defun repaint-mouse-docs ()
  (unless (null (bw::mouse-doc-status-x))
    ;; should be null if corner highlighting is disabled
    (let ((corner-x (bw::mouse-doc-status-x))
          (corner-y (bw::mouse-doc-status-y))
          (popup-doc (bw::mouse-doc-status-popup-doc)))
      (case (bw::mouse-doc-status-place)
        (:top-left     (draw-mouse-shrink-corner corner-x corner-y))
        (:top-right    (draw-mouse-expand-corner corner-x corner-y))
        (:bottom-left  (draw-mouse-toggle-corner corner-x corner-y))
        (:bottom-right (draw-mouse-resize-corner corner-x corner-y)))
      (unless (null popup-doc)
        (draw-doc popup-doc
                  (bw::mouse-doc-status-popup-x)
                  (bw::mouse-doc-status-popup-y))))))

(defmacro with-hilited-box ((box) &body body)
  "Any operations inside this body will happen with box being hilighted. The default style is usually
  a subtle blue background extending slightly beyond the borders. A typical use case of this is for
  hilighting a box while it's being saved."
  (let ((screen-box (gensym))
        (screen-box-x (gensym))   (screen-box-y (gensym))
        (screen-box-wid (gensym)) (screen-box-hei (gensym)))
    `(drawing-on-window (*boxer-pane*)
                        (let* ((,screen-box (or (car (displayed-screen-objs ,box))
                                                (when (superior? (outermost-box) ,box)
                                                  (outermost-screen-box))))
                               ,screen-box-wid ,screen-box-hei)
                          (multiple-value-bind (,screen-box-x ,screen-box-y)
                                               (when (screen-box? ,screen-box)
                                                 (setq ,screen-box-wid (screen-obj-wid ,screen-box)
                                                        ,screen-box-hei (screen-obj-hei ,screen-box))
                                                 (xy-position ,screen-box))
                                               (unwind-protect
                                                (progn
                                                 (unless (null ,screen-box-x)
                                                   (with-pen-color (bw::*blinker-color*)
                                                     (box::with-blending-on
                                                      (draw-rectangle
                                                                      ,screen-box-wid ,screen-box-hei
                                                                      ,screen-box-x ,screen-box-y)))
                                                   (swap-graphics-buffers))
                                                 . ,body)
                                                (unless (null ,screen-box-x)
                                                  (repaint)
                                                  (swap-graphics-buffers))))))))
