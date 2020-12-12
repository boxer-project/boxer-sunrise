;;;;-*- Mode:Lisp; Package:boxer; Syntax: Common-Lisp; -*-
;;;;
;;;;      Boxer
;;;;      Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
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
;;;;         The Code for handling the mouse is here
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   9/13/12 compiler warning free
;;;;   9/10/12 de-fixnum: position-in-screen-obj, get-cha-no, find-bp-values, get-area-of-box,
;;;;                      mouse-documentation-area, mouse-doc-place, screen-obj-at, xy-context
;;;;                      in-screen-box?, screen-offset->cha-no
;;;;   2/15/03 merged current LW and MCL files
;;;;   2/16/01 merged current LW and MCL files
;;;;  12/07/00 changed the :bottom clause in mouse-position-screen-row-values for better
;;;;           mouse tracking.  Also the row method of find-bp-values
;;;;  12/07/00 Started logging changes: source = Boxer version 2.4.1
;;;;

(in-package :boxer)

(defvar *mouse-bp* (make-bp ':fixed))

(defsubst visible-name-row? (screen-box)
  (and (name-row (screen-obj-actual-obj screen-box))
       (not (eq (outermost-screen-box) screen-box))))

;;; old
;(defsubst screen-boxes-in-row (screen-row)
;  (subset #'screen-box? (inferiors screen-row)))

(defun position-in-screen-obj? (x y screen-obj &optional (strict? t))
  (if strict?
    (and (inclusive-between? y 0 (screen-obj-hei screen-obj))
         (or (screen-row? screen-obj)
             (inclusive-between? x 0 (screen-obj-wid screen-obj))))
    ;; if not strict, ignore X and only check lower bounds on the
    ;; screen-row because we will be iterating downward
    (<= y (screen-obj-hei screen-obj))))

(defun find-inf-screen-box-in-sup-screen-row (x y screen-chas)
  (do-vector-contents (screen-box screen-chas)
    (when (screen-box? screen-box)
      (let ((relative-x (- x (screen-obj-x-offset screen-box)))
            (relative-y (- y (screen-obj-y-offset screen-box))))
        (when (position-in-screen-obj? relative-x relative-y screen-box)
          (return screen-box))))))

(defun get-cha-no (x screen-chas &optional fds)
  (let ((cha-no 0)
        (acc-wid 0))
    (or (with-font-hacking (fds)
          (do-vector-contents (screen-cha screen-chas :index-var-name s-cha-no)
            (check-and-handle-font-changes s-cha-no)
            (let ((current-width (screen-object-width screen-cha)))
              (incf acc-wid current-width)
              (incf cha-no)
              (when (>= acc-wid (+ x (/ current-width 2)))
                (return (1-& cha-no))))))
        ;; either we find it inside the row or else
        ;; we assume it's beyond the last character in the row
        (storage-vector-active-length screen-chas))))

(defmethod find-bp-values ((self screen-row) superior-x superior-y
                                             &optional (window *boxer-pane*))
  (declare (ignore window))
  (with-slots (x-offset y-offset hei actual-obj screen-chas screen-box)
    self
    (let* ((x (- superior-x x-offset))
           (y (- superior-y y-offset))
           (within-box (find-inf-screen-box-in-sup-screen-row
                        x y screen-chas)))
      (if (null within-box)
        (values actual-obj (if (> y hei)
                             ;; hack the below last row case, if below,
                             ;; return last cha-no
                             (screen-chas-length self)
                             (get-cha-no x screen-chas (row-fds actual-obj)))
                screen-box superior-x superior-y)
        (find-bp-values within-box x y)))))

;; this should either return a screen-row or else a symbol
;; we define bp-values handlers on these symbols
(defmethod get-area-of-box ((self screen-box) x y)
  "Returns the part of the box which (X, Y) is pointing to which can
   be a SCREEN-ROW, or one of the following keywords :NAME, :UNDERNAME,
   :LAST or NIL if (X, Y) is not inside a portion of the box. "
  (with-slots (wid hei screen-rows) self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths (box-type self) self)
                         (cond ((let ((display-style (display-style self)))
                                  (or (eq display-style :supershrunk)
                                      ;; test for boxtop...
                                      (and (eq display-style ':shrunk)
                                           (not (eq self *outermost-screen-box*))
                                           (boxtop (screen-obj-actual-obj self)))
                                      (and (eq display-style ':shrunk)
                                           (inclusive-between? x il (- wid ir))
                                           (inclusive-between? y (/ it 2) (- hei ib)))))
                                :inside)
                           ((and (inclusive-between? x il (- wid ir))
                                 (inclusive-between? y it (- hei ib)))
                            ;; pointing to main area of box (where the screen rows are)
                            (if (and (storage-vector? (slot-value self 'screen-rows))
                                     (not (zerop (storage-vector-active-length screen-rows))))
                              (find-inf-screen-row-in-sup-screen-box
                               (- x (slot-value self 'scroll-x-offset))
                               (- y (slot-value self 'scroll-y-offset)) screen-rows)
                              ':inside))
                           (t (let ((border-area (get-position-in-border self x y)))
                                (if (eq border-area :try-again)
                                  ;; no reasonable border area was found so try
                                  ;; for screen-rows again
                                  (or (find-inf-screen-row-in-sup-screen-box
                                       (- x (slot-value self 'scroll-x-offset))
                                       (- y (slot-value self 'scroll-y-offset))
                                       screen-rows nil)
                                      ':inside)
                                  border-area)))))))

(defun find-inf-screen-row-in-sup-screen-box (x y screen-rows
                                                &optional (strict? t))
  (unless (box-ellipsis-style? screen-rows)
    (or
     (do-vector-contents (screen-row screen-rows)
       (let ((relative-x (- x (screen-obj-x-offset screen-row)))
             (relative-y (- y (screen-obj-y-offset screen-row))))
         (when (position-in-screen-obj? relative-x relative-y
                                        screen-row strict?)
           (return screen-row))))
     ;; it's either going to find it or else, we assume its the last one
     (let ((infs-length (storage-vector-active-length screen-rows)))
       (if (zerop& infs-length)
         ':inside
         (sv-nth (1-& infs-length) screen-rows))))))

(defvar default-mouse-bp-values-handler
  #'(lambda (actual-obj &rest ignore)
            (declare (ignore ignore))
            (box-first-bp-values actual-obj)))

(defmethod find-bp-values ((self screen-box) superior-x superior-y
                                             &optional (window *boxer-pane*))
  (with-slots (x-offset y-offset actual-obj screen-row superior-screen-box)
    self
    (let* ((x (- superior-x x-offset))
           (y (- superior-y y-offset))
           (within-area (get-area-of-box self x y)))
      (cond ((and (eq self (outermost-screen-box window)) (null within-area))
             (multiple-value-bind (row cha-no)
                                  (box-first-bp-values actual-obj)
                                  (values row cha-no self x y)))
        ((null within-area)
         (multiple-value-bind (row cha-no)
                              (box-self-bp-values actual-obj)
                              (values row cha-no superior-screen-box
                                      (+ (screen-obj-x-offset screen-row) superior-x)
                                      (+ (screen-obj-y-offset screen-row) superior-y))))
        ((screen-row? within-area)
         (find-bp-values within-area
                         (- x (slot-value self 'scroll-x-offset))
                         (- y (slot-value self 'scroll-y-offset))))
        ((symbolp within-area)
         (let ((mouse-bp-values-handler (get within-area
                                             'mouse-bp-values-handler)))
           (if (null mouse-bp-values-handler)
             (funcall default-mouse-bp-values-handler
                      actual-obj x y self)
             (multiple-value-bind (row cha-no new-screen-box newx newy)
                                  (funcall mouse-bp-values-handler actual-obj x y self)
                                  (values row cha-no (or new-screen-box self)
                                          (or newx x) (or newy y) within-area)))))
        (t (error "Can't find a place in ~A for position ~D, ~D"
                  self x y))))))

;;; tracking inside of graphics boxes...

(define-border-mouse-handler :graphics #'(lambda (actual-obj &rest ignore)
                                                 (declare (ignore ignore))
                                                 (box-first-bp-values actual-obj)))

(define-border-mouse-handler :sprite #'(lambda (actual-obj &rest ignore)
                                               (declare (ignore ignore))
                                               (box-first-bp-values actual-obj)))

(defmethod get-area-of-box ((self graphics-screen-box) x y)
  (multiple-value-bind (il it ir ib)
                       (box-borders-widths (box-type self) self)
                       (let ((display-style (display-style self)))
                         (cond ((or (eq display-style :supershrunk)
                                    (and (eq display-style ':shrunk)
                                         (inclusive-between? x il (- (slot-value self 'wid) ir))
                                         (inclusive-between? y (/ it 2)
                                                             (- (slot-value self 'hei) ib)))
                                    (and (eq display-style ':shrunk)
                                         (not (eq self *outermost-screen-box*))
                                         (boxtop (screen-obj-actual-obj self))))
                                ':inside)
                           ((and (<= il x (- (slot-value self 'wid) ir))
                                 (<= it y (- (slot-value self 'hei) ib)))
                            ;; pointing to main area of box (where the SPRITES are)
                            (let ((in-x (- x il)) (in-y (- y it)))
                              (with-graphics-vars-bound ((screen-obj-actual-obj self) sheet)
                                (dolist (turtle (graphics-sheet-object-list sheet) ':graphics)
                                  (let ((under? (sprite-at-window-point turtle in-x in-y)))
                                    (unless (null under?)
                                      (return under?)))))))
                           (t (get-position-in-border self x y))))))

(defmethod find-bp-values ((self graphics-screen-box) superior-x superior-y
                                                      &optional (window *boxer-pane*))
  (with-slots (x-offset y-offset actual-obj screen-row superior-screen-box)
    self
    (let* ((x (- superior-x x-offset))
           (y (- superior-y y-offset))
           (within-area (get-area-of-box self x y)))
      (cond ((and (eq self (outermost-screen-box window)) (null within-area))
             (multiple-value-bind (row cha-no)
                                  (box-first-bp-values actual-obj)
                                  (values row cha-no self x y)))
        ((null within-area)
         (multiple-value-bind (row cha-no)
                              (box-self-bp-values actual-obj)
                              (values row cha-no superior-screen-box
                                      (+ (screen-obj-x-offset screen-row) superior-x)
                                      (+ (screen-obj-y-offset screen-row) superior-y))))
        ((graphics-object? within-area)
         (values (first-inferior-row (sprite-box within-area)) 0
                 self
                 0 0 ':sprite))
        ((symbolp within-area)
         (let ((mouse-bp-values-handler
                (or (get within-area 'mouse-bp-values-handler)
                    (progn (setq within-area ':graphics)
                           (get ':graphics 'mouse-bp-values-handler)))))
           (multiple-value-bind (row cha-no new-screen-box newx newy)
                                (funcall mouse-bp-values-handler actual-obj x y self)
                                (values row cha-no (or new-screen-box self)
                                        (or newx x) (or newy y) within-area))))
        (t (error "Can't find a place in ~A for position ~D, ~D"
                  self x y))))))

(defun mouse-position-values (x y &optional (window *boxer-pane*))
  ;; returns a mouse-bp, local-x, local-y and, optionally, an area
  (declare (values mouse-bp local-x local-y))
  (let ((screen-obj (outermost-screen-box window)))
    ;; sgithens TODO (check-screen-obj-arg screen-obj)
    (with-font-map-bound (*boxer-pane*)
      (multiple-value-bind (mouse-row mouse-cha-no mouse-screen-box
                                      local-x local-y border-area)
                           (find-bp-values screen-obj x y window)
                           ;; NOTE: this does not follow the normal protocol for setting the
                           ;; slots of a BP.  That is because we want to avoid the backpointers
                           ;; in editor structure that most BP's need
                           (setf (bp-row        *mouse-bp*) mouse-row
                                 (bp-cha-no     *mouse-bp*) mouse-cha-no
                                 (bp-screen-box *mouse-bp*) mouse-screen-box)
                           (values *mouse-bp* local-x local-y border-area)))))


;;; Tracking for mouse documentation
;; should be simpler than normal tracking
(defun mouse-documentation-area (x y &optional (window *boxer-pane*))
  (let ((screen-obj (outermost-screen-box window)))
    ;; sgithens TODO (check-screen-obj-arg screen-obj)
    (with-font-map-bound (window)
      (mouse-doc-place screen-obj
                       (- x (screen-obj-x-offset screen-obj))
                       (- y (screen-obj-y-offset screen-obj))))))

(defmethod mouse-doc-place ((self screen-box) x y)
  (let ((area (get-area-of-box self x y)))
    (cond ((screen-row? area)
           (mouse-doc-place area
                            (- x (slot-value self 'scroll-x-offset))
                            (- y (slot-value self 'scroll-y-offset))))
      (t (values area self)))))

(defmethod mouse-doc-place ((self screen-row) superior-x superior-y)
  (with-slots (x-offset y-offset screen-chas)
    self
    (let* ((x (- superior-x x-offset))
           (y (- superior-y y-offset))
           (within-box (find-inf-screen-box-in-sup-screen-row
                        x y screen-chas)))
      (if (null within-box)
        (values :inside (screen-box self))
        (mouse-doc-place within-box
                         (- x (screen-obj-x-offset within-box))
                         (- y (screen-obj-y-offset within-box)))))))

(defmethod mouse-doc-place ((self graphics-screen-box) x y)
  (let ((raw-area (get-area-of-box self x y)))
    (cond ((typep raw-area 'graphics-object)
           (values :sprite (slot-value raw-area 'sprite-box)))
      ((fast-memq raw-area '(:inside :outside :scroll-bar))
       (values :graphics self))
      (t (values raw-area self)))))


;;; the X,Y coordinates passed to these procedures will be
;;; in LOCAL coordinates

;; should this return the box if it is
;; not obviously on any row ?
(defmethod screen-obj-at ((self screen-box) x y &optional (recurse? t))
  (let* ((local-x (- x (slot-value self 'x-offset)))
         (local-y (- y (slot-value self 'y-offset)))
         (offset-y (- local-y (slot-value self 'scroll-y-offset)))
         (screen-row (first-screen-row self)))
    (cond ((and screen-row (< local-y (screen-obj-y-offset screen-row)))
           (values self (slot-value self 'x-offset) :top))
      ((box-ellipsis-style? (slot-value self 'screen-rows))
       ;; bottom visible section of a circular port
       (values self (slot-value self 'x-offset) :top))
      ((setq screen-row
             (do-vector-contents (row (slot-value self 'screen-rows))
               (when (< offset-y
                        (+ (screen-obj-y-offset row) (screen-obj-hei row)))
                 ;; it's IN the row
                 (return row))))
       (cond ((< local-x (screen-obj-x-offset screen-row))
              (values self (slot-value self 'x-offset) :left screen-row))
         ((> local-x (- (slot-value self 'wid)
                        *border-retry-margin*))
          (values self (slot-value self 'x-offset) :right screen-row))
         (t (screen-obj-at screen-row local-x offset-y recurse?))))
      (t
       (values self (slot-value self 'x-offset) :bottom)))))

(defmethod screen-obj-at ((self graphics-screen-box) x y &optional(recurse? t))
  (declare (ignore x y recurse?))
  (values self (slot-value self 'x-offset)))

(defun half-char-width () 4)

(defmethod screen-obj-at ((self screen-row) x y &optional (recurse? t))
  (let ((local-x (- x (slot-value self 'x-offset)))
        (local-y (- y (slot-value self 'y-offset)))
        (more-than-half-a-char? nil)
        (acc-x 0))
    (let ((char (do-screen-chas-with-font-info (cha (slot-value self 'screen-chas))
                  (let* ((width (screen-object-width cha))
                         (next-x (+ acc-x width))
                         (diff (- next-x local-x)))
                    (when (and (plusp diff)
                               ;; if we are more than half past the current
                               ;; character, then go one more iteration
                               (not (when (and (screen-cha? cha)
                                               (<= diff (half-char-width)))
                                      (setq more-than-half-a-char? t))))
                      (return cha))
                    (setq acc-x next-x)))))
      (cond ((null char) (values self acc-x))
        ((and recurse? (not (screen-cha? char))
              (not more-than-half-a-char?))
         (screen-obj-at char local-x local-y recurse?))
        ((not (screen-cha? char))
         (values self acc-x (unless more-than-half-a-char? char)))
        (t (values self acc-x))))))

(defun mouse-position-screen-values (global-x global-y)
  (multiple-value-bind (so local-offset position)
                       (screen-obj-at (outermost-screen-box) global-x global-y)
                       (cond ((screen-row? so)
                              (values so local-offset))
                         (t
                          ;; must be a screen box
                          (values (screen-row so) local-offset position)))))

#|
;;; **** this is now hacked in boxwin-mcl by modifying the coords
;;; **** of the mouse blip
;; make sure the returned value will be a row
;;; +++ I made these account for getting their coordinates in the window frame, parallel to
;;; mouse-position-values, above.  This is necessary to make tracking work right on the Mac
;;; version.  I have no idea what effect it will have on the Sun version. -- mt
#+mcl
(defun mouse-position-screen-row-values (global-x global-y)
  (multiple-value-bind (so local-offset position)
      (screen-obj-at (outermost-screen-box)
                     (- global-x (sheet-inside-left *boxer-pane*))
                     (- global-y (sheet-inside-top *boxer-pane*)))
    (cond ((screen-row? so)
       (values so local-offset))
      (t
       (let ((sr (screen-row so)))
         (if (not (screen-row? sr))
         (ecase position
           (:top (values (first-screen-row so) local-offset))
           (:bottom (values (last-screen-row so) local-offset)))
         (values sr local-offset)))))))

|#

(defun mouse-position-screen-row-values (global-x global-y)
  (multiple-value-bind (so local-offset position near-row)
                       (screen-obj-at (outermost-screen-box) global-x global-y)
                       (cond ((screen-row? so)
                              (values so local-offset))
                         (t
                          (case position
                            (:top (let ((fsr (first-screen-row so)))
                                    (if (null fsr) (values (screen-row so) local-offset)
                                      (values fsr local-offset))))
                            (:bottom (let ((lsr (last-screen-row so)))
                                       (if (null lsr) (values (screen-row so) local-offset)
                                         (values lsr (screen-obj-wid lsr))))) ; was local-offset
                            (:left (values near-row 0))
                            (:right (values near-row (screen-obj-wid near-row)))
                            (t (values (screen-row so) local-offset)))))))


(defmethod xy-context ((self screen-obj))
  (multiple-value-bind (obj-x obj-y)
                       (xy-position self)
                       (values (- obj-x (slot-value self 'x-offset))
                               (- obj-y (slot-value self 'y-offset)))))

;; also in local coordinates, use by the fast mouse tracker in coms-mouse
;; leave a little extra space on the outside of the box
(defmethod in-screen-box? ((self screen-box) x y)
  (and (< (+ (slot-value self 'x-offset) 2) x (+ (slot-value self 'x-offset)
                                                 (slot-value self 'wid) -2))
       (< (+ (slot-value self 'y-offset) 2) y (+ (slot-value self 'y-offset)
                                                 (slot-value self 'hei) -2))))

(defmethod screen-offset->cha-no ((self screen-row) x)
  (let ((acc-x 0) (cha-no 0))
    (with-font-hacking ((row-fds (screen-obj-actual-obj self)))
      (do-vector-contents (cha (slot-value self 'screen-chas)
                               :index-var-name s-cha-no)
        (check-and-handle-font-changes s-cha-no)
        (let ((width (screen-object-width cha)))
          (when (< x (+ acc-x width)) (return))
          (setq acc-x (+ acc-x width) cha-no (1+& cha-no))))
      cha-no)))

;;; this is obsolete
(defmacro with-mouse-bp-bound ((x y window) &body body)
  (declare (ignore x y window body))
  (error "This macro is obsolete"))

;;; This shouldn't be consing up a BP every time ....
;(defmacro with-mouse-bp-bound ((x y window) &body body)
;  "This macro sets up an environment where MOUSE-BP is bound to a BP which
;   indicates where in the actual structure the mouse is pointing to.
;   MOUSE-SCREEN-BOX is also bound to the screen box which the mouse is
;   pointing to. "
;  `(let ((mouse-bp (make-bp ':fixed)))
;     (multiple-value-bind (mouse-row mouse-cha-no mouse-screen-box)
;	 (screen-obj-at-position ,x ,y ,window)
;     (unwind-protect
;       (progn
;	 (set-bp-row mouse-bp mouse-row)
;	 (set-bp-cha-no mouse-bp mouse-cha-no)
;	 (set-bp-screen-box mouse-bp mouse-screen-box)
;	 . ,body)
;       (when (not-null (bp-row mouse-bp))
;	 (delete-bp (bp-row mouse-bp) mouse-bp))))))



#|
;;;; RESIZE Support

(defconstant *resize-blinker-width* 1)

(defun draw-resize-blinker (x y wid hei)
  (draw-rectangle alu-xor *resize-blinker-width* hei x y)
  (draw-rectangle alu-xor
          (-& wid *resize-blinker-width*) *resize-blinker-width*
          (+& x *resize-blinker-width*) y)
  (draw-rectangle alu-xor
          *resize-blinker-width* (-& hei *resize-blinker-width*)
          (-& (+& x wid ) *resize-blinker-width*)
          (+& y *resize-blinker-width*))
  (draw-rectangle alu-xor
          (-& wid (*& 2 *resize-blinker-width*)) *resize-blinker-width*
          (+& x *resize-blinker-width*)
          (-& (+& y hei) *resize-blinker-width*)))

(defun resize-tracker (editor-box x y screen-box)
  (multiple-value-bind (box-window-x box-window-y)
      (xy-position screen-box)
    (multiple-value-bind (left top right bottom)
        (box-borders-widths (slot-value screen-box 'box-type) screen-box)
     (drawing-on-window (*boxer-pane*)
    ;; draw the resize icon on the box...
        (bitblt-to-screen alu-seta 7 7 *mouse-resize-bitmap* 0 0
              (-& (+& box-window-x (screen-obj-wid screen-box))
                  right)
              (-& (+& box-window-y (screen-obj-hei screen-box))
                  bottom))
    (let ((old-wid x) (old-hei y))
      ;; draw-the initial resize blinker
      (draw-resize-blinker box-window-x box-window-y old-wid old-hei)
    ;; now track the mouse
    (multiple-value-bind (final-x final-y)
        (with-mouse-tracking ((mouse-x box-window-x)
                  (mouse-y box-window-y))
          (let ((new-wid (-& mouse-x box-window-x))
            (new-hei (-& mouse-y box-window-y)))
        (unless (or (minusp new-wid) (minusp new-hei))
          ;; erase the previous resize blinker
          (draw-resize-blinker box-window-x box-window-y
                       old-wid old-hei)
          ;; and update the values and draw the new one
          (setq old-wid new-wid old-hei new-hei)
          (draw-resize-blinker box-window-x box-window-y
                       new-wid new-hei))))
      ;; erase the last hollow blinker
      (draw-resize-blinker box-window-x box-window-y old-wid old-hei)
      (unless (or (<& final-x box-window-x)
              (<& final-y box-window-y))
        (set-fixed-size editor-box
                (- final-x box-window-x left right)
                (- final-y box-window-y top bottom))))))))
  (modified editor-box)
  (box-first-bp-values editor-box))


(setf (get :resize-tab 'mouse-bp-values-handler) 'resize-tracker)

|#


