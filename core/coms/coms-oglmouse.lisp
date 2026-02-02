;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER; -*-
;;;;
;;;;
;;;;     $Header: coms-mouse.lisp,v 1.0 90/01/24 22:08:41 boxer Exp $
;;;;
;;;;
;;;;
;;;;     $Log:	coms-mouse.lisp,v $
;;;;    ;;;Revision 1.0  90/01/24  22:08:41  boxer
;;;;    ;;;Initial revision
;;;;    ;;;
;;;;
;;;;        Boxer
;;;;        Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;        Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;        used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;        Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;        https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                             +-Data--+
;;;;                    This file is part of the | BOXER | system
;;;;                                             +-------+
;;;;
;;;;
;;;;         This file contains top level definitions for
;;;;         the set of BOXER Editor Mouse Commands.
;;;;
;;;;
;;;;    Modification History (most recent at top)
;;;;
;;;;     9/24/12 removed fixnum assumptions in: com-mouse-define-region, com-mouse-resize-box,
;;;;                mouse-corner-tracking, com-mouse-border-toggle-type
;;;;                com-mouse-scroll-box, com-mouse-page-scroll, com-mouse-limit-scroll-box,
;;;;                mouse-smooth-scroll-internal, mouse-in-v-scroll-bar-internal
;;;;     8/18/11 removed drawing-on-window from com-mouse-resize-box (apparently opengl::rendering-on is
;;;;             not, or has troubles with being, reentrant)
;;;;     8/11/11 com-mouse-resize-box
;;;;     5/20/09 scroll-bar commands
;;;;     2/27/07 border coms changed to use new track mouse paradigm
;;;;             (redisplay-cursor) => (repaint-cursor), (redisplay) => (repaint)
;;;;             remove all (add-redisplay-clue 's
;;;;    11/15/03 com-mouse-define-region uses restore-point-position to handle possible
;;;;             munging of the destination
;;;;    10/29/03 removed flush-port-buffer from com-mouse-define-region, graphics flush
;;;;             now occurs as part of the with-mouse-tracking macro
;;;;             #+ graphics-flush changed to (force-graphics-output) in
;;;;             mouse-{smooth-scroll,line-scroll,in-scroll-bar}-internal
;;;;    10/15/03 display-force-output changed to force-graphics-output in
;;;;             com-mouse-resize-box & com-christmas-tree
;;;;    10/26/03 flush-port-buffer added to mouse-smooth-scroll-internal and
;;;;             mouse-in-scroll-bar-internal
;;;;     4/21/03 merged current LW and MCL files
;;;;     1/15/02 changed com-mouse-define-region to default all args so that calling it
;;;;             from boxer code will no longer blow out.  Instead, it behaves as if
;;;;             the mouse were clicked in it's current position
;;;;     5/15/01 *smooth-scroll-pause-time* changed to 0.005 for smoother scrolling of more
;;;;             complicated rows (like with boxes)
;;;;     5/11/01 mouse-smooth-scroll-internal fixed to provide useful time for timed-body
;;;;             *smooth-scroll-pause-time* changed from .01 to .001
;;;;     2/13/01 merged current LW and MCL files
;;;;     4/11/00 calls to ENTER by mouse coms now check to see if we are entering a box
;;;;             from below and pass the arg to suppress entry triggers
;;;;    10/27/98 com-mouse-limit-scroll-box uses last-page-top-row instead of
;;;;             last-inferior-row
;;;;    10/26/98 refinements to mouse-in-scroll-bar-internal so that lowest scroll position
;;;;             will include a full box of text
;;;;    10/19/98 explicitly setup mouse-screen-row and mouse-x before entering tracking loop
;;;;             for shift click clause in mouse-define-region
;;;;    10/15/98 added initialization for row height array in mouse-in-scroll-bar-internal
;;;;    10/09/98 elevator-row-string used to limit the length row # display
;;;;    10/08/98 new-elevator-scrolled-row
;;;;     8/27/98 Added size reporting in the status line to com-mouse-resize-box
;;;;     8/18/98 Reduce blinkiness in com-mouse-define-region by recycling the tracking
;;;;             blinkers for use in the newly defined region
;;;;     6/24/98 fixed com-mouse-resize-box to handle minimum size redisplay bug
;;;;     5/14/98 fix mouse-smooth-scroll-internal bug which crashed when scrolled down
;;;;             from a newly created box
;;;;     5/14/98 started logging changes: source = boxer version 2.3alphaR
;;;;


(in-package :boxer)


;;;; MOUSE-CLICKS

(defboxer-command com-mouse-collapse-box (&optional (window *boxer-pane*)
                                                    (x (bw::boxer-pane-mouse-x))
                                                    (y (bw::boxer-pane-mouse-y))
                                                    (mouse-bp
                                                      (mouse-position-values x y))
                                                    (click-only? t))
  "make the one step smaller"
  ;; Note that this is designed to be called in the Boxer process,
  ;; not in the Mouse Process -- This is important!!!
  window x y click-only?    ;  (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((new-box (bp-box mouse-bp))
        (new-row (bp-row mouse-bp))
        (mouse-screen-box (bp-screen-box mouse-bp))
        (new-cha-no (bp-cha-no mouse-bp)))
    (when (and (not-null new-row)
                (box? new-box))
      (send-exit-messages new-box mouse-screen-box)
      (move-point-1 new-row new-cha-no mouse-screen-box)
      (com-collapse-box)))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-shrink-box (&optional (window *boxer-pane*)
                                                  (x (bw::boxer-pane-mouse-x))
                                                  (y (bw::boxer-pane-mouse-y))
                                                  (mouse-bp
                                                   (mouse-position-values x y))
                                                  (click-only? t))
  "make the box tiny"
  ;; Note that this is designed to be called in the Boxer process,
  ;; not in the Mouse Process -- This is important!!!
  window x y click-only? ; (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((new-box (bp-box mouse-bp))
        (new-row (bp-row mouse-bp))
        (mouse-screen-box (bp-screen-box mouse-bp))
        (new-cha-no (bp-cha-no mouse-bp)))
    (when (and (not-null new-row)
               (box? new-box))
      (send-exit-messages new-box mouse-screen-box)
      (move-point-1 new-row new-cha-no mouse-screen-box)
      (com-shrink-box)))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-super-shrink-box (&optional (window *boxer-pane*)
                                                        (x (bw::boxer-pane-mouse-x))
                                                        (y (bw::boxer-pane-mouse-y))
                                                        (mouse-bp
                                                         (mouse-position-values x y))
                                                        (click-only? t))
  "make the box tiny"
  ;; Note that this is designed to be called in the Boxer process,
  ;; not in the Mouse Process -- This is important!!!
  window x y click-only? ; (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((new-box (bp-box mouse-bp))
        (new-row (bp-row mouse-bp))
        (mouse-screen-box (bp-screen-box mouse-bp))
        (new-cha-no (bp-cha-no mouse-bp)))
    (when (and (not-null new-row)
               (box? new-box))
      (unless (and (not (eq mouse-screen-box (outermost-screen-box)))
                   mouse-screen-box
                   (eq :supershrunk
                       (display-style
                        (screen-obj-actual-obj mouse-screen-box))))
        (send-exit-messages new-box mouse-screen-box)
        (move-point-1 new-row new-cha-no mouse-screen-box)
        (com-super-shrink-box))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-expand-box (&optional (window *boxer-pane*)
                                                  (x (bw::boxer-pane-mouse-x))
                                                  (y (bw::boxer-pane-mouse-y))
                                                  (mouse-bp
                                                   (mouse-position-values x y))
                                                  click-only?)
  "make the box one step bigger"
  ;; Note that this is designed to be called in the Boxer process,
  ;; not in the Mouse Process -- This is important!!!
  window x y click-only? ;  (declare (ignore window x y click-only?))
  (let ((new-box (screen-obj-actual-obj (bp-screen-box mouse-bp)))
        (old-box (point-box))
        (new-row (bp-row mouse-bp))
        (mouse-screen-box (bp-screen-box mouse-bp))
        (new-cha-no (bp-cha-no mouse-bp)))
    ;; If the mouse cursor isn't over some type of shrunken box, we aren't going to
    ;; do anything. This prevents the region being reset after dragging to select some
    ;; text, which is bound to mouse-down.  - sgithens 2021-10-18
    (cond ((and (not-null new-row) (box? new-box) (shrunken? new-box)
            ;; This handles the case of when you are on the top level box targetted by a port.
            ;; Since com-mouse-expand-box is currently bound to mouse-click, if we click in a port, we
            ;; don't want the expand message to be sent to the actual box that is targetted by the box.
            ;; boxer-bugs-102
            ;; 1. The box is not a port box
            ;; 2. The screen-obj type is port
            ;; TODO what if it's a port that's pointing to a port?
            (eq mouse-screen-box (car (screen-objs new-box))))
           (reset-region)
           (unless (eq old-box new-box)
             (send-exit-messages new-box mouse-screen-box)
             (enter new-box (not (superior? old-box new-box))))
           (move-point-1 new-row new-cha-no mouse-screen-box)
           (com-expand-box))
          ((and (not-null new-row) (box? new-box) (shrunken? new-box))
           (reset-region)
           (unless (eq old-box new-box)
             (send-exit-messages new-box mouse-screen-box)
             (enter new-box (not (superior? old-box new-box))))
           (move-point-1 new-row new-cha-no mouse-screen-box)
           ;; If this top level port is actually shrunk (not the box it point to),
           ;; go ahead and exand it
           (let* ((new-actual (screen-obj-actual-obj mouse-screen-box))
                  (new-style-list (display-style-list new-actual))
                  (new-style (display-style-style new-style-list)))
             (when (member new-style '(:SHRUNK :SUPERSHRUNK))
               (com-expand-box)
             ))
           )))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-set-outermost-box (&optional (window *boxer-pane*)
                                                         (x (bw::boxer-pane-mouse-x))
                                                         (y (bw::boxer-pane-mouse-y))
                                                         (mouse-bp
                                                          (mouse-position-values x y))
                                                         (click-only? t))
  "make the box full screen"
  ;; Note that this is designed to be called in the Boxer process,
  ;; not in the Mouse Process -- This is important!!!
  window x y click-only? ;  (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((old-box (point-box))
        (new-box (bp-box mouse-bp))
        (new-row (bp-row mouse-bp))
        (new-cha-no (bp-cha-no mouse-bp)))
    (when (and (not-null new-row) (box? new-box))
      (unless (eq old-box new-box)
        (send-exit-messages new-box (bp-screen-box mouse-bp))
        (enter new-box (not (superior? old-box new-box))))
      (move-point-1 new-row new-cha-no (bp-screen-box mouse-bp))
      (if (graphics-box? new-box)
        (com-expand-box)
        (com-set-outermost-box))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-move-point (&optional (window *boxer-pane*)
                                                  (x (bw::boxer-pane-mouse-x))
                                                  (y (bw::boxer-pane-mouse-y))
                                                  (mouse-bp (mouse-position-values x y))
                                                  click-only?
                                                  (box-proc nil))
  "Go there"
  ;; Note that this is designed to be called in the Boxer process,
  ;; not in the Mouse Process -- This is important!!!
  window x y click-only? ;  (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((old-box (point-box))
        (new-box (bp-box mouse-bp))
        (new-row (bp-row mouse-bp))
        (mouse-screen-box (bp-screen-box mouse-bp))
        (new-cha-no (bp-cha-no mouse-bp)))
    (when (and (not-null new-row) (not-null new-cha-no) (not-null new-box))
      (unless (eq old-box new-box)
        (send-exit-messages new-box mouse-screen-box)
        (enter new-box (not (superior? old-box new-box))))
      (move-point-1 new-row new-cha-no mouse-screen-box))
    (when (and (not box-proc)
               (not (name-row? new-row))
               (shrunken? (screen-obj-actual-obj (screen-box-point-is-in))))
      (com-expand-box)))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-define-region (&optional (window *boxer-pane*)
                                                     (x (bw::boxer-pane-mouse-x))
                                                     (y (bw::boxer-pane-mouse-y))
                                                     (mouse-bp
                                                      (mouse-position-values x y))
                                                     (click-only? t)
                                                     (shift?
                                                      (bw::shift-key-pressed?)))
  "Define a region with the mouse"
  window ; (declare (ignore window))
  ;; first, if there already is an existing region, flush it
  (reset-region) ; might want to reposition instead when shift-clicking
  ;; then go to where the mouse is pointing
  (let ((old-box (point-box))
        (new-box (bp-box mouse-bp))
        (new-row (bp-row mouse-bp))
        (mouse-screen-box (bp-screen-box mouse-bp))
        (new-cha-no (bp-cha-no mouse-bp))
        ;; should probably eventually make this a global var...
        (mouse-position (fill-doit-cursor-position-vector
                          (make-process-doit-cursor-position) mouse-bp)))
    (when (and (not shift?)
               ;; if the shift key is pressed, don't move the point...
               (not-null new-row) (not-null new-cha-no) (not-null new-box)
               (not (shrunken? mouse-screen-box))
               )
      (unless (eq old-box new-box)
        (send-exit-messages new-box mouse-screen-box t )
        (enter new-box (not (superior? old-box new-box))))
      ;; enter method needs to be called 1st because we may need to fill
      ;; a boxes contents before moving
      (cond ((or (null (superior-box new-row))
                 (not (superior? (superior-box new-row) *initial-box*)))
             ;; sometimes the destination can become munged as a result of
             ;; trigger action via send-exit-messages
             (restore-point-position mouse-position t))
        (t
         (move-point-1 new-row new-cha-no mouse-screen-box))))
      )
  (when (and (or (null click-only?) shift?)  (not (shrunken? (bp-screen-box mouse-bp))))
    ;; now go about dragging a region defined by *point* and the mouse-bp
    ;; unless the user is no longer holding the mouse button down
    ;; now track the mouse
    #+lispworks (multiple-value-bind (original-screen-row original-x)
                         (if shift?
                           (let ((csr (current-screen-row (point-row))))
                             (values csr (cha-no->x-coord csr (point-cha-no))))
                           (mouse-position-screen-row-values x y))
                         (let ((original-screen-box (screen-box original-screen-row)))
                           ;; should this be (bp-screen-box mouse-bp) ?
                           (multiple-value-bind (original-context-x original-context-y)
                                                ;; these are the absolute offsets from which the
                                                ;; ORIGINAL-SCREEN-BOX is displaced
                                                (xy-context original-screen-box)
                                                ;;(decf original-context-x (sheet-inside-left *boxer-pane*))
                                                ;;(decf original-context-y (sheet-inside-top *boxer-pane*))
                                                ;; variables used by the loop
                                                ;; current-screen-box is the lowest level screen box that is
                                                ;; common to the *point* and the mark
                                                ;; current-screen-row is the screen-row WITHIN that box that
                                                ;; the mouse is on.
                                                ;; (screen-obj-x-offset <>) + context-x = window x-coordinate
                                                (let ((current-screen-box original-screen-box)
                                                      (osb (outermost-screen-box))
                                                      (mouse-screen-row original-screen-row)
                                                      (mark-screen-row original-screen-row)
                                                      (mouse-x original-x) (mark-x original-x) (mark-screen-box nil)
                                                      (context-x original-context-x)(context-y original-context-y))
                                                  (when shift?
                                                    (multiple-value-setq (mouse-screen-row mouse-x)
                                                                         (mouse-position-screen-row-values x y)))
                                                  (catch 'mouse-confusion
                                                    (unwind-protect
                                                     ;; the inner mouse tracking loop...
                                                     (with-mouse-tracking ((raw-mouse-x x) (raw-mouse-y y))
                                                                                        ;; first check to make sure that the mouse is still
                                                                                        ;; inside of the current-screen-box
                                                                                        ;; if it isn't, then reset the current-screen-box
                                                                                        ;; and all the other variables associated with it, then
                                                                                        ;; turn off the highlighting
                                                                                        ;;
                                                                                        ;; Find the box by walking upward from the original
                                                                                        (catch 'mouse-tracking-body
                                                                                          ;; this DO loop updates current-screen-box,
                                                                                          ;; mark-screen-row and mark-screen-box
                                                                                          (do* ((new-box original-screen-box (superior-screen-box new-box))
                                                                                                ;; these are updated at the bottom of the loop
                                                                                                (new-context-x original-context-x
                                                                                                               (- new-context-x
                                                                                                                  (screen-obj-x-offset new-box)
                                                                                                                  (screen-obj-x-offset sup-row)))
                                                                                                (new-context-y original-context-y
                                                                                                               (- new-context-y
                                                                                                                  (screen-obj-y-offset new-box)
                                                                                                                  (screen-obj-y-offset sup-row)))
                                                                                                (new-mark-box nil)
                                                                                                (sup-row nil))
                                                                                            ((or (eq new-box osb) (eq (superior-screen-box new-box) osb))
                                                                                             ;; if we get this far, then we are either on the
                                                                                             ;; box or else use the outermost box
                                                                                             (cond
                                                                                               ((in-screen-box? new-box
                                                                                                                (- raw-mouse-x new-context-x)
                                                                                                                (- raw-mouse-y new-context-y))
                                                                                                (unless (eq new-box current-screen-box)
                                                                                                  (setq current-screen-box new-box
                                                                                                        context-x new-context-x context-y new-context-y)
                                                                                                  (setq mark-screen-box new-mark-box
                                                                                                        mark-screen-row
                                                                                                        (if (null new-mark-box)
                                                                                                          original-screen-row
                                                                                                          (screen-row new-mark-box)))))
                                                                                               ((eq new-box osb)
                                                                                                ;; If we are outside the outermost box, skip
                                                                                                ;; the rest of the action and continue looping
                                                                                                ;; until we are inside again
                                                                                                (throw 'mouse-tracking-body nil))
                                                                                               (t
                                                                                                (setq current-screen-box osb
                                                                                                      context-x 0 context-y 0
                                                                                                      mark-screen-box new-box
                                                                                                      mark-screen-row (screen-row new-box)))))
                                                                                            ;; DO innards
                                                                                            (when (in-screen-box? new-box
                                                                                                                  (- raw-mouse-x new-context-x)
                                                                                                                  (- raw-mouse-y new-context-y))
                                                                                              (unless (eq new-box current-screen-box)
                                                                                                (setq current-screen-box new-box
                                                                                                      context-x new-context-x context-y new-context-y)
                                                                                                ;; when the current-screen-box changes, the mark
                                                                                                ;; values must also change but we need to wait
                                                                                                ;; until the mouse-screen-row is calculated before
                                                                                                ;; we can properly determine the mark-x (because
                                                                                                ;; inferior boxes should ALWAYS be included)
                                                                                                (setq mark-screen-box new-mark-box
                                                                                                      mark-screen-row
                                                                                                      (if (null new-mark-box)
                                                                                                        original-screen-row
                                                                                                        (screen-row new-mark-box))))
                                                                                              (return))
                                                                                            ;; update the new-mark-box and context values now
                                                                                            (setq new-mark-box new-box
                                                                                                  sup-row (screen-row new-box)))
                                                                                          ;; DO loop ends

                                                                                          ;; At this point, we know that current-screen-box,
                                                                                          ;; mark-screen-row and mark-screen-box are all valid
                                                                                          ;; we now want to update mark-x, mouse-screen-row and
                                                                                          ;; mouse-x.  this will depend upon the relative position
                                                                                          ;; of the mouse and the mark
                                                                                          (multiple-value-bind (new-screen-obj offset position near-row)
                                                                                                               (screen-obj-at current-screen-box
                                                                                                                              (- raw-mouse-x context-x )
                                                                                                                              (- raw-mouse-y context-y ) nil)
                                                                                                               (cond ((screen-row? new-screen-obj)
                                                                                                                      (setq mouse-screen-row new-screen-obj)
                                                                                                                      ;; compare relative positions of mouse and mark
                                                                                                                      ;; to generate correct mark-x and mouse-x
                                                                                                                      (cond ((eq new-screen-obj mark-screen-row)
                                                                                                                             (cond ((null mark-screen-box)
                                                                                                                                    (setq mark-x original-x
                                                                                                                                          mouse-x
                                                                                                                                          (if (or (not (screen-box? position))
                                                                                                                                                  (< offset mark-x))
                                                                                                                                            offset
                                                                                                                                            (+ offset
                                                                                                                                               (screen-obj-wid position)))))
                                                                                                                               ((< (screen-obj-x-offset mark-screen-box)
                                                                                                                                   offset)
                                                                                                                                (setq mark-x
                                                                                                                                      (screen-obj-x-offset mark-screen-box)
                                                                                                                                      mouse-x
                                                                                                                                      (if (screen-box? position)
                                                                                                                                        (+ offset
                                                                                                                                           (screen-obj-wid position))
                                                                                                                                        offset)))
                                                                                                                               (t
                                                                                                                                (setq mark-x
                                                                                                                                      (+ (screen-obj-x-offset
                                                                                                                                          mark-screen-box)
                                                                                                                                         (screen-obj-wid mark-screen-box))
                                                                                                                                      mouse-x offset))))
                                                                                                                        ((< (screen-obj-y-offset mark-screen-row)
                                                                                                                            (screen-obj-y-offset new-screen-obj))
                                                                                                                         ;; mouse is behind the mark
                                                                                                                         (setq mouse-x
                                                                                                                               (if (screen-box? position)
                                                                                                                                 (+ offset (screen-obj-wid position))
                                                                                                                                 offset)
                                                                                                                               mark-x
                                                                                                                               (if (null mark-screen-box)
                                                                                                                                 original-x
                                                                                                                                 (screen-obj-x-offset mark-screen-box))))
                                                                                                                        (t
                                                                                                                         ;; mark must be behind the mouse
                                                                                                                         (setq mouse-x offset
                                                                                                                               mark-x
                                                                                                                               (if (null mark-screen-box)
                                                                                                                                 original-x
                                                                                                                                 (+
                                                                                                                                  (screen-obj-x-offset mark-screen-box)
                                                                                                                                  (screen-obj-wid mark-screen-box)))))))
                                                                                                                 ((eq position :left)
                                                                                                                  (setq mouse-screen-row near-row mouse-x 0)
                                                                                                                  ;; mouse should be in front of the mark
                                                                                                                  (setq mark-x
                                                                                                                        (if (null mark-screen-box)
                                                                                                                          original-x
                                                                                                                          (+ (screen-obj-x-offset mark-screen-box)
                                                                                                                             (screen-obj-wid mark-screen-box)))))
                                                                                                                 ((eq position :right)
                                                                                                                  (setq mouse-screen-row near-row
                                                                                                                        mouse-x (screen-obj-wid near-row))
                                                                                                                  ;; mouse must be after the mark
                                                                                                                  (setq mark-x
                                                                                                                        (if (null mark-screen-box)
                                                                                                                          original-x
                                                                                                                          (screen-obj-x-offset mark-screen-box))))
                                                                                                                 ((eq position :top)
                                                                                                                  (setq mouse-screen-row (first-screen-row
                                                                                                                                          current-screen-box)
                                                                                                                        mouse-x 0)
                                                                                                                  ;; the mouse must be in front of the mark
                                                                                                                  (setq mark-x
                                                                                                                        (if (null mark-screen-box)
                                                                                                                          original-x
                                                                                                                          (+
                                                                                                                           (screen-obj-x-offset mark-screen-box)
                                                                                                                           (screen-obj-wid mark-screen-box)))))
                                                                                                                 ((eq position :bottom)
                                                                                                                  (setq mouse-screen-row (last-screen-row
                                                                                                                                          current-screen-box)
                                                                                                                        mouse-x (screen-obj-wid mouse-screen-row))
                                                                                                                  ;; mouse must be after the mark
                                                                                                                  (setq mark-x
                                                                                                                        (if (null mark-screen-box)
                                                                                                                          original-x
                                                                                                                          (screen-obj-x-offset mark-screen-box))))
                                                                                                                 (t
                                                                                                                  ;; we should be here....
                                                                                                                  (warn "Can't find mouse for ~A, ~D, ~A"
                                                                                                                        new-screen-obj offset position)
                                                                                                                  (setq mouse-screen-row
                                                                                                                        (first-screen-row current-screen-box)
                                                                                                                        mouse-x 0
                                                                                                                        mark-x (if (null mark-screen-box)
                                                                                                                                 original-x
                                                                                                                                 (+ (screen-obj-x-offset mark-screen-box)
                                                                                                                                    (screen-obj-wid mark-screen-box)))))))
                                                                                          ;; Now mark-x, mouse-screen-row and mouse-x should be correct

                                                                                          ;; sanity check
                                                                                          (unless (and (eq current-screen-box (screen-box mouse-screen-row))
                                                                                                       (eq current-screen-box (screen-box mark-screen-row)))
                                                                                            (warn "Mouse (~A) and Mark (~A) are confused"
                                                                                                  mouse-screen-row mark-screen-row)
                                                                                            (throw 'mouse-confusion nil))

                                                                                          ;; Now all the mark and mouse variables are valid and
                                                                                          ;; (maybe) repaint the screen, showing new region...
                                                                                          (let ((mark-row (screen-obj-actual-obj mark-screen-row))
                                                                                                (mouse-row (screen-obj-actual-obj mouse-screen-row))
                                                                                                (mark-cha-no (screen-offset->cha-no mark-screen-row mark-x))
                                                                                                (mouse-cha-no (screen-offset->cha-no mouse-screen-row
                                                                                                                mouse-x)))
                                                                                            (flet ((same-pos? (mouse-1st?)
                                                                                                              (let ((bp1 (if mouse-1st?
                                                                                                                           (interval-start-bp
                                                                                                                            *region-being-defined*)
                                                                                                                           (interval-stop-bp *region-being-defined*)))
                                                                                                                    (bp2 (if mouse-1st?
                                                                                                                           (interval-stop-bp *region-being-defined*)
                                                                                                                           (interval-start-bp
                                                                                                                            *region-being-defined*))))
                                                                                                                (and (eq mouse-row (bp-row bp1))
                                                                                                                     (= mouse-cha-no (bp-cha-no bp1))
                                                                                                                     (eq mark-row (bp-row bp2))
                                                                                                                     (= mark-cha-no (bp-cha-no bp2))))))
                                                                                                  (cond ((null *region-being-defined*)
                                                                                                         (let ((mark-bp (make-bp ':fixed))
                                                                                                               (mouse-bp (make-bp ':fixed)))
                                                                                                           (setf (bp-row mark-bp) mark-row
                                                                                                                 (bp-row mouse-bp) mouse-row
                                                                                                                 (bp-cha-no mark-bp) mark-cha-no
                                                                                                                 (bp-cha-no mouse-bp) mouse-cha-no)
                                                                                                           (setq *region-being-defined*
                                                                                                                 (if (or (row-> mark-row mouse-row)
                                                                                                                         (and (eq mark-row mouse-row)
                                                                                                                              (> mark-cha-no mouse-cha-no)))
                                                                                                                   (make-editor-region mouse-bp mark-bp)
                                                                                                                   (make-editor-region mark-bp mouse-bp))))
                                                                                                         (push *region-being-defined* *region-list*)
                                                                                                         (repaint))
                                                                                                    ((or (row-> mark-row mouse-row)
                                                                                                         (and (eq mark-row mouse-row)
                                                                                                              (> mark-cha-no mouse-cha-no)))
                                                                                                     ;; mouse is in front of mark
                                                                                                     (cond ((same-pos? t)
                                                                                                            ;; mouse hasn't moved, so do nothing
                                                                                                            )
                                                                                                       (t (let ((bp1 (interval-start-bp
                                                                                                                      *region-being-defined*))
                                                                                                                (bp2 (interval-stop-bp
                                                                                                                      *region-being-defined*)))
                                                                                                            (setf (bp-row bp1) mouse-row
                                                                                                                  (bp-cha-no bp1) mouse-cha-no
                                                                                                                  (bp-row bp2) mark-row
                                                                                                                  (bp-cha-no bp2) mark-cha-no))
                                                                                                          (repaint))))
                                                                                                    (t ; mark is in front of mouse...
                                                                                                       (cond ((same-pos? nil))
                                                                                                         (t (let ((bp1 (interval-start-bp
                                                                                                                        *region-being-defined*))
                                                                                                                  (bp2 (interval-stop-bp
                                                                                                                        *region-being-defined*)))
                                                                                                              (setf (bp-row bp1) mark-row
                                                                                                                    (bp-cha-no bp1) mark-cha-no
                                                                                                                    (bp-row bp2) mouse-row
                                                                                                                    (bp-cha-no bp2) mouse-cha-no))
                                                                                                            (repaint)))))))))
                                                     ;; END of the mouse tracking loop
                                                     ;; unwind-protect forms...
                                                     (let ((mark-cha-no (screen-offset->cha-no mark-screen-row mark-x))
                                                           (mouse-cha-no (screen-offset->cha-no mouse-screen-row mouse-x)))
                                                       (cond ((and (eq mark-screen-row mouse-screen-row)
                                                                   (=& mark-cha-no mouse-cha-no))
                                                              ;; no region to define so make sure we clean up the blinkers
                                                              (unless (null *region-being-defined*)
                                                                (setq *region-list*
                                                                      (fast-delq *region-being-defined* *region-list*)
                                                                      *region-being-defined*
                                                                      nil)))
                                                         (t
                                                          ;; region is still there so...
                                                          (entering-region-mode)))))
                                                    )			; Matches catch 'mouse-confusion
                                                  boxer-eval::*novalue*)))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-doit-now (&optional (window *boxer-pane*)
                                                (x (bw::boxer-pane-mouse-x))
                                                (y (bw::boxer-pane-mouse-y))
                                                (mouse-bp
                                                 (mouse-position-values x y))
                                                (click-only? t))
  "Go there and doit"
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (actual-box (screen-obj-actual-obj screen-box)))
    (cond ((null actual-box))
      ((and (shrunken? actual-box)
            (not (eq screen-box (outermost-screen-box))))
       ;; might as well open it
       (com-mouse-set-outermost-box window x y mouse-bp click-only?))
      (t
       (com-mouse-move-point window x y mouse-bp click-only?)
       (com-doit-now)
       boxer-eval::*novalue*))))

;;;; functions for cut,copy and paste


;;;;; killing and yanking stuff with the mouse and regions

(defvar *suitcase-region* nil
  "kill the current region and jam the crap into the suitcase.")

(defvar *old-region-location* nil)

(defun suck-region ()
  (setq *suitcase-region*
        (or *region-being-defined* (get-current-region)))
  (cond ((not (null *suitcase-region*))
         (with-region-top-level-bps (*suitcase-region* :start-bp-name start
                                                       :stop-bp-name stop)
           (setq *old-region-location* (copy-bp start)))
         (kill-region *suitcase-region*))
    (t
     ;; just in case...
     (unless (null *suitcase-mode*) (cleanup-suitcase))
     (boxer-editor-error "No region to kill."))))

(defun entering-suitcase-bindings ()
  (add-mode (suitcase-mode)))

(defun exiting-suitcase-bindings ()
  (remove-mode (suitcase-mode)))

;;; suck the region, make a suitcase out of it. rebind mouse-middle
(defun com-suck-region (&rest arguments)
  (declare (ignore arguments))
  (suck-region)
  (set-mouse-cursor :suitcase)
  (boxer-editor-message "Click to Insert Cut Text")
  (setq *suitcase-mode* t)
  ;; rebind various mouse-middle functions
  (entering-suitcase-bindings)
  (reset-region nil)
  (RESET-EDITOR-NUMERIC-ARG)
  boxer-eval::*novalue*)

;;; mouse-copy, non-destructive
(defun com-suck-copy-region (&rest arguments)
  (declare (ignore arguments))
  (suck-copy-region)
  (set-mouse-cursor :suitcase)
  (boxer-editor-message "Click to Insert Copied Text")
  (setq *suitcase-mode* t)
  (entering-suitcase-bindings)
  (reset-region nil)
  (RESET-EDITOR-NUMERIC-ARG)
  boxer-eval::*novalue*)

;;; suck the region up into a suitcase, without destroying the text sucked
(defun suck-copy-region ()
  (setq *suitcase-region*
        (or *region-being-defined* (get-current-region)))
  (cond (*suitcase-region* (setq *suitcase-region*
                                 (copy-interval *suitcase-region*)))
    (t
     ;; just in case...
     (unless (null *suitcase-mode*) (cleanup-suitcase))
     (boxer-editor-error "No region to copy."))))

;; cleanup function, it may be called after yanking or by the Abort key
(defun cleanup-suitcase ()
  (unless (null *suitcase-mode*)
    (unless (null *old-region-location*)
      (move-to-bp *old-region-location*)
      (unless (null *suitcase-region*)
        (yank-region *point* *suitcase-region*)
        (setq *old-region-location* nil)))
    (unless (null *suitcase-region*) (deallocate-region *suitcase-region*))
    (setq *suitcase-region* nil)
    (reset-mouse-cursor)
    (setq *suitcase-mode* nil)
    (exiting-suitcase-bindings)))

;; yank back the suitcase

(defun com-bring-back-region (window x y mouse-bp click-only?)
  (com-mouse-move-point window x y mouse-bp click-only?)
  (cond ((not (null *suitcase-region*))
         (yank-region  *point* *suitcase-region*)
         ;; if successful, no need to reset...
         (setq *old-region-location* nil)
         (cleanup-suitcase))
    ((not (null *suitcase-mode*))
     (cleanup-suitcase)))
  (reset-editor-numeric-arg)
  (reset-region)
  boxer-eval::*novalue*)





;;;; Commands for Mouse border Areas....
;; moving to boxdef.lisp 2020-03-27
;; (defvar *warn-about-disabled-commands* t)
;; (defvar *only-shrink-wrap-text-boxes* nil)

(defboxer-command com-mouse-resize-box (&optional (window *boxer-pane*)
                                                  (x (bw::boxer-pane-mouse-x))
                                                  (y (bw::boxer-pane-mouse-y))
                                                  (mouse-bp
                                                   (mouse-position-values x y))
                                                  (click-only? t))
  "Resize the box with the mouse.  Just clicking unfixes the box size"
  window x y  ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (actual-box (screen-obj-actual-obj screen-box))
         (box-type (box-type screen-box))
         (pixel-correction 5))
    (cond ((null actual-box))
      ((shrunken? actual-box)
       ;; might as well open it
       (com-mouse-expand-box window x y mouse-bp click-only?))
      ((not (null click-only?))
       ;; reset the scrolling to the top
       (set-scroll-to-actual-row screen-box nil)
       (set-fixed-size actual-box nil nil))
      ((and *only-shrink-wrap-text-boxes* (null (graphics-sheet actual-box)))
       (when *warn-about-disabled-commands*
         (boxer-editor-warning
          "Resizing Text Boxes is disabled, see the Preferences menu")))
      ((eq screen-box (outermost-screen-box))
       (boxer-editor-warning
        "Can't Resize the Outermost Box. Resize the Window instead."))
      (t
       ;; mouse grab, interactive loop
       #+lispworks (multiple-value-bind (box-window-x box-window-y)
                            (xy-position screen-box)
                            (multiple-value-bind (left top right bottom)
                                                 (box-borders-widths box-type screen-box)
                                                 (let ((minimum-track-wid *minimum-box-wid*)
                                                       (minimum-track-hei *minimum-box-hei*)
                                                       (first-movement-flag nil))
                                                   ;; if the box to be resized has a name, the minimum values
                                                   ;; should be different
                                                   (unless (null (name screen-box))
                                                     (multiple-value-bind (n-min-x n-min-y n-max-x n-max-y)
                                                                          (box-borders-name-tab-values box-type screen-box)
                                                                          (declare (ignore n-min-x))
                                                                          (setq minimum-track-wid (max n-max-x minimum-track-wid)
                                                                                minimum-track-hei (+ minimum-track-hei
                                                                                                     (- n-max-y n-min-y)))))
                                                   (multiple-value-bind (final-x final-y moved-p)
                                                                        (with-mouse-tracking ((mouse-x x) (mouse-y y)
                                                                                                                        :action :resize)
                                                                                                           (let ((new-wid (max minimum-track-wid
                                                                                                                                (- mouse-x box-window-x)))
                                                                                                                 (new-hei (max minimum-track-hei
                                                                                                                                (- mouse-y box-window-y)))
                                                                                                                 (last-wid (screen-obj-wid screen-box))
                                                                                                                 (last-hei (screen-obj-hei screen-box)))
                                                                                                             (cond ((and (null first-movement-flag)
                                                                                                                         ;; nothing is happening yet
                                                                                                                         (= mouse-x x) (= mouse-y y)))
                                                                                                               ((and (= new-wid last-wid) (= new-hei last-hei))
                                                                                                                ;; same place, so do nothing...
                                                                                                                )
                                                                                                               (t
                                                                                                                (when (null first-movement-flag)
                                                                                                                  (setq first-movement-flag t))
                                                                                                                (status-line-size-report screen-box
                                                                                                                                         new-wid new-hei)
                                                                                                                (let ((*update-bitmap?* nil))
                                                                                                                  ;; suppress allocation of multiple different
                                                                                                                  ;; sized bitmaps inside of loop
                                                                                                                  (set-fixed-size actual-box
                                                                                                                                  (+ new-wid pixel-correction)
                                                                                                                                  (+ new-hei pixel-correction))
                                                                                                                  (repaint))
                                                                                                                ))))
                                                                        ;; finalize..
                                                                        (cond ((null moved-p)
                                                                               ;; the mouse hasn't moved so we unfix the box size
                                                                               (if (and (graphics-screen-box?
                                                                                         (bp-screen-box mouse-bp))
                                                                                        (graphics-box? actual-box))
                                                                                 (modified actual-box)
                                                                                 (progn (set-scroll-to-actual-row screen-box nil)
                                                                                        (set-fixed-size actual-box nil nil))))
                                                                          (t
                                                                           ;; make sure the mouse ended up in
                                                                           ;; a reasonable place
                                                                           (set-fixed-size actual-box
                                                                                           (max minimum-track-wid
                                                                                                   (- final-x box-window-x (- pixel-correction)))
                                                                                           (max minimum-track-hei
                                                                                                   (- final-y box-window-y (- pixel-correction))))
                                                                           (when (and (data-box? actual-box)
                                                                                      (auto-fill? actual-box))
                                                                             ;; don't fill doit boxes !!
                                                                             (com-fill-box actual-box))
                                                                           (modified actual-box))))))))))
  boxer-eval::*novalue*)

(defun status-line-size-report (screen-box wid hei)
  (multiple-value-bind (lef top rig bot)
                       (box-borders-widths (box-type screen-box) screen-box)
                       (let ((reporting-wid (- wid lef rig))
                             (reporting-hei (- hei top bot)))
                         (status-line-display 'boxer-editor-error
                                              (if (graphics-screen-box? screen-box)
                                                (format nil "New Size will be: ~D x ~D"
                                                        reporting-wid reporting-hei)
                                                (multiple-value-bind (cwid chei)
                                                                     (current-font-values)
                                                                     (format nil "New Size will be: ~D x ~D"
                                                                             (round reporting-wid cwid)
                                                                             (floor (+ 2 reporting-hei) chei))))))))

(defmacro mouse-corner-tracking ((corner) hilite-fun screen-box)
  (let ((delta-x (gensym)) (delta-y (gensym))
                           (box-window-x (gensym)) (box-window-y (gensym))
                           (width (gensym)) (height (gensym)))
    (ecase corner
           (:top-left
            `(multiple-value-bind (,box-window-x ,box-window-y)
                                  (xy-position ,screen-box)
                                  (multiple-value-bind (,delta-x ,delta-y ,width ,height)
                                                       (tl-corner-tracking-info ,screen-box)
                                                       (track-mouse-area ,hilite-fun
                                                                          :x (+ ,box-window-x ,delta-x)
                                                                          :y (+ ,box-window-y ,delta-y)
                                                                          :width ,width :height ,height))))
           (:top-right
            `(multiple-value-bind (,box-window-x ,box-window-y)
                                  (xy-position ,screen-box)
                                  (multiple-value-bind (,delta-x ,delta-y ,width ,height)
                                                       (tr-corner-tracking-info ,screen-box)
                                                       (track-mouse-area ,hilite-fun
                                                                          :x (+ ,box-window-x ,delta-x)
                                                                          :y (+ ,box-window-y ,delta-y)
                                                                          :width ,width :height ,height))))
           (:bottom-left
            `(multiple-value-bind (,box-window-x ,box-window-y)
                                  (xy-position ,screen-box)
                                  (multiple-value-bind (,delta-x ,delta-y ,width ,height)
                                                       (bl-corner-tracking-info ,screen-box)
                                                       (track-mouse-area ,hilite-fun
                                                                          :x (+ ,box-window-x ,delta-x)
                                                                          :y (+ ,box-window-y ,delta-y)
                                                                          :width ,width :height ,height))))
           (:bottom-right
            `(multiple-value-bind (,box-window-x ,box-window-y)
                                  (xy-position ,screen-box)
                                  (multiple-value-bind (,delta-x ,delta-y ,width ,height)
                                                       (br-corner-tracking-info ,screen-box)
                                                       (track-mouse-area ,hilite-fun
                                                                          :x (+ ,box-window-x ,delta-x)
                                                                          :y (+ ,box-window-y ,delta-y)
                                                                          :width ,width :height ,height)))))))

(defboxer-command com-mouse-br-corner-collapse-box (&optional (window *boxer-pane*)
                                                              (x (bw::boxer-pane-mouse-x))
                                                              (y (bw::boxer-pane-mouse-y))
                                                              (mouse-bp
                                                               (mouse-position-values x y))
                                                              (click-only? t))
  "make the box one size larger"
  window x y  ; (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (mouse-corner-tracking (:bottom-right) #'shrink-corner-fun screen-box))
      (let ((new-box (bp-box mouse-bp))
            (new-row (bp-row mouse-bp))
            (new-cha-no (bp-cha-no mouse-bp)))
        (when (and (not-null new-row)
                   (box? new-box))
          (unless (and (not (eq screen-box (outermost-screen-box)))
                       (and screen-box
                            (shrunken? (screen-obj-actual-obj screen-box))))
            (send-exit-messages new-box screen-box)
            (move-point-1 new-row new-cha-no screen-box)
            (com-collapse-box))))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-tl-corner-collapse-box (&optional (window *boxer-pane*)
                                                              (x (bw::boxer-pane-mouse-x))
                                                              (y (bw::boxer-pane-mouse-y))
                                                              (mouse-bp
                                                               (mouse-position-values x y))
                                                              (click-only? t))
  "make the box one size larger"
  window x y  ; (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (mouse-corner-tracking (:top-left) #'shrink-corner-fun screen-box))
      (let ((new-box (bp-box mouse-bp))
            (new-row (bp-row mouse-bp))
            (new-cha-no (bp-cha-no mouse-bp)))
        (when (and (not-null new-row)
                   (box? new-box))
          (let* ((edbox (and screen-box (screen-obj-actual-obj screen-box)))
                 (ds (and edbox (display-style edbox))))
            (cond ((and (not (eq screen-box (outermost-screen-box)))
                        (eq ds :supershrunk)))
              ((and (not (eq screen-box (outermost-screen-box)))
                    (eq ds :shrunk))
               (com-collapse-box edbox))
              (t
               (send-exit-messages new-box screen-box)
               (move-point-1 new-row new-cha-no screen-box)
               (com-collapse-box))))))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-br-corner-shrink-box (&optional (window *boxer-pane*)
                                                            (x (bw::boxer-pane-mouse-x))
                                                            (y (bw::boxer-pane-mouse-y))
                                                            (mouse-bp
                                                             (mouse-position-values x y))
                                                            (click-only? t))
  "make the box one size larger"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (mouse-corner-tracking (:bottom-right) #'shrink-corner-fun screen-box))
      (let ((new-box (bp-box mouse-bp))
            (new-row (bp-row mouse-bp))
            (new-cha-no (bp-cha-no mouse-bp)))
        (when (and (not-null new-row)
                   (box? new-box))
          (let* ((edbox (and screen-box (screen-obj-actual-obj screen-box)))
                 (ds (and edbox (display-style edbox))))
            (cond ((and (not (eq screen-box (outermost-screen-box)))
                        (eq ds :supershrunk)))
              ((and (not (eq screen-box (outermost-screen-box)))
                    (eq ds :shrunk))
               (com-shrink-box edbox))
              (t
               (send-exit-messages new-box screen-box)
               (move-point-1 new-row new-cha-no screen-box)
               (com-shrink-box))))))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-tl-corner-super-shrink-box (&optional (window *boxer-pane*)
                                                                  (x (bw::boxer-pane-mouse-x))
                                                                  (y (bw::boxer-pane-mouse-y))
                                                                  (mouse-bp
                                                                   (mouse-position-values x y))
                                                                  (click-only? t))
  "make the box one size larger"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (mouse-corner-tracking (:top-left) #'shrink-corner-fun screen-box))
      (let ((new-box (bp-box mouse-bp))
            (new-row (bp-row mouse-bp))
            (new-cha-no (bp-cha-no mouse-bp)))
        (when (and (not-null new-row)
                   (box? new-box))
          (unless (and (not (eq screen-box (outermost-screen-box)))
                       screen-box
                       (eq :supershrunk
                           (display-style (screen-obj-actual-obj screen-box))))
            (send-exit-messages new-box screen-box)
            (move-point-1 new-row new-cha-no screen-box)
            (com-super-shrink-box))))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-tr-corner-expand-box (&optional (window *boxer-pane*)
                                                            (x (bw::boxer-pane-mouse-x))
                                                            (y (bw::boxer-pane-mouse-y))
                                                            (mouse-bp
                                                             (mouse-position-values x y))
                                                            (click-only? t))
  "make the box one size larger"
  window x y  ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (mouse-corner-tracking (:top-right) #'expand-corner-fun screen-box))
      (let ((old-box (point-box))
            (new-box (bp-box mouse-bp))
            (new-row (bp-row mouse-bp))
            (mouse-screen-box (bp-screen-box mouse-bp))
            (new-cha-no (bp-cha-no mouse-bp)))
        (when (and (not-null new-row) (box? new-box))
          (unless (eq old-box new-box)
            (send-exit-messages new-box mouse-screen-box)
            (enter new-box (not (superior? old-box new-box))))
          (move-point-1 new-row new-cha-no mouse-screen-box)
          (com-expand-box)))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-br-corner-expand-box (&optional (window *boxer-pane*)
                                                            (x (bw::boxer-pane-mouse-x))
                                                            (y (bw::boxer-pane-mouse-y))
                                                            (mouse-bp
                                                             (mouse-position-values x y))
                                                            click-only?)
  "make the box one size larger"
  window x y  ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (mouse-corner-tracking (:bottom-right) #'expand-corner-fun screen-box))
      (let ((old-box (point-box))
            (new-box (bp-box mouse-bp))
            (new-row (bp-row mouse-bp))
            (mouse-screen-box (bp-screen-box mouse-bp))
            (new-cha-no (bp-cha-no mouse-bp)))
        (when (and (not-null new-row) (box? new-box))
          (unless (eq old-box new-box)
            (send-exit-messages new-box mouse-screen-box)
            (enter new-box (not (superior? old-box new-box))))
          (move-point-1 new-row new-cha-no mouse-screen-box)
          (com-expand-box)))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-br-corner-set-outermost-box (&optional (window *boxer-pane*)
                                                                   (x (bw::boxer-pane-mouse-x))
                                                                   (y (bw::boxer-pane-mouse-y))
                                                                   (mouse-bp
                                                                    (mouse-position-values x y))
                                                                   (click-only? t))
  "make the box one size larger"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (mouse-corner-tracking (:bottom-right) #'expand-corner-fun screen-box))
      (let ((old-box (point-box))
            (new-box (bp-box mouse-bp))
            (new-row (bp-row mouse-bp))
            (mouse-screen-box (bp-screen-box mouse-bp))
            (new-cha-no (bp-cha-no mouse-bp)))
        (when (and (not-null new-row) (box? new-box))
          (unless (eq old-box new-box)
            (send-exit-messages new-box mouse-screen-box)
            (enter new-box (not (superior? old-box new-box))))
          (move-point-1 new-row new-cha-no mouse-screen-box)
          (if (and (graphics-box? new-box)
                   (display-style-graphics-mode? (display-style-list new-box)))
            (com-expand-box)
            (com-set-outermost-box))))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-tr-corner-toggle-closet (&optional (window *boxer-pane*)
                                                               (x (bw::boxer-pane-mouse-x))
                                                               (y (bw::boxer-pane-mouse-y))
                                                               (mouse-bp
                                                                (mouse-position-values x y))
                                                               click-only?)
  "Open the closet if it is closed and
   close the closet if it is open."
  window x y click-only?;  (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (cond ((closet-locked? screen-box)
           (when *warn-about-disabled-commands*
             (boxer-editor-warning "The Closet is currently locked")))
      ((or ;click-only?     ; try and suppress accidental closet clicks
           (mouse-corner-tracking (:top-right) #'toggle-corner-fun screen-box))
       (let ((old-box (point-box))
             (new-box (bp-box mouse-bp))
             (new-row (bp-row mouse-bp))
             (mouse-screen-box (bp-screen-box mouse-bp))
             (new-cha-no (bp-cha-no mouse-bp)))
         (when (and (not-null new-row) (box? new-box))
           (unless (eq old-box new-box)
             (send-exit-messages new-box mouse-screen-box)
             (enter new-box (not (superior? old-box new-box))))
           (move-point-1 new-row new-cha-no mouse-screen-box)
           (com-toggle-closets))))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-tl-corner-toggle-closet (&optional (window *boxer-pane*)
                                                               (x (bw::boxer-pane-mouse-x))
                                                               (y (bw::boxer-pane-mouse-y))
                                                               (mouse-bp
                                                                (mouse-position-values x y))
                                                               (click-only? t))
  "Open the closet if it is closed and
   close the closet if it is open."
  window x y click-only?;  (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (cond ((closet-locked? screen-box)
           (when *warn-about-disabled-commands*
             (boxer-editor-warning "The Closet is currently locked")))
      ((or ;click-only?     ; try and suppress accidental closet clicks
           (mouse-corner-tracking (:top-left) #'toggle-corner-fun screen-box))
       (let ((old-box (point-box))
             (new-box (bp-box mouse-bp))
             (new-row (bp-row mouse-bp))
             (mouse-screen-box (bp-screen-box mouse-bp))
             (new-cha-no (bp-cha-no mouse-bp)))
         (when (and (not-null new-row) (box? new-box))
           (unless (eq old-box new-box)
             (send-exit-messages new-box mouse-screen-box)
             (enter new-box (not (superior? old-box new-box))))
           (move-point-1 new-row new-cha-no mouse-screen-box)
           (com-toggle-closets))))))
  boxer-eval::*novalue*)


(defboxer-command com-mouse-bl-corner-toggle-box-view (&optional (window *boxer-pane*)
                                                                 (x (bw::boxer-pane-mouse-x))
                                                                 (y (bw::boxer-pane-mouse-y))
                                                                 (mouse-bp
                                                                  (mouse-position-values x y))
                                                                 (click-only? t))
  "Toggle the box view"
  window x y ;  (declare (ignore window x y))
  (reset-region)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box (screen-obj-actual-obj screen-box))
         (screen-objs (screen-objs box))
         (display-style (display-style-list box)))
    (cond ((and (not (display-style-graphics-mode? display-style))
                (not (graphics-box? box)))
           (boxer-editor-error "This box has no graphics"))
      ((eq screen-box *outermost-screen-box*)
       (boxer-editor-error "Can't toggle the view of the Outermost Box"))
      (t
       ;; modify the editor box
       (if (display-style-graphics-mode? display-style)
         (setf (display-style-graphics-mode? display-style) nil)
         (setf (display-style-graphics-mode? display-style) t))
       ;; then handle changes to the screen boxes
       (dolist (sb screen-objs)
         (toggle-type sb))
       (modified (box-screen-point-is-in)))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-border-name-box (&optional (window *boxer-pane*)
                                                       (x (bw::boxer-pane-mouse-x))
                                                       (y (bw::boxer-pane-mouse-y))
                                                       (mouse-bp
                                                        (mouse-position-values x y))
                                                       (click-only? t))
  "Bring up a name tab for the box"
  window x y   ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((screen-box (bp-screen-box mouse-bp)))
    (when (or click-only?
              (multiple-value-bind (box-window-x box-window-y)
                                   (xy-position screen-box)
                                   (multiple-value-bind (delta-x delta-y width height)
                                                        (name-tab-tracking-info screen-box)
                                                        (track-mouse-area #'default-gui-fun
                                                                          :x (+ box-window-x delta-x)
                                                                          :y (+ box-window-y delta-y)
                                                                          :width width
                                                                          :height height))))
      (if (eq (bp-box mouse-bp) *initial-box*)
        (boxer-editor-error  "You cannot name the outermost box")
        (let ((box-to-name (screen-obj-actual-obj (bp-screen-box mouse-bp))))
          (unless (row? (slot-value box-to-name 'name))
            (set-name box-to-name (make-name-row '())))
          (send-exit-messages box-to-name screen-box)
          (move-point-1 (slot-value box-to-name 'name)
                        0 (bp-screen-box mouse-bp))
          (modified box-to-name)))))
  boxer-eval::*novalue*)

(defboxer-command com-mouse-border-toggle-type (&optional (window *boxer-pane*)
                                                          (x (bw::boxer-pane-mouse-x))
                                                          (y (bw::boxer-pane-mouse-y))
                                                          (mouse-bp
                                                           (mouse-position-values x y))
                                                          (click-only? t))
  "Toggle the type of the box"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box)))
    (when (or (eq box-type 'data-box) (eq box-type 'doit-box))
      (toggle-type (bp-box mouse-bp))
      (mark-file-box-dirty (bp-box mouse-bp))))
  boxer-eval::*novalue*)



;;; Note: These scroll bar commands can now be triggerd by action in the horizontal
;;; as well as the (usual) vertical scroll bar
;;; The com-mouse-?-scroll commands dispatch to more specific action depending upon what
;;; scroll area was initially moused

(defboxer-command com-mouse-scroll-box (&optional (window *boxer-pane*)
                                                  (x (bw::boxer-pane-mouse-x))
                                                  (y (bw::boxer-pane-mouse-y))
                                                  (mouse-bp
                                                   (mouse-position-values x y))
                                                  (click-only? t))
  "Scroll or reposition the box"
  window ;; (declare (ignore window))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box)))
    ;; (unless (neq screen-box (point-screen-box)) ;; This was to only let you scroll the box if the point is in it
      (case (get-scroll-position x y screen-box box-type)
        (:v-bar (mouse-in-v-scroll-bar-internal screen-box x y click-only?))
        (:h-bar (mouse-in-h-scroll-bar-internal screen-box x y)))) ;)
  boxer-eval::*novalue*)

(defboxer-command com-sprite-follow-mouse (&optional (window *boxer-pane*)
                                                     (x (bw::boxer-pane-mouse-x))
                                                     (y (bw::boxer-pane-mouse-y))
                                                     (mouse-bp
                                                      (mouse-position-values x y))
                                                     (click-only? t))
  "Grabs a sprite with the mouse and moves it around"
  window x y click-only? ; (declare (ignore window x y click-only?))
  (let ((box (bp-box mouse-bp)))
    (when (sprite-box? box)
      (let ((*current-sprite* box))
        (bu::follow-mouse))))
  boxer-eval::*novalue*)
