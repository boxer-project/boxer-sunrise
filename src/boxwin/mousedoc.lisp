#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



Modification History (most recent at top)

 3/14/11 document-mouse-handler conditionalized for #+/-opengl
12/05/10 added popup info handling to the mouse-doc-status
10/16/10 undocument-mouse
 9/23/10 initial FULL OpenGL implementation
 6/15/05 document-mouse-dispatch & undocument-mouse-dispatch changed to use new
         special case mouse docs for auto resize, outermost & initial boxes
 6/09/05 mouse doc string tuning: "Other choices" changed to "More Choices"
         also added special cases for top left of initial-box (can't shrink it)
         and top right of outermost box (can't expand it)
         document-top-{left,right}-mouse changed to check for & handle these cases
 2/12/05 *mouse-doc-wait-time* reduced from 1 to .5 secs
         maybe-undocument-mouse added for more frequent checks on the validity
         of existing mouse docs
         check to see if mouse is still on the same spot added to popup-doc-delay
 8/24/04 used new var *change-mouse-on-hotspots* to conditionalize mouse cursor
         changing
 6/18/04 conditionalized mouse doc strings for macs ("Hold" instead of
         "Right Click" for "Other Choices")
 6/15/04 started file (unifying mouse documentation code from the various
         boxwin-xxx files

|#



(in-package :boxer-window)

;;;; Mouse Documentation

(defvar *document-mouse* t)

(defvar *current-mouse-doc* nil)
(defvar *current-mouse-string-id* 0)

(defvar *custom-string-id* 2001)
(defvar *custom-mouse-doc-string* "")

(defmacro def-mouse-doc-string (docfun id string )
  `(progn
     (defun ,docfun ()
       (unless (=& ,id *current-mouse-string-id*)
         (setq *current-mouse-string-id* ,id)
         (status-line-display :mouse-doc ,string)))))

(defun clear-mouse-doc-string ()
  (setq *current-mouse-string-id* 0)
  (boxer::status-line-undisplay :mouse-doc))

(def-mouse-doc-string mouse-doc-supershrink 1
  #+mcl "Click to Supershrink (Hold for more choices)"
  #-mcl "Click to Supershrink (Right click for more choices)")

(def-mouse-doc-string mouse-doc-shrink 2
  #+mcl "Click to Shrink (Hold for more choices)"
  #-mcl "Click to Shrink (Right click for more choices)")

(def-mouse-doc-string mouse-doc-hotspot-off 3 "Press to enable Clicking")

(def-mouse-doc-string mouse-doc-expand 4
  #+mcl "Click to Expand (Hold for more choices)"
  #-mcl "Click to Expand (Right Click for more choices)")

(def-mouse-doc-string mouse-doc-fullscreen 5
  #+mcl "Click to Expand to Fullscreen (Hold for more choices)"
  #-mcl "Click to Expand to Fullscreen (Right Click for more choices)")

(def-mouse-doc-string mouse-doc-graphics-flip 6
  #+mcl "Flip to Graphics (Hold for more choices)"
  #-mcl "Flip to Graphics (Right Click for more choices)")

(def-mouse-doc-string mouse-doc-text-flip 7
  #+mcl "Flip to Text (Hold for more choices)"
  #-mcl "Flip to Text (Right Click for more choices)")

(def-mouse-doc-string mouse-doc-resize 8 "Drag to Resize (Click to Auto-Size)")

(def-mouse-doc-string mouse-doc-resize-off 9 "Press to Select Manual Sizing")

(def-mouse-doc-string mouse-doc-type 10
  "Press to Change Type and Other Properties")

(def-mouse-doc-string mouse-doc-name 11  "Give this box a name")

(def-mouse-doc-string mouse-doc-graphics 12 "Graphics Mouse Click")

(def-mouse-doc-string mouse-doc-sprite 13 "Sprite Mouse Click")

;; special cases for top left & top right

(def-mouse-doc-string mouse-doc-tl-menu-only 14
  #+mcl "Hold for more choices"
  #-mcl "Right Click for more choices")

(def-mouse-doc-string mouse-doc-tr-menu-only 15
  #+mcl "Hold for more choices"
  #-mcl "Right Click for more choices")

(defparameter *mouse-doc-wait-time* .5
  "time in seconds to wait before popping up mouse documentation")

(defvar *change-mouse-on-hotspots* nil)

;; Non OpenGL needs to keep track of a pixmap which is
;; a snapshot of the highlighted part of the box before the highlight
;;
;; OpenGL uses 2 slots which cache the X,Y coords of where to draw the
;; highlighting

(defvar *mouse-doc-status* (make-array #+opengl 7 #-opengl 3))

;; mouse status is a vector
(defun mouse-doc-status () *mouse-doc-status*)
(defun set-mouse-doc-status (place screen-box)
  (setf (svref *mouse-doc-status* 0) place
        (svref *mouse-doc-status* 1) screen-box))

;; deallocate backing store here ? (can't, need originating bitmap)
(defun clear-mouse-doc-status ()
  (setf (svref *mouse-doc-status* 0) nil
        (svref *mouse-doc-status* 1) nil
        (svref *mouse-doc-status* 2) nil
        #+opengl (svref *mouse-doc-status* 3) #+opengl nil
        #+opengl (svref *mouse-doc-status* 4) #+opengl nil
        #+opengl (svref *mouse-doc-status* 5) #+opengl nil
        #+opengl (svref *mouse-doc-status* 6) #+opengl nil
        ))

;; field selectors and mutators
(defun mouse-doc-status-place () (svref& *mouse-doc-status* 0))
(defun mouse-doc-status-screen-box ()  (svref& *mouse-doc-status* 1))

#+opengl
(defun mouse-doc-status-x () (svref& *mouse-doc-status* 2))
#+opengl
(defun mouse-doc-status-y () (svref& *mouse-doc-status* 3))

#-opengl
(defun mouse-doc-status-backing () (svref& *mouse-doc-status* 2))


(defun set-mouse-doc-status-place (newplace)
  (setf (svref& *mouse-doc-status* 0) newplace))
(defun set-mouse-doc-status-screen-box (newsb)
  (setf (svref& *mouse-doc-status* 1) newsb))
#+opengl
(defun set-mouse-doc-status-xy (x y)
  (setf (svref& *mouse-doc-status* 2) x
        (svref& *mouse-doc-status* 3) y))
#-opengl
(defun set-mouse-doc-status-backing (newback)
  (setf (svref& *mouse-doc-status* 2) newback))

#+opengl
(defun mouse-doc-status-popup-doc () (svref& *mouse-doc-status* 4))
#+opengl
(defun mouse-doc-status-popup-x () (svref& *mouse-doc-status* 5))
#+opengl
(defun mouse-doc-status-popup-y () (svref& *mouse-doc-status* 6))

#+opengl
(defun set-mouse-doc-popup-info (doc px py)
  (setf (svref& *mouse-doc-status* 4) doc
        (svref& *mouse-doc-status* 5) px
        (svref& *mouse-doc-status* 6) py))

;;; here is the top level interface called by event handlers
;; this is suppose to wait for a second, then popup some doc
;; we perform the bulk of the work in the boxer process so we
;; won't lock up event processing during the wait and also so we
;; don't mess up event handling if there is an error
;;
;; There are 3 types of mouse documentation functions
;;  o The mouse-doc-{shrink,supershrink,expand,etc} functions display a
;;    documentation phrase in the status line
;;  o The highlight-doc-xxx functions are responsible for highlighting the
;;    sensitive area under the mouse and (perhaps) changing the mouse cursor
;;  o The popup-doc-xxx functions popup a terse doc message on the screen (bubble
;;    help)
;;

(defun document-mouse ()
  (when *document-mouse*
    (unless (member 'document-mouse-handler *boxer-eval-queue*)
      (queue-event 'document-mouse-handler))))

(defun document-mouse-handler ()
  (multiple-value-bind (place screen-box)
      (boxer::drawing-on-window (*boxer-pane*) (mouse-place))
    (cond ((and (eq place (mouse-doc-status-place))
                (eq screen-box (mouse-doc-status-screen-box))
                (or (null boxer::*popup-mouse-documentation?*)
                    (not (null boxer::*popup-doc-on?*))))
           ;; we are in the same place as existing documentation so do nothing
           )
          ((and (eq place (mouse-doc-status-place))
                (eq screen-box (mouse-doc-status-screen-box))
                boxer::*popup-mouse-documentation?*)
           ;; we are in the same place but the popup docs haven't fired
           #-opengl
           (boxer::drawing-on-window (*boxer-pane*)
             (document-mouse-dispatch place screen-box T))
           #+opengl
           (document-mouse-dispatch place screen-box T))
          (t
           ;; otherwise, remove any existing documentation
           (undocument-mouse)
           ;; now wait
           ;; another event has occurred
           (set-mouse-doc-status place screen-box)
           #-opengl
           (boxer::drawing-on-window (*boxer-pane*)
             (document-mouse-dispatch place screen-box))
           #+opengl
           (document-mouse-dispatch place screen-box)))))


;;; popup-doc-delay should return T to cancel any popup documentation
#+lispworks
(defun popup-doc-delay ()
  (let ((original-event-id (1+ (event-id))))
    (or
     (mp:process-wait-with-timeout "Mouse Documentation"
                                   *mouse-doc-wait-time*
                                   #'(lambda ()
                                       (> (event-id) original-event-id)))
     (neq (mouse-place) (mouse-doc-status-place)))))

#+mcl
(defun popup-doc-delay ()
  (let ((original-event-id (event-id)))
    (or
     (process-wait-with-timeout "Mouse Documentation"
                                (round (* 60 *mouse-doc-wait-time*))
                                #'(lambda ()
                                    (not (= (event-id) original-event-id))))
     (boxer::drawing-on-window (*boxer-pane*)
       ;; why is this neccessarry ? shouldn't we already be in a drawing-on-window ?
       ;; perhaps the process switch messes the graphics state up ?
       (neq (mouse-place) (mouse-doc-status-place))))))


;; top level, can be called from within an event handler
(defun undocument-mouse ()
  ;; check status, if doc'd, clear it
  (unless (null (mouse-doc-status-place))
    ;; erase hotspot and popup doc
    #-opengl ; only need to "undraw" for non OpenGL
    (boxer::drawing-on-window (*boxer-pane*) (undocument-mouse-dispatch))
    (clear-mouse-doc-status))
  (unless (null *current-mouse-doc*)
    (setq *current-mouse-doc* nil) (clear-mouse-doc-string))
    )

;; called in the main event loop to make sure that any posted documentation is
;; still relevant
(defun maybe-undocument-mouse ()
  (when (and *document-mouse* boxer::*popup-doc-on?*)
    (multiple-value-bind (place screen-box)
                         (boxer::drawing-on-window (*boxer-pane*) (mouse-place))
      (cond ((and (eq place (mouse-doc-status-place))
                  (eq screen-box (mouse-doc-status-screen-box))
                  )
             ;; we are in the same place as existing documentation so do nothing
             )
            ;; do we need to check for partial documentation states here ?
            (t (undocument-mouse))))))

;; have to be careful because we can move from the same corner of one box
;; to another.  E.g. top right shrunken to top right container
;; this is the main entry point
(defun document-mouse-dispatch (place screen-box &optional popup-only?)
  (case place
    (:top-left    (document-top-left-mouse screen-box popup-only?))
    (:top-right   (document-top-right-mouse screen-box popup-only?))
    (:bottom-left (document-bottom-left-mouse screen-box popup-only?))
    (:bottom-right
     (unless (eq *current-mouse-doc* :bottom-right)
       (setq *current-mouse-doc* :bottom-right)
       (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :hotspot)))
     (cond ((boxer::bottom-right-hotspot-active?
             (boxer::screen-obj-actual-obj screen-box))
            (unless popup-only? (mouse-doc-resize))
            (boxer::popup-doc-resize screen-box popup-only?))
           (t (unless popup-only? (mouse-doc-resize-off))
              (boxer::popup-doc-resize screen-box popup-only? t))))
    ((:type :port-target-type)
     (unless (eq *current-mouse-doc* :type-tab-doc)
       (setq *current-mouse-doc* :type-tab-doc)
       (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :type-tab)))
     (unless (eq (boxer::box-type screen-box) 'boxer::sprite-box)
       (unless popup-only? (mouse-doc-type))
       (boxer::popup-doc-toggle-type screen-box popup-only?)))
    ((:name-handle)
     (unless (eq *current-mouse-doc* :name-doc)
       (setq *current-mouse-doc* :name-doc)
        (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :name-box)))
     (mouse-doc-name))
    (:graphics
     (unless (eq *current-mouse-doc* :graphics)
       (setq *current-mouse-doc* :graphics)
        (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :normal)))
     (unless popup-only? (mouse-doc-graphics))
     ;(boxer::popup-doc-graphics screen-box popup-only?)
     )
    (:sprite
     (unless (eq *current-mouse-doc* :sprite)
       (setq *current-mouse-doc* :sprite)
        (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :normal)))
     (unless popup-only?
       (let ((custom-doc (safe-get-sprite-mouse-doc screen-box)))
         (if (null custom-doc)
             (mouse-doc-sprite)
           (unless (and (=& *current-mouse-string-id* *custom-string-id*)
                        (string= *custom-mouse-doc-string* custom-doc))
             (setq *current-mouse-string-id* *custom-string-id*)
             (boxer::boxer-editor-message
              (setq *custom-mouse-doc-string* custom-doc)))))))
    (t  #-opengl
        (when *change-mouse-on-hotspots*
          (set-mouse-cursor-internal *current-mouse-cursor*))
        (unless (null *current-mouse-doc*)
          (setq *current-mouse-doc* nil)
          (clear-mouse-doc-string)))))

#+opengl
(defun undocument-mouse-dispatch () (clear-mouse-doc-status)) ;(boxer::clear-mouse-docs))

#-opengl
(defun undocument-mouse-dispatch ()
  (let* ((place (mouse-doc-status-place))
         (screen-box (mouse-doc-status-screen-box))
         (edbox (cond ((boxer::screen-box? screen-box)
                       (boxer::screen-obj-actual-obj screen-box))
                      ((boxer::sprite-box? screen-box)
                       screen-box)
                      (t nil)))
         (target (when edbox (boxer::box-or-port-target edbox))))
    (case place
      (:top-left     (boxer::popup-undoc-shrink
                      screen-box
                      (and (eq (boxer::display-style edbox) :shrunk)
                           (not (eq screen-box (outermost-screen-box))))))
      (:top-right    (boxer::popup-undoc-expand
                      screen-box (neq (boxer::display-style edbox) :shrunk)))
      (:bottom-left  (unless (null (slot-value target 'boxer::graphics-sheet))
                       (boxer::popup-undoc-view-flip
                        screen-box (not (boxer::graphics-screen-box? screen-box)))))
      (:bottom-right (if (boxer::bottom-right-hotspot-active? edbox)
                       (boxer::popup-undoc-resize screen-box)
                       (boxer::popup-undoc-resize screen-box t)))
      ((:type :port-target-type) (boxer::popup-undoc-toggle-type screen-box))
      ;; future expansion...
      (:name-handle)
      (:graphics (boxer::popup-undoc-graphics screen-box))
      (:sprite))))

(defun safe-get-sprite-mouse-doc (sprite-box)
  (when (boxer::sprite-box? sprite-box)
    (let ((doc nil))
      (ignore-errors
       (setq doc
             (let* ((boxer-eval::*lexical-variables-root* sprite-box)
                    (docbox (boxer-eval::boxer-symeval 'bu::sprite-mouse-documentation)))
               (unless (eq docbox boxer-eval::*novalue*)
                 (boxer::box-text-string docbox)))))
      doc)))

(defun mouse-place ()
  (multiple-value-bind (xpos ypos)
      (boxer-pane-mouse-position)
    (multiple-value-bind (area box)
        (ignore-errors (boxer::mouse-documentation-area xpos ypos))
      (values area box))))

(defun document-top-left-mouse (screen-box popup-only?)
  (unless (eq *current-mouse-doc* :top-left)
    (setq *current-mouse-doc* :top-left)
    ;; leave mouse alone and highlight hotspot instead....
    ;(set-mouse-cursor-internal :shrinkspot)
    )
  (let ((edbox (boxer::screen-obj-actual-obj screen-box)))
    (cond ((not (boxer::top-left-hotspot-on? edbox)) (mouse-doc-hotspot-off))
          ((and (eq (boxer::display-style edbox) :shrunk)
                (not (eq screen-box (outermost-screen-box))))
           ;; this prints a relevant status line message
           (unless popup-only? (mouse-doc-supershrink))
           ;; this highlights the hotspot, and pops up a doc bubble
           (boxer::popup-doc-shrink screen-box T popup-only?))
          (t (unless popup-only?
               (if (eq edbox boxer::*initial-box*)
                 (mouse-doc-tl-menu-only)
                 (mouse-doc-shrink)))
             (boxer::popup-doc-shrink screen-box nil popup-only?)))))

(defun document-top-right-mouse (screen-box popup-only?)
  (unless (eq *current-mouse-doc* :top-right)
    (setq *current-mouse-doc* :top-right)
    (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :expandspot)))
  (let ((edbox (boxer::screen-obj-actual-obj screen-box)))
    (cond ((not (boxer::top-right-hotspot-on? edbox)) (mouse-doc-hotspot-off))
          ((and (null popup-only?) (eq screen-box (outermost-screen-box)))
           (mouse-doc-tr-menu-only))
          ((eq (boxer::display-style edbox) :shrunk)
           (unless popup-only? (mouse-doc-expand))
           (boxer::popup-doc-expand screen-box nil popup-only?))
          (t (unless popup-only? (mouse-doc-fullscreen))
             (boxer::popup-doc-expand screen-box t popup-only?)))))

(defun document-bottom-left-mouse (screen-box popup-only?)
  (let* ((edbox (box::screen-obj-actual-obj screen-box))
         (target (box::box-or-port-target edbox)))
    (cond ((not (boxer::bottom-left-hotspot-on? edbox))
           (unless (eq *current-mouse-doc* :bottom-left)
             (setq *current-mouse-doc* :bottom-left)
             (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :hotspot)))
           (mouse-doc-hotspot-off))
          ((not (boxer::graphics-sheet? (boxer::graphics-sheet target)))
           (unless (eq *current-mouse-doc* :normal)
             (setq *current-mouse-doc* :normal)
             (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :normal)))
           (unless (=& *current-mouse-string-id* 0)
             (clear-mouse-doc-string)))
          ((boxer::graphics-screen-box? screen-box)
           (unless (eq *current-mouse-doc* :bottom-left)
             (setq *current-mouse-doc* :bottom-left)
             (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :hotspot)))
           (unless popup-only? (mouse-doc-text-flip))
           (boxer::popup-doc-view-flip screen-box nil popup-only?))
          (t
           (unless (eq *current-mouse-doc* :bottom-left)
             (setq *current-mouse-doc* :bottom-left)
             (when *change-mouse-on-hotspots* (set-mouse-cursor-internal :hotspot)))
           (unless popup-only? (mouse-doc-graphics-flip))
           (boxer::popup-doc-view-flip screen-box t popup-only?)))))

