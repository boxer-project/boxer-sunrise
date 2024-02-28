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

(defun repaint-window (&OPTIONAL (WINDOW *BOXER-PANE*) (flush-buffer? t) &KEY (process-state-label "stopped"))
  (opengl:rendering-on (window)
    (bw::check-for-window-resize)
    (REDISPLAYING-WINDOW (WINDOW)
                         (clear-window window)
                         (repaint-guts)
                         (repaint-mouse-docs)
                         (repaint-dev-overlay process-state-label)
                         (when flush-buffer? (swap-graphics-buffers window)))))

(defvar *use-repaint2024* t)

;;; called also by printing routines.
(defun repaint-guts ()
  (unless (null *outermost-screen-box*)
    ;; can happen asynch, during window startup if window systems tries to update before
    ;; all the boxer innards are created
    (if *use-repaint2024*
      (multiple-value-bind (max-wid max-hei)
                       (outermost-screen-box-size *redisplay-window*)
                       (repaint-fill-dimensions *outermost-screen-box* max-wid max-hei))
      (top-level-repaint-pass-1))
    (top-level-repaint-pass-2)))

(defun update-window-title ()
  (set-window-name (current-file-status (point-box))))

(defun repaint-internal (&optional just-windows?)
  (repaint-window *boxer-pane* nil)
  (dolist (region *region-list*)
    (when (not (null region)) (interval-update-repaint-all-rows region)))
  ;; comment out next line for outermost box save document, updates will
  ;; occur inside of set-outermost-box instead...
  (when (bp? *point*)
    (capi:apply-in-pane-process *boxer-pane* 'update-window-title)
    (unless just-windows?
      (repaint-cursor *point* nil)))
  ;; swap buffers here, after all drawing is complete
  (swap-graphics-buffers *boxer-pane*))

(defun repaint (&optional just-windows?)
  (opengl:rendering-on (*boxer-pane*)
    (update-matrices-ubo bw::*boxgl-device*)
    (when *reload-shaders*
      (update-boxgl-programs)
      (setf *reload-shaders* nil))
    (repaint-internal just-windows?)))

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
                                                                                     (+ c-height on-scroll-row-offset)))))))))

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
