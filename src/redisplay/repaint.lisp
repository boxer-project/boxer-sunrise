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

;;;; top level function

;;; this file approximates the functionality of the old redisp file.

;; call this, then swap buffers
;; rebuild the entire screen, later wewill want to be more targeted, especially for
;; speed inside of eval when we can localize to fixed areas i.e. inside graphics boxes



;;; 1st pass: walk the screen hierarchy, establishing positions and sizes
;;; calculate active sprite shapes
;; must occur in user process and be able to handle errors (especially in calculating
;; sprite shapes
;;
;;; GPU Caching strategies:
;;    x fonts
;;    o sprite shapes
;;        smart sprite caching - constant shape, never cache flag....
;;    o boxes
;;    o parts of boxes (corners, labels, etc)
;;

;; ??? how to ID portions of the screen which have changed....
;; have to assume any modified row is changed
;; buried active sprite problem (no modified propagation for programatic shapes
;; like [type random 4] or [type a + b] when a,b are not in sprite's hierarchy)

;; unmodified rows ABOVE modified rows OK, unmodified rows below may have moved
;; Union of change rects:
;;  clear all changed rects, repaint, update-screen wwith rects
;;                       vs.
;;  clear-window, repaint, update-screen wwith rects
;;  (this is better if we transform stuff)

;; 1st cut, recalculate everything
;; 2nd cut, get smarter (need to hack deep active sprite problem)
;; if no deep active sprite, then use timestamps or force-repaint flag
(defmethod needs-repaint-pass-1? ((self screen-obj)
                                  &optional (max-wid nil) (max-hei nil)) t)
  ;; sgithens 2021-11-19 Experiementing with just always repainting pass 1
  ;; (cond ((or (<= (slot-value self 'tick)
  ;;                (actual-obj-tick (screen-obj-actual-obj self)))
  ;;            ;; actual obj has changed
  ;;            (not (null *complete-redisplay-in-progress?*))
  ;;            ;; got less room...
  ;;            (and (not-null max-wid) (< max-wid (slot-value self 'wid)))
  ;;            (and (not-null max-hei) (< max-hei (slot-value self 'hei)))
  ;;            ;; was clipped but got more room...
  ;;            (and (not-null (slot-value self 'x-got-clipped?))
  ;;                 (not-null max-wid) (> max-wid (slot-value self 'wid)))
  ;;            (and (not-null (slot-value self 'y-got-clipped?))
  ;;                 (not-null max-hei) (> max-hei (slot-value self 'hei)))
  ;;            ;; deep active sprite
  ;;            ;             (active-sprite-inside? self)
  ;;            )
  ;;        t)
  ;;   (t nil)))

;(defmethod needs-repaint-pass-1? ((self screen-obj)
;                                  &optional (max-wid nil) (max-hei nil))
;  (declare (ignore max-wid max-hei))
;  t)

;; if we intend to rebuild the entire screen with every key stroke,
;; this will always be T, we may want to be more selective in what we erase
;; and redraw for efficiency
;(defmethod needs-repaint-pass-2? ((self screen-obj))
;  (or (not-null (slot-value self 'needs-redisplay-pass-2?))
;      (not-null *complete-redisplay-in-progress?*)))

(defmethod needs-repaint-pass-2? ((self screen-obj)) T)

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

;(defmethod got-repainted ((self screen-box))
;  (call-next-method)
;  (setf (inf-shift self) nil))

;; Pass 1 should be accumalating changed rects...
(defmethod repaint-inferiors-pass-1-sb ((self screen-box)
                                        infs-new-max-wid infs-new-max-hei
                                        &optional
                                        (first-inf-x-offset 0) (first-inf-y-offset 0)
                                        (scroll-to-inf nil))
  (declare (values wid hei clipped-x? cliiped-y?))
  ;; borders size
  (with-slots (actual-obj box-type screen-rows scroll-x-offset scroll-y-offset)
    self
    (port-redisplaying-history (actual-obj)
                               (multiple-value-bind (il it ir ib)
                                                    (box-borders-widths box-type self)
                                                    (declare (ignore ir ib))
                                                    (cond ((and (port-box? actual-obj)
                                                                (port-has-been-displayed-enough? actual-obj))
                                                           ;; The Actual Box is part of a circular structure AND
                                                           ;; we have already displayed the port the required number
                                                           ;; of times, so we erase and remove whatever is in
                                                           ;; the box, then...
                                                           (when (and (not-null screen-rows)
                                                                      (not (box-ellipsis-style? screen-rows)))
                                                             ;; PORTNOTE: previously
                                                             ;;    erase-and-queue-for-deallocation-screen-rows-from
                                                             (queue-for-deallocation-screen-rows-from screen-rows 0)
                                                             (kill-screen-rows-from self 0))
                                                           ;; If there was an ellipsis marker already there, then
                                                           ;; we need to erase it in order to leave a blank space
                                                           ;; for the marker to be drawn during pass-2
                                                           ;; PORTNOTE: still needed ?
                                                           (when (box-ellipsis-style? screen-rows)
                                                             (funcall (get screen-rows 'erase-self) il it))
                                                           ;; put a Box ellipsis marker into the
                                                           ;; inferiors slot of the screen box
                                                           (setq screen-rows *box-ellipsis-current-style*)
                                                           ;; then return the necessary values
                                                           (multiple-value-bind (ewid ehei)
                                                                                (funcall (get *box-ellipsis-current-style* 'size))
                                                                                (values (min ewid infs-new-max-wid)
                                                                                        (min ehei infs-new-max-hei)
                                                                                        (>   ewid infs-new-max-wid)
                                                                                        (>   ehei infs-new-max-hei))))
                                                      (t
                                                       ;; If the port has an ellipsis marker when it shouldn't,
                                                       ;; then erase and remove it
                                                       (when (and (port-box? actual-obj)
                                                                  (box-ellipsis-style? screen-rows))
                                                         ;; PORTNOTE: still needed ?
                                                         ;(funcall (get screen-rows 'erase-self) il it)
                                                         (setq screen-rows (allocate-storage-vector 8)))
                                                       ;; here's the meat....
                                                       ;; Bind some useful vars for the main loop to side effect
                                                       (let ((infs-new-wid scroll-x-offset) ; handle scrolling
                                                                                            (infs-new-hei scroll-y-offset) ; handle scrolling
                                                                                            (infs-new-x-got-clipped? nil)
                                                                                            (infs-new-y-got-clipped? nil)
                                                                                            (inf-x-offset first-inf-x-offset)
                                                                                            (inf-y-offset first-inf-y-offset))
                                                         ;; at the start of each pass through the loop bind
                                                         ;; inf-screen-obj, and inf-actual-obj to the next obj in
                                                         ;; the screen and actual structures respectively.
                                                         (setq infs-new-max-wid (- infs-new-max-wid scroll-x-offset)
                                                               infs-new-max-hei (- infs-new-max-hei scroll-y-offset))
                                                         ;; reset the horizontal scroll info
                                                         (setf (slot-value self 'max-scroll-wid) nil)
                                                         (do* ((row-no 0 (1+& row-no))
                                                               (inf-actual-obj (or scroll-to-inf
                                                                                   (first-inferior-row actual-obj))
                                                                               (next-row inf-actual-obj))
                                                               (inf-screen-obj (screen-row-at-row-no self row-no)
                                                                               (screen-row-at-row-no self row-no)))
                                                           ;; If there are no more inferior screen-objs or if the
                                                           ;; current state of the clipping means that there is no
                                                           ;; room to display any more inferiors or the box is
                                                           ;; shrunken we quit. If there are any inferior
                                                           ;; screen-objs left in the old screen structure
                                                           ;; punt them.
                                                           ((or (null inf-actual-obj)
                                                                infs-new-y-got-clipped?
                                                                (eq (display-style self) ':shrunk))
                                                            (when (not-null inf-screen-obj)
                                                              (rp1-sb-punt-extra-screen-objs self inf-screen-obj))
                                                            (if (eq (display-style self) ':shrunk)
                                                              (values (min infs-new-max-wid *shrunk-box-wid*)
                                                                      (min infs-new-max-hei *shrunk-box-hei*)
                                                                      (> *shrunk-box-wid* infs-new-max-wid)
                                                                      (> *shrunk-box-hei* infs-new-max-hei))
                                                              (values infs-new-wid infs-new-hei
                                                                      infs-new-x-got-clipped?
                                                                      infs-new-y-got-clipped?)))
                                                           ;; if the screen & actual objs don't match, patch up screen
                                                           ;; structure
                                                           (when (or (null inf-screen-obj)
                                                                     (not (eq (screen-obj-actual-obj inf-screen-obj)
                                                                              inf-actual-obj)))
                                                             (setq inf-screen-obj
                                                                   (rp1-sb-patch-out-of-synch-lossage
                                                                    self inf-actual-obj inf-screen-obj
                                                                    inf-x-offset inf-y-offset)))
                                                           ;; at this point we know that inf-screen-obj and
                                                           ;; inf-actual-obj match. If it wants to let
                                                           ;; inf-screen-obj do :redisplay-pass-1.
                                                           (when (needs-repaint-pass-1? inf-screen-obj
                                                                                        infs-new-max-wid infs-new-max-hei)
                                                             (set-screen-obj-offsets inf-screen-obj inf-x-offset inf-y-offset)
                                                             (repaint-pass-1-sr inf-screen-obj
                                                                                infs-new-max-wid infs-new-max-hei))
                                                           ;; Finally, let inf-screen-obj make its contibution to the
                                                           ;; total new-wid, new-hei etc. of all the inf-screen-objs.
                                                           (multiple-value-setq (infs-new-wid infs-new-hei
                                                                                              infs-new-x-got-clipped?
                                                                                              infs-new-y-got-clipped?
                                                                                              inf-y-offset
                                                                                              infs-new-max-hei)
                                                                                ;; inf-screen-obj has to be a
                                                                                ;; screen-row, so we don't pass
                                                                                ;; INF-X-OFFSET and NEW-MAX-WID
                                                                                (rp1-sr-increment-superior-parameters
                                                                                 inf-screen-obj
                                                                                 infs-new-wid infs-new-hei
                                                                                 infs-new-x-got-clipped?
                                                                                 infs-new-y-got-clipped?
                                                                                 inf-y-offset
                                                                                 infs-new-max-hei))))))))))


(defmethod rp1-sb-punt-extra-screen-objs ((self screen-box) first-screen-row-to-punt)
  (let ((pos (screen-row-row-no self first-screen-row-to-punt)))
    ;; PORTNOTE: previously erase-and-queue-for-deallocation-screen-rows-from
    (queue-for-deallocation-screen-rows-from (slot-value self 'screen-rows) pos)
    (kill-screen-rows-from self pos)))

(defmethod rp1-sb-patch-out-of-synch-lossage ((self screen-box)
                                              inf-actual-obj inf-screen-obj
                                              screen-obj-x-offset
                                              screen-obj-y-offset)
  (let* ((matching-screen-obj
          (allocate-screen-obj-for-use-in inf-actual-obj
                                          (lowest-screen-box self)))
         (matching-screen-obj-superior
          (superior matching-screen-obj)))
    (cond ((eq matching-screen-obj-superior self)
           ;; the screen-obj which matches inf-actual-obj must be
           ;; farther along in this screen obj somewhere.
           ;; (One common cause for this is a deletion).
           (let ((start-pos (screen-row-row-no self inf-screen-obj))
                 (stop-pos  (screen-row-row-no self matching-screen-obj)))
             (queue-for-deallocation-screen-rows-from (slot-value self 'screen-rows)
                                                      start-pos stop-pos)
             (delete-screen-rows-from-to self start-pos stop-pos)
             (set-screen-obj-offsets matching-screen-obj
                                     screen-obj-x-offset screen-obj-y-offset)
             matching-screen-obj))
      (t
       ;; the screen-obj which matches inf-actual-obj is not in
       ;; use anywhere. This means inf-actual-obj is a new actual-
       ;; obj. (Probably the most common cause for this is an insertion)
       ;;Just insert the matching-screen-obj in the right place and we're done.
       (insert-screen-row self matching-screen-obj inf-screen-obj)
       (set-screen-obj-offsets matching-screen-obj
                               screen-obj-x-offset screen-obj-y-offset)
       matching-screen-obj))))

(defmethod rp1-sr-increment-superior-parameters ((self screen-row)
                                                 infs-new-wid infs-new-hei
                                                 infs-new-x-got-clipped?
                                                 infs-new-y-got-clipped?
                                                 inf-y-offset
                                                 infs-new-max-hei)
  (values (max infs-new-wid (slot-value self 'wid))
          (+ infs-new-hei (slot-value self 'hei))
          (or infs-new-x-got-clipped? (slot-value self 'x-got-clipped?))
          (or infs-new-y-got-clipped? (slot-value self 'y-got-clipped?))
          (+ inf-y-offset (slot-value self 'hei))
          (- infs-new-max-hei (slot-value self 'hei))))



;;;; Methods used for redisplaying ROWS
;;;
;;; The main difference between redisplaying rows and redisplaying boxes is
;;; that rows have to know what is going on with their inferiors because chas
;;; cannot take care of such things as clipping and drawing by themselves (like
;;; rows can).
;;; simple basic version, loop along maching screen-chas to actual-chas, if
;;; we encounter a box, adjust the box's offsets, then patch it into the row
;;; if we're doing selective erasing, should do it now...
;;;
;;; New: to support horizontal scrolling we have to walt to the end of the row even after we
;;; run out of screen space so we can calculate the size of the row in order to get the scroll info right

(defmethod repaint-inferiors-pass-1-sr ((self screen-row)
                                        infs-new-max-wid infs-new-max-hei
                                        &optional
                                        (first-inf-x-offset 0)
                                        (first-inf-y-offset 0))
  (with-slots (wid hei actual-obj screen-chas)
    self
    (let* ((infs-new-wid 0)
           (infs-new-hei 0)
           (infs-new-x-got-clipped? nil)
           (infs-new-y-got-clipped? nil)
           (new-baseline 0)
           (inf-x-offset first-inf-x-offset)
           (inf-y-offset first-inf-y-offset))
      ;; Watch out, this is a little flaky since we aren't going through the
      ;; normal deletion protocol for inferior screen boxes
      (clear-storage-vector screen-chas)

      (with-font-hacking ((row-fds actual-obj))
        (do* ((cha-no 0 (+& cha-no 1))
              (inf-actual-obj (cha-at-cha-no actual-obj cha-no)
                              (cha-at-cha-no actual-obj cha-no))
              (inf-screen-obj))
          ((or (null inf-actual-obj)
               infs-new-x-got-clipped?)
           ;; hmmm, sb's in the row wont get properly dealloced if they
           ;; fall off the end due to clipping....
           ;              (when (not-null inf-screen-obj)
           ;                (rp1-sr-punt-extra-screen-objs-from self cha-no
           ;                                                    infs-new-x-got-clipped?
           ;                                                    inf-x-offset inf-y-offset))
           ;; we've run out of screen space, but if there are any more actual objs, we need to
           ;; calculate the appoximate size of the remainder for the benefit of horizontal scrolling UI
           (cond ((null inf-actual-obj)
                  (values infs-new-wid infs-new-hei
                          infs-new-x-got-clipped? infs-new-y-got-clipped?
                          new-baseline
                          ;; it is possible for the row to be clipped AND have all screen-chas displayed
                          ;; for example, the last char on a row in a fixed size screen box
                          (if infs-new-x-got-clipped? infs-new-wid nil)))
             (t ; got clipped, see what's left for
                (values infs-new-wid infs-new-hei
                        infs-new-x-got-clipped? infs-new-y-got-clipped?
                        new-baseline
                        ;; new-max-scroll-width calculation
                        (let ((new-max-scroll  infs-new-wid))
                          (do* ((rest-cha-no cha-no (+ rest-cha-no 1))
                                (rest-cha (cha-at-cha-no actual-obj rest-cha-no)
                                          (cha-at-cha-no actual-obj rest-cha-no)))
                            ((null rest-cha) new-max-scroll)
                            (check-and-handle-font-changes rest-cha-no)
                            (incf new-max-scroll (if (screen-cha? rest-cha)
                                                   (cha-wid rest-cha)
                                                   ;; just guess for a box since we dont want to
                                                   ;; have to recurse for unseen boxes
                                                   (unseen-box-width rest-cha)))))))))
          ;; handle any font changes first
          (check-and-handle-font-changes cha-no)
          ;; now match screen and editor...
          (setq inf-screen-obj
                (if (cha? inf-actual-obj)
                  (make-screen-cha inf-actual-obj)
                  (let ((new-obj (allocate-screen-obj-for-use-in
                                  inf-actual-obj (lowest-screen-box self))))
                    (set-screen-obj-offsets new-obj inf-x-offset inf-y-offset)
                    new-obj)))
          (append-screen-cha self inf-screen-obj)
          ;; At this point we know that inf-screen-obj and inf-actual-obj
          ;; match. If it wants to (and is a screen-box) let inf-screen-obj do
          ;; redisplay-pass-1.
          (cond ((screen-cha? inf-screen-obj)
                 ;; must be a screen cha so the ROW has to check for clipping
                 ;; and increment its own infs-screen-objs parameters
                 (multiple-value-setq (infs-new-wid infs-new-hei
                                                    infs-new-x-got-clipped?
                                                    infs-new-y-got-clipped?
                                                    inf-x-offset infs-new-max-wid
                                                    new-baseline)
                                      (screen-cha-increment-superior-parameters
                                       inf-screen-obj
                                       infs-new-wid infs-new-hei
                                       infs-new-x-got-clipped? infs-new-y-got-clipped?
                                       inf-x-offset
                                       infs-new-max-wid infs-new-max-hei new-baseline)))
            (t
             ;; must be a box so let the box do some work...
             ;; that is, redisplay if it wants to and then make its
             ;; contribution to all the infs-screen-objs parameters
             (when (needs-repaint-pass-1? inf-screen-obj
                                          infs-new-max-wid infs-new-max-hei)
               (repaint-pass-1-sb inf-screen-obj
                                  infs-new-max-wid infs-new-max-hei))
             (multiple-value-setq (infs-new-wid infs-new-hei
                                                infs-new-x-got-clipped?
                                                infs-new-y-got-clipped?
                                                inf-x-offset infs-new-max-wid)
                                  (rp1-sb-increment-superior-parameters
                                   inf-screen-obj
                                   infs-new-wid            infs-new-hei
                                   infs-new-x-got-clipped? infs-new-y-got-clipped?
                                   inf-x-offset            infs-new-max-wid)))))))))

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

;; this mainly has to inform any further boxes that their services
;; will no longer be required....
(defmethod rp1-sr-punt-extra-screen-objs-from ((self screen-row)
                                               no-of-first-obj-to-punt
                                               clipped?
                                               x-coord y-coord)
  ;; erase the glyphs from the screen...
  (erase-screen-chas (slot-value self 'screen-chas) no-of-first-obj-to-punt
                     x-coord y-coord)
  ;; handle the deallocation of any boxes
  (queue-screen-objs-for-deallocation-from (slot-value self 'screen-chas)
                                           no-of-first-obj-to-punt)
  ;; remove the extra screen characters and boxes from the screen row
  (kill-screen-chas-from self no-of-first-obj-to-punt))

(defun screen-cha-increment-superior-parameters (screen-cha
                                                 infs-new-wid
                                                 infs-new-hei
                                                 infs-new-x-got-clipped?
                                                 infs-new-y-got-clipped?
                                                 inf-x-offset
                                                 infs-new-max-wid
                                                 infs-new-max-hei
                                                 current-baseline)
  (let ((wid (cha-wid screen-cha))
        (hei (cha-hei)))
    (values (+ infs-new-wid wid)
            (max infs-new-hei hei)
            (or infs-new-x-got-clipped? (> wid infs-new-max-wid))
            (or infs-new-y-got-clipped? (> hei infs-new-max-hei))
            (+ inf-x-offset wid)
            (- infs-new-max-wid wid)
            (max current-baseline (cha-ascent)))))

;;;only boxes and rows should be getting this message (NOT chas)
(defmethod rp1-sb-increment-superior-parameters ((self screen-box)
                                                 infs-new-wid infs-new-hei
                                                 infs-new-x-got-clipped?
                                                 infs-new-y-got-clipped?
                                                 inf-x-offset
                                                 infs-new-max-wid)
  (values (+ infs-new-wid (slot-value self 'wid))
          (max infs-new-hei (slot-value self 'hei))
          (or infs-new-x-got-clipped? (slot-value self 'x-got-clipped?))
          (or infs-new-y-got-clipped? (slot-value self 'y-got-clipped?))
          (+ inf-x-offset (slot-value self 'wid))
          (- infs-new-max-wid (slot-value self 'wid))))




(defmethod repaint-pass-1-sr ((self screen-row) max-wid max-hei)
  (with-drawing-inside-region ((slot-value self 'x-offset)
                               (slot-value self 'y-offset)
                               (slot-value self 'wid)
                               (slot-value self 'hei))
    ;; During redisplay-pass-1 the only region of the screen the redisplay
    ;; methods are allowed to draw in is the region of the screen currently
    ;; occupied by the screen obj.
    (multiple-value-bind (nw nh nxc nyc base total-wid)
                         (repaint-inferiors-pass-1-sr self max-wid max-hei)
                         (setf (slot-value self 'wid) nw)
                         (let ((1st-font (bfd-font-no (closest-bfd (slot-value self 'actual-obj) 0))))
                           (rebind-font-info (1st-font)
                                             (setf (slot-value self 'hei) (max nh (cha-hei))
                                                   (slot-value self 'baseline) (max base (cha-ascent)))))
                         (setf (slot-value self 'x-got-clipped?) nxc)
                         (setf (slot-value self 'y-got-clipped?) nyc)
                         (cond
                           ;; sgithens crash fix. Sometimes we get here and there is no screen-box on the screen-row
                           ((null (screen-box self)))
                           ((not (null nxc))
                                ;; if the row is clipped, update the max-scroll-wid slot
                                (update-scroll-wid (screen-box self) total-wid))
                           ((not (zerop (slot-value (screen-box self) 'scroll-x-offset)))
                            ;; update if we are already scrolled...
                            (update-scroll-wid (screen-box self) (slot-value self 'wid)))))))

(defmethod update-scroll-wid ((self screen-box) new-scroll-wid)
  (with-slots (max-scroll-wid) self
    (cond ((null max-scroll-wid)
           (setq max-scroll-wid new-scroll-wid))
      ((numberp max-scroll-wid)
       (setq max-scroll-wid (max max-scroll-wid new-scroll-wid))))))

(defmethod repaint-pass-1-sb ((self screen-box) max-wid max-hei)
  (with-slots (wid hei x-got-clipped? y-got-clipped?
                   actual-obj scroll-to-actual-row box-type)
    self
    (let ((new-box-type (class-name (class-of actual-obj)))
          (new-display-style (display-style actual-obj))
          (boxtop (boxtop actual-obj)))
      (cond ((and (eq new-display-style :supershrunk)
                  (not (eq self *outermost-screen-box*)))
             ;; SUPERSHRUNK
             (multiple-value-bind (sswid sshei)
                                  (super-shrunk-size)
                                  (unless (eq (display-style self) :supershrunk)
                                    (set-display-style self :supershrunk))
                                  (unless (and (= sswid wid) (= sshei hei))
                                    ;(erase-rectangle wid hei 0 0)
                                    (setq wid sswid hei sshei
                                          x-got-clipped? nil y-got-clipped? nil))
                                  ;; make sure to punt the inf screen objs or else they may try
                                  ;; and redisplay themselves (like after change-graphics)
                                  (unless (graphics-screen-box? self)
                                    (rp1-sb-punt-extra-screen-objs self (first-screen-row self)))
                                  (values sswid sshei nil nil)))
        ((and (eq new-display-style :shrunk)
              (not (eq self *outermost-screen-box*))
              (not (null boxtop)))
         ;; if there is a BOXTOP...
         (unless (eq (display-style self) :shrunk)
           (set-display-style self :shrunk))
         (multiple-value-bind (btwid bthei) (boxtop-size boxtop actual-obj)
                              ;; check for clipping
                              (setq wid (min max-wid btwid)
                                    hei (min max-hei bthei)
                                    x-got-clipped? (> btwid max-wid)
                                    y-got-clipped? (> bthei max-hei))
                              ;; we may want to put this into a redisplay-boxtop method
                              ;; if boxtops get too blinky
                              ;; NOTE: we always erase because the boxtop graphics may have been
                              ;; changed
                              ;(erase-rectangle wid hei 0 0)
                              ;; make sure to punt the inf screen objs or else they may try
                              ;; and redisplay themselves (like after change-graphics)
                              (unless (graphics-screen-box? self)
                                (rp1-sb-punt-extra-screen-objs self (first-screen-row self)))
                              (values wid hei x-got-clipped? y-got-clipped?)))
        (t
         (when (eq (display-style self) :supershrunk)
           (set-display-style self nil))
         (when (neq box-type new-box-type) (setq box-type new-box-type))
         (multiple-value-bind (l-border-wid t-border-wid
                                            r-border-wid b-border-wid)
                              (box-borders-widths new-box-type self)
                              ;; ought to hack the borders to return all
                              ;; 6 values at the same time
                              (multiple-value-bind (min-wid min-hei)
                                                   (box-borders-minimum-size new-box-type self)
                                                   (multiple-value-bind (fixed-wid fixed-hei)
                                                                        (fixed-size self)
                                                                        (let (;; If the screen-box has a fixed size, then the
                                                                              ;; fixed size effectively sets both upper and lower
                                                                              ;; limits on the size of the box.
                                                                              (real-max-wid (if (null fixed-wid)
                                                                                              max-wid
                                                                                              (min max-wid
                                                                                                   (+ fixed-wid
                                                                                                      l-border-wid
                                                                                                      r-border-wid))))
                                                                              (real-max-hei (if (null fixed-hei)
                                                                                              max-hei
                                                                                              (min max-hei
                                                                                                   (+ fixed-hei
                                                                                                      t-border-wid
                                                                                                      b-border-wid))))
                                                                              (real-min-wid (if (null fixed-wid)
                                                                                              min-wid
                                                                                              (max min-wid
                                                                                                   (+ fixed-wid
                                                                                                      l-border-wid
                                                                                                      r-border-wid))))
                                                                              (real-min-hei (if (null fixed-hei)
                                                                                              min-hei
                                                                                              (max min-hei
                                                                                                   (+ fixed-hei
                                                                                                      t-border-wid
                                                                                                      b-border-wid)))))
                                                                          (setf (slot-value self 'wid) (+ l-border-wid r-border-wid))
                                                                          (setf (slot-value self 'hei) (+ t-border-wid b-border-wid))
                                                                          ;; Now that we know how much room the borders are going
                                                                          ;; to take up, and we know the real max size of the
                                                                          ;; screen-box, we can go off and figure out how much
                                                                          ;; space the screen-rows are going to take up.
                                                                          (multiple-value-bind (rows-new-wid rows-new-hei
                                                                                                             rows-new-x-got-clipped?
                                                                                                             rows-new-y-got-clipped?)
                                                                                               (repaint-inferiors-pass-1-sb
                                                                                                self
                                                                                                (- real-max-wid wid)
                                                                                                (- real-max-hei hei)
                                                                                                l-border-wid
                                                                                                t-border-wid
                                                                                                scroll-to-actual-row)
                                                                                               (incf (slot-value self 'wid) rows-new-wid)
                                                                                               (incf (slot-value self 'hei) rows-new-hei)
                                                                                               ;; make sure that we are at least
                                                                                               ;; as big as our minimum size.
                                                                                               (setf (slot-value self 'wid)
                                                                                                     (min (max (screen-obj-wid self) real-min-wid)
                                                                                                          real-max-wid))
                                                                                               (setf (slot-value self 'hei)
                                                                                                     (min (max hei real-min-hei) real-max-hei))
                                                                                               (setf (slot-value self 'x-got-clipped?)
                                                                                                     (and (or (< real-max-wid real-min-wid)
                                                                                                              rows-new-x-got-clipped?)
                                                                                                          (or (not fixed-wid)
                                                                                                              (> (+ fixed-wid
                                                                                                                    l-border-wid r-border-wid)
                                                                                                                 max-wid))))
                                                                                               (setf (slot-value self 'y-got-clipped?)
                                                                                                     (and (or (< real-max-hei real-min-hei)
                                                                                                              rows-new-y-got-clipped?)
                                                                                                          (or (not fixed-hei)
                                                                                                              (> (+ fixed-hei
                                                                                                                    t-border-wid b-border-wid)
                                                                                                                 max-hei)))))
                                                                          ;; What hair!!! If we are changing size, then we need to
                                                                          ;; erase the part of our borders that need are going to
                                                                          ;; need erasing.
                                                                          (values wid hei x-got-clipped? y-got-clipped?))))))))))

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
                                 (when (needs-repaint-pass-2? inf-screen-obj)
                                   (repaint-pass-2-sr inf-screen-obj)))))))))

(defmethod repaint-inferiors-pass-2-sr ((self screen-row))
  (let* ((inf-x-offset 0)
         (inf-y-offset 0)
         (row-baseline (slot-value self 'baseline))
         (row-fds (row-fds (slot-value self 'actual-obj))))
    (do-screen-chas-with-font-info (inf-screen-obj (slot-value self 'screen-chas)
                                                   :index-var-name cha-no
                                                   :font-descriptors row-fds
                                                   :cha-drawing? t)
      (cond ((screen-cha? inf-screen-obj)
             ;; draw the char
             (draw-cha inf-screen-obj
                       inf-x-offset (+ row-baseline inf-y-offset))
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
         (when (needs-repaint-pass-2? inf-screen-obj)
           (repaint-pass-2-sb inf-screen-obj))
         ;; finally update inf-x-offset, screen-box style
         (incf inf-x-offset (screen-obj-wid inf-screen-obj)))))))

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
         (:supershrunk (draw-super-shrunk-box 0 0 box-type))
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
                                          (colorit  (- wid rig lef) (- hei bot top) lef top)))))
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
                       (let ((inside-wid (- (slot-value self 'wid) (+ ir il)))
                             (inside-hei (- (slot-value self 'hei) (+ ib it))))
                         (with-pen-color (*gray*)
                           (draw-rectangle inside-wid inside-hei il it)))))



;;;redisplay for graphics boxes

(defmethod repaint-inferiors-pass-1-sb ((self graphics-screen-box)
                                        infs-new-max-wid infs-new-max-hei
                                        &optional
                                        (first-inf-x-offset 0)
                                        (first-inf-y-offset 0)
                                        ignore)
  (declare (ignore ignore))
  (let* ((graphics-sheet (graphics-sheet (screen-obj-actual-obj self)))
         (desired-wid (graphics-sheet-draw-wid graphics-sheet))
         (desired-hei (graphics-sheet-draw-hei graphics-sheet)))
    ;; first make-sure that there is a screen object for the graphics sheet
    (when (not (graphics-screen-sheet? (screen-sheet self)))
      (let ((screen-sheet (allocate-screen-sheet-for-use-in graphics-sheet
                                                            self)))
        (set-screen-sheet self screen-sheet)))
    (let ((screen-sheet (screen-sheet self)))
      ;; now adjust the slots of the graphics-screen-sheet
      (unless (= first-inf-x-offset
                 (graphics-screen-sheet-x-offset screen-sheet))
        (set-graphics-screen-sheet-x-offset screen-sheet first-inf-x-offset))
      (unless (= first-inf-y-offset
                 (graphics-screen-sheet-y-offset screen-sheet))
        (set-graphics-screen-sheet-y-offset screen-sheet
                                            first-inf-y-offset))
      (setf (graphics-screen-sheet-actual-obj screen-sheet) graphics-sheet))
    ;; make sure we have the Right graphics-sheet
    (unless (eq graphics-sheet
                (graphics-screen-sheet-actual-obj (screen-sheet self)))
      (setf (graphics-screen-sheet-actual-obj (screen-sheet self))
            graphics-sheet)
      )
    ;; error check, remove this SOON !!!!!!!
    (IF (NOT (GRAPHICS-SCREEN-SHEET? (SCREEN-ROWS SELF)))
        (BARF "The object ~S, inside of ~S is not a GRAPHICS-SHEET. "
              (SCREEN-ROWS SELF) SELF)
        (VALUES (min desired-wid infs-new-max-wid)	;width of the innards
                ;; either there is enough room for the entire bit-array to
                ;; be displayed or else we return whatever room we are given
                (min desired-hei infs-new-max-hei)	;height of the innards
                ;; same argument as above
                (> desired-wid infs-new-max-wid)	;x-got-clipped?
                (> desired-hei infs-new-max-hei)))))	;y-got-clipped?

(defmethod repaint-inferiors-pass-2-sb ((SELF GRAPHICS-SCREEN-BOX))
  (LET ((GRAPHICS-SHEET (GRAPHICS-SCREEN-SHEET-ACTUAL-OBJ
                          (SCREEN-SHEET SELF)))
        (av-info (av-info (slot-value self 'actual-obj))))
         (multiple-value-bind (x y)
                              (graphics-screen-sheet-offsets (screen-sheet self))
                              (multiple-value-bind (il it ir ib)
                                                   (box-borders-widths (slot-value self 'box-type) self)
                                                   (let ((inner-width  (min (- (slot-value self 'wid) il ir)
                                                                            (graphics-sheet-draw-wid graphics-sheet)))
                                                         (inner-height (min (- (slot-value self 'hei) it ib)
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
                                                             (#-X BITBLT-TO-SCREEN #+(and SUN X) bitblt-pixrect-to-screen
                                                               (min inner-width  (offscreen-bitmap-width  ba))
                                                               (min inner-height (offscreen-bitmap-height ba))
                                                               ba 0 0 0 0)))
                                                         ;; then handle any sprite graphics...
                                                         (unless (null (graphics-sheet-graphics-list graphics-sheet))
                                                           (redisplay-graphics-sheet graphics-sheet self)))))))))



;;;; Screen Sprites....

;; sprites can (and usually DO) extend into negative x, y
;; so in editor space, they have to be drawn with an offset
;; so after calculating the dimensions of the sprite, calculate it's
;; center offset
(defmethod repaint-pass-1-sb ((self sprite-screen-box) max-wid max-hei)
  ;; subsprites
  ;; shape
  )

(defmethod repaint-pass-2-sb ((self sprite-screen-box))
  )



(defun repaint-window (&OPTIONAL (WINDOW *BOXER-PANE*) (flush-buffer? t) &KEY (process-state-label "stopped"))
  (bw::check-for-window-resize)
  (REDISPLAYING-WINDOW (WINDOW)
                         (clear-window window)
                         (repaint-guts)
                         (repaint-mouse-docs)
                         (repaint-dev-overlay process-state-label)
                         (when flush-buffer? (swap-graphics-buffers window))))

;;; called also by printing routines.
(defun repaint-guts ()
  (unless (null *outermost-screen-box*)
    ;; can happen asynch, during window startup if window systems tries to update before
    ;; all the boxer innards are created
    (top-level-repaint-pass-1)
    (top-level-repaint-pass-2)))

(defun top-level-repaint-pass-1 ()
  (multiple-value-bind (max-wid max-hei)
                       (outermost-screen-box-size *redisplay-window*)
                       (when (needs-repaint-pass-1? *outermost-screen-box* max-wid max-hei)
                         (repaint-pass-1-sb *outermost-screen-box* max-wid max-hei))))

;;
(defun top-level-repaint-pass-2 ()
  (when (needs-repaint-pass-2? *outermost-screen-box*)
    (repaint-pass-2-sb *outermost-screen-box*)))

;; if not fullscreen, pass-1 should clear changed areas
;; Note: should scrolling ops, pass in the box being scrolled as the changed area ?


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
      (swap-graphics-buffers *boxer-pane*))
     )

(defun repaint (&optional just-windows?)
  ;; #+opengl (capi:apply-in-pane-process *boxer-pane* #'repaint-internal just-windows?)
  ;; #-opengl (repaint-internal just-windows?)

  ; (unless  (equal "CAPI Execution Listener 1" (mp:process-name mp:*current-process*))
  ;   (format t "~%what process is this2: ~A" (mp:process-name mp:*current-process*))
  ; )

  ;; sgithens hacking
  ; (unless (not (null bw::*suppress-expose-handler*)) (repaint-internal just-windows?))
  (repaint-internal just-windows?)
  )

(defun repaint-with-cursor-relocation ()
  (let ((*allow-redisplay-encore? t))
    (repaint)))

;;;; Ephemera: cursors, regions
(defun repaint-cursor (&optional (cursor *point*)(flush-buffer? T))
  (drawing-on-window (*boxer-pane*)
                     ;; this updates the position & size
                     (repaint-cursor-internal cursor)
                     ;; now draw it
                     (draw-blinker *point-blinker*)
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
                                                                     (set-cursorpos *boxer-pane* cursor-x (- (+ cursor-y offset-from-top) on-scroll-row-offset))
                                                                     (set-cursor-size *point-blinker* c-width (+ c-height on-scroll-row-offset))))
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
        (setq *last-eval-repaint* now)
        (process-editor-mutation-queue-within-eval)
        (unless (null bw::*suppressed-actions*)
          (funcall (pop bw::*suppressed-actions*)))
        (repaint-window *boxer-pane* t :process-state-label "eval")
        (setq *last-repaint-duration* (- (get-internal-real-time) now))))
)
