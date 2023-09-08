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
;;;;         This file contains the first pass of the repaint mechanism, which scans over the entire
;;;;         box data structure and creates/updates screen-box, screen-row structures which have the
;;;;         calculated dimensions for rendering and are attached to the box/row data structures.
;;;;         The repaint-pass-2 mechanism renders the screen based off of these calculations.
;;;;
(in-package :boxer)

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
  (values (max infs-new-wid (screen-obj-wid self))
          (+ infs-new-hei (screen-obj-hei self))
          (or infs-new-x-got-clipped? (slot-value self 'x-got-clipped?))
          (or infs-new-y-got-clipped? (slot-value self 'y-got-clipped?))
          (+ inf-y-offset (screen-obj-hei self))
          (- infs-new-max-hei (screen-obj-hei self))))

;; Pass 1 should be accumalating changed rects...
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
                                                           (set-screen-obj-offsets inf-screen-obj inf-x-offset inf-y-offset)
                                                           (repaint-pass-1-sr inf-screen-obj
                                                                              infs-new-max-wid infs-new-max-hei)
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

;; this mainly has to inform any further boxes that their services
;; will no longer be required....
(defmethod rp1-sr-punt-extra-screen-objs-from ((self screen-row)
                                               no-of-first-obj-to-punt
                                               clipped?
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
  (values (+ infs-new-wid (screen-obj-wid self))
          (max infs-new-hei (screen-obj-hei self))
          (or infs-new-x-got-clipped? (slot-value self 'x-got-clipped?))
          (or infs-new-y-got-clipped? (slot-value self 'y-got-clipped?))
          (+ inf-x-offset (screen-obj-wid self))
          (- infs-new-max-wid (screen-obj-wid self))))

(defmethod update-scroll-wid ((self screen-box) new-scroll-wid)
  (with-slots (max-scroll-wid) self
    (cond ((null max-scroll-wid)
           (setq max-scroll-wid new-scroll-wid))
      ((numberp max-scroll-wid)
       (setq max-scroll-wid (max max-scroll-wid new-scroll-wid))))))

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
             (repaint-pass-1-sb inf-screen-obj
                                infs-new-max-wid infs-new-max-hei)
             (multiple-value-setq (infs-new-wid infs-new-hei
                                                infs-new-x-got-clipped?
                                                infs-new-y-got-clipped?
                                                inf-x-offset infs-new-max-wid)
                                  (rp1-sb-increment-superior-parameters
                                   inf-screen-obj
                                   infs-new-wid            infs-new-hei
                                   infs-new-x-got-clipped? infs-new-y-got-clipped?
                                   inf-x-offset            infs-new-max-wid)))))))))

(defmethod repaint-pass-1-sr ((self screen-row) max-wid max-hei)
  (with-drawing-inside-region ((slot-value self 'x-offset)
                               (slot-value self 'y-offset)
                               (screen-obj-wid self)
                               (screen-obj-hei self))
    ;; During redisplay-pass-1 the only region of the screen the redisplay
    ;; methods are allowed to draw in is the region of the screen currently
    ;; occupied by the screen obj.
    (multiple-value-bind (nw nh nxc nyc base total-wid)
                         (repaint-inferiors-pass-1-sr self max-wid max-hei)
                         (setf (screen-obj-wid self) nw)
                         (let ((1st-font (bfd-font-no (closest-bfd (slot-value self 'actual-obj) 0))))
                           (rebind-font-info (1st-font)
                                             (setf (screen-obj-hei self) (max nh (cha-hei))
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
                            (update-scroll-wid (screen-box self) (screen-obj-wid self)))))))

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
                                                                          (setf (screen-obj-wid self) (+ l-border-wid r-border-wid))
                                                                          (setf (screen-obj-hei self) (+ t-border-wid b-border-wid))

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
                                                                                               (incf (screen-obj-wid self) rows-new-wid)
                                                                                               (incf (screen-obj-hei self) rows-new-hei)
                                                                                               ;; make sure that we are at least
                                                                                               ;; as big as our minimum size.
                                                                                               (setf (screen-obj-wid self)
                                                                                                     (min (max (screen-obj-wid self) real-min-wid)
                                                                                                          real-max-wid))
                                                                                               (setf (screen-obj-hei self)
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

;; sprites can (and usually DO) extend into negative x, y
;; so in editor space, they have to be drawn with an offset
;; so after calculating the dimensions of the sprite, calculate it's
;; center offset
(defmethod repaint-pass-1-sb ((self sprite-screen-box) max-wid max-hei)
  ;; subsprites
  ;; shape
  )

(defun top-level-repaint-pass-1 ()
  (multiple-value-bind (max-wid max-hei)
                       (outermost-screen-box-size *redisplay-window*)
                       (repaint-pass-1-sb *outermost-screen-box* max-wid max-hei)))
