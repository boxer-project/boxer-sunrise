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
;;;;         Code for painting the cursor/blinker.
(in-package :boxer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           Redisplay  of  REGIONS                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regions are displayed as one or more blinkers.  With Each blinker
;;; corresponding to screen representation(s) for the rows which make
;;; up the region

;;; We provide two different messages for redisplay of regions.  One of them
;;; will just mark the screen rows corresponding to the region in
;;; the *CURRENT-SCREEN-BOX* while the other one will mark *ALL* the screen
;;; rows of the region.

;; Blinkers positions are with respect to the window WITH THE BORDERS INCLUDED
                                        ;(DEFMACRO FIXUP-COORDINATES-FOR-BLINKER (X Y BL)
                                        ;  `(LET ((SHEET (SEND ,BL :SHEET)))
                                        ;     (SETF ,X (+ ,X (SEND SHEET :LEFT-MARGIN-SIZE)))
                                        ;     (SETF ,Y (+ ,Y (SEND SHEET :TOP-MARGIN-SIZE)))))

;; used to make rows with 0 characters highlighted.
(defvar *minimum-row-blinker-wid* 0)

(defun update-region-row-blinker (region-row-blinker)
  (let* ((screen-row (region-row-blinker-uid region-row-blinker))
         (wid (screen-obj-wid screen-row))
         (hei (screen-obj-hei screen-row)))
    (multiple-value-bind (x y)
        (xy-position screen-row)
                                        ;      ;; Blinker positions are measured with the borders included
                                        ;      (FIXUP-COORDINATES-FOR-BLINKER X Y REGION-ROW-BLINKER)
      (when (or (not (= wid (blinker-wid region-row-blinker)))
                (not (= hei (blinker-hei region-row-blinker)))
                (not (= x   (blinker-x   region-row-blinker)))
                (not (= y   (blinker-y   region-row-blinker))))
        ;; might be better to use timestamps (we might
        ;; have to use timestamps in addition anyway)
        (setf (blinker-wid region-row-blinker)
              (if (zerop wid) *minimum-row-blinker-wid* wid))
        (setf (blinker-hei region-row-blinker) hei)
        (setf (blinker-x region-row-blinker) x)
        (setf (blinker-y region-row-blinker) y)))))

(defun left-half-blinker-trim (blinker cha-no)
  (let* ((screen-row (region-row-blinker-uid blinker))
         (row-wid (screen-obj-wid screen-row))
         (row-hei (screen-obj-hei screen-row))
         (amount-to-trim
           (with-summation
             (do-screen-chas-with-font-info (cha (slot-value screen-row 'screen-chas)
                                             :stop cha-no)
               (sum (if (screen-cha? cha)
                        (cha-wid cha)
                        (screen-obj-wid cha))))))
         (desired-wid (- row-wid amount-to-trim)))
    (multiple-value-bind (x y)
        (xy-position screen-row)
      (when (or (not (= desired-wid        (blinker-wid blinker)))
                (not (= row-hei            (blinker-hei blinker)))
                (not (= (+ x amount-to-trim)
                        (blinker-x   blinker)))
                (not (= y                  (blinker-y   blinker))))
        (setf (blinker-wid blinker)
              (if (zerop desired-wid) *minimum-row-blinker-wid* desired-wid))
        (setf (blinker-hei blinker) row-hei)
        (setf (blinker-x   blinker) (+ x amount-to-trim))
        (setf (blinker-y   blinker) y)))))

(defun right-half-blinker-trim (blinker cha-no)
  (let* ((screen-row (region-row-blinker-uid blinker))
         (row-wid (screen-obj-wid screen-row))
         (row-hei (screen-obj-hei screen-row))
         (amount-to-trim
           (if (>=& cha-no (screen-chas-length screen-row))
               0
               (with-summation
                 (do-screen-chas-with-font-info (cha (slot-value screen-row
                                                                 'screen-chas)
                                                 :start cha-no)
                   (sum (if (screen-cha? cha) (cha-wid cha) (screen-obj-wid cha)))))))
         (desired-wid (- row-wid amount-to-trim)))
    (multiple-value-bind (x y)
        (xy-position screen-row)
      (when (or (not (= desired-wid  (blinker-wid blinker)))
                (not (= row-hei      (blinker-hei blinker)))
                (not (= x            (blinker-x   blinker)))
                (not (= y            (blinker-y   blinker))))
        (setf (blinker-wid blinker)
              (if (zerop desired-wid) *minimum-row-blinker-wid* desired-wid))
        (setf (blinker-hei blinker) row-hei)
        (setf (blinker-x   blinker) x)
        (setf (blinker-y   blinker) y)))))

(defun both-ends-blinker-trim (blinker start-cha-no stop-cha-no)
  (let* ((screen-row (region-row-blinker-uid blinker))
         (screen-chas (slot-value screen-row 'screen-chas))
         (row-wid (screen-obj-wid screen-row))
         (row-hei (screen-obj-hei screen-row))
         (left-trim (with-summation
                      (do-screen-chas-with-font-info (cha screen-chas
                                                      :stop start-cha-no)
                        (if cha (sum (if (screen-cha? cha)
                                         (cha-wid cha)
                                         (screen-obj-wid cha)))))))
         (right-trim
           (if (>=& stop-cha-no (screen-chas-length screen-row))
               0
               (with-summation
                 (do-screen-chas-with-font-info (cha (slot-value screen-row
                                                                 'screen-chas)
                                                 :start stop-cha-no)
                   (if cha (sum (if (screen-cha? cha)
                                    (cha-wid cha)
                                    (screen-obj-wid cha))))))))
         (desired-wid (- row-wid left-trim right-trim)))
    (multiple-value-bind (x y)
        (xy-position screen-row)
      (when (or (not (= desired-wid     (blinker-wid blinker)))
                (not (= row-hei         (blinker-hei blinker)))
                (not (= (+ x left-trim) (blinker-x   blinker)))
                (not (= y               (blinker-y   blinker))))
        (setf (blinker-wid blinker)
              (if (zerop desired-wid) *minimum-row-blinker-wid* desired-wid))
        (setf (blinker-hei blinker) row-hei)
        (setf (blinker-x   blinker) (+ x left-trim))
        (setf (blinker-y   blinker) y)))))

;; is the row conneted to the editor hierarchy ?
;; used for detemining whether we should bother trying to redisplay
;; a region...
(defun row-connected? (row &optional
                             (outermost-visible-box
                              (screen-obj-actual-obj
                               (outermost-screen-box))))
  (let ((superior-box (superior-box row)))
    (cond ((null superior-box) nil)
          ((eq superior-box *initial-box*) t)
          (t (let ((superior-row (superior-row superior-box)))
               (cond ((null superior-row) nil)
                     (t (row-connected? superior-row outermost-visible-box))))))))

(defun update-row-blinker-list (row-blinkers screen-rows)
  "A blinker for every row and no extra blinkers. Returns a list of blinkers"
  (prog1
      (with-collection
        (dolist (screen-row screen-rows)
          (collect (let ((existing-region
                           (car (member screen-row row-blinkers
                                        :test #'(lambda (uid reg)
                                                  (eq uid
                                                      (region-row-blinker-uid
                                                       reg)))))))
                     (cond ((null existing-region)
                            (make-region-row-blinker :uid screen-row))
                           (t
                            (setq row-blinkers (fast-delq existing-region
                                                          row-blinkers))
                            existing-region))))))
    (dolist (old-blinker row-blinkers)
      (remove-region-row-blinker old-blinker))))

(defun interval-update-repaint-all-rows (region &optional
                                                  (window *boxer-pane*))
  ;; we have to bind this because region redisplay can
  ;; legitimately be called OUTSIDE of normal redisplay
  (cond ((not (row-connected? (bp-row (interval-start-bp region))))
         ;; the region is no longer part of the visible editor hierarchy
         ;; so blank it....
         (dolist (blinker (interval-blinker-list region))
           (remove-region-row-blinker blinker))
         (setf (interval-blinker-list region) nil))
        (t
         (with-region-top-level-bps (region :start-bp-name region-start-bp
                                            :stop-bp-name region-stop-bp)
           ;; First we do "allocation" that is, make sure that there
           ;; is a blinker for every screen row and vice versa.  Note
           ;; that blinker list will be ordered from top to bottom
           (setf (interval-blinker-list region)
                 (update-row-blinker-list
                  (interval-blinker-list region)
                  (let ((displayed-rows nil))
                    (do-region-rows (row region)
                      (setq displayed-rows
                            (append displayed-rows (displayed-screen-objs row))))
                    displayed-rows)))
           (let ((starting-row (bp-row region-start-bp))
                 (starting-cha-no (bp-cha-no region-start-bp))
                 (stopping-row (bp-row region-stop-bp))
                 (stopping-cha-no (bp-cha-no region-stop-bp)))
             (dolist (blinker (interval-blinker-list region))
               (let* ((blinker-row (region-row-blinker-uid blinker))
                      (editor-row (screen-obj-actual-obj blinker-row)))
                 (cond ((and (eq starting-row editor-row)
                             (eq stopping-row editor-row))
                        ;; the row is both the first and last one in a
                        ;; region so we should trim both ends of it
                        (both-ends-blinker-trim blinker starting-cha-no
                                                stopping-cha-no))
                       ((eq starting-row editor-row)
                        ;; If the row is the first one in a region then it
                        ;; needs to be trimmed to correspond to where
                        ;; the BP is pointing
                        (left-half-blinker-trim blinker starting-cha-no))
                       ((eq stopping-row editor-row)
                        ;; If the row is the last one in the region, then
                        ;; it ALSO needs to be trimmed to correspond to
                        ;; where the BP is pointing
                        (right-half-blinker-trim blinker stopping-cha-no))
                       (t
                        ;; finally, take care of all the other rows
                        (update-region-row-blinker blinker)))))))))
  ;; @ this point all the blinkers are the correct size and inthe right place...
  (dolist (blinker (interval-blinker-list region)) (draw-blinker blinker)))

;; timing
(defvar delta-time 0.0) ;; time between current frame and last frame
(defvar last-frame 0.0)
(defvar blinker-time 0.0)
(defvar blinker-on t
  "Is the blinker cursor currently visible?")

(defun toggle-blinker ()
  (cond
    (blinker-on
      (setf boxer::*point-color* #(:rgb .3 .3 .9 .5))
      (setf blinker-on nil)
    )
    (t
      (setf boxer::*point-color* #(:rgb .3 .3 .9 .0))
      (setf blinker-on t))))

(defun draw-blinker (blinker &key (color *blinker-color*))
  (with-pen-color (color)
      (draw-rectangle (blinker-wid blinker) (blinker-hei blinker)
                      (blinker-x blinker)   (blinker-y blinker))))
