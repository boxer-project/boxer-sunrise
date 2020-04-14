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


  VCGC: virtual copy garbage collection utilities

  The Problem: all modifications to boxes which are in the editor are
  retained during the entire duration of an evaluation.  The reason for
  this is because the editor box may be a deep inferior of a virtual copy
  created during the eval.  If the virtual copy is returned, or incorporated
  in other editor structure, we will need the particular version of the
  editor box which corresponds to the creation time of the virtual copy in
  order to render the correct structure.

  The problem with this approach is that unneeded versions can accumalate
  and use up available memory.  The canonical bad behavior is demonstrated
  by the program:  repeat 1000000000 [... change x x + 1 ...]
  which, by the end of an evaluation will have 1000000000 versions of the
  box "x".

  Most, if not all of those versions of "x" will never be needed.  We need
  to try and keep only those editor box versions which are neccessary.  There
  are 2 possible methods for doing this:
   1) incrementally at modification time, compacting the list of versions
      prior to appending the latest version or
   2) VCGC at regular intervals or on a pre GC hook
      run through all the versions in every currently modified editor box
      and removed unneeded versions.

  Method 1 has the advantage that the extra work is spread out.

  Both methods require that we have a list of all currently active virtual
  copies. Including changed editor rows

  Method 2 requires also needs us to keep track of all the modified editor
  boxes fortunately we already keep track of modified editor objects in
  *editor-objects-to-be-modified*


Modification History (most recent at top)

 2/28/10 global-vcgc-required-printing-times cannot use lisp::delete-duplicates anymore because it
         doesn't preserve the attributes of the original arg (the fill-pointer in this case)
 2/15/03 merged current LW and MCL files
 4/20/02 global-vcgc-required-printing-times: better consistency checking
11/19/01 vcgc checks for *editor-objects-to-be-modified* bound in case it is
         called from another process (like LWW net file loaders)
 8/23/01 handle possible box interfaces in xxx-required-printing-times and vcgc
 8/20/01 finished initial implementation
11/28/00 started logging changes

|#

(in-package :boxer)

; debugging (move to vrtdef ? )

;; this can also have the value :metering-only
(eval-when (compile load eval)
(defvar *include-vcgc-debugging?* t
  "Compile time variable to control the inclusing of extra vcgc code")
)

(defvar *debug-vcgc?* nil)

(defun vcgc-debug () (setq *debug-vcgc?* t))
(defun vcgc-nodebug () (setq *debug-vcgc?* nil))

(defmacro vcgc-debugging (&body body)
  (if (eq *include-vcgc-debugging?* T)
      `(when *debug-vcgc?* (progn . ,body))
    nil))

(defmacro vcgc-metering (&body body)
  (if *include-vcgc-debugging?*
      `(progn . ,body)
    nil))

(defmacro do-stack-vcps ((vc-var) &body body)
  (let ((index-var (gensym)) (stack-idx (gensym)))
    `(progn
       ;; returned values
       (dotimes (,index-var boxer-eval::*vpdl-index*)
         (let ((,vc-var (svref& boxer-eval::*vpdl* ,index-var)))
           (when (or (virtual-copy? ,vc-var) (virtual-port? ,vc-var))
             . ,body)))
       ;; stack vars
       (do* ((,stack-idx boxer-eval::*dynamic-variables-bottom* (1+ ,stack-idx))
             (,vc-var (svref& boxer-eval::*dynamic-variables-values-array* ,stack-idx)
                      (svref& boxer-eval::*dynamic-variables-values-array* ,stack-idx)))
            ((>= ,stack-idx boxer-eval::*dynamic-variables-top*))
         (when (or (virtual-copy? ,vc-var) (virtual-port? ,vc-var))
           . ,body)))))





;;;; Metering
;; we want to keep track of:
;;  o number of VCGC's
;;  o vcgc-ctime-vector stats for both global and local algorthms
;;     o number of vectors created
;;     o average vector size (keep track of total and divide when reporting)
;;     o maximum vector size
;;     o minimum vector size
;;  It might be useful inthe future to crank up the level of metering and
;;  then record the distribution of vector sizes
;;  o number of modified rows release for GCing
;;  o number of editor boxes VCGC'd
;;  o min and max of rows released
;;  o report on average number of release rows per box (divide numbers)
;;

(defstruct (vcgc-recording-vector (:type vector)
                                  (:conc-name vcgc-metering-))
  (vcgcs 0) ;; number of vcgc's
  (time 0)  ;; in internal-time-units
  (boxes 0)
  ;; global ctime vector stats
  (global-ctime-vects 0)
  (global-ctime-size 0)
  (global-ctime-min 100)
  (global-ctime-max 0)
  ;;local ctime stats
  (local-ctime-vects 0)
  (local-ctime-size 0)
  (local-ctime-min 100)
  (local-ctime-max 0)
  ;; row liberacion stats
  (rows-libbed 0)
  (rows-min 100)
  (rows-max 0))

(defvar *vcgc-recording-vector* (make-vcgc-recording-vector))
(defvar *old-vcgc-recording-vectors* nil)

(defun reset-vcgc-metering (&optional keep-old?)
  (when keep-old?
    (push *vcgc-recording-vector* *old-vcgc-recording-vectors*))
  (setq *vcgc-recording-vector* (make-vcgc-recording-vector)))

;; reporting function
(defun vcgc-metering-report (&optional old-too?)
  (vcgc-metering-report-1 *vcgc-recording-vector*)
  (when old-too?
    (dolist (old-vect *old-vcgc-recording-vectors*)
      (terpri *standard-output*)
      (vcgc-metering-report-1 old-vect))))

(defun vcgc-metering-report-1 (vector)
  (format *standard-output*
          "~&~4D Total VCGC's in a time of ~D milliseconds~&~
             ~4D Boxes vcgc'd~&~
             ~4D Global creation time vectors~&~
             ~4D Average Global ctime vector size (min:~D, max:~D)~&~
             ~4D Local creation time vectors~&~
             ~4D Average local ctime vector size (min:~D, max:~D)~&~
             ~4D Rows released ~&~
             ~4D Average rows per box (min:~D, max:~D)~&"
          (vcgc-metering-vcgcs vector) (vcgc-metering-time vector)
          (vcgc-metering-boxes vector)
          (vcgc-metering-global-ctime-vects vector)
          (/ (vcgc-metering-global-ctime-size vector)
             (let ((x (vcgc-metering-global-ctime-vects vector)))
               (if (zerop x) 1 (float x))))
          (vcgc-metering-global-ctime-min vector)
          (vcgc-metering-global-ctime-max vector)
          (vcgc-metering-local-ctime-vects vector)
          (/ (vcgc-metering-local-ctime-size vector)
             (let ((x (vcgc-metering-local-ctime-vects vector)))
               (if (zerop x) 1 (float x))))
          (vcgc-metering-local-ctime-min vector)
          (vcgc-metering-local-ctime-max vector)
          (vcgc-metering-rows-libbed vector)
          (/ (vcgc-metering-rows-libbed vector)
             (let ((x (vcgc-metering-boxes vector)))
               (if (zerop x) 1 (float x))))
          (vcgc-metering-rows-min vector)
          (vcgc-metering-rows-max vector)))

;; metering recording functions
(defun record-vcgc ()
  (incf& (vcgc-metering-vcgcs *vcgc-recording-vector*)))

(defun record-vcgc-time (time)
  (incf (vcgc-metering-time *vcgc-recording-vector*) time))

(defun record-vcgc-box ()
  (incf& (vcgc-metering-boxes *vcgc-recording-vector*)))

(defun record-vcgc-global-vect (vector-size)
  (incf& (vcgc-metering-global-ctime-vects *vcgc-recording-vector*))
  (incf& (vcgc-metering-global-ctime-size *vcgc-recording-vector*)
         vector-size)
  (setf (vcgc-metering-global-ctime-min *vcgc-recording-vector*)
        (min& (vcgc-metering-global-ctime-min *vcgc-recording-vector*)
              vector-size))
  (setf (vcgc-metering-global-ctime-max *vcgc-recording-vector*)
        (max& (vcgc-metering-global-ctime-max *vcgc-recording-vector*)
              vector-size)))

(defun record-vcgc-local-vect (vector-size)
  (incf& (vcgc-metering-local-ctime-vects *vcgc-recording-vector*))
  (incf& (vcgc-metering-local-ctime-size *vcgc-recording-vector*)
         vector-size)
  (setf (vcgc-metering-local-ctime-min *vcgc-recording-vector*)
        (min& (vcgc-metering-local-ctime-min *vcgc-recording-vector*)
              vector-size))
  (setf (vcgc-metering-local-ctime-max *vcgc-recording-vector*)
        (max& (vcgc-metering-local-ctime-max *vcgc-recording-vector*)
              vector-size)))

(defun record-vcgc-rows-libbed (number-of-rows)
  (incf& (vcgc-metering-rows-libbed *vcgc-recording-vector*) number-of-rows)
  (setf (vcgc-metering-rows-min *vcgc-recording-vector*)
        (min& (vcgc-metering-rows-min *vcgc-recording-vector*)
              number-of-rows))
  (setf (vcgc-metering-rows-max *vcgc-recording-vector*)
        (max& (vcgc-metering-rows-max *vcgc-recording-vector*)
              number-of-rows)))



;;; Simulation utilities
;; we add these utilities to VC/print/chunker code using the vcgc-debugging
;; macro.  When we are in VCGC sim mode, instead of actually removing vc
;; rows entries, we keep track of them.  Later, during print time, we can see
;; if we would have lost by comparing with the list of stored vc rows entries.
(defvar *simulate-vcgc?* nil)

(defvar *initial-sim-vcgc-array-size* 200)

(defvar *sim-vcgc-array*
  (if *include-vcgc-debugging?*
      ;; no need to CONs up this big array if we aren't debugging
      (make-array *initial-sim-vcgc-array-size* :adjustable t :fill-pointer 0)
    nil))

(defun clear-sim-vcgc-array ()
  (when *sim-vcgc-array*
    (dotimes (i (fill-pointer *sim-vcgc-array*))
      (setf (aref *sim-vcgc-array* i) nil))
    (setf (fill-pointer *sim-vcgc-array*) 0)))

(defun sim-remove-vc-rows-entry (vcre)
  (when *sim-vcgc-array* (vector-push-extend vcre *sim-vcgc-array*)))

(defun sim-vcgc-member (vcre)
  (when *sim-vcgc-array*
    (dotimes (i (fill-pointer *sim-vcgc-array*))
      (when (eq (aref *sim-vcgc-array* i) vcre) (return t)))))



;;; The core

#|
  The algorithm:

  For each modified editor box, either in a batch or incrementally for
  each box at modification time:

Notes:
The important data structure is the vcgc creation time vector which is
used to compare against the mod times of the editor rows in order to
eliminate unneeded row mods.

actually there are 2 possible algorthms for building a vcgc-ctime-vector
  1) Use a single global vector for all modified editor boxes
     we can just loop through all the ROOTs once, accumalating ctimes
     advantages:    can build it fast, time will be of Order # ROOTs
     disadvantages: extra row mods may be kept because of ctimes for
        VC's which are not superiors of the mod-ed-box
  2) Use a specialized vector for each mod-ed-box
     for each mod-ed-box, loop through the ROOTs, 1st checking for
     superior-ness of the ROOT, and then, only if that is true, adding
     the ctime of the ROOT to the specialized vector
     advantages:    maximal paring of modified rows
     disadvantages: possibly slow, since time will be of Order
                    # mod-ed-boxes * # ROOTs

    loop through the ROOTS[existing VC's and newest VC rows entries in
                           modified editor boxes]
      IF the modified editor box is an inferior of a ROOT (indicating the
         possibility that the modified editor box will be required to articulate
         the ROOT at print time)
      THEN compare the ROOT's creation time with the creation time of the
         vc rows entries of the modified editor box
         A vc rows entry can be safely removed
            IF there is another vc rows which is newer than it but older
               or the same age as the ROOTs

               note that since modified editor boxes are already ROOTs,
               the newest vc rows entry should always be kept



|#
(defvar *use-local-ctime-vectors?* nil)

;;; useful for debugging

(defun show-vcgc-roots (&optional (stream *standard-output*))
  (format stream "~&CHANGEd Editor Boxes:~&")
  (dolist (meb *editor-objects-to-be-modified*) (print meb stream))
  (format stream "~&Virtual Objects:~&")
  (do-stack-vcps (stack-vcp)
    (cond ((virtual-port? stack-vcp)
           (print-virtual-port stack-vcp stream 0) (terpri stream))
          (t (print-virtual-copy stack-vcp stream 0) (terpri stream)))))

;; start with a modified editor box, and loops through the vcgc roots
;; looking for superior's of the ed box, then returns a list of creation times
;; algorthm 2

(defun local-vcgc-required-printing-times (edbox)
  (let ((vcgc-ctime-vector (make-array *initial-vcgc-vector-length*
                                       :fill-pointer 0 :adjustable t)))
    (do-stack-vcps (stack-vcp)
                  (cond ((virtual-port? stack-vcp)
	                 (when (and (virtual-copy? (vp-target stack-vcp))
                                    (vc-eb-inferior? edbox (vp-target stack-vcp)))
                           (vector-push-extend (vc-creation-time
                                                (vp-target stack-vcp))
                                                vcgc-ctime-vector)))
                        (t ; must be a virtual copy then
                         (when (vc-eb-inferior? edbox stack-vcp)
                           (vector-push-extend (vc-creation-time stack-vcp)
                                               vcgc-ctime-vector)))))
    (dolist (meb *editor-objects-to-be-modified*)
      ;; loop through CHANGEd boxes
      ;; no ports, only boxes are found in the mod queue
      (let ((box (cond ((box? meb) meb)
                       ((box-interface? meb) (box-interface-box meb)))))
        (unless (null box)
          (when (ebgc-inferior? edbox box)
            (vector-push-extend (vcis-creation-time
                                 (car (slot-value box 'virtual-copy-rows)))
                                vcgc-ctime-vector)))))
    ;; sort and remove duplicates from the vector
    (setq vcgc-ctime-vector
          (delete-duplicates (sort vcgc-ctime-vector #'>)))
    (vcgc-metering (record-vcgc-local-vect (length vcgc-ctime-vector)))
    vcgc-ctime-vector))

;; will the VC need info from the editor box at print time ?
;; look in progenitor and pedigree slots for editor boxes
;; NIL means possible general match (newly constructed VC's will have null
;; ancestor slots, the inferiors are differentiated via creation time)
;; support for algorthm 2

;; is the editor box an inferior of a creator of the VC
(defun vc-eb-inferior? (edbox vc)
  (or (null (vc-progenitor vc))
      ;; NIL means it has been consed and therefore can possibly
      ;; contain any arbitrary editor structure
      (and (box? (vc-progenitor vc))
           (or (eq edbox (vc-progenitor vc))
               (superior? edbox (vc-progenitor vc))))
      ;; the VC's creator is a superior of the editor box?
      (member edbox (vc-pedigree vc)
              :test #'(lambda (a b)
                        (let ((pedbox (car b)))
                          (cond ((box? pedbox)
                                 (or (eq a pedbox) (superior? edbox pedbox)))
                                ((virtual-copy? pedbox)
                                 (let ((ped-creator (vc-progenitor pedbox)))
                                   (or (eq a ped-creator)
                                       (superior? edbox ped-creator))))))))))


(defun ebgc-inferior? (edbox mod-ed-box)
  (or (eq edbox mod-ed-box)
      (superior? edbox mod-ed-box)
      (let ((newest-rows (car (slot-value mod-ed-box 'virtual-copy-rows))))
        (when (and (not (null newest-rows)) (vcis-modified? newest-rows))
          (member edbox (vcis-pedigree newest-rows)
                  :test #'(lambda (a b)
                            (let ((pedbox (car b)))
                              (cond ((box? pedbox)
                                     (or (eq a pedbox) (superior? edbox pedbox)))
                                    ((virtual-copy? pedbox)
                                     (let ((ped-creator (vc-progenitor pedbox)))
                                       (or (eq a ped-creator)
                                           (superior? edbox ped-creator))))))))))))

;;; algorthm 1
(defvar *initial-vcgc-vector-length* 20)

(defun global-vcgc-required-printing-times ()
  (let ((vcgc-ctime-vector (make-array *initial-vcgc-vector-length*
                                       :fill-pointer 0 :adjustable t)))
    (flet ((my-delete-duplicates (vector)
             ;; lisp::delete-duplicates is not guaranteed to retain the vector attributes of the original
             ;; arg.  Since we know we will be feeding it a sorted 1 dimensional array of fixnum elements
             ;; with a fill pointer, we  won't bother covering the general cases.
             (let ((last-entry 0)
                   (return-vector (make-array (fill-pointer vector) :fill-pointer 0 :adjustable t)))
               (dotimes (i (fill-pointer vector))
                 (let ((current-entry (aref vector i)))
                   (cond ((zerop i)
                          (setq last-entry current-entry)
                          (vector-push current-entry return-vector))
                         ((= current-entry last-entry))
                         (t (setq last-entry current-entry)
                            (vector-push current-entry return-vector)))))
               return-vector)))
      (do-stack-vcps (stack-vcp)
        (cond ((virtual-port? stack-vcp)
               (when (virtual-copy? (vp-target stack-vcp))
                 (vector-push-extend (vc-creation-time (vp-target stack-vcp))
                                     vcgc-ctime-vector)))
              (t ; must be a virtual copy then
               (vector-push-extend (vc-creation-time stack-vcp)
                                   vcgc-ctime-vector))))
      ;; iterate over mod ed boxes
      (dolist (meb *editor-objects-to-be-modified*)
        ;; no ports, only boxes are found in the mod queue
        (let* ((box (cond ((box? meb) meb)
                          ((box-interface? meb) (box-interface-box meb))))
               (vcrs (when (box? box) (car (slot-value box 'virtual-copy-rows)))))
          (unless (null vcrs)
            (vector-push-extend (vcis-creation-time vcrs) vcgc-ctime-vector))))
      ;; sort and remove duplicates from the vector
      (setq vcgc-ctime-vector
            (my-delete-duplicates (sort vcgc-ctime-vector #'>)))
      (vcgc-metering (record-vcgc-global-vect (length vcgc-ctime-vector)))
      vcgc-ctime-vector)))

;;; Main function for row elimination.  Takes a vcgc-ctime-vector
;;; note that both the times-vector and the list of virtual-copy-rows are
;;; ordered backward in time
(defmethod vcgc-liberate-rows ((box box) times-vector)
  (vcgc-debugging (format t "~&Liberating Rows for ~A using ~A" box times-vector))
  (let ((row-count 0)
        ;; we always need to keep the newest set of rows
        (rows-to-keep (list (car (slot-value box 'virtual-copy-rows))))
        (ctime-idx 0) (ctime (aref times-vector 0)))
    (flet ((next-ctime ()
                       (incf& ctime-idx)
                       (setq ctime (cond ((>= ctime-idx
                                              (fill-pointer times-vector))
                                          nil)
                                         (t (let ((c (aref times-vector
                                                           ctime-idx)))
                                              (if (= -1 c) nil c)))))))
      (dolist (vcrows (cdr (slot-value box 'virtual-copy-rows)))
        (cond ((null ctime)
               ;; no more ctimes so prune all but the last remaing vcrows
               (vcgc-debugging (format t "~&Null ctime pruning rows, timestamps:"))
               (do* ((remaining-rows (fast-memq vcrows
                                                (slot-value box
                                                            'virtual-copy-rows))
                                     (cdr remaining-rows))
                     (rows (car remaining-rows) (car remaining-rows)))
                    ((null (cadr remaining-rows)) (push rows rows-to-keep))
                 (incf& row-count)
                 (vcgc-debugging (format t " ~D" (vcis-creation-time rows)))
                 (vcgc-debugging (when *simulate-vcgc?*
                                   (sim-remove-vc-rows-entry vcrows))))
               (return nil))
              ((>& (vcis-creation-time vcrows) ctime)
               ;; any set of rows newer than the current ctime can be thrown out
               ;; if simming, save it
               (incf& row-count)
               (vcgc-debugging (format t "~&ctime:~D Pruning rows, timestamp: ~D"
                                     ctime (vcis-creation-time vcrows)))
               (vcgc-debugging (when *simulate-vcgc?*
                                 (sim-remove-vc-rows-entry vcrows))))
              (t ;; hit rows older than the current ctime so save the rows
                 (vcgc-debugging (format t "~&Saving rows, timestamp: ~D"
                                         (vcis-creation-time vcrows)))
                 (push vcrows rows-to-keep)
                 ;; now cycle to the next applicable ctime
                 (vcgc-debugging (format t "~&Calculating next ctime: "))
                 (do ((new-ctime (next-ctime) (next-ctime)))
                     ((or (null new-ctime)
                          (<& new-ctime (vcis-creation-time vcrows))))
                   (vcgc-debugging (format t " ~D " new-ctime)))))))
    (vcgc-metering (record-vcgc-rows-libbed row-count))
    (vcgc-debugging (format t "~&Pruned ~D rows" row-count))
    (setf (slot-value box 'virtual-copy-rows) (nreverse rows-to-keep))))

;*use-local-ctime-vectors?*


;;; main functions

(defun vcgc ()
  (when (boundp '*editor-objects-to-be-modified*)
    (vcgc-metering (record-vcgc))
    (let ((start-time (get-internal-real-time))
          (global-ctime-vector (unless *use-local-ctime-vectors?*
                                 (global-vcgc-required-printing-times))))
      (dolist (meb *editor-objects-to-be-modified*)
        (let ((box (cond ((box? meb) meb)
                         ((box-interface? meb) (box-interface-box meb)))))
          (unless (null box)
            (vcgc-liberate-rows box (if *use-local-ctime-vectors?*
                                        (local-vcgc-required-printing-times box)
                                      global-ctime-vector))))
        (vcgc-metering (record-vcgc-box)))
      (vcgc-metering (record-vcgc-time (- (get-internal-real-time) start-time))))))

;; this is called from boxer-eval::poll-internal which is called periodically
;; by the evaluator (see boxer-eval::*initial-poll-count*)

;; for now, we just use a decremeting counter.  Eventually, we may want to
;; be smarter about this and check on the state of available memory or something

(defvar *initial-vcgc-count* 50)

(defvar *vcgc-count* *initial-vcgc-count*)

(defun vcgc-check ()
  (when *evaluation-in-progress?*
    (decf& *vcgc-count*)
    (when (zerop *vcgc-count*)
      (setq *vcgc-count* *initial-vcgc-count*)
      (vcgc))))





