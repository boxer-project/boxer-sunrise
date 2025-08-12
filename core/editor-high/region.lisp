;;;; -*- Mode:LISP; Syntax: Common-Lisp; Package:BOXER ;-*-
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
;;;;                                            +-Data--+
;;;;                   This file is part of the | BOXER | system
;;;;                                            +-------+
;;;;
;;;;
;;;;   This file defines Boxer editor REGIONS
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   9/23/12 fixnum assumption lossage in update-region-row-blinker &
;;;;           {{left,right}-half,both-ends}-blinker-trim
;;;;  12/01/10 #-opengl (flush-port-buffer) for region redisplay
;;;;   2/15/03 merged current LW and MCL files, no changes, copyright updated
;;;;   5/03/98 changed copy-interval to hack fonts
;;;;   5/03/98 started logging: source = boxer version 2.3
;;;;

(in-package :boxer)



(defun make-editor-region (start-bp &optional (stop-bp *point*))
  (if (and (bp? start-bp) (bp? stop-bp))
      (%make-interval start-bp stop-bp)
      (error "One or both of the args: ~S, ~S was not a Boxer pointer"
             start-bp stop-bp)))

(defmacro do-region-rows ((row-var region) &body body)
  (declare (ignore row-var region body))
  (warn "DO-REGION-ROWS should only be used inside WITH-REGION-TOP-LEVEL-BPS")
  '(error
    "DO-REGION-ROWS should only be used inside WITH-REGION-TOP-LEVEL-BPS"))

(defmacro do-region-chas ((cha-var region) &body body)
  (declare (ignore cha-var region body))
  (warn "DO-REGION-CHAS should only be used inside WITH-REGION-TOP-LEVEL-BPS")
  '(error
    "DO-REGION-CHAS should only be used inside WITH-REGION-TOP-LEVEL-BPS"))

(defmacro with-region-top-level-bps ((editor-region
                                      &key start-bp-name stop-bp-name)
                                     &body body)
  "Creates and environment with REGION-START-BP and REGION-STOP-BP bound
   to BPs which are at the same level and ordered.  Then cleans up afterwards."
  (let ((real-start-bp-name (or start-bp-name (gensym)))
        (real-stop-bp-name (or stop-bp-name (gensym))))
    `(multiple-value-bind (,real-start-bp-name ,real-stop-bp-name)
         (order-bps (interval-start-bp ,editor-region)
                    (interval-stop-bp ,editor-region))
       (macrolet ((do-region-rows ((row-var region) &body body)
                    (declare (ignore region)) ; region is here to look right
                    (let ((terminating-row (gensym))
                          (next-row-var (gensym))
                          (last-row-flag (gensym)))
                      `(let ((,terminating-row (bp-row ,',real-stop-bp-name))
                             (,next-row-var nil)
                             (,last-row-flag nil))
                         (do ((,row-var (bp-row ,',real-start-bp-name)
                                        ,next-row-var))
                             ((or (null ,row-var)
                                  (not (null ,last-row-flag))))
                           ;; we have to handle updating the next-row inside
                           ;; the body because the current row can be
                           ;; side-effected in the body in particular, the
                           ;; next-row slot is sometimes bashed
                           (setq ,next-row-var (next-row ,row-var))
                           (when (eq ,row-var ,terminating-row)
                             (setq ,last-row-flag t))
                           ,@body))))
                  (do-region-chas ((cha-var region) &body body)
                    (declare (ignore region))
                    (let ((starting-row (gensym))
                          (terminating-row (gensym))
                          (row-var (gensym))
                          (last-row-flag (gensym)))
                      `(let ((,starting-row (bp-row ,',real-start-bp-name))
                             (,terminating-row (bp-row ,',real-stop-bp-name))
                             (,last-row-flag nil))
                         (do ((,row-var ,starting-row (next-row ,row-var)))
                             ((or (null ,row-var)
                                  (not (null ,last-row-flag))))
                           (when (eq ,row-var ,terminating-row)
                             (setq ,last-row-flag t))
                           (do-row-chas ((,cha-var ,row-var
                                          :start
                                          (if (eq ,row-var
                                                  ,starting-row)
                                              (bp-cha-no ,',real-start-bp-name)
                                              0)
                                          :stop
                                          (when (eq ,row-var
                                                    ,terminating-row)
                                            (bp-cha-no ,',real-stop-bp-name))))
                             ,@body))))))

         (unwind-protect
              (progn . ,body)
           (deallocate-bp ,real-start-bp-name)
           (deallocate-bp ,real-stop-bp-name))))))

;; this has to make copies of the rows that the start-bp and stop-bp
;; are pointing to (and any intervening rows as well), then set the
;; start-bp and stop-bp of the new copy to point into the newly created rows
;; the first and last rows are handled specially

(defun copy-interval (interval &optional (copy-box-arg nil c-b-a-supplied?))
  (with-editor-port-relinking
    (with-region-top-level-bps (interval :start-bp-name start-bp
                                         :stop-bp-name  stop-bp)
      (let ((new-start-bp (make-bp (bp-type start-bp)))
            (new-stop-bp  (make-bp (bp-type stop-bp))))
        (flet ((first-row? (row) (eq row (bp-row start-bp)))
               (last-row?  (row) (eq row (bp-row stop-bp)))
               (copy-partial-row (row &key (start 0)
                                           (stop (length-in-chas row)))
                 (let* ((new-row (make-initialized-row))
                        (new-chas-array (chas-array new-row))
                        (idx 0))
                   (do-row-chas ((c row :start start :stop stop))
                     (let ((new-cha
                             (cond ((cha? c) c)
                                   (c-b-a-supplied? (copy-box c copy-box-arg))
                                   (t (copy-box c)))))
                       (chas-array-insert-cha new-chas-array
                                              idx
                                              new-cha)
                       (unless (cha? new-cha)
                         (set-superior-row new-cha new-row)
                         (insert-self-action new-cha)))
                     (incf& idx))
                   ;; copy some/all FD's
                   (let ((copied-fds nil))
                     (dolist (fd (row-fds row))
                       (when (and (>=& (bfd-cha-no fd) start)
                                  (<&  (bfd-cha-no fd) stop))
                         (push (make-cfd (-& (bfd-cha-no fd) start)
                                         (bfd-font-no fd)
                                         (bfd-color   fd))
                               copied-fds)))
                     (setf (chas-array-fds new-chas-array)
                           (nreverse copied-fds)))
                   new-row)))
          (cond ((eq (bp-row start-bp) (bp-row stop-bp))
                 ;; there is only one (or part of one) row in the region
                 ;; so copy the row (or part of it)
                 (let ((new-region-row (copy-partial-row
                                        (bp-row start-bp)
                                        :start (bp-cha-no start-bp)
                                        :stop (bp-cha-no stop-bp))))
                   ;; now bash the new bps to the proper values
                   ;; the new-start-bp
                   (setf (bp-row new-start-bp) new-region-row)
                   (setf (bp-cha-no new-start-bp) 0)
                   ;; and the new-stop-bp
                   (setf (bp-row new-stop-bp) new-region-row)
                   (setf (bp-cha-no new-stop-bp)
                         (length-in-chas new-region-row))))
                (t
                 ;; looks like there is more than one row in this region
                 (let ((previous-row nil))
                   (do-region-rows (rr interval)
                     (cond ((first-row? rr)
                            (setq previous-row
                                  (copy-partial-row rr
                                                    :start
                                                    (bp-cha-no
                                                     start-bp)))
                            ;; setup the new-start-bp
                            (setf (bp-row new-start-bp)
                                  previous-row)
                            (setf (bp-cha-no new-start-bp) 0))
                           ((last-row? rr)
                            (let ((new-row (copy-partial-row
                                            rr
                                            :stop
                                            (bp-cha-no stop-bp))))
                              ;; append it to the existing rows
                              (set-previous-row new-row
                                                previous-row)
                              (set-next-row previous-row new-row)
                              ;; and then setup the new-stop-bp
                              (setf (bp-row new-stop-bp) new-row)
                              (setf (bp-cha-no new-stop-bp)
                                    (length-in-chas new-row))))

                           (t
                            ;; otherwise append existing to rows
                            (let ((new-row (if c-b-a-supplied?
                                               (copy-row rr nil nil copy-box-arg)
                                               (copy-row rr))))
                              (set-previous-row new-row
                                                previous-row)
                              (set-next-row previous-row new-row)
                              (setq previous-row new-row)))))))))
        ;; now that we have set up the BP's and hooked together the
        ;; (intervening) rows, we can make the new copy of the region
        (make-editor-region new-start-bp new-stop-bp)))))


(defun set-interval-bps (interval bp1 bp2)
  (setf (interval-start-bp interval) bp1)
  (setf (interval-stop-bp interval) bp2))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;               INTERACTIONS BETWEEN REGIONS AND BOXES                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-region-check-superiors ((self box))
  (with-slots (region) self
    (let ((superior-box (superior-box self)))
      (cond ((interval? region) region)
            ((box? superior-box) (get-region-check-superiors superior-box))
            (t nil)))))

(defun set-box (interval new-box)
  (setf (interval-box interval) new-box))

(defmethod set-region ((self box) new-region)
  (setf (region self) new-region)
  (modified self))

;; Use this one from the outside since at some point in the future, we
;; may allow more than one region in a BOX or ONLY one region in ALL of BOXER
;; No matter what, this is guaranteed to get you whatever the most appropriate
;; region is if it exists

(defun get-current-region ()
  (let ((box (point-box)))
    (unless (null box)
      (get-region-check-superiors box))))

(defun get-local-region (&optional (bp *point*))
  (region (bp-box bp)))

(defun install-region (region &optional (bp *point*))
  (set-box region (bp-box bp))
  (set-region (bp-box bp) region)
  (when (eq region *region-being-defined*)
    (setq *region-being-defined* nil))
  (when (eq region *following-mouse-region*)
    (setq *following-mouse-region* nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                TOP  LEVEL  REGION  MANIPULATING  COMMANDS               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun flush-region (region)
  (when (not (null region))
    (let ((i-box (interval-box region)))
      (unless (null i-box)
        (set-region i-box nil)))
    (dolist (blinker (interval-blinker-list region))
      (remove-region-row-blinker blinker))
    (setq *region-list* (fast-delq region *region-list*))
    (when (eq region *region-being-defined*)
      (setq *region-being-defined* nil))
    (when (eq region *following-mouse-region*)
      (setq *following-mouse-region* nil))
    (deallocate-region region)))

(defun deallocate-region (region)
  "take region out of the screen-row and screen-box"
  (let ((start (interval-start-bp region))
        (stop (interval-stop-bp region)))
    (unless (eq start *point*) (deallocate-bp start))
    (unless (eq stop  *point*) (deallocate-bp stop))))

;;; snip the interval delineated by the region out of the editor hierarchy...
(defun kill-region (region)
  ;; now bash away...
  (with-region-top-level-bps (region :start-bp-name region-start-bp
                                     :stop-bp-name region-stop-bp)
    ;; first, if one of the region's BP's is the *point*, replace it
    (cond ((eq *point* (interval-start-bp region))
           (setf (interval-start-bp region) (copy-bp *point*)))
          ((eq *point* (interval-stop-bp region))
           (setf (interval-stop-bp region) (copy-bp *point*))))
    ;; if the *point* is in the region being killed,
    ;; we need to get it out of there
    (let* ((first-row (bp-row region-start-bp))
           (last-row (bp-row region-stop-bp))
           (region-box (superior-box first-row)))
      (cond ((eq first-row last-row)
             (let ((killed-row (delete-chas-between-cha-nos
                                first-row
                                (bp-cha-no region-start-bp)
                                (bp-cha-no region-stop-bp))))
               (set-bp-row (interval-start-bp region) killed-row)
               (set-bp-cha-no (interval-start-bp region) 0)
               (set-bp-row (interval-stop-bp region) killed-row)
               (set-bp-cha-no (interval-stop-bp region)
                              (length-in-chas killed-row))))
            (t
             (let ((previous-row nil))
               (do-region-rows (rr region)
                 (cond ((eq rr first-row)
                        (setq previous-row
                              (kill-chas-at-cha-no
                               rr (bp-cha-no
                                   region-start-bp)))
                        ;; setup the start-bp
                        (set-bp-row (interval-start-bp region) previous-row)
                        (set-bp-cha-no (interval-start-bp region) 0))
                       ((eq rr last-row)
                        (let ((region-last-row
                                (delete-chas-between-cha-nos
                                 last-row
                                 0
                                 (bp-cha-no region-stop-bp))))
                          ;; setup the stop-bp
                          (set-bp-row (interval-stop-bp region)
                                      region-last-row)
                          (set-bp-cha-no (interval-stop-bp region)
                                         (length-in-chas region-last-row))
                          ;; adjoin the leftovers from the 1st and last row
                          ;; of where the region was in the editor
                          (insert-row-chas-at-cha-no first-row last-row
                                                     (length-in-chas first-row))
                          (delete-row region-box last-row)
                          ;; hook it up to any existing rows
                          (set-previous-row region-last-row previous-row)
                          (set-next-row previous-row region-last-row)
                          (set-next-row region-last-row nil)))
                       (t
                        ;; must be an in between row....
                        ;; so remove it from the editor...
                        (delete-row region-box rr)
                        ;; and hook it up to the rows we already have
                        (set-previous-row rr previous-row)
                        (set-next-row previous-row rr)
                        (setq previous-row rr)))))))
      ;; move the *point* outside the killed area
      (set-bp-row *point* (bp-row region-start-bp))
      (set-bp-cha-no *point* (bp-cha-no region-start-bp))
      (set-bp-screen-box *point* (dest-screen-box (bp-box region-start-bp)
                                                  (point-screen-box)))))
  region)

;; calculate a reasonable screen-box for the *point* to end up in
;; this can be a bit complicated if the *point* is inferior to
;; the top level BP's of the region
;; the algorthm is to start at the (point-screen-box) and
;; walk outward until the actual-obj of the screen-box is EQ
;; to the bp-box
(defun dest-screen-box (bp-box screen-box)
  (let ((superior-screen-row (screen-row screen-box)))
    (cond ((eq bp-box (box-or-port-target (screen-obj-actual-obj screen-box)))
           screen-box)
          ((not (screen-row? superior-screen-row))
           (error "Can't find a screen-box to move the *point* to"))
          (t (dest-screen-box bp-box (screen-box superior-screen-row))))))

(defun yank-region (bp region &optional (force-bp-type ':moving))
  (with-region-top-level-bps (region
                              :start-bp-name start-bp
                              :stop-bp-name stop-bp)
    (action-at-bp-internal
      (let* ((box (bp-box bp))
             (row (bp-row bp))
             (cha-no (min (length-in-chas row) (bp-cha-no bp)))
             (remains (unless (> (bp-cha-no bp) (length-in-chas row))
                        (kill-chas-at-cha-no row cha-no)))
             (first-new-row (bp-row start-bp))
             (previous-added-row row)
             (last-new-row (bp-row stop-bp)))
        ;;remember where we are
        (move-bp (interval-start-bp region) (bp-values bp))
        ;; add the rows in the region to the editor hierarchy
        (cond ((eq first-new-row last-new-row)
               ;; the region is only on (part of) one row
               ;; insert the (part of) one row from the region
               (insert-row-chas-at-cha-no row first-new-row cha-no
                                          (bp-cha-no start-bp)
                                          (bp-cha-no stop-bp))
               ;; then add the remainder of the original row back
               (unless (null remains)
                 (insert-row-chas-at-cha-no row remains (length-in-chas row)))
               ;; move the interval-stop-bp as well as
               ;; the bp from the arglist (usually
               ;; the *point*) here
               (set-bp-row (interval-stop-bp region) row)
               (set-bp-cha-no (interval-stop-bp region)
                              (+& cha-no (length-in-chas last-new-row)))
               ;; This would keep the point at the beginning of the newly pasted section
               ;;  (set-bp-row bp row)
               ;;  (set-bp-cha-no bp (+& cha-no (length-in-chas last-new-row)))
               )
              (t
               ;; looks like we have more than one row in the region so
               ;; insert the part of the region's first row
               (do-region-rows (rr region)
                 (cond ((eq rr first-new-row)
                        ;; handle the first row specially
                        (insert-row-chas-at-cha-no
                         row first-new-row cha-no
                         (bp-cha-no start-bp)))
                       ((eq rr last-new-row)
                        ;; handle the last row specially...
                        (insert-row-after-row box
                                              rr
                                              previous-added-row)
                        (unless (null remains)
                          (insert-row-chas-at-cha-no
                           rr remains (length-in-chas rr)))
                        ;; move the interval-stop-bp as well as
                        ;; the bp from the arglist (usually
                        ;; the *point*) here
                        (set-bp-row (interval-stop-bp region) rr)
                        (set-bp-cha-no (interval-stop-bp region)
                                       (length-in-chas rr))
                        ;; This would keep the point at the beginning of the newly pasted section
                        ;; (set-bp-row bp rr)
                        ;; (set-bp-cha-no bp (length-in-chas rr))
                        )
                       (t
                        (insert-row-after-row box rr
                                              previous-added-row)
                        (setq previous-added-row rr))))))))))



(defun remove-region-row-blinker (row-blinker)
  ;; sgithens TODO 2023-03-23 There may be something useful to do here, such
  ;; as removing it from the actual region list... investigate further.
  ; (setf (region-row-blinker-visibility row-blinker) nil)
  ; (setf (bw::sheet-blinker-list *boxer-pane*)
  ;       (fast-delq row-blinker (bw::sheet-blinker-list *boxer-pane*)))
)
