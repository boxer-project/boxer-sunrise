;;;; -*- Mode:lisp;Syntax:Common-Lisp; Package:BOXER; Base:10.-*-
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
;;;;   This file contains low-level code for the BOXER editor.
;;;;
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;  11/27/09 added closet-row?
;;;;   7/08/09 screen-box-last-visible-bp-values
;;;;   6/24/09 defined (point-screen-row)
;;;;   3/17/07 added :graphics-info handler to initialize-instance for box's
;;;;   3/14/07 insert/delete-self-action handles spriteness
;;;;  10/13/06 added name-string-or-null, using name is deprecated, use name-string instead
;;;;   2/13/03 merged current LW and MCL files
;;;;   9/02/02 added file-box-dirty?
;;;;   6/05/02 removed start box loading in setup-editor, moved to boxwin files
;;;;   3/20/02 added shrink-on-exit? handling to exit method
;;;;   2/14/01 merged current LW and MCL files
;;;;  11/30/00 exit method fixed to handle possible null screen-structure
;;;;   7/12/00 start.box used in setup-editor
;;;;   6/16/99 fixed circular-port? to recognize circularity that doesn't include its
;;;;           original port parameter
;;;;   2/15/99 make-intialized-row checks for existing BFD vars because they may not be
;;;;           defined in the LW version until after *boxer-frame* is made
;;;;   2/09/99 avoid-box-name-conflict-by-removing-name checks for boxness
;;;;   1/ 8/99 removed keyword check for set-type, removed obsolete warning TYPE method
;;;;  10/30/98 get-string-from-status-line now looks for and can handle menu item aborts
;;;;   9/ 2/98 added bfd= from comsf.lisp so it will be defined before its' 1st use
;;;;   8/ 4/98 added interval clause to listp clause in
;;;;           queue-editor-objs-for-deallocation
;;;;   7/30/98 changed make-initialized-row to set the font on new row to avoid
;;;;           reversion to default font bug
;;;;
;;;;   7/30/98 Started Logging changes: source = boxer version 2.3beta
;;;;

(in-package :boxer)

;;;;INIT methods.

;; these next 2 should be examined more closely and the call to
;; shared-initialize shopuld be made more specific

(defun make-uninitialized-row (&rest init-plist)
  (declare (ignore init-plist))
  (let ((new (allocate-instance (find-class 'row))))
    (shared-initialize new t)
    new))

(defun make-uninitialized-box (&optional type)
  ;; a crock to get NIL into all the slots
  (let ((new (allocate-instance (find-class (or type 'doit-box)))))
    (shared-initialize new t)
    new))

;; added 9/2/98 so it can be defined before 1st use (in chunker)
(defun bfd= (fd1 fd2)
  (and (=& (bfd-font-no fd1) (bfd-font-no fd2))
       (color= (bfd-color fd1) (bfd-color fd2))))

(defun make-initialized-row (&rest init-plist)
  (let ((new-row (apply #'make-instance 'row init-plist)))
    ;; make sure the new row starts with the current font or else
    ;; the font changing mechanism will reset the current font to
    ;; the default font (see move-point-1 for details)
    (unless (or  (null *current-font-descriptor*)
                 (null *default-font-descriptor*)
                 ;; certain make-box inits can occur before the BFD's are
                 ;; defined because the LW version doesn't define the BFD's
                 ;; until after the *boxer-frame* is made
                 (bfd= *current-font-descriptor* *default-font-descriptor*))
      (insert-bfd new-row (copy-fd-at *current-font-descriptor* 0)))
    new-row))


(defun make-initialized-box (&rest init-plist)
  (apply #'make-instance  'box (list* :allow-other-keys t init-plist)))

(defmethod initialize-instance ((self row) &rest init-plist)
  (shared-initialize self t)
  (with-slots (tick superior-box previous-row next-row chas-array)
    self
    (setq tick              -1
          superior-box       (getf init-plist ':superior-box)
          previous-row       (getf init-plist ':previous-row)
          next-row           (getf init-plist ':next-row)
          chas-array         (or (getf init-plist ':chas-array)
                                 (make-chas-array)))))

(defmethod initialize-instance ((self box)  &rest init-plist)
  (shared-initialize self t)
  (with-slots (superior-row closets first-inferior-row
                            display-style-list cached-rows)
    self
    (setq ;; These we inherit from chas.
          superior-row       (getf init-plist ':superior-row)
          ;; These come from box proper.
          closets      (getf init-plist ':closets)
          first-inferior-row nil
          display-style-list (make-display-style)
          cached-rows        nil))
  ;; graphics
  (let ((gi (getf init-plist :graphics-info)))
    (unless (null gi) (setf (slot-value self 'graphics-info) gi)))
  ;; initialize flags...
  (when (null *global-hotspot-control?*)
    (when *top-left-hotspots-on?*    (set-top-left-hotspot-active? self    t))
    (when *top-right-hotspots-on?*   (set-top-right-hotspot-active? self   t))
    (when *bottom-left-hotspots-on?* (set-bottom-left-hotspot-active? self t))
    (when (null *only-shrink-wrap-text-boxes*)
      (set-bottom-right-hotspot-active? self t))
    )
  (when (eq (find-class 'box) (class-of self))
    ;; vanilla boxes not allowed so look for :TYPE in the PLIST
    (set-type self (or (getf init-plist ':type)
                       ;; or doit-box (default)
                       'doit-box)))
  (append-row self (or (getf init-plist ':first-inferior-row)
                       (make-initialized-row))))

(defmethod initialize-instance ((self doit-box) &rest init-plist)
  (unless (or (eq (class-name (class-of self)) 'doit-box)
              (getf init-plist ':type))
    (set-type self 'doit-box))
  (call-next-method))

(defmethod initialize-instance ((self data-box) &rest init-plist)
  (unless (or (eq (class-name (class-of self)) 'data-box)
              (getf init-plist ':type))
    (set-type self 'data-box))
  (call-next-method))



;;; These are used ONLY by the comaptability loader and should
;;; eventually be flushed or moved to compat-loader.lisp

(DEFMETHOD SEMI-INIT ((SELF BOX) INIT-PLIST)
           (SETF   ;;these come from box proper
                   (FIRST-INFERIOR-ROW SELF) (GETF INIT-PLIST ':SUPERIOR-ROW)
                   (CACHED-ROWS SELF)        NIL
                   (NAME SELF)       (WHEN (GETF INIT-PLIST :NAME)
                                           (MAKE-NAME-ROW `(,(GETF INIT-PLIST :NAME))))
                   (DISPLAY-STYLE-LIST SELF) (OR (GETF INIT-PLIST ':DISPLAY-STYLE-LIST)
                                                 (DISPLAY-STYLE-LIST SELF)))
           (WHEN (NAME-ROW? (slot-value self 'name))
                 (SET-SUPERIOR-BOX (slot-value self 'name) SELF))
           (SET-TYPE SELF (OR (GETF INIT-PLIST ':TYPE) 'DOIT-BOX)))

(DEFMETHOD RETURN-INIT-PLIST-FOR-FILING ((SELF BOX))
           `(:TYPE ,(class-name (class-of SELF))
                    :DISPLAY-STYLE-LIST ,(DISPLAY-STYLE-LIST SELF)))






;;;these are messages to boxes which are used for moving up and down levels
;;;in box structures

(defmethod enter ((self box) &optional (moved-p? t))
  (when (and (null (slot-value self 'first-inferior-row))
             (storage-chunk? self))
    (fill-box-from-server self))
  (setq boxer-eval:*lexical-variables-root* self)
  (when (not (null moved-p?))
    (maybe-run-trigger self 'bu::entry-trigger)))

(defmethod enter ((self port-box) &optional (moved-p? t))
  ;; if this is an unarticulated cross file port, relink it
  (when (and (null (slot-value self 'ports))
             (not (null (cross-file-port-branch-links self))))
    (articulate-target-branch (car (cross-file-port-branch-links self))))
  (setq boxer-eval::*lexical-variables-root* (ports self))
  ;; is this appropriate ?
  (when (not (null moved-p?))
    (maybe-run-trigger self 'bu::entry-trigger)))

(defmethod exit ((self box)
                 &optional (new-screen-box
                            (superior-screen-box
                             (point-screen-box)))
                 (new-actual-box (superior-box self))
                 one-step-up?)
  (when (not (null one-step-up?))
    (maybe-run-trigger self 'bu::exit-trigger)
    (maybe-run-modified-trigger self))
  (let ((new-actual-box-via-screen (when (screen-box? new-screen-box)
                                     (screen-obj-actual-obj new-screen-box))))
    (cond ((and (eq self (outermost-box))(not (null (shrink-proof? self)))))
      ((eq self (outermost-box))
       (multiple-value-bind (new-outermost-box new-outermost-screen-box)
                            (get-previous-outermost-box-values)
                            (set-outermost-box new-outermost-box new-outermost-screen-box))
       (cond ((or (eq new-actual-box new-actual-box-via-screen)
                  (and (box? new-actual-box-via-screen)
                       (eq new-actual-box (ports new-actual-box-via-screen))))
              ;; prefer the screen structure route (in order
              ;; to properly exit port inferiors) but go with
              ;; the editor structure if they dont agree (this
              ;; can happen when the *point* is deep inside
              ;; a graphics box)
              (move-point (box-self-bp-values (or (box-screen-point-is-in)
                                                  (point-box))))
              (set-point-screen-box new-screen-box))
         ((superior? new-actual-box new-actual-box-via-screen)
          ;; screen/editor mismatch, dont walk up screen structure if
          ;; it is already above us
          (move-point (box-self-bp-values (point-box))))
         (t
          ;; this clause is a hook, actually if this happens we
          ;; are probably lost so don't play with the screen structure
          (move-point (box-self-bp-values (point-box)))
          (when *boxer-system-hacker*
            (warn "EXIT: screen/editor mismatch and~%~
                           screen box is NOT above editor box"))))
       (move-point (bp-forward-cha-values *point*))	;
       (enter new-actual-box (not (eq (superior-box self) new-actual-box))))
      (t
       (cond ((or (eq new-actual-box new-actual-box-via-screen)
                  (and (box? new-actual-box-via-screen)
                       (eq new-actual-box (ports new-actual-box-via-screen))))
              ;; prefer the screen structure route (in order
              ;; to properly exit port inferiors) but go with
              ;; the editor structure if they dont agree (this
              ;; can happen when the *point* is deep inside
              ;; a graphics box)
              (move-point (box-self-bp-values (or (box-screen-point-is-in)
                                                  (point-box))))
              (set-point-screen-box new-screen-box))
         ((superior? new-actual-box new-actual-box-via-screen)
          ;; screen/editor mismatch, dont walk up screen structure if
          ;; it is already above us
          (move-point (box-self-bp-values (point-box))))
         (t
          ;; this clause is a hook, actually if this happens we
          ;; are probably lost so don't play with the screen structure
          (move-point (box-self-bp-values (point-box)))
          (when *boxer-system-hacker*
            (warn "EXIT: screen/editor mismatch and~%~
                           screen box is NOT above editor box"))))
       (when (shrink-on-exit? self) (shrink self))
       (move-point (bp-forward-cha-values *point*))
       (enter new-actual-box
              (not (eq (superior-box self) new-actual-box)))))))




;;;;PRINT-SELF methods.

(defmethod show-chas ((row row))
  (format *standard-output* "~%")
  (dolist (cha (chas row))
    (if (cha? cha)
      (format *standard-output* "~c" cha)
      (print-object cha *standard-output*))))

(defmethod print-object ((box box) stream)
  (format stream "#<~a " (class-name (class-of box)))
  (box-print-self-internal box stream)
  (format stream " >"))

;(defmethod print-object ((self graphics-box) stream)
;  (declare (ignore depth))
;  (format stream "#<~a ~a >" (class-name (class-of self)) (name-string self)))

(defmethod print-object ((row row) stream)
  (format stream "#<row ")
  (row-print-self-internal row stream)
  (format stream " >"))

(defmethod print-object ((row name-row) stream)
  (format stream "#<name-row ")
  (row-print-self-internal row stream)
  (format stream " >"))

(defmethod print-object ((self screen-box) stream)
  (format stream "#<screen-box ")
  (let ((actual-obj (screen-obj-actual-obj self)))
    (when actual-obj (box-print-self-internal actual-obj stream)))
  (format stream " >"))

(defmethod print-object ((row screen-row) stream)
  (format stream "#<screen-row ")
  (let ((actual-row (screen-obj-actual-obj row)))
    (and actual-row (row-print-self-internal actual-row stream)))
  (format stream " >"))

(defun cha-print-self-internal (cha stream)
  (cond ((box? cha)
         (format stream "[]"))
    ((standard-char-p cha)
     (format stream "~A" cha))
    (t
     (format stream "~c" cha))))

(defun row-print-self-internal (row stream)
  (if (chas-array? (chas-array row))
    (do-row-chas ((cha row) (cha-no 0 (+ cha-no 1)))
      (cond ((and (null *print-boxer-structure*) (> cha-no 5))
             (format stream "...")
             (return))
        (t
         (cha-print-self-internal cha stream))))
    (format stream "*** Malformed Row ***")))

(defun box-print-self-internal (box stream)
  (let ((first-row (row-at-row-no box 0)))
    (cond ((null first-row))
      (t
       (row-print-self-internal first-row stream)))))




;;;;USEFUL-MAPPING-FUNCTIONS

(DEFUN MAP-OVER-ALL-INFERIOR-BOXES (SUPERIOR-BOX FUNCTION)
       (DO ((ROW (FIRST-INFERIOR-ROW SUPERIOR-BOX) (NEXT-ROW ROW)))
           ((NULL ROW))
           (DO* ((CHA-NO 0 (+ CHA-NO 1))
                 (CHA (CHA-AT-CHA-NO ROW CHA-NO) (CHA-AT-CHA-NO ROW CHA-NO)))
                ((NULL CHA))
                (COND ((BOX? CHA)
                       (FUNCALL FUNCTION CHA)
                       (MAP-OVER-ALL-INFERIOR-BOXES CHA FUNCTION))))))

(DEFUN MAP-OVER-INFERIOR-BOXES (SUPERIOR-BOX FUNCTION)
       (DOLIST (ROW (ROWS SUPERIOR-BOX))
               (DOLIST (CHA (CHAS ROW))
                       (WHEN (BOX? CHA)
                             (FUNCALL FUNCTION CHA)))))



;;;; Modified


;;; When modified is called in the middle of an evaluation, instead of
;;; walking up the hierarchy immediately, we queue the object for later
;;; modification.  This can save us A LOT of work in the case of duplicates
;;; since multiple walks up the hierarchy will neccessarily share many
;;; common nodes (this gets especially bad when we have ports)
;;; What we have to watch out for is to make sure that the modified queue will
;;; get processed before any redisplay happens

;;; Still Need to figure out how to handle I/O (process the queue ?)

(defvar *objects-to-be-modified* nil)
(defvar *modified-unique-id* nil)

(defmacro with-modified-queueing (&body body)
  `(let ((*objects-to-be-modified* nil)
         (*modified-unique-id* (gensym)))
     (unwind-protect
      (progn . ,body)
      (dolist (obj *objects-to-be-modified*)
        (modified obj)))))

(defun queue-object-to-be-modified (obj)
  ;; catch the common case
  (unless (eq obj (car *objects-to-be-modified*))
    (push obj *objects-to-be-modified*)))

(defun mark-this-modified-pass (obj)
  (unless (null *modified-unique-id*)
    (putprop obj *modified-unique-id* 'modified-pass-id)))

(defun already-modified? (obj)
  (unless (null *modified-unique-id*)
    (eq *modified-unique-id* (getprop obj 'modified-pass-id))))

;;; Hook for editing in the middle of an EVAL
;;; actual definition is in vrtdef.lisp
;(defvar *propagate-modified-messages?* nil)

(defun edit-during-eval? () *propagate-modified-messages?*)

;; used to be in redisp.lisp but it seesm that (for the moment)
;; call-next-method wants the super's methods to be already
;; defined when it is compiled
(defmethod modified ((self actual-obj-subclass))
  (setf (actual-obj-tick self) (tick)))

(defmethod modified ((self box))
  (flet ((redisplay-related-modified ()
                                     ;; this has to set the timestamps
                                     (call-next-method)
                                     ;; and walk up the hierarchy
                                     (let ((superior-row (superior-row self)))
                                       (unless (null superior-row) (modified superior-row)))))
        (let ((ports (ports self)))
          (cond ((not (null *evaluation-in-progress?*))
                 ;; in the middle of an EVAL, so queue the obj for later
                 (queue-object-to-be-modified self)
                 ;; this is to keep some virtual copy procedures in sync
                 (setf (cached-rows self) nil)
                 (decache-build-function self)
                 (when (edit-during-eval?)
                   ;; let the redisplay know what is going on
                   (redisplay-related-modified))
                 (when (consp ports)
                   (dolist (port ports) (modified port))))
            ((already-modified? self))		;do nothing
            (t
             (setf (cached-rows self) nil)
             (setf (slot-value self 'virtual-copy-rows) nil)
             (when (consp ports)
               (dolist (port ports) (modified port)))
             ;; this stuff in update-bindings via magic-name-insert/-delete
             #|	   ;; magic name stuff
             (let* ((name-row (slot-value self 'name))
                    (handler (and name-row
                                  (get (get-box-name name-row)
                                       'magic-name-modified))))
               (unless (null handler)
                 (funcall handler self (superior-box self))))
             |#
             (setf (cached-code self) nil)
             (let ((old-gss (getf (slot-value self 'plist) 'old-graphics-sheets)))
               (when (consp old-gss)
                 (dolist (gs-pair old-gss)
                   (let ((ba (graphics-sheet-bit-array (cdr gs-pair))))
                     (unless (null ba) (free-offscreen-bitmap ba))))
                 (setf (getf (slot-value self 'plist) 'old-graphics-sheets)
                       nil)))
             (decache-build-function self)
             ;; decache visible-screen-objs
             (decache-visible-screen-objs self)
             ;; finally mark this box as having been modified by THIS pass
             (mark-this-modified-pass self)
             (redisplay-related-modified))))))

;;; These are needed to handle :MODIFIED for circular structures
(defvar *ports-already-modified* nil)

(defmethod modified ((self port-box))
  (unless (member self *ports-already-modified*)
    (let ((*ports-already-modified* (append *ports-already-modified*
                                            (list self))))
      (call-next-method))))

(defmethod modified ((self row))
  (let ((superior-box (slot-value self 'superior-box)))
    (cond ((not (null *evaluation-in-progress?*))
           ;; in the middle of EVAL, so queue the obj for later
           (queue-object-to-be-modified self)
           ;; virtual copy can refer to these slots
           (setf (slot-value self 'cached?) nil)
           (setf (slot-value self 'cached-eval-objs) nil)
           (setf (slot-value self 'cached-chas) nil)
           (setf (slot-value self 'cached-evrow) nil)
           (setf (slot-value self 'cached-chunks) nil)
           (when (edit-during-eval?)
             ;; keep the redisplay informed about what has changed
             (call-next-method)			;tick
             (unless (null superior-box)	;walk up
               (modified superior-box))))
      ((already-modified? self))		;do nothing
      (t
       (setf (slot-value self 'cached?)          nil
             (slot-value self 'cached-chunks)    nil
             (slot-value self 'cached-eval-objs) nil
             (slot-value self 'cached-evrow)     nil
             (slot-value self 'cached-chas)      nil)
       (call-next-method)			;tick
       (unless (null superior-box)		;walk up
         (modified superior-box))
       (mark-this-modified-pass self)))))

;(defmethod modified ((self name-row))
;  (let ((superior-box (superior-box self)))
;    (unless (null superior-box)
;      ;; this is probably the wrong level of modularity
;      ;; for this (but convenient)
;      (invalidate-border-cache superior-box)
;#|    ;; The following calls to modified were necessary
;      ;; when typing in the name row caused the entire boxc
;      ;; to move over (moving boxes over)
;      (dolist (row (rows superior-box))
;	(modified row))|#
;    )
;    (call-next-method)))
;

;;; FS support...

(defvar *dirty-file-boxes* nil)

(defvar *processing-dirty-file-boxes?* nil)

(defvar *handled-dirty-file-boxes* nil)

(defmacro do-dirty-file-boxes ((boxvar) &body body)
  `(let ((*processing-dirty-file-boxes?* t)
         (*handled-dirty-file-boxes* nil))
     (unwind-protect
      (dolist (,boxvar *dirty-file-boxes*) . ,body)
      (setq *dirty-file-boxes*
            (remove-if #'(lambda (dfb)
                                 (fast-memq dfb *handled-dirty-file-boxes*))
                       *dirty-file-boxes*)))))

;; walk up the hierarchy until we find a file box, then mark it
(defmethod mark-file-box-dirty ((box box))
  (cond ((storage-chunk? box)
         (unless (file-modified? box)
           (push box *dirty-file-boxes*)
           (setf (file-modified? box) t)))
    (t (let ((sup (superior-box box)))
         (when (box? sup) (mark-file-box-dirty sup))))))

(defmethod mark-file-box-dirty ((self row))
  (let ((supbox (superior-box self)))
    (when (box? supbox) (mark-file-box-dirty supbox))))

(defmethod mark-file-box-clean ((box box))
  (cond ((storage-chunk? box)
         (if *processing-dirty-file-boxes?*
           (push box *handled-dirty-file-boxes*)
           (setq *dirty-file-boxes* (fast-delq box *dirty-file-boxes*)))
         (setf (file-modified? box) nil))
    (t (let ((sup (superior-box box)))
         (when (box? sup) (mark-file-box-clean sup))))))

(defmethod file-box-dirty? ((box box))
  (cond ((storage-chunk? box)
         (file-modified? box))
    (t (let ((sup (superior-box box)))
         (when (box? sup) (file-box-dirty? sup))))))



;;; Returning resources
;;; objects which are (permanently) removed from the editor hierarchy
;;; get queued on a deallocation list.  The list is processed and the
;;; objects in it are recycled either immediately or else during idle times.
;;; depending upon the value of *queue-editor-objects-for-deallocation?*

(defvar *queue-editor-objects-for-deallocation?* nil)

(defvar *editor-objs-waiting-to-be-deallocated* nil)

(defmethod queue-editor-obj-for-deallocation ((self box))
  (if (null *queue-editor-objects-for-deallocation?*)
    (deallocate-self self)
    (push self *editor-objs-waiting-to-be-deallocated*)))

(defmethod queue-editor-obj-for-deallocation ((self row))
  (if (null *queue-editor-objects-for-deallocation?*)
    (deallocate-self self)
    (push self *editor-objs-waiting-to-be-deallocated*)))

(defun queue-editor-objs-for-deallocation (objs)
  (cond ((or (null objs) (characterp objs) (eq objs :newline)))
    ((listp objs)
     (dolist (obj objs)
       (cond ((or (characterp obj) (null obj) (eq obj :newline)))
         ((listp obj) (queue-editor-objs-for-deallocation obj))
         ((or (box? obj) (row? obj))
          (queue-editor-obj-for-deallocation obj))
         ((interval? obj)
          (do-interval-chas (cha obj)
            (unless (cha? cha) (queue-editor-obj-for-deallocation cha))))
         ;; regions ?
         (t (warn "Don't know how to deallocate ~A" obj)))))
    ((or (box? objs) (row? objs))
     (queue-editor-obj-for-deallocation objs))
    ((interval? objs)
     (do-interval-chas (cha objs)
       (unless (cha? cha) (queue-editor-obj-for-deallocation cha))))
    (t (warn "Don't know how to deallocate ~A" objs))))

(defun deallocate-editor-objs ()
  (dolist (obj *editor-objs-waiting-to-be-deallocated*)
    (cond ((or (box? obj) (row? obj))
           (deallocate-self obj))
      (t
       (warn "Unhandled Object in Deallocation queue: ~A" obj))))
  (setq *editor-objs-waiting-to-be-deallocated* nil))

;; this should be called during idle times, it only deallocates one entry
;; and then returns so whatever system dependent inoput routine we are
;; using can check for idle
(defun process-editor-obj-deallocation-queue ()
  (when (and *queue-editor-objects-for-deallocation?*
             *editor-objs-waiting-to-be-deallocated*)
    (let ((obj (car *editor-objs-waiting-to-be-deallocated*)))
      (cond ((or (box? obj) (row? obj)) (deallocate-self obj))
        (t (warn "Unhandled Object in Dealocation queue: ~A" obj)))
      (setq *editor-objs-waiting-to-be-deallocated*
            (cdr *editor-objs-waiting-to-be-deallocated*)))))

;; this should probably check a PTTT table and remove it self
(defmethod deallocate-self ((self port-box))
  (unless (null (ports self))
    (remove-port (ports self) self))
  )

;;; Note: if we want to be clever about this we could check a timestamp
;;; of idle before recursing, if there is pending input, then push the
;;; inferior rows onto the queue for later processing

(defmethod deallocate-self ((self box))
  (when (storage-chunk? self)
    ;; Inform the server that the box will be deleted
    (boxnet::queue-for-server-deletion self))
  ;; should deallocate system dependent objects which have
  ;; been hung on the box
  (deallocate-system-dependent-structures self)
  (do-box-rows ((row self))
    (deallocate-self row)))

(defmethod deallocate-self ((self row))
  (let* ((chas (slot-value self 'chas-array))
         (contents (%sv-contents chas))
         (length (%sv-fill-pointer chas)))
    (dotimes& (i length)
      (let ((item (svref& contents i)))
        (unless (characterp item)
          (deallocate-self item))
        (setf (svref& contents i) nil)))
    (free-storage-vector chas nil)))

;;;;INCREMENT and SET-TYPE

(DEFVAR *TOGGLING-BOX-TYPES* `(DOIT-BOX DATA-BOX)
        ;; Preservation of the following documentation string
        ;; has been paid for by the Gregor Kiczales Hysterical Society.
        "This is a circular list of the different possible types of boxes.
   The list is circular to make it easy to define a next type for
   each type, this is used by (:method box :increment-type).")

(DEFUN TOGGLING-BOX-TYPES-NEXT-BOX-TYPE (OLD-TYPE)
       (or (cadr (member old-type *toggling-box-types*))
           (car *toggling-box-types*)))
#|
(DEFUN TOGGLING-BOX-TYPES-NEXT-BOX-TYPE (OLD-TYPE)
       (LET ((POS (POSITION OLD-TYPE *TOGGLING-BOX-TYPES*))
             (LEN (LENGTH *TOGGLING-BOX-TYPES*)))
            (COND ((NULL POS) (CAR *TOGGLING-BOX-TYPES*))
                  (T (NTH (REM (+ POS 1) LEN) *TOGGLING-BOX-TYPES*)))))
|#

(defmethod set-type ((self box) new-type)
  (let ((new-class (find-class new-type)))
    (unless (eq (class-of self) new-class)
      (change-class self new-class)
      (modified self))))

(defmethod set-class ((self box) new-class)
  ;; should do some error checking, NO ?
  (unless (eq (class-of self) new-class)
    (change-class self new-class)
    (modified self)))

(defmethod toggle-type ((self box))
  (set-type self
            (toggling-box-types-next-box-type (class-name (class-of self)))))





;;;; PORTS.

(defvar .LINK-TARGET-ALIST. nil
  "An association list of ported-to boxes and their copies. ")

(defvar .PORT-COPY-LIST. nil
  "A list of port copies which may want to have their destination changed at the end of a
higher level copy operation. ")

(defvar .copying-sprite-instance-var-alist. nil
  "An alist of old,new sprite-instance variable box pairs")

(defmacro with-editor-port-relinking (&body body)
  `(let ((.link-target-alist. nil)
         (.port-copy-list. nil)
         (.copying-sprite-instance-var-alist. nil))
     (prog1 (progn . ,body)
            (dolist (port .port-copy-list.)
              (let ((target-pair (fast-assq (ports port) .link-target-alist.)))
                (when (not-null target-pair)
                  (set-port-to-box port (cdr target-pair))))))))

;;; inform the box that it has a port and vice versa.
(defmethod set-port-to-box ((self port-box) new-value)
  (add-port new-value self)
  (setf (ports self) new-value))

(defmethod add-port ((self box) port-to-add)
  (unless (member port-to-add (ports self))
    (push port-to-add (ports self))))

(defmethod add-port ((self port-box) port-to-add)
  (add-port (ports self) port-to-add))

(defmethod remove-port ((self box) port-to-delete)
  (setf (ports self) (delete port-to-delete (ports self))))

;; what happens when a port's target is removed from the hierarchy ?
;; This is just a stub until we decide what to do.  Old proposal to
;; mark the port as "broken" needs some redisplay hacking
(defmethod target-has-been-deleted-handler ((self port-box))
  ;(declare (ignore self))
  nil)

;; Another stub
(defmethod target-has-been-inserted-handler ((self port-box) target)
  ;(declare (ignore self))
  target)

;; Doesn't create the back pointer so that the port can eventually be GC'd
;; ports which use this should NEVER, NEVER, NEVER be inserted into the editor
(defmethod set-port-to-box-for-eval ((self port-box) new-value)
  (setf (ports self) new-value))

(defmethod first-inferior-row ((self port-box))
  (let ((target (ports self)))
    (when (not-null target) (first-inferior-row target))))

(defmethod first-inferior-obj ((self port-box))
  (let ((target (ports self)))
    (when (not-null target) (first-inferior-row target))))

(defmethod row-at-row-no ((self port-box) row-no)
  (let ((target (ports self)))
    (when (not-null target) (row-at-row-no target row-no))))

(defmethod actual-obj-tick ((self port-box))
  (max (slot-value (slot-value self 'ports) 'tick) (slot-value self 'tick)))

(defmethod clear-ports ((self box))
  ;; for debugging
  (setf (ports self) nil))

; this hsould be automagically generated now
;(defmethod ports ((self box))
;  (ports self))

(defmethod graphics-sheet ((self port-box))
  (let ((target (ports self)))
    (when (not-null target) (graphics-sheet target))))

(defmethod bit-array-wid ((self port-box))
  (let ((target (ports self)))
    (when (not-null target) (bit-array-wid target))))

(defmethod bit-array-hei ((self port-box))
  (let ((target (ports self)))
    (when (not-null target) (bit-array-hei target))))

(defmethod graphics-sheet-size ((self port-box))
  (let ((target (ports self)))
    (when (not-null target) (graphics-sheet-size target))))

(defmethod toggle-type ((self port-box))
  (let ((target (ports self)))
    (when (not-null target) (toggle-type target))))

(defmethod set-type ((self port-box) type)
  (let ((target (ports self)))
    (when (not-null target) (set-type target type))))

(defmethod circular-port? ((self box) &optional ignore)
  (declare (ignore ignore))
  nil)

(defmethod circular-port? ((self port-box)
                           &optional (target (slot-value self 'ports)))
  (or (superior? self target)
      (let ((searched-ports (list self)))
        (declare (special searched-ports))
        (dolist (bl (branch-links target))
          (when (and (port-branch-link? bl)
                     (circular-port-r? self (link-target bl)))
            (return t))))))

(defun circular-port-r? (port target)
  (declare (special searched-ports))
  (or (superior? port target)
      (dolist (bl (branch-links target))
        (when (port-branch-link? bl)
          (let ((pblp (link-port bl)))
            (cond ((fast-memq pblp searched-ports)
                   (return t))
              (t
               (push pblp searched-ports)
               (when (circular-port-r? port (link-target bl))
                 (return t)))))))))


(defun direct-circular-port-depth (port-box)
  (do ((i 0 (1+& i))
       (target (ports port-box))
       (box port-box (superior-box box)))
    ((not (box? box)) nil)
    (when (eq box target) (return i))))

(defun direct-circular-port-target (port depth)
  (do ((i 0 (1+& i))
       (target port (superior-box target)))
    ((=& i depth) target)))



;;; in/out of the editor hierarchy

;;; hooks for various maintenance tasks that boxes have to perform themselves
;;; when they are inserted/deleted from the hierarchy
;;; Every Box needs to hack the namespace and the deallocation of screen objs

(defmethod delete-self-action ((self box) &optional superior-box)
  (setf (actual-obj-screen-objs self) nil)
  (unless *boxes-being-temporarily-deleted*
    ;; update port/link info for virtual copy if the box being
    ;; This is a call to the virtual-copy-subclass method this used
    ;; to be in calculated in the arglist but PCL cant seem to handle it
    (or superior-box (setq superior-box (superior-box self)))
    (delete-self-link-action self superior-box)
    ;; update the namespace
    (when (not-null superior-box)
      ;; handle sprites
      (when (sprite-box? self) (remove-inferior-sprite superior-box self))
      (with-mouse-cursor (:wait)
        ;; this does a tree walk so it can take a while
        (boxer-eval::evaluator-delete-self-action self superior-box))
      (boxnet::storage-chunk-delete-self-action self)
      (when (not-null (exports self))
        (boxer-eval::remove-all-exported-bindings self superior-box)
        (boxer-eval::unexport-inferior-properties self superior-box)))))

(defmethod delete-self-action ((self port-box) &optional superior-box)
  (unless *boxes-being-temporarily-deleted*
    ;; this used to be in calculated in the arglist but
    ;; PCL cant seem to handle it
    (or superior-box (setq superior-box (superior-box self)))
    (delete-self-link-action self superior-box)
    ;; update the namespace
    (when (not-null superior-box)
      (boxer-eval::evaluator-delete-self-action self superior-box))
    ;; Do this in the deallocate self method so that port (un)cracking
    ;; works correctly when target is killed/yanked at the same time
    ;; as the port
    ;; finally remove the port from the target
    ;(unless (null (ports self))
    ;  (remove-port (ports self) self))
    ))

(defmethod insert-self-action ((self box) &optional superior)
  (unless *boxes-being-temporarily-deleted*
    ;; update port/link info for virtual copy if the box being
    ;; This is a call to the virtual-copy-subclass
    (or superior (setq superior (superior-box self)))
    (insert-self-link-action self superior)
    ;; update the binding information
    (when (name-row? (slot-value self 'name))
      (update-bindings (slot-value self 'name) t))
    (when (not (null (slot-value self 'exports)))
      (when (not-null superior)
        (boxnet::storage-chunk-insert-self-action self)
        (boxer-eval::propagate-all-exported-bindings self superior)
        (boxer-eval::export-inferior-properties self superior)))
    (when (and (sprite-box? self) (not-null superior))
      (add-inferior-sprite superior self))))

(defmethod insert-self-action ((self port-box) &optional superior)
  (unless *boxes-being-temporarily-deleted*
    (or superior (setq superior (superior-box self)))
    (unless (null (ports self))
      (add-port (ports self) self))
    (insert-self-link-action self superior)
    (when (name-row? (slot-value self 'name))
      (update-bindings (slot-value self 'name) t))))

(defun string-from-eval-prop (ep)
  (let ((prop (eval-prop-prop ep))
        (contents (eval-prop-contents ep)))
    (format nil "~A~A" prop (if (eval-prop? contents)
                              (string-from-eval-prop contents)
                              contents))))

(defun get-box-name (name-row)
  (if (row? name-row)
    (let ((row-entries (eval-objs name-row)))
      (cond ((null row-entries) nil)
        ((eval-prop? (car row-entries))
         (intern-in-bu-package (string-from-eval-prop (car row-entries))))
        (t (intern-in-bu-package
            (let ((name (symbol-format nil "~A" (car row-entries))))
              (dolist (entry (cdr row-entries) name)
                (setq name (concatenate 'string name
                                        (symbol-format nil "_~A" entry)))))))))
    nil))

(defmethod name-row ((self box))
  (let ((name (slot-value self 'name)))
    (when (name-row?  name) name)))



;;;; BP's

(DEFUN SET-BP-ROW (BP NEW-ROW)
       (CHECK-BP-ARG BP)
       ; sgithens TODO (CHECK-ROW-ARG NEW-ROW)
       (LET ((OLD-ROW (BP-ROW BP)))
            (UNLESS (EQ OLD-ROW NEW-ROW)
                    (when (eq bp *point*) (new-point-row new-row))
                    (SETF (BP-ROW BP) NEW-ROW)
                    (UNLESS (NULL OLD-ROW)
                            (SETF (ROW-BPS OLD-ROW) (DELETE BP (ROW-BPS OLD-ROW))))
                    (SETF (ROW-BPS NEW-ROW) (CONS BP (ROW-BPS NEW-ROW))))))

;; a hook for functions to call when the *point*'s row changes
(defun new-point-row (new-row)
  (declare (ignore new-row))
  (let ((old-row (bp-row *point*)))
    (when (name-row? old-row)
      ;; moving off a name-row
      (let ((box (superior-box old-row)))
        (cond ((and (name-row? old-row) (null (get-box-name old-row)))
               ;; get rid of the name row if there are no more characters in it
               (update-bindings old-row)
               ;; probaly wrong level of modularity but...
               (setf (slot-value box 'name) nil) (modified box))
          ((name-row? old-row)
           ;; if there is a name row with stuff in it, make sure the
           ;; binding info is updated
           (update-bindings old-row)))))))

(DEFUN SET-BP-CHA-NO (BP NEW-CHA-NO)
       (CHECK-TYPE NEW-CHA-NO INTEGER "A number")
       (SETF (BP-CHA-NO BP) NEW-CHA-NO))

(DEFUN SET-BP-SCREEN-BOX (BP NEW-SCREEN-BOX)
       (CHECK-BP-ARG BP)
       ; sgithens TODO (OR (NULL NEW-SCREEN-BOX) (CHECK-SCREEN-BOX-ARG NEW-SCREEN-BOX))
       (LET ((OLD-SCREEN-BOX (BP-SCREEN-BOX BP)))
            (UNLESS (EQ OLD-SCREEN-BOX NEW-SCREEN-BOX)
                    (SETF (BP-SCREEN-BOX BP) NEW-SCREEN-BOX)
                    (UNLESS (NULL OLD-SCREEN-BOX) (DELETE-BP OLD-SCREEN-BOX BP))
                    (ADD-BP NEW-SCREEN-BOX BP))))

(DEFUN SET-BP-FROM-BP (BP FROM-BP &OPTIONAL (SCREEN-BOX-TOO? T))
       "Changes the first BP to point to the same place as the
   second BP without the type. "
       (CHECK-BP-ARG BP)
       (CHECK-BP-ARG FROM-BP)
       (SET-BP-CHA-NO BP (BP-CHA-NO FROM-BP))
       (SET-BP-ROW    BP (BP-ROW FROM-BP))
       (WHEN SCREEN-BOX-TOO?
             (SET-BP-SCREEN-BOX BP (BP-SCREEN-BOX FROM-BP))))

(DEFUN SET-BP-TYPE (BP NEW-TYPE)
       (COND ((MEMBER NEW-TYPE '(:MOVING :FIXED))
              (SETF (BP-TYPE BP) NEW-TYPE))
             (T
              (BARF "~S is an illegal type for a BP." new-type))))

;;; This is useful. Note that setting a BP's Box doesn't make any sense.

(DEFUN BP-BOX (BP)
       (SUPERIOR-BOX (BP-ROW BP)))

;;; Comparing BP's. BP-> returns T if <BP1> is farther along in the buffer
;;; than <BP2>.  Note that farther along is defined in a top-to-bottom
;;; left-to-right sense and that depth is ignored since the function traverses
;;;  upward into the lowest common superior box before doing the compare

;; Both rows are assumed to be in the same box.
(defun row-> (row1 row2 &optional box)
  (declare (ignore box))
  (do ((row (previous-row row1) (previous-row row)))
    ((null row) nil)
    (when (eq row row2) (return t))))

(defun row->= (row1 row2 &optional box)
  (declare (ignore box))
  (do ((row row1 (previous-row row)))
    ((null row) nil)
    (when (eq row row2) (return t))))

(defun row-< (row1 row2 &optional box)
  (declare (ignore box))
  (do ((row (next-row row1) (next-row row)))
    ((null row) nil)
    (when (eq row row2) (return t))))

(defun row-<= (row1 row2 &optional box)
  (declare (ignore box))
  (do ((row row1 (next-row row)))
    ((null row) nil)
    (when (eq row row2) (return t))))

;; this assumes that the BP's are in the same box and have already been
;; decoded into ROWs and CHA-NOs and returns T if the BP represented
;; by ROW1, CHA-NO1 come FIRST
(defsubst bp-compare-internal-simple (row1 row2 cha-no1 cha-no2)
  (cond ((and (eq row1 row2) (= cha-no1 cha-no2)) :equal)
    ((and (eq row1 row2) (< cha-no1 cha-no2)) t)
    ((eq row1 row2)                  	  nil)
    ((row-< row1 row2)                        t)
    (t                                        nil)))

;; this gets used ONLY IF the BP's aren't in the same box
;; returns the BP which occurs FIRST
;; since we are doing all this marching up and down in box structure, we
;; might as well also throw back the top level box which is inferior to
;; the lowest common superior for each BP so that other functions won't
;; have to do all this work.  The order of the values returned are:
;; 1) Leading BP. 2) Leading box. 3) Trailing Box

(defsubst bp-compare-internal-hairy (bp1 bp2 row1 row2 box1 box2)
  (multiple-value-bind (top12 path12)
                       (find-path box1 box2)
                       (multiple-value-bind (top21 path21)
                                            (find-path box2 box1)
                                            (let ((apparent-row1 (when (not-null (car path21))
                                                                   (superior-row (car path21))))
                                                  (apparent-row2 (when (not-null (car path12))
                                                                   (superior-row (car path12)))))
                                              (cond ((and (null top12)	;BP2 is in some inferior of BOX1
                                                          (bp-compare-internal-simple
                                                           row1            apparent-row2
                                                           (bp-cha-no bp1) (cha-cha-no apparent-row2 (car path12))))
                                                     (values bp1 (car path21) (car path12)))
                                                ((null top12)
                                                 (values bp2 (car path12) (car path21)))
                                                ((and (null top21)	;BP1 is in some inferior of BOX2 and
                                                      (eq :equal
                                                          (bp-compare-internal-simple
                                                           apparent-row1 row2
                                                           (cha-cha-no apparent-row1 (car path21))
                                                           (bp-cha-no bp2))))
                                                 (values bp2 (car path12) (car path21)))
                                                ((and (null top21)	;BP1 is in some inferior of BOX2
                                                      (bp-compare-internal-simple
                                                       apparent-row1 row2
                                                       (cha-cha-no apparent-row1 (car path21)) (bp-cha-no bp2)))
                                                 (values bp1 (car path21) (car path12)))
                                                ((null top21)
                                                 (values bp2 (car path12) (car path21)))
                                                ;; neither box is contained in the other
                                                ((bp-compare-internal-simple
                                                  apparent-row1 apparent-row2
                                                  (cha-cha-no apparent-row1 (car path21))
                                                  (cha-cha-no apparent-row2 (car path12)))
                                                 (values bp1 (car path21) (car path12)))
                                                (t (values bp2 (car path12) (car path21))))))))

(defun bp-compare (bp1 bp2)
  "returns the BP which occurs FIRST. If they are in the same place, the
first one is returned.  If they are on different levels, and the superior BP
points to the Box which contains the lower BP,then the superior BP is returned"
  (let ((row1 (bp-row bp1)) (box1 (bp-box bp1))
                            (row2 (bp-row bp2)) (box2 (bp-box bp2)))
    (cond ((and (eq box1 box2)
                (bp-compare-internal-simple row1 row2
                                            (bp-cha-no bp1) (bp-cha-no bp2)))
           bp1)
      ((eq box1 box2) bp2)
      ;; so much for the simple cases,it looks like we have to do some work
      (t (bp-compare-internal-hairy bp1 bp2 row1 row2 box1 box2)))))

(defun bp-< (bp1 bp2)
  (if (eq bp2 (bp-compare bp1 bp2)) nil t))

(defun bp-> (bp1 bp2)
  (if (eq bp1 (bp-compare bp1 bp2)) nil t))

(defun bp-= (bp1 bp2)
  (and (eq (bp-row bp1)    (bp-row bp2))
       (=  (bp-cha-no bp1) (bp-cha-no bp2))))

;;; These two functions take two BP's and return two BP's which are ordered
;;; according to location in the BUFFER and are guaranteed to be at the same
;;; level i.e. corresponding to rows in the same BOX.  Note that when the
;;; second BP is in a subbox, the returned second BP's CHA-NO will be one
;;; greater than the Box's own CHA-NO so that the Box itself will be included
;;; in the specified region.
;;; Note that ORDER-BPS creates new BP's to return so we don't have to worry
;;; about accidently mutating something like the *POINT*

(defun order-bps (bp1 bp2)
  (let ((start-bp (make-bp :fixed))
        (stop-bp  (make-bp :fixed)))
    (multiple-value-bind (first-bp first-box last-box)
                         (bp-compare bp1 bp2)
                         (cond ((and (null first-box)
                                     ;; both BPs are at the same level
                                     (null last-box)
                                     (eq first-bp bp1))		;and are ordered correctly
                                                         (move-bp start-bp (bp-values bp1))
                                                         ;; place the BP's to be returned in the right places
                                                         (move-bp stop-bp (bp-values bp2))
                                                         (values start-bp stop-bp))
                           ((and (null first-box) (null last-box))
                            (move-bp start-bp (bp-values bp2))
                            (move-bp stop-bp (bp-values bp1))
                            (values start-bp stop-bp))
                           ;; looks like the BPs are in different boxes
                           ;; first we look for the case where on BP's box is
                           ;; inside the other one's
                           ((and (null first-box)
                                 ;; the leading BP is at the right level
                                 (eq first-bp bp1))
                            (move-bp start-bp (bp-values bp1))
                            ;; point to where the box is
                            (move-bp stop-bp (box-self-bp-values last-box))
                            (set-bp-cha-no stop-bp (1+ (bp-cha-no stop-bp)))
                            ;; include the box itself
                            (values start-bp stop-bp))
                           ((null first-box)
                            (move-bp start-bp (bp-values bp2))
                            (move-bp stop-bp (box-self-bp-values last-box))
                            ;; point to where the box is
                            (set-bp-cha-no stop-bp (1+ (bp-cha-no stop-bp)))
                            ;; include the box itself
                            (values start-bp stop-bp))
                           ((and (null last-box) (eq first-bp bp1))
                            ;; the trailing BP is at the right level
                            (move-bp start-bp (box-self-bp-values first-box))
                            (move-bp stop-bp (bp-values bp2))
                            (values start-bp stop-bp))
                           ((null last-box)
                            (move-bp start-bp (box-self-bp-values first-box))
                            (move-bp stop-bp (bp-values bp1))
                            (values start-bp stop-bp))
                           ;; looks like neither BP was at the right level
                           (t
                            (move-bp start-bp (box-self-bp-values first-box))
                            (move-bp stop-bp  (box-self-bp-values last-box))
                            (set-bp-cha-no stop-bp (1+ (bp-cha-no stop-bp)))
                            (values start-bp stop-bp))))))

(defun deallocate-bp (bp)
  (unless (null (bp-row bp))
    (delete-bp (bp-row bp) bp))
  (unless (null (bp-screen-box bp))
    (delete-bp (bp-screen-box bp) bp)))

;;;move-point moves the *POINT* BP

(defun move-point-1 (new-row new-cha-no &optional(new-screen-box nil))
  (unless (null new-screen-box)
    (set-bp-screen-box *point* new-screen-box))
  (set-bp-row *point* new-row)
  (set-bp-cha-no *point* new-cha-no)
  (let ((bfd (bp-closest-bfd *point*)))
    (unless (bfd= *current-font-descriptor*  bfd)
      (setf (bfd-font-no *current-font-descriptor*) (bfd-font-no bfd)
            (bfd-color   *current-font-descriptor*) (bfd-color   bfd)))))

(defun move-bp-1 (bp new-row new-cha-no &optional (new-screen-box nil))
  (unless (null new-screen-box)
    (set-bp-screen-box bp new-screen-box))
  (set-bp-row bp new-row)
  (set-bp-cha-no bp new-cha-no))

;(DEFUN BP-COMPUTE-NEW-SCREEN-BOX 'IGNORE)

(DEFUN BP-COMPUTE-NEW-SCREEN-BOX-OUT (OLD-BOX NEW-BOX OLD-SCREEN-BOX)
       (LET ((LEVEL (LEVEL-OF-SUPERIORITY NEW-BOX OLD-BOX))
             (NEW-SCREEN-BOX OLD-SCREEN-BOX))
            (DOTIMES (I LEVEL)
                     (SETQ NEW-SCREEN-BOX (SCREEN-BOX NEW-SCREEN-BOX)))
            NEW-SCREEN-BOX))

(DEFUN BP-COMPUTE-NEW-SCREEN-BOX-IN (OLD-BOX NEW-BOX OLD-SCREEN-BOX)
       (COND ((EQ NEW-BOX OLD-BOX) OLD-SCREEN-BOX)
             (T
              (ALLOCATE-SCREEN-OBJ-FOR-USE-IN
               NEW-BOX
               (BP-COMPUTE-NEW-SCREEN-BOX-IN
                OLD-BOX (SUPERIOR-BOX NEW-BOX) OLD-SCREEN-BOX)))))

(DEFUN VISIBLE-SCREEN-OBJ-OF-INFERIOR-ACTUAL-OBJ (INFERIOR-ACTUAL-OBJ
                                                  SUPERIOR-SCREEN-OBJ)
       (CAR (MEMBER SUPERIOR-SCREEN-OBJ (DISPLAYED-SCREEN-OBJS INFERIOR-ACTUAL-OBJ)
                    :TEST #'(LAMBDA (SB SR) (SUPERIOR? SR SB)))))

(DEFUN LOWEST-VISIBLE-BOX (SUPERIOR-SCREEN-BOX BOXES)
       (DOTIMES (N (LENGTH BOXES) (CAR (LAST BOXES)))
                (LET* ((BOX (NTH N BOXES))
                       (SCREEN-BOX (VISIBLE-SCREEN-OBJ-OF-INFERIOR-ACTUAL-OBJ
                                    BOX SUPERIOR-SCREEN-BOX)))
                      (WHEN (NULL SCREEN-BOX) (RETURN (WHEN (> N 0) (NTH (1- N) BOXES)))))))



(DEFUN BP-FORWARD-CHA-VALUES (BP &OPTIONAL
                                 (TIMES 1) (NO-OF-TIMES-TO-COUNT-CRLF 1))
       (CHECK-BP-ARG BP)
       (BP-FORWARD-CHA-VALUES-1 (BP-ROW BP) (BP-CHA-NO BP)
                                TIMES NO-OF-TIMES-TO-COUNT-CRLF))

(DEFUN BP-FORWARD-CHA-VALUES-1 (OLD-ROW OLD-CHA-NO TIMES
                                        NO-OF-TIMES-TO-COUNT-CRLF)
       (LET ((OLD-ROW-LENGTH-IN-CHAS (LENGTH-IN-CHAS OLD-ROW)))
            (COND ((<= (+ OLD-CHA-NO TIMES) OLD-ROW-LENGTH-IN-CHAS)
                   ;; The destination is is this row. Our job easy.
                   (VALUES OLD-ROW (+ OLD-CHA-NO TIMES)))
                  ((NULL (NEXT-ROW OLD-ROW))
                   ;; The destination isn't in this row, and there
                   ;; is no next row. Just go the the end of this
                   ;; row.
                   (VALUES OLD-ROW OLD-ROW-LENGTH-IN-CHAS))
                  (T
                   ;; The destination isn't in this row, and there
                   ;; is a next row to go to. Move the BP to the
                   ;; beginning of the next row and call ourselves
                   ;; recursively.
                   (BP-FORWARD-CHA-VALUES-1 (NEXT-ROW OLD-ROW)
                                            0
                                            (- TIMES
                                               (- OLD-ROW-LENGTH-IN-CHAS OLD-CHA-NO)
                                               NO-OF-TIMES-TO-COUNT-CRLF)
                                            NO-OF-TIMES-TO-COUNT-CRLF)))))

(DEFUN BP-BACKWARD-CHA-VALUES (BP &OPTIONAL
                                  (TIMES 1) (NO-OF-TIMES-TO-COUNT-CRLF 1))
       (CHECK-BP-ARG BP)
       (BP-BACKWARD-CHA-VALUES-1 (BP-ROW BP) (BP-CHA-NO BP)
                                 TIMES NO-OF-TIMES-TO-COUNT-CRLF))

(DEFUN BP-BACKWARD-CHA-VALUES-1 (OLD-ROW OLD-CHA-NO TIMES
                                         NO-OF-TIMES-TO-COUNT-CRLF)
       (COND ((<= TIMES OLD-CHA-NO)
              ;; The destination is in this row. Our job is easy.
              (VALUES OLD-ROW (- OLD-CHA-NO TIMES)))
             ((NULL (PREVIOUS-ROW OLD-ROW))
              ;; The destination isn't in this row, and there
              ;; is no previous row to go to. Just go to the
              ;; beginning of this row.
              (VALUES OLD-ROW 0))
             (T
              ;; The destination isn't in this row, and there
              ;; is a previous row to go to. Go to the end of
              ;; the previous row and call ourselves recursivley.
              (LET ((OLD-PREVIOUS-ROW (PREVIOUS-ROW OLD-ROW)))
                   (BP-BACKWARD-CHA-VALUES-1 OLD-PREVIOUS-ROW
                                             (LENGTH-IN-CHAS OLD-PREVIOUS-ROW)
                                             (- TIMES
                                                OLD-CHA-NO
                                                NO-OF-TIMES-TO-COUNT-CRLF)
                                             NO-OF-TIMES-TO-COUNT-CRLF)))))



(DEFUN CHA-BP-VALUES (CHA)
       (LET ((ROW (SUPERIOR-ROW CHA)))
            (VALUES ROW (CHA-CHA-NO ROW CHA))))

(DEFUN CHA-NEXT-BP-VALUES (CHA)
       (LET ((ROW (SUPERIOR-ROW CHA)))
            (VALUES ROW (+ (CHA-CHA-NO ROW CHA) 1))))

(DEFUN ROW-FIRST-BP-VALUES (ROW)
       ; sgithens TODO (CHECK-ROW-ARG ROW)
       (VALUES ROW 0))

(DEFUN ROW-LAST-BP-VALUES (ROW)
       ; sgithens TODO (CHECK-ROW-ARG ROW)
       (VALUES ROW (LENGTH-IN-CHAS ROW)))

(DEFUN BOX-FIRST-BP-VALUES (BOX)
       ;; sgithens TODO (CHECK-BOX-ARG BOX)
       (VALUES (ROW-AT-ROW-NO BOX 0) 0))

(defun screen-box-first-visible-bp-values (screen-box)
  ; sgithens TODO (check-screen-box-arg screen-box)
  (values (or (scroll-to-actual-row screen-box)
              (row-at-row-no (screen-obj-actual-obj screen-box) 0))
          0
          screen-box))

;; this should probably be smarter about checking the horizontal scrolling
;; to determine the best cha-no
(defun screen-box-last-visible-bp-values (screen-box)
  ; sgithens TODO (check-screen-box-arg screen-box)
  (let ((row (screen-obj-actual-obj (last-screen-row screen-box))))
    (values row
            0 ; (1- (length-in-chas row))
            screen-box)))

(DEFUN BOX-LAST-BP-VALUES (BOX)
       ;; sgithens TODO (CHECK-BOX-ARG BOX)
       (LET* ((BOX-LENGTH-IN-ROWS (LENGTH-IN-ROWS BOX))
              (LAST-ROW (ROW-AT-ROW-NO BOX (- BOX-LENGTH-IN-ROWS 1)))
              (LAST-ROW-LENGTH-IN-CHAS (LENGTH-IN-CHAS LAST-ROW)))
             (VALUES LAST-ROW LAST-ROW-LENGTH-IN-CHAS)))

(DEFUN BOX-SELF-BP-VALUES (BOX)
       ;; sgithens TODO (CHECK-BOX-ARG BOX)
       (LET ((SUPERIOR-ROW (SUPERIOR-ROW BOX)))
            (VALUES SUPERIOR-ROW (CHA-CHA-NO SUPERIOR-ROW BOX))))

(DEFUN BP-VALUES (BP)
       (CHECK-BP-ARG BP)
       (VALUES (BP-ROW BP) (BP-CHA-NO BP) (BP-SCREEN-BOX BP)))

;;; glue to include new screen boxes

(defmacro all-bp-values (bp-form screen-box)
  `(multiple-value-bind (new-row new-cha-no)
                        ,bp-form
                        (values new-row new-cha-no ,screen-box)))


;;;;CURSOR-TRACKER

;;Given the fact that there is a variable *POINT* , we can define
;;these simple functions.

(DEFUN POINT-BOX ()
       (BP-BOX *POINT*))

(DEFUN POINT-ROW ()
       (BP-ROW *POINT*))

(DEFUN POINT-CHA-NO ()
       (BP-CHA-NO *POINT*))

(DEFUN POINT-SCREEN-BOX ()
       (BP-SCREEN-BOX *POINT*))

(DEFUN SET-POINT-SCREEN-BOX (NEW-SCREEN-BOX)
       (SET-BP-SCREEN-BOX *POINT* NEW-SCREEN-BOX))

(DEFUN POINT-CHA-AFTER-POINT ()
       (CHA-AT-CHA-NO (POINT-ROW) (POINT-CHA-NO)))

;(DEFF POINT-CHA 'POINT-CHA-AFTER-POINT)

(defun point-screen-row ()
  (cdr (assoc (point-screen-box) (slot-value (point-row) 'screen-objs))))

(DEFUN SETUP-EDITOR (&OPTIONAL initial-box)
       (SETQ *INITIAL-BOX* (or initial-box
                               (let ((new (make-instance 'data-box))
                                     (cr (make-row '())))
                                 (add-closet-row new cr)
                                 (let ((command-string "System-Preferences"))
                                   (dotimes (i (length command-string))
                                     (append-cha cr (aref command-string i))))
                                 new)))
       ;; if changing the top level box, hide the cursor
       (unless (and initial-box (not (null (name-row initial-box))))
         ;; allow named box at top level
         (SET-NAME *INITIAL-BOX* "WORLD"))
       (SETQ *POINT* (MAKE-BP ':MOVING))
       (SET-OUTERMOST-BOX *INITIAL-BOX*)	  ;this calls redisplay !
       (SETF (SCREEN-ROW (CAR (SCREEN-OBJS *INITIAL-BOX*))) *BOXER-PANE*)
       (SETF (SUPERIOR-SCREEN-BOX (CAR (SCREEN-OBJS *INITIAL-BOX*))) *BOXER-PANE*)
       ;; some inits...
       (initialize-horizontal-border-thicknesses)
       (MULTIPLE-VALUE-BIND (ROW CHA-NO)
                            (BOX-FIRST-BP-VALUES *INITIAL-BOX*)
                            (MOVE-POINT-1 ROW CHA-NO (CAR (SCREEN-OBJS *INITIAL-BOX*)))))



;;;; The Boxer Status Line
;;;  We are currently using ONE line of the *NAME-PANE*.  In the future, we
;;;  might want to expand this to several lines and make it like an EMACS
;;;  typein window (then again, maybe not)

(defvar *boxer-status-string-alist* nil
  "An alist of unique ids or symbols and strings")

(defvar *draw-status-line* t
  "A boolean indicating whether we should draw the status line.

  The initial usage of this is displabling the status line for
  unit tests, during which the entire pane/editor is not brought
  up.")

(defun status-line-display (who what)
  (setq *boxer-status-string-alist*
        (delete who *boxer-status-string-alist* :key #'car))
  (push (cons who what) *boxer-status-string-alist*)
  (redraw-status-line))

(defun status-line-undisplay (who)
  (when (assoc who *boxer-status-string-alist*)
    (setq *boxer-status-string-alist*
          (delete who *boxer-status-string-alist* :key #'car))
    (redraw-status-line)))

(defun status-line-clear ()
  (setq *boxer-status-string-alist* nil)
  (redraw-status-line))

(defun get-boxer-status-string ()
  (flet ((get-boxer-version-string ()
                                   (or *boxer-version-info*
                                       (system-version))))
        (let ((other-strings (mapcar #'cdr *boxer-status-string-alist*)))
          (cond ((and (null *editor-numeric-argument*) (null other-strings))
                 (get-boxer-version-string))
            ((null other-strings)
             (format nil "Arg: ~D" *editor-numeric-argument*))
            ((null *editor-numeric-argument*)
             (format nil "~{~A  ~}" other-strings))
            (t
             ;; numeric arg & other-strings
             (format nil "Arg: ~D ~{ | ~A ~}"
                     *editor-numeric-argument* other-strings))))))

(defun redraw-status-line (&rest ignore)
  (declare (ignore ignore))
  (when *draw-status-line*
    (window-system-dependent-redraw-status-line
      (get-boxer-status-string))))

;;; use 'boxer-editor-error for status-line ID so it will get
;; flushed on subsequent keyboard input
(defun status-line-y-or-n-p (prompt-string)
  (status-line-display 'boxer-editor-error prompt-string)
  (let ((char (get-character-input *boxer-pane*)))
    (status-line-display 'boxer-editor-error
                         (format nil "~A ~C" prompt-string char))
    (prog1
     (cond ((or (char= char #\y) (char= char #\Y))
            T)
       (t nil))
     (status-line-undisplay 'y-or-n-p))))

(defvar *input-throw-tag* nil)

(defvar *no-echo-char* #\*)
(defvar *no-echo-string* (make-array 10 :element-type
                                     'character
                                     :initial-element *no-echo-char*
                                     :fill-pointer 0 :adjustable t))

(defun get-string-from-status-line (prompt-string &optional (echo-chars? t))
  ;; remove all other messages that might already be on the status line
  (setq *boxer-status-string-alist* nil)
  (status-line-display 'boxer-editor-error prompt-string)
  (let ((return-string (make-array 10 :element-type
                                   'character
                                   :adjustable t :fill-pointer 0))
        ;; this so we can STOP from a menu
        (*input-throw-tag* :get-string)
        (cancelled? T))
    (unless echo-chars? (setf (fill-pointer *no-echo-string*) 0))
    (catch *input-throw-tag*
      (loop (multiple-value-bind (char bits)
                                 (get-character-input *boxer-pane*)
                                 (cond ((editor-abort-char? char bits)
                                        (status-line-undisplay 'boxer-editor-error)
                                        (return-from get-string-from-status-line (values "" T)))
                                   (t
                                    (case char
                                      ((#\return #\linefeed)
                                       (setq cancelled? nil) (return))
                                      ((#\delete #\backspace)
                                       (let ((fp (fill-pointer return-string)))
                                         (unless (zerop& fp)
                                           (setf (fill-pointer return-string) (1-& fp))
                                           (unless echo-chars?
                                             (setf (fill-pointer *no-echo-string*) (1-& fp))))))
                                      (otherwise
                                       (vector-push-extend char return-string)
                                       (unless echo-chars?
                                         (vector-push-extend *no-echo-char* *no-echo-string*))))))
                                 (status-line-display
                                  'boxer-editor-error
                                  (format nil "~A ~A" prompt-string
                                          (if echo-chars? return-string *no-echo-string*))))))
    (status-line-undisplay 'boxer-editor-error)
    (if cancelled? (values "" T) return-string)))

;;; Name Tab utilities

(defmethod insert-cha-at-cha-no ((row name-row) cha cha-no)
  ;  "Gives the characters in the naming area a different font. "
  (if (box? cha)
    (boxer-editor-error "Cannot insert boxes in name rows. An attempt was made to insert the box, ~S, into the row ~S"
                        cha row)
    (chas-array-insert-cha (chas-array row)
                           cha-no
                           (make-char cha 0)))
  (modified row))

(defmethod insert-row-chas-at-cha-no ((name-row name-row) row cha-no
                                                          &optional
                                                          (from-start-cha-no 0)
                                                          (from-stop-cha-no (length-in-chas row)))
  (declare (ignore from-start-cha-no from-stop-cha-no))
  (let ((row-chas-array (chas-array row))
        (new-boxes (boxes-in-row row)))
    (if (not-null new-boxes)
      (error "An attempt was made to insert the boxes, ~S, into the row ~S"
             new-boxes name-row)
      (chas-array-move-chas row-chas-array 0
                            (chas-array name-row) cha-no
                            (chas-array-active-length row-chas-array)
                            name-row)))
  (modified name-row))

(defmethod update-bindings ((self name-row) &optional (force-rename? nil))
  (let* ((new-name (get-box-name self))
         (magic-name-insert-handler (when (symbolp new-name) (get new-name 'magic-name-insert)))
         (cached-name (slot-value self 'cached-name))
         (magic-name-delete-handler (when (symbolp cached-name)
                                      (get cached-name 'magic-name-delete)))
         (superior-box (slot-value self 'superior-box))
         (environment (and superior-box (superior-box superior-box))))
    (cond ((and (or force-rename?
                    (not (eq new-name cached-name)))
                (not (null new-name)))
           ;; if the name has changed, then remove the old
           ;; name from the environment
           (unless (null environment)
             (unless (or (null cached-name)
                         (not (eq superior-box
                                  (boxer-eval:lookup-static-variable-in-box-only
                                   environment cached-name))))
               (boxer-eval:remove-static-variable environment cached-name)
               ;; and then run any magic name deletion code
               (unless (null magic-name-delete-handler)
                 (funcall magic-name-delete-handler superior-box environment)))
             (boxer-eval:add-static-variable-pair environment new-name superior-box)
             ;; and run any magic name insert code
             (unless (null magic-name-insert-handler)
               (funcall magic-name-insert-handler superior-box environment)))
           ;;  set up the new name
           (setf (slot-value self 'cached-name) new-name))
      ;; new-name is NIL so we just need to remove the
      ;; name from the environment
      ((not (eq new-name cached-name))
       (unless (or (null environment)
                   (null cached-name)
                   (not (eq superior-box
                            (boxer-eval:lookup-static-variable-in-box-only
                             environment cached-name))))
         (boxer-eval:remove-static-variable environment cached-name)
         ;; and then run any magic name deletion code
         (unless (null magic-name-delete-handler)
           (funcall magic-name-delete-handler superior-box environment)))
       (setf (slot-value self 'cached-name) new-name)))))

(defmethod set-name ((self box) new-value)
  (setf (slot-value self 'name) new-value)
  (when (name-row? new-value)
    (set-superior-box new-value self)))

(DEFUN GET-BOX-NAME-FOR-PRINTING (NAME)
       (COND ((STRINGP NAME) NAME)
             ((NULL NAME) "Un-Named")
             ((NAME-ROW? NAME)(TEXT-STRING NAME))
             (T "???")))

(defmethod name ((self box))
  (get-box-name-for-printing (slot-value self 'name)))

(DEFUN NAME-STRING (BOX)
       (GET-BOX-NAME-FOR-PRINTING (SLOT-VALUE BOX 'NAME)))

(defun name-string-or-null (box)
  (unless (null box)
    (let ((nr (slot-value box 'name)))
      (unless (null nr) (get-box-name-for-printing nr)))))

;; call this on an old box when inserting a new box with the same name.
;; the recursive case is handled automagically.
(defun avoid-box-name-conflict-by-versioning (box)
  (let* ((name-row (slot-value box 'name))
         (name (and name-row (text-string name-row)))
         (sharp-pos (position #\# name))
         (possible-number-string (and sharp-pos (subseq name (1+& sharp-pos))))
         (version-number
          (if (and possible-number-string
                   (every #'digit-char-p possible-number-string))
            (read-from-string possible-number-string)
            0))
         (new-name (symbol-format nil "~A#~D"  (if (zerop& version-number)
                                                 name
                                                 (subseq name 0 sharp-pos))
                                  (1+& version-number)))
         (new-row (make-name-row (list new-name))))
    (boxer-editor-warning "Changing box name ~A to ~A" name new-name)
    (set-name box new-row)
    (update-bindings new-row)
    (modified box)))

;; 2/9/99 ehl
;; under some anomalous conditions, sprite lite objects can be passed here
;; so we need to check for boxness

(defun avoid-box-name-conflict-by-removing-name (new-box)
  (when (box? new-box)
    (let* ((name-row (slot-value new-box 'name))
           (name (get-box-name name-row))
           (sup (superior-box new-box)))
      (unless (null name-row)
        (setf (slot-value new-box 'name) nil)
        (unless (or (null sup)
                    (not (eq (boxer-eval::lookup-static-variable-in-box-only sup name)
                             new-box)))
          (boxer-eval::remove-static-variable-pair sup name new-box))
        (boxer-editor-warning "There already is a box named ~A"
                              (text-string name-row))
        (modified new-box)))))


;;;; Closets

;;; closets are implemented as a (sometimes) invisible row in the
;;; box with 1 or more transparent boxes on it.  The invisibility is
;;; controlled by setting the scroll-to-actual-row of the corresponding screen
;;; boxes.  The closets slot of the box also points to this row so we
;;; have a fast check for closets as well as a way to recover closets info
;;; when we munge rows.

;;; Note that CHANGE is supposed to preserve closets info while munging the
;;; datastructure part of the box.  [The initial motivation for this was to
;;; preserve MODIFIED-TRIGGERs across calls to CHANGE]

;;; Also, check out the different methods to get a list of rows out of
;;; a box in infsup.lisp

(defmethod all-rows ((box box))
  (let ((rows (rows box))
        (closet-row (slot-value box 'closets)))
    (cond ((null closet-row) rows)
      ((eq closet-row (car rows)) rows)
      (t (nconc (list closet-row) rows)))))

(defmethod data-rows ((box box))
  (let ((rows (rows box))
        (closet-row (slot-value box 'closets)))
    (cond ((null closet-row) rows)
      ((fast-memq closet-row rows)
       (remove closet-row rows :test #'eq))
      (t rows))))

;;; returns an extra value of T when the row is newly consed

(defmethod add-closet-row ((box box) new-row)
  (let ((current-value (slot-value box 'closets)))
    (cond ((null current-value)
           (set-superior-box new-row box)
           (setf (slot-value box 'closets) new-row)
           (do-row-chas ((cha new-row))
             (when (box? cha) (insert-self-action cha box))))
      (t (error
          "Trying to add a closet row, ~S, when there already is one ~S"
          new-row current-value)))))

(defmethod closet-row ((box box) &optional (new-innards? t))
  (let ((closet-row (slot-value box 'closets)))
    (cond ((null closet-row)
           (let* ((exporting-box (make-box '(())))
                  (cr (make-row (if new-innards?
                                  (list "__________"
                                        exporting-box
                                        "__________")
                                  NIL))))
             (set-superior-box cr box)
             (boxer-eval::set-box-transparency exporting-box t)
             (set-name exporting-box (make-name-row '(bu::drawer)))
             (setf (slot-value box 'closets) cr)
             (do-row-chas ((cha cr))
               (when (box? cha) (insert-self-action cha box)))
             (values cr t)))
      (t closet-row))))

(defmethod closet-row? ((row row) box)
  (let ((bcr (when (box? box) (slot-value box 'closets))))
    (eq row bcr)))

;; we ought to provide some methods for removing the closet if all the entries
;; on it have been removed.  Still needs to thought....

(defun box-location-string (box &optional (return-string "") max-string-length)
  (if (not (box? box))
    return-string
    (let* ((bn (name-row box)) (box-string (if (null bn) "[]" (text-string bn))))
      (if (and max-string-length (>= (length return-string) max-string-length))
        (concatenate 'string "..." return-string)
        (box-location-string (superior-box box)
                             (if (string= return-string "")
                               box-string
                               (concatenate 'string box-string "." return-string))
                             max-string-length)))))
