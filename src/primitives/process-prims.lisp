;;;;-*-LISP-*-
;;;;
;;;;   $Header: process-prims.lisp,v 1.0 90/01/24 22:15:51 boxer Exp $
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
;;;;                                        +-------+
;;;;               This file is part of the | Boxer | System
;;;;                                        +-Data--+
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   4/21/03 merged current LW and MCL files, no diffs, updated copyright
;;;;

(in-package :boxer-eval)

;;; depends on processes.lisp

;; this function causes an eval continuation to be consed and returns to top
;; level.
;;hmm, we want to go to top level here.
;; we can just go back to eval-loop (like continue)
;; but reset "iline" to nil
;; we need to update the eval state mechanism
;; to be able to set the variables in the state
;; individually.
;; these things are really processes, huh?
;; we should flush the debug variables and instead
;; make a structure with named components.

;; what about moving the cursor to the new lexical root?

;;; 		DON'T BELIEVE THESE COMMENTS.
;;; use the *access-evaluator-state-sfun-continuation* to get the
;;; current contents of all eval variables stored in the vector
;;; *current-process*.  Return a function to be funcalled
;;; once the vector has been filled.
;;; That function should return T if it changes the contents of the
;;; vector or the value of the variable.
;;; If it doesn't change it, it should return nil,
;;; set *sfun-continuation* to  *std-sfun-continuation*,
;;; and set *returned-value* to whatever the primitive should return.

(defboxer-primitive bu::process-pause ()
  (setq *sfun-continuation* '*access-evaluator-state-sfun-continuation*)
  #'(lambda ()
            ;; Make the current process ready to continue execution when we resume it
            (setf (process-variable *current-process* *sfun-continuation*)
                  '*std-sfun-continuation*)
            (setf (process-variable *current-process* *returned-value*) *novalue*)
            ;; Queue the current process to be run later.
            (queue-process *current-process*)
            (let ((old-process *current-process*))
              ;; Make a new process.
              (init-process-state-vector)
              ;; Mark the old process as PAUSED, waiting for new process to resume it
              (setf (process-variable old-process *process-state*)
                    (cons :PAUSE *current-process*)))
            (process-display *boxer-processes*)
            ;; Set up the new process so it will exit the evaluator immediately.
            ;; It's already in :TOPLEVEL state.
            (setf (process-variable *current-process* *sfun-continuation*)
                  '*eval-loop-sfun-continuation*)
            t))

(defvar *multi-tasking-enabled* nil)

(defun enable-multi-tasking ()
  (setq *internal-polling-function* #'poll-internal-and-schedule
        *initial-poll-count* 10
        *poll-count*         10
        bw::*boxer-command-loop-internal-time* 0
        *multi-tasking-enabled* T))

(defboxer-primitive bu::please ((list-rest what-to-do))
  (let ((process (box-process *lexical-variables-root*)))
    (when (null *multi-tasking-enabled*) (enable-multi-tasking))
    (cond ((null process)
           (signal-error :PROCESS-ERROR
                         "Can't find or make a process associated with: "
                         *lexical-variables-root*))
      ((eq (process-variable process *process-state*) :RUN)
       (signal-error :PROCESS-ERROR
                     "The Box, " *lexical-variables-root*
                     ", is already active"))
      (t
       ;; shouldn't these already be true ?
       (setf (process-variable process *lexical-variables-root*)
             *lexical-variables-root*
             (process-variable process *current-process*)
             process)
       ;; give the new process something to do and make sure it knows it
       (setf (process-variable process *executing-pointer*) what-to-do
             ;; what about *executing-line* ?
             (process-variable process *returned-value*) *novalue*
             (process-variable process *process-state*) :RUN)
       ;; Queue the process to be run later.
       (cond ((null *boxer-processes*)
              (setq *boxer-processes* (list process)))
         ((fast-memq process *boxer-processes*))
         (t
          (nconc (last *boxer-processes*) (list process))))
       *novalue*))))

(defun box-process (box)
  (cond ((box::box? box)
         (or (box::getprop box :process)
             (let ((new (%make-process-state-vector)))
               (box::putprop box new :process)
               new)))
    (t (%make-process-state-vector))))

(defboxer-primitive bu::reset-process ((bu::port-to box))
  (let* ((target (boxer::box-or-port-target box))
         (process (box::getprop target :process)))
    (unless (null process)
      ;; what about (unwind-to-top-level) and (reset-stacks)
      (process-finish process)))
  *novalue*)


(defvar *zoom-to-edit-box* nil)

;;; This function is just like bu::process-pause except for the
;;; call to enter-box.  Note that the lambda has to be lexically in this
;;; function so it will close over BOX.
(defboxer-primitive bu::edit-box ((bu::port-to box))
  (setq box (boxer::get-port-target box))
  (cond ((not (boxer::box? box))
         (primitive-signal-error :EDITOR-ERROR
                                 "was not a displayable box" box))
    ((not (boxer::superior? box boxer::*initial-box*))
     ;; must be part of the hierarchy
     (primitive-signal-error :EDITOR-ERROR
                             "Box is not connected to the world" box))
    ((and (null (boxer::displayed-screen-objs box))
          (null *zoom-to-edit-box*))
     (primitive-signal-error :EDITOR-ERROR ; should tell how to zoom
                             "Box is not currently visible" box)))
  (setq *sfun-continuation* '*access-evaluator-state-sfun-continuation*)
  #'(lambda ()
            ;; We must process the editor mutation queue now, in preparation
            ;; for returning to top level.  If we wait and let the automatic
            ;; mechanism take care of it on return to top level, it will flush
            ;; valuable information  because it thinks we will no longer need it.
            (boxer::process-editor-mutation-queue-within-eval)
            ;; Make the current process ready to continue execution
            ;; when we resume it.
            (setf (process-variable *current-process* *sfun-continuation*)
                  '*std-sfun-continuation*)
            (setf (process-variable *current-process* *returned-value*) *novalue*)
            ;; Queue the current process to be run later.
            (queue-process *current-process*)
            (let ((old-process *current-process*))
              ;; enter the new box before creating the new process
              ;; so the init form for *lexical-variables-root* will
              ;; copy the value of (boxer:point-box) into the variable.
              (enter-io-box box)
              ;; Make a new process.
              (init-process-state-vector)
              ;; Mark the old process as PAUSED, waiting for new process to resume it
              (setf (process-variable old-process *process-state*)
                    (cons :PAUSE *current-process*)))
            (process-display *boxer-processes*)
            ;; Set up the new process so it will exit the evaluator immediately.
            ;; It's already in :TOPLEVEL state.  That will cause it to return
            ;; to toplevel sit in the edit loop.
            (setf (process-variable *current-process* *sfun-continuation*)
                  '*eval-loop-sfun-continuation*)
            t))

;;; bu::edit-box sets up bu::return-key (by calling enter-io-box) to call
;;; this function in the box you want to edit.  You could associate this
;;; function with a special tab in an editable box and make that code
(defboxer-primitive bu::process-finish ()
  (cond ((null *boxer-processes*)
         *novalue*)
    (t (setq *sfun-continuation*
             '*access-evaluator-state-sfun-continuation*)
       #'(lambda ()
                 ;; We must process the editor mutation queue now, in preparation
                 ;; for returning to top level.  If we wait and let the automatic
                 ;; mechanism take care of it on return to top level, it will
                 ;; flush valuable information  because it thinks we will no
                 ;; longer need it.
                 (boxer::process-editor-mutation-queue-within-eval)
                 ;; Do this for debugging purposes.  There ought not to be any
                 ;; more pointers to *current-process* after this.
                 (setf (process-variable *current-process* *process-state*)
                       :KILLED)
                 (let ((old-process *current-process*))
                   ;; the current contents of *current-process* will
                   ;; simply languish.
                   ;; clear *current-process* so it won't get put in the queue.
                   (setq *current-process* nil)
                   ;; If a process is in pause waiting for this process, run
                   ;; that one. Otherwise get the next process that's in :RUN.
                   ;; Otherwise, use the process in :TOPLEVEL.
                   (process-display (cdr *boxer-processes*))
                   (or (let ((rp (resume-process-waiting-for old-process)))
                         (unless (null rp)  (set-keyboard-process rp) rp))
                       (let ((tlp (next-toplevel-process)))
                         (unless (null tlp) (set-keyboard-process tlp) tlp))
                       (next-runnable-process)))
                 (boxer::restore-point-position
                  (process-variable *current-process*
                                    *process-doit-cursor-position*)
                  t)
                 t))))


(defboxer-primitive bu::process-allow-schedule ()
  (cond ((null *boxer-processes*) *novalue*)
    (t (setq *sfun-continuation*
             '*access-evaluator-state-sfun-continuation*)
       #'(lambda ()
                 (setf (process-variable *current-process* *sfun-continuation*)
                       '*std-sfun-continuation*)
                 (setf (process-variable *current-process* *returned-value*)
                       *novalue*)
                 (let ((next-process (next-runnable-process)))
                   ;; if there's no other runnable process, just return.
                   (cond ((null next-process)
                          (setq *returned-value* *novalue*)
                          (setq *sfun-continuation* '*std-sfun-continuation*)
                          NIL)
                     (t
                      ;; This cursor move here is questionable.
                      ;; but what are we supposed to do? The lexical root
                      ;; might be different.  (But we face this problem
                      ;; with TELL also).
                      (boxer::restore-point-position
                       (process-variable *current-process*
                                         *process-doit-cursor-position*)
                       t)
                      ;			(setf (process-variable *current-process*
                      ;						*sfun-continuation*)
                      ;			      '*std-sfun-continuation*)
                      (setf (process-variable *current-process* *sfun-continuation*)
                            '*eval-loop-sfun-continuation*)
                      (setf (process-variable *current-process*
                                              *returned-value*)
                            *novalue*)
                      t)))))))

;; assumes that the evaluator is already running
(defun resume-process-waiting-for (process)
  ;; Makes current process be the first process in :PAUSE for PROCESS and
  ;; returns process.  Returns nil if no process in waiting for PROCESS.
  (dotimes (i (length (the list *boxer-processes*)))
    (unless (null *current-process*)
      (nconc (last *boxer-processes*) (list *current-process*)))
    (setq *current-process* (pop *boxer-processes*))
    (let ((state (process-variable *current-process* *process-state*)))
      (when (and (consp state)
                 (eq (car state) :PAUSE)
                 (eq (cdr state) process))
        ;; unpause the process...
        (setf (process-variable *current-process* *process-state*) :toplevel)
        (return *current-process*)))))

(defun next-runnable-process ()
  ;; Makes current process be the first process in :RUN and
  ;; returns process.  Returns nil if no process in :RUN
  (dotimes (i (length (the list *boxer-processes*)))
    (unless (null *current-process*)
      (nconc (last *boxer-processes*) (list *current-process*)))
    (setq *current-process* (pop *boxer-processes*))
    (when (eq (process-variable *current-process* *process-state*)
              :RUN)
      (return *current-process*))))

;; like next-runnable-process EXCEPT that it doesn't
;; side effect *current-process*
(defun next-restartable-process (&optional (queue-current-process? t))
  (let ((torun nil))
    (dotimes (i (length (the list *boxer-processes*)))
      (setq torun (pop *boxer-processes*))
      (when (eq (process-variable torun *process-state*) :RUN)
        (when queue-current-process? (queue-process *current-process*))
        (return torun))
      (if (null *boxer-processes*)
        (setq *boxer-processes* (list torun))
        (nconc (last *boxer-processes*) (list torun))))))

(defun next-toplevel-process ()
  ;; return first process in :TOPLEVEL, or NIL if none.
  (dotimes (i (length (the list *boxer-processes*)))
    (unless (null *current-process*)
      (nconc (last *boxer-processes*) (list *current-process*)))
    (setq *current-process* (pop *boxer-processes*))
    (when (eq (process-variable *current-process* *process-state*)
              :TOPLEVEL)
      (return *current-process*))))

;; like next-toplevel-process EXCEPT that it doesn't
;; side effect *current-process*
(defun get-toplevel-process ()
  (let ((torun nil))
    (dotimes (i (length (the list *boxer-processes*)))
      (setq torun (pop *boxer-processes*))
      (when (eq (process-variable torun *process-state*) :TOPLEVEL)
        ;(queue-process *current-process*)
        (return torun))
      (if (null *boxer-processes*)
        (setq *boxer-processes* (list torun))
        (nconc (last *boxer-processes*) (list torun))))))

(defboxer-primitive bu::kill-all-processes ()
  (kill-boxer-processes)
  (process-display *boxer-processes*)
  *novalue*)


;;;
;; this stuff is pretty random down here.

(defun enter-io-box (box-or-port)
  (let ((target (boxer::box-or-port-target box-or-port)))
    ;; we would like to process the editor mutation queue
    ;; here, but it looks dangerous with those references to
    ;; *evaluation-in-progress?* and all.
    ; (boxer::process-editor-mutation-queue-within-eval)
    (unless (lookup-static-variable-in-box-only target 'bu::exit-trigger)
      (let ((et (boxer::make-box '((bu::com-exit-io-box)) 'boxer::doit-box)))
        ;; it would be nice not to cons this every time.
        (add-static-variable-pair target 'bu::exit-trigger et)
        (funcall (get 'bu::exit-trigger 'boxer::magic-name-insert) et target)))
    (if (and (null (boxer::screen-objs target))
             *zoom-to-edit-box*)
      (boxer::with-temporary-bp (target-bp (boxer::box-first-bp-values
                                            target))
                                (boxer::move-to-bp target-bp))
      (boxer::com-enter-box target (car (boxer::screen-objs target))))))


;; these next two are still all broken.
;;; even if this worked, they would have to share more
;; with doit-internal, particularly wrt cursor position.  and
;; process-finish shouldn't move the cursor, maybe.
;; the real problem is that keys don't invoke the evaluator
;; and doit-print-returned-value is outside the evaluator
;; and so our continuations don't encompass the two.
(boxer::defboxer-command boxer::com-exit-io-box ()
                         "Exit the I/O box entered with EDIT-BOX.
If the box was not entered with EDIT-BOX, just insert a return."
                         (cond ((null *boxer-processes*) (boxer::com-return))
                           (t (boxer::reset-editor-numeric-arg)
                              (let ((et (lookup-static-variable-in-box-only (boxer::point-box)
                                                                            'bu::exit-trigger)))
                                (remove-static-variable (boxer::point-box) 'bu::exit-trigger)
                                (funcall (get 'bu::exit-trigger 'boxer::magic-name-delete)
                                         et (boxer::point-box)))
                              (bu::process-finish))))

;; this isn't right either.  for one thing, it has to be in a doit box,
;; to work.  handle-boxer-key-doit-box needs to be hacked, at least
;; in this interpretation.
(defboxer-primitive bu::com-exit-io-box ()
  ;; this must be the last form in the primitive,
  ;; since it does sfun continuation/value stuff.
  (boxer::com-exit-io-box))


(defboxer-primitive bu::process-variable ((bu::port-to box) (list-rest name))
  (let ((process (box::getprop box :process)))
    (cond ((null name)
           (signal-error :process-variable "Need a name to look up"))
      ((not (symbolp (car name)))
       (signal-error :process-variable (car name) "should be a symbol"))
      ((null process)
       (boxer-symeval (car name)))
      (t
       (let ((*dynamic-variables-top*
              (process-variable process *dynamic-variables-top*))
             (*dynamic-variables-bottom*
              (process-variable process *dynamic-variables-bottom*))
             (*dynamic-variables-names-array*
              (process-variable process *dynamic-variables-names-array*))
             (*dynamic-variables-values-array*
              (process-variable process *dynamic-variables-values-array*)))
         (boxer-symeval (car name)))))))

;;; I/O prims

(defboxer-primitive bu::input? ()  (boxer-boolean (bw::valid-input?)))

(defun handle-input-epilogue-handler ()
  (boxer::process-editor-mutation-queue-within-eval)
  nil)

;;; very much like TELL
(defrecursive-funcall-primitive bu::handle-input ()
  :stack-frame-allocation (2 2 2 2)
  :state-variables (*lexical-variables-root* *dynamic-variables-bottom*)
  :before
  (let* ((input (bw::get-boxer-input))
         (mouse-p (not (or (system:gesture-spec-p input) (boxer::key-event? input)))))
    (multiple-value-bind (name mouse-bp)
                         (if (not mouse-p)
                           (progn (if (system:gesture-spec-p input)
                                      (let* ((data (sys::gesture-spec-data input))
                                          (charcode (bw::input-gesture->char-code input))
                                          (charbits (sys:gesture-spec-modifiers input)))
                                        (boxer::lookup-key-name (boxer::input-code charcode) charbits))
                                      (boxer::lookup-key-name (boxer::input-code input)
                                                              (boxer::input-bits input))))
                           (boxer::get-mouse-click-name input))
                         (let ((value (let ((*lexical-variables-root* (if mouse-bp
                                                                        (box::bp-box mouse-bp)
                                                                        (box::point-box))))
                                        (boxer-symeval name))))
                           (cond ((eq value *novalue*) *novalue*)
                             ((and mouse-p
                                   (possible-eval-object? value)
                                   (compiled-boxer-function? value))
                              ;; mouse editor primitives have to be handled specially
                              ;; see handle-boxer-mouse-click
                              (funcall (compiled-boxer-function-object value)
                                       (boxer::mouse-event-window input)
                                       (boxer::mouse-event-x-pos input)
                                       (boxer::mouse-event-y-pos input)
                                       mouse-bp
                                       (eq (boxer::mouse-event-type input)
                                           ':mouse-click)))
                             ((or (boxer-function? value)
                                  (and (port-box? value) (doit-box? (boxer::ports value)))
                                  (and (simple-vector-p value)
                                       ;; there must be a proper (eval) macro for this
                                       (fast-eval-port-box? value)
                                       (fast-eval-doit-box? (boxer::vp-target value)))
                                  (doit-box? value)
                                  (and (simple-vector-p value)
                                       ;; there must be a proper (eval) macro for this
                                       (fast-eval-doit-box? value)))
                              ;; looks like a function, if it is a mouse function, we
                              ;; have to move to the destination first
                              (when mouse-p
                                (boxer::com-mouse-move-point (boxer::mouse-event-window input)
                                                             (boxer::mouse-event-x-pos input)
                                                             (boxer::mouse-event-y-pos input)
                                                             mouse-bp
                                                             (eq (box::mouse-event-type input)
                                                                 ':mouse-click)))
                              (set-and-save-state-variables (boxer::point-box)
                                                            *dynamic-variables-top*)
                              (recursive-funcall-invoke
                               (make-interpreted-procedure-from-list (list (list name)))))
                             (t (copy-thing value))))))
  :after
  (progn (restore-state-variables) (handle-input-epilogue-handler) nil)
  :unwind-protect-form
  (progn (restore-state-variables) (handle-input-epilogue-handler) nil))

;;; what about the doit key?
(defboxer-primitive bu::handle-dynamic-input ()
  (let* ((input (bw::get-boxer-input))
         (mouse-p (boxer::key-event? input)))
    (multiple-value-bind (name mouse-bp)
                         (if (not mouse-p)
                           (boxer::lookup-key-name (boxer::input-code input)
                                                   (boxer::input-bits input))
                           (boxer::get-mouse-click-name input))
                         (let ((value (boxer-eval::boxer-symeval name)))
                           (cond ((eq value *novalue*) *novalue*)
                             ((or (boxer-function? value)
                                  (and (port-box? value) (doit-box? (boxer::ports value)))
                                  (and (simple-vector-p value)
                                       ;; there must be a proper (eval) macro for this
                                       (fast-eval-port-box? value)
                                       (fast-eval-doit-box? (boxer::vp-target value))))
                              (setq *sfun-continuation*
                                    '*ufuncall-sfun-result-sfun-continuation*
                                    *ufuncall-sfun-epilog-handler*
                                    #'handle-input-epilogue-handler)
                              (make-interpreted-procedure-from-list (list (list value))))
                             ((doit-box? value)
                              (setq *sfun-continuation*
                                    '*ufuncall-sfun-result-sfun-continuation*
                                    *ufuncall-sfun-epilog-handler*
                                    #'handle-input-epilogue-handler)
                              (cached-code-editor-box value))
                             ((and (simple-vector-p value)
                                   ;; there must be a proper (eval) macro for this
                                   (fast-eval-doit-box? value))
                              (setq *sfun-continuation*
                                    '*ufuncall-sfun-result-sfun-continuation*
                                    *ufuncall-sfun-epilog-handler*
                                    #'handle-input-epilogue-handler)
                              (cached-code-virtual-copy value))
                             (t (copy-thing value)))))))


;; should push and restore the *point*
(defrecursive-eval-primitive bu::handle-input-loop ((list-rest ignored))
  :stack-frame-allocation (2 2 2 2)
  :state-variables (*process-doit-cursor-position* *recursive-doit?*)
  :before
  (progn
   (cond ((not (null ignored))
          (signal-error
           :IMPROPER-ARGUMENT
           "Handle-Input-Loop should be" "the last item on a line"))
     (t
      (boxer::status-line-display 'boxer::boxer-editor-error
                                  (format
                                   nil "Procedure grabbing the keyboard"))
      (let ((exit-state (boxer::mini-boxer-command-loop)))
        (unless (null exit-state)
          (set-and-save-state-variables
           (boxer::fill-doit-cursor-position-vector
            (boxer::make-process-doit-cursor-position))
           nil)
          (setq *recursive-doit?* t)
          (push *current-process* *keyboard-owner*)
          (recursive-eval-invoke exit-state))))))
  :after
  (cond ((not (null *recursive-doit?*))
         (boxer::print-returned-value-when-possible *returned-value*)
         (setq *recursive-doit?* nil)
         ;; set things up so that the mini-command-loop will be re-entered
         (let ((exit-state (boxer::mini-boxer-command-loop)))
           (cond ((null exit-state)
                  ;; if we exit normally
                  (restore-state-variables)
                  (boxer::restore-point-position
                   *process-doit-cursor-position*)
                  (setq *returned-value* *novalue*)
                  nil)
             (t
              ;; if we exit as a result of pressing the doit key
              ;; then return the list to be eval'ed
              exit-state))))
    (t
     (restore-state-variables)
     (boxer::restore-point-position *process-doit-cursor-position*)
     (pop *keyboard-owner*)
     (setq *returned-value* *novalue*)
     nil))
  :unwind-protect-form
  (progn (restore-state-variables)
         (boxer::restore-point-position *process-doit-cursor-position*)
         (pop *keyboard-owner*)
         (setq *returned-value* *novalue*)))

(defvar *keyboard-owner* nil)

(defun keyboard-owning-process () *keyboard-owner*)



;;;
;;; display junk
;;;
(defun process-display (processes)
  (if (null processes)
    (boxer::status-line-undisplay '*boxer-processes*)
    (boxer::status-line-display
     '*boxer-processes*
     (format nil "~{[~*] ~}" processes))))

;;;
;;; editor junk
;;;

(defboxer-primitive bu::cursor-column-number ()
  (1+& (boxer::point-cha-no)))

(defboxer-primitive bu::cursor-row-number ()
  (1+& (boxer::row-row-no (boxer::point-box) (boxer::point-row))))

