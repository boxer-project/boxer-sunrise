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
;;;;                                           +-Data--+
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;  This file contains the boxer eval queue and command loop that can be used from the CAPI Boxwin
;;;;  implementation, and hopefully other environments in the future.

(in-package :boxer-window)

(defvar *use-mouse2021* nil
  "Should we use the new 2021 Mouse clicks? This updates the mouse handling behavior in boxer to use
   true clicks with release, and adds events for mouse-down, mouse-up. This behavior may require some
   changes to legacy microworlds that have used mouse-click rather than mouse-down for dragging and
   other behaviors.")

;; 2021-06-28 sgithens I'm not sure if this is really used anymore.. there is a reference to it in the
;; currently broken dribbler
(defvar *boxer-command-loop-event-handlers-queue* nil
  "A list of functions to funcall at the bottom of boxer-command-loop.")


(defvar *double-click-pause-time* 0.4 "Time to wait for a possible second click")

(defvar *double-click-wander* 5
  "Number of pixels the mouse is allowed to shift between clicks")

(defvar *boxer-eval-queue* nil)

(defun queue-event (event)
 ; (when (characterp event) (setq *dribble* (nconc (list event) *dribble*)))
  (setq *boxer-eval-queue* (nconc *boxer-eval-queue* (list event))))

(defun flush-input () (setq *boxer-eval-queue* nil))

(defvar *noisy-abort-key-chars* nil)

(defvar *interrupt-flag* nil)

(defun boxer-interrupt () (setq *interrupt-flag* t *boxer-eval-queue* nil))

;; this may need to force a process switch to give the input handlers a chance
;; to run
(defun keyboard-interrupt? (window)
  (declare (ignore window))
  (if *interrupt-flag*
       (progn (setq *interrupt-flag* nil) t)
       (values nil (valid-input?))))

(defvar just-redisplayed? nil)

;;; Mouse handling, mostly copied from clx
(defstruct (mouse-event (:conc-name mouse-event-)
      (:predicate mouse-event?))
  ;; these are required slots that handle-boxer-input uses
  (type :mouse-click) ;; other option is :mouse-hold
  (window *boxer-pane*)
  (x-pos 0)
  (y-pos 0)
  ;; The current values for click are:
  ;;  0 - Primary Button, usually left click
  ;;  1 - Third Button
  ;;  2 - Secondary Button, usually right click
  (click 0)
  ;; The current values for bits are:
  ;;  0 - No modifier keys
  ;;  1 - Control Key
  ;;  2 - Option Key
  ;;  3 - Control + Option Key
  ;; 2021-02-10 Double check these on windows. Currently on Mac, Shift-click and
  ;; Command click don't register anything.
  (bits 0)  ;; which shift bits are down
  ;; these are (+++ not) used in click processing
  (last-time-stamp 0)
  (number-of-clicks 1))

(defun user-event-in-queue? ()
  (dolist (ev *boxer-eval-queue*)
    (when (or (mouse-event? ev) (key-event? ev)) (return T))))

;; a hook for other stuff, on the mac, this ensures heap size
(defun boxer-idle-function ()
  #+mcl (ensure-macheap)
  )

(defmacro boxer-system-error-restart (&body body)
  (let ((warned-about-error-already-gensym (gensym)))
    `(let ((,warned-about-error-already-gensym nil))
       (restart-bind
  ((BOXER-CONTINUE
    #'(lambda () (throw 'system-error-restart-loop nil))
    :report-function
    #'(lambda (stream)
        (unless (or ,warned-about-error-already-gensym
        boxer::*boxer-system-hacker*
        boxer::*inside-lisp-breakpoint-p*)
    (beep) (beep) (beep)
    ;; this mechanism is a crock.
    (setq ,warned-about-error-already-gensym t))
        (format stream "--> Return to Boxer <--")))
   (BOXER-TOP-LEVEL
    #'(lambda () (boxer::com-goto-top-level)
             (throw 'system-error-restart-loop nil))
    :report-function
    #'(lambda (stream)
        (format stream "--> GOTO Top Level then return to Boxer <--"))))
  (handler-bind
   ((error
     #'(lambda (c)
         (cond ((or ,warned-about-error-already-gensym
        (not *automagic-lisp-error-handling*))
          (invoke-debugger c))
         (t
          (dotimes (i 3) (beep))
          ;(format t "~%Lisp Error:~A" c)
                      (when *report-crash* (write-crash-report))
          (boxer::boxer-editor-error "Lisp Error:~A" c)
          (invoke-restart 'BOXER-CONTINUE))))))
    (catch 'system-error-restart-loop
      . ,body)
    (setq ,warned-about-error-already-gensym nil))))))

(defmacro boxer-system-error-restart-loop (&body body)
  (let ((warned-about-error-already-gensym (gensym)))
    `(let ((,warned-about-error-already-gensym nil))
       (restart-bind
   ((BOXER-CONTINUE
     #'(lambda () (throw 'system-error-restart-loop nil))
     :report-function
     #'(lambda (stream)
         (unless (or ,warned-about-error-already-gensym
         boxer::*boxer-system-hacker*
         boxer::*inside-lisp-breakpoint-p*)
     (beep) (beep) (beep)
     ;; this mechanism is a crock.
     (setq ,warned-about-error-already-gensym t))
         (format stream "--> Return to Boxer <--")))
    (BOXER-TOP-LEVEL
     #'(lambda () (boxer::com-goto-top-level)
              (throw 'system-error-restart-loop nil))
     :report-function
     #'(lambda (stream)
         (format stream "--> GOTO Top Level then return to Boxer <--")))
          (ABORT
           #'(lambda () (boxer::com-abort))
           :report-function
           #'(lambda (stream) (format stream "Aborted"))))
   (handler-bind
     ((error
       #'(lambda (c)
           (cond ((or ,warned-about-error-already-gensym
          (not *automagic-lisp-error-handling*))
            (invoke-debugger c))
           (t
            (dotimes (i 3) (beep))
            ;(format t "~%Lisp Error:~A" c)
                        (when *report-crash* (write-crash-report))
            (boxer::boxer-editor-error "Lisp Error:~A" c)
            (invoke-restart 'BOXER-CONTINUE))))))
     (loop (catch 'system-error-restart-loop
                   (progn ,@body)
                   (flush-input)
             (setq ,warned-about-error-already-gensym nil))))))))

(defun peek-next-key-or-mouse-event ()
  (loop (let ((ev (car *boxer-eval-queue*)))
          (cond ((null ev) (return nil))
                ((mouse-event? ev) (return ev))
                ((key-event?   ev) (return ev))
                #+lispworks ((system:gesture-spec-p ev) (return ev))
                (t (pop *boxer-eval-queue*))))))

(defun modern-clicking-2021 (click)
  "New 2021 Click handling where clicks aren't delayed, clicks happen when the
  mouse comes up, and we support mouse-down, and mouse-up"
  (handle-boxer-input click)
)

;; pause and wait for another possible click
(defun maybe-unify-mouse-click (click)
  (let ((button (mouse-event-click click))
        (bits   (mouse-event-bits  click))
        (x-pos  (mouse-event-x-pos click))
        (y-pos  (mouse-event-y-pos click))
        ;(num-clicks (mouse-event-number-of-clicks click))
        ;(time  (mouse-event-last-time-stamp click))
        )
    #-win32 (declare (ignore button))
    #+lispworks (mp::process-wait-with-timeout "Double Click Wait" *double-click-pause-time*
                                   #'(lambda () (user-event-in-queue?)))
    (let ((new-input (peek-next-key-or-mouse-event)))
      (cond ((mouse-event? new-input)
             (cond ((and #+win32 (= (mod button 3) (mod (mouse-event-click new-input) 3))
                         ;; only need to button match for PC (3) button mouse
                         (= bits (mouse-event-bits new-input))
                         (boxer::<& (boxer::abs& (boxer::-& x-pos (mouse-event-x-pos
                                                                   new-input)))
                                    *double-click-wander*)
                         (boxer::<& (boxer::abs& (boxer::-& y-pos (mouse-event-y-pos
                                                                   new-input)))
                                    *double-click-wander*))
                    ;; looks like a double click
                    (cond ((> (mouse-event-number-of-clicks new-input) 1)
                           (handle-boxer-input (pop *boxer-eval-queue*)))
                          (t ;; looks like the event system recorded it as
                             ;; a pair of single clicks, throw out the second
                             ;; one and bash fields in the 1st one
                             (pop *boxer-eval-queue*)
                             (setf (mouse-event-click click)
                                   (+ (mouse-event-click click) 3)
                                   ;; if we allow wandering, should we use the pos
                                   ;; of the 1st or 2nd click
                                   (mouse-event-last-time-stamp click)
                                   (mouse-event-last-time-stamp new-input))
                             (incf (mouse-event-number-of-clicks click))
                             (handle-boxer-input click))))
                   (t (handle-boxer-input click))))
            (t (handle-boxer-input click))))))

;; on the mac, this checks to OS event queue to look for pending unhandled events
;; which might not
(defun no-more-input? () (not (peek-next-key-or-mouse-event)))
(defun valid-input? () (not (no-more-input?)))

(defun boxer-command-loop-internal ()
;  (setf (ccl::window-process *boxer-frame*) boxer::*boxer-process*)
  (flush-input)
  (loop
    (catch 'boxer::boxer-editor-top-level
      (let ((input (pop *boxer-eval-queue*)))
        (cond ((null input)
               (unless just-redisplayed?
                 (boxer::repaint-with-cursor-relocation) (setq just-redisplayed? t))
               #+lispworks (mp::process-allow-scheduling)
               (when (no-more-input?)
                 (boxer-idle-function)
                 ;; be a good multi-processing citizen
                 ;; NOTE: when the idle function is fixed to actually document
                 ;; the mouse, we will need to change this to
                 ;; mp::process-allow-scheduling so it can run continously...
                 #+lispworks (mp::process-wait "Boxer Input"
                                   #'(lambda ()
                                       (not (null (car *boxer-eval-queue*)))))))
              #+lispworks ((system:gesture-spec-p input)
               ;; We are adding this gesture condition in addition to the key-event? because at some point
               ;; during a lispworks major version change, the ability to encode the modifier keys as part of
               ;; the reader char seems to have gone away.  By adding an option to push an entire gesture-spec
               ;; to the *boxer-eval-queue* we can just manually pick out the char-code and input bits.
               (let* ((data (sys::gesture-spec-data input))
                      (charcode (input-gesture->char-code input))
                      (charbits (convert-gesture-spec-modifier input)))
                     (handle-boxer-input charcode charbits)
                     (setq just-redisplayed? nil)))
              ((key-event? input)
               (handle-boxer-input (input-code input) (input-bits input))
               (setq just-redisplayed? nil))
              ((mouse-event? input)
               ;; be sure to call redisplay BEFORE the
               ;; processing of any mouse actions to
               ;; insure that we have an up to date view
               ;; of the editor
               ;;
               ;; also check for double click by pausing and looking for a
               ;; double click event
               (when (null just-redisplayed?) (boxer::repaint))
               (if *use-mouse2021*
                 (modern-clicking-2021 input)
                 (maybe-unify-mouse-click input))
               (setq just-redisplayed? nil))
              ((and (symbolp input) (not (null (symbol-function input))))
               (when (null just-redisplayed?) (boxer::repaint-with-cursor-relocation))
               (funcall input)
               (setq just-redisplayed? nil))
              ((and (consp input)
                    (or (functionp (car input))
                        (and (symbolp (car input))
                             (not (null (symbol-function (car input)))))))
               (apply (car input) (cdr input)))
              ((not (null boxer::*boxer-system-hacker*))
               (error "Unknown object, ~A, in event queue" input)))))))

(defmacro boxer-editor-bindings (recursive-p &body body)
  `(progv '(*region-being-defined* boxer::*editor-numeric-argument*
      boxer::*propagate-modified-messages?*)
          `(nil nil ,',recursive-p)
     (unwind-protect
   (progn . ,body)
;       (when (not (null *region-being-defined*))
;       (flush-region *region-being-defined*))
)))

(defun boxer-command-loop ()
  (boxer-system-error-restart-loop
    (boxer-editor-bindings nil
      (boxer-command-loop-internal))))
