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
;;;;                                           +-Data--+
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;  This file contains the boxer eval queue and command loop that can be used from the CAPI Boxwin
;;;;  implementation, and hopefully other environments in the future.

(in-package :boxer-window)


;; 2021-06-28 sgithens I'm not sure if this is really used anymore.. there is a reference to it in the
;; currently broken dribbler
(defvar *boxer-command-loop-event-handlers-queue* nil
  "A list of functions to funcall at the bottom of boxer-command-loop.")

(defvar *double-click-wander* 5
  "Number of pixels the mouse is allowed to shift between clicks")

(defvar *boxer-eval-queue* nil)

(defvar *boxer-init-queue* nil
  "A queue for initialization items you want to be run after Boxer startup. Regular items
  on the queue are flushed when the eval loop is started.")

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
  ;;  3 - Primary Double Click
  ;;  4 - Middle Double Click
  ;;  5 - Right Double Click
  ;;  6 - Primary Down
  ;;  7 - Middle Down
  ;;  8 - Right Down
  ;;  9 - Primary Up
  ;; 10 - Middle Up
  ;; 11 - Right Up
  (click 0)
  ;; The current values for bits are:
  ;;  0 - No modifier keys
  ;;  1 - Control Key
  ;;  2 - Option Key
  ;;  3 - Control + Option Key
  ;; 2021-02-10 Double check these on windows. Currently on Mac, Shift-click and
  ;; Command click don't register anything.
  ;; 2021-11-11 We are going to adopt the bits conventions from LW and propagate
  ;; them through. These will be the same for keyboard events
  ;; 0 - none                8 - command
  ;; 1 - shift               9 - shift-command
  ;; 2 - ctrl               10 - ctrl-command
  ;; 3 - ctrl-shift         11 - ctrl-shift-command
  ;; 4 - option             12 - option-command
  ;; 5 - shift-option       13 - shift-option-command
  ;; 6 - ctrl-option        14 - ctrl-option-command
  ;; 7 - ctrl-shift-option  15 - ctrl-shift-option-command
  ;;
  ;; Alt = Option, Ctrl = Control, Command = Meta = Windows
  ;;
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
  (when *clicked-startup-file*
      (queue-event *clicked-startup-file*)
      (setf *clicked-startup-file* nil)))

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
                           (and (not *automagic-lisp-error-handling*) *debug-errors*))
                       (invoke-debugger c))
                      ((and (not *debug-errors*) (not *automagic-lisp-error-handling*))
                       ;; TODO sgithens Cross platform version of this
                       #+lispworks (show-error-dialog (format nil "Lisp Error:~A ~%  backtrace: ~%~A"
                                                  c (with-output-to-string (str)
                                                      (dbg::output-backtrace :brief :stream str)
                                                      (format str "~%~%Verbose:~%")
                                                      (dbg::output-backtrace :verbose :stream str)))))
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
                ((gesture-spec-p ev) (return ev))
                (t (pop *boxer-eval-queue*))))))

;; on the mac, this checks to OS event queue to look for pending unhandled events
;; which might not
(defun no-more-input? () (not (peek-next-key-or-mouse-event)))
(defun valid-input? () (not (no-more-input?)))

(defun boxer-command-loop-internal ()
  (flush-input)
  (loop
    (when *clicked-startup-file*
      (queue-event *clicked-startup-file*)
      (setf *clicked-startup-file* nil))
    (catch 'boxer::boxer-editor-top-level
      (let ((input (pop *boxer-eval-queue*)))
        (cond ((null input)
               #+lispworks (mp::process-allow-scheduling)
               (when (no-more-input?)
                 (boxer-idle-function)
                 ;; be a good multi-processing citizen
                 ;; NOTE: when the idle function is fixed to actually document
                 ;; the mouse, we will need to change this to
                 ;; mp::process-allow-scheduling so it can run continously...
                 #+lispworks (mp::process-wait-with-timeout "Boxer Input" 0.01
                                   #'(lambda ()
                                       (not (null (car *boxer-eval-queue*)))))))
              ((gesture-spec-p input)
               ;; We are adding this gesture condition in addition to the key-event? because at some point
               ;; during a lispworks major version change, the ability to encode the modifier keys as part of
               ;; the reader char seems to have gone away.  By adding an option to push an entire gesture-spec
               ;; to the *boxer-eval-queue* we can just manually pick out the char-code and input bits.
               (let* ((data (gesture-spec-data input))
                      (charbits (gesture-spec-modifiers input)))
                 (handle-boxer-input data charbits (key-to-keep-shifted? data))))
              ((key-event? input)
               (handle-boxer-input (input-code input) (input-bits input)))
              ((mouse-event? input)
               (handle-boxer-input input))
              ((eq input :stop-and-quit)
               #+lispworks (capi:apply-in-pane-process *boxer-pane* #'(lambda ()
                (setf (boxer::active *boxer-pane*) nil)
                (setf boxer::*quit-boxer* t))))
              ((and (symbolp input) (not (null (symbol-function input))))
               (funcall input))
              ((and (consp input)
                    (or (functionp (car input))
                        (and (symbolp (car input))
                             (not (null (symbol-function (car input)))))))
               (apply (car input) (cdr input)))
              ((not (null boxer::*boxer-system-hacker*))
               (error "Unknown object, ~A, in event queue" input)))
        (boxer::repaint))
      #+lispworks(capi:apply-in-pane-process *boxer-pane* 'update-toolbar-font-buttons)
      )))

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
