;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
;;;;
;;;;   $Header: misc-prims.lisp,v 1.0 90/01/24 22:14:49 boxer Exp $
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
;;;;                                        +-Data--+
;;;;               This file is part of the | BOXER | system
;;;;                                        +-------+
;;;;
;;;;
;;;;
;;;;
;;;;           This File Contains random leftover primitives that don't
;;;;           have an obvious place to go.
;;;;           Including:
;;;;
;;;;           Apropos
;;;;           Date-and-Time
;;;;           Beep
;;;;           Sleep
;;;;           Redisplay
;;;;           Shrink-Box
;;;;           Expand-Box
;;;;           Mouse Primitives (for now)
;;;;
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   1/29/13 removed fixnum math from mouse-{x,y}-coord(s)
;;;;   2/ 3/12 launch-internal-xref bug: mac-file-ref-pathname => xref-pathname
;;;;  12/30/11 launch-internal-xref, edit-internal-xref from {launch,edit}-internal-mac-file
;;;;  12/13/11 choose-file, choose-new-file, choose-file-info
;;;;  11/06/11 avoid calls to redisplay for special case of 0 arg in SLEEP
;;;;   1/12/10 Changed bu::redisplay to use repaint-window, shouldn't be using it at all but compat with old code
;;;;           and all...  Also temporarily added bu::lisp-error t debug crash reporter
;;;;  11/20/09 mouse-window-coords, sleep force redisplay
;;;;  11/19/09 restored redislay prim for OpenGL
;;;;   7/17/07 removed many duplicate defs - probably from an errant paste,
;;;;           calls to (redisplay) => (repaint)
;;;;  10/14/06 call to name changed to call to name-string in transform-inputs-row
;;;;   5/19/05 bug report variables moved to comsa.lisp
;;;;   5/11/05 choose-new-file (misc-prims.lisp)
;;;;   4/16/05 status-line-get-box
;;;;   8/27/04 copyright error message in scroll-to-row was still wrong, fixed now...
;;;;   7/16/04 Fixed Copyright error message in set-text-size, set-port-text-size,
;;;;           scroll-to-row
;;;;   5/05/04 mouse button & shift key state predicates now handle mac & PC
;;;;   4/21/03 merged current LW and MCL files
;;;;   1/31/03 choose-file-info added
;;;;   9/06/02 UC free versions of SET-(PORT-)TEXT-SIZE, added SET-(PORT-)TEXT-DIMENSIONS
;;;;           UC free SCROLL-TO-ROW added GET-SCROLL-ROW, SET-SCROLL-ROW
;;;;  10/24/01 clear-input
;;;;  10/21/01 mouse (and shift key) state prims
;;;;   2/16/01 merged current LW and MCL files
;;;;
;;;;  12/29/00 fixed typo in scroll-to-row input flavor
;;;;  12/20/00 added scroll-to-row
;;;;   5/22/00 set-text-size & set-port-text size added, args in chars
;;;;   6/03/99 moved guts of click-sound to boxwin-xxx
;;;;
;;;;   2/16/99 moved platform dependent parts of click-sound out
;;;;   2/14/99 Started Harlequin Lispworks changes
;;;;  12/15/98 added click-sound prim
;;;;   9/06/98 changed sleep to use looping wait with NO process switching when
;;;;           arg is small (*simple-sleep-threshold* is .25 seconds)
;;;;   6/20/98 changed mouse-{x,y,_}-coords tracking functions to use get-visible-screen-objs
;;;;           instead of displayed-screen-objs to support ports to hidden graphics boxes
;;;;   6/20/98 started logging changes: source = Boxer version 2.3
;;;;

(in-package :boxer)



(boxer-eval::defboxer-primitive bu::test-error ()
  #-ecl (/ 2 0))

;;;;; Totally Random
;;; if there are enough of these, we might want to fork off a misc-prims file

;;; NOTE: many of these need the equivalent of make-box for virtual copies
;;; that gets the chunking right. This is a crocked up temporary version:

(defun crock-make-vc (rows)
  (top-level-virtual-copy-editor-box (make-box rows)))

(boxer-eval::defboxer-primitive bu::name-help ((bu::datafy pattern))
                                (name-help-internal (string-upcase (box-text-string pattern))))

(defun name-help-internal (search-string)
  (let ((rows nil))
    (do-symbols (s (find-package "BOXER-USER"))
      (when (search search-string (symbol-name s))
        (let ((binding (boxer-eval::boxer-symeval s)))
          (if (eq boxer-eval::*novalue* binding)
            (let ((unbound-message (get s 'unbound-name-help-message)))
              (unless (null unbound-message)
                (push (list (format nil "~A ~A" s unbound-message)) rows)))
            (push (list* (symbol-name s)
                         (make-row-arg-for-apropos binding))
                  rows)))))
    (if (null rows)
      (make-empty-vc)
      (crock-make-vc (nreverse rows)))))

(defun make-row-arg-for-apropos (binding)
  (cond ((boxer-eval::boxer-function? binding)
         (let ((args (mapcar #'(lambda (arg)
                                       (if (and (listp arg)
                                                (eq (car arg) 'boxer-eval::dont-copy))
                                         (cadr arg)
                                         arg))
                             (boxer-eval::boxer-function-arglist binding))))
           (list (if (null args)
                   (format nil "is a primitive with no arguments")
                   (format nil "is a primitive with arguments of ~A" args)))))
    ((doit-box? binding)
     (let* ((code (boxer-eval::cached-code-editor-box binding))
            (args (and code (boxer-eval::boxer-function-arglist code))))
       (list (cond ((null args) "is a doit box with no inputs")
               (t (format nil "is a doit box with inputs of ~A"
                          args))))))
    ((virtual-port? binding)
     (list* "is a port whose target "
            (make-row-arg-for-apropos (vp-target binding))))
    ((port-box? binding)
     (list* "is a port whose target "
            (make-row-arg-for-apropos (ports binding))))
    (t
     ;; describe the binding
     (list (format nil "is a ~A" (apropos-describe binding))))))

;;; the default is to just print the object, try and catch
;;; large objects and print descriptions of them here
(defun apropos-describe (thing)
  (cond ((virtual-copy? thing)
         (vc-type thing))
    ((data-box? thing)
     "Data Box")
    ((box-interface? thing)
     ;; should we be more specific depending upon type of interface
     ;; and whether there is a current box in existence or should
     ;; we preserve the illusion that this is a data-box
     "Data Box")
    (t (format nil "~A" thing))))


(boxer-eval::defboxer-primitive bu::last-unprintable-error-box ()
                                (boxer-eval::primitive-signal-error :obsolete-function
                                                                    "Use INVISIBLE-ERROR instead"))

(boxer-eval::defboxer-primitive bu::invisible-error ()
                                *last-unprintable-error-box*)

(boxer-eval::defboxer-primitive bu::last-unprintable-returned-value ()
                                (boxer-eval::primitive-signal-error :obsolete-function
                                                                    "Use INVISIBLE-VALUE instead"))

(boxer-eval::defboxer-primitive bu::invisible-value ()
                                *last-unprintable-returned-value*)



(defvar *default-input-port-target*
  (make-box '(("Redirect" "this" "port"))))

(defun make-input-port (name)
  (let ((ip (port-to-internal *default-input-port-target*)))
    (set-name ip (make-name-row (list name)))
    ip))

(defmethod insert-string-at-cha-no ((row row) string cha-no)
  (do* ((stop (length string))
        (si 0 (1+& si))
        (ri (+& cha-no si) (+& cha-no si)))
    ((>=& si stop))
    (let ((cha (char string si)))
      (insert-cha-at-cha-no row cha ri))))

(defun transform-inputs-row (editor-row)
  (let ((row-chunks (chunks editor-row))
        (current-flavor nil))
    (when (eq (chunk-chunk (get-pointer-value (car row-chunks) nil))'bu::input)
      (do* ((chunks (cdr row-chunks) (cdr chunks))
            (chunk (car chunks) (car chunks))
            (chunk-no 1 (1+& chunk-no)))
        ((null chunks))
        (let ((item (chunk-chunk (get-pointer-value chunk nil)))
              (start (beginning-of-nth-chunk-cha-no editor-row
                                                    chunk-no)))
          (cond ((box? item)
                 ;; remove the box from the row
                 (delete-cha-at-cha-no editor-row start)
                 ;(format t "D~D " start)
                 (let ((name (name-string item)))
                   (cond ((port-box? item)
                          ;; special handling for ports derived from
                          ;; port-flavored inputs
                          (insert-string-at-cha-no editor-row "port-to " start)
                          (setq chunk-no (1+& chunk-no))
                          (insert-string-at-cha-no editor-row name
                                                   (+ start
                                                      #.(length "port-to "))))
                     (t (insert-string-at-cha-no editor-row name start))))
                 ;; add the name
                 (setq current-flavor nil))
            ((boxer-eval::flavored-input-marker? item)
             (setq current-flavor item)
             ;; port flavor is handled specially
             (when (and (eq item 'bu::port-to)
                        ;; check the next token
                        (not (box? (chunk-chunk (get-pointer-value
                                                 (cadr chunks) nil)))))
               ;; remove port-to flavor token
               (dotimes (i (-& (end-of-nth-chunk-cha-no editor-row
                                                        chunk-no)
                               (1-& start)))
                 (delete-cha-at-cha-no editor-row (1-& start)))
               ;; now that a token has been removed, decrement the
               ;; chunk-no so we keep track of where we are
               (setq chunk-no (1-& chunk-no))))
            (t
             ;; must be a symbol, convert it to a box based on
             ;; the current flavor
             (let ((new-input-box (if (eq current-flavor 'bu::port-to)
                                    (make-input-port item)
                                    (make-box '(()) 'data-box item))))
               (dotimes (i (-& (end-of-nth-chunk-cha-no editor-row
                                                        chunk-no)
                               start))
                 (delete-cha-at-cha-no editor-row start))
               ;; insert the new token
               (insert-cha-at-cha-no editor-row new-input-box start)
               (setq current-flavor nil)))))))))

;;; this relies on *lexical-variables-root* to get hold of the editor box
(boxer-eval::defboxer-primitive bu::input ((boxer-eval::list-rest args))
  (cond ((and (box? boxer-eval::*lexical-variables-root*)
              ;; we test for box? in case (point-box) is NIL
              (eq boxer-eval::*lexical-variables-root* (point-box))
              (eq 'bu::input
                  (car (eval-objs (first-inferior-row
                                    boxer-eval::*lexical-variables-root*))))
              (equal (remove-if
                      #'(lambda (x)
                                (eq x 'boxer-eval::*ignoring-definition-object*)) args)
                      (remove-if
                      #'(lambda (x)
                                (eq x 'boxer-eval::*ignoring-definition-object*))
                      (cdr (eval-objs (first-inferior-row
                                        boxer-eval::*lexical-variables-root*))))))
          ;; make sure that we are on the 1st line of the box
          ;; and that box's INPUT line matches
          (transform-inputs-row
          (first-inferior-row boxer-eval::*lexical-variables-root*))
          (modified boxer-eval::*lexical-variables-root*)
          boxer-eval::*novalue*)
    (t
      ;; otherwise error out, seeing the INPUT line anywhere else is an error
      (boxer-eval::primitive-signal-error
      :bad-input "The INPUT line  should be the 1st line of a box"))))


(boxer-eval::defboxer-primitive bu::beep ()
  (bw::beep)
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::click-sound ()
  (bw::click-sound)
  boxer-eval::*novalue*)

#|
(boxer-eval::defrecursive-funcall-primitive bu::sleep ((boxer-eval::numberize seconds))
  :stack-frame-allocation (5 5 5 5)
  :state-variables (boxer-eval::*internal-time-stamp*)
  :before
  (cond ((complexp seconds)
   (signal-error :IMPROPER-ARGUMENT
           "cannot accept complex numbers"))
  ((minusp seconds)
   (signal-error :IMPROPER-ARGUMENT
           "expected a positive number"))
  (t (set-and-save-state-variables
      ;; record the desire FINISH time
      (+ (get-internal-real-time)
         (round (* seconds internal-time-units-per-second)))
      what)
     (recursive-eval-invoke what)))
  :after)


(boxer-eval::defrecursive-eval-primitive bu::synchronize ((boxer-eval::numberize seconds)
                (boxer-eval::list-rest what))
  :stack-frame-allocation (5 5 5 5)
  :state-variables (boxer-eval::*internal-time-stamp* boxer-eval::*repeat-list*)
  :before
  (cond ((complexp seconds)
   (signal-error :IMPROPER-ARGUMENT
           "cannot accept complex numbers"))
  ((minusp seconds)
   (signal-error :IMPROPER-ARGUMENT
           "expected a positive number"))
  (t (set-and-save-state-variables
      ;; record the desire FINISH time
      (+ (get-internal-real-time)
         (round (* seconds internal-time-units-per-second)))
      what)
     (recursive-eval-invoke what)))
  :after
  )
|#

(boxer-eval::defboxer-primitive bu::status-line-y-or-n? (prompt)
  (boxer-eval::boxer-boolean (status-line-y-or-n-p (box-text-string prompt))))

(boxer-eval::defboxer-primitive bu::status-line-message (message)
  (status-line-display 'boxer-editor-error (box-text-string message))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::status-line-get-box (prompt-message)
  (multiple-value-bind (string cancelled?)
                        (get-string-from-status-line (box-text-string prompt-message))
                        (cond ((not (null cancelled?))
                              (boxer-eval::primitive-signal-error :cancelled "Cancelled by user"))
                          (t (make-vc (list string))))))

;; this is wrong, should time stamp, then run other processes
;; need to add sleep time to evaluator state vars

;; 9/4/98 changed to use SNOOZE for small args
;; NOTE: this is a uniprocessing solution, when we have multiple boxer
;;       processes, we'll need a boxer-wait-with-timeout instead of SNOOZE

(defvar *simple-sleep-threshold* 0.25)

(boxer-eval::defboxer-primitive bu::sleep ((boxer-eval::numberize seconds))
                                (cond ((< seconds 0)
                                       (boxer-eval::primitive-signal-error :bad-arg "Should be a positive number"))
                                  ((= seconds 0))
                                  (t
                                   (repaint-in-eval t)
                                   (cond ((> seconds *simple-sleep-threshold*)
                                          (sleep seconds))
                                     (t (snooze seconds)))))
                                boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::redisplay ()
  (repaint)
  boxer-eval::*novalue*)

(defvar *verbose-date-and-time* t)


;; these use values supplied by (decode-universal-time (get-universal-time))
;; the non verbose form results conform to date specs in RFC822

(defun ut-day (day-number &optional (verbose *verbose-date-and-time*))
  (if verbose
    (case day-number
      (0 "Monday") (1 "Tuesday") (2 "Wednesday") (3 "Thursday")
      (4 "Friday") (5 "Saturday") (6 "Sunday"))
    (case day-number
      (0 "Mon") (1 "Tue") (2 "Wed") (3 "Thu")
      (4 "Fri") (5 "Sat") (6 "Sun"))))


(defun ut-month (month-number &optional (verbose *verbose-date-and-time*))
  (if verbose
    (case month-number
      (1 "January") (2 "February") (3 "March") (4 "April")
      (5 "May") (6 "June") (7 "July") (8 "August") (9 "September")
      (10 "October") (11 "November") (12 "December"))
    (case month-number
      (1 "Jan") (2 "Feb") (3 "Mar") (4 "Apr") (5 "May") (6 "Jun")
      (7 "Jul") (8 "Aug") (9 "Sep") (10 "Oct") (11 "Nov") (12 "Dec"))))

(defun ut-tz (tz &optional (daylight-savings-p nil))
  (case tz
    (-11 "-1100") (-10 "-1000") (-9 "-0900") (-8 "-0800") (-7 "-0700")
    (-6 "-0600") (-5 "-0500") (-4 "-0400") (-3 "-0300") (-2 "-0200")
    (-1 "-0100") (0 "+0000") (1 "+0100") (2 "+0200") (3 "+0300") (4 "+0400")
    (5 (if daylight-savings-p "EDT" "EST"))
    (6 (if daylight-savings-p "CDT" "CST"))
    (7 (if daylight-savings-p "MDT" "MST"))
    (8 (if daylight-savings-p "PDT" "PST"))
    (9 "+0900") (10 "+1000") (11 "+1100")
    (otherwise "")))


(boxer-eval::defboxer-primitive bu::date-and-time ()
  (multiple-value-bind (sec min hour date month year day daylight-p timezone)
                        (decode-universal-time (get-universal-time))
                        (crock-make-vc `((,(ut-day day)
                                          ,(ut-month month)
                                          ,(format nil "~d" date)
                                          ,(format nil "~d" year)
                                          ,(format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)
                                          ,(ut-tz timezone daylight-p)
                                          )))))

(boxer-eval::defboxer-primitive bu::boxer-version ()
  (crock-make-vc `((,(system-version))
                    (,(lisp-implementation-version) ,(lisp-implementation-type))
                    (,(machine-type) ,(machine-instance)))))



;;;; display control


(boxer-eval::defboxer-primitive bu::set-text-dimensions ((bu::port-to box)
                                                         (boxer-eval::numberize width)
                                                         (boxer-eval::numberize height))
  (let ((realbox (box-or-port-target box)))
    (when (box? realbox)
      (let ((*current-font-descriptor* (closest-bfd (first-inferior-row realbox) 0)))
        (multiple-value-bind (font-cha-wid font-cha-hei)
                              (current-font-values)
                              (set-fixed-size realbox
                                              (* width font-cha-wid) (* height font-cha-hei))
                              ;; allow further mousing
                              (unless (bottom-right-hotspot-active? realbox)
                                (set-bottom-right-hotspot-active? realbox t))
                              (modified realbox)))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::set-port-text-dimensions ((boxer-eval::dont-copy port)
                                                              (boxer-eval::numberize width)
                                                              (boxer-eval::numberize height))
  (let ((realbox (box-or-port-target port)))
    (when (virtual-port? port)
      (setq port (vp-editor-port-backpointer port)))
    (when (and (box? realbox) (port-box? port))
      (let ((*current-font-descriptor* (closest-bfd
                                        (first-inferior-row realbox) 0)))
        (multiple-value-bind (font-cha-wid font-cha-hei)
                              (current-font-values)
                              (set-fixed-size port
                                              (* width font-cha-wid) (* height font-cha-hei))
                              ;; allow further mousing
                              (unless (bottom-right-hotspot-active? port)
                                (set-bottom-right-hotspot-active? port t))
                              (modified port)))))
  boxer-eval::*novalue*)

(defun shrink-editor-box-internal (target super?)
  (cond ((and (box? target) (eq target (outermost-box)))
         (if super? (supershrink target) (shrink target))
         (let ((pb (point-box)))
           ;; need to bind point-box BEFORE set-outermost-box
           ;; need to move *point* up to the level of the tarfet
           ;; unless it is already there
           (unless (eq pb target)
             (move-point (box-first-bp-values target))
             (set-point-screen-box (outermost-screen-box)))
           (multiple-value-bind (new-outermost-box new-outermost-screen-box)
                                (get-previous-outermost-box-values)
                                (set-outermost-box new-outermost-box new-outermost-screen-box))
           (when (superior? pb target)
             (exit target (if (circular-port? target)
                            ;; get the highest screen-box
                            (let ((sb (screen-box-point-is-in)))
                              (do ((nsb sb (superior-screen-box nsb)))
                                ((or (not (screen-box? nsb))
                                     (eq (outermost-screen-box) nsb)))
                                (when (eq (screen-obj-actual-obj nsb) target)
                                  (setq sb nsb)))
                              (superior-screen-box sb))
                            (or (superior-screen-box (screen-box-point-is-in))
                                (car (displayed-screen-objs
                                      (superior-box (point-box))))))
                   (superior-box target) t)))
         (fill-doit-cursor-position-vector
          boxer-eval::*process-doit-cursor-position*))
    ((box? target)
     (if super?
       (unless (eq (display-style target) ':supershrunk)
         (supershrink target))
       (unless (eq (display-style target) ':shrunk)
         (shrink target))))
    (t
     (boxer-eval::primitive-signal-error
      :resize-error "You Can only Shrink Editor boxes"))))


(boxer-eval::defboxer-primitive bu::supershrink-box ((bu::port-to editor-box))
  (let ((target (box-or-port-target editor-box)))
    (shrink-editor-box-internal target t)
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::supershrink-port ((boxer-eval::dont-copy port))
  (let ((box (cond ((virtual-port? port)
                    (let ((orig (vp-editor-port-backpointer port)))
                      (if (and orig (superior? orig *initial-box*))
                        orig
                        (vp-target port))))
                ((port-box? port)
                (if (superior? port *initial-box*) port (ports port)))
                (t (box-or-port-target port)))))
    (shrink-editor-box-internal box t)
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::shrink-box ((bu::port-to editor-box))
  (let ((target (box-or-port-target editor-box)))
    (shrink-editor-box-internal target nil)
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::shrink-port ((boxer-eval::dont-copy port))
  (let ((box (cond ((virtual-port? port)
                    (let ((orig (vp-editor-port-backpointer port)))
                      (if (and orig (superior? orig *initial-box*))
                        orig
                        (vp-target port))))
                ((port-box? port)
                (if (superior? port *initial-box*) port (ports port)))
                (t (box-or-port-target port)))))
    (shrink-editor-box-internal box nil)
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::expand-box ((bu::port-to editor-box))
  (let ((target (box-or-port-target editor-box)))
    (cond ;((and (box? target) (eq target (outermost-box))
      ;	        (not (eq target *initial-box*)))
      ;	   (unshrink target) ; make sure it will end up expanded
      ;	   (let ((pb (point-box)))
      ;	     ;; need to bind point-box BEFORE set-outermost-box
      ;	     ;; need to move *point* up to the level of the tarfet
      ;	     ;; unless it is already there
      ;	     (unless (eq pb target)
      ;	       (move-point (box-first-bp-values target))
      ;	       (set-point-screen-box (outermost-screen-box)))
      ;	     (multiple-value-bind (new-outermost-box new-outermost-screen-box)
      ;	         (get-previous-outermost-box-values)
      ;	       (set-outermost-box new-outermost-box new-outermost-screen-box))
      ;	     (when (superior? pb target)
      ;	       (exit target (if (circular-port? target)
      ;				;; get the highest screen-box
      ;				(let ((sb (screen-box-point-is-in)))
      ;				  (do ((nsb sb (superior-screen-box nsb)))
      ;				      ((or (not (screen-box? nsb))
      ;					   (eq (outermost-screen-box) nsb)))
      ;				    (when (eq (screen-obj-actual-obj nsb)
      ;					      target)
      ;				      (setq sb nsb)))
      ;				  (superior-screen-box sb))
      ;				(or (superior-screen-box
      ;				     (screen-box-point-is-in))
      ;				    (car (displayed-screen-objs
      ;					  (superior-box (point-box))))))
      ;		     (superior-box target) t)))
      ;	   (fill-doit-cursor-position-vector
      ;	    boxer-eval::*process-doit-cursor-position*))
      ((box? target)
        (unless (eq (display-style target) ':normal)(unshrink target)))
      (t (boxer-eval::primitive-signal-error
          :resize-error "You Can only Expand Editor boxes")))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::expand-port ((boxer-eval::dont-copy port))
  (let ((box (cond ((virtual-port? port)
                    (let ((orig (vp-editor-port-backpointer port)))
                      (if (and orig (superior? orig *initial-box*))
                        orig
                        (vp-target port))))
                ((port-box? port)
                (if (superior? port *initial-box*) port (ports port)))
                (t (box-or-port-target port)))))
    (cond ((box? box)
            (unless (eq (display-style box) :normal) (unshrink box)))
      (t (boxer-eval::primitive-signal-error
          :resize-error "You Can only Expand Editor boxes")))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::full-screen-box ((bu::port-to editor-box))
  editor-box
  (boxer-eval::primitive-signal-error :obsolete-function
        "Use FULLSCREEN-BOX instead"))

;; added check for already fullscreen box 1/15/96
(boxer-eval::defboxer-primitive bu::fullscreen-box ((bu::port-to editor-box))
  (let ((target (box-or-port-target editor-box)))
    (cond ((eq target (outermost-box)))
      ((and (box? target) (not (null (displayed-screen-objs target))))
        (push *outermost-screen-box* *outermost-screen-box-stack*)
        (set-outermost-box target)
        (move-point (box-first-bp-values target))
        (set-point-screen-box (outermost-screen-box))
        (fill-doit-cursor-position-vector
        boxer-eval::*process-doit-cursor-position*))
      (t
        (boxer-eval::primitive-signal-error
        :resize-error "You Can only Zoom Visible Editor boxes")))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::fullscreen-port ((boxer-eval::dont-copy port))
  (let ((target (cond ((virtual-port? port)
                        (let ((orig (vp-editor-port-backpointer port)))
                          (if (and orig (superior? orig *initial-box*))
                            orig
                            (vp-target port))))
                  ((port-box? port)
                    (if (superior? port *initial-box*) port (ports port)))
                  (t (box-or-port-target port)))))
    (cond ((eq target (outermost-box)))
      ((and (box? target) (not (null (displayed-screen-objs target))))
        (push *outermost-screen-box* *outermost-screen-box-stack*)
        (set-outermost-box target)
        (move-point (box-first-bp-values target))
        (set-point-screen-box (outermost-screen-box))
        (fill-doit-cursor-position-vector
        boxer-eval::*process-doit-cursor-position*))
      (t
        (boxer-eval::primitive-signal-error
        :resize-error "You Can only Zoom Visible Editor boxes")))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::box-display ((bu::port-to editor-box))
  (let ((target (box-or-port-target editor-box)))
    (cond ((not (box? target))
            (boxer-eval::primitive-signal-error :bad-arg target " is not an editor box"))
      ((eq target (outermost-box))
        (make-vc (list 'bu::fullscreen)))
      (t (make-vc (list (intern-in-bu-package (display-style target))))))))

(boxer-eval::defboxer-primitive bu::port-display ((boxer-eval::dont-copy port))
  (let ((box (cond ((virtual-port? port)
                    (let ((orig (vp-editor-port-backpointer port)))
                      (if (and orig (superior? orig *initial-box*))
                        orig
                        (vp-target port))))
                ((port-box? port)
                (if (superior? port *initial-box*) port (ports port)))
                (t (box-or-port-target port)))))
    (cond ((not (box? box))
            (boxer-eval::primitive-signal-error :bad-arg box " is not an editor box"))
      ((eq box (outermost-box))
        (make-vc (list 'bu::fullscreen)))
      (t (make-vc (list (intern-in-bu-package (display-style box))))))))

(boxer-eval::defboxer-primitive bu::move-cursor ((bu::port-to editor-box)
                                                 (boxer-eval::numberize row)
                                                 (boxer-eval::numberize cha-no))
  (let ((target (box-or-port-target editor-box)))
    (cond  ((or (not (and (typep row 'fixnum) (plusp& row)))
                (not (and (typep cha-no 'fixnum) (plusp& cha-no))))
            (boxer-eval::primitive-signal-error
              :move-cursor-error "Row and Cha-no should be positive integers"))
      ((and (box? target) (superior? target *initial-box*))
        ;; need to make sure screen structure is up to date
        (process-editor-mutation-queue-within-eval)
        ;; might want to Bind *move-bp-zoom-pause-time* here for effect
        (let ((edrow (row-at-row-no target (1- row))))
          (if (null edrow)
            (boxer-eval::primitive-signal-error
            :move-cursor-error "No row, " row ", in box: " editor-box)
            (progn
            (with-temporary-bp (target-bp (values edrow (1- cha-no)))
              (move-to-bp target-bp))
            (fill-doit-cursor-position-vector
              boxer-eval::*process-doit-cursor-position*)))))
      (t
        (boxer-eval::primitive-signal-error
        :move-cursor-error "You can only move to Editor Boxes"))))
  boxer-eval::*novalue*)

;; new! 9/06/02

(boxer-eval::defboxer-primitive bu::get-scroll-row ((bu::port-to box))
  (let ((target (box-or-port-target box)))
    (cond ((box? target)
            (let* ((screen-box (car (screen-objs target)))
                  (box-scroll-info (getf (slot-value target 'plist) 'scroll-info))
                  (screen-scroll (when (screen-box? screen-box)
                                    (slot-value screen-box 'scroll-to-actual-row)))
                  (box-scroll (cdar box-scroll-info)))
              (1+
                (cond ((not (null screen-scroll))
                      (row-row-no target screen-scroll))
                  ((not (null box-scroll))
                  (row-row-no target box-scroll))
                  (t 0)))))
      (t
        (boxer-eval::primitive-signal-error
        :scrolling-error "Only editor boxes have scrolling info")))))

(boxer-eval::defboxer-primitive bu::set-scroll-row ((bu::port-to box) (boxer-eval::numberize row))
  (let ((target (box-or-port-target box)))
    (cond ((and (box? target)
                (typep row 'fixnum) (plusp& row))
            (let ((edrow (row-at-row-no target (1- row)))
                  (screen-objs (screen-objs target)))
              (unless (null edrow)
                (cond ((null screen-objs)
                      (record-scroll-info target nil edrow))
                  (t
                  (dolist (screen-obj screen-objs)
                    (set-scroll-to-actual-row screen-obj edrow))))))
            boxer-eval::*novalue*)
      (t
        (boxer-eval::primitive-signal-error
        :scrolling-error "You can only scroll editor boxes")))))

;;; get/set-scroll-x/y-position

;;; #|
;;; (boxer-eval::defboxer-primitive bu::get-scroll-x-position ((bu::port-to box)
;;;
;;; (boxer-eval::defboxer-primitive bu::get-scroll-y-position ((bu::port-to box)
;;;
;;; (boxer-eval::defboxer-primitive bu::set-scroll-x-position ((bu::port-to box)
;;;                                                (boxer-eval::numberize new-pos))
;;;
;;; (boxer-eval::defboxer-primitive bu::set-scroll-y-position ((bu::port-to box)
;;;                                                (boxer-eval::numberize new-pos))
;;;
;;; |#




;;;; NAMES in the environment

(defun get-names (box)
  (cond ((box? box)
         (make-vc
          (if (editor-box-changed? box)
            (progn
             ;; a hack to fill the cache...
             (lookup-variable-in-vc-rows-entry
              (car (slot-value box 'virtual-copy-rows))
              ':bogus-value
              box
              NIL)
             ;; The vc-rows-entry variable cache is no a valid
             ;; indication of the state of named boxes
             (let ((cache (vc-rows-entry-cached-binding-alist
                           (car (slot-value box 'virtual-copy-rows)))))
               (if (eq cache *no-names-in-vc-marker*)
                 (list (make-empty-evrow))
                 (mapcar #'(lambda (binding)
                                   (make-evrow-from-entry
                                    (boxer-eval::static-variable-name binding)))
                         cache))))
            ;; looks like the editor box knows what's in it
            (mapcar #'(lambda (binding)
                              (make-evrow-from-entry
                               (boxer-eval::static-variable-name binding)))
                    (slot-value box 'static-variables-alist)))))
    ((virtual-copy? box)
     ;; a hack to fill the cache...
     (lookup-variable-in-virtual-copy box ':bogus-value)
     ;; the cache is now guaranteed to be valid so...
     (let ((cache (vc-cached-binding-alist box)))
       (make-vc (if (eq cache *no-names-in-vc-marker*)
                  (list (make-empty-evrow))
                  (mapcar #'(lambda (binding)
                                    (make-evrow-from-entry
                                     (boxer-eval::static-variable-name binding)))
                          cache)))))
    ((numberp box)
     (make-empty-vc))
    (t
     (data-primitive-error "Don't know how to get the names from ~A"
                           box))))

(boxer-eval::defboxer-primitive bu::names ()
  (get-names (box-or-port-target (static-root))))

(boxer-eval::defboxer-primitive bu::name? ((boxer-eval::dont-copy name))
  (let ((symbol (car (boxer::flat-box-items name))))
    (boxer-eval::boxer-boolean
      (when (and (symbolp symbol) (not (null symbol)))
        (not (eq boxer-eval::*novalue* (boxer-eval::boxer-symeval symbol)))))))

(boxer-eval::defboxer-primitive bu::name-in-box? ((boxer-eval::dont-copy name))
  (let ((box (static-root))
        (symbol (caar (boxer::raw-unboxed-items name))))
    (boxer-eval::boxer-boolean
      (when (and (symbolp symbol) (not (null symbol)))
        (cond ((virtual-copy? box)
              (not (null (lookup-variable-in-virtual-copy box symbol))))
          ((box? box)
          (not (null (boxer-eval::lookup-static-variable-in-box-only
                      box symbol))))
          (t (boxer-eval::signal-error :IMPROPER-ARGUMENT "Expected a box")))))))

(defun bound-values (symbol)
  (when (symbolp symbol)
    (let ((local (boxer-eval::boxer-symeval symbol))
          (global (when (boundp symbol)
                    (boxer-eval::static-variable-value (symbol-value symbol)))))
      (values (unless (eq local boxer-eval::*novalue*) local) global))))

(boxer-eval::defboxer-primitive bu::top-level-name? ((boxer-eval::dont-copy name))
  (multiple-value-bind (local global)
                        (bound-values (caar (raw-unboxed-items name)))
                        (boxer-eval::boxer-boolean (and (not (null local)) (eq global local)))))

(boxer-eval::defboxer-primitive bu::local-name? ((boxer-eval::dont-copy name))
  (multiple-value-bind (local global)
                        (bound-values (caar (raw-unboxed-items name)))
                        (boxer-eval::boxer-boolean (and (not (null local)) (not (eq global local))))))

(boxer-eval::defboxer-primitive bu::target-name (pbox)
  (let ((box (box-or-port-target pbox)))
    (if (numberp box) (make-empty-vc)
      (make-vc
        (list
        (cond ((or (virtual-port? box) (virtual-copy? box))
                (if (not (null (vc-name box)))
                  (make-evrow-from-entry (box-name box))
                  (make-empty-evrow)))
          ((box? box)
            (if (not (null (name-row box)))
              (make-evrow-from-entry (editor-box-name-symbol box))
              (make-empty-evrow)))
          (t nil)))))))

(defvar *unique-symbol-counter* 0)

(boxer-eval::defboxer-primitive bu::unique-symbol ()
  (make-vc
    (list (make-evrow-from-entry
          (intern-in-bu-package
            (format nil "UNIQUE-SYMBOL-~D" (incf *unique-symbol-counter*)))))))




(boxer-eval::defboxer-primitive bu::dribble-on ((boxer-eval::dont-copy file))
  (dribble-on (box-text-string (box-or-port-target file)))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::dribble-off ()
  (dribble-off)
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::playback-dribble-file ((boxer-eval::dont-copy file))
  (let ((filename (box-text-string (box-or-port-target file))))
    ;; the mac version has trouble with redisplay in recursive editors
    (playback-dribble-file filename))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::dribble-pause () (record-pause-state))


;(boxer-eval::defboxer-primitive bu::dribble-grab-mouse-state ()   )

(boxer-eval::defboxer-primitive bu::choose-file ()
  (let ((name nil)
        (cancel-flag t))
    (catch 'cancel-boxer-file-dialog
      (setq name (boxer-open-file-dialog :prompt "Choose File:"))
      (setq cancel-flag nil))
    (cond ((not (null cancel-flag)) (boxer-eval::primitive-signal-error :Cancelled))
      ((null name) (make-empty-vc))
      (t (make-vc (list (namestring name)))))))

(boxer-eval::defboxer-primitive bu::choose-new-file ()
  (let ((name nil)
        (cancel-flag t))
    (catch 'cancel-boxer-file-dialog
      (setq name (boxer-new-file-dialog :prompt "Choose File:"))
      (setq cancel-flag nil))
    (cond ((not (null cancel-flag)) (boxer-eval::primitive-signal-error :Cancelled))
      ((null name) (make-empty-vc))
      (t (make-vc (list (namestring name)))))))

(boxer-eval::defboxer-primitive bu::choose-file-info ()
  (let ((name nil)
        (cancel-flag t))
    (catch 'cancel-boxer-file-dialog
      (setq name (boxer-open-file-dialog :prompt "Choose File:"))
      (setq cancel-flag nil))
    (cond ((not (null cancel-flag)) (boxer-eval::primitive-signal-error :Cancelled))
      ((null name) (make-empty-vc))
      (t (make-file-storage-info-box name)))))

;;; #|
;;; (boxer-eval::defboxer-primitive bu::launch-xref-file ((boxer-eval::dont-copy filename))
;;;   (let ((pathname (box-text-string (box-or-port-target filename))))
;;;     (cond ((not (probe-file pathname))
;;;            (boxer-eval::primitive-signal-error :mac-interface
;;;                                          "File not Found" pathname))
;;;           (t (applescript-open-xref pathname)))
;;;     boxer-eval::*novalue*))
;;; |#

(boxer-eval::defboxer-primitive bu::launch-internal-xref ()
  (let* ((box (static-root))
          (xfile (when (box? box) (getprop box :xref))))
    (cond ((null xfile)
            (boxer-eval::primitive-signal-error :mac-interface "No file to launch"))
      ((not (probe-file (xref-pathname xfile)))
        (boxer-eval::primitive-signal-error :mac-interface
                                            "File not Found"
                                            (xref-pathname xfile)))
      (t
        (os-open-xref xref)))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::edit-internal-xref ()
  (let* ((box (static-root))
          (xfile (when (box? box) (getprop box :xref))))
    (catch 'boxer::cancel-boxer-file-dialog ;; edit-xref may use file dialogs which can throw to this tag
      (edit-xref box xfile)))
  boxer-eval::*novalue*)




(boxer-eval::defboxer-primitive bu::show-key-name ()
  (status-line-display 'show-key-name "Press a key or click the mouse...")
  (let* ((input (get-boxer-input *boxer-pane*))
         (key-name (lookup-input-name input)))
    (status-line-undisplay 'show-key-name)
    (make-vc (list (make-evrow-from-entry key-name)))))

(boxer-eval::defboxer-primitive bu::restore-original-keyboard ()
  (maphash #'(lambda (key-name function)
                      (boxer-eval::boxer-toplevel-set-nocache key-name function))
            (slot-value *global-top-level-mode* 'comtab))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::unset-key ((bu::datafy key-name))
  (boxer-eval::boxer-toplevel-nocache-unset
    (intern-in-bu-package (string-upcase (box-text-string key-name))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::set-key ((bu::datafy key-name) (bu::datafy command-name))
  (let* ((command-string (string-upcase (box-text-string command-name)))
          (command (intern (if (search "COM-" command-string)
                            command-string
                            (concatenate 'string "COM-" command-string))
                          'boxer)))
    (if (fast-memq command *boxer-editor-commands*)
      (boxer-eval::boxer-toplevel-set-nocache
        (intern-in-bu-package (string-upcase (box-text-string key-name)))
        (boxer-eval::encapsulate-key-function command))
      (boxer-eval::primitive-signal-error :bad-arg command " is not an editor command")))
  boxer-eval::*novalue*)



;;;; Mousing around

(boxer-eval::defboxer-primitive bu::mouse-buttons ()
  (mouse-button-state))

; moved to comdef.lisp
;(defvar *inside-mouse-coords-for-prims?* nil)

(defun mouse-window-coords-for-prims (&optional wait-action)
  (unless (null wait-action)
    ;; if we are going to wait, make sure screen is up to date
    (repaint-in-eval t))
  (let ((success? nil) (x 0) (y 0)
                       (*inside-mouse-coords-for-prims?* T))
    (catch 'mouse-coord-cancel
      (multiple-value-setq (x y) (mouse-window-coords :wait-action wait-action))
      (setq success? t))
    (if success?
      (values x y)
      (boxer-eval::primitive-signal-error :mouse
                                          "Get Mouse Coordinate Cancelled"))))

(boxer-eval::defboxer-primitive bu::mouse-box ()
  (multiple-value-bind (x y)
                        (mouse-window-coords-for-prims)
                        (let* ((mbp (mouse-position-values x y))
                              (mrow (bp-row mbp)))
                          (if (and (row? mrow)
                                  (not (null (superior-box mrow))))
                            (port-to (superior-box mrow))
                            ;; maybe should error out instead ?
                            (make-empty-vc)))))

(boxer-eval::defboxer-primitive bu::mouse-box-on-release ()
  (multiple-value-bind (x y)
                        (mouse-window-coords-for-prims :up)
                        (let* ((mbp (mouse-position-values x y))
                              (mrow (bp-row mbp)))
                          (if (and (row? mrow)
                                  (not (null (superior-box mrow))))
                            (port-to (superior-box mrow))
                            ;; maybe should error out instead ?
                            (make-empty-vc)))))

(boxer-eval::defboxer-primitive bu::mouse-box-on-click ()
  (multiple-value-bind (x y)
                        (mouse-window-coords-for-prims :down)
                        (let* ((mbp (mouse-position-values x y))
                              (mrow (bp-row mbp)))
                          (boxer-eval::reset-poll-count)
                          (if (and (row? mrow)
                                  (not (null (superior-box mrow))))
                            (port-to (superior-box mrow))
                            ;; maybe should error out instead ?
                            (make-empty-vc)))))



;;; box relative mousing around

;;; basically a hook for further mods.  For example, we might want to
;;; make it prefer a box seen via boxes as opposed to a box seen via
;;; ports.  On the other hand, there probably isn't a really good
;;; heuristic for selecting the best box out of multiple view.
;;; other possibilities are, location (farthest right/left/top/bottom)
;;; or unclipped over clipped....

(defun get-screen-box-for-mouse (box)
  (car (displayed-screen-objs box)))

(defun xy-inside-box (screen-box)
  (multiple-value-bind (left top right bottom)
                       (box-borders-widths (box-type screen-box) screen-box)
                       (declare (ignore right bottom))
                       (multiple-value-bind (out-x out-y)
                                            (xy-position screen-box)
                                            (values (+ left out-x) (+ top out-y)))))


(boxer-eval::defboxer-primitive bu::mouse-rc ((bu::port-to relative-box))
  (let ((box (box-or-port-target relative-box)))
    (cond ((not (or (box? box) (superior? box *initial-box*)))
            (boxer-eval::primitive-signal-error :mouse-error
                                                "The box, " box
                                                ", is not in the editor"))
      (t
        (multiple-value-bind (x y)
                            (mouse-window-coords-for-prims)
                            (let* ((mbp (mouse-position-values x y))
                                    (sb  (bp-screen-box mbp))
                                    (eb (box-or-port-target (screen-obj-actual-obj sb))))
                              (cond ((eq eb box)
                                      (make-vc (list (make-evrow-from-entries
                                                      (list (1+& (row-row-no eb (bp-row mbp)))
                                                            (1+& (chunk-no-at-cha-no
                                                                  (bp-row mbp) (bp-cha-no mbp) t)))))))
                                ((superior? eb box)
                                  (do* ((inf eb next-up)
                                        (next-up (superior-box inf) (superior-box inf)))
                                    ((eq next-up box)
                                    (let ((subrow (superior-row inf)))
                                      (make-vc (list (make-evrow-from-entries
                                                      (list (1+& (row-row-no box subrow))
                                                            (1+& (chunk-no-at-cha-no
                                                                  subrow
                                                                  (cha-cha-no subrow inf) t))))))))
                                    ))
                                (t
                                  (boxer-eval::primitive-signal-error
                                  :mouse-error "The mouse is not in the box, " box)))))))))

(boxer-eval::defboxer-primitive bu::mouse-rc-on-click ((bu::port-to relative-box))
  (let ((box (box-or-port-target relative-box)))
    (cond ((not (or (box? box) (superior? box *initial-box*)))
            (boxer-eval::primitive-signal-error :mouse-error
                                                "The box, " box
                                                ", is not in the editor"))
      (t
        (multiple-value-bind (x y)
                            (mouse-window-coords-for-prims :down)
                            (let* ((mbp (mouse-position-values x y))
                                    (sb  (bp-screen-box mbp))
                                    (eb (box-or-port-target (screen-obj-actual-obj sb))))
                              (cond ((eq eb box)
                                      (make-vc (list (make-evrow-from-entries
                                                      (list (1+& (row-row-no eb (bp-row mbp)))
                                                            (1+& (chunk-no-at-cha-no
                                                                  (bp-row mbp) (bp-cha-no mbp)
                                                                  t)))))))
                                ((superior? eb box)
                                  (do* ((inf eb next-up)
                                        (next-up (superior-box inf) (superior-box inf)))
                                    ((eq next-up box)
                                    (let ((subrow (superior-row inf)))
                                      (make-vc (list (make-evrow-from-entries
                                                      (list (1+& (row-row-no box subrow))
                                                            (1+& (chunk-no-at-cha-no
                                                                  subrow
                                                                  (cha-cha-no subrow inf) t))))))))
                                    ))
                                (t
                                  (boxer-eval::primitive-signal-error
                                  :mouse-error "The mouse is not in the box, " box)))))))))

(boxer-eval::defboxer-primitive bu::mouse-rc-on-release ((bu::port-to relative-box))
  (let ((box (box-or-port-target relative-box)))
    (cond ((not (or (box? box) (superior? box *initial-box*)))
            (boxer-eval::primitive-signal-error :mouse-error
                                                "The box, " box
                                                ", is not in the editor"))
      (t
        (multiple-value-bind (x y)
                            (mouse-window-coords-for-prims :up)
                            (let* ((mbp (mouse-position-values x y))
                                    (sb  (bp-screen-box mbp))
                                    (eb (box-or-port-target (screen-obj-actual-obj sb))))
                              (cond ((eq (box-or-port-target box) eb)
                                      (make-vc (list (make-evrow-from-entries
                                                      (list (1+& (row-row-no eb (bp-row mbp)))
                                                            (1+& (chunk-no-at-cha-no
                                                                  (bp-row mbp) (bp-cha-no mbp)
                                                                  t)))))))
                                ((superior? eb box)
                                  (do* ((inf eb next-up)
                                        (next-up (superior-box inf) (superior-box inf)))
                                    ((eq next-up box)
                                    (let ((subrow (superior-row inf)))
                                      (make-vc (list (make-evrow-from-entries
                                                      (list (1+& (row-row-no box subrow))
                                                            (1+& (chunk-no-at-cha-no
                                                                  subrow
                                                                  (cha-cha-no subrow inf) t))))))))
                                    ))
                                (t
                                  (boxer-eval::primitive-signal-error
                                  :mouse-error "The mouse is not in the box, " box)))))))))

(boxer-eval::defboxer-primitive bu::mouse-rc-box ()
  (multiple-value-bind (x y)
                        (mouse-window-coords-for-prims)
                        (let* ((mbp (mouse-position-values x y))
                              (sb (bp-screen-box mbp))
                              (mrow (bp-row mbp))
                              (eb (box-or-port-target (screen-obj-actual-obj sb))))
                          (if (and (box? eb) (row? mrow))
                            (make-vc (list (make-evrow-from-entries
                                            (list (if (name-row? mrow)
                                                    'bu::name
                                                    (1+& (row-row-no eb mrow)))
                                                  (1+& (chunk-no-at-cha-no mrow
                                                                          (bp-cha-no mbp) t))
                                                  (port-to eb)))))
                            (boxer-eval::primitive-signal-error :mouse-error
                                                                "Can't find box under mouse")))))

(boxer-eval::defboxer-primitive bu::mouse-rc-box-on-click ()
  (multiple-value-bind (x y)
                        (mouse-window-coords-for-prims :down)
                        (let* ((mbp (mouse-position-values x y))
                              (sb (bp-screen-box mbp))
                              (mrow (bp-row mbp))
                              (eb (box-or-port-target (screen-obj-actual-obj sb))))
                          (if (and (box? eb) (row? mrow))
                            (make-vc (list (make-evrow-from-entries
                                            (list (if (name-row? mrow)
                                                    'bu::Name
                                                    (1+& (row-row-no eb mrow)))
                                                  (1+& (chunk-no-at-cha-no mrow
                                                                          (bp-cha-no mbp) t))
                                                  (port-to eb)))))
                            (boxer-eval::primitive-signal-error :mouse-error
                                                                "Can't find box under mouse")))))

(boxer-eval::defboxer-primitive bu::mouse-rc-box-on-release ()
  (multiple-value-bind (x y)
                        (mouse-window-coords-for-prims :up)
                        (let* ((mbp (mouse-position-values x y))
                              (sb (bp-screen-box mbp))
                              (mrow (bp-row mbp))
                              (eb (box-or-port-target (screen-obj-actual-obj sb))))
                          (if (and (box? eb) (row? mrow))
                            (make-vc (list (make-evrow-from-entries
                                            (list (if (name-row? mrow)
                                                    'bu::Name
                                                    (1+& (row-row-no eb mrow)))
                                                  (1+& (chunk-no-at-cha-no mrow
                                                                          (bp-cha-no mbp) t))
                                                  (port-to eb)))))
                            (boxer-eval::primitive-signal-error :mouse-error
                                                                "Can't find box under mouse")))))





;;; sprite relative mousing around

(defun translate-sprite-mouse-x-coordinate (graphics-box x)
  (with-graphics-vars-bound (graphics-box)
    (user-coordinate-x x)))

(defun translate-sprite-mouse-y-coordinate (graphics-box y)
  (with-graphics-vars-bound (graphics-box)
    (user-coordinate-y y)))

(defun translate-sprite-mouse-coordinates (graphics-box x y)
  (with-graphics-vars-bound (graphics-box)
    (values (user-coordinate-x x) (user-coordinate-y y))))

(defun mouse-x-coord (abs-mouse-x abs-mouse-y)
  (let* ((graphics-box (get-relevant-graphics-box))
         (screen-boxes (and graphics-box (get-visible-screen-objs graphics-box))))
    (if (null screen-boxes)
      (boxer-eval::primitive-signal-error :mouse-error "Graphics Box is not visible")
      (do* ((sbs screen-boxes (cdr sbs)) (sb (car sbs) (car sbs)) (last-x 0))
        ((null sb) last-x)
        (multiple-value-bind (bx by)
                             (xy-inside-box sb)
                             (if (and (< bx abs-mouse-x (+ bx (screen-obj-wid sb)))
                                      (< by abs-mouse-y (+ by (screen-obj-hei sb))))
                               (return (translate-sprite-mouse-x-coordinate
                                        graphics-box (- abs-mouse-x bx)))
                               (setq last-x (translate-sprite-mouse-x-coordinate
                                             graphics-box (- abs-mouse-x bx)))))))))

(defun mouse-y-coord (abs-mouse-x abs-mouse-y)
  (let* ((graphics-box (get-relevant-graphics-box))
         (screen-boxes (and graphics-box (get-visible-screen-objs graphics-box))))
    (if (null screen-boxes)
      (boxer-eval::primitive-signal-error :mouse-error "Graphics Box is not visible")
      (do* ((sbs screen-boxes (cdr sbs)) (sb (car sbs) (car sbs)) (last-y 0))
        ((null sb) last-y)
        (multiple-value-bind (bx by)
                             (xy-inside-box sb)
                             (if (and (< bx abs-mouse-x (+ bx (screen-obj-wid sb)))
                                      (< by abs-mouse-y (+ by (screen-obj-hei sb))))
                               (return (translate-sprite-mouse-y-coordinate
                                        graphics-box (- abs-mouse-y by)))
                               (setq last-y (translate-sprite-mouse-y-coordinate
                                             graphics-box (- abs-mouse-y by)))))))))

(defun mouse-coords (abs-mouse-x abs-mouse-y)
  (let* ((graphics-box (get-relevant-graphics-box))
         (screen-boxes (and graphics-box (get-visible-screen-objs graphics-box))))
    (if (null screen-boxes)
      (boxer-eval::primitive-signal-error :mouse-error "Graphics Box is not visible")
      (do* ((sbs screen-boxes (cdr sbs))
            (sb (car sbs) (car sbs)))
        ((null (cdr sbs))
         (multiple-value-bind (bx by)
                              (xy-inside-box sb)
                              (multiple-value-bind (sx sy)
                                                   (translate-sprite-mouse-coordinates
                                                    graphics-box (- abs-mouse-x bx) (- abs-mouse-y by))
                                                   (make-vc (list (make-evrow-from-entries (list sx sy)))))))
        (multiple-value-bind (bx by)
                             (xy-inside-box sb)
                             (when (and (< bx abs-mouse-x (+ bx (screen-obj-wid sb)))
                                        (< by abs-mouse-y (+ by (screen-obj-hei sb))))
                               (multiple-value-bind (sx sy)
                                                    (translate-sprite-mouse-coordinates
                                                     graphics-box (- abs-mouse-x bx) (- abs-mouse-y by))
                                                    (return
                                                     (make-vc
                                                      (list (make-evrow-from-entries (list sx sy))))))))))))

(boxer-eval::defboxer-primitive bu::mouse-x-position ()
  (multiple-value-bind (x y) (mouse-window-coords-for-prims) (mouse-x-coord x y)))

(boxer-eval::defboxer-primitive bu::mouse-x-position-on-release ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :up)
    (mouse-x-coord x y)))

(boxer-eval::defboxer-primitive bu::mouse-x-position-on-click ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :down)
    (mouse-x-coord x y)))

(boxer-eval::defboxer-primitive bu::mouse-y-position ()
  (multiple-value-bind (x y) (mouse-window-coords-for-prims) (mouse-y-coord x y)))

(boxer-eval::defboxer-primitive bu::mouse-y-position-on-release ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :up)
    (mouse-y-coord x y)))

(boxer-eval::defboxer-primitive bu::mouse-y-position-on-click ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :down)
    (mouse-y-coord x y)))

(boxer-eval::defboxer-primitive bu::mouse-position ()
  (multiple-value-bind (x y) (mouse-window-coords-for-prims) (mouse-coords x y)))

(boxer-eval::defboxer-primitive bu::mouse-position-on-release ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :up)
    (mouse-coords x y)))

(boxer-eval::defboxer-primitive bu::mouse-position-on-click ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :down)
    (mouse-coords x y)))



;; mouse (and shift key) state prims

(boxer-eval::defboxer-primitive bu::mouse-down? ()   (boxer-eval::boxer-boolean (bw::mouse-down?)))
(boxer-eval::defboxer-primitive bu::mouse-left? ()   (boxer-eval::boxer-boolean (bw::mouse-left?)))
(boxer-eval::defboxer-primitive bu::mouse-middle? () (boxer-eval::boxer-boolean (bw::mouse-middle?)))
(boxer-eval::defboxer-primitive bu::mouse-right? ()  (boxer-eval::boxer-boolean (bw::mouse-right?)))

;; Note: on mac, we could make it OR of command and control keys
(boxer-eval::defboxer-primitive bu::control-key? () (boxer-eval::boxer-boolean (bw::control-key?)))

(boxer-eval::defboxer-primitive bu::command-key? ()
  (boxer-eval::boxer-boolean #+mcl   (bw::command-key?)
                       #+lwwin (bw::control-key?)))

(boxer-eval::defboxer-primitive bu::alt-key?     ()
  (boxer-eval::boxer-boolean #+mcl   (bw::option-key?)
                       #+lwwin (bw::alt-key?)))

(boxer-eval::defboxer-primitive bu::option-key?     ()
  (boxer-eval::boxer-boolean #+mcl   (bw::option-key?)
                       #+lwwin (bw::alt-key?)))

(boxer-eval::defboxer-primitive bu::meta-key?     ()
  (boxer-eval::boxer-boolean #+mcl   (bw::option-key?)
                       #+lwwin (bw::alt-key?)))

(boxer-eval::defboxer-primitive bu::shift-key?   () (boxer-eval::boxer-boolean (bw::shift-key?)))
(boxer-eval::defboxer-primitive bu::capslock-key?() (boxer-eval::boxer-boolean (bw::capslock-key?)))

;;; temp for debugging crash reporter

(boxer-eval::defboxer-primitive bu::lisp-error () (error "Foo !") boxer-eval::*novalue*)
