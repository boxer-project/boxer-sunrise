;; -*- Mode:LISP; Syntax:Common-Lisp; Package:(BOXER :USE (LISP) :NICKNAMES (BOX)) -*-
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
;;;;
;;;;         This file contains the useful macros for boxer
;;;;
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   3/ 7/13 removed fixnum arithmetic from snooze.  MacOS time can be larger than fixnums
;;;;   2/10/03 merged current mac & PC source
;;;;   4/17/02 added char-case macro
;;;;   4/08/02 added string-case macro
;;;;   3/27/00 smarter LWWIN version of input-code
;;;;   8/03/99 added lwwin version of key-event?
;;;;   4/10/99 added constant *degs->rads* = (/ pi 180.0) (can't believe this wasn't
;;;;           already done !!)
;;;;   2/14/99 added lispworks version of input-bits, make-char
;;;;   1/08/99 added compiler-let for for Lispworks
;;;;  10/25/98 Additions for Harlequin Lispworks for Windows 4.1
;;;;   5/27/98 added with-lisp-error-reporting
;;;;   5/27/98 Started Logging Changes: source = Boxer version 2.3 alphaR1
;;;;

(in-package :box)

;;; DEFSUBST (some implementation have it and others don't)
;;; https://lisp-hug.lispworks.narkive.com/xK5EMsyd/defsubst
(defmacro defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args
       ,@body)))

;;;; Macros to avoid using Common Lisp declarations

;;; Things that ought to be done right in Come-on Lisp but aren't.
;;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node153.html
;;; Compatibility note: In MacLisp, the assoc function uses an equal
;;; comparison rather than eql, which is the default test for assoc
;;; in Common Lisp. Where in MacLisp one would write (assoc x y), in
;;; Common Lisp one must write (assoc x y :test #'equal) to get the
;;; completely identical effect. Similarly, one can get the precise
;;; effect, and no more, of the MacLisp (assq x y) by writing in
;;; Common Lisp (assoc x y :test #'eq).
(defmacro fast-assq (thing place)
  `(assoc ,thing ,place :test #'eq))

;;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node144.html
;;; Compatibility note: In MacLisp, the delete function uses an equal
;;; comparison rather than eql, which is the default test for delete
;;; in Common Lisp. Where in MacLisp one would write (delete x y), one
;;; must in Common Lisp write (delete x y :test #'equal) to get the
;;; completely identical effect. Similarly, one can get the precise
;;; effect, and no more, of the MacLisp (delq x y) by writing in
;;; Common Lisp (delete x y :test #'eq).
(defmacro fast-delq (thing place)
  `(delete ,thing ,place :test #'eq))

(defmacro fast-del-if (fun place)
  `(delete-if ,fun ,place))

;;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node152.html
;;; Compatibility note: In MacLisp, the member function uses an equal
;;; comparison rather than eql, which is the default test for member
;;; in Common Lisp. Where in MacLisp one would write (member x y), in
;;; Common Lisp one must write (member x y :test #'equal) to get a
;;; completely identical effect. Similarly, one can get the precise
;;; effect, and no more, of the MacLisp (memq x y) by writing in
;;; Common Lisp (member x y :test #'eq).
(defmacro fast-memq (thing place)
  `(member ,thing ,place :test #'eq))

;;; Arithmetic

;;; single floats are sufficient for most of the things we
;;; need to do, unfortunately, different implementations require
;;; different types of declarations so...
(deftype boxer-float () 'float)


;;; lcl::fix and lcl::fixr are not supported.

(defmacro fix  (a) `(values (floor ,a)))
(defmacro fixr (a) `(values (truncate ,a)))

(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (defvar *constant-folding-macros* nil)
  (defvar *constant-folding-macro-bindings* nil)
  )



;; sgithens commenting out these compiler-let and constant-let items for now.
;; They only seem to be used in sicilian-borders.lisp, border-macros.lisp, and evalmacs.lisp
;; There is also a reference to compiler-let in pkg.lisp

;; (defmacro constant-let* (varlist &body body)
;;   (if (null varlist) `(progn . ,body)
;;       `(constant-let (,(car varlist))
;; 	 (constant-let* ,(cdr varlist)
;; 	   . ,body))))

;; +++ a portable replacement should be found now that compiler-let is no longer in the language
;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node83.html
#+lispworks
(defmacro compiler-let (let-forms &body body)
  `(lispworks:compiler-let ,let-forms ,@body))

;; (defmacro constant-let (varlist &body body)
;;   (let ((new-varlist
;; 	  (mapcar #'(lambda (pair) (list (car pair) (simplify-arg (cadr pair))))
;; 		  varlist)))
;;     `(compiler-let ((*constant-folding-macro-bindings*
;;                      ',(append *constant-folding-macro-bindings* new-varlist)))
;;        (let ,new-varlist . ,body))))

(defmacro defconstant-folding-macro (name args &body body)
  (unless (fast-memq name *constant-folding-macros*)
    (push name *constant-folding-macros*))
  `(defmacro ,name ,args . ,body))


(eval-when
    (:compile-toplevel :load-toplevel :execute)

  (defun constant-folding-macro? (thing)
    (fast-memq thing *constant-folding-macros*))

  (defun simplify-arg (arg)
    (cond ((constantp arg)
           (if (symbolp arg)
               (symbol-value arg)
               arg))
          ((and (listp arg) (constant-folding-macro? (car arg)))
           (let ((value (macroexpand arg)))
             (cond ((constantp value) value)
                   (t (values value t)))))
          ((and (symbolp arg)
                (fast-assq arg *constant-folding-macro-bindings*))
           (let ((value (fast-assq arg *constant-folding-macro-bindings*)))
             (cond ((and value (constantp (cadr value))) (cadr value))
                   (t (values arg t)))))
          (t (values arg t))))


  (defun replace-constant-vars (args)
    (let ((new-args nil)
          (constant-x t))
      (dolist (arg args)
        (multiple-value-bind (newarg not-c?)
            (simplify-arg arg)
          (push newarg new-args)
          (when not-c?
            (setq constant-x nil))))
      (values (nreverse new-args) constant-x)))

  )


(defconstant-folding-macro +& (&rest args) `(fixnum-plus . ,args))
(defconstant-folding-macro fixnum-plus (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'+ newargs)
        (case (length newargs)
          (0 '0)
          (1 (first newargs))
          (2 `(the fixnum (+ (the fixnum ,(first newargs))
                             (the fixnum ,(second newargs)))))
          (t `(the fixnum
                   (+ (the fixnum ,(first newargs))
                      (fixnum-plus . ,(rest newargs)))))))))

(defconstant-folding-macro -& (&rest args) `(fixnum-minus . ,args))
(defconstant-folding-macro fixnum-minus (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'- newargs)
        (case (length newargs)
          (0 '0)
          (1 `(- (the fixnum ,(first newargs))))
          (2 `(the fixnum (- (the fixnum ,(first newargs))
                             (the fixnum ,(second newargs)))))
          (t `(the fixnum
                   (- (the fixnum ,(first newargs))
                      (fixnum-plus . ,(rest newargs)))))))))

(defconstant-folding-macro *& (&rest args) `(fixnum-times . ,args))
(defconstant-folding-macro fixnum-times (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'* newargs)
        (case (length newargs)
          (0 '0)
          (1 (first newargs))
          (2 `(the fixnum (* (the fixnum ,(first newargs))
                             (the fixnum ,(second newargs)))))
          (t `(the fixnum
                   (* (the fixnum ,(first newargs))
                      (fixnum-times . ,(rest newargs)))))))))

(defconstant-folding-macro =& (&rest args) `(fixnum-= . ,args))
(defconstant-folding-macro fixnum-= (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'= newargs)
        (case (length newargs)
          (0 (warn "Better Watch Out, ~A needs at least 1 arg" 'fixnum-=)
           '(error "~A needs at least 1 arg" 'fixnum-=))
          (1 t)
          (t `(= . ,(mapcar #'(lambda (x) `(the fixnum ,x)) newargs)))))))

(defconstant-folding-macro <& (&rest args) `(fixnum-< . ,args))
(defconstant-folding-macro fixnum-< (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'< newargs)
        (case (length newargs)
          (0 (warn "Better Watch Out, ~A needs at least 1 arg" 'fixnum-<)
           '(error "~A needs at least 1 arg" 'fixnum-<))
          (1 t)
          (t `(< . ,(mapcar #'(lambda (x) `(the fixnum ,x)) newargs)))))))

(defconstant-folding-macro >& (&rest args) `(fixnum-> . ,args))
(defconstant-folding-macro fixnum-> (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'> newargs)
        (case (length newargs)
          (0 (warn "Better Watch Out, ~A needs at least 1 arg" 'fixnum->)
           '(error "~A needs at least 1 arg" 'fixnum->))
          (1 t)
          (t `(> . ,(mapcar #'(lambda (x) `(the fixnum ,x)) newargs)))))))

(defconstant-folding-macro <=& (&rest args) `(fixnum-<= . ,args))
(defconstant-folding-macro fixnum-<= (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'<= newargs)
        (case (length newargs)
          (0 (warn "Better Watch Out, ~A needs at least 1 arg" 'fixnum-<=)
           '(error "~A needs at least 1 arg" 'fixnum-<=))
          (1 t)
          (t `(<= . ,(mapcar #'(lambda (x) `(the fixnum ,x)) newargs)))))))

(defconstant-folding-macro >=& (&rest args) `(fixnum->= . ,args))
(defconstant-folding-macro fixnum->= (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'>= newargs)
        (case (length newargs)
          (0 (warn "Better Watch Out, ~A needs at least 1 arg" 'fixnum->=)
           '(error "~A needs at least 1 arg" 'fixnum->=))
          (1 t)
          (t `(>= . ,(mapcar #'(lambda (x) `(the fixnum ,x)) newargs)))))))

(defconstant-folding-macro minusp& (arg)
  `(minusp (the fixnum ,arg)))

(defconstant-folding-macro plusp& (arg)
  `(plusp (the fixnum ,arg)))

(defmacro incf& (arg &optional (amount nil supplied?))
  (if supplied?
      `(the fixnum (incf (the fixnum ,arg) (the fixnum ,amount)))
      `(the fixnum (incf (the fixnum ,arg)))))

(defmacro decf& (arg &optional (amount nil supplied?))
  (if supplied?
      `(the fixnum (decf (the fixnum ,arg) (the fixnum ,amount)))
      `(the fixnum (decf (the fixnum ,arg)))))

;;; Lucid's 1+ doesn't compile as well.
;;; One hopes that everybody else will optimize (+ 1 ...) into (1+ ...) if they
;;; want to.
(defconstant-folding-macro 1+& (arg)
  `(the fixnum (+ 1 (the fixnum ,(simplify-arg arg)))))

;;; Ditto for 1-.
(defconstant-folding-macro 1-& (arg)
  `(the fixnum (- (the fixnum ,(simplify-arg arg)) 1)))

(defmacro /2& (a)
  `(the fixnum (ash (the fixnum ,a) -1)))

(defconstant-folding-macro zerop& (arg)
  `(zerop (the fixnum ,(simplify-arg arg))))

(defmacro svref& (vector index)
  ;; `(svref ,vector ,index))
  `(svref (the simple-vector ,vector) (the fixnum ,index)))
;; sgithens TODO (svref (the simple-vector ,vector) (the fixnum ,index))))

(defmacro svlength (vector)
  `(length (the simple-vector ,vector)))

(defmacro svposition (thing vector)
  `(position ,thing (the simple-vector ,vector)))

(defmacro abs& (n)
  `(the fixnum
        (if (minusp& ,n)
            (- (the fixnum ,n))
            ,n)))

(defconstant-folding-macro max& (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'max newargs)
        (case (length newargs)
          (0 (warn "Better Watch Out, ~A needs at least 1 arg" 'max&)
           '(error "~A needs at least 1 arg" 'max&))
          (1 (first newargs))
          (2 `(the fixnum (max (the fixnum ,(first newargs))
                               (the fixnum ,(second newargs)))))
          (t `(the fixnum
                   (max (the fixnum ,(first newargs))
                        (max& ,@(rest newargs)))))))))

(defconstant-folding-macro min& (&rest args)
  (multiple-value-bind (newargs constant?)
      (replace-constant-vars args)
    (if (not (null constant?))
        (apply #'min newargs)
        (case (length newargs)
          (0 (warn "Better Watch Out, ~A needs at least 1 arg" 'min&)
           '(error "~A needs at least 1 arg" 'min&))
          (1 (first newargs))
          (2 `(the fixnum (min (the fixnum ,(first newargs))
                               (the fixnum ,(second newargs)))))
          (t `(the fixnum
                   (min (the fixnum ,(first newargs))
                        (min& ,@(rest newargs)))))))))

;;; Lucid's 1+ doesn't follow FIXNUM declarations, which
;;; the Lucid's DOTIMES is always (but illegally) inserting.
;;; We need a DOTIMES which uses a working 1+&.
(defmacro dotimes& ((iteration-var times &optional return-value) &body body)
  (let ((limit-var (gensym)))
    `(let ((,limit-var ,times))
       (do ((,iteration-var 0 (1+& ,iteration-var)))
           ((=& ,iteration-var ,limit-var) ,return-value)
        . ,body))))

;;;; byte diddling...
;;; Ideally these should expand into machine instructions
;;; for any particular implementation

(defmacro ldb& (bytespec integer)
  `(the fixnum (ldb ,bytespec (the fixnum ,integer))))

(defmacro dpb& (newbyte bytespec integer)
  `(the fixnum (dpb (the fixnum ,newbyte) ,bytespec (the fixnum ,integer))))

(defmacro ash& (a b)
  `(the fixnum (ash (the fixnum ,a) (the fixnum ,b))))

(defmacro logior& (a b)
  `(logior (the fixnum ,a) (the fixnum ,b)))

(defmacro logand& (a b)
  `(logand (the fixnum ,a) (the fixnum ,b)))

(defmacro logxor& (a b)
  `(logxor (the fixnum ,a) (the fixnum ,b)))

(defmacro lognand& (a b)
  `(lognand (the fixnum ,a) (the fixnum ,b)))

(defmacro lognor& (a b)
  `(lognor (the fixnum ,a) (the fixnum ,b)))

(defmacro logeqv& (a b)
  `(logeqv (the fixnum ,a) (the fixnum ,b)))

;;; I wonder if these will be useful...
;;; Perhaps in lucid when compiling for the 68881

(defmacro float-plus (&rest args)
  (case (length args)
    (0 '0)
    (1 (first args))
    (2 `(the float (+ (the float ,(first args))
                      (the float ,(second args)))))
    (t `(the float
             (+ (the float ,(first args))
                (float-plus ,@(rest args)))))))

(defmacro float-minus (&rest args)
  (case (length args)
    (0 '0)
    (1 `(- (the float ,(first args))))
    (2 `(the float (- (the float ,(first args))
                      (the float ,(second args)))))
    (t `(the float
             (- (the float ,(first args))
                (float-minus ,@(rest args)))))))

(defmacro float-times (&rest args)
  (case (length args)
    (0 '0)
    (1 (first args))
    (2 `(the float (* (the float ,(first args))
                      (the float ,(second args)))))
    (t `(the float
             (* (the float ,(first args))
                (float-times ,@(rest args)))))))

;;; Random useful macros.

(defsubst cha? (cha) (characterp cha))

(DEFMACRO BARF (string . args)
  `(ERROR ,string . ,args))

(DEFMACRO NOT-NULL (X)
  `(NOT (NULL ,X)))

(defmacro ensure-list (item . cruft)
  (declare (ignore cruft))
  `(if (and ,item (not (listp ,item))) (setf ,item (list ,item))))

(DEFMACRO LIST-OR-LISTIFY (ITEM)
  `(IF (NOT (LISTP ,ITEM)) (LIST ,ITEM) ,ITEM))


;; COLLECT is straight from the book, and is documented there.

(DEFMACRO WITH-COLLECTION (&BODY BODY)
  (LET ((VAR (GENSYM)))
    `(LET ((,VAR NIL))
       (MACROLET ((COLLECT (ARGUMENT)
                    `(PUSH ,ARGUMENT ,',VAR)))
         . ,BODY)
       (NREVERSE ,VAR))))

;; and to catch unbounded COLLECTs
;; There seems to be some compiler lossage with multiple passes or
;; something that makes this warning lose.
(DEFMACRO COLLECT (ARGUMENT)
  ;; (WARN "~S Used Outside of ~S" 'COLLECT 'WITH-COLLECTION)
  `(ERROR "~S Used Outside of ~S" `(COLLECT ,,ARGUMENT) 'WITH-COLLECTION))

;; SUM is just like COLLECT

(DEFMACRO WITH-SUMMATION (&BODY BODY)
  (LET ((VAR (GENSYM)))
    `(LET ((,VAR 0))
       (MACROLET ((SUM (ARGUMENT)
                    `(INCF ,',VAR ,ARGUMENT)))
         . ,BODY)
       ,VAR)))

;; To catch errors...
(DEFMACRO SUM (ARGUMENT)
  ;;  (WARN "~S Used Outside of ~S" 'SUM 'WITH-SUMMATION)
  `(ERROR "~S Used Outside of ~S" `(SUM ,,ARGUMENT) 'WITH-SUMMATION))


;; BETWEEN
(DEFMACRO BETWEEN? (X A B)
  `(OR (AND (> ,X ,A) (< ,X ,B))
       (AND (< ,X ,A) (> ,X ,B))))

;; We may be forced to use >= and <= (sigh.)
(DEFMACRO INCLUSIVE-BETWEEN? (X A B)
  `(OR (AND (>= ,X ,A) (<= ,X ,B))
       (AND (<= ,X ,A) (>= ,X ,B))))

;;;; Stack Consing...
(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements))) ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST* (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (APPLY #'LIST* (MAPCAR #'EVAL '(exp1 ... expN))))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list* ,@elements))) ,@body))

;;;; ZetaLispy things

(defmacro neq (x y)
  `(not (eq ,x ,y)))

;; looping sleep, no scheduling
;; top version assumes max (get-internal-real-time) is a fixnum
(defun snooze (seconds)
  (let* ((internal-duration (fix (* seconds internal-time-units-per-second)))
         (end (+ (get-internal-real-time) internal-duration)))
    (loop (when (>= (get-internal-real-time) end) (return nil)))))

(DEFUN SPACE-OUT (TIME-IN-60THS)
  (SLEEP (/ TIME-IN-60THS 60.)))

(defmacro old-functionp (thing)
  `(or (functionp ,thing)
       (and (symbolp ,thing)
            (functionp (symbol-function ,thing)))))

(defconstant +degs->rads+ (/ pi 180.0))

;; Trig: double precision
;; See the-attic for anecdotes about the previous implementations of "Trig in Degrees"
(defun sind (x) (sin (float-times (float x) +degs->rads+)))

(defun cosd (x) (cos (float-times (float x) +degs->rads+)))

(DEFMACRO SPLICE-LIST-ONTO-LIST (ONTO-LIST LIST)
  `(SETF ,ONTO-LIST (NCONC ,ONTO-LIST ,LIST)))

(DEFMACRO SPLICE-ITEM-ONTO-LIST (ONTO-LIST ITEM)
  `(SPLICE-LIST-ONTO-LIST ,ONTO-LIST `(,,ITEM)))

;;;
;;; Time Utilities
;;;
(defun current-time-milliseconds ()
  (* (/ (get-internal-real-time) internal-time-units-per-second) 1000))

(defmacro simple-wait-with-timeout (timeout function &rest args)
  `(let* ((initial-time (get-internal-real-time))
          (end-time (+ (round (* internal-time-units-per-second ,timeout))
                       initial-time)))
     (do ((result (funcall ,function . ,args) (funcall ,function . ,args))
          (current-time (get-internal-real-time) (get-internal-real-time)))
         ((or (not (null result)) (> current-time end-time))
          result)
       )))

(defmacro wait-with-timeout (wstring timeout function &rest args)
  `(progn
     (unless (null ,wstring) (status-line-display 'timeout ,wstring))
     (prog1
         (simple-wait-with-timeout ,timeout ,function . ,args)
       (unless (null ,wstring) (status-line-undisplay 'timeout)))))

;; time should be in internal-time-units to avoid floating pt inside the loop
(defmacro timed-body ((time) &body body)
  `(let ((end-time (+ (get-internal-real-time) ,time)))
     (prog1 (progn . ,body)
       (do ((current-time (get-internal-real-time) (get-internal-real-time)))
           ((>= current-time end-time))
         ))))

(defmacro symbol-format (stream string &rest args)
  `(let ((*print-case* :upcase))
     (format ,stream ,string ,@args)))

;;;; keyboard input event selectors

;; for keyboard events which are characters, we can use the usual
;; character selectors.  MCL characters do not include modifier bits
;; which makes them unsuitable for encoding inut keyboard events
;; instead, we slam the event modifier bits into the top word
;; of the event message bits
(defun key-event? (event) (or (characterp event) (numberp event)))

(defun input-code (key-event)
  (if (numberp key-event) key-event (char-code key-event)))

;;; INPUT-BITS should return 1 for :control, 2 for :meta
(defun input-bits (key-event)
  (char-code key-event))
;; sgithens TODO (lispworks:char-bits key-event))

;; #+lispworks
;; (defun make-char (char &optional (bits 0) font)
;;   (declare (ignore font))
;;   (lispworks:make-char char bits))

(defun make-char (cc &optional (bits 0) (font 0))
  "CC must be a character, BITS and FONT must be non-negative
integers; the last two are optional with default values of 0.
Returns a character object which is the same as CC but with the
attributes given, or NIL if this is not possible."
  (if (and (zerop bits) (zerop font))
      cc
      nil))

;;; lisp error handling

(defmacro with-lisp-error-reporting (&body body)
  `(ignore-errors (progn . ,body)))

;; useful for networking dispatch

;; use string-equal for comparison
;; should have more syntax checking...
(defmacro string-case (key &rest clauses)
  (list* 'cond (mapcar #'(lambda (clause)
                           (cond ((eq (car clause) 'otherwise)
                                  (list* t (cdr clause)))
                                 ((eq (car clause) 't) clause)
                                 ((stringp (car clause))
                                  (list* `(string-equal ,key ,(car clause))
                                         (cdr clause)))
                                 ((listp (car clause))
                                  (list* `(member ,key ',(car clause)
                                                  :test #'string-equal)
                                         (cdr clause)))))
                       clauses)))

;;; http://www.sbcl.org/manual/#Idiosyncrasies
;;; Many of the constants in boxer fail the eql test used by defconstant
;;; and trigger errors in SBCL
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
