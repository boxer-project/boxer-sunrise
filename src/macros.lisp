;; -*- Mode:LISP; Syntax:Common-Lisp; Package:(BOXER :USE (LISP) :NICKNAMES (BOX)) -*-
#|


 $Header: macros.lisp,v 1.0 90/01/24 22:14:20 boxer Exp $

 $Log:	macros.lisp,v $
;;;Revision 1.0  90/01/24  22:14:20  boxer
;;;Initial revision
;;;










    Copyright 1987 - 1998 Regents of the University of California

    Additional Portions Copyright 1998 - 2013 PyxiSystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


       This file contains the useful macros for boxer



Modification History (most recent at top)

 3/ 7/13 removed fixnum arithmetic from snooze.  MacOS time can be larger than fixnums
 2/10/03 merged current mac & PC source
 4/17/02 added char-case macro
 4/08/02 added string-case macro
 3/27/00 smarter LWWIN version of input-code 
 8/03/99 added lwwin version of key-event?
 4/10/99 added constant *degs->rads* = (/ pi 180.0) (can't believe this wasn't
         already done !!)
 2/14/99 added lispworks version of input-bits, make-char
 1/08/99 added compiler-let for for Lispworks
10/25/98 Additions for Harlequin Lispworks for Windows 4.1
 5/27/98 added with-lisp-error-reporting
 5/27/98 Started Logging Changes: source = Boxer version 2.3 alphaR1


|#

#-(or mcl lispm lispworks) 
(in-package 'boxer :use '(lisp) :nicknames '(box))

#+(or mcl lispworks)
(in-package :box)



;;;; Macros to avoid using Common Lisp declarations

;;;; Things that ought to be done right in Come-on Lisp but aren't.
(defmacro fast-assq (thing place)
#+3600      `(zl:assq ,thing ,place)
#+MCL       `(ccl:assq ,thing ,place)
#+lispworks `(sys:assq ,thing ,place)
#-(or 3600 mcl lispworks)  (cond ((fboundp 'user::assq) `(user::assq ,thing ,place))
                                 (t `(assoc ,thing ,place :test #'eq))))

(defmacro fast-delq (thing place)
#+3600      `(zl:delq ,thing ,place)
#+MCL       `(ccl:delq ,thing ,place)
#+lispworks `(sys:delq ,thing ,place)
#-(or 3600 mcl lispworks)  (cond ((fboundp 'user::delq) `(user::delq ,thing ,place))
                                 (t `(delete ,thing ,place :test #'eql))))

(defmacro fast-del-if (fun place)
  #+3600 `(zl:del-if ,fun ,place)
  #-3600 (cond #-MCL ((fboundp 'user::del-if) `(user::del-if ,fun ,place))
               (t `(delete-if ,fun ,place))))

(defmacro fast-memq (thing place)
#+3600      `(zl:memq ,thing ,place)
#+MCL       `(ccl:memq ,thing ,place)
#+lispworks `(sys:memq ,thing ,place)
#-(or 3600 mcl lispworks)  (cond ((fboundp 'user::memq) `(user::memq ,thing ,place))
                                 (t `(member ,thing ,place))))



;;; Arithmetic

;;; single floats are sufficient for most of the things we
;;; need to do, unfortunately, different implementations require
;;; different types of declarations so...
(deftype boxer-float ()
  #+lucid 'float ; lucid only has 1 floating pt representation
  #+excl 'single-float
  #+symbolics 'single-float
  #-(or lucid excl symbolics) 'float)


;;; lcl::fix and lcl::fixr are not supported.

;;;#+Lucid (defmacro fix  (a) `(lcl::fix  ,a))
;;;#+Lucid (defmacro fixr (a) `(lcl::fixr ,a))
(defmacro fix  (a) `(values (floor ,a)))
(defmacro fixr (a) `(values (truncate ,a)))

(defvar *constant-folding-macros* nil)
(defvar *constant-folding-macro-bindings* nil)

(defmacro constant-let* (varlist &body body)
  (if (null varlist) `(progn . ,body)
      `(constant-let (,(car varlist))
	 (constant-let* ,(cdr varlist)
	   . ,body))))

; +++ a portable replacement should be found now that compiler-let is no longer in the language
#+MCL
(defmacro compiler-let (let-forms &body body)
  `(ccl:compiler-let ,let-forms ,@body))

#+lispworks
(defmacro compiler-let (let-forms &body body)
  `(lispworks:compiler-let ,let-forms ,@body))

(defmacro constant-let (varlist &body body)
  (let ((new-varlist
	  (mapcar #'(lambda (pair) (list (car pair) (simplify-arg (cadr pair))))
		  varlist)))
    `(compiler-let ((*constant-folding-macro-bindings*
                     ',(append *constant-folding-macro-bindings* new-varlist)))
       (let ,new-varlist . ,body))))

(defmacro defconstant-folding-macro (name args &body body)
  (unless (fast-memq name *constant-folding-macros*)
    (push name *constant-folding-macros*))
  `(defmacro ,name ,args . ,body))

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
      #-lispm
      `(the fixnum (incf (the fixnum ,arg) (the fixnum ,amount)))
      #+lispm
      `(the fixnum (setf ,arg (the fixnum (+ (the fixnum ,arg)
					     (the fixnum ,amount)))))
      #-lispm
      `(the fixnum (incf (the fixnum ,arg)))
      #+lispm
      `(the fixnum (setf ,arg (the fixnum (+ (the fixnum ,arg) 1))))))

(defmacro decf& (arg &optional (amount nil supplied?))
  (if supplied?
      #-lispm
      `(the fixnum (decf (the fixnum ,arg) (the fixnum ,amount)))
      #+lispm
      `(the fixnum (setf ,arg (the fixnum (- (the fixnum ,arg)
					     (the fixnum ,amount)))))
      #-lispm
      `(the fixnum (decf (the fixnum ,arg)))
      #+lispm
      `(the fixnum (setf ,arg (the fixnum (- (the fixnum ,arg) 1))))))

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
  
#-mcl
(defmacro svref& (vector index)
  `(svref (the simple-vector ,vector) (the fixnum ,index)))

#+mcl  ; extra juice
(defsubst svref& (vector index)
  (declare (optimize (speed 3) (safety 0)))
  (svref (the simple-vector vector) (the fixnum index)))

#+mcl
(defsetf svref& set-svref&)

#+mcl
(defsubst set-svref& (vector index new-val)
  (declare (optimize (speed 3) (safety 0)))
  (setf (svref (the simple-vector vector) (the fixnum index))
        new-val))

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

#+Lucid 
(defmacro ldb& (bytespec integer)
  `(lucid::ldb& ,bytespec ,integer))

#-Lucid
(defmacro ldb& (bytespec integer)
  `(the fixnum (ldb ,bytespec (the fixnum ,integer))))

#+Lucid
(defmacro dpb& (newbyte bytespec integer)
  `(lucid::dpb& ,newbyte ,bytespec ,integer))

#-Lucid
(defmacro dpb& (newbyte bytespec integer)
  `(the fixnum (dpb (the fixnum ,newbyte) ,bytespec (the fixnum ,integer))))

#+Lucid
(defmacro ash& (a b)
  `(lucid::ash& ,a ,b))

#-Lucid
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

;; for LispMs and lucid, we just import the system version in pkg.lisp

#+excl
(defmacro defsubst (name arglist &rest body)
  `(progn (defun ,name ,arglist ,@body)
          (defun (:property ,name comp::.compiler-macro.) (macroarg env)
            (sublis (mapcar 'cons ',arglist (cdr macroarg))
                   '(locally ,@body)))))

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
#+lispm
(defmacro with-stack-list ((var &rest elements) &body body)
  `(sys:with-stack-list (,var ,@elements) ,@body))

#+lispm
(defmacro with-stack-list* ((var &rest elements) &body body)
  `(sys:with-stack-list* (,var ,@elements) ,@body))

#+lcl3.0
(defmacro with-stack-list ((var &rest elements) &body body)
  `(let ((,var (list ,@elements)))
     (declare (user::dynamic-extent ,var))
     ,@body))

#+lcl3.0
(defmacro with-stack-list* ((var &rest elements) &body body)
  `(let ((,var (list* ,@elements)))
     (declare (user::dynamic-extent ,var))
     ,@body))

#-(or lispm lcl3.0)
(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements))) ,@body))

#-(or lispm lcl3.0)
(defmacro with-stack-list* ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST* (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (APPLY #'LIST* (MAPCAR #'EVAL '(exp1 ... expN))))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list* ,@elements))) ,@body))



;;;; ZetaLispy things

#-mcl
(defmacro neq (x y)
  `(not (eq ,x ,y)))

;; sleep trashes the scaling (clipping?) state
#+ccl-3
(shadow "SLEEP" (find-package 'boxer))

#+ccl-3
(defun boxer::sleep (seconds)
  (ccl::sleep seconds)
  (window-system-dependent-set-origin %origin-x-offset %origin-y-offset))
  
;; looping sleep, no scheduling
;; top version assumes max (get-internal-real-time) is a fixnum
#-mcl
(defun snooze (seconds)
  (let* ((internal-duration (fix (* seconds internal-time-units-per-second)))
         (end (+ (get-internal-real-time) internal-duration)))
    (loop (when (>= (get-internal-real-time) end) (return nil)))))

#+mcl
(defun snooze (seconds)
  (let* ((internal-duration (fix (* seconds internal-time-units-per-second)))
         (end (+ (get-internal-real-time) internal-duration)))
    (loop (when (>= (get-internal-real-time) end) (return nil)))))

(DEFUN SPACE-OUT (TIME-IN-60THS)
  #+LISPM (SI:PROCESS-SLEEP TIME-IN-60THS)
  #+X (progn time-in-60ths (xlib::xflush))
  #+clx (progn time-in-60ths (bw::display-force-output bw::*display*))
  #-(OR X clx LISPM) (SLEEP (/ TIME-IN-60THS 60.)))

(defmacro old-functionp (thing)
  `(or (functionp ,thing)
       (and (symbolp ,thing)
            (functionp (symbol-function ,thing)))))

#|
;; This is here because MULTIPLE-VALUE-SETQ expands into ZL:MULTIPLE-VALUE 
;; which is a special form that the PCL walker can't currently handle
#+(or SYMBOLICS excl)
(SHADOW 'BOXER::MULTIPLE-VALUE-SETQ)
#+(or SYMBOLICS excl)
(DEFMACRO MULTIPLE-VALUE-SETQ (VARS FORM)
  (LET ((NEW-VALS (MAPCAR #'(LAMBDA (X) (GENSYM)) VARS))
	(SETSEXPRS NIL))
    `(MULTIPLE-VALUE-BIND ,NEW-VALS
	 ,FORM
       ,@(DOTIMES (N (LENGTH VARS) (NREVERSE SETSEXPRS))
	    (PUSH (LIST 'SETQ (NTH N VARS) (NTH N NEW-VALS)) SETSEXPRS)))))
|#


;;; Trig in Degrees
;; There are 3 flavors of trig here, use the one which works best in 
;; your implementation.  The three flavors are:
;;  . double precision floating point.  Seems to work best when there
;;    is good hardware support for IEEE floating point and also
;;    in implementations which do not carry out single precision
;;    arithmetic (like lucid's)
;;
;;  . single precision floating point.  Can be faster is the implementation
;;    actually suports it.  As opposed to pretending to support it but
;;    really doing double precision and then coercing
;;
;;  . Table Lookup + linear interpolation.  Useful for implementations
;;    which lack fast trig functions
;;

(defconstant *degs->rads* (/ pi 180.0))

#-(or lispm tltrig)
(progn					; double precision
  (defun sind (x) (sin (float-times (float x) *degs->rads*)))

  (defun cosd (x) (cos (float-times (float x) *degs->rads*)))
  )

#+lispm
(progn					; single precision
  (defconstant *simple-pi* (coerce pi 'single-float))

  (defun sind (x) (sin (* x (/ *simple-pi* 180.))))

  (defun cosd (x) (cos (* x (/ *simple-pi* 180.))))
  )

#+tltrig
(progn					; table lookup + linear interpolation
;;; we could phase shift instead of having 2 tables
;;; but lets not get TOO complicated unless we have to
  (defun make-sin-table ()
    (let ((table (make-array 360 :element-type 'double-float)))
      (dotimes (i 360)
	(setf (aref table i) (sin (* i *degs->rads*))))
      table))

  (defun make-cos-table ()
    (let ((table (make-array 360 :element-type 'double-float)))
      (dotimes (i 360)
	(setf (aref table i) (cos (* i *degs->rads*))))
      table))

  (defvar *sin-lookup-table* (make-sin-table))
  (defvar *cos-lookup-table* (make-cos-table))

  (defmacro sin-table-lookup (idx)
    `(the float (aref (the (simple-array float (360)) *sin-lookup-table*)
		      (the fixnum ,idx))))

  (defmacro cos-table-lookup (idx)
    `(the float (aref (the (simple-array float (360)) *cos-lookup-table*)
		      (the fixnum ,idx))))

  (defun tlsin-float-internal (degrees)
    (declare (float degrees))
    (multiple-value-bind (idx frac)
	(the (values fixnum float) (floor degrees))
      (declare (fixnum idx) (float frac))
      (setq idx (mod idx 360))
      (let ((t1 0.0) (t2 0.0))
	(declare (float t1 t2))
	(setq t1 (sin-table-lookup idx)
	      t2 (sin-table-lookup (1+& idx)))
	;; make t2 hold the difference
	(setq t2 (float-minus t2 t1))
	;; now scale the difference
	(setq t2 (float-times t2 frac))
	;; the answer is...
	(float-plus t1 t2))))

  (defun sind (degrees)
    (if (typep degrees 'fixnum)
	(sin-table-lookup (mod (the fixnum degrees) 360))
	(tlsin-float-internal degrees)))

  (defun tlcos-float-internal (degrees)
    (declare (float degrees))
    (multiple-value-bind (idx frac)
	(the (values fixnum float) (floor degrees))
      (declare (fixnum idx) (float frac))
      (setq idx (mod idx 360))
      (let ((t1 0.0) (t2 0.0))
	(declare (float t1 t2))
	(setq t1 (cos-table-lookup idx)
	      t2 (cos-table-lookup (1+& idx)))
	;; make t2 hold the difference
	(setq t2 (float-minus t2 t1))
	;; now scale the difference
	(setq t2 (float-times t2 frac))
	;; the answer is...
	(float-plus t1 t2))))

  (defun cosd (degrees)
    (if (typep degrees 'fixnum)
	(cos-table-lookup (mod (the fixnum degrees) 360))
	(tlcos-float-internal degrees)))
  )

#|

#+LISPM
(SI:DEFF SUBSET #'SI:SUBSET)
#-LISPM
(DEFUN SUBSET (PRED LIST)
  (WITH-COLLECTION
    (DOLIST (ITEM LIST)
      (WHEN (FUNCALL PRED ITEM)
	(COLLECT ITEM)))))

#+LISPM
(SI:DEFF SUBSET-NOT #'SI:SUBSET-NOT)
#-LISPM
(DEFUN SUBSET-NOT (PRED LIST)
  (WITH-COLLECTION
    (DOLIST (ITEM LIST)
      (UNLESS (FUNCALL PRED ITEM)
	(COLLECT ITEM)))))

|#

#+LISPM (SI:DEFF WITHOUT-INTERRUPTS 'SI:WITHOUT-INTERRUPTS)
#+MCL
(defmacro without-interrupts (&body body)
  `(ccl:without-interrupts ,@body))
#+(and (not lispworks6) lispworks)
(defmacro without-interrupts (&body body)
  `(lispworks:without-interrupts ,@body))
#+lispworks6
(defmacro without-interrupts (&body body)
  `(mp:with-interrupts-blocked ,@body))

#-(or LISPM MCL lispworks)
(DEFMACRO WITHOUT-INTERRUPTS (&BODY BODY)
  `(PROGN ,@BODY))


;;; Lifted from PCL (comments and all) (it is shadowed in Boxsys.lisp)
;;; ONCE-ONLY does the same thing as it does in zetalisp.  I should have just
;;; lifted it from there but I am honest.  Not only that but this one is
;;; written in Common Lisp.  I feel a lot like bootstrapping, or maybe more
;;; like rebuilding Rome.
(defmacro once-only (vars &body body)
  (let ((gensym-var (gensym))
        (run-time-vars (gensym))
        (run-time-vals (gensym))
        (expand-time-val-forms ()))
    (dolist (var vars)
      (push `(if (or (symbolp ,var)
                     (numberp ,var)
                     (and (listp ,var)
			  (member (car ,var) '(quote function))))
                 ,var
                 (let ((,gensym-var (gensym)))
                   (push ,gensym-var ,run-time-vars)
                   (push ,var ,run-time-vals)
                   ,gensym-var))
            expand-time-val-forms))    
    `(let* (,run-time-vars
            ,run-time-vals
            (wrapped-body
              ((lambda ,vars . ,body) . ,(reverse expand-time-val-forms))))
       `((lambda ,(nreverse ,run-time-vars)  ,wrapped-body)
         . ,(nreverse ,run-time-vals)))))



(DEFMACRO SPLICE-LIST-INTO-LIST (INTO-LIST LIST BEFORE-ITEM)
  `(SETF ,INTO-LIST (SPLICE-LIST-INTO-LIST-1 ,INTO-LIST ,LIST ,BEFORE-ITEM)))

(DEFMACRO SPLICE-ITEM-INTO-LIST (INTO-LIST ITEM BEFORE-ITEM)
  `(SETF ,INTO-LIST (SPLICE-LIST-INTO-LIST-1 ,INTO-LIST `(,,ITEM) ,BEFORE-ITEM)))

(DEFUN SPLICE-LIST-INTO-LIST-1 (INTO-LIST LIST BEFORE-ITEM)
  (LET ((BEFORE-ITEM-POSITION (POSITION BEFORE-ITEM INTO-LIST)))
    (COND ((OR (NULL BEFORE-ITEM-POSITION)
	       (=& BEFORE-ITEM-POSITION 0))
	   (NCONC LIST INTO-LIST)
	   LIST)
	  (T
	   (DO* ((TAIL INTO-LIST (CDR TAIL))
		 (NEXT-ITEM (CADR TAIL) (CADR TAIL)))
		((EQ NEXT-ITEM BEFORE-ITEM)
		 (NCONC LIST (CDR TAIL))
		 (RPLACD TAIL LIST)
		 INTO-LIST))))))

(DEFMACRO SPLICE-LIST-ONTO-LIST (ONTO-LIST LIST)
  `(SETF ,ONTO-LIST (NCONC ,ONTO-LIST ,LIST)))

(DEFMACRO SPLICE-ITEM-ONTO-LIST (ONTO-LIST ITEM)
  `(SPLICE-LIST-ONTO-LIST ,ONTO-LIST `(,,ITEM)))

;(DEFMACRO SPLICE-LIST-OUT-OF-LIST (&YOW LOSING-BADLY)) ;doesn't make sense

(DEFMACRO SPLICE-ITEM-OUT-OF-LIST (OUT-OF-LIST ITEM)
  `(SETF ,OUT-OF-LIST (DELETE ,ITEM ,OUT-OF-LIST)))

(DEFMACRO SPLICE-ITEM-AND-TAIL-OUT-OF-LIST (OUT-OF-LIST ITEM)
  `(SETF ,OUT-OF-LIST (SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-1 ,OUT-OF-LIST ,ITEM)))

(DEFUN SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-1 (OUT-OF-LIST ITEM)
  (LET ((ITEM-POSITION (POSITION ITEM OUT-OF-LIST)))
    (COND ((NULL ITEM-POSITION) OUT-OF-LIST)
	  ((=& ITEM-POSITION 0) NIL)
	  (T (RPLACD (NTHCDR (-& ITEM-POSITION 1) OUT-OF-LIST) NIL)
	     OUT-OF-LIST))))

(DEFMACRO SPLICE-BETWEEN-ITEMS-OUT-OF-LIST (LIST FROM-ITEM TO-ITEM)
  `(DO ((FROM-ITEM-PREVIOUS-CONS NIL FROM-ITEM-PREVIOUS-CONS)
	(TO-ITEM-PREVIOUS-CONS NIL TO-ITEM-PREVIOUS-CONS)
	(SCAN ,LIST (CDR SCAN)))
       ((OR (NULL SCAN) (NOT-NULL TO-ITEM-PREVIOUS-CONS))
	(COND ((NULL FROM-ITEM-PREVIOUS-CONS)
	       (SETF ,LIST (CDR TO-ITEM-PREVIOUS-CONS)))
	      (T
	       (RPLACD FROM-ITEM-PREVIOUS-CONS (CDR TO-ITEM-PREVIOUS-CONS))))
	(RPLACD TO-ITEM-PREVIOUS-CONS NIL))
     (COND ((EQ (CADR SCAN) ,FROM-ITEM)
	    (SETQ FROM-ITEM-PREVIOUS-CONS SCAN))
	   ((EQ (CADR SCAN) ,TO-ITEM)
	    (SETQ TO-ITEM-PREVIOUS-CONS SCAN)))))


;;;new list splicing macros that use index numbers...

(DEFMACRO SPLICE-LIST-INTO-LIST-AT (INTO-LIST LIST POSITION)
  `(COND ((zerop& ,POSITION)
	  (SETF ,INTO-LIST (NCONC ,LIST ,INTO-LIST)))
	 ((>=& ,POSITION (LENGTH ,INTO-LIST))
	  (SETF ,INTO-LIST (NCONC ,INTO-LIST ,LIST)))
	 (T (SETF ,INTO-LIST (NCONC (SUBSEQ ,INTO-LIST 0 ,POSITION)
				    ,LIST
				    (NTHCDR ,POSITION ,INTO-LIST))))))

(DEFMACRO SPLICE-ITEM-INTO-LIST-AT (INTO-LIST ITEM POSITION)
  `(SPLICE-LIST-INTO-LIST-AT ,INTO-LIST `(,,ITEM) ,POSITION))

(DEFMACRO SPLICE-ITEM-OUT-OF-LIST-AT (LIST POSITION)
  `(COND ((=& ,POSITION 0)
	  (SETF ,LIST (CDR ,LIST)))
	 ((>=& ,POSITION (LENGTH ,LIST))
	  (SETF ,LIST (BUTLAST ,LIST)))
	 (T (SETF ,LIST (NCONC (SUBSEQ ,LIST 0 ,POSITION)
			       (NTHCDR (+& ,POSITION 1) ,LIST))))))

(DEFMACRO SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-FROM (LIST POSITION)
  `(COND ((>=& ,POSITION (LENGTH ,LIST)))
	 (T (SETF ,LIST (SUBSEQ ,LIST 0 ,POSITION)))))

(DEFMACRO SPLICE-ITEMS-FROM-TO-OUT-OF-LIST (LIST START-POSITION STOP-POSITION)
  `(COND ((>& ,START-POSITION ,STOP-POSITION)
	  (ERROR "The Starting number: ~S is greater than the ending number ~S"
		  ,START-POSITION ,STOP-POSITION))
	 ((>=& ,START-POSITION (LENGTH ,LIST)))
	 ((=& ,START-POSITION ,STOP-POSITION)
	  (SPLICE-ITEM-OUT-OF-LIST-AT ,LIST ,START-POSITION))
	 ((>=& ,STOP-POSITION (LENGTH ,LIST))
	  (SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-FROM ,LIST ,START-POSITION))
	 (T (SETF ,LIST (NCONC (SUBSEQ ,LIST 0 ,START-POSITION)
			       (NTHCDR ,STOP-POSITION ,LIST))))))

(DEFMACRO ITEMS-SPLICED-FROM-TO-FROM-LIST (LIST START-POSITION STOP-POSITION)
  `(COND ((>& ,START-POSITION ,STOP-POSITION)
	  (ERROR "The Starting number: ~S is greater than the ending number ~S"
		  ,START-POSITION ,STOP-POSITION))
	 ((>=& ,START-POSITION (LENGTH ,LIST))
	  '())
	 ((=& ,START-POSITION ,STOP-POSITION)
	  (LIST (NTH ,START-POSITION ,LIST)))
	 ((>=& ,STOP-POSITION (LENGTH ,LIST))
	  (NTHCDR ,START-POSITION ,LIST))
	 (T (SUBSEQ (NTHCDR ,START-POSITION ,LIST)
		    0 (-& ,STOP-POSITION ,START-POSITION)))))


(defmacro simple-wait-with-timeout (timeout function &rest args)
  #+(and lucid (not clx)) ; bad interaction with CLX
  `(lcl::process-wait-with-timeout "Simple Wait" ,timeout ,function . ,args)
  #-(and lucid (not clx))
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


#| defined differently in vrtdef.lisp

;;;; Fast Type checking mechanisms
;; The evaluator assumes that all of the objects with which it has to deal 
;; with will be either a symbol, a number, or a simple vector.  We will also
;; require that the first slot in the array contains a symbol that specifies
;; the type to the evaluator.  This lets us do (svref <thing> 0) in order 
;; to obtain an object's type.

(defmacro fast-eval-doit-box? (thing)
  `(eq (svref ,thing 0) 'doit-box))

(defmacro fast-eval-data-box? (thing)
  `(eq (svref ,thing 0) 'data-box))

(defmacro fast-eval-sprite-box? (thing)
  `(eq (svref ,thing 0) 'sprite-box))

(defmacro fast-eval-port-box? (thing)
  `(eq (svref ,thing 0) 'port-box))

|#

#|
(defmacro fast-editor-doit-box? (thing)
  `(doit-box? ,thing))

(defmacro fast-editor-data-box? (thing)
  `(data-box? ,thing))

(defmacro fast-editor-sprite-box? (thing)
  `(sprite-box? ,thing))

(defmacro fast-editor-port-box? (thing)
  `(port-box? ,thing))
|#


(defmacro symbol-format (stream string &rest args)
  `(let ((*print-case* :upcase))
     (format ,stream ,string ,@args)))

;;; execute something at the listener level. Useful for issuing warnings from
;;; within drawing code, for instance.
#+mcl
(defmacro at-user-level (&body body)
  `(let ((continuation #'(lambda () ,@body)))
     (ccl:eval-enqueue `(funcall ,continuation))))

;;; do nothing in other implementations
#-mcl
(defmacro at-user-level (&body body)
  `(progn ,@body))

;;;; keyboard input event selectors

;; for keyboard events which are characters, we can use the usual
;; character selectors.  MCL characters do not include modifier bits
;; which makes them unsuitable for encoding inut keyboard events
;; instead, we slam the event modifier bits into the top word
;; of the event message bits
#-(or opengl lwwin mcl)
(defun key-event? (event) (characterp event))

#+(or opengl lwwin mcl)
(defun key-event? (event) (or (characterp event) (numberp event)))

#-(or opengl lwwin mcl)
(defun input-code (key-event) (char-code key-event))

#+(or opengl lwwin)
(defun input-code (key-event) 
  (if (numberp key-event) key-event (char-code key-event)))

#+mcl ; special key handling should have already happened in the event handler
(defun input-code (key-event) (ldb #.(byte 9 0) key-event))

#| ; old mcl version
(defun input-code (key-event) 
  (let* ((key (ldb #.(byte 8 8) key-event))
         (special (assoc key *mcl-special-keycodes*)))
    (if special
      (get (cadr special) :mcl-special-char-code)
      (ldb #.(byte 8 0) key-event))))
|#
  
;;; INPUT-BITS should return 1 for :control, 2 for :meta
#-(or mcl lispworks)
(defun input-bits (key-event) (char-bits key-event))

#+lispworks
(defun input-bits (key-event) (lispworks:char-bits key-event))

#+lispworks
(defun make-char (char &optional (bits 0) font)
  (declare (ignore font))
  (lispworks:make-char char bits))

;; Inside Mac, Mac ToolBox Essentials pg 2-126, 2-20
;; modifiers are high byte of high word , so position 24 +
;; cmd=0, shift=1, alphalock=2, option=3, control=4
#+mcl
(defun input-bits (key-event) 
  (+& (*& 2 (ldb #.(byte 1 27) key-event)) ; meta bit
      (if (and (zerop& (ldb #.(byte 1 24) key-event)) ; cmd bit
               (zerop& (ldb #.(byte 1 28) key-event)))
        0 1)))
        
;;; lisp error handling

#+mcl
(defmacro with-lisp-error-reporting (&body body)
  (let ((ctag (gensym)))
    `(catch ',ctag
       (ignore-errors 
        (ccl::handler-bind 
          ((ccl::error #'(lambda (c)
                           (boxer-editor-error "Lisp Error:~A" c)
                           (throw ',ctag nil))))
          (progn . ,body))))))

#-mcl
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

(defmacro char-case (key &rest clauses)
  (list* 'cond (mapcar #'(lambda (clause)
                           (cond ((eq (car clause) 'otherwise)
                                  (list* t (cdr clause)))
                                 ((eq (car clause) 't) clause)
                                 ((characterp (car clause))
                                  (list* `(char-equal ,key ,(car clause))
                                         (cdr clause)))
                                 ((listp (car clause))
                                  (list* `(member ,key ',(car clause) 
                                                  :test #'char-equal)
                                         (cdr clause)))))
                       clauses)))
