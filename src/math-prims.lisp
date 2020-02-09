;;; -*- Package: EVAL; Mode: LISP; Base: 10; Syntax: Common-lisp -*-
#|

 $Header: math-prims.lisp,v 1.0 90/01/24 22:14:43 boxer Exp $

 $Log:	math-prims.lisp,v $
;;;Revision 1.0  90/01/24  22:14:43  boxer
;;;Initial revision
;;;


        Copyright 1987 - 1998 Regents of the University of California

     Enhancements and Modifications Copyright 1998 - 2005 Pyxisystems LLC


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+



Modification History (most recent at top)

 3/11/05 added =/= and <> as infix prims for not equal
 4/19/03 merged current LW and MCL sources
 3/06/02 can't use integer sqrt when arg is negative sqrt 
 1/24/02 sqrt changed to try extra hard to preserve integerness for non mac
 9/19/01 RANDOM changed back to allow flonum values
 2/05/01 random utility prims
 2/05/01 Started Logging changes: source = boxer version 2.5+


|#
;;;

#-(or lispworks mcl LISPM) (in-package 'eval :use '(lisp sm))
#+(or lispworks mcl)       (in-package :eval)

;;;
;;; Infix arithmetic and numeric relational operators
;;;
(DEFBOXER-PRIMITIVE (bu::+ 6 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (+ A B))

(DEFBOXER-PRIMITIVE (BU::- 6 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (- A B))

(DEFBOXER-PRIMITIVE (BU::* 7 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (* A B))

(DEFBOXER-PRIMITIVE (BU::/ 7 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (if (zerop b)
      (primitive-signal-error :math-error "Dividing by Zero")
      (/ A B)))

(DEFBOXER-PRIMITIVE (BU::< 2 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (< A B)))

;; this is generic, use bu::#= for speed
(defboxer-primitive (bu::= 2 infix) ((dont-copy b1) (dont-copy b2))
  (boxer-boolean (boxer::box-equal-internal b1 b2)))

(defboxer-primitive (bu::#= 2 infix) ((numberize a) (numberize b))
  (boxer-boolean (= a b)))

;; when we figure out the extended character situation we can use
;; option= on the mac as well
(defboxer-primitive (bu::=/= 2 infix) ((dont-copy b1) (dont-copy b2))
  (boxer-boolean (not (boxer::box-equal-internal b1 b2))))

(defboxer-primitive (bu::<> 2 infix) ((dont-copy b1) (dont-copy b2))
  (boxer-boolean (not (boxer::box-equal-internal b1 b2))))


(DEFBOXER-PRIMITIVE (BU::> 2 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (> A B)))

(DEFBOXER-PRIMITIVE (BU::<= 2 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (<= A B)))

(DEFBOXER-PRIMITIVE (BU::>= 2 INFIX) ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (>= A B)))

(DEFBOXER-PRIMITIVE BU::NEGATIVE ((NUMBERIZE A))
  (- A))

;prefix bu::- is converted into this primitive.
(DEFBOXER-PRIMITIVE %NEGATIVE-INTERNAL ((NUMBERIZE A))
  (- A))

;;;
;;; Prefix arithmetic operators
;;;
(DEFBOXER-PRIMITIVE BU::PLUS ((NUMBERIZE A) (NUMBERIZE B))
  (+ A B))

(DEFBOXER-PRIMITIVE BU::MINUS ((NUMBERIZE A) (NUMBERIZE B))
  (- A B))

(DEFBOXER-PRIMITIVE BU::TIMES ((NUMBERIZE A) (NUMBERIZE B))
  (* A B))

(DEFBOXER-PRIMITIVE BU::DIVIDE ((NUMBERIZE A) (NUMBERIZE B))
  (if (zerop b)
      (primitive-signal-error :math-error "Dividing by Zero")
      (/ A B)))

;;;
;;; Predicates
;;;

; actually defined in dataprims.lisp
;(defboxer-primitive bu::equal? ((dont-copy b1) (dont-copy b2))
;  (boxer-boolean (boxer::box-equal-internal b1 b2)))

(defboxer-primitive bu::number-equal? ((numberize a) (numberize b))
  (boxer-boolean (= a b)))

(DEFBOXER-PRIMITIVE bu::less? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (< A B)))

;; no equal yet
(DEFBOXER-PRIMITIVE bu::less-or-equal? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (<= A B)))

(DEFBOXER-PRIMITIVE bu::greater? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (> A B)))

(DEFBOXER-PRIMITIVE bu::greater-or-equal? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (>= A B)))

;;; 

(defboxer-primitive bu::=? ((dont-copy b1) (dont-copy b2))
  (boxer-boolean (boxer::box-equal-internal b1 b2)))

(defboxer-primitive bu::#=? ((numberize a) (numberize b))
  (boxer-boolean (= a b)))

(DEFBOXER-PRIMITIVE bu::<? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (< A B)))

;; no equal yet
(DEFBOXER-PRIMITIVE bu::<=? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (<= A B)))

(DEFBOXER-PRIMITIVE bu::>? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (> A B)))

(DEFBOXER-PRIMITIVE bu::>=? ((NUMBERIZE A) (NUMBERIZE B))
  (BOXER-BOOLEAN (>= A B)))

;;; Other numeric predicates
(defboxer-primitive bu::minus? ((numberize number))
  (boxer-boolean (minusp number)))

(defboxer-primitive bu::plus? ((numberize number))
  (boxer-boolean (plusp number)))

(defboxer-primitive bu::zero? ((numberize number))
  (boxer-boolean (zerop number)))

(defboxer-primitive bu::even? ((numberize number))
  (boxer-boolean (and (integerp number) (evenp number))))

(defboxer-primitive bu::odd? ((numberize number))
  (boxer-boolean (and (integerp number) (oddp number))))

;;this and the numberize stuff is a crock and should be re-done
(defboxer-primitive bu::number? ((dont-copy number-or-box))
  (boxer-boolean (boxer::numberize-or-nil number-or-box)))

(defboxer-primitive bu::integer? ((numberize number))
  (boxer-boolean (integerp number)))

;(defboxer-primitive bu::integer? ((numberize number))
;  (boxer-boolean (or (integerp number)
;		     (zerop (multiple-value-bind (int flo)
;				(floor number)
;			      flo)))))

(defboxer-primitive bu::rational? ((numberize number))
  (boxer-boolean (rationalp number)))

(defboxer-primitive bu::float? ((numberize number))
  (boxer-boolean (floatp number)))

(defboxer-primitive bu::real? ((numberize number))
  (boxer-boolean (not (complexp number))))

(defboxer-primitive bu::complex? ((numberize number))
  (boxer-boolean (complexp number)))

;;;
;;; Rational number functions
;;;
(defboxer-primitive bu::numerator ((numberize rational))
  (numerator rational))

(defboxer-primitive bu::denominator ((numberize rational))
  (denominator rational))

(defboxer-primitive bu::rationalize ((numberize number))
  (rationalize number))

;;;
;;; Good-old math functions
;;; 

(DEFBOXER-PRIMITIVE BU::REMAINDER ((NUMBERIZE NUMBER) (NUMBERIZE MODULUS))
  (if (zerop modulus)
      (primitive-signal-error :math-error "Dividing by Zero")
      ;;? do we want mod instead?? they differ only on floats
      (rem NUMBER MODULUS)))

(DEFBOXER-PRIMITIVE BU::mod ((NUMBERIZE NUMBER) (NUMBERIZE MODULUS))
  (if (zerop modulus)
      (primitive-signal-error :math-error "Dividing by Zero")
      (mod NUMBER MODULUS)))

(defboxer-primitive bu::abs ((numberize number))
  (abs number))

(defboxer-primitive bu::signum ((numberize number))
  (signum number))

(defboxer-primitive bu::max ((numberize a) (numberize b))
  (max a b))

(defboxer-primitive bu::min ((numberize a) (numberize b))
  (min a b))



;;;
;;; The Real->Integer conversion functions
;;;
(defboxer-primitive bu::floor ((numberize number))
  (fix number))

(defboxer-primitive bu::ceiling ((numberize number))
  (values (ceiling number)))

(defboxer-primitive bu::truncate ((numberize number))
  (values (truncate number)))

(defboxer-primitive bu::round ((numberize number))
  (values (round number)))

;;;
;;; Random-number primitives
;;;
(defboxer-primitive BU::RANDOM ((numberize n))
  ; removed 9/19/01 seems flonums have a useful meaning
  ;(when (not (integerp n)) (setq n (round n)))
  (cond	((zerop n) 0)
	((plusp n) (random n))
	(t (primitive-signal-error :math-error
				   "Random of a negative number"))))

(defvar *copied-random-state* (make-random-state))

(defboxer-primitive bu::copy-random-state ()
  (setq *copied-random-state* (make-random-state))
  *novalue*)

(defboxer-primitive bu::reset-random-state ()
  (setq *random-state* (make-random-state *copied-random-state*))
  *novalue*)

(defboxer-primitive bu::new-random-state ()
  (setq *random-state* (make-random-state T))
  *novalue*)

;;;
;;; Transcendental functions and that ilk
;;;
(defboxer-primitive bu::log ((numberize number))
  (if (zerop number)
      (primitive-signal-error :math-error "Log of 0")
      (log number 10)))

(defboxer-primitive bu::ln ((numberize number))
  (if (zerop number)
      (primitive-signal-error :math-error "Log of 0")
      (log number)))

(defboxer-primitive bu::power ((numberize number) (numberize exponent))
  (expt number exponent))

(defboxer-primitive (bu::** 8 infix) ((numberize number) (numberize exponent))
  (expt number exponent))

(defboxer-primitive bu::exp ((numberize number))
  (exp number))

;;; Trig functions
(defboxer-primitive bu::sin ((numberize angle))
  (let ((normalized-angle (if (minusp angle) (- (mod (- angle) 360))
			      (mod angle 360))))
    (cond ((= (abs normalized-angle) 180) 0)
	  (t (boxer::sind normalized-angle)))))

(defboxer-primitive bu::cos ((numberize angle))
  (let ((normalized-angle (if (minusp angle) (- (mod (- angle) 360))
			      (mod angle 360))))
    (cond ((= (abs normalized-angle)  90) 0)
	  ((= (abs normalized-angle) 270) 0)
	  (t (boxer::cosd normalized-angle)))))

(defboxer-primitive bu::tan ((numberize angle))
  (tan (* angle (/ pi 180))))

(DEFBOXER-PRIMITIVE BU::ASIN ((NUMBERIZE Y))
  (/ (* 180 (ASIN Y)) pi))

(DEFBOXER-PRIMITIVE BU::ACOS ((NUMBERIZE X))
  (/ (* 180 (ACOS X)) pi))

(defboxer-primitive BU::ATAN ((numberize y) (numberize x))
  (if (and (zerop y) (zerop x))
      (primitive-signal-error :math-error "Args out of domain")
      (/ (* 180 (atan y x)) pi)))


(eval-when (eval load)
  (eval::boxer-toplevel-set 'bu::pi pi)
  )

;;; sqrt/integer approximation
;;; Try and return integers whenever we can (mac does this already)
(DEFBOXER-PRIMITIVE BU::SQRT ((NUMBERIZE NUMBER))
  #+mcl (SQRT NUMBER)
  #-mcl (cond ((and (integerp number) (not (minusp number)))
               (let ((fsqrt (sqrt  number))
                     (isqrt (isqrt number)))
                 (cond ((= fsqrt isqrt) isqrt) (t fsqrt))))
              (t (sqrt number))))


;;;
;;; Complex number functions
;;;
(defboxer-primitive bu::complex ((numberize real) (numberize imag))
  (complex real imag))

(defboxer-primitive bu::realpart ((numberize number))
  (realpart number))

(defboxer-primitive bu::imagpart ((numberize number))
  (imagpart number))

(defboxer-primitive bu::phase ((numberize number))
  (phase number))

(defboxer-primitive bu::magnitude ((numberize number))
  (abs number))

(defboxer-primitive bu::conjugate ((numberize number))
  (conjugate number))

(defboxer-primitive bu::cis ((numberize number))
  (cis number))
