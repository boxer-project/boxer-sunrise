;;-*- Mode:Lisp; Syntax: Common-Lisp; Base: 10.;package: Boxer; -*-

#|


 $Header: dataprims.lisp,v 1.0 90/01/24 22:09:19 boxer Exp $


 $Log:	dataprims.lisp,v $
;;;Revision 1.0  90/01/24  22:09:19  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


  This file contains the Data Manipulation Primitives for Boxer


                Selectors

FIRST               FIRST-ROW         FIRST-COLUMN         RC
BUTFIRST            BUTFIRST-ROW      BUTFIRST-COLUMN
LAST                LAST-ROW          LAST-COLUMN
BUTLAST             BUTLAST-ROW       BUTLAST-COLUMN
ITEM                ROW               COLUMN
BUTITEM             BUTROW            BUTCOLUMN
ITEMS               ROWS              COLUMNS


                Predicates

EMPTY?
BOX-EQUAL?  (remember that = is generic)
WORD?
UNBOXABLE?
ALPHA>     ALPHA<

                Information

EMPTY?
LENGTH     NUMBER-OF-ITEMS     COUNT (logo compatibility)
WIDTH      NUMBER-OF-COLUMNS
HEIGHT     NUMBER-OF-ROWS
ITEM-NUMBERS    ROW-NUMBERS   COLUMN-NUMBERS


                Constructors

JOIN-BOTTOM
JOIN-RIGHT
BUILD    (see the newbuild.lisp file)


                Mutators

CHANGE

DELETE-LAST-ITEM    DELETE-LAST-ROW   DELETE-LAST-COLUMN  DELETE-RC
DELETE-ITEM         DELETE-ROW        DELETE-COLUMN
DELETE-ITEMS        DELETE-ROWS       DELETE-COLUMNS

INSERT-RC
INSERT-ITEM         INSERT-ROW        INSERT-COLUMN

APPEND-ITEM         APPEND-ROW        APPEND-COLUMN

CHANGE-LAST-ITEM    CHANGE-LAST-ROW   CHANGE-LAST-COLUMN  CHANGE-RC
CHANGE-ITEM         CHANGE-ROW        CHANGE-COLUMN


                Other

LETTERS      WORD
UNBOX        BOXIFY
RETARGET


Modification History (most recent at top)

12/01/09 implode-row-items uses whitespace-char? instead of char= #\Space
 7/16/04 Fixed Copyright error message for REDIRECT
 6/22/03 fixed typo in bu::retarget
 4/21/03 merged current LW and MCL files
 9/03/02 added RETARGET-INTERNAL, used by RETARGET and UC free REDIRECT
 2/13/01 merged current LW and MCL files
10/29/99 added bu::redirect as a synonym for bu::retarget
 6/02/99 box-equal-internal now hacks possible circular structures
 1/08/99 retarget hacks the case where the point is in the port
 7/05/98 *replace-when-adding-rows-to-empty-box* (default T) added to
         customize behavior of row mutators, append/insert-row
 7/05/98 started logging changes: source = Boxer version 2.3alpha

|#

#-(or lispworks mcl lispm)(in-package 'boxer :nicknames '(box))
#+(or lispworks mcl)      (in-package :boxer)




;;;; Top level Accessors
;;  Accessors should include the following phases:
;;  1) type check--port or box.  Very different behavior for each....
;;  2) prepare active row(s) [only item operations need to do this].
;;     Neccessary mostly for ports. See PREPARE-ROW-FOR-SUB-PORTS.
;;  3) operate on active row(s) to return list(s) of items [only item
;;     operations do this]
;;  4) bundle up returned list(s) of items into new (a) row(s).  This is the
;;     job of PORT-FLAVORED-ROW-CONSTRUCTOR and COPY-FLAVORED-ROW-CONSTRUCTOR.
;;  5) Process all the other rows for selector [see
;;     RETURN-UNTOUCHED-ROW-FOR-PORT-SELECTOR
;;     and RETURN-UNTOUCHED-ROW-FOR-COPY-SELECTOR].
;;  6) Bundle up rows into a box to be returned [What NEW-VC-ROWS does]

(defmacro with-data-prim-number-handling ((arg) number-consequent &body body)
  `(cond ((numberp ,arg)
	  ,number-consequent)
	 (t
	  . ,body)))

(defmacro data-primitive-error (format-string &rest format-args)
  `(eval::primitive-signal-error :data-primitive ,format-string . ,format-args))

;; if there are no new rows, then it makes a single empty row
(defun make-new-box (old-box new-rows)
  (when (null new-rows)
    (setq new-rows (list (make-empty-evrow))))
  (cond ((virtual-copy? old-box)
	 (new-vc-rows old-box new-rows))
	((fast-eval-port-box? old-box)
	 (make-virtual-copy :rows new-rows))
	((box? old-box)
	 (new-vc-rows-from-editor-box old-box new-rows))
	(t (error "~A is not an evaluator object" old-box))))

;;; How to handle Bad numeric args....
;;;

(defun out-of-range-mutator-action (index box &optional (type "items"))
  (if (consp index)
      (eval::primitive-signal-error
       :data-mutator
       "inputs row" (car index)
       "and column" (cdr index)
       "are outside the range of the box" (port-to box))
      (eval::primitive-signal-error
       :data-mutator
       type "input" index "is out of the range of the box" (port-to box))))

(defun out-of-range-access-result (index box &optional (type "index"))
  (if (consp index)
      (eval::primitive-signal-error
       :data-selector
       "inputs row" (car index) "and column" (cdr index)
       "are outside of the range of the box" box)
      (eval::primitive-signal-error
       :data-selector
       type
       index
       "is outside of the range of the box" box)))

(defun out-of-range-filter-result (index box &optional (type "index"))
  (if (consp index)
      (eval::primitive-signal-error
       :data-selector
       "inputs row" (car index) "and column" (cdr index)
       "are outside of the range of the box" box)
      (eval::primitive-signal-error
       :data-selector
       type
       index
       "is outside of the range of the box" box)))




;;; Item Accessors

(defun construct-evrow-from-pointer (ptr &optional target-superior)
  (if (null target-superior)
      (make-evrow-from-pointer ptr)
      (make-evrow-from-port-with-pointer ptr target-superior)))

(defun construct-evrow-from-pointers (ptrs &optional target-superior)
  (if (null target-superior)
      (make-evrow-from-pointers ptrs)
      (make-evrow-from-port-with-pointers ptrs target-superior)))

(defun construct-evrow-from-evrow (evrow &optional target-superior)
  (if (null target-superior)
      evrow
      (make-evrow-from-port-with-pointers (evrow-pointers evrow)
					  target-superior)))

;; this exists in order to avoid having to CONS up a new list of evrows
;; in the virtual-copy case
(defun map-construct-evrow-from-evrow (evrows &optional target-superior)
  (if (null target-superior)
      evrows
      (mapcar #'(lambda (er)
		  (make-evrow-from-port-with-pointers (evrow-pointers er)
						      target-superior))
	      evrows)))

(defun butstart (box)
  (with-data-prim-number-handling (box) (make-empty-vc)
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (make-new-box box
		    (or (let ((target (when (fast-eval-port-box? box)
					(or vc-rows-entry (vp-target box)))))
			  (dolist (row rows)
			    (unless (empty-evrow? row)
			      (return
				(cons (construct-evrow-from-pointers
				       (get-butfirst-element-in-evrow row)
				       target)
				      (map-construct-evrow-from-evrow
				       (cdr (fast-memq row rows)) target))))))
			(list (make-empty-evrow)))))))


(defun %last (box)
  (with-data-prim-number-handling (box) (boxify-number box)
     (multiple-value-bind (row vc-rows-entry)
	 (last-non-empty-row box)
       (make-new-box box
		     (list
		      (cond ((null row) (make-empty-evrow))
			    (t (construct-evrow-from-pointer
				(get-last-element-in-evrow row)
				(when (fast-eval-port-box? box)
				  (or vc-rows-entry (vp-target box)))))))))))

(defun %butlast (box)
  (with-data-prim-number-handling (box) (make-empty-vc)
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (setq rows (reverse rows))
      (make-new-box
       box (or (let ((target (when (fast-eval-port-box? box)
			       (or vc-rows-entry (vp-target box)))))
		 (dolist (row rows)
		   (unless (empty-evrow? row)
		     (return (nreverse
			      (cons
			       ;; the last row minus the last item...
			       (construct-evrow-from-pointers
				(get-butlast-element-in-evrow row) target)
			       ;; the rest of the rows in
			       ;; reverse order
			       (map-construct-evrow-from-evrow
				(cdr (fast-memq row rows)) target)))))))
	       (list (make-empty-evrow)))))))


(defun item (n box)
  (with-data-prim-number-handling (box)
    (if (= 1 n)	box (out-of-range-access-result n box))
    (if (< n 1)
	(out-of-range-access-result n box)
	(multiple-value-bind (row col)
	    (get-row-and-col-number n box)
	  (if (null row)
	      (out-of-range-access-result n box)
	      (multiple-value-bind (active-row vc-rows-entry)
		  (get-nth-row-in-box row box)
		(make-new-box
		 box
		 (list
		  (construct-evrow-from-pointer
		   (get-nth-element-in-evrow active-row col)
		   (when (fast-eval-port-box? box)
		     (or vc-rows-entry (vp-target box))))))))))))

;; start & stop are 1-based
(defun items (start stop box)
  (with-data-prim-number-handling (box)
    (if (= start stop 1) box (out-of-range-access-result start box))
    (cond ((< start 1) (out-of-range-access-result start box))
	  ((< stop  1) (out-of-range-access-result stop  box))
	  (t (multiple-value-bind (start-row start-col)
		 (get-row-and-col-number start box)
	       (multiple-value-bind (stop-row stop-col)
		   (get-row-and-col-number stop box)
		 (cond ((null start-row)(out-of-range-access-result start box))
		       ((null stop-row) (out-of-range-access-result stop box))
		       ((= start-row stop-row)
			(make-new-box
			 box (list (multiple-value-bind (row vc-rows-entry)
				       (get-nth-row-in-box start-row box)
				     (construct-evrow-from-pointers
				      (get-elements-in-evrow row start-col
							     (1+& stop-col))
				      (when (fast-eval-port-box? box)
					(or vc-rows-entry(vp-target box))))))))
		       (t
			(multiple-value-bind (rows inlinks? new? vc-rows-entry)
			    (get-box-rows box)
			  (declare (ignore inlinks? new?))
			  (let ((target (when (fast-eval-port-box? box)
					  (or vc-rows-entry (vp-target box)))))
			    (make-new-box
			     box (nconc
				  ;; partial 1st row
				  (list
				   (construct-evrow-from-pointers
				    (get-elements-in-evrow (nth start-row rows)
							   start-col)
				    target))
				  ;; middle rows
				  (map-construct-evrow-from-evrow
				   (subseq rows (1+& start-row) stop-row)
				   target)
				  ;; partial last row
				  (list
				   (construct-evrow-from-pointers
				    (get-elements-in-evrow (nth stop-row rows)
							   0 (1+& stop-col))
				    target))))))))))))))


(defun butitem (n box)
  (with-data-prim-number-handling (box)
    (if (= 1 n)
	(make-empty-vc)
	(out-of-range-filter-result n box))
    (cond ((< n 1) (out-of-range-access-result n box))
	  ((empty-box? box)
	   (out-of-range-filter-result n box))
	  (t (multiple-value-bind (row-no col)
	       (get-row-and-col-number n box)
	       (if (null row-no)
		   (out-of-range-filter-result n box)
		   (multiple-value-bind (rows inlinks? new? vc-rows-entry)
		       (get-box-rows box)
		     (declare (ignore inlinks? new?))
		     (let ((active-row (nth row-no rows)))
		       (make-new-box
			box
			(let ((target (when (fast-eval-port-box? box)
					(or vc-rows-entry (vp-target box)))))
			  (nconc (map-construct-evrow-from-evrow
				  (subseq rows 0 row-no) target)
				 (list
				  (construct-evrow-from-pointers
				   (get-butnth-element-in-evrow active-row col)
				   target))
				 (map-construct-evrow-from-evrow
				  (subseq rows (1+ row-no)) target))))))))))))



;;;; UNBOX

;;; UNBOX is different from @.  UNBOX requires a box with exactly one
;;; box in it, and returns (a copy of) the box.
;;; Gives an error if the resulting box is a DOIT box.
;;; Returns raw numbers if given numbers or if inside box is a number.
;;; Removes name, if present.  Produces a port if given a port.
(defun %unbox (box)
  (with-data-prim-number-handling (box) box
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (when (not (null (cdr rows)))
	(unless (every #'empty-evrow? (cdr rows))
	  (eval::primitive-signal-error
	   :BOX-CONTENTS "expected a box containing one element")))
      (when (not (let ((entries (evrow-pointers (car rows))))
		   (and entries (null (cdr entries)))))
	(eval::primitive-signal-error
	 :BOX-CONTENTS "expected a box containing one element"))
      (let ((item (access-evrow-element
		   (or vc-rows-entry (box-or-port-target box))
		   (get-first-element-in-evrow (car rows))
		   t)))
	(cond ((numberp item) item)
	      ((symbolp item)
	       (eval::primitive-signal-error
		:BOX-CONTENTS "expected a box containing one box or number"))
	      ((and (virtual-copy? item)
		    (not (fast-eval-doit-box? item)))
	       (if (virtual-port? box)
		   (port-to item)
		   item))
	      ((virtual-port? item) item)
	      ((port-box? box) (port-to item))
	      ((virtual-port? box) (port-to item))
	      ((and (box? item) (not (doit-box? item)))
	       (virtual-copy item))
	      (t
	       (eval::primitive-signal-error
		:BOX-CONTENTS
		"expected a box containing a DATA box, port, or number")))))))

;; Returnes T if UNBOX will work on the box, NIL otherwise.
;; this may have to hack exact pointers like %unbox....
(defun %unboxable? (box)
  (with-data-prim-number-handling (box) t
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (cond ((and (not (null (cdr rows)))
		  (not (every #'empty-evrow? (cdr rows)))) nil)
	    ((not (let ((entries (evrow-pointers (car rows))))
		    (and entries (null (cdr entries)))))
	     nil)
	    (t
	     (let ((item (access-evrow-element
			  (or vc-rows-entry (box-or-port-target box))
			  (get-first-element-in-evrow (car rows)))))
	       (cond ((numberp item) t)
		     ((symbolp item) nil)
		     ((virtual-copy? item) t)
		     ((port-box? item) t)
		     ((virtual-port? item) t)
		     ((and (box? item) (not (doit-box? item))) t)
		     ((virtual-port? item) t)
		     ((doit-box? item) nil)
		     (t (error "Unrecognized object ~S in UNBOXABLE?"
			       item)))))))))

;; returns T for boxes containing one word, nil otherwise
(defun %word? (box)
  (with-data-prim-number-handling (box) t
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (cond ((and (not (null (cdr rows)))
		  (not (every #'empty-evrow? (cdr rows)))) nil)
	    ((not (let ((entries (evrow-pointers (car rows))))
		    (and entries (null (cdr entries)))))
	     nil)
	    (t
	     (let ((item (access-evrow-element
			  (or vc-rows-entry (box-or-port-target box))
			  (get-first-element-in-evrow (car rows)))))
	       (or (numberp item) (symbolp item))))))))



;;;; Row Accessors

(defun last-row (box)
  (with-data-prim-number-handling (box) (make-empty-vc)
    (make-new-box box
		  (multiple-value-bind (rows inlinks? new? vc-rows-entry)
		      (get-box-rows box)
		    (declare (ignore inlinks? new?))
		    (list (construct-evrow-from-evrow
			   (car (last rows))
			   (when (fast-eval-port-box? box)
			     (or vc-rows-entry (vp-target box)))))))))

(defun butlast-row (box)
  (with-data-prim-number-handling (box) (make-empty-vc)
     (make-new-box box
		   (multiple-value-bind (rows inlinks? new? vc-rows-entry)
		       (get-box-rows box)
		     (declare (ignore inlinks? new?))
		     (let ((target (when (fast-eval-port-box? box)
				     (or vc-rows-entry (vp-target box)))))
		       (or
			(map-construct-evrow-from-evrow (butlast rows)
							target)
			(list (make-empty-evrow))))))))

;; Note that 1-based boxer user coords <==> 0-based Common Lisp prims

(defun nth-row (n box)
  (with-data-prim-number-handling (box)
    (if (= 1 n) box (out-of-range-access-result n box "row"))
    (if (<= 1 n (number-of-rows box))
	(make-new-box box
		      (multiple-value-bind (nthrow vc-rows-entry)
			  (get-nth-row-in-box (1- n) box)
			(list (construct-evrow-from-evrow
			       nthrow (when (fast-eval-port-box? box)
					(or vc-rows-entry
					    (vp-target box)))))))
	(out-of-range-access-result n box "row"))))

(defun nth-rows (start stop box)
  (with-data-prim-number-handling (box)
    (if (= start stop 1) box (out-of-range-access-result start box "row"))
    (let ((nrows (number-of-rows box)))
      (cond ((not (<= 1 start nrows))
	     (out-of-range-access-result start box "row"))
	    ((not (<= start stop nrows))
	     (out-of-range-access-result stop box "row"))
	    (t
	     (multiple-value-bind (rows inlinks? new? vc-rows-entry)
		 (get-box-rows box)
	       (declare (ignore inlinks? new?))
	       (make-new-box box
			     (or (map-construct-evrow-from-evrow
				  (subseq rows (1-& start) stop)
				  (when (fast-eval-port-box? box)
				    (or vc-rows-entry (vp-target box))))
				 (list (make-empty-evrow))))))))))


(defun butnth-row (n box)
  (with-data-prim-number-handling (box)
    (if (= 1 n) (make-empty-vc) (out-of-range-filter-result n box "row"))
    (if (<= 1 n (number-of-rows box))
	(make-new-box box
		      (multiple-value-bind (rows inlinks? new? vc-rows-entry)
			  (get-box-rows box)
			(declare (ignore inlinks? new?))
			(let ((target (when (fast-eval-port-box? box)
					(or vc-rows-entry (vp-target box)))))
			  (map-construct-evrow-from-evrow
			   (nconc (subseq rows 0 (1- n)) (subseq rows n))
			   target))))
        (out-of-range-filter-result n box "row"))))




;;;; Column accessors

(defun butfirst-column (box)
  (with-data-prim-number-handling (box) (make-empty-vc)
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (make-new-box box
		    (let ((target (when (fast-eval-port-box? box)
				    (or vc-rows-entry (vp-target box)))))
		      (mapcar
		       #'(lambda (r)
			   (let ((items (get-butfirst-element-in-evrow r)))
			     (if (null items)
				 (make-empty-evrow)
				 (construct-evrow-from-pointers items
								target))))
			      rows))))))

(defun last-column (box)
  (with-data-prim-number-handling (box) (boxify-number box)
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (make-new-box box
		    (let ((target (when (fast-eval-port-box? box)
				    (or vc-rows-entry (vp-target box)))))
		      (mapcar #'(lambda (r)
				  (let ((item (get-last-element-in-evrow r)))
				    (if (null item)
					(make-empty-evrow)
					(construct-evrow-from-pointer
					 item target))))
			      rows))))))

(defun butlast-column (box)
  (with-data-prim-number-handling (box) (make-empty-vc)
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (make-new-box box
		    (let ((target (when (fast-eval-port-box? box)
				    (or vc-rows-entry (vp-target box)))))
		      (mapcar
		       #'(lambda (r)
			   (let ((items (get-butlast-element-in-evrow r)))
			     (if (null items)
				 (make-empty-evrow)
				 (construct-evrow-from-pointers items
								target))))
		       rows))))))

;; remember to convert 1-based Boxer-User coordinates to 0-based Lisp

(defun nth-column (n box)
  (with-data-prim-number-handling (box)
    (if (= n 1) box (out-of-range-access-result n box "column"))
    (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	(get-box-rows box)
      (declare (ignore inlinks? new?))
      (if (<= 1 n (max-length-in-elements box))
	  (make-new-box
	   box (let ((target (when (fast-eval-port-box? box)
			       (or vc-rows-entry (vp-target box)))))
		 (mapcar #'(lambda (r)
			     (let ((item (get-nth-element-in-evrow r (1- n))))
			       (if (null item)
				   (make-empty-evrow)
				   (construct-evrow-from-pointer item
								 target))))
			 rows)))
	  (out-of-range-access-result n box "column")))))

(defun butnth-column (n box)
  (with-data-prim-number-handling (box)
    (if (= n 1)
	(make-empty-vc)
	(out-of-range-filter-result
	 n box "column"))
    (if (<= 1 n (max-length-in-elements box))
	(multiple-value-bind (rows inlinks? new? vc-rows-entry)
	    (get-box-rows box)
	  (declare (ignore inlinks? new?))
	  (make-new-box
	   box (let ((target (when (fast-eval-port-box? box)
			       (or vc-rows-entry (vp-target box)))))
		 (mapcar #'(lambda (r)
			     (let ((l (evrow-length-in-elements r)))
			       (cond ((> n l)
				      (construct-evrow-from-evrow r target))
				     ((or (= l 1) (= l 0))
				      (make-empty-evrow))
				     (t
				      (construct-evrow-from-pointers
				       (get-butnth-element-in-evrow r (1- n))
				       target)))))
			 rows))))
        (out-of-range-filter-result n box "column"))))

(defun nth-columns (start stop box)
  (with-data-prim-number-handling (box)
    (if (= start stop 1) box (out-of-range-filter-result start box "column"))
    (cond ((not (<= 1 start (max-length-in-elements box)))
	   (out-of-range-filter-result start box "column"))
	  ((not (<= start stop (max-length-in-elements box)))
	   (out-of-range-filter-result stop box "column"))
	  (t
	   (multiple-value-bind (rows inlinks? new? vc-rows-entry)
	       (get-box-rows box)
	     (declare (ignore inlinks? new?))
	     (make-new-box
	      box
	      (let ((target (when (fast-eval-port-box? box)
			      (or vc-rows-entry (vp-target box)))))
		(mapcar #'(lambda (r)
			    (let ((l (evrow-length-in-elements r)))
			      (cond ((> start l) (make-empty-evrow))
				    (t
				     (construct-evrow-from-pointers
				      (get-elements-in-evrow r (1- start)
							     (unless (> stop l)
							       stop))
				      target)))))
			rows))))))))

;;;; 2-Dimensional Accessors
;;  we have to convert 1-based Boxer User coordinates into 0-based
;;  primitive coordinates

(defun row-col-of-box (row col box)
  (with-data-prim-number-handling (box)
    (if (and (= 1 row)
	     (= 1 col))
	(boxify-number box)
	(out-of-range-access-result
	 (cons row col) box))
    (if (<= 1 row (number-of-rows box))
	(multiple-value-bind (active-row vc-rows-entry)
	    (get-nth-row-in-box (1- row) box)
	  (if (<= 1 col (evrow-length-in-elements active-row))
	      (make-new-box box
			    (cond ((virtual-copy? box)
				   (list (make-evrow-from-pointer
					  (get-nth-element-in-evrow
					   active-row (1- col)))))
				  ((fast-eval-port-box? box)
				   (list (make-evrow-from-entry
					  (port-to-item
					   (get-nth-element-in-evrow
					    active-row (1- col))
					   (or vc-rows-entry
					       (vp-target box))))))
				  (t (data-primitive-error
				      "doesn't understand"
				      box))))
	      (out-of-range-access-result (cons row col) box)))
	(out-of-range-access-result (cons row col) box))))



;;;;; Mutators
;;;  Mutators will take port-flavored input but expect the port to be
;;;  dereferenced at the defboxer-primitive level


;;; what about progenitor ?
(defun modified-vc (vc)
  (setf (vc-cached-elements vc)      nil
	(vc-cached-binding-alist vc) nil
	(vc-modified? vc)            t))

(defun handle-possible-editor-caches (evrow)
  (when (row? (evrow-original-row evrow))
    (modified (evrow-original-row evrow))
    (setf (evrow-original-row evrow) nil)))

(defun pedigree-for-copy (box)
  (cond ((virtual-copy? box) (vc-pedigree-for-copy box))
	((box? box)
	 (multiple-value-bind (rows inlinks? new? rows-entry)
	     (virtual-copy-rows box)
	   (declare (ignore rows inlinks? new?))
	   (if (vcis-modified? rows-entry)
	       (acons rows-entry (now) (vcis-pedigree rows-entry))
	       (vcis-pedigree rows-entry))))
	(t (error "~A should be either a virtual-copy or box" box))))

;;; these next 2 functions are used to process new-item args for mutators.
;;; if you need more than a single item or row, then use
;;; formatted-unboxed-items

(defun get-new-row-entries (box &optional (row 0))
  (with-data-prim-number-handling (box) (list box)
    (let* ((sup (box-or-port-target box))
	   (rows (if (box? sup)
		     (multiple-value-bind (rows inlinks? new? rows-entry)
			 (virtual-copy-rows sup)
		       (declare (ignore inlinks? new?))
		       (setq sup rows-entry)
		       rows)
		     (vc-rows sup))))
      (mapcar #'(lambda (p) (handle-pointer-for-unboxing
			     p sup nil (or (port-box? box)
					   (virtual-port? box))))
	      (evrow-pointers (nth row rows))))))

(defun get-new-item (box)
  (with-data-prim-number-handling (box) box
    (let ((row (first-non-empty-row box))
	  (sup (box-or-port-target box)))
      (cond ((null row)
	     (data-primitive-error
	      "NEW-ITEM input," box ", seems to be empty"))
	    (t
	     (when (box? sup)
	       (setq sup (multiple-value-bind (rows inlinks? new? rows-entry)
			     (virtual-copy-rows sup)
			   (declare (ignore rows inlinks? new?))
			   rows-entry)))
	     (handle-pointer-for-unboxing
	      (get-first-element-in-evrow row)
	      sup nil (or (port-box? box) (virtual-port? box))))))))

(defun mutate-rows (box rows &optional (old-pedigree :dont-touch)
			(inlinks :dont-touch) (exact-new-inferiors t))
  (cond ((virtual-copy? box)
	 (setf (vc-rows box) rows)
	 (unless (eq old-pedigree :dont-touch)
	   (setf (vc-pedigree box) old-pedigree))
	 (unless (eq inlinks :dont-touch) (setf (vc-inlinks? box) inlinks))
	 (modified-vc box))
	((box? box)
	 ;; first change the VC representation
	 ;; note that there is no pedigree for us to extend so we
	 (change-virtual-copy-rows box rows old-pedigree inlinks
				   exact-new-inferiors)
	 ;; handle the changes to the editor box
	 (modify-editor-structure box))))

;;; what do we do when the second arg to change is a port ?
;;; 1) text copy of target  or
;;; 2) port-to inferiors as in butfirst port

;;; Still need to put triggers into virtual copies
;;; Hopefully no one will need this for a little while....

(defun change (box new)
  (if (typep box 'foreign-data)
      (foreign-data-set box new)
      (let* ((number? (numberp new))
	     (new-box (and (not number?) (box-or-port-target new)))
             (nfd? (typep new-box 'foreign-data))
	     (rows (if (null number?)
		     (get-box-rows new-box)
		     (list (make-evrow-from-entry new)))))
        (mutate-rows box rows (unless (or number? nfd?)
                                (pedigree-for-copy new-box))
		     (cond ((or nfd? number?) nil)
		           ((virtual-copy? new-box) (vc-inlinks? new-box))
		           (t		; must be an editor box
			    (not-null (slot-value new-box 'contained-links))))
		     nil)))
    eval::*novalue*)

#|
(defun change (box new)
  (let* ((number? (numberp new))
	 (new-box (and (not number?) (box-or-port-target new)))
	 (rows (if (null number?)
		   (get-box-rows new-box)
		   (list (make-evrow-from-entry new)))))
    (cond
     ((virtual-copy? box)
      (setf (vc-rows box) rows)
      (modified-vc box)
      (unless number?
	(setf (vc-pedigree box)
	      (if (vc-modified? new-box)
		  (cons (cons new-box (tick)) (vc-pedigree new-box))
		  (vc-pedigree new-box))))
      (setf (vc-modified? box) t)
      eval::*novalue*)
     ((box? box)
      ;; first change the VC representation
      ;; note that there is no pedigree for us to extend so we
      (change-virtual-copy-rows box
				(if (null number?)
				    rows
				    ;; have to disambigute multipointers (?)
;				    (mapcar #'(lambda (r)
;						(assure-unique-evrow-items
;						 r new-box))
;					    rows)
				    rows)
				;; extend and pass the pedigree
				(when (virtual-copy? new-box)
				  (pedigree-for-copy new-box))
				;; inlinks?
				(cond ((not (null number?)))
				      ((virtual-copy? new-box)
				       (vc-inlinks? new-box))
				      (t ; must be an editor box
				       (not-null
					(slot-value new-box
						    'contained-links)))))
      ;; handle the changes to the editor box
      (modify-editor-structure box)
      eval::*novalue*))))
|#




;;;; Top Level Mutators (DELETE, INSERT, APPEND, CHANGE)

;;; DELETE-<position>-<reference type>

;; row and col are in 0-based internal coords
(defun delete-rc-internal (row col box)
  (let ((rows (get-box-rows box)))
    (mutate-rows box
		 (nconc (unless (zerop& row) (subseq rows 0 row))
			(list (make-evrow-from-pointers
			       (delete-nth-element-in-evrow
				(nth row rows) col)))
			(subseq rows (1+& row))))))

;; n is in 1-based boxer user coords
(defun delete-item (n box)
  (multiple-value-bind (row col)
      (get-row-and-col-number n box)
    (cond ((null row) (out-of-range-mutator-action n box "item"))
	  (t (delete-rc-internal row col box)))))

;; boxer user 1-based start and stop
(defun delete-items (start stop box)
  (multiple-value-bind (start-row start-col)
      (get-row-and-col-number start box)
    (multiple-value-bind (stop-row stop-col)
	(get-row-and-col-number stop box)
      (cond ((null start-row) (out-of-range-mutator-action start box "item"))
	    ((null stop-row) (out-of-range-mutator-action stop box "item"))
	    (t
	     (let ((rows (get-box-rows box)))
	       (if (=& start-row stop-row)
		   (mutate-rows
		    box (nconc (subseq rows 0 start-row)
			       (let ((current-row(nth start-row rows)))
				 (unless (and (zerop& start-col)
					      (=& (1+& stop-col)
						  (evrow-length-in-elements
						   current-row)))
				   (list (make-evrow-from-pointers
					  (delete-elements-in-evrow
					   current-row
					   start-col (1+& stop-col))))))
			       (subseq rows (1+& stop-row))))
		   (mutate-rows
		    box (nconc (subseq rows 0 start-row)
			       (unless (zerop& start-col)
				 (list (make-evrow-from-pointers
					(delete-elements-in-evrow
					 (nth start-row rows) start-col))))
			       (let ((current-row (nth stop-row rows)))
				 (unless (=& (1+& stop-col)
					     (evrow-length-in-elements
					      current-row))
				   (list (make-evrow-from-pointers
					  (delete-elements-in-evrow
					   current-row
					   0 (1+& stop-col))))))
			       (subseq rows (1+& stop-row)))))))))))

;; Start and Stop args are in 1-based boxer user coords
(defun delete-rows (start box &optional (stop start))
  (let ((nrows (number-of-rows box)))
    (cond ((not (<= 1 start nrows))
	   (out-of-range-mutator-action start box "row"))
	  ((not (<= 1 stop nrows))
	   (out-of-range-mutator-action stop box "row"))
	  ((> start stop)
	   (eval::primitive-signal-error :data-mutator
					 "The Start Row," start
					 ", is > than the Stop Row, " stop))
	  (t
	   (let ((rows (get-box-rows box)))
	     (mutate-rows box (nconc (subseq rows 0 (1- start))
				     (subseq rows stop))))))))

;; 1-based
(defun delete-nth-column (n box)
  (if (<= 1 n (max-length-in-elements box))
      (mutate-rows
       box (mapcar #'(lambda (r)
		       (if (<= n (evrow-length-in-elements r))
			   (make-evrow-from-pointers
			    (delete-nth-element-in-evrow r (1-& n)))
			   r))
		   (get-box-rows box)))
      (out-of-range-mutator-action n box "column")))

(defun delete-last-column (box)
  (mutate-rows box (mapcar #'(lambda (r) (make-evrow-from-pointers
					  (delete-last-element-in-evrow r)))
			   (get-box-rows box))))

;; start and stop are in 1-based boxer user coords
(defun delete-columns (start box &optional (stop start))
  (let ((max-length (max-length-in-elements box)))
    (cond ((not (<= 1 start max-length))
	   (out-of-range-mutator-action start box "column"))
	  ((not (<= start stop max-length))
	   (out-of-range-mutator-action stop box "column"))
	  (t
	   (mutate-rows
	    box (mapcar #'(lambda (r)
			    (cond ((> start (evrow-length-in-elements r)) r)
				  ((> stop (evrow-length-in-elements r))
				   (make-evrow-from-pointers
				    (delete-elements-in-evrow r (1-& start))))
				  (t
				   (make-evrow-from-pointers
				    (delete-elements-in-evrow r (1-& start)
							   stop)))))
			(get-box-rows box)))))))



;;; INSERT-<position>-<reference type> and APPEND-<reference type>

(defun insert-rc-internal (row col box new-box old-rows inrow)
  (let ((new-row-entries (formatted-unboxed-items new-box)))
    (mutate-rows
     box (nconc (unless (zerop& row) (subseq old-rows 0 row))
		(if (null (cdr new-row-entries)) ;1 new row
		    (list (make-evrow-from-pointers
			   (nconc (get-elements-in-evrow inrow 0 col)
				  (mapcar #'make-pointer (car new-row-entries))
				  (get-elements-in-evrow inrow col))))
		    (nconc (list (make-evrow-from-pointers
				  (nconc (get-elements-in-evrow inrow 0 col)
					 (mapcar #'make-pointer
						 (car new-row-entries)))))
			   (mapcar #'make-evrow-from-entries
				   (butlast (cdr new-row-entries)))
			   (list (make-evrow-from-pointers
				  (nconc (mapcar #'make-pointer
						 (car (last new-row-entries)))
					 (get-elements-in-evrow inrow col))))))
		(subseq old-rows (1+& row))))))

(defun insert-rc (row col box new-box)
  (let ((height+1 (1+ (number-of-rows box))))
    (cond ((not (<= 1 row height+1))
	   (out-of-range-mutator-action (cons row col) box "item"))
	  ((= row height+1)
	   (if (= col 1)
	       (append-new-row box new-box)
	       (out-of-range-mutator-action (cons row col) box "item")))
	  (t
	   (let* ((old-rows (get-box-rows box))
		  (inrow (nth (1-& row) old-rows)))
	     (if (not (<= 1 col (1+ (evrow-length-in-elements inrow))))
		 (out-of-range-mutator-action (cons row col) box "item")
		 (insert-rc-internal (1-& row) (1-& col) box new-box
				     old-rows inrow)))))))


(defun insert-item (n box new-box)
  (multiple-value-bind (row col)
      (get-row-and-col-number n box)
    (cond ((null row)
	   (if (=& col (1-& n))
	       ;; special case inserting just beyond number of items (like append-item)
	       (append-item box new-box)
	       (out-of-range-mutator-action n box "item")))
	  (t
	   (let* ((old-rows (get-box-rows box)) (inrow (nth row old-rows)))
	     (insert-rc-internal row col box new-box old-rows inrow))))))

(defun append-item (box new-box)
  (let ((old-rows (get-box-rows box))
	(new-row-entries (formatted-unboxed-items new-box)))
    (mutate-rows box
		 (nconc (butlast old-rows)
			(list (make-evrow-from-pointers
			       (append (evrow-pointers (car (last old-rows)))
				       (mapcar #'make-pointer (car new-row-entries)))))
			(mapcar #'make-evrow-from-entries
				(cdr new-row-entries))))))


(defvar *replace-when-adding-rows-to-empty-box* T)

;; 1-based
(defun insert-nth-row (n box new-box)
  (cond ((and *replace-when-adding-rows-to-empty-box* (empty-box? box))
	 (mutate-rows box (mapcar #'make-evrow-from-entries
				   (formatted-unboxed-items new-box))))
	((<= 1 n (1+ (number-of-rows box)))
	 (let ((old-rows (get-box-rows box)))
	   (mutate-rows box (nconc (subseq old-rows 0 (1-& n))
				   (mapcar #'make-evrow-from-entries
					   (formatted-unboxed-items new-box))
				   (subseq old-rows (1-& n))))))
	(t
	 (out-of-range-mutator-action n box "row"))))

(defun append-new-row (box new-box)
  (if (and *replace-when-adding-rows-to-empty-box* (empty-box? box))
      (mutate-rows box (mapcar #'make-evrow-from-entries
				   (formatted-unboxed-items new-box)))
      (mutate-rows box (append (get-box-rows box)
			       (mapcar #'make-evrow-from-entries
				       (formatted-unboxed-items new-box))))))

;; 1-based
(defun insert-column (n box new-box)
  (if (<= 1 n (1+ (max-length-in-elements box)))
      (let ((old-rows (get-box-rows box))
	    (new-row-entries (formatted-unboxed-items new-box)))
	(mutate-rows
	 box (nconc (mapcar #'(lambda (evrow entries)
				(cond ((> n (evrow-length-in-elements evrow))
				       (make-evrow-from-pointers
					(append
					 (evrow-pointers evrow)
					 (mapcar #'make-pointer entries))))
				      (t
				       (make-evrow-from-pointers
					(append
					 (get-elements-in-evrow evrow 0(1-& n))
					 (mapcar #'make-pointer entries)
					 (get-elements-in-evrow evrow
								(1-& n)))))))
			    old-rows new-row-entries)
		    (let ((old-row-length (length old-rows))
			  (new-row-length (length new-row-entries)))
		      (cond ((=& old-row-length new-row-length) nil)
			    ((> old-row-length new-row-length)
			     (nthcdr new-row-length old-rows))
			    (t
			     (mapcar #'make-evrow-from-entries
				     (nthcdr old-row-length
					     new-row-entries))))))))
      (out-of-range-mutator-action n box "column")))

(defun append-column (box new-box)
  (let ((old-rows (get-box-rows box))
	(new-row-entries (formatted-unboxed-items new-box)))
    (mutate-rows
     box (nconc (mapcar #'(lambda (evrow entries)
			    (make-evrow-from-pointers
			     (append (evrow-pointers evrow)
				    (mapcar #'make-pointer entries))))
			old-rows new-row-entries)
		(let ((old-row-length (length old-rows))
		      (new-row-length (length new-row-entries)))
		  (cond ((=& old-row-length new-row-length) nil)
			((> old-row-length new-row-length)
			 (nthcdr new-row-length old-rows))
			(t
			 (mapcar #'make-evrow-from-entries
				 (nthcdr old-row-length
					 new-row-entries)))))))))

;;;;; CHANGE-<position>-<reference type>

;; 1-based
(defun change-nth-row (n box new-box)
  (if (<= 1 n (number-of-rows box))
      (let ((rows (get-box-rows box)))
	(mutate-rows box (nconc (subseq rows 0 (1-& n))
				(mapcar #'make-evrow-from-entries
					(formatted-unboxed-items new-box))
				(subseq rows n))))
      (out-of-range-mutator-action n box "row")))


;; range checks have been made and coords should be 0-based
(defun change-rc-internal (row col box new-box old-rows inrow)
  (let ((new-row-entries (formatted-unboxed-items new-box)))
    (if (and (null (cdr new-row-entries)) (car new-row-entries)
	     (null (cdar new-row-entries)))
	;; check and handle the one new item case
	(multiple-value-bind (row vc-rows-entry)
	    (get-nth-row-in-box row box)
	  (append-value-to-pointer (get-nth-element-in-evrow row col)
				   (or vc-rows-entry box) (now)
				   (caar new-row-entries))
	  (handle-possible-editor-caches row)
	  (if (box? box)
	      (modify-editor-structure box)
	    (modified-vc box)))
	(mutate-rows
	 box (nconc (unless (zerop& row) (subseq old-rows 0 row))
		    (if (null (cdr new-row-entries)) ;1 new row
			(list (make-evrow-from-pointers
			       (nconc (get-elements-in-evrow inrow 0 col)
				      (mapcar #'make-pointer
					      (car new-row-entries))
				      (get-elements-in-evrow inrow
							     (1+& col)))))
			(nconc (list (make-evrow-from-pointers
				      (nconc (get-elements-in-evrow
					      inrow 0 col)
					     (mapcar #'make-pointer
						     (car new-row-entries)))))
			       (mapcar #'make-evrow-from-entries
				       (butlast (cdr new-row-entries)))
			       (list (make-evrow-from-pointers
				      (nconc (mapcar #'make-pointer
						     (car (last
							   new-row-entries)))
					     (get-elements-in-evrow
					      inrow (1+& col)))))))
		    (subseq old-rows (1+& row)))))))


;; 1-based

(defun change-rc (row col box new-box)
  (cond ((<= 1 row (number-of-rows box))
	 (let* ((old-rows (get-box-rows box)) (inrow (nth (1-& row) old-rows)))
	   (if (<= 1 col (evrow-length-in-elements inrow))
	       (change-rc-internal (1-& row) (1-& col) box new-box
				   old-rows inrow)
	       (out-of-range-mutator-action col box "col"))))
	(t
	 (out-of-range-mutator-action row box "row"))))

;; 1-based

(defun change-item (n box new-item)
  (multiple-value-bind (row col)
      (get-row-and-col-number n box)
    (cond ((null row) (out-of-range-mutator-action n box "item"))
	  (t
	   (let* ((old-rows (get-box-rows box)) (inrow (nth row old-rows)))
	     (change-rc-internal row col box new-item old-rows inrow))))))

;; 1-based

(defun change-column (n box new-box)
  (if (or (eq n :last) (<= 1 n (max-length-in-elements box)))
      (multiple-value-bind (old-rows inlinks? new? vc-rows-entry)
	  (get-box-rows box)
	(declare (ignore inlinks? new?))
	(let* ((new-row-entries (formatted-unboxed-items new-box))
	       (old-row-length (length old-rows))
	       (new-row-length (length new-row-entries)))
	  (if (and (every #'(lambda (newrow) (and (car newrow)
						  (null (cdr newrow))))
			  new-row-entries)
		   (>= old-row-length new-row-length))
	      ;; check and handle the one new item case
	      (do* ((rows old-rows (cdr rows))
		    (nrows new-row-entries (cdr nrows))
		    (row (car rows) (car rows))
		    (nrow (car nrows) (car nrows)))
		   ((null rows)
		    (if (box? box) (modify-editor-structure box)
			(modified-vc box)))
		(unless (null nrow)
		  (append-value-to-pointer (if (or (eq n :last)
						   (>= n (evrow-length-in-elements
							  row)))
					       (get-last-element-in-evrow row)
					       (get-nth-element-in-evrow row
									 (1-& n)))
					   (or vc-rows-entry box) (now)
					   (car nrow))
		  (handle-possible-editor-caches row)))
	      (mutate-rows
	       box (nconc (mapcar #'(lambda (evrow entries)
				      (cond ((or (eq n :last)
						 (>= n (evrow-length-in-elements
							evrow)))
					     (make-evrow-from-pointers
					      (nconc
					       (butlast (evrow-pointers evrow))
					       (mapcar #'make-pointer entries))))
					    (t
					     (make-evrow-from-pointers
					      (nconc
					       (get-elements-in-evrow evrow 0
								      (1-& n))
					       (mapcar #'make-pointer entries)
					       (get-elements-in-evrow evrow
								      n))))))
				  old-rows new-row-entries)
			  (cond ((=& old-row-length new-row-length) nil)
				((> old-row-length new-row-length)
				 (nthcdr new-row-length old-rows))
				(t
				 (mapcar #'make-evrow-from-entries
					 (nthcdr old-row-length
						 new-row-entries)))))))))
      (out-of-range-mutator-action n box "column")))




;;;; Top Level Constructors
;;  These differ from accessors in that the newly CONSed VC has NO pedigree
;;  but all the sub items need to have been disambiguated BEFORE being
;;  bundled up into a box.  This is due to the fact that accessors essentially
;;  move inferior items from ONLY one box to another
;;  box and so we can win by extending the PEDIGREE.  On the other hand,
;;  constructors can move inferior items from several boxes and the new
;;  containing box cannot properly represent all the different PEDIGREES.

(defun join-bottom (b1 b2)
  (let ((brs1 (formatted-unboxed-items b1))
	(brs2 (formatted-unboxed-items b2)))
    (make-vc (mapcar #'make-evrow-from-entries
		     (cond ((and (null (cdr brs1)) (null (car brs1))) brs2)
			   ((and (null (cdr brs2)) (null (car brs2))) brs1)
			   (t (nconc  brs1 brs2)))))))

(defun join-right (b1 b2)
  (make-vc
   (let ((rows nil))
     (do* ((r1s (formatted-unboxed-items b1) (cdr r1s))
	   (r2s (formatted-unboxed-items b2) (cdr r2s))
	   (r1 (car r1s) (car r1s))
	   (r2 (car r2s) (car r2s)))
	  ((and (null r1) (null r2)) (nreverse rows))
       (push (make-evrow-from-entries (nconc r1 r2)) rows)))))

(defboxer-primitive bu::join-bottom (b1 b2) (join-bottom b1 b2))
(defboxer-primitive bu::join-right  (b1 b2) (join-right  b1 b2))



;;;; Quick Hack # 107
(defboxer-primitive bu::equal? ((eval::dont-copy b1) (eval::dont-copy b2))
  (eval::boxer-boolean (box-equal-internal b1 b2)))

;; This shouldn't call structured-box-items, since it's been declared
;; obsolete, but it works on editor boxes whereas the new functions don't work.

(defun box-equal-internal (b1 b2)
  (let ((*box-equal-port-crumbs1* nil)
        (*box-equal-port-crumbs2* nil))
    ;; hack possible circular port structures...
    (declare (special *box-equal-port-crumbs1*
                      *box-equal-port-crumbs2*))
    (cond ((and (numberp b1) (numberp b2))
	   (= b1 b2))
	  (t (cond ((numberp b1) (setq b1 (list (list b1))))
	           ((numberp b2) (setq b2 (list (list b2)))))
	     (box-equal-internal-1 b1 b2)))))

(defun box-equal-internal-1 (b1 b2)
  (declare (special *box-equal-port-crumbs1*
                    *box-equal-port-crumbs2*))
  (when (not (or (symbolp b1) (numberp b1) (listp b1)))
    ;; must be a box of some sort
    (cond ((or (port-box? b1) (virtual-port? b1))
           (let ((t1 (box-or-port-target b1)))
             (cond ((fast-memq t1 *box-equal-port-crumbs1*)
                    (setq b1 :circular))
                   (t
                    (push t1 *box-equal-port-crumbs1*)
                    (setq b1 (raw-unboxed-items b1))))))
          (t (setq b1 (raw-unboxed-items b1)))))
  (when (not (or (symbolp b2) (numberp b2) (listp b2)))
    (cond ((or (port-box? b2) (virtual-port? b2))
           (let ((t2 (box-or-port-target b2)))
             (cond ((fast-memq t2 *box-equal-port-crumbs2*)
                    (setq b2 :circular))
                   (t
                    (push t2 *box-equal-port-crumbs2*)
                    (setq b2 (raw-unboxed-items b2))))))
          (t (setq b2 (raw-unboxed-items b2)))))
  (or (and (numberp b1) (numberp b2) (= b1 b2))
      (equal b1 b2)
      (and (listp b1) (listp b2)
	   (= (length b1) (length b2))
	   (every #'box-equal-internal-1 b1 b2))))

(defun rc-position (item box &optional (member?))
  (let* ((item-contents (raw-unboxed-items item))
         (test-items (car item-contents))
         (test-item-length (length test-items))
	 (box-items (raw-unboxed-items box))
	 (item-counter 1) (row-counter 1) (col-counter 1)
	 (items nil) (rows nil) (cols nil))
    (when (or (null test-items) (not (null (cdr item-contents))))
      (eval::primitive-signal-error
       :data-primitive "The pattern item should have" "only one non-empty row"))
    (catch 'found
      (dolist (row-items box-items)
        (do ((box-items row-items (cdr box-items))
             (remaining-length (length row-items) (1- remaining-length)))
            ((null box-items))
          (when (and (>= remaining-length test-item-length)
                     (every #'box-equal-internal test-items box-items))
	    (push item-counter items)
	    (push row-counter rows)
            (push col-counter cols)
            (when member?
              ;; stop immediately if we are just looking for member?
              (throw 'found T)))
	  (incf& item-counter) (incf& col-counter))
        (incf& row-counter) (setq col-counter 1)))
    (values (nreverse items) (nreverse rows) (nreverse cols))))

;;;; Miscellaneous


(defun alpha-sort-top-level (box1 box2 &optional (direction 1))
  (catch 'alpha-top-level
    (if (plusp direction)
	(alpha->-internal box1 box2)
	(alpha-<-internal box1 box2))))

;;; boxes are handled by representing them as #\Null's in the
;;; test strings

(defun alpha->-internal (b1 b2)
  (multiple-value-bind (rows1 ignore1a ignore1b vc-rows-entry1)
      (get-box-rows b1)
    (declare (ignore ignore1a ignore1b))
    (multiple-value-bind (rows2 ignore2a ignore2b vc-rows-entry2)
	(get-box-rows b2)
      (declare (ignore ignore2a ignore2b))
      (let ((str1 (make-array 32 :element-type 'standard-char
			      :adjustable t :fill-pointer 0))
	    (str2 (make-array 32 :element-type 'standard-char
			      :adjustable t :fill-pointer 0)))
	(do* ((r1s rows1 (cdr r1s)) (r1 (car r1s) (car r1s))
	      (r2s rows2 (cdr r2s)) (r2 (car r2s) (car r2s)))
	     ((or (null r1) (null r2)) (not (null r1)))
	  ;; these side effect the string variables
	  (evrow-text-string r1 (or vc-rows-entry1 b1) str1 #\null #\null)
	  (evrow-text-string r2 (or vc-rows-entry2 b2) str2 #\null #\null)
	  (let ((test1 (string-trim '(#\space #\tab) str1))
		(test2 (string-trim '(#\space #\tab) str2)))
	    (cond ((string= test1 test2)
		   ;; these are equal so reset
		   (setf (fill-pointer str1) 0 (fill-pointer str2) 0))
		  (t (return (string> test1 test2))))))))))

(defun alpha-<-internal (b1 b2)
  (multiple-value-bind (rows1 ignore1a ignore1b vc-rows-entry1)
      (get-box-rows b1)
    (declare (ignore ignore1a ignore1b))
    (multiple-value-bind (rows2 ignore2a ignore2b vc-rows-entry2)
	(get-box-rows b2)
      (declare (ignore ignore2a ignore2b))
      (let ((str1 (make-array 32 :element-type 'standard-char
			      :adjustable t :fill-pointer 0))
	    (str2 (make-array 32 :element-type 'standard-char
			      :adjustable t :fill-pointer 0)))
	(do* ((r1s rows1 (cdr r1s)) (r1 (car r1s) (car r1s))
	      (r2s rows2 (cdr r2s)) (r2 (car r2s) (car r2s)))
	     ((or (null r1) (null r2)) (not (null r2)))
	  ;; these side effect the string variables
	  (evrow-text-string r1 (or vc-rows-entry1 b1) str1 #\null #\null)
	  (evrow-text-string r2 (or vc-rows-entry2 b2) str2 #\null #\null)
	  (let ((test1 (string-trim '(#\space #\tab) str1))
		(test2 (string-trim '(#\space #\tab) str2)))
	    ;; remove surrounding whitespace for the purpose of the test
	    (cond ((string= test1 test2)
		   ;; these are equal so reset
		   (setf (fill-pointer str1) 0 (fill-pointer str2) 0))
		  (t (return (string< test1 test2))))))))))

#|
;;; These iterate through the contents rather than CONSing strings to compare
(defun alpha-compare-loop-internal (box1 box2 compare-char-function)
  (do* ((r1s (formatted-unboxed-items box1) (cdr r1s))
	(r2s (formatted-unboxed-items box2) (cdr r2s))
	(r1 (car r1s) (car r1s))
	(r2 (car r2s) (car r2s)))
       ((null r1) (throw 'alpha-top-level nil))
    (do* ((items1 r1 (cdr items1))
	  (item1 (car items1) (car items1)))
	 ;; to be continued
	 )))

;; case insensitive, use char= otherwise
;; Boxes are more important that any char
(defun char->-internal (c1 c2)
  (cond ((null c1) (throw 'alpha-top-level nil))
	((null c2) (throw 'alpha-top-level T))
	((and (characterp c1) (characterp c2))
	 (cond ((char-equal c1 c2))
	       ((char> c1 c2) (throw 'alpha-top-level T))
	       (t (throw 'alpha-top-level nil))))
	;; one of the characters must be a box
	((characterp c1) (throw 'alpha-top-level nil))
	((characterp c2) (throw 'alpha-top-level T))
	(t ; both chars must be boxes
	 (alpha->-internal c1 c2))))

(defun char-<-internal (c1 c2)
  (cond ((null c1) (throw 'alpha-top-level T))
	((null c2) (throw 'alpha-top-level nil))
	((and (characterp c1) (characterp c2))
	 (cond ((char-equal c1 c2))
	       ((char< c1 c2) (throw 'alpha-top-level T))
	       (t (throw 'alpha-top-level nil))))
	;; one of the characters must be a box
	((characterp c1) (throw 'alpha-top-level T))
	((characterp c2) (throw 'alpha-top-level nil))
	(t ; both chars must be boxes
	 (alpha-<-internal c1 c2))))
|#




;;;; Implode and Explode

(defun explode-row-items (items &optional evrow)
  (when (and evrow (not (null (evrow-row-format evrow)))
	     (only-formatting-chunk? (evrow-row-format evrow)))
    (setq items (list (evrow-row-format evrow))))
  (make-evrow-from-pointers
   (with-collection
       (flet ((new-pointer (chas idx)
		(let ((new-fi (make-formatting-info :chas chas
						    :start idx
						    :stop (1+& idx))))
		  (collect
		   (make-pointer
		    (make-chunk 1 new-fi (read-formatting-info new-fi) 1)))))
	      (make-formatting-array-from-item (thing)
		(let ((string (format nil "~A" thing)))
		  (let ((unfilled-array (make-array (length string))))
		    (dotimes (i (length string))
		      (setf (svref& unfilled-array i) (aref string i)))
		    (values unfilled-array (length string))))))
	 (dolist (item items)
	   (cond ((not (chunk-p item))
		  (cond ((or (virtual-copy? item)
			     (virtual-port? item)
			     (box? item))
			 (collect (make-pointer item)))
			(t (multiple-value-bind (formatting-array length)
			       (make-formatting-array-from-item item)
			     (do ((idx 0 (1+& idx)))
				 ((>=& idx length))
			       (new-pointer formatting-array idx))))))
		 (t
		  ;; must be a chunk...
		  (let ((value (chunk-chunk item)))
		    (flet ((explode-fi (fi)
			     (let ((chas (fi-chas fi))
				   (start (fi-start fi)) (stop (fi-stop fi)))
			       (do ((idx start(1+& idx))) ((>=& idx stop))
				 (new-pointer chas idx)))))
		      (cond ((or (virtual-copy? value)
				 (virtual-port? value)
				 (box? value))
			     (collect (make-pointer item)))
			    ((formatting-info? (chunk-pname item))
                             (when (%formatting-info? (chunk-left-format item))
                               (explode-fi (chunk-left-format item)))
			     (explode-fi (chunk-pname item)))
			    ((eq value 'format-only)
			     ;; we've failed the chunk-pname test, so try
			     ;; looking in either the chunk-left/right-format
			     ;; we use the "%" version because we are NOT
			     ;; interested in numbers which represent spaces
			     (cond ((%formatting-info?
				     (chunk-left-format item))
				    (explode-fi (chunk-left-format item)))
				   ((%formatting-info?
				     (chunk-right-format item))
				    (explode-fi (chunk-right-format item)))))
			    (t (multiple-value-bind (formatting-array length)
				   (make-formatting-array-from-item value)
				 (do ((idx 0 (1+& idx)))
				     ((>=& idx length))
				   (new-pointer formatting-array
						idx))))))))))))))


(defvar *formatting-array-length-for-implode* 32)

(defun implode-row-items (items)
  (if (null items)
      (make-empty-evrow)
      (make-evrow-from-pointers
       (with-collection
	   (let* ((length *formatting-array-length-for-implode*)
		  (idx 0)
		  (start 0)
		  (array (make-array (the fixnum length))))
	     (flet ((array-append (item)
		      (setf (aref array idx) item)
		      (incf& idx))
		    (array-ok (additional-length)
		      (cond ((>=& (+& idx additional-length) length)
			     ;; need to grow the array
			     (let ((new (make-array
					 (setq length
					       (+& length
						   additional-length)))))
			       (dotimes& (i idx)
				 (setf (aref new i) (aref array i)))
			       (setq array new)))
			    (t array))))
	       (dolist (item items (collect
				    (let ((fi (make-formatting-info
					       :chas array
					       :start start :stop idx)))
				      (make-pointer
				       (make-chunk
					0 fi (read-formatting-info fi) 0)))))
		 (cond ((not (chunk-p item))
			(cond ((or (virtual-copy? item)
				   (virtual-port? item)
				   (box? item))
			       ;; either there is a fi being made or there
			       ;; isn't if there is, then we return the fi then
			       ;;  the box otherwise, just return the box
			       (cond ((=& idx start)
				      (collect (make-pointer item)))
				     (t (collect
					 (let ((fi (make-formatting-info
						    :chas array
						    :start start
						    :stop idx)))
					   (make-pointer
					    (make-chunk
					     0 fi
					     (read-formatting-info fi) 0))))
					(collect (make-pointer item))
					(setq start idx))))
			      (t
			       (let ((string (format nil "~A" item)))
				 (array-ok (length string))
				 (dotimes& (i (length string))
				   (array-append (aref string i)))))))
		       (t
			;; must be a chunk of some sort
			(let ((value (chunk-chunk item)))
			  (cond ((or (virtual-copy? value)
				     (virtual-port? value)
				     (box? value))
				 ;; either there is a fi being made or there
				 ;; isn't if there is, then we return the fi
				 ;; then the box otherwise, just return the box
				 (cond ((=& idx start)
					(collect (make-pointer item)))
				       (t (collect
					   (let ((fi (make-formatting-info
						      :chas array
						      :start start
						      :stop idx)))
					     (make-pointer
					      (make-chunk
					       0 fi
					       (read-formatting-info fi) 0))))
					  (collect (make-pointer item))
					  (setq start idx))))
				((%formatting-info? (chunk-pname item))
				 (array-ok (fi-length (chunk-pname item)))
				 (do-fi-chas (cha (chunk-pname item))
				   (unless (whitespace-char? cha)
				     (array-append cha))))
				(t
				 (let ((string (format nil "~A" value)))
				   (array-ok (length string))
				   (dotimes& (i (length string))
				     (array-append (aref string i))))))))))))))))




;;;; Boxer Function Definitions

;;;;  Info
(defboxer-primitive bu::empty? ((eval::dont-copy box))
  (eval:boxer-boolean (unless (numberp box) (empty-box? box))))

(defboxer-primitive bu::length ((eval::dont-copy box))
  (if (numberp box) 1 (box-length-in-elements box)))

(defboxer-primitive bu::count ((eval::dont-copy box))
  (if (numberp box) 1 (box-length-in-elements box)))

(defboxer-primitive bu::number-of-items ((eval::dont-copy box))
  (if (numberp box) 1 (box-length-in-elements box)))


(defboxer-primitive bu::width  ((eval::dont-copy box))
  (if (numberp box) 1 (max-length-in-elements box)))

(defboxer-primitive bu::number-of-columns ((eval::dont-copy box))
  (if (numberp box) 1  (max-length-in-elements box)))


(defboxer-primitive bu::height ((eval::dont-copy box))
  (if (numberp box) 1 (number-of-rows box)))

(defboxer-primitive bu::number-of-rows ((eval::dont-copy box))
  (if (numberp box) 1 (number-of-rows box)))

(defboxer-primitive bu::item-numbers ((eval::dont-copy item)
				      (eval::dont-copy box))
  (let ((items (rc-position (box-or-port-target item)
			    (box-or-port-target box))))
    (if (null items)
	(make-empty-vc)
	(make-vc (list (make-evrow-from-entries items))))))

(defboxer-primitive bu::row-numbers ((eval::dont-copy item)
				     (eval::dont-copy box))
  (multiple-value-bind (items rows cols)
      (rc-position (box-or-port-target item) (box-or-port-target box))
    (declare (ignore items cols))
    (if (null rows)
	(make-empty-vc)
	(make-vc (list (make-evrow-from-entries
			(delete-duplicates rows :test #'=)))))))

(defboxer-primitive bu::column-numbers ((eval::dont-copy item)
					(eval::dont-copy box))
  (multiple-value-bind (items rows cols)
      (rc-position (box-or-port-target item) (box-or-port-target box))
    (declare (ignore items rows))
    (if (null cols)
	(make-empty-vc)
	(make-vc (list (make-evrow-from-entries
			(delete-duplicates cols :test #'=)))))))

(defboxer-primitive bu::rc-numbers ((eval::dont-copy item)(eval::dont-copy box))
  (multiple-value-bind (items rows cols)
      (rc-position (box-or-port-target item) (box-or-port-target box))
    (declare (ignore items))
    (if (null rows)
	(make-empty-vc)
	(make-vc (list (make-evrow-from-entries
			(mapcar #'(lambda (r c)
				    (make-vc (list (make-evrow-from-entries
						    (list r c)))))
				rows cols)))))))

(defboxer-primitive bu::member? ((eval::dont-copy item) (eval::dont-copy box))
  (eval::boxer-boolean (rc-position (box-or-port-target item)
                                    (box-or-port-target box))))

;; old keyword based primitive
;(DEFBOXER-FUNCTION NUMBER-OF (BOX SPECIFIER) (NUMBER-OF BOX SPECIFIER))

;;;; Accessors

;;; Items
(defboxer-primitive bu::first    (box)
  (if (empty-box? box) (make-empty-vc) (item 1 box)))
(defboxer-primitive bu::butfirst (box)  (butstart box))
(defboxer-primitive bu::last     (box)  (%last box))
(defboxer-primitive bu::butlast  (box)  (%butlast box))

(defboxer-primitive bu::item     ((eval::numberize n) box)
  (if (and (integerp n) (plusp& n))
      (item n box)
      (eval::primitive-signal-error :data-primitive
				    "The index, "
				    n ", should be a positive integer")))

(defboxer-primitive bu::butitem  ((eval::numberize n) box)
  (if (and (integerp n) (plusp& n))
      (butitem n box)
      (eval::primitive-signal-error :data-primitive
				    "The index, "
				    n ", should be a positive integer")))

(defboxer-primitive bu::items ((eval::numberize start)
			       (eval::numberize stop) box)
  (cond ((not (and (integerp start) (plusp& start)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " start
				       ", should be a positive integer"))
	((not (and (integerp stop) (plusp& stop)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " stop
				       ", should be a positive integer"))
	(t (items start stop box))))

;;; rows
(defboxer-primitive bu::first-row    (box) (nth-row 1 box))
(defboxer-primitive bu::butfirst-row (box) (butnth-row 1 box))
(defboxer-primitive bu::last-row     (box) (last-row box))
(defboxer-primitive bu::butlast-row  (box) (butlast-row box))
(defboxer-primitive bu::row ((eval::numberize n) box)
  (if (and (integerp n) (plusp& n))
      (nth-row n box)
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer")))

(defboxer-primitive bu::butrow ((eval::numberize n) box)
  (if (and (integerp n) (plusp& n))
      (butnth-row n box)
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer")))

(defboxer-primitive bu::rows ((eval::numberize start)(eval::numberize stop)box)
  (cond ((not (and (integerp start) (plusp& start)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " start
				       ", should be a positive integer"))
	((not (and (integerp stop) (plusp& stop)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " stop
				       ", should be a positive integer"))
	(t (nth-rows start stop box))))

;;; unbox
(defboxer-primitive bu::unbox (box) (%unbox box))
(defboxer-primitive bu::unboxable? ((eval::dont-copy box))
  (eval::boxer-boolean (%unboxable? box)))
(defboxer-primitive bu::word? ((eval::dont-copy box))
  (eval::boxer-boolean (%word? box)))

;;; Columns
(defboxer-primitive bu::first-column    (box) (nth-column 1 box))
(defboxer-primitive bu::butfirst-column (box) (butfirst-column box))
(defboxer-primitive bu::last-column     (box) (last-column box))
(defboxer-primitive bu::butlast-column  (box) (butlast-column box))
(defboxer-primitive bu::column ((eval::numberize n) box)
  (if (and (integerp n) (plusp& n))
      (nth-column n box)
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer")))

(defboxer-primitive bu::butcolumn ((eval::numberize n) box)
  (if (and (integerp n) (plusp& n))
      (butnth-column n box)
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer")))

(defboxer-primitive bu::columns ((eval::numberize start)(eval::numberize stop)
				 box)
  (cond ((not (and (integerp start) (plusp& start)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " start
				       ", should be a positive integer"))
	((not (and (integerp stop) (plusp& stop)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " stop
				       ", should be a positive integer"))
	(t (nth-columns start stop box))))

(defboxer-primitive bu::rc ((eval::numberize row) (eval::numberize col) box)
  (cond ((not (and (integerp row) (plusp& row)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " row
				       ", should be a positive integer"))
	((not (and (integerp col) (plusp& col)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " col
				       ", should be a positive integer"))
	(t (row-col-of-box row col box))))

;;;;; Mutators

(defboxer-primitive bu::change ((bu::port-to box) to-box)
  (change (get-port-target box) to-box))


;;; Delete Mutators

(defboxer-primitive bu::delete-item ((eval::numberize n) (bu::port-to box))
  (if (and (integerp n) (plusp& n))
      (delete-item n (box-or-port-target box))
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::delete-last-item ((bu::port-to box))
  (let ((realbox (box-or-port-target box)))
    (delete-item (box-length-in-elements realbox) realbox)
    eval::*novalue*))

(defboxer-primitive bu::delete-items ((eval::numberize start)
				      (eval::numberize stop)
				      (bu::port-to box))
  (cond ((not (and (integerp start) (plusp& start)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " start
				       ", should be a positive integer"))
	((not (and (integerp stop) (plusp& stop)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " stop
				       ", should be a positive integer"))
	(t (delete-items start stop (box-or-port-target box))))
  eval::*novalue*)

(defboxer-primitive bu::delete-rc ((eval::numberize row) (eval::numberize col)
				   (bu::port-to box))
  (cond ((not (and (integerp row) (plusp& row)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " row
				       ", should be a positive integer"))
	((not (and (integerp col) (plusp& col)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " col
				       ", should be a positive integer"))
	(t (let ((realbox (box-or-port-target box)))
	     (cond ((good-rc-coords? (1-& row) (1-& col) realbox)
		    (delete-rc-internal (1-& row) (1-& col) realbox))
		   (t (out-of-range-mutator-action (cons row col) box "rc"))))
	   eval::*novalue*)))

(defboxer-primitive bu::delete-row  ((eval::numberize n) (bu::port-to box))
  (if (and (integerp n) (plusp& n))
      (delete-rows n (box-or-port-target box))
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::delete-last-row  ((bu::port-to box))
  (let ((realbox (box-or-port-target box)))
    (delete-rows (number-of-rows realbox) realbox)
    eval::*novalue*))

(defboxer-primitive bu::delete-rows ((eval::numberize start)
				     (eval::numberize stop)
				     (bu::port-to box))
  (cond ((not (and (integerp start) (plusp& start)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " start
				       ", should be a positive integer"))
	((not (and (integerp stop) (plusp& stop)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " stop
				       ", should be a positive integer"))
	(t (delete-rows start (box-or-port-target box) stop)))
  eval::*novalue*)

(defboxer-primitive bu::delete-column ((eval::numberize col) (bu::port-to box))
  (if (and (integerp col) (plusp& col))
      (delete-nth-column col (box-or-port-target box))
      (eval::primitive-signal-error :data-primitive
				    "The index, " col
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::delete-last-column ((bu::port-to box))
  (delete-last-column (box-or-port-target box))
  eval::*novalue*)

(defboxer-primitive bu::delete-columns ((eval::numberize start)
					(eval::numberize stop)
					(bu::port-to box))
  (cond ((not (and (integerp start) (plusp& start)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " start
				       ", should be a positive integer"))
	((not (and (integerp stop) (plusp& stop)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " stop
				       ", should be a positive integer"))
	(t (delete-columns start (box-or-port-target box) stop)))
  eval::*novalue*)


;;; Insert and Append Mutators

(defboxer-primitive bu::insert-rc (new (bu::port-to box)
				       (eval::numberize row)
				       (eval::numberize col))
  (cond ((not (and (integerp row) (plusp& row)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " row
				       ", should be a positive integer"))
	((not (and (integerp col) (plusp& col)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " col
				       ", should be a positive integer"))
	(t (insert-rc row col (box-or-port-target box) new)))
  eval::*novalue*)

(defboxer-primitive bu::insert-item (new (bu::port-to in)
					 (eval::numberize at))
  (if (and (integerp at) (plusp& at))
      (insert-item at (box-or-port-target in) new)
      (eval::primitive-signal-error :data-primitive
				    "The index, " at
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::append-item (new-item (bu::port-to to))
  (append-item (box-or-port-target to) new-item)
  eval::*novalue*)

(defboxer-primitive bu::insert-row (new (bu::port-to in) (eval::numberize at))
  (if (and (integerp at) (plusp& at))
      (insert-nth-row at (box-or-port-target in) new)
      (eval::primitive-signal-error :data-primitive
				    "The index, " at
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::append-row (new (bu::port-to to))
  (append-new-row (box-or-port-target to) new)
  eval::*novalue*)

(defboxer-primitive bu::insert-column (new(bu::port-to in)(eval::numberize at))
  (if (and (integerp at) (plusp& at))
      (insert-column at (box-or-port-target in) new)
      (eval::primitive-signal-error :data-primitive
				    "The index, " at
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::append-column (new (bu::port-to to))
  (append-column (box-or-port-target to) new)
  eval::*novalue*)


;;; Change Mutators

(defboxer-primitive bu::change-item ((eval::numberize n) (bu::port-to box) new)
  (if (and (integerp n) (plusp& n))
      (change-item n (box-or-port-target box) new)
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::change-last-item ((bu::port-to box) new)
  (let ((realbox (box-or-port-target box)))
    (change-item (box-length-in-elements realbox) realbox new))
  eval::*novalue*)

(defboxer-primitive bu::change-rc ((eval::numberize row) (eval::numberize col)
				   (bu::port-to box) new)
  (cond ((not (and (integerp row) (plusp& row)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " row
				       ", should be a positive integer"))
	((not (and (integerp col) (plusp& col)))
	 (eval::primitive-signal-error :data-primitive
				       "The index, " col
				       ", should be a positive integer"))
	(t (change-rc row col (box-or-port-target box) new)))
  eval::*novalue*)

(defboxer-primitive bu::change-row ((eval::numberize n) (bu::port-to box) new)
  (if (and (integerp n) (plusp& n))
      (change-nth-row n (box-or-port-target box) new)
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::change-last-row ((bu::port-to box) new)
  (change-nth-row (number-of-rows box) (box-or-port-target box) new)
  eval::*novalue*)

(defboxer-primitive bu::change-column ((eval::numberize n)(bu::port-to box)new)
  (if (and (integerp n) (plusp& n))
      (change-column n (box-or-port-target box) new)
      (eval::primitive-signal-error :data-primitive
				    "The index, " n
				    ", should be a positive integer"))
  eval::*novalue*)

(defboxer-primitive bu::change-last-column ((bu::port-to box) new)
  (change-column :last (box-or-port-target box) new)
  eval::*novalue*)


;;;; Explode/Implode

(defun explode-box (box)
  (with-data-prim-number-handling (box)
    (make-virtual-copy :rows (mapcar #'explode-row-items
				     (list (list box))))
    (make-virtual-copy :rows (mapcar #'explode-row-items
				     (formatted-unboxed-items box)
				     (get-box-rows box)))))

(defun implode-box (box)
  (with-data-prim-number-handling (box) box
    (make-virtual-copy :rows (mapcar #'implode-row-items
				     (formatted-unboxed-items box)))))

(defboxer-primitive bu::letters (box)
  (explode-box box))

(defboxer-primitive bu::word (box)
  (implode-box box))


;;;;; Low-level grungy box primitives
(defboxer-primitive bu::boxify (thing)
  (boxer::data-boxify thing))

;; check if cursor is inside of port...
(defboxer-primitive bu::retarget ((eval::dont-copy port) (bu::port-to target))
  "retarget the port to the given box"
  (retarget-internal port target)
  eval::*novalue*)

(defun retarget-internal (port target)
  (let ((editor-port (vp-editor-port-backpointer port))
	(real-target (box-or-port-target target)))
    (cond ((null editor-port)
	   ;; must be a virtual port
	   (setf (vp-target port) real-target)
	   (when (virtual-copy? real-target)
	     (setf (vc-port-target? real-target) t)))
	  ((box? real-target)
	   (retarget-port editor-port real-target)
           (when (point-in-port? editor-port)
             ;; have to move the point so it looks right
             (new-target-cursor-position eval::*process-doit-cursor-position*
                                         editor-port))
	   (modified editor-port))
	  (t
	   ;; target is a VC but the port is already in the
	   ;; editor hierarchy, this MAY involve later retargetting
	   (let ((new-target (lookup-new-target real-target)))
	     (cond ((not (null new-target))
		    (retarget-port editor-port new-target))
		   (t
		    (putprop editor-port real-target 'retargetting-vc)
		    (record-port-printing editor-port))))
	   (modified editor-port)))))

(defboxer-primitive bu::redirect ((eval::dont-copy port) (bu::port-to target))
  "retarget the port to the given box"
  (cond ((not (null *uc-copyright-free*))
         (eval::primitive-signal-error :copyright
                                       'bu::redirect " is no longer available"))
        (t
         (retarget-internal port target)
         eval::*novalue*)))

(defun point-in-port? (editor-port)
  (let ((screen-objs (screen-objs editor-port))
        (psb (point-screen-box)))
    (member psb screen-objs :test #'superior?)))

(defun new-target-cursor-position (vector port)
  (let ((port-screen-box (or (car (member (point-screen-box) (screen-objs port)
                                          :test #'superior?))
                             (car (screen-objs port))))
        (new-row (first-inferior-row port)))
    (move-point-1 new-row 0 port-screen-box)
    (setf (process-doit-cursor-position-box vector) (ports port))
    (setf (process-doit-cursor-position-row vector) new-row)
    (setf (process-doit-cursor-position-cha-no vector) 0)
    (setf (process-doit-cursor-position-row-no vector) 0)
    (setf (process-doit-cursor-position-box-stack vector)
	  (with-collection
	    (do* ((screen-box port-screen-box
                   (unless (null screen-box)
                    (superior-screen-box screen-box)))
		  (box (if (null screen-box)
                        (point-box)
                        (screen-obj-actual-obj screen-box))
                   (if (null screen-box)
                    (superior-box box)
                    (screen-obj-actual-obj screen-box))))
		 ((or (eq box *initial-box*)
		      (when (null box)
		        (warn "The *point* is not in the editor hierarchy")
		        t)))
	         (collect box))))
    (setf (process-doit-cursor-position-screen-box vector) port-screen-box))
  vector)

(defboxer-primitive bu::alpha> (box1 box2)
  (eval::boxer-boolean
   (alpha-sort-top-level (box-or-port-target box1)
			 (box-or-port-target box2) 1)))

(defboxer-primitive bu::alpha< (box1 box2)
  (eval::boxer-boolean
   (alpha-sort-top-level (box-or-port-target box1)
			 (box-or-port-target box2) -1)))

(defboxer-primitive bu::superior ((bu::port-to box))
  (let ((targ (box-or-port-target box)))
    (let ((sup (if (virtual-copy? targ)
		   (vc-superior-for-scoping targ)
		   (superior-box targ))))
      (if (null sup)
	  (make-empty-vc)
	  (port-to sup)))))


