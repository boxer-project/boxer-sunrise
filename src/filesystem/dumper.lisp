;-*- mode:lisp; syntax:common-lisp;  package:boxer -*-
#|


 $Header: dumper.lisp,v 1.0 90/01/24 22:10:28 boxer Exp $


 $Log:	dumper.lisp,v $
;;;Revision 1.0  90/01/24  22:10:28  boxer
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


      This is a machine independent binary dumper for the BOXER system
      It is meant to convert box structure into a binary format for
      storing in files

Modification History (most recent at the top)
 8/ 6/13 *dump-relative-font-sizes?*, dump-font
 2/ 1/13 dump-cha: no more char-bits
 4/03/09 dump-string, dump-row-chas-as-string expanded to use UTF-8 encoding
 4/01/09 UTF-8 utility funs: utf-8-{size,length,length-from-row-chas}, encode-utf-8
 4/22/08 dump-graphics-list to handle new hidden slot
 2/01/05 added dump support for private graphics lists in turtle methods
 9/06/05 platform specific network REQUIRE's moved from here to fildfs
10/01/03 cleaned up and added opentransport to REQUIRE of network packages
 2/16/03 merged current LW and MCl files
 9/20/02 dump-true-color-pixmap changed pixel compare to color= to fix expanding
         file size bug when moving mac files to PC
 2/17/02 use pixel-dump-value-internal in dump-true-color-pixmap for
         platform independence
 2/11/02 dump-graphics-sheet-plist now checks dirty? flag before dumping pixmaps
 2/14/01 merged current LW and MCL files
 8/22/00 canonicalize-pixel-color in dump-plist-internal method for
         graphics-cursors
 7/14/00 array-bits-per-element for lispworks
 5/12/00 new file-stream-position used in dump-box for stream independent position
         information
 4/13/99 dump-plist-length fixed to match cross file port changes in
         dump-plist-internal
 3/23/99 dump-plist-internal method for ports changed to dump out a cracked-port
         property for any cross file port
 2/15/99 fast-mac-dump-8-bit-pixmap changed to remove mac trap calls
 1/23/99 added xtension file hooks
 6/24/98 make (:method dump-plist-internal graphics-cursor) use font dumping
         mechanism also changed dump-graphics-list for same reason
 4/26/98 check dumping font-map in dump-font to avoid duplicate dumping of font
         specs change (dump-plist-internal (box)) and (dump-plist-length (box))
         to check for standard-display-style
 4/24/98 change in color dump format from list of 3 floats to single fixnums
         with 8-bit RGB fields.  Functions changed: dump-font-descriptor,
         dump-graphics-sheet-plist,  dump-graphics-list, and
         fast-mac-dump-8-bit-pixmap all check for (> *version-number* 12)
 4/22/98 added *basic-ops* list of binops for use by simple-print-dump-file
         includes all simple ops without any compound data
 4/20/98 Changes to handle fonts:
           o toggle-font-dumping
           o changed (dump-self row) to dump chas as string segments and
             font descriptors when (> *version-number* 12)
           o added dump-string-preamble to support open coded string dumping
             dump-row-chas-as-string
           o dump-font-descriptor and dump-font
           o added standard-display-style? check so we can avoid always
             dumping out display styles for the common case
 4/18/98 Start logging changes: source = boxer version 2.2r4

|#

(in-package :boxer)



;;;;;;;; THESE Comments are Obsolete !!!!!!!!!!!!!
;;; The boxer world has three kinds of objects which must be dumped out
;;; They are: CHARACTERS, ROWS, and BOXES.
;;;
;;; CHARACTERS are dumped out as themselves, that is, fixnums
;;;
;;; ROWS are essentially arrays of characters and are dumped out as such
;;; keeping in mind that some of the characters may be BOXES
;;;
;;; BOXES come in three major types.  Regular, Port and Graphics.
;;;    ALL boxes have to preserve their display info (i.e. desired size),
;;;    their name, the superior row
;;;
;;;    GRAPHICS boxes have to dump out their bit-arrays (although in the
;;;    case of turtle boxes it may be optional)
;;;
;;;    REGULAR boxes will have to keep track of their inferior rows,
;;;    and Any pointers to PORTS
;;;
;;;    PORTS only have to keep track of the ported to box

;******************************************************************************
;*                             DUMPING   FUNCTIONS                            *
;******************************************************************************

;;; Top level Dumping Function (this is called from BOXER and takes 2 args
;;;  a <box> and a <filename>)

(defun dump-top-level-box (box filename &optional file-attribute-list)
  (let ((dps (getprop box :dump-properties)))
    (unless (null dps)
      (setq file-attribute-list (append file-attribute-list dps))))
  (unless (getf file-attribute-list :package)
    (setf (getf file-attribute-list :package) ':boxer))
  (with-mouse-cursor (:file-io)
    (writing-bin-file (box stream filename)
                      (dump-attribute-list file-attribute-list stream)
                      (dump-box box stream))
    #+mcl (ccl::set-mac-file-type filename "BOXR")
    #+mcl (ccl::set-mac-file-creator filename "BOXR")
    ))

(defun dump-top-level-box-to-stream (box stream
                                         &optional stream-attribute-list)
  (let ((dps (getprop box :dump-properties)))
    (unless (null dps)
      (setq stream-attribute-list (append stream-attribute-list dps))))
  (unless (getf stream-attribute-list :package)
    (setf (getf stream-attribute-list :package) ':boxer))
  (writing-bin-stream (box stream)
                      (dump-attribute-list stream-attribute-list stream)
                      (dump-self box stream)))

(defun start-bin-file (stream)
  (clrhash *bin-dump-table*)
  (write-file-word bin-op-format-version stream)
  (dump-boxer-thing *version-number* stream))

(defun end-bin-file (stream)
  (write-file-word bin-op-eof stream)
  (close stream)
  ;; don't know about ExCl yet...
  #+(or lucid lispm)
  (and (system:file-stream-p stream) (truename stream)))

(defun enter-table (form &optional stream (explicit nil))
  (when explicit (write-file-word bin-op-table-store stream))
  (setf (gethash form *bin-dump-table*) *bin-dump-index*)
  (incf *bin-dump-index*))

(defun record-dumped-box (box) (setf (gethash box *dumped-box-table*) t))

(defun box-finished-dumping? (box) (gethash box *dumped-box-table*))

(defun dump-boxer-thing (thing stream &aux index)
  (cond;; these items are not mutable and have no
          ;; sharing issues, so we don't need preserve
          ;; eq-ness.
          ((integerp thing) (dump-fixnum thing stream))
          ((rationalp thing) (dump-rational thing stream))
          ((floatp thing) (dump-float thing stream))
          ((complexp thing) (dump-complex thing stream))
          ((cha? thing) (dump-cha thing stream))
          ;; begin table lookup items
          ((setq index (gethash thing *bin-dump-table*))
           ;; thing is EQ to something which has already been dumped
           (dump-table-lookup stream index))
          ((symbolp thing) (dump-symbol thing stream))
          ((stringp thing) (dump-string thing stream))
          ((consp thing) (dump-cons thing stream))
          ((graphics-sheet? thing) (dump-graphics-sheet thing stream))
          ((row? thing) (dump-row thing stream))
          ((box? thing) (dump-box thing stream))
          ((graphics-object? thing) (dump-graphics-object thing stream))
          ;; this has to be down here because PCL instances are implemented as
          ;; arrays in some implementations
          ((arrayp thing) (dump-array thing stream))
          (t (internal-dumping-error "Unknown object encountered"))))

(defun dump-attribute-list (plist stream)
  (let ((pkg (getf  plist ':package)))
    (and pkg (setq *bin-dump-package* (find-package pkg))))
  (write-file-word bin-op-file-property-list stream)
  ;; Put package prefixes on everything in the plist since it will be loaded
  ;; in the wrong package.  This way the symbols in the plist will always
  ;; be loaded into exactly the same package they were dumped from,
  ;; while the rest of the symbols in the file will be free to follow
  ;; the usual rules for intern.
  (let ((*bin-dump-package* nil))
    (dump-boxer-thing plist stream)))

(defun dump-table-lookup (stream index)
  (cond ((< index %%bin-op-im-arg-size)
         ;; will it fit into 12. bit immediate arg ?
         (write-file-word (dpb bin-op-table-fetch-immediate
                               %%bin-op-high index)
                          stream))
    (t
     (write-file-word bin-op-table-fetch stream)
     (dump-boxer-thing index stream))))

;;; The lisp package in Slime is called COMMON-LISP rather than LISP which
;;; causes problems in loading the dumped symbol into another lisp
;;; same problem for the mac....
#+(or Symbolics mcl)
(defvar *the-lisp-package* (find-package 'lisp))

#+(or Symbolics mcl)
(defun canonicalize-package-name (pkg)
  (if (eq pkg *the-lisp-package*)
    "LISP"
    (package-name pkg)))

(defun dump-symbol (symbol stream)
  (enter-table symbol)
  (cond ((null (symbol-package symbol))
         (write-file-word bin-op-package-symbol stream)
         (dump-boxer-thing 'nil stream))
    (t
     (let ((package-string #-(or Symbolics mcl)
                           (package-name (symbol-package symbol))
                           #+(or Symbolics mcl)
                           (canonicalize-package-name
                            (symbol-package symbol))))
       (cond ((null package-string)
              (write-file-word bin-op-symbol stream))
         (t
          (write-file-word bin-op-package-symbol stream)
          (dump-boxer-thing package-string stream))))))
  (dump-boxer-thing (symbol-name symbol) stream))

(defun dump-fixnum (num stream)
  (flet ((small-fix? (number)
                     (< (- %%bin-op-im-number-size) number %%bin-op-im-number-size))
         (dump-small-fixnum (number stream)
                            (write-file-word (dpb bin-op-number-immediate
                                                  %%bin-op-high
                                                  (ldb %%bin-op-low number))
                                             stream))
         (dump-large-fixnum (number stream)
                            (cond ((minusp number)
                                   (write-file-word bin-op-negative-fixnum stream)
                                   (let ((length (floor (+ (integer-length (- number))
                                                           15.) 16.)))
                                     (dump-boxer-thing length stream)
                                     (dotimes (i length)
                                       (write-file-word (ldb (byte 16. (* 16. i)) (- number))
                                                        stream))))
                              (t
                               (write-file-word bin-op-positive-fixnum stream)
                               (let ((length (floor (+ (integer-length number) 15.) 16.)))
                                 (dump-boxer-thing length stream)
                                 (dotimes (i length)
                                   (write-file-word (ldb (byte 16. (* 16. i)) number)
                                                    stream)))))))
        (if (small-fix? num)
          (dump-small-fixnum num stream)
          (dump-large-fixnum num stream))))

(defun dump-float (number stream)
  (cond ((>= number 0)
         (write-file-word bin-op-positive-float stream))
    (t
     (setq number (- number))
     (write-file-word bin-op-negative-float stream)))
  (multiple-value-bind (int exp)		; don't need the sign anymore
                       (integer-decode-float number)
                       (dump-boxer-thing int stream)
                       (dump-boxer-thing exp stream)))

(defun dump-rational (number stream)
  (write-file-word bin-op-rational stream)
  (dump-boxer-thing (numerator number) stream)
  (dump-boxer-thing (denominator number) stream))

(defun dump-complex (number stream)
  (write-file-word bin-op-complex stream)
  (dump-boxer-thing (realpart number) stream)
  (dump-boxer-thing (imagpart number) stream))

;; UTF-8 utilities
(defun utf-8-size (char)
  (let ((code (char-code char)))
    (cond ((<& code 128)  1)
      ((<& code 2048) 2)
      ((<& code #.(expt 2 16)) 3)
      (t 4))))

(defun utf-8-length (string)
  (with-summation
    (dotimes (i (length string))
      (sum (utf-8-size (char string i))))))

(defun utf-8-length-from-row-chas (row from to)
  (with-summation
    (do-row-chas ((c row :start from :stop to))
      (sum (utf-8-size c)))))

;; values byte1, byte2, byte3, byte4 anything other than byte1 can be NIL
(defun encode-utf-8 (code)
  (cond ((<& code 128)  code)
    ((<& code 2048)
     (values (dpb& (ldb& %utf-8-2byte-1stbyte-src-bytespec code)
                   %utf-8-2byte-1stbyte-dst-bytespec
                   %utf-8-2byte-start)
             (dpb& (ldb& %utf-8-last-byte-bytespec code)
                   %utf-8-last-byte-bytespec
                   %utf-8-more-bytes)))
    ((<& code #.(expt 2 16))
     (values (dpb& (ldb& %utf-8-3byte-1stbyte-src-bytespec code)
                   %utf-8-3byte-1stbyte-dst-bytespec
                   %utf-8-3byte-start)
             (dpb& (ldb& %utf-8-3byte-2ndbyte-src-bytespec code)
                   %utf-8-3byte-2ndbyte-dst-bytespec
                   %utf-8-more-bytes)
             (dpb& (ldb& %utf-8-last-byte-bytespec code)
                   %utf-8-last-byte-bytespec
                   %utf-8-more-bytes)))
    (t
     (values (dpb& (ldb& %utf-8-4byte-1stbyte-src-bytespec code)
                   %utf-8-4byte-1stbyte-dst-bytespec
                   %utf-8-4byte-start)
             (dpb& (ldb& %utf-8-4byte-2ndbyte-src-bytespec code)
                   %utf-8-4byte-2ndbyte-dst-bytespec
                   %utf-8-more-bytes)
             (dpb& (ldb& %utf-8-4byte-3rdbyte-src-bytespec code)
                   %utf-8-4byte-3rdbyte-dst-bytespec
                   %utf-8-more-bytes)
             (dpb& (ldb& %utf-8-last-byte-bytespec code)
                   %utf-8-last-byte-bytespec
                   %utf-8-more-bytes)))))

;; extend dump-string to use UTF-8 encoding for
;; unicode chars with char-codes > 128
(defun dump-string (string stream)
  (let* ((ulength (utf-8-length string))
         (slength (length string))
         (prev-byte nil))
    (dump-string-preamble ulength stream string)
    (do* ((i 0 (1+& i)))
      ((>=& i slength)
       (unless (null prev-byte) (write-file-word prev-byte stream)))
      (let ((code (char-code (char string i))))
        (multiple-value-bind (byte1 byte2 byte3 byte4)
                             (encode-utf-8 code)
                             (cond ((null byte2) ; ASCII char case
                                                 (cond ((null prev-byte) (setq prev-byte byte1))
                                                   (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                                       stream)
                                                      (setq prev-byte nil))))
                               ((null byte3) ; char code < 2048
                                             (cond ((null prev-byte)
                                                    (write-file-word (dpb& byte2 %%bin-op-top-half byte1)
                                                                     stream)
                                                    (setq prev-byte nil))
                                               (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                                   stream)
                                                  (setq prev-byte byte2))))
                               ((null byte4) ; 3 byte encoding
                                             (cond ((null prev-byte)
                                                    (write-file-word (dpb& byte2 %%bin-op-top-half byte1)
                                                                     stream)
                                                    (setq prev-byte byte3))
                                               (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                                   stream)
                                                  (write-file-word (dpb& byte3 %%bin-op-top-half byte2)
                                                                   stream)
                                                  (setq prev-byte nil))))
                               (t ; 4 byte encoding
                                  (cond ((null prev-byte)
                                         (write-file-word (dpb& byte2 %%bin-op-top-half byte1)
                                                          stream)
                                         (write-file-word (dpb& byte4 %%bin-op-top-half byte3)
                                                          stream)
                                         (setq prev-byte nil))
                                    (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                        stream)
                                       (write-file-word (dpb& byte3 %%bin-op-top-half byte2)
                                                        stream)
                                       (setq prev-byte byte4))))))))))



(defun dump-string-preamble (length stream &optional table-entry)
  (enter-table (or table-entry 'string))
  (cond ((< length %%bin-op-im-arg-size)
         (write-file-word (dpb bin-op-string-immediate %%bin-op-high length)
                          stream))
    (t
     (write-file-word bin-op-string stream)
     (dump-boxer-thing length stream))))

(defun dump-cons (cons stream)
  (flet ((cons-length (cons)
                      (do ((rest cons (cdr rest))
                           (l 0 (1+ l)))
                        ((or (null rest) (atom rest))
                         (if (null rest) l (values (1+ l) t))))))
        (enter-table cons)
        (multiple-value-bind (length dotted)
                             (cons-length cons)
                             (cond ((< length %%bin-op-im-arg-size)
                                    (write-file-word (dpb bin-op-list-immediate %%bin-op-high length)
                                                     stream))
                               (t
                                (write-file-word bin-op-list stream)
                                (dump-boxer-thing length stream)))
                             (dump-boxer-thing dotted stream)
                             (do ((l cons (if (atom l) l (cdr l)))
                                  (idx 0 (1+ idx)))
                               ((= idx length))
                               (dump-boxer-thing (if (and dotted (= idx (1- length)))
                                                   l
                                                   (car l))
                                                 stream)))))

;; we dump some lists by hand (usually plists) to
;; avoid unneccessarily CONsing them
(defun dump-list-preamble (length stream &optional (dotted nil))
  ;; temorary hack until the loader knows about reassembling these
  ;; plists and NOT to increment the load table index when doing so
  ;; this needs to STAY until EVERY reconstruction of the lists
  ;; dumped out using this preamble are reconstructed by hand and
  ;; NOT by using Load-List
  (enter-table 'plist)
  (cond ((< length %%bin-op-im-arg-size)
         (write-file-word (dpb bin-op-list-immediate %%bin-op-high length)
                          stream))
    (t
     (write-file-word bin-op-list stream)
     (dump-boxer-thing length stream)))
  (dump-boxer-thing dotted stream))

;;; We might want to use some array resources here to keep the consing down
;;; The Zippy Lisp implementation was able to call RETURN-ARRAY after it
;;; was done.  But no such luck in Common Lisp so at the moment, we are just
;;; CONSing them and then throwing them away

;;; Numeric arrays with element bit sizes of 1, 2, 4 and 8 will get packed into
;;; 16 bit words.  Anything else just gets shoved to dump-boxer-thing.
;;; Think about run length encoding later.  Probably wont matter much since the
;;; main offender (graphics-sheets) have their own dump method

(defun dump-array (array stream)
  (flet ((pack-word (fa start-index element-size)
                    (let ((word 0)
                          (fal (length fa)))
                      (dotimes (i (floor (/ 16. element-size)))
                        (let ((idx (+ start-index i)))
                          (setq word (dpb (if (>= idx fal) 0 (aref fa idx))
                                          (byte element-size (* i element-size))
                                          word))))
                      word)))
        (enter-table array)
        (multiple-value-bind (dimensions options)
                             (decode-array array)
                             (let ((length (array-total-size array))	;Flattened size
                                                                     (n-bits (array-bits-per-element array)))
                               (cond ((and (numberp n-bits)
                                           (<= n-bits 16.))
                                      (let ((flat-array (if (atom dimensions)
                                                          array
                                                          (make-array length
                                                                      :element-type
                                                                      `(unsigned-byte ,n-bits))))
                                            (word-length (floor (/ (+ (* length n-bits) 15.) 16.))))
                                        (write-file-word bin-op-initialize-and-return-numeric-array
                                                         stream)
                                        (dump-array-1 stream dimensions options)
                                        (dump-boxer-thing word-length stream)
                                        (dotimes (i word-length)
                                          (write-file-word
                                           (pack-word flat-array (* i (floor 16. n-bits)) n-bits)
                                           stream))))
                                 (t
                                  (write-file-word bin-op-initialize-and-return-array stream)
                                  (dump-array-1 stream dimensions options)
                                  (dump-boxer-thing length stream)
                                  (let ((q-array (if (atom dimensions)
                                                   array
                                                   (make-array length :displaced-to array))))
                                    (dotimes (i length)
                                      (dump-boxer-thing (aref q-array i) stream)))))))))

(defun dump-array-1 (stream dimensions options)
  (write-file-word (dpb bin-op-array %%bin-op-high (/ (length options) 2))
                   stream)
  (dump-boxer-thing dimensions stream)
  (dolist (form options)
    (dump-boxer-thing form stream)))

;;; array-bits-per-element is implementation-dependent
;;; The default is to signal an error
#-(or lucid symbolics mcl lispworks)
(defun array-bits-per-element (array)
  (declare (ignore array))
  (cerror "Return number of Bits Per element of this array"
          "ARRAY-BITS-PER-ELEMENT is not defined for ~A ~A"
          (lisp-implementation-type) (lisp-implementation-version))
  (format t "Number of Bits Per Element (or NIL): ")
  (read-from-string (read-line)))

#+symbolics
(defun array-bits-per-element (array)
  (let ((et (si:array-type array)))
    (cdr (fast-assq et si::array-bits-per-element))))

;;; Note that there IS a LUCID::ARRAY-BITS-PER-ELEMENT but that it doesn't
;;; quite do what we want (returning 32 for T type arrays for example)
#+(or lucid mcl lispworks)                        ; I guess this is OK for MCL +++
(defun array-bits-per-element (array)
  (let ((et (array-element-type array)))
    (cond ((eq et 'bit) 1)
      ((and (consp et) (eq (car et) 'unsigned-byte))
       (cadr et))
      (t nil))))

;;; NO DISPLACED Arrays !!!!!!!
;;; There isn't a Common Lisp Predicate (or accessors) for them and
;;; I Don't feel like looking at implementation dependent frobs for something
;;; that Boxer doesn't currently (and probably never will) use...

(defun decode-array (array &aux dimensions options)
  (declare (values dimensions array-options))
  (setq dimensions (if (= (array-rank array) 1) (length array)
                     (array-dimensions array)))
  (let ((type (array-element-type array)))
    (or (eq type t)
        (setq options `(:element-type ,type . ,options))))
  (when (array-has-fill-pointer-p array)
    (setq options `(:fill-pointer ,(fill-pointer array) . ,options)))
  (when (adjustable-array-p array)
    (setq options `(:adjustable t . ,options)))
  (values dimensions options))

;;;; Boxer Specific Objects

;;; chas

(defun dump-cha (cha stream)
  (write-file-word (dpb bin-op-cha-immediate %%bin-op-high (char-code cha))
                   stream))

; now we don't have to worry about bits - in fact they conflict with larger unicode chars
;(defun dump-cha (cha stream)
;  (unless (< (lisp::char-bits cha) 16)
;    (warn "~C has more than 4 control-bits, only dumping out the low 4"))
;  (write-file-word (dpb bin-op-cha-immediate
;			%%bin-op-high
;			(dpb (lisp::char-bits cha)
;			     %%bin-op-im-cha-bits (char-code cha)))
;		    stream))

;;; we don't have to worry about fonts anymore
;(defun dump-cha (cha stream)
;  (flet ((im-char? (c)
;	   (and (< (char-bits c)  4)
;		(null (char-font-family c)))))
;   (if (im-char? cha)
;	(write-file-word (dpb bin-op-cha-immediate
;			 %%bin-op-high
;			 (dpb (map-style-to-index (char-style cha))
;			      %%bin-op-im-cha-style
;			      (dpb (char-bits cha)
;				   %%bin-op-im-cha-bits
;				   (char-code cha))))
;		    stream)
;	(progn
;	  (write-file-word bin-op-fat-cha
;		      stream)
;	  (write-file-word (dpb (map-family-to-index (char-font-family cha))
;			 %%bin-op-fat-cha-family
;			 (dpb (map-style-to-index (char-style cha))
;			      %%bin-op-fat-cha-style
;			      (dpb (char-bits cha)
;				   %%bin-op-fat-cha-bits
;				   (char-code cha))))
;		    stream)))))

;;; rows

(defun dump-row (row stream)
  (enter-table row stream t)
  (dump-self row stream))

;; we'll want to increment the file version number and force all rows
;; to use the new character compacting routine
;; But this will mean that files written using the latest version will be
;; unloadable by older version so, for now, use this variable
(defvar *dump-all-rows-using-fonts* t)

#|
(eval-when (load)
  (format t "~&~&Remember to flush dump font row toggling ~&~&")
)

(defun toggle-font-dumping ()
  (cond ((null *dump-all-rows-using-fonts*)
         (setq *version-number* 12 *dump-all-rows-using-fonts* t))
        (t
         (setq *version-number* 11 *dump-all-rows-using-fonts* nil))))
|#

(defmethod dump-self ((self row) stream)
  (let ((length (length-in-chas self)))
    (cond ((< length %%bin-op-im-arg-size)
           (write-file-word (dpb bin-op-row-immediate %%bin-op-high length)
                            stream))
      (t
       (write-file-word bin-op-row stream)
       (dump-boxer-thing length stream)))
    (cond ((null *dump-all-rows-using-fonts*)
           ;; old style row format with NO fonts and fat fluffy chars
           (do-row-chas ((cha self))
             (cond ((or (cha? cha) (box? cha))
                    (dump-boxer-thing cha stream))
               (t (internal-dumping-error
                   "Non box or char encountered in row")))))
      (t ;; new style row format WITH fonts
       ;; do look ahead to collapse as many pairs
       ;; of characters as we can into successive single 16-bit words
       (let ((last-dumped-pos 0)
             (current-pos 0))
         (do-row-chas ((cha self))
           (cond ((box? cha)
                  ;; dump out a string if we've gone past some chars
                  (unless (=& current-pos last-dumped-pos)
                    (dump-row-chas-as-string self stream
                                             last-dumped-pos current-pos))
                  (dump-boxer-thing cha stream)
                  (incf& current-pos)
                  (setq last-dumped-pos current-pos))
             ((cha? cha) (incf& current-pos))
             (t (internal-dumping-error
                 "Non box or char encountered in row"))))
         ;; check for any undumped chars and dump them
         (unless (=& current-pos last-dumped-pos)
           (dump-row-chas-as-string self stream last-dumped-pos current-pos)))
         ;; now dump the font info
         (let ((fds (row-fds self)))
           (cond ((null fds) (dump-boxer-thing nil stream))
             (t (dump-list-preamble (length fds) stream)
                (dolist (fd fds) (dump-font-descriptor fd stream)))))))))

;; expand this to dump chas out in UTF-8 format to handle large (unicode)
;; chars (usually as a result of pasting from other apps)
(defun dump-row-chas-as-string (row stream from &optional (to (length-in-chas row)))
  (let ((ulength (utf-8-length-from-row-chas row from to))
        (prev-byte nil))
    (dump-string-preamble ulength stream)
    (do-row-chas ((c row :start from :stop to))
      (let ((code (char-code c)))
        (multiple-value-bind (byte1 byte2 byte3 byte4)
                             (encode-utf-8 code)
                             (cond ((null byte2) ; ASCII char case
                                                 (cond ((null prev-byte) (setq prev-byte byte1))
                                                   (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                                       stream)
                                                      (setq prev-byte nil))))
                               ((null byte3) ; char code < 2048
                                             (cond ((null prev-byte)
                                                    (write-file-word (dpb& byte2 %%bin-op-top-half byte1)
                                                                     stream)
                                                    (setq prev-byte nil))
                                               (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                                   stream)
                                                  (setq prev-byte byte2))))
                               ((null byte4) ; 3 byte encoding
                                             (cond ((null prev-byte)
                                                    (write-file-word (dpb& byte2 %%bin-op-top-half byte1)
                                                                     stream)
                                                    (setq prev-byte byte3))
                                               (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                                   stream)
                                                  (write-file-word (dpb& byte3 %%bin-op-top-half byte2)
                                                                   stream)
                                                  (setq prev-byte nil))))
                               (t ; 4 byte encoding
                                  (cond ((null prev-byte)
                                         (write-file-word (dpb& byte2 %%bin-op-top-half byte1)
                                                          stream)
                                         (write-file-word (dpb& byte4 %%bin-op-top-half byte3)
                                                          stream)
                                         (setq prev-byte nil))
                                    (t (write-file-word (dpb& byte1 %%bin-op-top-half prev-byte)
                                                        stream)
                                       (write-file-word (dpb& byte3 %%bin-op-top-half byte2)
                                                        stream)
                                       (setq prev-byte byte4))))))))
    ;; handle any leftovers
    (unless (null prev-byte) (write-file-word prev-byte stream))))

(defun dump-font-descriptor (fd stream)
  (dump-list-preamble 3 stream)
  (dump-boxer-thing (bfd-cha-no fd) stream)
  (dump-font (bfd-font-no fd) stream)
  (dump-boxer-thing (if (>=& *version-number* 12)
                      (pixel-dump-value (bfd-color fd))
                      (pixel-rgb-values (bfd-color fd)))
                    stream))

;; the dump format for fonts is a list consisting of:
;; 1. a name (string)
;; 2. a size (fixnum)
;; 3. optional series of style keywords (symbols)

;; sgithens - July 2020. Notes on fixing loss of font size during file saving.
;;
;; Internally boxer keeps an array of font sizes in a zero indexed list:  (vector 8 9 10 12 14 16 20 24 28 32 40 48 56 64)
;; So, index 3 would be size 12, 4 would be 14, etc.
;;
;; Now, in the code there is an option to save fonts using either the “real” font size, or the array location.
;; So, Arial of size 12, would be “Arial 12” using real sizes, and “Arial 3” would be the same using the relative index
;; size.  As the code was, when boxer saved a file it was using the internal version “Arial 3”, but when it loaded a saved
;; file was expecting the real size version “Arial 12”. We dont’have any fonts under size 8 which is why it would always
;; switch them to a tiny font when you reopened the file.
;;
;; To fix this, I’ve done the following. First, I toggled the save format, so that going forward if we save new files,
;; it will use the real size information. This makes a lot more sense, especially int he future when we’ll have font sizes
;; in between those of the predefined sizes, espeically since vector based fonts will be able to have in between sizes.
;; And then, in the opening code, I check to see the size. Since the smallest fonts we have are size 8 anyways, if there
;; is a number smaller than this, I’m treating it as an array index in to the relative sizes, and if it’s larger then that
;; then I use the real font size.
;;
;; The more correct thing to do will be to bump the version of the boxer file format and use that as a check, which I
;; should look at. But for now this works pretty well.
;;
;; sgithens - May 08, 2021 A further update.
;; As of now, we are no longer using relative font sizes in the Boxer font infrastructure.
;; Font sizes are now stored as their actual sizes, so we can always write out the fonts name, size, and styles as is.
;; We will keep this dump relative variable commented out here for future reference, and remember that to support older
;; saved files we still only allow fonts sized 8 or larger, as sizes 1 thru 7 were indexes into the list of supported
;; font sizes, which for a while we will still check when loading fonts. But for now, we will always save the actual size.
;;
;; (defvar *dump-relative-font-sizes?* nil)

(defun dump-font (font stream)
  (let ((font-id (assoc font *dumping-font-alist* :test #'=)))
    (cond ((null font-id)
           (let ((styles (font-styles font)))
             ;; save away this particular font
             (push (cons font *bin-dump-index*) *dumping-font-alist*)
             ;; fake a dump list
             (dump-list-preamble (+ 2 (length styles)) stream)
             (dump-boxer-thing (font-name font) stream)
             (dump-boxer-thing (font-size font) stream)
             (dolist (style styles) (dump-boxer-thing style stream))))
      (t
       (dump-table-lookup stream (cdr font-id))))))

(defmethod dump-self ((self name-row) stream)
  (let ((length (length-in-chas self)))
    (cond ((< length %%bin-op-im-arg-size)
           (write-file-word (dpb bin-op-name-row-immediate
                                 %%bin-op-high length)
                            stream))
      (t
       (write-file-word bin-op-name-row stream)
       (dump-boxer-thing length stream)))
    (dump-boxer-thing (cached-name self) stream)
    (do-row-chas ((cha self))
      (cond ((or (cha? cha) (box? cha))
             (dump-boxer-thing cha stream))
        (t (internal-dumping-error "Non char encountered in name-row"))))))



;;; boxes

;; not quite right in TCP case, need to figure out direction of the stream at a
;; higher level to know whether to use bytes-received or bytes-transmitted slots.
;; For now, assume output because that is the only current possibility
#+mcl
(defun file-stream-position (stream)
  (etypecase stream
             (ccl::file-stream (file-length stream))
             (ccl::tcp-stream  (boxnet::writing-stream-position stream))))

#+lispworks
(defun file-stream-position (stream)
  (etypecase stream
             (hcl::file-stream (file-length stream))
             (comm::socket-stream (boxnet::writing-stream-position stream))))

#-(or mcl lispworks)
(defun file-stream-position (stream) (file-length stream))

(defun dump-box (box stream)
  (enter-table box stream t)
  (setq *current-dumping-box* box)
  (dump-self box stream)
  (when *file-system-verbosity*
    (cond ((not (null *file-status-line-update-function*))
           (funcall *file-status-line-update-function* stream))
      ((not (null  *status-line-saving-format-string*))
       (status-line-display 'saving-box
                            (format nil *status-line-saving-format-string*
                                    (file-stream-position stream))))))
  (record-dumped-box box))

;;; The main box method is responsible for dumping a property list
;;; which is passed to a box-load-handler along with the result
;;; of MAKE-INSTANCE.
;;; See the dump-plist method.  The main method returns a standard plist
;;; for all boxes and more specific methods can append more keyword
;;; pairs to this plist
;;; Specific methods for particular kinds of boxes should dump the
;;; bin-op corresponding to the TYPE of box, then call the main method,
;;; then do whatever (type specific) stuff it has to do, finally finishing
;;; with a end-of-box bin-op


;;; If we are SAVEing, make sure that the rows have been read in
(defmethod rows-for-file ((box box))
  (when (and (null (slot-value box 'first-inferior-row)) (storage-chunk? box))
    (fill-box-from-server box))
  (rows box))

(defmethod rows-for-file ((box port-box))
  nil)

;; this may change when we allow multiple rows for a closet
(defmethod closets-for-file ((box box))
  (slot-value box 'closets))

;; the initial 3 consists accounts for :rows, :display-style-list and :closets
(defvar *vanilla-box-initial-dump-plist-item-length* 3)

;;; we do this by hand to avoid CONSing the plist at dump time
(defun dump-box-plist (box stream)
  (let ((length (dump-plist-length box)))
    ;; first, write out the list opcode
    (dump-list-preamble length stream)
    ;; then let the box do the rest
    (dump-plist-internal box stream)))

(defmethod dump-plist-internal ((self box) stream)
  (let ((name (slot-value self 'name))
        (exports (slot-value self 'exports))
        (boxtop-prop (getprop self :boxtop))
        (gi (slot-value self 'graphics-info))
        (dsl (display-style-list self)))
    (unless (null name)
      (dump-boxer-thing :name stream) (dump-boxer-thing name stream))
    (unless (null exports)
      (dump-boxer-thing :exports stream) (dump-boxer-thing exports stream))
    (unless (null boxtop-prop)
      (dump-boxer-thing :boxtop stream)
      (if (graphics-sheet? boxtop-prop)
        (dump-boxer-thing :standard stream)
        (dump-boxer-thing boxtop-prop stream)))
    (unless (null gi)
      (dump-boxer-thing :graphics-info stream)
      (dump-boxer-thing gi stream))
    (unless (zerop& (slot-value self 'flags))
      (dump-boxer-thing :flags stream)
      (dump-boxer-thing (flags-for-dumping self (slot-value self 'flags)) stream))
    (dump-storage-chunk-plist-items self stream)
    ;; loadable modules
    (dolist (hook *dump-plist-internal-hook*) (funcall hook self stream))
    (cond ((no-inferiors-for-file? self)
           ;; Must make sure the display style gets dumped out
           (dump-boxer-thing :display-style-list stream)
           (dump-canonicalized-display-style self stream))
      (t
       (unless (standard-display-style? dsl)
         (dump-boxer-thing :display-style-list stream)
         (dump-canonicalized-display-style self stream))
       (dump-boxer-thing :closets stream)
       (dump-boxer-thing (slot-value self 'closets) stream)
       (dump-boxer-thing :rows stream)
       (dump-boxer-thing (rows-for-file self) stream)))))

;; make sure the file-modified flag is cleared
(defun flags-for-dumping (box flags)
  (declare (ignore box))
  (set-box-flag-file-modified? flags nil))

#|
  (if (or (eq box *outermost-dumping-box*) (in-bfs-environment?)
          (box-flag-read-only-box? flags))
|#



    (defmethod dump-plist-internal ((self port-box) stream)
      (cond ((and (in-bfs-environment?) (cross-file-port? self))
             ;; should shrink it before dumping out display-style in the
             ;; vanilla box dump-plist-internal method
             (shrink self)
             (call-next-method)
             (dump-cross-file-port-reference self stream))
        ((circular-port? self)
         (call-next-method)
         (let* ((target (slot-value self 'ports))
                (index (gethash target *bin-dump-table*)))
           (cond ((and index (not (box-finished-dumping? target)))
                  ;; we are still dumping the inferiors of the target
                  (dump-boxer-thing :circular-port-reference stream)
                  (dump-boxer-thing index stream))
             (t
              ;; it's circular but still ok to dump out the target
              (dump-boxer-thing :port-target stream)
              (dump-boxer-thing (slot-value self 'ports) stream)))))
        (t
         (call-next-method)
         (when (or (cracked-port? self) (cross-file-port? self))
           (dump-boxer-thing :cracked-port stream)
           (dump-boxer-thing T stream))
         (dump-boxer-thing :port-target stream)
         (dump-boxer-thing (slot-value self 'ports) stream))))

    (defmethod dump-plist-length ((self box))
      (let ((plist-half-length *vanilla-box-initial-dump-plist-item-length*))
        (unless (null (slot-value self 'name))
          (incf& plist-half-length))
        (unless (null (slot-value self 'exports))
          (incf& plist-half-length))
        (unless (zerop& (slot-value self 'flags))
          (incf& plist-half-length))
        (unless (null (getprop self :boxtop))
          (incf& plist-half-length))
        (unless (null (slot-value self 'graphics-info))
          (incf& plist-half-length))
        (incf& plist-half-length (storage-chunk-plist-half-length self))
        (cond ((no-inferiors-for-file? self)
               ;; this is the case where we decide NOT to dump out
               ;; the rows or closets, but we DO dump out a display-style list
               (decf& plist-half-length 2))
          ((standard-display-style? (display-style-list self))
           ;; check to see if the display-style-list needs to be dumped out
           (decf& plist-half-length)))
        (+& (*& plist-half-length 2)
            ;; add in any items from loaded modules...
            (with-summation
              (dolist (hook *dump-plist-length-hook*)
                (sum (funcall hook self)))))))

    (defmethod dump-plist-length ((self port-box))
      (+& (call-next-method)
          (if (and (not (circular-port? self))
                   (not (and (in-bfs-environment?) (cross-file-port? self)))
                   (or (cracked-port? self) (cross-file-port? self)))
            4 2)))

    ;; use this to avoid dumping display styles if we don't have to since even the
    ;; standard style will cost at least 5 bytes
    (defun standard-display-style? (ds)
      (and (eq (display-style-style ds) :normal)
           (null (display-style-fixed-wid ds)) (null (display-style-fixed-hei ds))
           (null (display-style-graphics-mode? ds)) (null (display-style-border-style ds))))

    (defmethod dump-canonicalized-display-style ((self box) stream)
      (let ((ds (slot-value self 'display-style-list))
            (shrink-dump? (no-inferiors-for-file? self)))
        (cond ((and (null (display-style-graphics-mode? ds))
                    (null (display-style-border-style ds)))
               (dump-list-preamble 3 stream)
               (dump-boxer-thing (if (and shrink-dump?
                                          (not (eq (display-style-style ds)
                                                   ':supershrunk)))
                                   ':shrunk
                                   (display-style-style ds))
                                 stream)
               (dump-boxer-thing (display-style-fixed-wid ds) stream)
               (dump-boxer-thing (display-style-fixed-hei ds) stream))
          (t
           (dump-list-preamble 5 stream)
           (dump-boxer-thing (if (and shrink-dump?
                                      (not (eq (display-style-style ds)
                                               ':supershrunk)))
                               ':shrunk
                               (display-style-style ds))
                             stream)
           (dump-boxer-thing (display-style-fixed-wid ds) stream)
           (dump-boxer-thing (display-style-fixed-hei ds) stream)
           (dump-boxer-thing (display-style-graphics-mode? ds) stream)
           (dump-boxer-thing (display-style-border-style ds) stream)))))

;; Obsolete, use dump-canonicalized-display-style
;(defun canonicalize-display-style (ds)
;  (cond ((and (null (display-style-graphics-mode? ds))
;	      (null (display-style-border-style ds)))
;	 (list (display-style-style ds)
;	       (display-style-fixed-wid ds)
;	       (display-style-fixed-hei ds)))
;	(t
;	 (list (display-style-style ds)
;	       (display-style-fixed-wid ds)
;	       (display-style-fixed-hei ds)
;	       (display-style-graphics-mode? ds)
;	       (display-style-border-style ds)))))


(defmethod dump-self ((self doit-box) stream)
  (write-file-word bin-op-doit-box stream)
  (dump-box-plist self stream)
  (write-file-word bin-op-end-of-box stream))

(defmethod dump-self ((self data-box) stream)
  (write-file-word bin-op-data-box stream)
  (dump-box-plist self stream)
  (write-file-word bin-op-end-of-box stream))

;; this needs some sort of consistency check.  See the old method for details
(defmethod dump-self ((self port-box) stream)
  (write-file-word bin-op-port-box stream)
  (dump-box-plist self stream)
  (write-file-word bin-op-end-of-box stream))

;; the old port-box method
;;(DEFMETHOD (PORT-BOX :DUMP-SELF) (STREAM)
;;  ;; all we have to do now is to dump the ported to box
;;  (COND ((NULL PORTS) (FERROR "Can't find ported to box"))
;;	((TELL PORTS :SUPERIOR? *OUTERMOST-DUMPING-BOX*)
;;	 (DUMP-BOXER-THING PORTS STREAM))
;;	(T (FERROR "The ported to box, ~S, will not get dumped" PORTS))))

#|  no more sprtie boxes....
(defmethod dump-self ((self sprite-box) stream)
  (write-file-word bin-op-sprite-box stream)
  (dump-box-plist self stream)
  (write-file-word bin-op-end-of-box stream))
|#




;;; graphics stuff

(defun dump-graphics-object (turtle stream)
  (enter-table turtle stream t)
  (write-file-word bin-op-turtle stream)
;  (dump-boxer-thing (dump-plist turtle) stream) ; the old version
  (dump-graphics-object-plist turtle stream))

(defun dump-graphics-object-plist (go stream)
  (let ((length (dump-plist-length go)))
    ;; first, write out the list opcode
    (dump-list-preamble length stream)
    ;; then let the graphics object do the rest
    (dump-plist-internal go stream)))

;;;
;;; The Dump-Plist-Length methods should be...
;;; (+ (2 * <number of slots to be dumped>) (call-next-method))
;;;
;;; The Dump-Plist-Internal methods should (call-next-method)
;;; and then dump out their own slots as successive keyword/value pairs
;;;

(defmethod dump-plist-length ((self graphics-object))
  ;(declare (ignore self))
  8)

;;; this is the core method, as such, it is responsible for
;;; dumping any pre/post-ambles

(defmethod dump-plist-internal :before ((self graphics-object) stream)
  (dump-boxer-thing 'type stream)
  (dump-boxer-thing (class-name (class-of self)) stream))

(defmethod dump-plist-internal ((self graphics-object) stream)
  (dump-boxer-thing 'x-position stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'x-position)) stream)
  (dump-boxer-thing 'y-position stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'y-position)) stream)
  (dump-boxer-thing 'subsprites stream)
  (dump-boxer-thing (slot-value self 'subsprites) stream))

(defmethod dump-plist-length ((self button))
  (+ 4 (call-next-method)))

(defmethod dump-plist-internal ((self button) stream)
  (call-next-method)
  (dump-boxer-thing 'shape stream)
  (dump-graphics-list (box-interface-value (slot-value self 'shape)) stream)
  (dump-boxer-thing 'save-under stream)
  (dump-boxer-thing (if (eq (slot-value self 'save-under) 'xor-redraw)
      'xor-redraw
      'save-under)
        stream))

(defmethod dump-plist-length ((self graphics-cursor))
  (+ 10 (call-next-method)))

(defmethod dump-plist-internal ((self graphics-cursor) stream)
  (call-next-method)
  (dump-boxer-thing `shown? stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'shown?)) stream)
  (dump-boxer-thing `pen stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'pen)) stream)
  (dump-boxer-thing `pen-width stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'pen-width)) stream)
  (dump-boxer-thing `pen-color stream)
  (dump-boxer-thing (canonicalize-pixel-color
                     (box-interface-value (slot-value self 'pen-color)))
                    stream)
  (dump-boxer-thing `type-font stream)
  (if *dump-all-rows-using-fonts*
    (dump-font (box-interface-value (slot-value self 'type-font)) stream)
    (dump-boxer-thing (box-interface-value (slot-value self 'type-font)) stream)))

;; check for private graphics lists
(defmethod dump-plist-length ((self turtle))
  (let ((pgl (slot-value self 'private-gl)))
    (+ (cond ((zerop (graphics-command-list-fill-pointer pgl)) 6)
             (t 8))
       (call-next-method))))

(defmethod dump-plist-internal ((self turtle) stream)
  (dump-boxer-thing 'heading stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'heading)) stream)
  (dump-boxer-thing 'home-position stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'home-position))
        stream)
  (dump-boxer-thing 'sprite-size stream)
  (dump-boxer-thing (box-interface-value (slot-value self 'sprite-size))
        stream)
  (let ((pgl (slot-value self 'private-gl)))
    (unless (zerop (graphics-command-list-fill-pointer pgl))
      (dump-boxer-thing 'private-gl stream)
      (dump-graphics-list pgl stream)))
  ;; sprite size needs to be dumped/loaded BEFORE save-under
  (call-next-method))

(defun dont-dump-picture? (sheet)
  (null (graphics-sheet-bit-array sheet)))

(defun dump-graphics-sheet (sheet stream)
  ;; no need to make the table bigger since graphics
  ;; sheets should NOT be appearing in more than one place
  ;; the (enter-table sheet) should be removed in the next version
  ;; along with the corresponding table store in loader.lisp
  (enter-table sheet)
  (write-file-word bin-op-graphics-sheet stream)
  (dump-graphics-sheet-plist sheet stream)
  ;;
  ;; Pictures are now dumped in the plist
  ;; leave this here so we know how things used to work if there
  ;; are problems with the loading of old files
;  (unless (dont-dump-picture? sheet)
;    (dump-picture #+lispm (graphics-sheet-bit-array sheet)
;		  ;; need to chase some pointers to
;		  ;; get at the REAL data
;		  #+X (pixrect::mpr_data.md-image
;			 (pixrect::pixrect.pr-data
;			     (graphics-sheet-bit-array sheet)))
;		  #+clx
;		  (car (bw::image-xy-bitmap-list
;			(bw::get-image
;			 (graphics-sheet-bit-array sheet)
;			 :x 0 :y 0
;			 :width (graphics-sheet-draw-wid sheet)
;			 :height (graphics-sheet-draw-hei sheet)
;			 :format :xy-pixmap)))
;		  (graphics-sheet-draw-wid sheet)
;		  (graphics-sheet-draw-hei sheet)
;		  stream))
  )

(defun dump-graphics-sheet-plist (sheet stream)
  (let ((plist-length 6))
    (unless (null (graphics-sheet-graphics-list sheet)) (incf& plist-length 2))
    ;; **OLD** style, leave here in case we have to debug the loader
    ;(when (dont-dump-picture? sheet) (incf& plist-length 2))
    (unless (null (graphics-sheet-background sheet)) (incf& plist-length 2))
    (unless (or (null (graphics-sheet-bit-array sheet))
                (not (graphics-sheet-bit-array-dirty? sheet)))
      (incf& plist-length 2))
    ;; now the length has been adjusted...
    (dump-list-preamble plist-length stream)
    ;; now dump the info (first 3 key/value pairs are  required)
    (dump-boxer-thing :draw-wid stream)
    (dump-boxer-thing (graphics-sheet-draw-wid sheet) stream)
    (dump-boxer-thing :draw-hei stream)
    (dump-boxer-thing (graphics-sheet-draw-hei sheet) stream)
    (dump-boxer-thing :draw-mode stream)
    (dump-boxer-thing (graphics-sheet-draw-mode sheet) stream)
    ;; now see what else there is to dump
    (unless (null (graphics-sheet-graphics-list sheet))
      (dump-boxer-thing :graphics-list stream)
      ;; might want to be smarter about this and only dump
      ;; as much of the graphics-list as is currently active
      (dump-graphics-list (graphics-sheet-graphics-list sheet) stream))
    ;; **OLD** style, leave here in case we have to debug the loader
    ;    (when (dont-dump-picture? sheet)
    ;      (dump-boxer-thing :picture-was-not-dumped stream)
    ;      (dump-boxer-thing T stream))
    (unless (null (graphics-sheet-background sheet))
      (dump-boxer-thing :background stream)
      (if (>=& *version-number* 12)
        (dump-boxer-thing (pixel-dump-value (graphics-sheet-background sheet)) stream)
        (dump-boxer-thing (canonicalize-pixel-color
                            (graphics-sheet-background sheet)
                            (graphics-sheet-superior-box sheet)) stream)))
    (unless (or (null (graphics-sheet-bit-array sheet))
                (not (graphics-sheet-bit-array-dirty? sheet)))
      (dump-boxer-thing :pixmap stream)
      (dump-pixmap (graphics-sheet-bit-array sheet) stream))
    plist-length))

(defun dump-graphics-list (gl stream)
  (declare (simple-vector gl))
  (let* ((fp (storage-vector-active-length gl))
          (length (length gl))
          (contents (%sv-contents gl))
          (dump-length (min& (max& 8 (expt 2 (integer-length fp)))
                            (length contents))))
    (enter-table gl)
    (multiple-value-bind (dims options)
                          (decode-array gl)
                          (write-file-word bin-op-initialize-and-return-array stream)
                          (dump-array-1 stream dims options)
                          (dump-boxer-thing length stream)
                          ;; special case the display list by
                          ;; faking a dump-array of smaller size...
                          (enter-table contents)
                          (write-file-word bin-op-initialize-and-return-array stream)
                          (dump-array-1 stream dump-length nil)
                          (dump-boxer-thing dump-length stream)
                          (dotimes (j dump-length)
                            (let ((element (aref contents j)))
                              (if (typep element 'simple-vector) ; looks like a graphics command...
                                (dump-graphics-command element stream)
                                ;; otherwise, be generic
                                (dump-boxer-thing element stream))))
                          ;; now dump the other parts of the graphics list
                          ;; some of them need to be canonicalized before dumping
                          (dump-boxer-thing (%sv-fill-pointer gl) stream)
                          (dump-boxer-thing (graphics-command-list-agent gl) stream)
                          (dump-boxer-thing (canonicalize-file-alu (graphics-command-list-alu gl))
                                            stream)
                          (dump-boxer-thing (graphics-command-list-pen-width gl) stream)
                          (if (>=& *version-number* 12)
                            (dump-font (graphics-command-list-font-no   gl) stream)
                            (dump-boxer-thing (graphics-command-list-font-no   gl) stream))
                          (if (>=& *version-number* 12)
                            (dump-boxer-thing (pixel-dump-value
                                              (or (graphics-command-list-pen-color gl)
                                                  *background-color*))
                                              stream)
                            (dump-boxer-thing (canonicalize-pixel-color
                                              (or (graphics-command-list-pen-color gl)
                                                  *background-color*))
                                              stream))
                          (dump-boxer-thing (graphics-command-list-hidden gl) stream))))


;; This is the core of dumping window system dependent bitmapped data
;; All implementation MUST first dump a keyword used by the loader
;; to dispatch to the right load method.  In the future we might want
;; to be smarter or more flexible about which dump method to use including
;; using off the shelf GIF or JPEG encoders
(defun dump-pixmap (pixmap stream)
  (enter-table pixmap)
  (write-file-word bin-op-pixmap stream)
  (let ((depth (offscreen-bitmap-depth pixmap)))
    (case depth
      (1 (dump-1-bit-pixmap pixmap stream))
      ((2 3 4 5 6 7) (dump-8-bit-pixmap pixmap stream))
      (8 #+mcl (fast-mac-dump-8-bit-pixmap pixmap stream)
          #-mcl (dump-8-bit-pixmap pixmap stream))
      ((16 24 32) (dump-true-color-pixmap pixmap stream))
      (t (error "Don't know how to dump out ~D bit pixmaps" depth)))))

(defun dump-8-bit-pixmap (pixmap stream)
  (dump-boxer-thing '8-bit-run-length-encoded stream)
  (let ((pixdata (offscreen-bitmap-image pixmap))
        (width (offscreen-bitmap-width pixmap))
        (height (offscreen-bitmap-height pixmap))
        (remap-idx 0)
        (colormap nil))
    (declare (list colormap) (fixnum width height remap-idx))
    ;; dump out width and height.  This can usually be inferred from the
    ;; containing graphics-sheet's draw-wid/hei but we do it here as
    ;; well to support the future possibility of the underlying bitarray
    ;; to be larger (to allow for smooth scrolling)
    (dump-boxer-thing width stream) (dump-boxer-thing height stream)
    ;; first pass through to generate the colormap
    (dotimes& (y height)
      (dotimes& (x width)
        (let* ((pix (image-pixel x y pixdata))
                (existing (assoc pix colormap :test #'=)))
          (when (null existing)
            (push (list pix (pixel-rgb-values pix) remap-idx) colormap)
            (incf& remap-idx)))))
    ;; now dump out the colormap (we fake a dump-array)
    ;; first flip the colormap so it is ordered in increasing remap-idx's
    (setq colormap (nreverse colormap))
    (enter-table colormap)
    (write-file-word bin-op-initialize-and-return-array stream)
    (let ((lc (length colormap)))
      (dump-array-1 stream lc nil) (dump-boxer-thing lc stream)
      (do* ((j 0 (1+& j))
            (colors colormap (cdr colors))
            (color (cadr (car colors)) (cadr (car colors))))
        ((>=& j lc))
        (dump-boxer-thing color stream)))
    ;; now dump out the pix data as run length encoded words
    (let ((current-byte (image-pixel 0 0 pixdata)) (current-count 0))
      (declare (fixnum current-byte current-count))
      (dotimes& (y height)
        (dotimes& (x width)
          (let ((pix (image-pixel x y pixdata)))
            (cond ((or (=& current-count 255)
                        (not (=& pix current-byte)))
                    ;; write out a word as high byte = count, low byte = pixel
                    (write-file-word (dpb current-count %%bin-op-top-half
                                          (caddr (assoc current-byte colormap)))
                                    stream)
                    (setq current-byte pix current-count 1))
              ((=& pix current-byte) (incf& current-count))
              (t (error "Bad case in dumping bitmap (byte = ~D, count = ~D"
                        current-byte current-count))))))
      ;; finally write out the last word
      (write-file-word (dpb current-count %%bin-op-top-half
                            (caddr (assoc current-byte colormap)))
                        stream))))

#+mcl
(defun fast-mac-dump-8-bit-pixmap (pixmap stream)
  (dump-boxer-thing '8-bit-run-length-encoded stream)
  (let* ((width (offscreen-bitmap-width pixmap))
          (height (offscreen-bitmap-height pixmap))
          (pixdata (get-gworld-pixmap pixmap))
          (row-bytes (ldb& #.(byte 14 0) (ccl::rref pixdata :pixmap.rowbytes)))
          (pix-addr (get-pix-base-addr pixdata))
          (remap-idx 0)
          (colormap nil))
    (declare (list colormap) (fixnum width height remap-idx))
    ;; dump out width and height.  This can usually be inferred from the
    ;; containing graphics-sheet's draw-wid/hei but we do it here as
    ;; well to support the future possibility of the underlying bitarray
    ;; to be larger (to allow for smooth scrolling)
    (dump-boxer-thing width stream) (dump-boxer-thing height stream)
    ;; first pass through to generate the colormap
    (dotimes& (y height)
      (dotimes& (x width)
        (let* ((pix (%get-8pixel pix-addr x y row-bytes))
                (existing (assoc pix colormap :test #'=)))
          (when (null existing)
            (if (>=& *version-number* 12)
              (push (list pix (8pixel->dump-value pix) remap-idx) colormap)
              (multiple-value-bind (r g b) (8pixel->boxer-rgb-values pix)
                                    (push (list pix (list r g b) remap-idx) colormap)))
            (incf& remap-idx)))))
    ;; now dump out the colormap (we fake a dump-array)
    ;; first flip the colormap so it is ordered in increasing remap-idx's
    (setq colormap (nreverse colormap))
    (enter-table colormap)
    (write-file-word bin-op-initialize-and-return-array stream)
    (let ((lc (length colormap)))
      (dump-array-1 stream lc nil) (dump-boxer-thing lc stream)
      (do* ((j 0 (1+& j))
            (colors colormap (cdr colors))
            (color (cadr (car colors)) (cadr (car colors))))
        ((>=& j lc))
        (dump-boxer-thing color stream)))
    ;; now dump out the pix data as run length encoded words
    (let ((current-byte (%get-8pixel pix-addr 0 0 row-bytes)) (current-count 0))
      (declare (fixnum current-byte current-count))
      (dotimes& (y height)
        (dotimes& (x width)
          (let ((pix (%get-8pixel pix-addr x y row-bytes)))
            (cond ((or (=& current-count 255)
                        (not (=& pix current-byte)))
                    ;; write out a word as high byte = count, low byte = pixel
                    (write-file-word (dpb current-count %%bin-op-top-half
                                          (caddr (assoc current-byte colormap)))
                                    stream)
                    (setq current-byte pix current-count 1))
              ((=& pix current-byte) (incf& current-count))
              (t (error "Bad case in dumping bitmap (byte = ~D, count = ~D"
                        current-byte current-count))))))
      ;; finally write out the last word
      (write-file-word (dpb current-count %%bin-op-top-half
                            (caddr (assoc current-byte colormap)))
                        stream))))

;; sort of flaky, underlying 24 bit pixel format assumed even though
;; it is supposed to handle 16 and 32 bit pixels as well...
(defun dump-true-color-pixmap (pixmap stream)
  (dump-boxer-thing 'true-color-run-length-encoded stream)
  (let ((pixdata (offscreen-bitmap-image pixmap))
        (width (offscreen-bitmap-width pixmap))
        (height (offscreen-bitmap-height pixmap)))
    (declare (fixnum width height))
    ;; dump out width and height.  This can usually be inferred from the
    ;; containing graphics-sheet's draw-wid/hei but we do it here as
    ;; well to support the future possibility of the underlying bitarray
    ;; to be larger (to allow for smooth scrolling)
    (dump-boxer-thing width stream) (dump-boxer-thing height stream)
    ;; now dump out the pix data as
    (let ((current-pixel (pixel-dump-value-internal (image-pixel 0 0 pixdata)))
          (current-count 0))
      (declare (fixnum current-pixel current-count))
      (dotimes& (y height)
        (dotimes& (x width)
          (let ((pix (pixel-dump-value-internal (image-pixel x y pixdata))))
            (cond ((or (=& current-count 255)
                        (not #+opengl (opengl::pixel= pix current-pixel)
                            #-opengl (color= pix current-pixel)
                            ))
                    (write-file-word (dpb& current-count %%bin-op-top-half
                                          (ldb& (byte 8 16) current-pixel))
                                    stream)
                    (write-file-word (ldb& (byte 16 0) current-pixel) stream)
                    ;; update the vars
                    (setq current-pixel pix current-count 1))
              (#+opengl (opengl::pixel= pix current-pixel)
                #-opengl (color= pix current-pixel)
                (incf& current-count))
              (t (error "Bad case in dumping bitmap (pixel = ~X, count = ~D"
                        current-pixel current-count))))))
      ;; finally write out the last word
      (write-file-word (dpb current-count %%bin-op-top-half
                            (ldb& (byte 8 16) current-pixel)) stream)
      (write-file-word (ldb& (byte 16 0) current-pixel) stream))))

;; The data for a boxer bitmap picture is run length encoded
;; rows are padded out to 16-bit words
;; The encoding is high-byte= repeat count, low-byte= data
;; if the high byte = #x80, then the low byte will be a count
;; specifying the next <count> bytes (high AND low) as pure data
;; words
;; This is superficially similiar to the run length encoding found in
;; MacPaint files although byte ordering differences may make the files
;; incompatible
;;

;; get a horizontal byte's worth of B&W pixels
;;
;; The byte ordering of the data is low order bits to the left
;;
(defun get-picture-byte (pic x y &optional size)
  (declare (type (simple-array bit (* *)) pic)
            (fixnum x y))
  (let ((byte 0))
    (dotimes& (i (or size 8))
      (setq byte
            (dpb& (image-pixel pic (+& x i) y) (byte 1 (-& 7 i)) byte)))
    byte))

(defconstant *max-pic-repeat-count* (1- (ash 1 7)))
(defconstant *pic-data-count-prefix* #x80)
(defconstant *max-pic-count* (1- (ash 1 8)))


;; could probably be smarter about when to switch from building a count
;; list to building a repeat (and vice versa)
;;

(defun dump-1-bit-pixmap (pixmap stream)
  (dump-boxer-thing '1-bit-run-length-encoded stream)
  (let ((pixdata (offscreen-bitmap-image pixmap))
        (width (offscreen-bitmap-width pixmap))
        (height (offscreen-bitmap-height pixmap))
        ;; vars
        (current-byte 0)
        (rep-count 0)
        (data-count 0)
        (current-data (make-storage-vector)))
    ;; first dump width and height
    (dump-boxer-thing width stream) (dump-boxer-thing height stream)
    (flet ((write-repeat-word ()
                              (write-file-word (dpb rep-count %%bin-op-top-half current-byte)
                                                stream)
                              (setq current-byte 0 rep-count 0))

            (init-repeat (byte) (setq current-byte byte) (setq rep-count 1))

            (add-count-data (byte)
                            (setq current-byte byte) (sv-append current-data byte)
                            (incf& data-count))

            (write-count-data (data)
                              (write-file-word (dpb *pic-data-count-prefix* %%bin-op-top-half
                                                    data-count)
                                              stream)
                              ;; now write out the accumalated data
                              ;; it is should be properly ordered at this point
                              (do ((i 0 (+& i 2)))
                                ((>=& i (storage-vector-active-length data)))
                                (write-file-word
                                (dpb& (if (>=& (1+& i) (storage-vector-active-length data))
                                        0
                                        (sv-nth (1+& i) data))
                                      %%bin-op-top-half
                                      (sv-nth i data))
                                stream))))
          (flet ((do-leftovers ()
                    (cond ((not (zerop& rep-count)) (write-repeat-word))
                      ((not (zerop& data-count)) (write-count-data
                                                  current-data))))
                  (handle-byte (byte)
                              (cond ((=& byte current-byte)
                                      ;; we have another byte of the same so, if we
                                      ;; are building the count list, then it's time
                                      ;; to send it out or else we incf
                                      ;; the repeat counter unless it is maxed out
                                      (cond ((not (zerop& (storage-vector-active-length
                                                          current-data)))
                                            ;; must be building a count list
                                            (cond ((=& 1 data-count)
                                                    ;; change from building a count list
                                                    ;; to building a repeat
                                                    (clear-storage-vector current-data)
                                                    (setq data-count 0 rep-count 2))
                                              (t
                                                ;; write out what's there (except for
                                                ;; the last one) and start
                                                ;; building a repeat
                                                (decf& data-count)
                                                (sv-delete-at
                                                current-data
                                                (1-& (storage-vector-active-length
                                                      current-data)))
                                                (write-count-data current-data)
                                                (clear-storage-vector current-data)
                                                (setq data-count 0 rep-count  2))))
                                        ;; must be building a repeat
                                        ((=& rep-count *max-pic-repeat-count*)
                                        (write-repeat-word)
                                        (init-repeat byte))
                                        (t (incf& rep-count))))
                                (t
                                  (cond ((not (zerop& rep-count))
                                        ;; must be building a repeat
                                        ;; so send the repeat out and start
                                        ;; building a count list
                                        (write-repeat-word)
                                        (add-count-data byte))
                                    ;; otherwise, we're building a count list
                                    ((=& data-count *max-pic-count*)
                                    (write-count-data current-data)
                                    (init-repeat byte))
                                    (t (add-count-data byte)))))))
                (multiple-value-bind (whole-words-per-row leftover-pixels
                                                          #+X bytes-per-row)
                                      (floor width 16.)
                                      #+X (setq bytes-per-row (+& (*& whole-words-per-row 2)
                                                                  (if (zerop& leftover-pixels)
                                                                    0
                                                                    2)))
                                      (dotimes& (row height)
                                        (dotimes& (rb whole-words-per-row)
                                          (handle-byte (get-picture-byte pixdata
                                                                        (ash& rb 4)	; (* rb 16)
                                                                        row
                                                                        #+X 8.
                                                                        #+X bytes-per-row))
                                          (handle-byte (get-picture-byte pixdata
                                                                        (+& (ash& rb 4) 8.)
                                                                        row
                                                                        #+X 8.
                                                                        #+X bytes-per-row)))
                                        (when (not (zerop& leftover-pixels))
                                          (Handle-byte (get-picture-byte pixdata
                                                                        (ash& whole-words-per-row 4)
                                                                        row
                                                                        (min& 8 leftover-pixels)
                                                                        #+X bytes-per-row))
                                          (handle-byte (if (<& leftover-pixels 8.)
                                                        0
                                                        (get-picture-byte pixdata
                                                                          (+& (ash&
                                                                                whole-words-per-row
                                                                                4)
                                                                              8)
                                                                          row
                                                                          (-& leftover-pixels 8)
                                                                          #+X bytes-per-row)))))
                                      (do-leftovers))))
    (free-storage-vector current-data)))




;;; Hook installation

(eval-when (load)
            ;; file system hooks
            (unless (member 'xref-dump-plist-length *dump-plist-length-hook*)
              (push 'xref-dump-plist-length *dump-plist-length-hook*))
            (unless (member 'xref-dump-plist-internal *dump-plist-internal-hook*)
              (push 'xref-dump-plist-internal *dump-plist-internal-hook*))
            )

;;; Leave this here in case we have to debug loading of bitmaps in
;;; pre file version 11 files

#|
(defun dump-picture (pic width height stream)
  (write-file-word bin-op-picture stream)
  (let ((current-byte 0)
  (rep-count 0)
  (data-count 0)
  (current-data (make-storage-vector)))
    (flet ((write-repeat-word ()
       (write-file-word (dpb rep-count
           %%bin-op-top-half
           current-byte)
            stream)
       (setq current-byte 0
       rep-count 0))

     (init-repeat (byte)
       (setq current-byte byte)
       (setq rep-count 1))

     (add-count-data (byte)
       (setq current-byte byte)
       (sv-append current-data byte)
       (incf& data-count))

     (write-count-data (data)
       (write-file-word (dpb *pic-data-count-prefix*
           %%bin-op-top-half
           data-count)
            stream)
       ;; now write out the accumalated data
       ;; it is should be properly ordered at this point
       (do ((i 0 (+& i 2)))
     ((>=& i (storage-vector-active-length data)))
         (write-file-word (dpb& (if (>=& (1+& i)
                 (storage-vector-active-length
            data))
            0
            (sv-nth (1+& i) data))
              %%bin-op-top-half
              (sv-nth i data))
        stream))))

  (flet ((do-leftovers ()
      (cond ((not (zerop& rep-count))
       (write-repeat-word))
      ((not (zerop& data-count))
       (write-count-data current-data))))
         (handle-byte (byte)
     (cond ((=& byte current-byte)
         ;; we have another byte of the same so, if we
         ;; are building the count list, then it's time
         ;; to send it out or else we incf
         ;; the repeat counter unless it is maxed out
         (cond ((not (zerop& (storage-vector-active-length
            current-data)))
          ;; must be building a count list
          (cond ((=& 1 data-count)
           ;; change from building a count list
           ;; to building a repeat
           (clear-storage-vector current-data)
           (setq data-count 0
           rep-count 2))
          (t
           ;; write out what's there (except for
           ;; the last one) and start
           ;; building a repeat
           (decf& data-count)
           (sv-delete-at
            current-data
            (1-& (storage-vector-active-length
            current-data)))
           (write-count-data current-data)
           (clear-storage-vector current-data)
           (setq data-count 0
           rep-count  2))))
         ;; must be building a repeat
         ((=& rep-count *max-pic-repeat-count*)
          (write-repeat-word)
          (init-repeat byte))
         (t (incf& rep-count))))
        (t
         (cond ((not (zerop& rep-count))
          ;; must be building a repeat
          ;; so send the repeat out and start
          ;; building a count list
          (write-repeat-word)
          (add-count-data byte))
         ;; otherwise, we're building a count list
         ((=& data-count *max-pic-count*)
          (write-count-data current-data)
          (init-repeat byte))
         (t (add-count-data byte)))))))
    (multiple-value-bind (whole-words-per-row leftover-pixels
                #+X bytes-per-row)
        (floor width 16.)
      #+X (setq bytes-per-row (+& (*& whole-words-per-row 2)
          (if (zerop& leftover-pixels)
              0
              2)))
      (dotimes& (row height)
        (dotimes& (rb whole-words-per-row)
    (handle-byte (get-picture-byte pic
                 (ash& rb 4)	; (* rb 16)
                 row
                 #+X 8.
                 #+X bytes-per-row))
    (handle-byte (get-picture-byte pic
                 (+& (ash& rb 4) 8.)
                 row
                 #+X 8.
                 #+X bytes-per-row)))
        (when (not (zerop& leftover-pixels))
    (Handle-byte (get-picture-byte pic
                 (ash& whole-words-per-row 4)
                 row
                 (min& 8 leftover-pixels)
                 #+X bytes-per-row))
    (handle-byte (if (<& leftover-pixels 8.)
         0
         (get-picture-byte pic
               (+& (ash&
              whole-words-per-row
              4)
                   8)
               row
               (-& leftover-pixels 8)
               #+X bytes-per-row)))))
      (do-leftovers))))
    (free-storage-vector current-data)))
|#




;;;; Debugging support

(defun test-dump (filename)
  (flet ((print-instructions ()
                              (format t "~%This will run in a loop Querying for things to~
                  Dump.~%What you type is Evaluated.~%The token ':END ~
                  will terminate the test~%"))
          (get-thing ()
                    (format t "What now ?: ")
                    (eval (read-from-string (read-line)))))
        (print-instructions)
        (writing-bin-file (nil s filename)
                          (loop (let ((thing (get-thing)))
                                  (if (eq thing ':end)
                                    (return)
                                    (dump-boxer-thing thing s)))))))

(defun decode-word (word)
  (let* ((high (ldb %%bin-op-high word))
          (low  (ldb %%bin-op-low  word))
          (imop (decode-bin-op high)))
    (cond ((or (eq imop 'bin-op-command-immediate)
                (eq imop 'bin-op-box-immediate))
            (when (< low 64.)
              (decode-bin-op low)))
      ((null imop) nil)
      (t (values imop low)))))

;;; This is the minimal thing.  Use this to print out the file
;;; doesn't do any reconstruction, just decodes bin ops and
;;; prints them out.  Sometimes prints out data as a bin op
;;; but, oh well....

(defun simple-print-dump-file (filename &optional (stream *standard-output*))
  (with-open-file (s filename :direction :input
                      :element-type '(unsigned-byte 8.))
    (loop (let ((word (read-file-word s nil)))
            (if (null word)
              (return)
              (format stream "~%~5o ~12o ~16o ~A"
                      (ldb %%bin-op-high word)
                      (ldb %%bin-op-low word)
                      word
                      (let ((b (decode-word word)))
                        (if (null b) "Data" b))))))))

(defvar *number-ops*
  `(,bin-op-number-immediate
    ,bin-op-positive-fixnum ,bin-op-negative-fixnum
      ,bin-op-positive-float ,bin-op-negative-float))

(defvar *basic-ops*
  `(,bin-op-number-immediate
    ,bin-op-positive-fixnum ,bin-op-negative-fixnum
      ,bin-op-positive-float ,bin-op-negative-float
      ,bin-op-package-symbol ,bin-op-string ,bin-op-string-immediate
      ,bin-op-cha-immediate ,bin-op-table-fetch-immediate))

(defvar *simple-ops*
  `(,bin-op-number-immediate
    ,bin-op-positive-fixnum ,bin-op-negative-fixnum
      ,bin-op-positive-float ,bin-op-negative-float
      ,bin-op-string ,bin-op-package-symbol ,bin-op-string-immediate
      ,bin-op-list-immediate ,bin-op-list
      ,bin-op-array ,bin-op-initialize-and-return-array
      ,bin-op-initialize-and-return-numeric-array))

(defun minimal-print-dump-file (filename &optional (stream *standard-output*)
                                          (ops-to-process *simple-ops*)
                                          &aux current-result)
  "Will reassemble number, symbols, lists and other SIMPLE
Lisp objects.  It works by calling the corresponding
Load Function on any bin ops that are in the ops-to-process list"
  (let ((*current-file-length* 100) (*file-bin-version* *version-number*))
    (with-open-file (s filename :direction :input
                        :element-type '(unsigned-byte 8.))
      (using-resource (*bin-load-table* bin-load-table)
                      (let ((*bin-load-index* 0)
                            (*bin-next-command-function* 'bin-load-next-command))
                        (loop
                          (let ((word (read-file-word s nil)))
                            (cond ((null word) (return "File has run out"))
                              ((and (or (= bin-op-box-immediate
                                            (ldb %%bin-op-high word))
                                        (= bin-op-command-immediate
                                            (ldb %%bin-op-high word)))
                                    (>= (ldb %%bin-op-low word) 64))
                                (format stream "~%random data: ~O" word))
                              (t
                                (multiple-value-bind (opcode arg)
                                                    (decode-bin-opcode word)
                                                    (let ((op (decode-bin-op opcode)))
                                                      (cond ((eq op 'bin-op-eof) (return))
                                                        ((if (null arg)
                                                            (member word ops-to-process :test #'=)
                                                            (member opcode ops-to-process :test #'=))
                                                          (format stream
                                                                  "~%Processing ~A==> ~S"
                                                                  op
                                                                  (setq current-result
                                                                        (if (null arg)
                                                                          (funcall
                                                                          (bin-op-dispatch
                                                                            *bin-op-load-command-table*
                                                                            opcode)
                                                                          s)
                                                                          (funcall
                                                                          (bin-op-dispatch
                                                                            *bin-op-load-command-table*
                                                                            opcode)
                                                                          s
                                                                          arg)))))
                                                        ((not (null op))
                                                          ;; make an attempt to keep the load-table index
                                                          ;; consistent with the dumped index
                                                          (when (or (get op 'bin-table-loading-command)
                                                                    (eq op 'bin-op-table-store))
                                                            (enter-bin-load-table op))
                                                          (if (null arg)
                                                            (format stream "~%~A: ~o"
                                                                    op word)
                                                            (format stream "~%~A with arg ~o: ~o"
                                                                    op arg word)))
                                                        (t (format stream "~%random data: ~o"
                                                                    word)))))))))))))
  current-result)

