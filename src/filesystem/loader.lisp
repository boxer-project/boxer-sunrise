;;;-*- mode:lisp; syntax:common-lisp; package:boxer -*-

#|


 $Header: loader.lisp,v 1.0 90/01/24 22:14:02 boxer Exp $

 $Log:	loader.lisp,v $
;;;Revision 1.0  90/01/24  22:14:02  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                    +-Data--+
           This file is part of the | BOXER | system
                                    +-------+


      This is a machine independent binary loader for the BOXER system
      It is meant to convert a binary format for storing boxes in files
      (see dumper.lisp for details) back into box structure.


Modification History (most recent at the top)

12/31/11 partial-initialize (box) changed calls to xxx-mac-file-ref-xxx to xxx-xref-xxx
 8/24/11 #+opengl versions for load-true-color-run-length-encoded-pixmap &
         load-8-bit-run-length-encoded-pixmap
         stub for old-style-put-picture-byte to suppress warnings for clean compile
11/02/09 utf-8-string-for-load: fixed bugs
 4/26/09 utf-8-string-for-load done
 4/17/09 UTF-8 utilities
 9/15/08 bug in post-load-process-graphics-list zeroed out converted graphics
 4/22/08 post-load-process-graphics-list
 2/29/08 turtle's private lists need to be post-load-process-graphics-list'd
 2/01/08 added support for private graphics lists in turtle's partial-initialize method
 8/24/07 bin-op-sprite-box should make data-box's in the opengl port
 8/31/05 partial-initialize method for box now handles :associated-file-{dirs,name,type}
 2/16/03 merged current LW and MCL files
 2/11/02 bin-op-graphics-sheet & old-style-load-graphics-sheet mark the graphics
         sheet's dirty? flag
 1/09/02 red and blue byte order is reversed for PC in
         load-true-color-run-length-encoded-pixmap
11/12/01 load-box makes sure file-position returns a number before using in status
 2/15/01 merged current LW and MCL files
         returned fast-mac-load-8-bit-run-length-encoded-pixmap because #_ mac
         traps have been removed to the draw-low-mcl file
 8/22/00 partial-initialize method of graphics-cursor now uses
         reallocate-pixel-color to handle the pen-color value
 5/03/00 code-char-value to hack CR vs LF lossage in converting strings
         in files on PC.  Used in buffered-string-for-load and cons-string-for-load
10/08/99 flags-for-file-init changed to prefer some original flags
 3/22/99 initialize-box-from-box explicitly sets the cached-name of possible
         name-rows. It is usually done via insert-self-action which won't be
         called for boxes already in the hierarchy
 2/15/99 fast-mac-load-8-bit-run-length-encoded-pixmap moved to draw-low-mcl
 6/24/98 (:method partial-initialize graphics-cursor) now uses font loading
         mechanism for type-font.
         Also use font loading in post-load-process-graphics-list
 4/26/98 change font dumper to check dumping font-map
 4/20/98 Changes to handle fonts:
             o Load-row
             o make-bfd-from-file
 4/18/98 Start logging changes: source = boxer version 2.2r4

|#

(in-package :boxer)



(define-load-command bin-op-number-immediate (ignore value)
  (declare (ignore ignore))
  (flet ((sign-extend-immediate-operand (number)
            (if (ldb-test %%bin-im-arg-msb number)
                (- number %%bin-op-im-arg-size)
                number)))
    (sign-extend-immediate-operand value)))


(define-load-command-for-effect bin-op-format-version (stream)
  (let ((version (bin-next-value stream)))
    (unless (null *boxer-system-hacker*)
      (format t "~%Loading in Boxer File with version ~D..." version))
    (cond ((= version *version-number*)
           (setq *file-bin-version* version))
          ((member version *supported-obsolete-versions*)
           (setq *file-bin-version* version))
          (t
           (if (find-package 'boxer-compatibility-file-system)
               (error "File Format version is ~D, you MAY be able read~%~
                       this file in with the COMPATIBILITY-READ Command"
                      version)
               (error "File Format version is ~D, which is not supported"
                      version))))))

(define-load-command-for-effect bin-op-file-property-list (stream)
  (let* ((*package* (find-package 'boxer))
         (plist (bin-next-value stream)))
    ;; first deal with the package
    (setq *load-package* (getf plist :package))
    ;; now handle the plist in a general way...
    (handle-file-property-list plist)))

(define-load-command-for-effect bin-op-box-server-info-start (stream)
  (boxnet::skip-box-server-info stream))

(define-load-command-for-effect bin-op-eof (ignore)
  (declare (ignore ignore))
  (throw 'bin-load-done t))

(define-load-command bin-op-table-store (stream)
  (enter-bin-load-table (bin-next-value stream)))

#|
;;; this is used for debugging
;;; when we are only executing SOME of the load commands, then the table will
;;; get out of sync from the dumped indexes because some table-stores are
;;; being ignored, this lets us hand patch it to a certain extent
(define-load-command bin-op-table-fetch-immediate (ignore idx)
  (declare (ignore ignore))
  (format t "~%Table Fetching: ~o (~S)" idx (aref *bin-load-table* idx))
  (format t "~%Value OK ? (y, n, or show) ")
  (let ((answer (read-line)))
    (cond ((string-equal "y" answer)
           (aref *bin-load-table* idx))
          ((string-equal "show" answer)
           (let ((offset (if (< idx 5) 0 (- idx 5))))
             (dotimes (i 10)
               (format t "~%~5o: ~S"
                       (+ offset i)
                       (aref *bin-load-table* (+ offset i)))))
           (format t "~%Value to Return: ")
           (eval (read-from-string (read-line))))
          (t
           (format t "~%Value to Return: ")
           (eval (read-from-string (read-line)))))))
|#

(define-load-command bin-op-table-fetch-immediate (ignore index)
  (declare (ignore ignore))
  (aref *bin-load-table* index))

(define-load-command bin-op-table-fetch (stream)
  (aref *bin-load-table* (bin-next-value stream)))

(define-load-command-for-value bin-op-symbol (stream)
  (with-buffered-string-loading (intern (bin-next-value stream))))


;; some implementations call the LISP package "COMMON-LISP"
;; other implementations will fail on (find-package "COMMON-LISP")
;; this has been fixed in dumper.lisp to always print out the name
;; of the LISP package as "LISP"
;; we have this hack installed for files created (usually on the mac)
;; before the dumper fix was installed
(defvar *hack-symbol-package-names* t)

(define-load-command-for-value bin-op-package-symbol (stream)
  (let* ((package-string (let ((name (bin-next-value stream)))
                           (if (and *hack-symbol-package-names*
                  (or (string= name "LUCID-COMMON-LISP")
                      (string= name "COMMON-LISP")))
                               "LISP"
                               name)))
         (package (find-package package-string))
         (pname (bin-next-value stream)))
    (intern pname package)))

(define-load-command-for-value bin-op-string-immediate (stream length)
  (load-string stream length))

(define-load-command-for-value bin-op-string (stream)
  (load-string stream))

(define-load-command-for-value bin-op-list-immediate (stream length)
  (load-list stream length))

(define-load-command-for-value bin-op-list (stream)
  (load-list stream))

(define-load-command bin-op-positive-fixnum (stream)
  (load-fixnum stream))

(define-load-command bin-op-negative-fixnum (stream)
  (- (load-fixnum stream)))

(define-load-command bin-op-positive-float (stream)
  (load-float stream))

(define-load-command bin-op-negative-float (stream)
  (- (load-float stream )))

(define-load-command bin-op-rational (stream)
  (/ (bin-next-value stream) (bin-next-value stream)))

(define-load-command bin-op-complex (stream)
  (complex (bin-next-value stream) (bin-next-value stream)))

(define-load-command-for-value bin-op-array (stream length)
  (load-array stream length))

(define-load-command bin-op-initialize-and-return-array (stream)
  (initialize-array stream))

(define-load-command bin-op-initialize-and-return-numeric-array (stream)
  (initialize-numeric-array stream))



;;;The actual LOAD functions

(defun load-list (stream &optional (length (bin-next-value stream)))
  (let* ((dotted (bin-next-value stream))
         (itl (if dotted (1- length) length))
         (list (make-list itl))
         (wl list))
    (dotimes (i itl)
      (rplaca wl (bin-next-value stream))
      (setq wl (cdr wl)))
    ;; bleechhh...
    (when dotted
      (rplacd (nthcdr (1- itl) list) (bin-next-value stream)))
    list))


;(> *file-bin-version* 8)
#|
(defun load-string (stream &optional (length (bin-next-value stream)))
  (cond ((null *use-load-string-buffer*) (cons-string-for-load stream length))
        (t (buffered-string-for-load stream length))))
|#

(defun load-string (stream &optional (length (bin-next-value stream)))
  (cond ((null *use-load-string-buffer*) (utf-8-string-for-load stream length))
        (t (buffered-string-for-load stream length))))

#-lispworks
(defun code-char-value (value) (code-char value))

;; indirection for converting chars
;; for now, the main culprit is #\NewLine #\Return lossage
;; strings in mac files have #\Return, but we need #\Newline for correct display in
;; graphics boxes
#+lispworks
(defun code-char-value (value)
  (cond ((= value #.(char-code #\Return)) #\NewLine)
        (t (code-char value))))

;;; UTF-8 decoding...
;; byte testing
(defun utf-8-1-byte-char? (byte)
  (=& %utf-8-1byte-id-value (ldb& %utf-8-1byte-id-bytespec byte)))

(defun utf-8-2-byte-char? (byte)
  (=& %utf-8-2byte-id-value (ldb& %utf-8-2byte-id-bytespec byte)))

(defun utf-8-3-byte-char? (byte)
  (=& %utf-8-3byte-id-value (ldb& %utf-8-3byte-id-bytespec byte)))

(defun utf-8-4-byte-char? (byte)
  (=& %utf-8-4byte-id-value (ldb& %utf-8-4byte-id-bytespec byte)))

(defun utf-8-more-byte-char? (byte)
  (=& %utf-8-more-byte-id-value (ldb& %utf-8-more-byte-id-bytespec byte)))

;; Note: length will be # of encoded bytes, not # of eventual chars
;; no matter what, we have to be sure to pull out <length> bytes of data
;; so as not to get out of synch with data further down the filestream
;; this MAY involve handling illegal UTF-8 content since it may have been
;; dumped there by older non UTF-8 string functions
;; In multi byte encodings, if we get illegal bytes, we'll write out any
;; accumalated bytes as separate chars using the theory that the original
;; bytestream probably wasn't UTF-8 encoded (like old boxer files)
(defun utf-8-string-for-load (stream length)
  (let ((return-string (make-array length ;length will always be <= encoded bytes
                                   :initial-element #\o
                                   :element-type 'character
                                   :fill-pointer 0))
        (word-count 0)
        (word-length (ceiling length 2))
        (byte1 nil) (byte2 nil)) ; nil means "empty"
    (flet ((next-word ()
             (cond ((>= word-count word-length) 'End-of-string)
                   (t (let ((word (bin-next-byte stream)))
                        (setq byte1 (ldb& %%bin-op-low-half word)
                              byte2 (ldb& %%bin-op-top-half word))
                        (incf word-count)))))
           (shift-input-bytes ()
             (cond ((null byte2) (setq byte1 nil))
                   (t (setq byte1 byte2 byte2 nil)))))
      (loop
       (let (ubyte1 ubyte2 ubyte3) ; state variables
         ;; leave for now, will need for full 4 byte implementation
         (declare (ignore ubyte3))
         (when (null byte1) (next-word))
         (cond ((null byte1)
                (return return-string))
               ((utf-8-1-byte-char? byte1)
                (vector-push (code-char byte1) return-string)
                (shift-input-bytes))
               ((utf-8-2-byte-char? byte1)
                ;; 2 byte char
                (setq ubyte1 byte1) ; keep track of byte1 in case we need to overwrite (if null byte2)
                (cond ((null byte2)
                       ;; need more data so get it...
                       (when (eq (next-word) 'end-of-string) (return return-string))
                       (cond ((null byte1)
                              ;; this can happen if we are out of synch
                              ;; make char out of what we have
                              (vector-push (code-char ubyte1) return-string))
                             ((utf-8-more-byte-char? byte1)
                              ;; what SHOULD happen
                              (vector-push (code-char
                                            (dpb& (ldb& %utf-8-2byte-1stbyte-dst-bytespec ubyte1)
                                                  %utf-8-2byte-1stbyte-src-bytespec
                                                  (ldb& %utf-8-last-byte-bytespec
                                                        byte1)))
                                           return-string)
                              (shift-input-bytes))
                             (t ; must be illegal input in byte1
                              ;; write out what we have, then loop again
                              (vector-push (code-char ubyte1) return-string))))
                      (t ; there is data there...
                       (cond ((utf-8-more-byte-char? byte2)
                              (vector-push (code-char
                                            (dpb& (ldb& %utf-8-2byte-1stbyte-dst-bytespec ubyte1)
                                                  %utf-8-2byte-1stbyte-src-bytespec
                                                  (ldb& %utf-8-last-byte-bytespec
                                                        byte2)))
                                           return-string)
                              (setq byte1 nil byte2 nil))
                             (t ; must be illegal input in byte1
                              ;; write out what we have, then loop again
                              (vector-push (code-char ubyte1) return-string)
                              (shift-input-bytes))))))
               ((utf-8-3-byte-char? byte1)
                ;; 3 byte char
                (setq ubyte1 byte1)
                (cond ((null byte2)
                       (when (eq (next-word) 'end-of-string) (return return-string))
                       (cond ((null byte1)
                              ;; this can happen if we are out of synch
                              ;; make char out of what we have
                              (vector-push (code-char ubyte1) return-string))
                             ((and (utf-8-more-byte-char? byte1)
                                   (utf-8-more-byte-char? byte2))
                              ;;what SHOULD happen
                              (vector-push
                               (code-char (dpb& (ldb& %utf-8-3byte-1stbyte-dst-bytespec ubyte1)
                                                %utf-8-3byte-1stbyte-src-bytespec
                                                (dpb&
                                                 (ldb& %utf-8-last-byte-bytespec
                                                       byte1)
                                                 %utf-8-3byte-2ndbyte-src-bytespec
                                                 (ldb& %utf-8-last-byte-bytespec
                                                       byte2))))
                               return-string)
                              (setq byte1 nil byte2 nil))
                             (t
                              ;; illegal content,
                              (vector-push (code-char ubyte1) return-string))))
                      ((utf-8-more-byte-char? byte2)
                       ;; save what we have so far..
                       (setq ubyte2 (ldb& %utf-8-3byte-2ndbyte-dst-bytespec byte2))
                       ;; get more data, refill byte1 & byte2
                       (when (eq (next-word) 'end-of-string) (return return-string))
                       (cond ((null byte1)
                              (vector-push (code-char ubyte1) return-string)
                              (vector-push (code-char ubyte2) return-string))
                             ((utf-8-more-byte-char? byte1)
                              ;; what SHOULD happen
                              (vector-push
                               (code-char (dpb& (ldb& %utf-8-3byte-1stbyte-dst-bytespec ubyte1)
                                                %utf-8-3byte-1stbyte-src-bytespec
                                                (dpb&
                                                 (ldb& %utf-8-last-byte-bytespec
                                                       ubyte2)
                                                 %utf-8-3byte-2ndbyte-src-bytespec
                                                 (ldb& %utf-8-last-byte-bytespec
                                                       byte1))))
                               return-string)
                              (shift-input-bytes))
                             (t ; illegal content
                              (vector-push (code-char ubyte1) return-string)
                              (vector-push (code-char ubyte2) return-string))))
                      (t ; illegal content
                       (vector-push (code-char ubyte1) return-string)
                       (shift-input-bytes))))
               ((utf-8-4-byte-char? byte1)
                ;; 4 byte char
                ;; just make a char for now, converting old non utf-8 data
                (vector-push (code-char byte1) return-string)
                (shift-input-bytes)
                )
               (t
                ;; unexpected input, just make a char out of it
                (vector-push (code-char byte1) return-string)
                (shift-input-bytes))))))))

(defun cons-string-for-load (stream length)
  (let* ((string (make-string length))
         (fits? (evenp length))
         (dolength (if fits? length (1-& length))))
    (do ((i 0 (+& i 2)))
        ((=& i dolength))
      (let ((word (bin-next-byte stream)))
        (setf (char string i)
              (code-char-value (ldb& %%bin-op-low-half word)))
        (setf (char string (1+& i))
              (code-char-value (ldb& %%bin-op-top-half word)))))
    (when (null fits?)
      (setf (char string dolength)
            (code-char-value (bin-next-byte stream))))
    string))

(defun buffered-string-for-load (stream length)
  (setf (fill-pointer *load-string-buffer*) 0)
  (let* ((fits? (evenp length))
         (dolength (if fits? length (1-& length))))
    (do ((i 0 (+& i 2)))
        ((=& i dolength))
      (let ((word (bin-next-byte stream)))
        (vector-push-extend (code-char-value (ldb& %%bin-op-low-half word))
                            *load-string-buffer*)
        (vector-push-extend (code-char-value (ldb& %%bin-op-top-half word))
                            *load-string-buffer*)))
    (when (null fits?)
      (vector-push-extend (code-char-value (bin-next-byte stream))
                          *load-string-buffer*))
    *load-string-buffer*))


;; despite the name, fixnums are not lisp:fixnum, but lisp:integer.
(defun load-fixnum (stream &optional (length (bin-next-value stream)))
  (cond ((= length 1) (bin-next-byte stream))
        (t (let ((number 0))
             (dotimes (i length)
               ;; this should not use DPB& because we might load a
               ;; bignum, even thoush this function is called
               ;; load-fixnum.  Or am i wrong?
               (setq number (dpb (bin-next-byte stream)
                                 (byte 16. (* i 16.))
                                 number)))
             number))))

(defun load-float (stream)
  (let ((mantissa (bin-next-value stream))
        (exponent (bin-next-value stream)))
    (scale-float (float mantissa) exponent)))

;;(DEFUN TRANSPOSE-BIT-ARRAY (ARRAY)
;;  "Returns a new array with width = heigth of arg and height - width of arg"
;;  (MULTIPLE-VALUE-BIND (DIMS OPTS)
;;      (DECODE-ARRAY ARRAY)
;;    (LET ((RETURN-ARRAY (LEXPR-FUNCALL #'MAKE-ARRAY (REVERSE DIMS) OPTS)))
;;      (COPY-ARRAY-CONTENTS ARRAY RETURN-ARRAY)
;;      RETURN-ARRAY)))

(defun load-array (stream opt-length)
  (let ((dimensions (bin-next-value stream))
        (options (make-list (* opt-length 2))))
    (let ((l options))
      (dotimes (i opt-length)
        (setf (car l)  (bin-next-value stream))	; keyword
        (setf (cadr l) (bin-next-value stream))	; value
        (setq l (cddr l))))
    (apply #'make-array dimensions options)))

;;; We might want to use some array resources here to keep the consing down
;;; The Zippy Lisp implementation was able to call RETURN-ARRAY after it
;;; was done.  But no such luck in Common Lisp so at the moment, we are just
;;; CONSing them and then throwing them away

(defun initialize-array (stream)
  (let* ((array (bin-next-value stream))
         (length (bin-next-value stream))
         (q-array (if (=& (array-rank array) 1) array
                      (make-array length ':displaced-to array))))
    (dotimes (i length)
      (setf (aref q-array i) (bin-next-value stream)))
    array))

(defun initialize-numeric-array (stream)
  (let* ((array (bin-next-value stream))
         (length (bin-next-value stream))
         (flat-array (if (=& (array-rank array) 1) array
                         (make-array length :displaced-to array)))
         (flat-length (length flat-array))
         (n-bits (array-bits-per-element array)))
    (flet ((unpack-word (word start-index)
             (dotimes (i (floor (/ 16. n-bits)))
               (let ((idx (+ start-index i)))
                 (unless (>= idx flat-length)
                   (setf (aref flat-array idx)
                         (ldb (byte n-bits (* i n-bits)) word)))))))
      (dotimes (i length)
        (unpack-word (bin-next-byte stream) (* i (floor 16 n-bits)))))
    array))



;;;; Loading Boxer Objects

;;; Characters and Rows

(define-load-command bin-op-cha-immediate (stream cha)
  (declare (ignore stream))
  (make-char (code-char (ldb& %%bin-op-char-code cha))
             (ldb& (byte #o2 #o10) cha)))
;; should be %%bin-op-im-cha-bits but use (byte #o2 #o10) so that old
;; files with fonts in the chas have the font part stripped out on loading

;;; we've flushed fonts...
;(define-load-command bin-op-cha-immediate (stream cha)
;  (declare (ignore stream))
;  (make-char (code-char (ldb %%bin-op-char-code cha))
;	     (ldb %%bin-op-im-cha-bits cha)
;	     (map-index-to-style (ldb %%bin-op-im-cha-style cha))))

(define-load-command bin-op-row-immediate (stream length)
  (load-row stream length))

(define-load-command bin-op-row (stream)
  (load-row stream))

(define-load-command bin-op-name-row-immediate (stream length)
  (load-name-row stream length))

(define-load-command bin-op-name-row (stream)
  (load-name-row stream))

#|
(defun load-row (stream &optional (length (bin-next-value stream)))
  (let ((new-row (make-instance 'row)))
    ;; this is an abstraction violation but it's fast for now.
    (chas-array-assure-room (chas-array new-row) length)
    (dotimes& (i length)
      ;; have to use append-cha to set superior, etc.
      (append-cha new-row (bin-next-value stream)))
    new-row))
|#

(defmethod file-initialize ((row row) chas-array)
  (shared-initialize row t)
  (setf (slot-value row 'tick) -1
        (slot-value row 'chas-array) chas-array))


;; check for version, version 12 uses new row format with font info expected
(defun load-row (stream &optional (length (bin-next-value stream)))
  (let ((chas-array  (make-chas-array length))
        (new-row (allocate-instance (find-class 'row))))
    (file-initialize new-row chas-array)
    ;; this is an abstraction violation but it's fast
    (with-fast-chas-array-manipulation (chas-array chas-array-chas)
      (cond ((< *file-bin-version* 12)
             ;; old style
             (dotimes& (i length)
               (let ((obj (bin-next-value stream)))
                 (fast-chas-array-set-cha chas-array-chas i obj)
                 ;; we don't do INSERT-SELF-ACTION because that needs
                 ;; to be done when the row is put into the box, not
                 ;; when the cha is put into the row.
                 (cond ((box? obj) (set-superior-row obj new-row))
                       ((cha? obj))
                       (t (warn "~A was not a character or a box" obj))))))
             (t ; new style, packed chars and look for fonts
              (let ((idx 0))
                (unless (zerop length)
                  (loop (let ((obj (bin-next-value stream)))
                          ;; obj can be either a box or a string
                          (cond ((box? obj)
                                 (fast-chas-array-set-cha chas-array-chas idx obj)
                                 (set-superior-row obj new-row)
                                 (incf& idx))
                                ((stringp obj)
                                 (dotimes (i (length obj))
                                   (fast-chas-array-set-cha chas-array-chas
                                                            idx (char obj i))
                                   (incf& idx))))
                          (when (>=& idx length) (return nil))))))
              ;; get font info
                (let ((fds (bin-next-value stream)))
                  (dolist (file-fd fds)
                    (insert-bfd new-row (make-bfd-from-file file-fd)))
                ))))
    (setf (chas-array-active-length chas-array) length)
    new-row))

;; a file-fd is a list
(defun make-bfd-from-file (file-fd)
  (make-cfd (car file-fd) (make-boxer-font (cadr file-fd))
            (reallocate-pixel-color (caddr file-fd))))

#|
(defun load-name-row (stream &optional (length (bin-next-value stream)))
  (let* ((chas-array (make-chas-array length))
         (new-row (make-instance 'name-row
                                 :chas-array chas-array
                                 :cached-name (bin-next-value stream))))
    (dotimes& (i length)
      (append-cha new-row (bin-next-value stream)))
    new-row))
|#

(defun load-name-row (stream &optional (length (bin-next-value stream)))
  (log:debug "load-name-row length: ~A" length)
  (let* ((chas-array (make-chas-array length))
         (new-row (make-instance 'name-row
                                 :chas-array chas-array
                                 :cached-name (bin-next-value stream)))
         )
    (cond
      ((>= *file-bin-version* 13.)
        (let ((new-row-name (bin-next-value stream)))
          (with-fast-chas-array-manipulation (chas-array chas-array-chas)
            (dotimes& (i length)
              (fast-chas-array-set-cha chas-array-chas i (elt new-row-name i))))
        )
      )
      (t (with-fast-chas-array-manipulation (chas-array chas-array-chas)
        (dotimes& (i length)
          (let ((next-value (bin-next-value stream)))
            (log:debug "  Name row i: ~A cha: ~A" i next-value)
            (fast-chas-array-set-cha chas-array-chas i next-value))))))
    (setf (chas-array-active-length chas-array) length)
    new-row))



;;; Box loading commands

(define-load-command-for-effect bin-op-end-of-box (stream)
  (declare (ignore stream))
  (throw 'done-with-box ':yay))

;;; put the new variable here
(defvar *top-level-box-to-load-into* nil
  "Usually NIL, this variable can be set to point to a box to load into.
DOIT and DATA box loaders should check this variable via the appropriate macro and
use its result regardless of the type of the object.  SPRITE and PORT box loaders
should ignore it.")

(defun top-level-box-to-load-into ()
  (and *top-level-box-to-load-into*
       (prog1 *top-level-box-to-load-into*
         (setq *top-level-box-to-load-into* nil))))

(define-load-command bin-op-doit-box (stream)
  (load-box (or (top-level-box-to-load-into)
                (make-uninitialized-box 'doit-box))
            stream))

(define-load-command bin-op-data-box (stream)
  (load-box (or (top-level-box-to-load-into)
                (make-uninitialized-box 'data-box))
            stream))

(define-load-command bin-op-port-box (stream)
  ;; ports can't be read into at top level.
  (load-box (make-uninitialized-box 'port-box) stream))

;; just make a regular box...
(define-load-command bin-op-sprite-box (stream)
  ;; sprite boxes can't be read into at top level.
  (load-box (make-uninitialized-box 'data-box) stream))


(defun load-box (box stream)
  (when *file-system-verbosity*
    (cond ((not (null *file-status-line-update-function*))
           (funcall *file-status-line-update-function* stream))
          ((not (null *current-file-length*))
           ;; print out status reports to keep everybody happy....
           (let ((fp (file-position stream)))
             (when (numberp fp)
               (status-line-display 'loading-box
                                    (format nil *status-line-loading-format-string*
                                            fp (floor (* 100 fp)
                                                      *current-file-length*))))))))
  #+mcl (ccl::update-cursor) ; hack to get moving cursor
  (boxer-eval::check-for-interrupt :interrupt "Stopped by User !")
  ;; now do the real work...
  (catch 'done-with-box
    ;; if debugging, this might be useful intead (lets us look at the plist)
    ;; Still need to do this for old files because of ordering
    ;; problems.  Specifically, for sprite-boxes, the turtle needs to
    ;; already be installed before any rows are appended
    (if (< *file-bin-version* 8)
        (let ((plist (bin-next-value stream)))
          (init-box-from-file box plist))
        ;; Non Consing version of initializing from the PLIST in a file
        (let ((current-keyword nil))
          (with-loading-list (stream item-no)
            (cond ((evenp item-no)
                   ;; should be a keyword...
                   (let ((next-keyword (bin-next-value stream)))
                     (unless (symbolp next-keyword)
                       (error "Expected a Keyword instead of ~S" next-keyword))
                     (setq current-keyword next-keyword)))
                  (t
                   (partial-initialize box
                                       current-keyword
                                       (bin-next-value stream)))))))
    ;; this should THROW...
    (bin-next-command stream)
    ;; or else something is wrong
    (cerror "Continue trying to load"
            "Looks like there's some crud in the file after ~A" box))
  ;; compatibility crocks...
  (when (and (= *file-bin-version* 5)
             *check-for-and-convert-old-vlist-style*
             (not (null (slot-value box 'closets))))
    (crock-fix-sprite-closet-for-changed-slot-names (slot-value box 'closets)
                                                    box))
;    ;; add an empty row to keep things happy
;    ;; since ports get dumped with a NIL first-inferior-row
;    (when (port-box? box)
;      (let ((fir (make-row '())))
;	(setf (slot-value box 'first-inferior-row) fir)
;	(setf (superior-box fir) box)))
  box)

;; some flags should be inherited by from the framing context
;; and not overridden by freshly filled values
;; Note, could be faster if we directly slam the bits
;; but since this isn't called often, better to preserve abstraction
(defmethod flags-for-file-init ((box box) (from-box box))
  (let ((raw-flags (slot-value box 'flags))
        (new-flags (slot-value from-box 'flags)))
    (setf (slot-value box 'flags)
          (set-box-flag-read-only-box?
           (set-box-flag-autoload-file?
            (set-box-flag-relative-filename?
             new-flags
             (box-flag-relative-filename? raw-flags))
            (box-flag-autoload-file? raw-flags))
           (box-flag-read-only-box? raw-flags)))))

(defmethod initialize-box-from-box ((box box) from-box)
  (set-class box (class-of from-box))
  ;;  (copy-special-box-properties from-box box T)
  ;; display style...
  (set-display-style-list box
                          (copy-display-style
                           (display-style-list from-box)))
  ;; there should be a better way to do this, like maybe
  ;; moving the dashed border attribute into a flag ?
  (setf (display-style-border-style (display-style-list box)) nil)
  (setf (slot-value box 'flags) (flags-for-file-init box from-box))
  ;; graphics...
  (when (not (null (graphics-info from-box)))
    (setf (graphics-info box)
          (copy-graphics-sheet (graphics-info from-box) box)))
  ;; copy any export info
  (unless (null (exports from-box))
    (boxer-eval::set-exports box (exports from-box))
    ;; make sure the box looks right
    (setf (display-style-border-style (display-style-list box))
          ':dashed))
  ;; and the name
  (let ((name (slot-value from-box 'name)))
    (unless (null name)
      (let ((new-name-row (make-name-row `(,(if (stringp name) name
                                                (text-string name))))))
        ;; normally, this would be handled via update-bindings during an
        ;; insert-self-action, however, since we are modifying a box which
        ;; is already in the editor, we do it by hand
        (setf (cached-name new-name-row) (get-box-name new-name-row))
        (set-name box new-name-row))))
  (initialize-box-plist-from-box box from-box)
  (move-box-internals from-box box (no-cross-file-links? box))
  box)

(defmethod initialize-box-plist-from-box ((box box) from-box)
  (let ((from-plist (slot-value from-box 'plist)))
    ;; only worth copying for now...
    (let ((as (getf from-plist 'active-sprite)))
      (unless (null as) (putprop box as 'active-sprite)))))


(defvar *subclass-file-init-keywords*
  '(:port-target :circular-port-reference :cracked-port :cross-file-link-id
    :associated-turtle))

;; should return T if the keyword is handled
(defmethod partial-initialize ((self box) keyword value)
  (case keyword
    (:name (setf (slot-value self 'name) value)
           (when (row? value)
             (setf (superior-box value) self))
           t)
    (:display-style-list (setf (slot-value self 'display-style-list)
                               (restore-display-style value)))
    (:exports (unless (null value)
                (boxer-eval::set-box-transparency self t)
                (unless (eq value boxer-eval::*exporting-all-variables-marker*)
                  (setf (slot-value self 'exports) value)))
              t)
    (:closets (setf (slot-value self 'closets) value)
              (when (row? value)
                (setf (superior-box value) self)
                (do-row-chas ((obj value))
                  (unless (cha? obj)
                    (set-superior-row obj value)
                    (insert-self-action obj))))
              t)
    (:flags (setf (slot-value self 'flags) value)
            (when (and boxnet::*during-login-action?*
                       (box-flag-load-box-on-login? value)
                       (not (fast-memq self boxnet::*login-postpass-list*)))
              (push self boxnet::*login-postpass-list*))
            (when (and *in-autoload-environment*
                       (box-flag-autoload-file? value)
                       (not (fast-memq self *autoload-list*)))
              (push self *autoload-list*))
            t)
    ;; these next 2 are for old files
    (:graphics-sheet (setf (slot-value self 'graphics-info) value)
                     ;; restore the backpointer from the graphics sheet
                     (if (null value)
                         t
                         (setf (graphics-sheet-superior-box value) self)))
    (:associated-turtle
     (cond ((graphics-object? value)
            (setf (slot-value self 'graphics-info) value)
            (set-sprite-box value self))
           (t
            (warn "The sprite box, ~A, doesn't have a graphics object"
                  self))))
    ;; the new paradigm...
    (:graphics-info
     (cond ((graphics-object? value)
            (setf (slot-value self 'graphics-info) value)
            (set-sprite-box value self))
           ((graphics-sheet? value)
            (setf (slot-value self 'graphics-info) value)
            ;; restore the backpointer from the graphics sheet
            (setf (graphics-sheet-superior-box value) self))))
    (:rows (let ((prev-row (car value)))
             (unless (null value)
               (insert-row-at-row-no self (car value) 0)
               (dolist (row (cdr value))
                 (insert-row-after-row self row prev-row)
                 (setq prev-row row)))
             t))
    ;; box server specific keys...
    (:server-box-id (load-server-box-id self value) t)
    (:cross-file-contained-links
     (load-cross-file-contained-links self value) t)
    (:cross-file-port-branch-links
     (load-cross-file-port-branch-links self value) t)
    (:cross-file-target-branch-links
     (load-cross-file-target-branch-links self value) t)
    (:cross-file-target-ends (load-cross-file-target-ends self value) t)
    ;; obsolete, should warn...
    (:port-target-translation-table
     (warn "Old style cross file port target reference")
     (putprop self value :port-target-translation-table) t)
    ;; url key
    (:url (setf (getf (slot-value self 'plist) :url) (load-url value)))
    ;; filename stuff
    (:associated-file (setf (getf (slot-value self 'plist) :associated-file)
                            (pathname value)))
    (:associated-file-dirs (setf (getf (slot-value self 'plist) :associated-file-dirs)
                                 value) t)
    (:associated-file-name (setf (getf (slot-value self 'plist) :associated-file-name)
                                 value) t)
    (:associated-file-type
     ;; at this point all the pieces should be loaded so we should try and reassemble
     ;; the full filename from the components
     (let ((dirs (getf (slot-value self 'plist) :associated-file-dirs :novalue))
           (name (getf (slot-value self 'plist) :associated-file-name :novalue)))
       (cond ((or (eq dirs :novalue) (eq name :novalue))
              ;; guess we don't have all the values...
              ;; just save the info
              (setf (getf (slot-value self 'plist) :associated-file-type)
                    value) t)
             (t
;              (format t "Converting filename ~A from ~A, ~A, ~A"
;                      (getf (slot-value self 'plist) :associated-file)
;                      dirs name value)
              (setf (getf (slot-value self 'plist) :associated-file)
                    (make-pathname :directory dirs :name name :type value))
              (remf (slot-value self 'plist) :associated-file-dirs)
              (remf (slot-value self 'plist) :associated-file-name)
              (remf (slot-value self 'plist) :associated-file-type)
              t))))
    (:boxtop (setf (getf (slot-value self 'plist) :boxtop) value))
    (:cached-boxtop (setf (getf (slot-value self 'plist) :cached-boxtop) value))
    (:xref (cond ((listp value) ;; means there are mime values
                  (putprop self (make-xref :pathname (pathname (car value))
                                           :mime-type (cadr value))
                           :xref)
                  (multiple-value-bind (creatrix ftype)
                      (boxnet::mime-values->mac-file-values (cadr value))
                    (set-xref-boxtop-info self creatrix ftype)))
                 (t
                  (putprop self (make-xref :pathname (pathname value))
                           :Xref)
                  (set-xref-boxtop-info self))))
    (t (cond ((fast-memq keyword *load-module-init-keywords*)
              (funcall (get keyword 'load-module-function) self value))
             ((fast-memq keyword *subclass-file-init-keywords*) ; yuck
              ;; don't warn for subclass keywords
              ;; bad modularity but we're sort of prisoners of the module scheme
              nil)
             (t
              ;; should warn about unhandled keyword here, most likely the result
              ;; of an unloaded extension
              (boxer-editor-warning "Warning: Unhandled keyword, ~S, during open"
                                    keyword))))))

(defmethod partial-initialize ((self port-box) keyword value)
  (or (call-next-method)		; returns non NIL if already handled
      ;; try and handle it here...
      (case keyword
        (:port-target
         (if (box? value)
             (set-port-to-box self value)
             (cerror "Try and continue to Load the File"
                     "~A is not a valid target for a port"
                     value)))
        (:circular-port-ref
         ;; old style reference, flush soon.  Probably only affects ML
         (push (cons self value) *circular-ports*))
        (:circular-port-reference
         (push (cons self value) *post-load-relink-ports*))
        (:cracked-port
         (putprop self t :cracked-port))
        ;; Cross file port keys...
        (:cross-file-link-id (load-cross-file-link-id self value) t)
        ;; obsolete, should probably warn
        (:cross-file-port-target
         (warn "Old style cross file port reference")
         (putprop self (boxnet::translate-port-target-path value)
                  :cross-file-port-target))
        (t nil))))

;; functionality copied to regular box method to support old files
#|
(defmethod partial-initialize ((self sprite-box) keyword value)
  (or (call-next-method)		; returns non NIL if already handled
      ;; try and handle it here...
      (when (eq keyword ':associated-turtle)
        (cond ((graphics-object? value)
               (setf (slot-value self 'associated-turtle) value)
               (set-sprite-box value self))
              (t
               (warn "The sprite box, ~A, doesn't have a graphics object"
                     self))))))
|#

;; in the future, we may want to dump out graphics boxes with just the
;; graphics objects and NOT the bit array.  In those cases, we will not
;; need to redraw the turtle an extra time

(defun graphics-sheet-image-restored-from-file? () t)

(defun restore-display-style (dds)
  (unless (null dds)
    (let ((ds (make-display-style :style     (first  dds)
                                  :fixed-wid (second dds)
                                  :fixed-hei (third  dds))))
      (when (> (length dds) 3)
        (setf (display-style-graphics-mode? ds) (fourth dds))
        (setf (display-style-border-style   ds) (fifth  dds)))
      ds)))

;;; Init-Box-From-File is ONLY used by the debugging version
;;; of Load-Box which CONSes a Plist.  The default version does NOT
;;; CONS Plists and uses Partial-Initialize instead.

;; still need to move sprite-box method functionality here....

(defmethod init-box-from-file ((box box) plist)
  ;; first fix up some slots which make-instance would have handled
  (setf (slot-value box 'tick) 0)
  ;; now look for things in the plist
  (setf (slot-value box 'name) (getf plist :name))
;;; Hey! we shouldn't be saving the static-variables list out like this.
;;; it's got time-stamps in it that might conflict with our own time-stamps.
;;; until now it's been working fine becuase the box was "modified" on loading,
;;; so this variable was flushed.
;;;  (setf (slot-value box 'static-variables-alist)
;;;	(getf plist :static-variables-alist))
  (setf (slot-value box 'display-style-list)
        (restore-display-style (getf plist :display-style-list)))
  (let ((exports (getf plist :exports)))
    (unless (null exports)
      (boxer-eval::set-box-transparency box t)
      ;; there may be some of these still left?
      (unless (eq exports boxer-eval::*exporting-all-variables-marker*)
        (setf (slot-value box 'exports) exports))))
  (setf (slot-value box 'closets) (getf plist :closets))
  (setf (slot-value box 'graphics-info) (getf plist :graphics-sheet))
  ;; restore the backpointer from the graphics sheet
  (unless (null (slot-value box 'graphics-info))
    (setf (graphics-sheet-superior-box (slot-value box 'graphics-info)) box))
  (when (row? (name-row box))
    (setf (superior-box (name-row box)) box))
  (let* ((rows (getf plist :rows))
         (prev-row (car rows)))
    (unless (null rows)
      (insert-row-at-row-no box (car rows) 0)
      (dolist (row (cdr rows))
        (insert-row-after-row box row prev-row)
        (setq prev-row row))))
  (let ((closets (slot-value box 'closets)))
    (unless (null closets)
      (setf (superior-box closets) box)
      (do-row-chas ((obj closets))
        (unless (cha? obj)
          (set-superior-row obj closets)
          (insert-self-action obj)))))
  (dolist (var (slot-value box 'static-variables-alist))
    (let ((handler (get (boxer-eval::static-variable-name var) 'magic-name-insert)))
      (when (not (null handler))
        (funcall handler (boxer-eval::static-variable-value var) box)))))

(defmethod init-box-from-file ((box port-box) plist)
  (call-next-method)
  (let ((target (getf plist :port-target)))
    (if (box? target)
        (set-port-to-box box target)
        (cerror "Try and continue to Load the File"
                "~A is not a valid target for a port"
                target)))
  ;; add an empty row to keep things happy
  ;; since ports get dumped with a NIL first-inferior-row
  (let ((fir (make-row '())))
    (setf (slot-value box 'first-inferior-row) fir)
    (setf (superior-box fir) box)))

(defun crock-fix-sprite-closet-for-changed-slot-names (closet-row sup-box)
  ;; temporarily hook up row
;  (setf (superior-box closet-row) sup-box)
  (do-row-chas ((cha closet-row))
    (when (and (box? cha)
               (not (null (name-row cha))))
      (let ((name-row (name-row cha)))
        (cond ((and (eq (slot-value name-row 'cached-name) 'bu::size)
                    (fast-assq 'bu::sprite-size
                               (slot-value sup-box
                                           'static-variables-alist)))
               (setf (slot-value name-row 'cached-name) 'bu::sprite-size)
               (update-bindings name-row))
              ((and (eq (slot-value name-row 'cached-name) 'bu::home)
                    (fast-assq 'bu::home-position
                               (slot-value sup-box
                                           'static-variables-alist)))
               (setf (slot-value name-row 'cached-name) 'bu::home-position)
               (update-bindings name-row))))))
  ;; now disconnect the row again
;  (setf (superior-box closet-row) nil)
  )

;; moved to box method (no more sprite-box's)
#|
(defmethod init-box-from-file ((box sprite-box) plist)
  (let ((turtle (getf plist :associated-turtle)))
    (cond ((graphics-object? turtle)
           (setf (slot-value box 'associated-turtle) turtle)
           (set-sprite-box turtle box))
          (t
           (warn "The Sprite Box, ~A, doesn't have an associated turtle"
                 box))))
  (call-next-method)
  (when (and (= *file-bin-version* 5)
             *check-for-and-convert-old-vlist-style*
             (not (null (slot-value box 'closets))))
    (crock-fix-sprite-closet-for-changed-slot-names (slot-value box 'closets)
                                                    box)))
|#


;;; Graphics

(defvar *old-style-restore-bitmaps?* nil
  "Probably only Steve ever needs to set this for his Dinosaurs")

(defun load-picture? (&rest args)
  "Used to test whether to load the picture since there might be
  color vs B&W lossage as well as other problems that could prevent a
  successful restoration of a dumped out bitmap.

  For previous systems such as clx and mcl this returned true, but now
  it is alwayw nil."
  (declare (ignore args))
  nil)

;; make sure new graphics list has a HIDDEN slot
(defun post-load-process-graphics-list (graphics-list)
  (let* ((newlist? (< (length graphics-list) 8))
         (newlist (if newlist? (make-graphics-command-list) graphics-list)))
    (when newlist?
      (setf (graphics-command-list-agent newlist)        (graphics-command-list-agent graphics-list)
            (graphics-command-list-pen-width newlist)    (graphics-command-list-pen-width graphics-list)
            (graphics-command-list-font-no newlist)      (graphics-command-list-font-no graphics-list)
            (graphics-command-list-fill-pointer newlist) (graphics-command-list-fill-pointer graphics-list)
            (graphics-command-list-contents newlist)     (graphics-command-list-contents graphics-list)))
    (setf (graphics-command-list-alu newlist)
          (reallocate-file-alu (graphics-command-list-alu graphics-list)))
    (setf (graphics-command-list-pen-color newlist)
          (reallocate-pixel-color (graphics-command-list-pen-color graphics-list)))
    (when (>=& *version-number* 12)
      (setf (graphics-command-list-font-no newlist)
            (make-font-from-file-value (graphics-command-list-font-no graphics-list))))
    (do-vector-contents (command newlist)
      (load-graphics-command command))
    newlist))

(define-load-command-for-value bin-op-graphics-sheet (stream)
  (if (< *file-bin-version* 11.)
      (old-style-load-graphics-sheet stream)
      (let* ((plist (bin-next-value stream))
             ;; plist values
             (background (getf plist :background))
             (graphics-list (getf plist :graphics-list))
             (bit-array (getf plist :pixmap))
             (new-sheet (make-graphics-sheet-from-file
                         (getf plist :draw-wid) (getf plist :draw-hei)
                         (getf plist :draw-mode))))
        (unless (null graphics-list)
          (setf (graphics-sheet-graphics-list new-sheet) (post-load-process-graphics-list graphics-list)))
        (unless (null bit-array)
          (setf (graphics-sheet-bit-array new-sheet) bit-array)
          ;; mark the dirty? flag
          (setf (graphics-sheet-bit-array-dirty? new-sheet) t))
        (unless (null background)
          (setf (graphics-sheet-background new-sheet)
                (reallocate-pixel-color background)))
        new-sheet)))

#+mcl
(defvar *use-mac-fast-bitmap-loaders* t)

(define-load-command-for-value bin-op-pixmap (stream)
  (let ((dump-type (bin-next-value stream)))
    (case dump-type
      (8-bit-run-length-encoded
       #-mcl (load-8-bit-run-length-encoded-pixmap stream)
       #+mcl (if *use-mac-fast-bitmap-loaders*
               (fast-mac-load-8-bit-run-length-encoded-pixmap stream)
               (load-8-bit-run-length-encoded-pixmap-by-strips stream))
       )
      (1-bit-run-length-encoded (load-1-bit-run-length-encoded-pixmap stream))
      (true-color-run-length-encoded
       #-mcl (load-true-color-run-length-encoded-pixmap stream)
       #+mcl (if *use-mac-fast-bitmap-loaders*
               (fast-mac-load-true-color-run-length-encoded-pixmap stream)
               (load-true-color-run-length-encoded-pixmap-by-strips stream)))
      (t (error "Don't know how to load ~A" dump-type)))))


;;; This relies upon reallocate-pixel-color to do the right thing so
;;; that the pixel values are valid for the current depth of *boxer-pane*

(defun load-8-bit-run-length-encoded-pixmap (stream)
  ;; first get the width and the height out
  (let ((width (bin-next-value stream)) (height (bin-next-value stream))
        ;; now the raw colormap
        (colormap (bin-next-value stream)))
    ;; now process the colormap to get a pixel remap
    (dotimes& (i (length colormap))
      (setf (aref colormap i) (reallocate-pixel-color (aref colormap i))))
    ;; now render the data
    (let* ((pixmap (make-offscreen-bitmap *boxer-pane* width height))
           (pixdata (offscreen-bitmap-image pixmap))
           (x 0) (y 0))
      #+opengl
      (loop
          (let* ((word (bin-next-byte stream))
                 (count (ldb& %%bin-op-top-half word))
                 (pixel (aref colormap (ldb& %%bin-op-low-half word))))
            (dotimes& (i count)
              (setf (image-pixel x y pixdata) pixel)
              (incf& x)
              (when (>=& x width) (setq x 0 y (1+& y))))
            (when (>=& y height) (return))))
      #-opengl
      (drawing-on-bitmap (pixmap)
        (loop
          (let* ((word (bin-next-byte stream))
                 (count (ldb& %%bin-op-top-half word))
                 (pixel (aref colormap (ldb& %%bin-op-low-half word))))
            (dotimes& (i count)
              (setf (image-pixel x y pixdata) pixel)
              (incf& x)
              (when (>=& x width) (setq x 0 y (1+& y))))
            (when (>=& y height) (return)))))
      #-opengl
      (set-offscreen-bitmap-image pixmap pixdata)
      pixmap)))

#+mcl
(defun fast-mac-load-8-bit-run-length-encoded-pixmap (stream)
  ;; first get the width and the height out
  (let* ((width (bin-next-value stream)) (height (bin-next-value stream))
         ;; now the raw colormap
         (colormap (bin-next-value stream))
         ;;
         (gworld (make-offscreen-bitmap *boxer-pane* width height))
         (depth (offscreen-bitmap-depth gworld))
         (setpixfun (ecase depth
                      (1 #'%set-1pixel) (8 #'%set-8pixel)
                      (16 #'%set-16pixel) (32 #'%set-32pixel)))
         (rgbpixfun (ecase depth
                      (1 #'mcl-rgb->1pixel) (8 #'mcl-rgb->8pixel)
                      (16 #'mcl-rgb->16pixel) (32 #'mcl-rgb->32pixel)))
         (pixmap (Get-Gworld-Pixmap gworld))
         (row-bytes (ldb& #.(byte 14 0) (ccl::rref pixmap :pixmap.rowbytes)))
         (pix-addr (Get-Pix-Base-Addr pixmap))
         (x 0) (y 0))
    ;(format t "~&Filling ~D pixmap (~A) at ~A with row=~D colormap is ~D" depth pixmap pix-addr row-bytes (length colormap))
    (if (not (unpacked-pixmap? gworld))
        ;; use the all purpose (SLOWER) function
        (load-8-bit-run-length-encoded-pixmap-by-strips stream width height
                                                        colormap gworld)
        (progn
          ;; now process the colormap to get a pixel remap
          (dotimes& (i (length colormap))
            (setf (aref colormap i)
                  ;; we could use the list of rgb values directly (a la
                  ;; boxer-rgb-values->pixel) but go through
                  ;; reallocate-pixel-color to hack fixnum values from old files
                  (funcall rgbpixfun (reallocate-pixel-color (aref colormap i)))))
          ;; now render the data
          (drawing-on-bitmap (gworld)
            (loop
              (let* ((word (bin-next-byte stream))
                     (count (ldb& %%bin-op-top-half word))
                     (pixel (aref colormap (ldb& %%bin-op-low-half word))))
                (let ((newpixaddr (Get-Pix-Base-Addr pixmap)))
                  (unless (eql newpixaddr pix-addr)
                    (warn "Pixmap base address has moved from ~X to ~X"
                          pix-addr newpixaddr)
                    (setq pix-addr newpixaddr)))
                (dotimes& (i count)
                  (funcall setpixfun pix-addr pixel x y row-bytes)
                  (incf& x)
                  (when (>=& x width) (setq x 0 y (1+& y))))
                (when (>=& y height) (return)))))
          gworld))))

;; all the optional args are for the possibility that we come here via the
;; fast-mac-load-xxx functions when all those parameters have already been
;; computed

(defun load-8-bit-run-length-encoded-pixmap-by-strips (stream
                                                       &optional
                                                       (width
                                                        (bin-next-value stream))
                                                       (height
                                                        (bin-next-value stream))
                                                       (colormap
                                                        (bin-next-value stream))
                                                       (pixmap
                                                        (make-offscreen-bitmap
                                                         *boxer-pane*
                                                         width height)))
  ;; now process the colormap to get a pixel remap
  (dotimes& (i (length colormap))
    (setf (aref colormap i) (reallocate-pixel-color (aref colormap i))))
  ;; now render the data
  (let* ((pixdata (offscreen-bitmap-image pixmap)) (x 0) (y 0))
    (drawing-on-bitmap (pixmap)
      (loop
        (let* ((word (bin-next-byte stream))
               (count (ldb& %%bin-op-top-half word))
               (pixel (aref colormap (ldb& %%bin-op-low-half word)))
               (draw-wid (min& count (-& width x))))
          (loop
            (with-pen-color (pixel)
              (draw-rectangle draw-wid 1 x y))
            (setq count (-& count draw-wid)
                  x (let ((newx (+& x draw-wid)))
                      (cond ((>=& newx width) (setq y (1+& y)) 0)
                            (t newx)))
                  draw-wid (min& count (-& width x)))
            (when (<=& count 0) (return)))
          (when (>=& y height) (return)))))
    (set-offscreen-bitmap-image pixmap pixdata)
    pixmap))

(defun load-true-color-run-length-encoded-pixmap (stream)
  (let* ((width (bin-next-value stream)) (height (bin-next-value stream))
         ;; first get the width and the height out
         (pixmap (make-offscreen-bitmap *boxer-pane* width height))
         (pixdata (offscreen-bitmap-image pixmap))
         (true-color? (>= (offscreen-bitmap-depth pixmap) 16.))
         (x 0) (y 0))
    (declare (fixnum width height x y))
    #+opengl
    (loop
        (let* ((1st-word (bin-next-byte stream))
               (2nd-word (bin-next-byte stream))
               (count (ldb& %%bin-op-top-half 1st-word))
               (red (ldb& %%bin-op-low-half 1st-word))
               (green (ldb& %%bin-op-top-half 2nd-word))
               (blue (ldb& %%bin-op-low-half 2nd-word))
               (pixel (if true-color?
                        (opengl::make-offscreen-pixel red green blue)
                        ;; we should stack cons this list...
                        (reallocate-pixel-color (list red green blue)))))
          (dotimes& (i count)
            (setf (image-pixel x y pixdata) pixel)
            (incf& x)
            (when (>=& x width) (setq x 0 y (1+& y))))
          (when (>=& y height) (return))))
    #-opengl
    (drawing-on-bitmap (pixmap)
      (loop
        (let* ((1st-word (bin-next-byte stream))
               (2nd-word (bin-next-byte stream))
               (count (ldb& %%bin-op-top-half 1st-word))
               (red (ldb& %%bin-op-low-half 1st-word))
               (green (ldb& %%bin-op-top-half 2nd-word))
               (blue (ldb& %%bin-op-low-half 2nd-word))
               (pixel (if true-color?
                        #+mcl
                        (dpb& red #.(byte 8 16) 2nd-word)
                        #+lwwin
                        (dpb& blue #.(byte 8 16)
                              (dpb& green #.(byte 8 8) red))
                        #-(or mcl lwwin opengl)
                        (dpb& red (byte 8 16) 2nd-word)
                        ;; we should stack cons this list...
                        (reallocate-pixel-color (list red green blue)))))
          (dotimes& (i count)
            (setf (image-pixel x y pixdata) pixel)
            (incf& x)
            (when (>=& x width) (setq x 0 y (1+& y))))
          (when (>=& y height) (return)))))
    #-opengl
    (set-offscreen-bitmap-image pixmap pixdata)
    pixmap))

#+mcl
(defun fast-mac-load-true-color-run-length-encoded-pixmap (stream)
  (let* ((width (bin-next-value stream)) (height (bin-next-value stream))
         ;; first get the width and the height out
         (gworld (make-offscreen-bitmap *boxer-pane* width height))
         (depth (offscreen-bitmap-depth gworld))
         (setpixfun (ecase depth
                      (1 #'%set-1pixel) (8 #'%set-8pixel)
                      (16 #'%set-16pixel) (32 #'%set-32pixel)))
         (boxpixfun (ecase depth
                      (1  #'byte-rgb-values->1pixel)
                      (8  #'byte-rgb-values->8pixel)
                      (16 #'byte-rgb-values->16pixel)
                      (32 #'byte-rgb-values->32pixel)))
         (pixmap (Get-Gworld-Pixmap gworld))
         (row-bytes (ldb& #.(byte 14 0) (ccl::rref pixmap :pixmap.rowbytes)))
         (pix-addr (Get-Pix-Base-Addr pixmap))
         (x 0) (y 0))
    (declare (fixnum width height x y))
    (if (not (unpacked-pixmap? gworld))
        ;; do the generic thing if the pixmap is packed in some way
        (load-true-color-run-length-encoded-pixmap-by-strips stream width height
                                                             gworld)
        (drawing-on-bitmap (gworld)
          (loop
            (let* ((1st-word (bin-next-byte stream))
                   (2nd-word (bin-next-byte stream))
                   (count (ldb& %%bin-op-top-half 1st-word))
                   (red (ldb& %%bin-op-low-half 1st-word))
                   (green (ldb& %%bin-op-top-half 2nd-word))
                   (blue  (ldb& %%bin-op-low-half 2nd-word))
                   (pixel (funcall boxpixfun red green blue)))
              (let ((newpixaddr (Get-Pix-Base-Addr (Get-Gworld-Pixmap gworld))))
                  (unless (eql newpixaddr pix-addr)
                    (warn "Pixmap base address has moved from ~X to ~X"
                          pix-addr newpixaddr)
                    (setq pix-addr newpixaddr)))
              (dotimes& (i count)
                (funcall setpixfun pix-addr pixel x y row-bytes)
                (incf& x)
                (when (>=& x width) (setq x 0 y (1+& y))))
              (when (>=& y height) (return))))
          gworld))))

(defun load-true-color-run-length-encoded-pixmap-by-strips (stream
                                                            &optional
                                                            (width (bin-next-value
                                                                    stream))
                                                            (height (bin-next-value
                                                                     stream))
                                                            (pixmap
                                                             (make-offscreen-bitmap
                                                              *boxer-pane*
                                                              width height)))
  (let ((pixdata (offscreen-bitmap-image pixmap))
        (true-color? (>= (offscreen-bitmap-depth pixmap) 16.))
        (x 0) (y 0))
    (declare (fixnum width height x y))
    (drawing-on-bitmap (pixmap)
      (loop
        (let* ((1st-word (bin-next-byte stream))
               (2nd-word (bin-next-byte stream))
               (count (ldb& %%bin-op-top-half 1st-word))
               (red (ldb& %%bin-op-low-half 1st-word))
               (green (ldb& %%bin-op-top-half 2nd-word))
               (blue (ldb& %%bin-op-low-half 2nd-word))
               (pixel (if true-color?
                        (dpb& red (byte 8 16) 2nd-word)
                        ;; we should stack cons this list...
                        (reallocate-pixel-color (list red green blue))))
               (draw-wid (min& count (-& width x))))
            (loop
              (with-pen-color (pixel)
                (draw-rectangle draw-wid 1 x y))
                (setq count (-& count draw-wid)
                      x (let ((newx (+& x draw-wid)))
                          (cond ((>=& newx width) (setq y (1+& y)) 0)
                                (t newx)))
                      draw-wid (min& count (-& width x)))
                (when (<=& count 0) (return)))
            (when (>=& y height) (return)))))
    (set-offscreen-bitmap-image pixmap pixdata)
    pixmap))

(defun put-picture-byte (pic x y byte &optional size)
  (declare (type (simple-array bit (* *)) pic)
           (fixnum x y byte))
  (dotimes& (i (or size 8))
    (setf (image-pixel pic y (+& x i))
          ;; have to swap the bytes, blecchh...
          (if (zerop& (ldb& (byte 1 (-& 7 i)) byte))
              *background-color*
              *foreground-color*))))

(defun load-1-bit-run-length-encoded-pixmap (stream)
  ;; first get the width and the height out
  (let* ((width (bin-next-value stream)) (height (bin-next-value stream))
         (pixmap (make-offscreen-bitmap *boxer-pane* width height))
         (pixdata (offscreen-bitmap-image pixmap))
         ;; vars
         (current-byte 0) (rep-count 0) (data-count 0)
         (extra-data-p nil) (other-half 0))
    (labels ((handle-word ()
               ;; this should only be called if repeat AND count are 0
               ;; it will set up either a repeat or count situation
               (let* ((word (bin-next-byte stream))
                      (top (ldb& %%bin-op-top-half word))
                      (low (ldb& %%bin-op-low-half word)))
                 (cond ((=& top *pic-data-count-prefix*)
                        ;; looks like we should set up a count situation
                        (setq data-count low))
                       (t
                        ;; must be a repeat
                        (setq rep-count top current-byte low)))))

             (get-count-data-word ()
               (let* ((word (bin-next-byte stream))
                      (top (ldb& %%bin-op-top-half word))
                      (low (ldb& %%bin-op-low-half word)))
                 (setq extra-data-p t other-half top)
                 low))

             (get-count-data-bits ()
               (if (null extra-data-p)
                   (get-count-data-word)
                   (progn (setq extra-data-p nil) other-half)))

             (get-bits ()
               (cond ((=& 1 data-count)
                      (decf& data-count)
                      (setq current-byte (get-count-data-bits)
                            extra-data-p  nil)
                      current-byte)
                     ((not (zerop& data-count))
                      (decf& data-count)
                      (setq current-byte (get-count-data-bits)))
                     ((not (zerop& rep-count))
                      (decf& rep-count)
                      current-byte)
                     (t (handle-word) (get-bits)))))
      ;; Here's the body
      (multiple-value-bind (whole-words-per-row leftover-pixels)
          (floor width 16.)
        (drawing-on-bitmap (pixmap)
          (dotimes& (row height)
            (dotimes& (rb whole-words-per-row)
              (put-picture-byte pixdata (ash& rb 4) row (get-bits) 8.)
              (put-picture-byte pixdata (+& (ash& rb 4) 8.) row (get-bits) 8.))
            ;; handle any protrusions
            (when (not (zerop& leftover-pixels))
              (put-picture-byte pixdata (ash& whole-words-per-row 4)
                                row (get-bits) (min& 8 leftover-pixels))
              ;; remember that mutiples of 16 bit words are
              ;; being written out so that even if there was
              ;; nothing there, we still need to read in the padding
              (if (<& leftover-pixels 8.)
                (get-bits)
                (put-picture-byte pixdata (+& (ash& whole-words-per-row 4) 8)
                                  row (get-bits) (-& leftover-pixels 8))))))))
    (set-offscreen-bitmap-image pixmap pixdata)
    pixmap))


(defun old-style-load-graphics-sheet (stream)
  (let* ((plist (bin-next-value stream))
         (new-sheet (make-graphics-sheet-from-file (getf plist :draw-wid)
                                                   (getf plist :draw-hei)
                                                   (getf plist :draw-mode))))
    (cond ((not (null (getf plist :graphics-list)))
           (post-load-process-graphics-list (getf plist :graphics-list))
           (setf (graphics-sheet-graphics-list new-sheet)
                 (getf plist :graphics-list)))
          ((= *file-bin-version* 5)
           (setf (graphics-sheet-graphics-list new-sheet)
                 (make-graphics-command-list))))
    (let ((background (getf plist :background)))
      (unless (null background)
        (setf (graphics-sheet-background new-sheet)
              (reallocate-pixel-color background))))
    ;; now fill the bit array if we have bits to fill
    (unless (getf plist :picture-was-not-dumped)
      (let ((bitmap (when (load-picture?)
                      (make-offscreen-bitmap *boxer-pane*
                                             (getf plist :draw-wid)
                                             (getf plist :draw-hei))))
            #+clx
            (image (bw::create-image :width  (getf plist :draw-wid)
                                     :height (getf plist :draw-hei)
                                     :depth 1
                                     :data (list
                                            (make-array
                                             `(,(getf plist :draw-hei)
                                                ,(getf plist :draw-wid))
                                             :element-type 'bit))
                                     :format :xy-pixmap)))
        ;; install the bitmap
        (unless (and (= *file-bin-version* 5)
                     (null *old-style-restore-bitmaps?*))
          (setf (graphics-sheet-bit-array new-sheet) bitmap))
        (initialize-picture ;; need to dereference a few more pointers for X
                            #+X (pixrect::mpr_data.md-image
                                    (pixrect::pixrect.pr-data
                                       (graphics-sheet-bit-array new-sheet)))
                            #+CLX (car (bw::image-xy-bitmap-list image))
                            #-(or X CLX) bitmap
                            (graphics-sheet-draw-wid  new-sheet)
                            (graphics-sheet-draw-hei  new-sheet)
                            stream)
        #+clx
        ;; update the pixmap in the server
        (when (load-picture?)
          (let ((gc (bw::create-gcontext :drawable bitmap :function alu-seta)))
            (bw::put-image bitmap GC image :x 0 :y 0
                           :width (graphics-sheet-draw-wid  new-sheet)
                           :height (graphics-sheet-draw-hei  new-sheet)
                           :bitmap-p t)
            ;; IWBNI we could free the consed up image here
            (bw::free-gcontext gc))))
      ;; mark the dirty? flag
      (setf (graphics-sheet-bit-array-dirty? new-sheet) t))
    new-sheet))



;;; bash the alu's to the boxer package if they have been dumped out
;;; in the keyword package

(defvar *check-for-and-convert-old-vlist-style* t)

(defun fix-alus (shape-slot)
  (rplaca shape-slot
          (mapcar #'(lambda (x) (if (fast-memq x '(:up :down :xor :rase))
                                    (intern (symbol-name x)
                                            (find-package 'boxer))
                                  x))
                  (car shape-slot)))
  shape-slot)

(defun convert-old-style-shape (shape turtle)
  (let* ((new-shape (make-graphics-command-list
                     (expt 2 (+ 2 (integer-length
                                   (length (the list shape)))))))
         (%graphics-list new-shape))
    (flet ((new-shape-preamble (agent graphics-command-list)
             (setf (graphics-command-list-agent graphics-command-list) agent)
             ;; the  alu of old turtle's should be XOR
             (setf (graphics-command-list-alu graphics-command-list) alu-xor)
             (record-boxer-graphics-command-change-alu alu-xor)
             ;; the pen-width of old turtle's is 1
             (setf (graphics-command-list-pen-width graphics-command-list) 1)
             (setf (slot-value turtle 'pen-width)
                   (%make-vv-box-interface 1 'pen-width))
             (record-boxer-graphics-command-change-pen-width 1)))
      (if (not (eq (car shape) 'turtle-vector-list))
          (error "~S is not a valid turtle old style turtle shape" shape)
          (let ((current-x 0) (current-y 0) (pen 'down))
            ;; initial setup
            (new-shape-preamble turtle new-shape)
            (dolist (com (cdr shape))
              (cond ((symbolp com)
                     (if (fast-memq com '(up down))
                         (setq pen com)
                         (warn "Ignoring ~S in shape" com)))
                    ((and (consp com)
                          (numberp (car com))
                          (numberp (cdr com)))
                     ;; looks like a turtle coordinate
                     (let ((next-x (+& current-x (fixr (car com))))
                           (next-y (+& current-y (fixr (- (cdr com))))))
                       (when (eq pen 'down)
                         (record-boxer-graphics-command-line-segment
                          current-x current-y next-x next-y))
                       (setq current-x next-x current-y next-y)))
                    ((eq pen 'up))
                    (t
                     (case (car com)
                       (c-rect
                        (record-boxer-graphics-command-centered-rectangle
                         current-x current-y (cadr com) (caddr com)))
                       (dot
                        (record-boxer-graphics-command-dot
                         current-x current-y))
                       (otherwise
                        (error "Don't know how to handle ~S" com)))))))))
    (when *boxer-system-hacker*
      (format t "~%Converted ~A to:" shape)
      (show-graphics-list new-shape))
    new-shape))

(defun convert-turtle-instance-slot-box-name (box new-name)
  (let ((new-name-row (make-name-row (list new-name))))
    (setf (superior-box new-name-row) box)
    (setf (slot-value box 'name) new-name-row))
  box)

(defun coerce-turtle-values (turtle)
  (setf (box-interface-value (slot-value turtle 'x-position))
        (float (box-interface-value (slot-value turtle 'x-position))))
  (setf (box-interface-value (slot-value turtle 'y-position))
        (float (box-interface-value (slot-value turtle 'y-position))))
  (setf (box-interface-value (slot-value turtle 'home-position))
        (let  ((old-hp
                (box-interface-value (slot-value turtle 'home-position))))
          (list (float (car old-hp)) (float (cadr old-hp)))))
  ;; fill in new instance slots (with reasonable values)
  (setf (slot-value turtle 'type-font)
        (%make-iv-box-interface *sprite-type-font-no* 'type-font))
  (setf (slot-value turtle 'pen-color)
        (%make-iv-box-interface *foreground-color* 'pen-color))
  ;; a hack to convert the values of the graphics commands to
  (do-vector-contents (gc (shape turtle))
    (unless (>= (svref gc 0) 32.)
      (setf (svref gc 0) (+ (svref gc 0) 32)))
    (do* ((idx 1 (1+& idx))
          (tp-items (graphics-command-transform-template gc) (cdr tp-items))
          (tp-action (car tp-items) (car tp-items)))
         ((null tp-items))
      (unless (null tp-action)
        (setf (svref gc idx) (float (svref gc idx)))))))

;;; this should be changed to check for the file bin version in order to
;;; move all the detailed checks into the old style file case

(define-load-command bin-op-turtle (stream)
  (if (>= *file-bin-version* 8.)
      (new-style-load-graphics-object stream)
      (old-style-load-turtle stream)))

;; try and pull things out of the stream WITHOUT consing a Plist...
;; for a debugging version, we can CONS the list...
(defun new-style-load-graphics-object (stream)
  (let ((graphics-object nil)
        (current-keyword nil))
    (with-loading-list (stream item-no)
      (cond ((=& item-no 0)
             (unless (eq (setq current-keyword (bin-next-value stream)) 'type)
               ;; sanity check
               (error "Expected the TYPE keyword instead of ~S"
                      current-keyword)))
            ((=& item-no 1)
             (let ((go-class (bin-next-value stream)))
               (unless (find-class go-class nil)
                 (error "~S is not a valid class of graphics object" go-class))
               (setq graphics-object
                     (let ((new (allocate-instance (find-class go-class))))
                       (setf (slot-value new 'assoc-graphics-box) nil)
                       (setf (slot-value new 'superior-turtle) nil)
                       new))))
            ((evenp item-no)
             ;; expect a keyword next...
             (let ((next-keyword (bin-next-value stream)))
               (unless (symbolp next-keyword)
                 (error "Expected a Keyword instead of ~S" next-keyword))
               (setq current-keyword next-keyword)))
            (t
             (partial-initialize graphics-object
                                 current-keyword (bin-next-value stream)))))
    ;; fixup any private graphics lists...
    (if (slot-boundp graphics-object 'private-gl)
        (setf (slot-value graphics-object 'private-gl)
              (post-load-process-graphics-list (slot-value graphics-object 'private-gl)))
      (setf (slot-value graphics-object 'private-gl) (make-graphics-command-list)))
    graphics-object))

(defmethod partial-initialize ((self graphics-object) keyword value)
  (case keyword
    (x-position (setf (slot-value self 'x-position)
                      (%make-vv-box-interface value 'x-position)))
    (y-position (setf (slot-value self 'y-position)
                      (%make-vv-box-interface value 'y-position)))
    (subsprites  (setf (slot-value self 'subsprites) value))))

(defmethod partial-initialize ((self button) keyword value)
  (or (call-next-method)		; returns non NIL if already handled
      ;; try and handle it here...
      (case keyword
        (shape (setf (slot-value self 'shape)
                     (%make-sv-box-interface
                      (post-load-process-graphics-list value) 'shape nil 'shape-box-updater))
               ;; now that we have (or should have) a shape, fill in
               ;; the auxiliary data structures
               (setf (slot-value self 'window-shape)
                     (make-turtle-window-shape (shape self))))
        (save-under (progn
                      ;; crock for initializing in save-unders
                      ;; when sprite-size is undefined
                      (when (<= *file-bin-version* 9)
                        (setf (slot-value self 'sprite-size)
                              (%make-iv-box-interface 1 'sprite-size)))
                      (cond ((eq value 'save-under)
                             (setf (slot-value self 'save-under) nil)
                             ;; handle
                             (update-save-under self))
                            ((fast-memq value *valid-save-under-keywords*)
                             (setf (slot-value self 'save-under) value))
                            (t
                             (warn "~S is a bad value for the save-under")
                             (setf (slot-value self 'save-under) nil)
                             (update-save-under self))))))))

(defmethod partial-initialize ((self graphics-cursor) keyword value)
  (or (call-next-method)		; returns non NIL if already handled
      ;; try and handle it here...
      (case keyword
        (shown? (setf (slot-value self 'shown?)
                      (%make-iv-box-interface value 'shown?)))
        (pen (setf (slot-value self 'pen) (%make-iv-box-interface value 'pen)))
        (pen-width (setf (slot-value self 'pen-width)
                         (%make-vv-box-interface value 'pen-width)))
        ;; as of 8/22/00 in the PC version, these values will have been
        ;; canonicalized (list of 3 RGB floats).  (mac) Files made earlier may have
        ;; a 24 bit RGB fixnum here instead. fortunately, reallocate-pixel-color
        ;; is smart enough to deal with both
        (pen-color
         (setf (slot-value self 'pen-color)
               (%make-sv-box-interface (reallocate-pixel-color value) 'pen-color
                                       nil 'pen-color-box-updater)))
        (type-font (setf (slot-value self 'type-font)
                        (%make-sv-box-interface
                         (if (>=& *version-number* 12)
                             (make-font-from-file-value value)
                             value)
                         'type-font nil 'type-font-box-updater)))
        ;; compatibility blues...
        (pen-font (setf (slot-value self 'type-font)
                        (%make-iv-box-interface value 'type-font)))
        (shown-p (setf (slot-value self 'shown?)
                       (%make-iv-box-interface value 'shown?))))))

(defmethod partial-initialize ((self turtle) keyword value)
  (or (call-next-method)		; returns non NIL if already handled
      ;; try and handle it here...
      (case keyword
        (heading (setf (slot-value self 'heading)
                       (%make-vv-box-interface value 'heading)))
        (home-position (setf (slot-value self 'home-position)
                             (%make-iv-box-interface value 'home-position)))
        (sprite-size (prog1
                         (setf (slot-value self 'sprite-size)
                               (%make-iv-box-interface value 'sprite-size))
                       ;; need to check to see if the save-under needs
                       ;; to be reitinitialized since it was created
                       ;; with an assumed sprite-size of 1
                       (when (and  (<= *file-bin-version* 9)
                                   (not (= value 1))
                                   (slot-boundp self 'save-under)
                                   (save-under-p (slot-value self
                                                             'save-under)))
                         (update-save-under self))))
        (private-gl (setf (slot-value self 'private-gl) value)))))


(defun old-style-load-turtle (stream)
  (let ((plist (bin-next-value stream))
        (turtle
         #+pcl
          (let ((pcl::*slot-unbound* nil))
            (allocate-instance (find-class 'turtle)))
          #+(or clos mcl)
          (let ((new (allocate-instance (find-class 'turtle))))
            (setf (slot-value new 'assoc-graphics-box) nil)
            (setf (slot-value new 'superior-turtle) nil)
            (setf (slot-value new 'type-font)
                  *initial-graphics-state-current-font-no*)
            (setf (slot-value new 'pen-color)
                  *initial-graphics-state-current-pen-color*)
            new)))
    (do* ((list plist (cddr list))
          (slot (car list) (car list))
          (value (cadr list) (cadr list)))
         ((null list))
      (cond ((and (not (null *check-for-and-convert-old-vlist-style*))
                  (eq slot 'shape))
             (setf (slot-value turtle slot)
                   (%make-iv-box-interface
                    (if (listp (car value))
                        (convert-old-style-shape (car value) turtle)
                        (car value))
                    'shape))
             ;(update-save-under turtle)
             (setf (turtle-save-under turtle) 'xor-redraw))
            ((and (not (null *check-for-and-convert-old-vlist-style*))
                  (eq slot 'size))
             (setf (slot-value turtle 'sprite-size)
                   (%make-iv-box-interface (float (car value)) 'sprite-size)))
            ((and (not (null *check-for-and-convert-old-vlist-style*))
                  (eq slot 'home))
             (setf (slot-value turtle 'home-position)
                   (%make-iv-box-interface (car value) 'home-position)))
            ((eq slot 'save-under)
             (cond ((eq value 'xor-redraw)
                    (setf (slot-value turtle slot) value))
                   ((eq value 'save-under)
                    (setf (slot-value turtle slot) nil)
                    (update-save-under turtle))
                   (t
                    (warn "~S is a bad value for the save-under")
                    (setf (slot-value turtle slot) nil)
                    (update-save-under turtle))))
            ((fast-memq slot
                        '(x-position y-position heading pen-width sprite-size))
             (setf (slot-value turtle slot)
                   (%make-vv-box-interface (car value) slot)))
            ((eq slot 'shown-p)
             (setf (slot-value turtle 'shown?)
                   (%make-iv-box-interface (car value) 'shown?)))
            ((eq slot 'pen-font)
             (setf (slot-value turtle 'type-font)
                   (%make-iv-box-interface (car value) 'type-font)))
            ((fast-memq slot '(pen pen-color home-position))
             (setf (slot-value turtle slot)
                   (%make-iv-box-interface (car value) slot)))
            (t
             (setf (slot-value turtle slot) value))))
    ;; now that the slots have been filled from the file, handle
    ;; the remaining slots that were not dumped out but still need valid
    ;; values
    (when (<= *file-bin-version* 6)
      (coerce-turtle-values turtle))
    ;; temporary redundant sanity check
    (when (null (slot-value turtle 'type-font))
      (warn "Null pen-font, fixing...")
      (setf (slot-value turtle 'type-font)
            (%make-iv-box-interface *sprite-type-font-no* 'type-font)))
    (when (null (slot-value turtle 'pen-color))
      (warn "Null pen-color, fixing...")
      (setf (slot-value turtle 'pen-color)
            (%make-iv-box-interface *foreground-color* 'pen-color)))
    (setf (slot-value turtle 'window-shape)
          (make-turtle-window-shape (shape turtle)))
    turtle))

;(define-load-command bin-op-turtle (stream)
;  (let ((plist (bin-next-value stream))
;	(turtle (allocate-instance (find-class 'turtle))))
;    (do* ((list plist (cddr list))
;	  (slot (car list) (car list))
;	  (value (cadr list) (cadr list)))
;	 ((null list) turtle)
;      (setf (slot-value turtle slot) value))))

#+clx
(defun old-style-put-picture-byte (pic x y byte &optional size)
  (declare (type (simple-array bit (* *)) pic)
           (fixnum x y byte size))
  (dotimes& (i (or size 8))
    (setf (aref pic y (+& x i))
          ;; have to swap the bytes, blecchh...
          (ldb& (byte 1 (-& 7 i)) byte))))

#+X
(defun old-style-put-picture-byte (pic x y byte &optional size bytes-per-row)
  (declare (ignore size))
  (setf (xlib::crefi-byte pic (+& (ash& x -3) (*& y bytes-per-row)))
        byte))

#+(or MCL OpenGL)
(defun old-style-put-picture-byte (pic x y byte &optional size bytes-per-row)
  (declare (ignore pic x y byte &optional size bytes-per-row))
  nil)

;;; this is old style, leave here in order to load old files
(defun initialize-picture (pic width height stream)
  ;; sanity check
  (if (/= (bin-next-byte stream) bin-op-picture)
      (error "Expected a picture to start here")
      (let ((current-byte 0)
            (rep-count 0)
            (data-count 0)
            (extra-data-p nil)
            (other-half 0))
        (labels ((handle-word ()
                   ;; this should only be called if repeat AND count are 0
                   ;; it will set up either a repeat or count situation
                   (let* ((word (bin-next-byte stream))
                          (top (ldb& %%bin-op-top-half word))
                          (low (ldb& %%bin-op-low-half word)))
                     (cond ((=& top *pic-data-count-prefix*)
                            ;; looks like we should set up a count situation
                            (setq data-count low))
                           (t
                            ;; must be a repeat
                            (setq rep-count    top
                                  current-byte low)))))

                 (get-count-data-word ()
                   (let* ((word (bin-next-byte stream))
                          (top (ldb& %%bin-op-top-half word))
                          (low (ldb& %%bin-op-low-half word)))
                     (setq extra-data-p t
                           other-half   top)
                     low))

                 (get-count-data-bits ()
                   (if (null extra-data-p)
                       (get-count-data-word)
                       (progn (setq extra-data-p nil)
                              other-half)))

                 (get-bits ()
                   (cond ((=& 1 data-count)
                          (decf& data-count)
                          (setq current-byte (get-count-data-bits)
                                extra-data-p  nil)
                          current-byte)
                         ((not (zerop& data-count))
                          (decf& data-count)
                          (setq current-byte (get-count-data-bits)))
                         ((not (zerop& rep-count))
                          (decf& rep-count)
                          current-byte)
                         (t (handle-word)
                            (get-bits)))))
          ;; Here's the body
          (multiple-value-bind (whole-words-per-row leftover-pixels
                                                    #+X bytes-per-row)
              (floor width 16.)
            #+X (setq bytes-per-row (+& (*& 2 whole-words-per-row)
                                        (if (zerop& leftover-pixels)
                                            0
                                            2)))
            (dotimes& (row height)
                      (dotimes& (rb whole-words-per-row)
                                (old-style-put-picture-byte
                                 pic (ash& rb 4) ; (* rb 16)
                                 row (get-bits) 8.
                                 #+X bytes-per-row)
                                (old-style-put-picture-byte
                                 pic (+& (ash& rb 4) 8.)
                                 row (get-bits) 8.
                                 #+X bytes-per-row))
                      ;; handle any protrusions
                      (when (not (zerop& leftover-pixels))
                        (old-style-put-picture-byte
                         pic (ash& whole-words-per-row 4)
                         row (get-bits) (min& 8 leftover-pixels)
                         #+X bytes-per-row)
                        ;; remember that mutiples of 16 bit words are
                        ;; being written out so that even if there was
                        ;; nothing there, we still need to read in the padding
                        (if (<& leftover-pixels 8.)
                            (get-bits)
                            (old-style-put-picture-byte
                             pic (+& (ash& whole-words-per-row 4) 8)
                             row (get-bits) (-& leftover-pixels 8)
                             #+X bytes-per-row)))))))))



;;;; File Property utilities
;; change from lists to vectors 12/2/95 to support multiple boxes

(defun make-boxer-file-property (creation-date author box &rest other-properties)
  ;(list* creation-date author box other-properties)
  (vector creation-date author (list box) other-properties))

(defun boxer-file-creation-date (file-prop)
  (svref& file-prop 0))

(defun set-boxer-file-creation-date (file-prop new-date)
  (setf (svref& file-prop 0) new-date))

(defun boxer-file-author (file-prop)
  (svref& file-prop 1))

(defun set-boxer-file-author (file-prop new-author)
  (setf (svref& file-prop 1) new-author))

;; this just returns the top of the list, we don;t eliminate bad (no longer
;; part of the hierarchy) entries here because it is possible to delete,
;; check and then paste back.  If it becomes real important, we can add
;; a check in deallocate-self.  For now, the caller should check and
;; call cycle-boxer-file-boxes if the entry is bad

;; nononono, cycle bad entries to the back and return 1st good one
(defun boxer-file-box (file-prop)
  (let* ((1st (car (svref& file-prop 2)))
         (current nil))
    ;; check the 1st entry
    (cond ((null 1st) nil)
          ((superior? 1st *initial-box*) 1st)
          (t ; loop through the rest
           (cycle-boxer-file-boxes file-prop)
           (setq current (car (svref& file-prop 2)))
           (loop (cond ((eq 1st current) (return 1st))
                       ((superior? current *initial-box*) (return current))
                       (t (cycle-boxer-file-boxes file-prop)
                          (setq current (car (svref& file-prop 2))))))))))

(defun cycle-boxer-file-boxes (file-prop)
  (setf (svref& file-prop 2) (nconc (cdr (svref& file-prop 2))
                                    (list (car (svref& file-prop 2))))))

(defun member-boxer-file-box (box file-prop) (fast-memq box (svref& file-prop 2)))

;; make sure that there are no duplicates, new-box gets
;; pushed onto the head of the box lsit so the list becomes time ordered
(defun set-boxer-file-box (file-prop new-box)
  (let* ((boxes (svref& file-prop 2))
         (here? (fast-memq new-box boxes)))
    (when here? (fast-delq new-box (svref& file-prop 2)))
    (push new-box (svref& file-prop 2))))

(defun get-boxer-file-properties (pathname)
  (gethash pathname *file-properties-table*))

(defun record-boxer-file-properties (pathname creation-date author box
                                              &rest other-props)
  (let ((existing-props (gethash pathname *file-properties-table*)))
    (cond ((null existing-props)
           (setf (gethash pathname *file-properties-table*)
                 (apply #'make-boxer-file-property
                        creation-date author box other-props)))
          (t
           (set-boxer-file-creation-date existing-props creation-date)
           (set-boxer-file-author existing-props author)
           (set-boxer-file-box existing-props box)))))




;;; Top level interface

(defun load-binary-box-internal (pathname)
  (with-open-file (filestream pathname :element-type '(unsigned-byte 8.))
    (with-mouse-cursor (:file-io)
      (if (null *file-system-verbosity*)
          (load-binary-box-from-stream-internal filestream)
          (unwind-protect
               (let ((*status-line-loading-format-string*
                      (format nil "Reading ~A ~~D (~~D %)"
                              (filename-for-format-string filestream))))
                 (status-line-display 'loading-box
                                      (format nil
                                              "Loading Box from ~A"
                                              pathname))
                 (let ((*current-file-length* (file-length filestream)))
                   (load-binary-box-from-stream-internal filestream)))
            (status-line-undisplay 'loading-box))))))

(defvar *verbose-filename-for-format-string* nil)

(defun filename-for-format-string (filestream)
  (let* ((raw-name (truename filestream))
         (name (if *verbose-filename-for-format-string*
                   (namestring raw-name)
                   (let ((type (pathname-type raw-name)))
                     (if (or (null type) (eq type :unspecific))
                         (pathname-name raw-name)
                         (format nil "~A.~A" (pathname-name raw-name) type)))))
         (length (length name))
         (return-string (make-array length :element-type 'standard-char
                                    :adjustable t :fill-pointer 0)))
    (dotimes (i length )
      (let ((char (char name i)))
        (if (char= char #\~)
            (dotimes (i 2) (vector-push-extend #\~ return-string))
            (vector-push-extend char return-string))))
    return-string))

(defun load-binary-box-from-stream-internal (filestream)
  (with-post-load-autoloading (filestream)
    (loading-bin-file (filestream 'bin-load-next-command)
      (let ((*package* (find-package 'boxer))
            (boxer-eval::*primitive-shadow-warning-on?* nil))
        (bin-load-top-level filestream nil)))))

(defun bin-load-top-level (stream &optional skip-reading-property-list
                                  &aux box-to-return initial-word)
  (unless skip-reading-property-list
    (let ((word (bin-next-byte stream)))
      (setq initial-word word)
      (when (=& word bin-op-file-property-list)
        ;; if there is a PLIST, then process it
        (bin-next-command stream word)
        ;; if we've used the initial word up, set it back to nil
        (setq initial-word nil))))
  ;; presumably, the only thing left after the file's
  ;; plist will be the top level box
  (catch 'bin-load-done
    (setq box-to-return (bin-next-value stream initial-word))  ;top level box
    (loop (bin-next-command stream)))
    box-to-return)


(defun decode-bin-opcode (word)
  (let ((high (ldb& %%bin-op-high word))
        (low (ldb& %%bin-op-low word)))
    (if (or (=& high bin-op-command-immediate) (=& high bin-op-box-immediate))
        low
        (values high low))))

(defun bin-load-start (stream)
  (let ((word (bin-next-byte stream)))
    (unless (or (=& word bin-op-format-version)
                (when (=& word *swapped-bin-op-format-version*)
                  (warn "Toggling byte swapping of file words...")
                  (toggle-reader-byte-swapping)
                  (setq word bin-op-format-version)
                  t))
      (boxer-eval::primitive-signal-error :bad-file-arg
                                    (if (typep stream 'file-stream)
                                        (namestring (truename stream))
                                        (type-of stream))
                                    "is not a Boxer file"))
    (bin-next-command stream word)))

(defun enter-bin-load-table-internal (value index)
  (and (>= index (length *bin-load-table*))
       (adjust-array *bin-load-table* (* 2 (length *bin-load-table*))))
  (setf (aref *bin-load-table* index) value)
  value)

(defun bin-next-byte (stream)
  (read-file-word stream t))

;; The optional arg is used when the file stream doesn't support either
;; peek or bidirectionality.

(defun bin-load-next-command (stream &optional word)
  (multiple-value-bind (index extra-arg)
      (decode-bin-opcode (or word (bin-next-byte stream)))
    (let ((function (bin-op-dispatch *bin-op-load-command-table* index)))
      (if extra-arg
          (funcall function stream extra-arg)
          (funcall function stream)))))

(defun bin-next-value (stream &optional word)
  (do (val1 val2 val3) (nil)
    (multiple-value-setq (val1 val2 val3)
      (bin-next-command stream word))
    (or (eq val1 *no-value-marker*)
        (return (values val1 val2 val3)))))

;;; This is useful for interactively restoring stuff

(defvar *pause-on-load-command* t)

(defun interactive-bin-load-next-command (stream &optional word)
  (multiple-value-bind (index extra-arg)
      (decode-bin-opcode (or word (bin-next-byte stream)))
    (let ((function (bin-op-dispatch *bin-op-load-command-table* index)))
      (if (or (null *pause-on-load-command*)
              (y-or-n-p "Function is ~A. Continue to pause ?" function))
          (if extra-arg
              (funcall function stream extra-arg)
              (funcall function stream))
          (let ((*pause-on-load-command* nil))
            (format t "~%(~A)==>" function)
            (prog1
                (prin1
                 (if extra-arg
                     (funcall function stream extra-arg)
                     (funcall function stream)))
              (terpri)))))))

(defun interactive-load-binary-box-from-stream-internal (filestream)
  (with-post-load-autoloading (filestream)
    (loading-bin-file (filestream 'interactive-bin-load-next-command)
      (let ((*package* (find-package 'boxer))
            (boxer-eval::*primitive-shadow-warning-on?* nil))
        (bin-load-top-level filestream nil)))))

