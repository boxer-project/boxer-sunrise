;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-
#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



    This file contains functions and interface to the base64 and
    Quoted-Printable mail encoding functions as described in the
    MIME standard (RFC1521)


Modification History (most recent at top)

 2/14/05 #+mcl get-line mothod for net streams changed to ccl::basic-tcp-stream
         from ccl::tcp-stream
10/15/04 changed #+mcl put-line method for network streams to apply
         to ccl::basic-tcp-stream instead of ccl::tcp-stream because
         we switched to using OpenTransport streams which are between
         basic-tcp-streams and tcp-streams in the class hierarchy
 2/21/01 added #+lispworks stream methods
 2/12/01 merged into lispworks port
 9/06/00 added reading/writing-stream-position methods
12/12/98 fill-24word now hacks empty lines (usually at the end of a block of
         data before a boundary marker is encountered)
12/07/98 added specialized methods for stream-element-type
12/07/98 started logging changes version = boxer 2.3beta

|#

(in-package 'boxnet)


;;;; Low level Base64 functions

;; convert a character to a 6-bit quantity
(defun char->64 (char)
  (ecase char
    (#\A 0) (#\B 1) (#\C 2) (#\D 3) (#\E 4) (#\F 5) (#\G 6) (#\H 7) (#\I 8)
    (#\J 9) (#\K 10) (#\L 11) (#\M 12) (#\N 13) (#\O 14) (#\P 15) (#\Q 16)
    (#\R 17) (#\S 18) (#\T 19) (#\U 20) (#\V 21) (#\W 22) (#\X 23) (#\Y 24) (#\Z 25)
    (#\a 26) (#\b 27) (#\c 28) (#\d 29) (#\e 30) (#\f 31) (#\g 32) (#\h 33) (#\i 34)
    (#\j 35) (#\k 36) (#\l 37) (#\m 38) (#\n 39) (#\o 40) (#\p 41) (#\q 42)
    (#\r 43) (#\s 44) (#\t 45) (#\u 46) (#\v 47) (#\w 48) (#\x 49) (#\y 50)
    (#\z 51) (#\0 52) (#\1 53) (#\2 54) (#\3 55) (#\4 56) (#\5 57) (#\6 58) (#\7 59)
    (#\8 60) (#\9 61) (#\+ 62) (#\/ 63) (#\= nil)))

(defparameter *base64-chars*
  (make-array 64 :element-type 'character
              :initial-contents '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K
                                  #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
                                  #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f #\g
                                  #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r
                                  #\s #\t #\u #\v #\w #\x #\y #\z #\0 #\1 #\2
                                  #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/)))

;; converts a 6bit number to a base64 encoded character
(defun 64->char (6bit-number)
  (aref #+mcl       (the (array (character 64)) *base64-chars*)
        #+lispworks (the (array character) *base64-chars*)
        #-(or mcl lispworks) *base64-chars*
        6bit-number))

;; instream should be (:element-type 'character) and
;; outstream should be (:element-type (unsigned-byte 8))
(defun decode64 (instream outstream)
  (do* ((eofvalue (list 'foo))
        (line (read-line instream nil eofvalue)
              (read-line instream nil eofvalue)))
      ((eq line eofvalue))
    (let ((idx 0) (acc 0) (max (length line)) (last 0))
      (loop
        (cond ((>= idx max) (return))
              (t
               (dotimes (i 4 (incf idx 4))
                 (let ((x (char->64 (char line (+ idx i)))))
                   (if (null x) (incf last)
                       (setq acc (+ (* acc 64) x)))))
               (unless (> last 0) (write-byte (ldb (byte 8 16) acc) outstream))
               (unless (> last 1) (write-byte (ldb (byte 8 8) acc) outstream))
               (write-byte (ldb (byte 8 0) acc) outstream)
               (setq acc 0)))))))



;;; Stream interface
;;; The basic theory is that a character stream (like file or network) is
;;; associated with a base64 stream upon instantiation
;;; Lines are read/written to/from the character stream and the conversion
;;; to binary data is performed on the characters in the line buffer
;;;
;;; TO DO: doesn't hack eof issues
;;;        also should do a write/read-array methods for speed

(defclass base64-input-stream
  #+mcl (stream) ;; maybe ccl::output-stream ?
  #+lispworks (stream::fundamental-stream)  ;; maybe stream::stdobj-stream ?
  ((char-stream :initarg :char-stream)
   (line-buffer :initform (make-array 80 :element-type 'character
                                      :fill-pointer 0 :adjustable t))
   (index :initform 0)
   (byte0 :initform 0)
   (byte1 :initform 0)
   (byte2 :initform 0)
   (byte-pos :initform 0))
  )

(defclass base64-output-stream
  #+mcl (stream) ;; maybe ccl::output-stream ?
  #+lispworks (stream::fundamental-stream)  ;; maybe stream::stdobj-stream ?
  ((char-stream :initarg :char-stream)
   (line-buffer :initform (make-array 80 :element-type 'character
                                      :fill-pointer 0 :adjustable t))
   (word :initform 0)
   (byte-pos :initform 0))
  )

;; methods for reading and writing lines of characters from the character
;; streams inside of the base64 streams
;;
;; Need to specialize these methods for each implementation's class of
;; network stream

;; the default
(defmethod get-line ((stream stream)) (read-line stream nil nil))
;; net version
#+mcl
(defmethod get-line ((stream ccl::basic-tcp-stream)) (ccl::telnet-read-line stream))

#+lispworks
(defmethod get-line ((stream comm::socket-stream)) (net-read-line stream))

;; the default
(defmethod put-line ((stream stream) string) (write-line string stream))
;; net version
#+mcl
(defmethod put-line ((stream ccl::basic-tcp-stream) string)
  (ccl::telnet-write-line stream string))

#+lispworks
(defmethod put-line ((stream comm::socket-stream) string)
  (net-write-line stream string))

;;; Note that a base64 input stream being read from as a part of a MIME multipart
;;; message essentially reaches EOF when a multipart boundary is encountered
;;; multipart message functions and methods therefore bind the boundary value
;;; to enable detection of this condition (use MIME-BOUNDARY-= to check)
(defvar *mime-multipart-boundary-value* nil)
;; if a multipart boundary IS encountered, we need some way to inform
;; the calling functions of the fact...
(defvar *mime-multipart-boundary-encountered* nil)

(defmethod fill-24word ((stream base64-input-stream))
  ;; first see if we need to read in a line
  (loop
    (cond ((null (slot-value stream 'line-buffer)) ; EOF
           (return))
          ((>=& (slot-value stream 'index) (length (slot-value stream 'line-buffer)))
           (setf (slot-value stream 'line-buffer)
                 ;; get-line can return NIL if the char stream is EOF
                 (get-line (slot-value stream 'char-stream))
                 (slot-value stream 'index)
                 0))
          (t (return))))
  (let ((mime-boundary-value nil)) ; boundary comparison can return either T or :END
    (cond  ((null (slot-value stream 'line-buffer)) ; used to be 'char-stream, WHY???
            (setf (slot-value stream 'byte0) nil
                  (slot-value stream 'byte1) nil
                  (slot-value stream 'byte2) nil))
           ((and *mime-multipart-boundary-value*
                 (setq mime-boundary-value
                       (mime-boundary-= (slot-value stream 'line-buffer)
                                        *mime-multipart-boundary-value*)))
            (setq *mime-multipart-boundary-encountered* mime-boundary-value)
            (setf (slot-value stream 'byte0) nil
                  (slot-value stream 'byte1) nil
                  (slot-value stream 'byte2) nil))
           (t
            (let ((acc 0) (last 0))
              (dotimes (i 4)
                (let ((6byte (char->64 (char (slot-value stream 'line-buffer)
                                             (slot-value stream 'index)))))
                  ;; 6byte can be nil when line has been padded out with ='s
                  (when (null 6byte) (incf& last))
                  (setq acc (+& (*& acc 64) (or 6byte 0)))
                 (incf& (slot-value stream 'index))))
             (setf (slot-value stream 'byte0) (boxer::ldb& '#.(byte 8 16) acc)
                   (slot-value stream 'byte1) (unless (>& last 1)
                                                (boxer::ldb& '#.(byte 8  8) acc))
                   (slot-value stream 'byte2) (unless (>& last 0)
                                                (boxer::ldb& '#.(byte 8  0)
                                                             acc))))))))

;; input stream methods
;; NOTE:ccl::stream-read-byte on the mac is supposed to return NIL if EOF is reached
(defmethod #+mcl ccl::stream-read-byte #+lispworks stream::stream-read-byte
  ((stream base64-input-stream))
  (case (slot-value stream 'byte-pos)
    (0 ;; first we get the value of the 24 bit word, if the line
       (fill-24word stream)
       (incf&  (slot-value stream 'byte-pos)) (slot-value stream 'byte0))
    (1 (incf&  (slot-value stream 'byte-pos)) (slot-value stream 'byte1))
    (2 (setf (slot-value stream 'byte-pos) 0) (slot-value stream 'byte2))))

#+mcl
(defmethod ccl::stream-eofp ((stream base64-input-stream))
  (or (null (case (slot-value stream 'byte-pos)
              (0 (slot-value stream 'byte0))
              (1 (slot-value stream 'byte1))
              (2 (slot-value stream 'byte2))))
      (and (>=& (slot-value stream 'index)
                (length (the (array (character *)) (slot-value stream 'line-buffer))))
           ;; should we check for multipart boundary here ??
           (ccl::stream-eofp (slot-value stream 'char-stream)))))

(defmethod #+mcl ccl::stream-listen #+lispworks stream::stream-listen
  ((stream base64-input-stream))
  (not (null (case (slot-value stream 'byte-pos)
               ;; check for multipart boundary ?
               (0 (listen (slot-value stream 'char-stream)))
               (1 (slot-value stream 'byte1))
               (2 (slot-value stream 'byte2))))))

#+mcl
(defmethod ccl::stream-close ((stream base64-input-stream))
  (close (slot-value stream 'char-stream))
  (setf (slot-value stream 'index) 0
        (slot-value stream 'byte-pos) 0))

#+lispworks
(defmethod close ((stream base64-input-stream) &key abort)
  (declare (ignore abort))
  (close (slot-value stream 'char-stream))
  (setf (slot-value stream 'index) 0
        (slot-value stream 'byte-pos) 0))

(defmethod stream-element-type ((stream base64-input-stream)) '(unsigned-byte 8))

;; output stream methods...

;; from the MIME spec
(defvar *max-base64-output-line-length* 76)

;; the basic version gathers characters together until the line buffer
;; is filled than uses put-line to write it out

(defmethod #+mcl ccl::stream-write-byte #+lispworks stream::stream-write-byte
  ((stream base64-output-stream) byte)
  (with-slots (char-stream word byte-pos line-buffer) stream
    (case byte-pos
      (0 (setq word (ash byte 16)) (incf& byte-pos))
      (1 (setq word (dpb byte '#.(byte 8 8) word)) (incf& byte-pos))
      (2
       (setq byte-pos 0)
       (setq word (dpb byte '#.(byte 8 0) word))
       (decode-24-word word line-buffer)
       ;; could actually clear the WORD here but it'll get setf'd in the 0 clause
       (when (>= (fill-pointer line-buffer) *max-base64-output-line-length*)
         ;; the line is filled, so send it out, then reset all the parameters...
         (put-line char-stream line-buffer)
         (setf (fill-pointer line-buffer) 0))))))

(defun decode-24-word (word line-buffer)
  (dotimes (i 4)
    (vector-push (64->char (boxer::ldb& (byte 6 (-& 18 (*& i 6))) word))
                 line-buffer)))

;; use when binary data has ended to (possibly) pad out the end of the
;; character group with ='s and send the final line
#+mcl
(defmethod ccl::stream-finish-output ((stream base64-output-stream))
  (with-slots (char-stream line-buffer) stream
    (unless (zerop& (fill-pointer line-buffer))
      (put-line char-stream line-buffer)
      (setf (fill-pointer line-buffer) 0)
      (setf (slot-value stream 'byte-pos) 0) ; is this really neccessary ?
      ;; make sure the char-stream does what we've asked the byte stream to do
      (ccl::stream-finish-output char-stream))))

#+lispworks
(defmethod stream::stream-finish-output ((stream base64-output-stream))
  (with-slots (char-stream line-buffer) stream
    (unless (zerop& (fill-pointer line-buffer))
      (put-line char-stream line-buffer)
      (setf (fill-pointer line-buffer) 0)
      (setf (slot-value stream 'byte-pos) 0) ; is this really neccessary ?
      ;; make sure the char-stream does what we've asked the byte stream to do
      (stream::stream-finish-output char-stream))))

#+mcl
(defmethod ccl::stream-force-output ((stream base64-output-stream))
  (with-slots (char-stream line-buffer) stream
    (unless (zerop& (fill-pointer line-buffer))
      (put-line char-stream line-buffer)
      (setf (fill-pointer line-buffer) 0)
      (setf (slot-value stream 'byte-pos) 0) ; is this really neccessary ?
      ;; make sure the char-stream does what we've asked the byte stream to do
      (ccl::stream-force-output char-stream))))

#+lispworks
(defmethod stream::stream-force-output ((stream base64-output-stream))
  (with-slots (char-stream line-buffer) stream
    (unless (zerop& (fill-pointer line-buffer))
      (put-line char-stream line-buffer)
      (setf (fill-pointer line-buffer) 0)
      (setf (slot-value stream 'byte-pos) 0) ; is this really neccessary ?
      ;; make sure the char-stream does what we've asked the byte stream to do
      (stream::stream-force-output char-stream))))


;; hacks the possibility of 8 or 16 bit leftovers
(defmethod finalize-b64-data ((stream base64-output-stream))
  (with-slots (word byte-pos line-buffer) stream
    (case byte-pos
      (0); no data left in word, line-buffer is up to date
      (1 ; one byte of data in top 8 bits of word...
       (vector-push (64->char (boxer::ldb& '#.(byte 6 18) word)) line-buffer)
       (vector-push (64->char (boxer::ldb& '#.(byte 6 12) word)) line-buffer)
       (dotimes (i 2) (vector-push #\= line-buffer)))
      (2 ; two bytes of data in top 16 bits of word...
       (vector-push (64->char (boxer::ldb& '#.(byte 6 18) word)) line-buffer)
       (vector-push (64->char (boxer::ldb& '#.(byte 6 12) word)) line-buffer)
       (vector-push (64->char (boxer::ldb& '#.(byte 6 6)  word)) line-buffer)
       (vector-push #\= line-buffer)))))

;; make sure all buffered data is sent out
#+mcl
(defmethod ccl::stream-close ((stream base64-output-stream))
  (with-slots (char-stream byte-pos line-buffer) stream
    (unless (and (zerop& (fill-pointer line-buffer)) (zerop& byte-pos))
      (finalize-b64-data stream)
      (ccl::stream-finish-output stream))))

#+lispworks
(defmethod close ((stream base64-output-stream) &key abort)
  (declare (ignore abort))
  (with-slots (char-stream byte-pos line-buffer) stream
    (unless (and (zerop& (fill-pointer line-buffer)) (zerop& byte-pos))
      (finalize-b64-data stream)
      (stream::stream-finish-output stream))))

(defmethod stream-element-type ((stream base64-output-stream)) '(unsigned-byte 8))

;; should do some reality checking on the char-stream
(defmacro with-base64-stream ((stream-var char-stream &key (direction :input))
                              &body body)
  (ecase direction
    (:output
     `(let ((,stream-var (make-instance 'base64-output-stream
                               :char-stream ,char-stream)))
        (unwind-protect
          (progn . ,body)
          (close ,stream-var))))
     (:input
      `(let ((,stream-var (make-instance 'base64-input-stream
                            :char-stream ,char-stream)))
         . ,body))))

;; informational, not quite right...
(defmethod file-length ((s base64-input-stream) #-lispworks &optional #-lispworks ignore)
  #-lispworks (declare (ignore ignore))
  (length (slot-value s 'line-buffer)))

(defmethod ccl::stream-position ((s base64-input-stream) &optional ignore)
  (declare (ignore ignore))
  (slot-value s 'index))

;; what about redirecting to the char stream ?


;; instream should be (:element-type (unsigned-byte 8)) and
;; outstream should be (:element-type 'character)

(defun encode64 (instream outstream)
  (with-base64-stream (byte-stream outstream :direction :output)
    (loop (let ((byte (read-byte instream)))
            (if (null byte) (return) (write-byte byte byte-stream))))))

;; status reporting

(defmethod reading-stream-position ((stream base64-input-stream))
  (let ((cs (slot-value stream 'char-stream)))
    (unless (null cs)
      (reading-stream-position cs))))

(defmethod writing-stream-position ((stream base64-output-stream))
  (let ((cs (slot-value stream 'char-stream)))
    (unless (null cs)
      (writing-stream-position cs))))


;; testing...
#|
(defun un64 (&optional (infile (ccl::choose-file-dialog))
                       (outfile (ccl::choose-new-file-dialog)))
  (with-open-file (in infile)
    (with-open-file (out outfile :direction :output :element-type '(unsigned-byte 8))
      (decode64 in out))))

(defun ff (&optional (file (ccl::choose-file-dialog)) (bytes 20))
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (dotimes (i bytes)
      (format t "~X " (read-byte s)))))
|#
