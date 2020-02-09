;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-
#|


 $Header$

 $Log$


  Copyright 1998 - 1999 Regents of the University of California

  Enhancements and Modifications Copyright 1999 - 2003 PyxiSystems LLC



                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



    This file contains functions and interface to the binhex
    file encoding functions as described in RFC1741


Modification History (most recent at top)

 2/12/01 merged into lispworks port changes made to binhex-decode-stream,
         binhex-decode-sub and binhex-stream-reader method for comm::socket-stream
         had to comment out #+mcl set-finder-flags
12/01/98 finised conversion from the original byte based to char based reading
11/23/98 changed some constant names (by adding "binhex-") to avoid conflicts
11/20/98 crib the basic binhex decoding/encoding utilities from MCL's
         CCL:Examples:BinHex:binhex.lisp
11/18/98 Started file by copying base64.lisp


|#



#+(or lispworks mcl) (in-package :boxnet)
#-(or lispworks mcl) (in-package 'boxnet)


;;;; Low level BinHex functions (cribbed from CCL:Examples:BinHex:binhex.lisp)

; magic number for the crc calculation
(defconstant binhex-magic #.(ash #x1021 8))

; encoding translation table
(defconstant char-table
  "!\"#$%&'()*+,-012345689@ABCDEFGHIJKLMNPQRSTUVXYZ[`abcdefhijklmpqr")


; the  file creator for encoded binhex files
(defconstant binhex-file-creator :|BnHQ|)

; this value denotes a white space character in the decoding translation table
(defconstant return-code #xc0)

(defconstant colon-code (char-code #\:))

(defparameter decode-table nil)

(defparameter crc-table (make-array 256))

;;; Create the table used by the crc calculation.
(defun make-crc ()
  (dotimes (i 256)
    (setf (svref crc-table i)
          (let ((mgc (ash binhex-magic -1))
                (val (ash i 16)))
            (do ((bit 23 (1- bit)))
                ((<= bit 15))
              (when (logbitp bit val)
                (setq val (logxor val mgc)))
              (setq mgc (ash mgc -1)))
            (logand val #xffff)))))

; xor f(crc high byte) with crc low byte and new byte

(defmacro crc-byte (crc byte)
  (let ((cc (gensym))(bb (gensym)))
    `(let ((,cc ,crc)(,bb ,byte))
       (declare (type (unsigned-byte 16) ,cc)(type (unsigned-byte 8) ,bb)
                (type (simple-vector 256) crc-table))
       (logand #xffff (logxor (the fixnum (svref  crc-table (the (unsigned-byte 8)(ash ,cc -8))))
                              (logior (ash ,cc 8) ,bb))))))

(eval-when (:execute :load-toplevel)
    (setq decode-table
          (make-array 256 #|:element-type '(unsigned-byte 8)|# :initial-element #xFF))
    (dotimes (i (length char-table))
      (let ((code (char-code (schar char-table i))))
        (setf (aref decode-table code) i)))
    (dolist (c '(#\newline #\return #\linefeed #\tab #\space))
      (setf (aref decode-table  (char-code c)) return-code))
    (make-crc))

; the full header to print at the front of a binhex file
(defconstant binhex-full-header
  "(This file must be converted with BinHex 4.0)
:")

; that part of the header to check when decoding a binhex file
(defconstant binhex-short-header
  "This file must be converted with BinHex")

(defun binhex-error (fstring fargs)
  ;(error fstring fargs)
  (eval::primitive-signal-error :binhex (format nil fstring fargs))
  )

; bx-byte reads a byte from the binhex file - gets the 6 bit translation
; combines those bits with some left over from the last 6 bit translation
; and returns 8 bits for output. Note that we cannot do the
; CRC here because the byte(s) actually output may be different.

(defun bx-byte (reader readarg)
  (declare (special bits-left count last-nibble last-byte istream))
  (declare (type (unsigned-byte 8) bits-left last-nibble))
  (declare (fixnum count))
  (declare (optimize (speed 3)(safety 0)))
  (flet
    ((bx-error ()
       (error (make-condition 'file-error
                              :pathname (cond ((typep istream 'file-stream)
                                               (let ((fn #+mcl (ccl::stream-filename
                                                                istream)
                                                         #-mcl "foo"))
                                                 (or (probe-file fn) fn)))
                                              (t "Net Stream"))
                              :error-type "End of file ~S"
                              :format-arguments nil))))
    (macrolet
      ((read-byte-reader ()
         `(let ((c (funcall reader readarg)))
            (cond
             (c (locally (declare (type (unsigned-byte 8) c))
                  ;(when (eq c colon-code)(binhex-error "premature colon in ~A" istream))
                  (setq c (svref table c))
                  (when (eq c #xFF)
                    (binhex-error "~A contains an illegal character" istream))
                  (loop (when (neq c return-code)(return))
                        (setq c (svref table (funcall reader readarg))))
                  c))
             (t (bx-error)))))
       (bx-byte-sub ()
         `(let ((c1 (read-byte-reader)))
            (declare (type (unsigned-byte 8) c1))
            (case bits-left
              (0
               (setq last-nibble  (read-byte-reader))
               (setq bits-left 4)
               (logior (ash c1  2)(ash last-nibble -4)))
              (4
               (setq bits-left 2)
               (logior (logand #xf0 (ash last-nibble 4))
                       (ash (setq last-nibble c1) -2)))
              (t (setq bits-left 0)
                 (logand #xff (logior (ash last-nibble 6) c1)))))))
      (let ((table decode-table))
        (declare (type (simple-array fixnum 256) table))
        (cond ((> count 0)
               (setq count (1- count)))
              (t (let ((byte (bx-byte-sub)))
                   (cond
                    ((and (eq  byte #x90)(neq 0 (setq count (bx-byte-sub))))
                     (setq count (- count 2)))
                    (t (setq last-byte byte))))))
        last-byte))))

(defun binhex-decode-stream (istream &optional outfile (infile istream))
  (declare (special istream))
  (declare (optimize (speed 3)(safety 0)))
  (let ((bits-left 0)(last-nibble 0)(count 0) last-byte)
    (declare (special bits-left count last-nibble last-byte))
    (declare (type (unsigned-byte 8) bits-left last-nibble))
    (declare (fixnum count))
    (multiple-value-bind (reader readarg)(binhex-stream-reader istream)
        (macrolet
          ((bx-long ()
             `(let ((c1 (bx-byte reader readarg))(c2 (bx-byte reader readarg))
                    (c3 (bx-byte reader readarg))(c4 (bx-byte reader readarg)))
                (setq crc (crc-byte (crc-byte (crc-byte (crc-byte crc c1) c2) c3) c4))
                (logior
                 (ash c1 24)
                 (ash c2 16)
                 (ash c3 8)
                 c4))))
          (let ((c 0))
            (declare (fixnum c))
            (when (not (find-binhex-header istream))
              (binhex-error "~A does not have a binhex header" infile))
            ; skip to return
            (loop
              (setq c (byte-from-char-stream istream))
              (unless (eq c (char-code #\space))
                (when (eq (aref decode-table c) return-code)
                  (return))))
            ; skip returns til colon
            (loop
              (setq c (byte-from-char-stream istream))
              (when (eq c colon-code) (return))
              (when (neq (aref decode-table c) return-code)
                (binhex-error "Bad stuff in text header of ~A" infile)))
            ; time to read the header describing the contents
            (let* ((namelength (bx-byte reader readarg))
                   (name (make-string namelength))
                   (type (make-string 4))
                   (creator (make-string 4))
                   (crc 0)
                   flags dlen rlen hdr-crc)
              ; get the filename - will be the default for the dialog
              (setq crc (crc-byte 0 namelength))
              (dotimes (i namelength)
                (declare (fixnum i))
                (let ((c (bx-byte reader readarg)))
                  (setq crc (crc-byte crc c))
                  (setf (aref name i)(code-char c))))
              ; skip a 0 byte
              (when (neq 0 (bx-byte reader readarg))(binhex-error "Error reading file name in header of ~A" infile))
              (setq crc (crc-byte crc 0))
              #+mcl
              (when (null outfile)
                (setq outfile
                      (ccl:catch-cancel (ccl:choose-new-file-dialog :directory name)))
                (when (eq outfile :cancel)(return-from binhex-decode-stream nil)))
              #+lispworks
              (when (null outfile)
                (multiple-value-bind (path success?)
                    (capi:prompt-for-file "File for binhex data"
                                          :filter bw::*boxer-file-filters*
                                          :pathname name
                                          :operation :open
                                          :owner bw::*boxer-frame*)
                  (if (null success?)
                      (return-from binhex-decode-stream nil)
                    (setq outfile path))))
              #-(or mcl lispworks)
              (progn (warn "prompting for binhex data not defined for ~A"
                           (lisp-implementation-type))
                (return-from binhex-decode-stream nil))
                  ;;
              (bw::with-mouse-cursor (:wait)  ; have to do this after the modal dialog
                ; get mac type and creator
                (dotimes (i 4)
                  (declare (fixnum i))
                  (let ((c (bx-byte reader readarg)))
                    (setq crc (crc-byte crc c))
                    (setf (aref type i) (code-char c))))
                (setq type (intern type (find-package :keyword)))
                (dotimes (i 4)
                  (declare (fixnum i))
                  (let ((c (bx-byte reader readarg)))
                    (setq crc (crc-byte crc c))
                    (setf (aref creator i)(code-char c))))
                (setq creator (intern creator (find-package :keyword)))
                ; finder flags
                (let ((c (bx-byte reader readarg)) (c2 (bx-byte reader readarg)))
                  (setq crc (crc-byte (crc-byte crc c) c2))
                  (setq flags (logior (ash c 8) c2)))
                ; lengths of data and resource forks
                (setq dlen (bx-long))
                (setq rlen (bx-long))
                (setq crc (crc-byte (crc-byte crc 0) 0))
                (setq hdr-crc (logior (ash (bx-byte reader readarg) 8)(bx-byte reader readarg)))
                (when (neq crc hdr-crc)
                  (binhex-error "crc failure in header of ~A" infile))
                (binhex-decode-sub outfile reader readarg type creator dlen :data)
                (binhex-decode-sub outfile reader readarg type creator rlen :resource)
                (set-finder-flags outfile
                                  (logand flags
                                          (lognot (+  (ash 1 8) ;#$fInitted  - where is he
                                                      1      ; #$fOnDesk
                                                      #x4000 ; #$fInvisible
                                                      ))))
                outfile)))))))

(defun find-binhex-header (s)
  (let ((eof-value (list 'eof))
        (hlength (length binhex-short-header))
        (1st (schar binhex-short-header 0)))
    (declare (fixnum hlength))
    (declare (optimize (speed 3) (safety 0)))
    (catch 'header-match
      (loop (let* ((char (read-char s nil eof-value)))
              (cond ((eq char eof-value) (return nil))
                    ((char= char 1st)
                     (dotimes (i (1- hlength) (throw 'header-match t))
                       (declare (fixnum i))
                       (let ((stream-char (peek-char nil s nil eof-value)))
                         (cond ((char= (schar binhex-short-header (1+ i)) stream-char)
                                ;; if it looks good, take it out
                                (read-char s))
                               (t (return nil))))))
                    (t nil)))))))

#| ; original code assumed stream-position works, which isn't true for tcp-streams
(defun find-binhex-header (s)
  (let ((hlength (length binhex-short-header))
         (flength (file-length s))
         (pos))
    (declare (fixnum hlength flength))
    (declare (optimize (speed 3)(safety 0)))
    (dotimes (i (- flength hlength) nil)
      (declare (fixnum i))
      (let ((c (code-char (read-byte s))))
        (when (eq c (schar binhex-short-header 0))
          (setq pos (ccl::stream-position s))
          (when (dotimes (i (1- hlength) t)
                  (declare (fixnum i))
                  (when (neq (schar binhex-short-header (1+ i)) (code-char (read-byte s)))
                    (return nil)))
            (return-from find-binhex-header t))
          (ccl::stream-position s  pos))))))
|#

; decode the resource or data fork section of the binhex data file

(defun binhex-decode-sub (outfile reader readarg type creator dlen fork)
  (declare (optimize (speed 3)(safety 0)))
  (declare (special istream))
  #+mcl
  (with-open-file (ostream outfile :direction :output
                           :if-exists (if (eq fork :data) :supersede :overwrite)
                           :external-format type
                           :mac-file-creator creator
                           :fork fork
                           :element-type '(unsigned-byte 8))
    (multiple-value-bind (writer writearg)(ccl::stream-writer ostream)
      (let ((crc 0))
        (do ((i dlen (1- i)))
            ((<= i 0))
          ; does the length include the crc? assume not
          (let ((byte (bx-byte reader readarg)))
            (funcall writer writearg byte)
            (setq crc (crc-byte crc byte))))
        ; account for 2 crc bytes as if zero
        (setq crc (crc-byte crc 0))(setq crc (crc-byte crc 0))
        (when (not (and (eq  (logand #xFF (ash crc -8))(bx-byte reader readarg))
                        (eq (logand #xff crc)(bx-byte reader readarg))))
          (binhex-error "crc failure in ~A" istream)))))
  #-mcl
  ;; only write data forks
  (if (eq fork :data)
      (with-open-file (ostream outfile :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
        (let ((crc 0))
          (do ((i dlen (1- i)))
              ((<= i 0))
          ; does the length include the crc? assume not
            (let ((byte (bx-byte reader readarg)))
              ; assume we are writing out to a file here
              (write-byte byte ostream)
              (setq crc (crc-byte crc byte))))
        ; account for 2 crc bytes as if zero
          (setq crc (crc-byte crc 0))(setq crc (crc-byte crc 0))
          (when (not (and (eq  (logand #xFF (ash crc -8))(bx-byte reader readarg))
                          (eq (logand #xff crc)(bx-byte reader readarg))))
            (binhex-error "crc failure in ~A" istream))))
    ;; empty out the bytes but don't write them anywhere for resource forks
    (let ((crc 0))
      (dotimes (i dlen)
        (setq crc (crc-byte crc (bx-byte reader readarg))))
       ; account for 2 crc bytes as if zero
      (setq crc (crc-byte crc 0))(setq crc (crc-byte crc 0))
      (when (not (and (eq  (logand #xFF (ash crc -8))(bx-byte reader readarg))
                      (eq (logand #xff crc)(bx-byte reader readarg))))
        (binhex-error "crc failure in ~A" istream))))
  )


;; finder interface
#| ;;comment out for lispworks because #_HSetFInfo blows out reader
#+mcl
(defun set-finder-flags (file flags)
  (ccl::%stack-iopb (pb np)
    (ccl::%path-to-iopb file pb :errchk)
    (setf (ccl:pref pb hparamblockrec.ioFlFndrInfo.fdFlags) flags)
    (ccl::file-errchk (#_HSetFInfo pb) file)))
|#

#-mcl
(defun set-finder-flags (file flags)
  (declare (ignore file flags))
  nil)

#+mcl
(defun get-finder-flags (file)
  (ccl::%stack-iopb (pb np)
    (ccl::%Path-to-iopb file pb :errchk)
    (ccl:pref pb hparamblockrec.ioFlFndrInfo.fdFlags)))

#-mcl
(defun get-finder-flags (file)
  0)


(defun decode-binhex (instream outstream)
  )

;; instream should be (:element-type (unsigned-byte 8)) and
;; outstream should be (:element-type 'character)



;; stream-readers
(defun byte-from-char-stream (s) (char-code (read-char s)))

(defmethod binhex-stream-reader ((stream file-stream))
  (cond ((subtypep (stream-element-type stream) 'character)
         (values #'byte-from-char-stream stream))
        (t (values #'read-byte stream))))

#+mcl
(defmethod binhex-stream-reader ((stream ccl::tcp-stream))
  (cond ((subtypep (stream-element-type stream) 'character)
         (values #'byte-from-char-stream stream))
        (t (values #'read-byte stream))))

#+lispworks
(defmethod binhex-stream-reader ((stream comm::socket-stream))
  (cond ((subtypep (stream-element-type stream) 'character)
         (values #'byte-from-char-stream stream))
        (t (values #'read-byte stream))))


#|
(ccl::method ccl::stream-read-byte (ccl::fred-input-stream))


;; what about redirectiing to the char stream ?
|#

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
