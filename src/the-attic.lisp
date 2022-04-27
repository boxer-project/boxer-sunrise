;;;;
;;;; The attic
;;;;
;;;; Interesting bits:
;;;;   - Boxer Send Server: boxnet.lisp and net-prims.lisp
;;;;     These contain the legendary code for sending boxes between computers based on
;;;;     a socket implementation on Sun boxes using Lucid.
;;;;     net-prims has an interesting defun `safe-load-binary-box-from-stream-internal`
;;;;     to consider in the future as a reference for sending boxes around.
;;;;   - bu::configuration-info is a nice example of creating a box of stuff programmatically
;;;;     with lists
;;;;   - sysprims.lisp contains a number of unused preferences, some of which we *may* want
;;;;     to bring back in the future, such as some options for the serial line interface
;;;;   - compile-lambda-if-possible in evalmacs.lisp is an interesting usage of performing
;;;;     optimizations to particular lambda optimizations


;;;;
;;;; FILE: applefile.lisp
;;;;
;;;; --entire-file--

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



    This file contains functions and interface for the applesingle and
    appledouble file encodings as described in RFC1740


Modification History (most recent at top)

12/30/11 get-mime-applefile-box, get-mime-appledouble changed calls to xxx-mac-file-ref-xxx to xxx-xref-xxx
 1/29/01 conditionalize resource fork handler for #+/-mcl in initial lispworks
         version
 6/04/99 added realname (filename) dumping to send-mime-applesingle-data-internal
         because some base64 decoders need it to work.
 1/25/99 fixed fork dumping in send-mime-applesingle-data-internal also
         added more debugging statements
 1/23/99 send-mime-applesingle-data-internal had the order for filler and
         entry counts reversed
11/30/98 Started file


|#

(in-package :boxnet)


(defconstant *applesingle-magic-number* #x00051600)
(defconstant *appledouble-magic-number* #x00051607)

(defvar *applefile-supported-version* #x00020000)

(defvar *applefile-strict-error-checking* T)

(defvar *applefile-debug* nil)

;; should be (member ':applesingle :appledouble :binhex)
;; :binhex is not currently supported
(defvar *mac-mime-file-encoding* :appledouble)


;; utilities, (these must be defined somewhere else already)
(defun read-32bit (stream)
  (logior (ash (read-byte stream) 24)
          (ash (read-byte stream) 16)
          (ash (read-byte stream) 8)
          (read-byte stream)))

(defun read-16bit (stream)
  (logior (boxer::ash& (read-byte stream) 8) (read-byte stream)))

(defun write-32bit (word stream)
  (write-byte (ldb '#.(byte 8 24) word) stream)
  (write-byte (ldb '#.(byte 8 16) word) stream)
  (write-byte (ldb '#.(byte 8 8)  word) stream)
  (write-byte (ldb '#.(byte 8 0)  word) stream))

(defun write-16bit (short stream)
  (write-byte (boxer::ldb& '#.(byte 8 8) short) stream) ; high
  (write-byte (boxer::ldb& '#.(byte 8 0) short) stream)) ; low

;; If we want error checking, make sure these are all zeros
(defun read-applefile-filler (stream)
  (dotimes (i 16)
    (let ((byte (read-byte stream)))
      (unless (zerop byte)
        (when *applefile-strict-error-checking*
          (error "Unexpected value, #X~X, in applefile filler" byte))))))

;; stream args should be byte streams, this may mean using base64 streams
;; to convert from character streams

;;    AppleSingle file header:
;;
;;   Field               Length
;;
;;   Magic number         4 bytes
;;   Version number       4 bytes
;;   Filler              16 bytes
;;   Number of entries    2 bytes
;;
;;   Byte ordering in the file fields follows MC68000 conventions, most
;;   significant byte first.  The fields in the header file follow the
;;   conventions described in the following sections.
;;
;;   Magic number
;;      This field, modelled after the UNIX magic number feature,
;;      specifies the file's format.  Apple has defined the magic number
;;      for the AppleSingle format as $00051600 or 0x00051600.
;;
;;   Version number
;;      This field denotes the version of AppleSingle format in the event
;;      the format evolves (more fields may be added to the header).  The
;;      version described in this note is version $00020000 or
;;      0x00020000.
;;
;;   Filler
;;      This field is all zeros ($00 or 0x00).
;;
;;   Number of entries
;;      This field specifies how many different entries are included in
;;      the file.  It is an unsigned 16-bit number.  If the number of
;;      entries is any number other than 0, then that number of entry
;;      descriptors immediately follows the number of entries field.

;; returns (values number-of-entries appledouble? version)

(defun applefile-header-values (stream)
  (let ((magic   (read-32bit stream))
        (version (read-32bit stream))
        (double? nil))
    (cond ((= magic *applesingle-magic-number*))
          ((= magic *appledouble-magic-number*) (setq double? T))
          (t (error "Invalid Applefile (magic = #x~X" magic)))
    (unless (= *applefile-supported-version* version)
      (if *applefile-strict-error-checking*
          (error "#x~X is an unsupported applefile version" version)
          (warn  "#x~X is an unsupported applefile version" version)))
    (read-applefile-filler stream)
    (values (read-16bit stream) double? version)))


;;    Entry descriptor for each entry:
;;
;;   Entry ID             4 bytes
;;   Offset               4 bytes
;;   Length               4 bytes
;;
;;      The entry descriptor is made up of the following three fields:
;;
;;      Entry ID:   an unsigned 32-bit number, defines what the entry is.
;;                  Entry IDs range from 1 to $FFFFFFFF. Entry ID 0 is
;;                  invalid.
;;      Offset:     an unsigned 32-bit number, shows the offset from the
;;                  beginning of the file to the beginning of the entry's
;;                  data.
;;      Length:     an unsigned 32-bit number, shows the length of the
;;                  data in bytes.  The length can be 0.

;; returns (values entry-ID offset length)

(defun applefile-entry-values (stream)
  (values (read-32bit stream) (read-32bit stream) (read-32bit stream)))


;; Entry ID's
;; from RFC1740
;;   Predefined entry ID's
;;
;;      Apple has defined a set of entry IDs and their values as follows:
;;
;;      Data Fork              1 Data fork
;;      Resource Fork          2 Resource fork
;;      Real Name              3 File's name as created on home file
;;                               system
;;      Comment                4 Standard Macintosh comment
;;      Icon, B&W              5 Standard Macintosh black and white icon
;;      Icon, Colour           6 Macintosh colour icon
;;      File Dates Info        8 File creation date, modification date,
;;                               and so on
;;      Finder Info            9 Standard Macintosh Finder information
;;      Macintosh File Info   10 Macintosh file information, attributes
;;                               and so on
;;      ProDOS File Info      11 ProDOS file information, attributes and
;;                               so on
;;      MS-DOS File Info      12 MS-DOS file information, attributes and
;;                               so on
;;      Short Name            13 AFP short name
;;      AFP File Info         14 AFP file, information, attributes and so
;;                               on
;;      Directory ID          15 AFP directory ID
;;
;;      Apple reserves the range of entry IDs from 1 to $7FFFFFFF. The
;;      rest of the range is available for applications to define their
;;      own entries.  Apple does not arbitrate the use of the rest of the
;;      range.

(defvar *applefile-entry-names*
  (make-array 16 :initial-contents '(:unused :data :resource :filename :comment
                                     :bwicon :coloricon :unused :filedates :finderinfo
                                     :fileinfo :prodos :msdos :afpname :afpinfo
                                     :directoryid)))

(defun applefile-entry-name (entry-id)
  (if (<= 0 entry-id (length *applefile-entry-names*) )
      (svref *applefile-entry-names* entry-id)
      :unknown))

(defstruct (applefile-entry (:print-function print-applefile-entry))
  (id 1)
  (offset 0)
  (length 0))

(defun print-applefile-entry (entry stream depth)
  (declare (ignore depth))
  (format stream "#<~A Applefile Entry ~D ~D>"
          (applefile-entry-name (applefile-entry-id entry))
          (applefile-entry-offset entry) (applefile-entry-length entry)))

;; does error checking and returns a (possibly NIL) list of entries
(defun parse-applefile-header (stream)
  (multiple-value-bind (number-of-entries appledouble? version)
      (applefile-header-values stream)
    (when *applefile-debug*
      (format t "~&~A file, version #x~X with ~D entries"
              (if appledouble? "Appledouble" "Applesingle")
              version number-of-entries))
    (let ((entries nil))
      (dotimes (i number-of-entries (nreverse entries))
        (multiple-value-bind (id offset length) (applefile-entry-values stream)
          (push (make-applefile-entry :id id :offset offset :length length)
                entries))))))


;; decoding entry handlers

;; handlers expect a byte stream arg, filename, length and &optional offset
;; if no offset is supplied, we can assume that the stream is already correctly
;; positioned and we can just loop for <length> bytes
;; filename must be supplied by the calling function so all the entries will
;; be synchronized to the same file
;; ??? How to use filename entry info ???

(defvar *applefile-entry-handlers*
  (make-array 16 :initial-element 'applefile-ignore-entry))

(defmacro def-applefile-handler ((name idx) arglist &body body)
  (let ((handler-name (intern (format nil "APPLEFILE-~A-ENTRY-HANDLER" name))))
    `(progn
       (defun ,handler-name ,arglist
         ,@body)
       (setf (aref *applefile-entry-handlers* ,idx) ',handler-name)
       ',handler-name)))

(defun handle-applefile-entry (entry stream pathname)
  (let* ((entry-id (applefile-entry-id entry))
         (handler (if (<= 0 entry-id (1- (length *applefile-entry-handlers*)))
                      (svref *applefile-entry-handlers* entry-id)
                      'applefile-ignore-entry)))
    (when *applefile-debug* (format t "~&Applefile handler: ~A" handler))
    (cond ((typep stream 'file-stream)
           (funcall handler stream pathname (applefile-entry-length entry)
                    (applefile-entry-offset entry)))
          (t
           (funcall handler stream pathname (applefile-entry-length entry))))))

;; generic, used to empty out bytes for entries which we want to ignore
(defun applefile-ignore-entry (stream filename length &optional offset)
  (declare (ignore filename))
  (cond ((null offset)
         (dotimes (i length) (read-byte stream)))
        ((typep stream 'file-stream)
         ;; really, we don't have to do anything here
         ;; but we'll move the pointer to the next blobk as a convenience
         (file-position stream (+ offset length)))
        (t (error "Offset provided for a non file stream, ~A" stream))))


;; 0 unused
(def-applefile-handler (data 1) (stream filename length &optional offset)
  (cond ((null offset)
         ;; netstream, assume we are in the right place, checking
         ;; should happen at a higher level
         )
        ((typep stream 'file-stream)
         ;; move to the right place if we can
         (file-position stream offset))
        (t (error "Offset provided for a non file stream, ~A" stream)))
  ;; we are in the right place now, so do the work
  (with-open-file (outstream filename :direction :output
                             :element-type '(unsigned-byte 8)
                             #+mcl :fork #+mcl :data
                             :if-exists :supersede :if-does-not-exist :create)
    (dotimes (i length) (write-byte (read-byte stream) outstream))))

;; resource forks are only useful for macs
#+mcl
(def-applefile-handler (resource 2) (stream filename length &optional offset)
  (cond ((null offset)
         ;; netstream, assume we are in the right place, checking
         ;; should happen at a higher level
         )
        ((typep stream 'file-stream)
         ;; move to the right place if we can
         (file-position stream offset))
        (t (error "Offset provided for a non file stream, ~A" stream)))
  ;; we are in the right place now, so do the work (for macs...)
  #+mcl
  (with-open-file (outstream filename :direction :output
                             :element-type '(unsigned-byte 8)
                             :fork :resource :if-exists :overwrite
                             :if-does-not-exist :create)
    (when *applefile-debug* (format t "~&Reading Resource bytes: "))
    (dotimes (i length)
      (when (and *applefile-debug* (zerop (mod i 10))) (format t "~D.." i))
      (write-byte (read-byte stream) outstream)))
  #-mcl ;; all others, just empty out the bytes from the stream
  (dotimes (i length) (read-byte stream))
  )


;; need to figure out how to pass this info to caller and then decide whether
;; to do a rename file or not
;(def-applefile-handler (filename 3) (stream fielname length &optional offset)
;  )

;; (comment 4) ignore for now
;; (bwicon  5) ignore for now
;; (coloricon 6) ignore for now
;; 7 is unused
;; (filedates 8) ignore for now, useful for general case but not for attachments

;; important, we get file type and creator out of this one
;; they will be the 1st 2 groups of 4 bytes
;; the full def from Inside Mac: "Macintosh Toolbox Essentials" pg 7-76 is:
;;
;; struct FInfo {
;;        OSType         fdType;      /* file type */                    4 bytes
;;        OSType         fdCreator;   /* file creator */                 4 bytes
;;        unsigned short fdFlags;     /* Finder flags */                 2 bytes
;;        Point          fdLocation;  /* file's location in window */    4 bytes
;;        short           fdFldr;     /* directory that contains file */ 2 bytes
;; };                                                                   16 bytes
;;
;; struct FXInfo {
;;        short  fdIconID;    /* Icond ID */               2 bytes
;;        short  fdUnused[3]; /* reserve 6 bytes */        6 bytes
;;        char   fdScript;    /* script flag and code */   1 byte
;;        char   fdXFlags;    /* reserved */               1 byte
;;        short  fdComment;   /* COmment ID */             2 bytes
;;        long   fdPutAway;   /* home directory ID */      4 bytes
;; };                                                     16 bytes
;;

(def-applefile-handler (finderinfo 9) (stream filename length &optional offset)
  (declare (ignore filename))
  (declare (special mac-creator mac-type))
  (let ((tstring "1234") (cstring "1234"))
    (cond ((null offset)
         ;; netstream, assume we are in the right place, checking
         ;; should happen at a higher level
         )
        ((typep stream 'file-stream)
         ;; move to the right place if we can
         (file-position stream offset))
        (t (error "Offset provided for a non file stream, ~A" stream)))
    (unless (< length 8)
      (dotimes (i 4) (setf (schar tstring i) (code-char (read-byte stream))))
      (dotimes (i 4) (setf (schar cstring i) (code-char (read-byte stream)))))
    ;; empty out the rest of the entry
    (dotimes (i (- length 8)) (read-byte stream))
    (setq mac-type    (intern tstring (find-package "KEYWORD"))
          mac-creator (intern cstring (find-package "KEYWORD")))))

;; (fileinfo 10) ignore for now
;; (prdos    11) ignore for now
;; (msdos    12) ignore for now
;; (afpname  13) ignore for now
;; (afpinfo  14) ignore for now
;; (directoryid 15) ignore for now




;; Top level interface (called from append-pop-message-body in mail.lisp)
(defun get-mime-appledouble (message stream boundary mime-values bytes-read)
  (let ((name (getf mime-values :name))
        (*mime-multipart-boundary-value* boundary)
        (*mime-multipart-boundary-encountered* nil)
        (apathname nil))
    (loop (let* ((line (unless *mime-multipart-boundary-encountered*
                         (net-mail-read-line stream)))
                 (boundary? (or *mime-multipart-boundary-encountered*
                                (mime-boundary-= line boundary))))
            ;; looking for 1st boundary
            (cond ((eq boundary? :end) ;; empty out the rest of the epilogue
                  ; (empty-out-mail-message stream)
                   (return))
                  (boundary? ;; we hit a boundary, call someone else to
                   (setq *mime-multipart-boundary-encountered* nil)
                   (multiple-value-bind (header-box amime-type amime-values
                                                    header-size)
                       (make-header-box-from-stream stream)
                     ;; the 1st part should be application/applefile
                     ;; the 2nd part should be added to the file created
                     ;; in the 1st part
                     (cond ((eq amime-type :application/applefile)
                            (get-mime-applefile-box
                             header-box stream bytes-read
                             (or name (getf amime-values :name))
                             (getf amime-values :encoding))
                            (shrink header-box)
                            (setq apathname
                                  (boxer::xref-pathname
                                   (getprop header-box :xref)))
                            (append-row message (make-row (list header-box))))
                           (t ;; in theory, we are supposed to append a data fork
                            ;; after the other stuff has been created
                            (cond ((null apathname) ; no file created
                                   ;; fill in the usual way
                                   (append-pop-message-body header-box stream
                                                            amime-type
                                                            amime-values
                                                            header-size)
                                   ;; and append the sub box
                                   (append-row message
                                               (make-row (list header-box))))
                                  (t ;; we just want to add to the data fork
                                   ;; of the already created file...
                                   (get-mime64-binary-data message stream
                                                           bytes-read
                                                           apathname))))))))))))


;; in practice, it seems that the entries are already sorted...
;; make sure the finder info comes before either resource or data and
;; that each entry comes one after the other with no gaps, If so, then
;; it will be possible to decode on the fly (important for a net stream)
;; If this isn't the case, we'll have to save bytes into a scratch file
(defun verify-applefile-entry-order (entries)
  (let ((last-offset (applefile-entry-offset (car entries)))
        (found-finderinfo? nil))
    (dolist (entry entries T)
      (cond ((= (applefile-entry-offset entry) last-offset)
             (incf last-offset (applefile-entry-length entry)))
            (t (return nil)))
      (let ((current-id (applefile-entry-id entry)))
        (when (= current-id 9) (setq found-finderinfo? t))
        (when (and (or (= current-id 1) (= current-id 2)) ; data or resource
                   (not found-finderinfo?))
          (return nil))))))

(defun make-stub-file (filename)
  (with-open-file (s filename :direction :output :if-does-not-exist :create)
    s))

;; converts a char stream (like a mail message stream) to a byte stream
;; stub for now...
(defmacro with-char-byte-stream ((byte-stream-var char-stream) &body body)
  `(let ((,byte-stream-var ,char-stream))
     ,@body))

(defun get-mime-applefile-box (message stream bytes-read name encoding)
  (declare (ignore bytes-read))
  (let* ((pathname (unique-mime-path name))
         (xref (boxer::make-xref :pathname pathname)))
    ;; now associate the file with the message box
    (boxer::add-xref-closet-boxes message)
    (putprop message xref :xref)
    ;(shrink message)
    (multiple-value-bind (path mac-creator mac-type)
        (if (eq encoding :base64)
          (with-base64-stream (byte-stream stream)
            (get-mime-applefile byte-stream pathname))
          (with-char-byte-stream (byte-stream stream)
            (get-mime-applefile byte-stream pathname)))
      (declare (ignore path))
      (boxer::set-xref-boxtop-info message mac-creator mac-type))
    message))

;; this expects a byte stream
(defun get-mime-applefile (stream pathname)
  (let* ((entries (parse-applefile-header stream))
         (mac-creator :BOXR)
         (mac-type    :TEXT))
    (declare (special mac-creator mac-type))
    (unless (verify-applefile-entry-order entries)
      ;; should branch here and use binary scratch file implementation
      (when *applefile-strict-error-checking*
        (error "Out of order apple file entries")))
    (dolist (entry entries) (handle-applefile-entry entry stream pathname))
    ;;at this point, a file should have been created
    ;; if this is the 1st part of an appledouble file, there won't
    ;; be a data fork, if there was no resource fork in the original
    ;; file, then there may not even be a file yet
    (when (null (probe-file pathname)) (make-stub-file pathname))
    ;; now we are sure there is a file...
    #+mcl
    (progn
      (ccl::set-mac-file-creator pathname mac-creator)
      (ccl::set-mac-file-type    pathname mac-type))
    (values pathname mac-creator mac-type)))




;;; Sending

(defvar *default-applefile-type* :text)
(defvar *default-applefile-creator* :????)

;; this is for appledouble attachments inside of multipart messages
;; so it has a somewhat abbreviated header
(defun send-mime-appledouble-data (box file mailstream)
  (let ((bv (mime-separator-value)))
    ;; first, a simple header,
    (net-write-smtp-data-line
     mailstream
     (format nil "Content-Type: multipart/appledouble;boundary=\"~A\"; name=\"~A~A\""
             bv (pathname-name file)
             (let ((type (pathname-type file)))
               (if (stringp type) (format nil ".~A" type) ""))))
    (net-write-smtp-data-line mailstream "Content-Transfer-Encoding: 7bit")
    (net-write-smtp-data-line mailstream
                              (format nil "Content-Description: ~A"
                                      (namestring file)))
    (net-terpri mailstream)
    ;; then the data
    (send-appledouble-box-data box mailstream bv file)))

;; very similiar to send-multipart-box-data
(defun send-appledouble-box-data (box smtp-stream bv pathname)
  ;; in theory, box can be used to get an xref with a pathname which should
  ;; match with the supplied pathname...
  (declare (ignore box))
  ;; skip the prologue message cause we may come here recursively as part
  ;; of a MIME multipart (if we are attaching to a non null message
  (write-mime-boundary smtp-stream bv)
  ;; now the resource and finderinfo as an applesingle
  (send-mime-applesingle-data pathname smtp-stream T)
  (write-mime-boundary smtp-stream bv)
  ;; now the data fork
  (multiple-value-bind (type subtype)
     (mac-file->mime-values pathname)
    (send-mime-binary-data type subtype pathname smtp-stream))
  (write-mime-boundary smtp-stream bv T))

;; called from mail
;; this writes out the header, then uses send-applesingle-box-data
(defun send-mime-applesingle-data (file mailstream &optional double?)
  (net-write-smtp-data-line mailstream
                            (format nil "Content-type: application/applefile ; name=\"~A~A\""
                                    (pathname-name file)
                                    (let ((type (pathname-type file)))
                                      (if (stringp type)
                                        (format nil ".~A" type)
                                        ""))))
  (net-write-smtp-data-line mailstream "Content-Transfer-Encoding: base64")
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Description: ~A" (namestring file)))
  (net-terpri mailstream)
  (send-mime-applesingle-data-internal file mailstream double?))


;;
(defun send-mime-applesingle-data-internal (pathname smtp-stream double?)
  ;; 1st figure out sizes...
  (let* ((rsize 0) (dsize 0)
         (ftype *default-applefile-type*)
         (creator *default-applefile-creator*)
         (filename (file-namestring pathname))
         (flength (length filename))
        ;; where the data starts, = magic(4) + version(4) + # entries (2) +
        ;; filler(16) + 12/entry (3 entries if double? otherwise 4)
        (header-offset (if double? 62 74)))
    (when (probe-file pathname)
      #+mcl
      (setq rsize (with-open-file (rs pathname :fork :resource
                                      :element-type '(unsigned-byte 8))
                    (file-length rs)))
      (setq dsize (with-open-file (ds pathname #+mcl :fork #+mcl :data
                                      :element-type '(unsigned-byte 8))
                    (file-length ds)))
      #+mcl
      (setq ftype (ccl::mac-file-type pathname)
            creator (ccl::mac-file-creator pathname))
      ;; now we can assemble the applefile entries
      ;; we only use filename, resource, data and finderinfo entries
      (with-base64-stream (byte-stream smtp-stream :direction :output)
        ;; the prologue
        (write-32bit
         (if double? *appledouble-magic-number* *applesingle-magic-number*)
         byte-stream)
        (write-32bit *applefile-supported-version* byte-stream)
        (write-applefile-filler byte-stream)
        (write-16bit (if double? 3 4) byte-stream)
        ;; now the entry descriptors (ID, offset, length) 4 bytes each
        ;; 1st the filename
        (progn (write-32bit 3 byte-stream) ; real name (filename)
               (write-32bit header-offset byte-stream)
               (write-32bit flength byte-stream))
        ;; now the finderinfo
        (progn
          (write-32bit 9 byte-stream)  ; FInderinfo Entry ID
          (write-32bit (+ header-offset flength) byte-stream)
          (write-32bit 32 byte-stream))
        ;; now the resource fork
        (progn
          (write-32bit 2 byte-stream) ; Resource Entry ID
          (write-32bit (+ header-offset flength 32) byte-stream)
          (write-32bit rsize byte-stream))
        (unless double?
          (write-32bit 1 byte-stream)  ; Data Entry ID
          (write-32bit (+ header-offset flength 32 rsize) byte-stream)
          (write-32bit dsize byte-stream))
        ;; now the actual entries, 1st the filename
        (dotimes (i flength) (write-byte (char-code (char filename i))
                                         byte-stream))
        ;; we synthesize a finderinfo entry
        (write-applefile-finderinfo ftype creator byte-stream)
        ;; the resource fork...
        (unless (zerop rsize)
          (with-open-file (rs pathname :fork :resource
                              :element-type '(unsigned-byte 8))
            (loop (let ((byte (read-byte rs nil nil)))
                    (cond ((null byte) (return))
                          (t (write-byte byte byte-stream)))))))
        ;; the data fork
        (unless (or double? (zerop dsize))
          (with-open-file (ds pathname :fork :data
                              :element-type '(unsigned-byte 8))
            (loop (let ((byte (read-byte ds nil nil)))
                    (cond ((null byte) (return))
                          (t (write-byte byte byte-stream)))))))))))

(defun write-applefile-filler (stream)
  (dotimes (i 16) (write-byte 0 stream)))

(defun write-applefile-finderinfo (type creator stream)
  ;; write out the type and creator
  (let ((typestring (symbol-name type)))
    (dotimes (i 4) (write-byte (char-code (char typestring i)) stream)))
  (let ((creatstring (symbol-name creator)))
    (dotimes (i 4) (write-byte (char-code (char creatstring i)) stream)))
  ;; pad out 8 bytes for the rest of the FInfo
  (dotimes (i 8) (write-byte 0 stream))
  ;; pad out another 16 bytes for the entire FXInfo
  (dotimes (i 16) (write-byte 0 stream)))



;;;;
;;;; FILE: base64.lisp
;;;;
;;;; --entire-file--

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


;;;;
;;;; FILE: binhex.lisp
;;;;
;;;; --entire-file--

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

(in-package :boxnet)


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
  (boxer-eval::primitive-signal-error :binhex (format nil fstring fargs))
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



;;;;
;;;; FILE: boxdef.lisp
;;;;

(defun boxer::valid-boxer-license? () t)

;;
;; this could be a lot faster, we should be able to do a compile time
;; check to see if TYPE is a class and then put in the appropriate code instead
;; of the OR which is there now.
(defmacro fast-iwmc-class-p (thing)
  (warn "You need to define a version of FAST-IWMC-CLASS-P for ~A of ~A"
        (lisp-implementation-version) (lisp-implementation-type))
  `(typep ,thing 'structure))

(DEFVAR *COMPLETE-REDISPLAY-IN-PROGRESS?* NIL
  "Binding this variable to T around a call to redisplay will 'force'
   the redisplay. That is it will cause a complete redisplay of the
   screen. FORCE-REDISPLAY-WINDOW uses this.")

(defvar *uc-copyright-free* t)

(DEFVAR *MARK* NIL)

(DEFVAR *CURSOR-BLINKER-WID* 3.)

(DEFVAR *CURSOR-BLINKER-MIN-HEI* 12.)

(DEFVAR *MINIMUM-CURSOR-HEIGHT* 12.
  "The minimum height to draw the cursor so that it doesn't dissapear.")

(DEFVAR *MULTIPLICATION* 1)

(DEFVAR *BOXER-READTABLE* (COPY-READTABLE nil))

(DEFVAR *BOXER-FUNCTIONS* NIL
  "This variable contains a list of symbols for all the
   lisp functions imported to Boxer.")

;; sgithens 2021-05-07 Oddly it doesn't look like these three are used anymore...

(DEFVAR *CURRENT-SCREEN-BOX* NIL
  "The Lowest Level Screen Box Which Contains the *Point*")

(DEFVAR *MARKED-SCREEN-BOX* NIL
  "The Lowest Level Scren Box Which Contains the *mark*")

(DEFVAR *OUTERMOST-BOX* NIL
  "Inside of REDISPLAYING-WINDOW, this variable is bound to the window
   being redisplayed's outermost-box. This is the box which currently
   fills that window.")

(defvar *egc-enabled?* nil)

(DEFVAR *CONTROL-CHARACTER-DISPLAY-PREFIX* #\
  "For display of control characters (all of them until we decide on different prefixes")

(defvar *default-font-map-length* 10)

(DEFVAR *GRAY* nil
  "Bound to a window system specific tiling pattern used for drawing shrunken boxes")


;;;;
;;;; FILE: boxnet.lisp
;;;;
;;;; --entire-file--

;;;-*-LISP-*-


;;; $Header: boxnet.lisp,v 1.0 90/01/24 22:07:00 boxer Exp $

;;; $Log:	boxnet.lisp,v $
;;;Revision 1.0  90/01/24  22:07:00  boxer
;;;Initial revision
;;;

#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+


Modification History (most recent at top)

 2/13/03 merged current LW and MCL files, no differences, copyright updated


|#

(in-package :boxnet)

;;; Call setup-server.
;;; Then call either SETUP-CONNECTION-WAITING-PROCESS
;;; or ENABLE-POLLING with a function which receives
;;; a stream as input.  That function should deal with the stream,
;;; but should not interact with the display or anything.
;;; It should use the bw::*boxer-command-loop-event-handlers-queue*
;;; mechanism for getting things to run later.
;;;
;;; We should macroize all that and relax the restrictions and
;;; probably just settle on polling and punt the interrupt stuff
;;; and regularize the names.

#|
(eval-when (load eval)
           #+(and (not solaris) Lucid) (progn
                                        #+sparc   (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "receive.sun4.o"))
                                        #+MC68000 (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "receive.sun3.o"))
                                        #+sparc   (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "send.sun4.o"))
                                        #+MC68000 (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "send.sun3.o")))
           )

;;; At least remind during compilation....

(eval-when (compile)
           (format t "~%***~%NOTE:You may have to recompile the C files send.c ~%~
             and receive.c for this architecture~%***")
           )

|#

(defvar *foreign-netsocket-file*
  #+(and sparc (not solaris)) "netsocket.sun4.o"
  #+(and sparc solaris)       nil ; "netsocket.solaris.o"
  #+MC68000                   "netsocket.sun3.o")

(eval-when (eval load)
           #+lucid (unless (null *foreign-netsocket-file*)
                     (lcl::load-foreign-files
                      (merge-pathnames
                       (getf (sm::system-properties (find-system-named 'boxer))
                             :pathname-default)
                       *foreign-netsocket-file*)))
           )

;;;; compilation reminder (shouldn't the defsystem do this ?)
(eval-when (compile)
           (format t "~%***~%NOTE:You may have to recompile the C files netsocket.c ~%~
             for this architecture~%***")
           )


;;returns the file descriptor bits to give to select-and-accept.
#+(and Lucid (not solaris))
(lcl:define-c-function (socket-listen-and-bind "_socket_listen_and_bind")
                       ((port-number :integer))
                       :result-type :integer)

;; returns a unix file handle for make-lisp-stream.
#+(and Lucid (not solaris))
(lcl:define-c-function (select-and-accept "_select_and_accept")
                       ((socket-object :integer)
                        (poll-time-in-usec :integer))
                       :result-type :integer)


#+(and Lucid (not solaris))
(lcl:define-c-function (%close-socket "_close") ((fd :integer)) :result-type :integer)

;; returns a unix file handle for make-lisp-stream.
#+(and Lucid (not solaris))
(lcl:define-c-function (socket-open-and-connect "_socket_open_and_connect")
                       ((hostname :string) (port-number :integer))
                       :result-type :integer)




(defvar *socket* nil)
(defvar *boxer-port-number* 7000)

(defvar *connection-waiting-process* nil)
(defvar *connection-port* nil)

(defun setup-server ()
  (when (or (null *socket*) (not (plusp *socket*)))
    (let ((socket (socket-listen-and-bind *boxer-port-number*)))
      (when (minusp socket)
        (warn "Failed to set up for remote connection:"))
      (setq *socket* socket))))

#+Lucid
(defun setup-connection-waiting-process (handler-function)
  (setq *connection-waiting-process*
        (lcl::make-process
         :function #'handle-connection :args (list handler-function)
         :wait-function (let ((socket *socket*)
                              (process lcl::*current-process*))
                          #'(lambda ()
                                    ;; can't use global variables in the wait function.
                                    (let ((port (select-and-accept socket 100)))
                                      (if (not (zerop port))
                                        (progn
                                         (setf (lcl::symbol-process-value
                                                '*connection-port* process) port)
                                         t)
                                        nil)))))))

;;; the server process calls this on the stream when the connection is made.
(defun handle-connection (handler-function)
  (cond ((minusp *connection-port*)
         (when (eq boxer::*boxer-send-server-status* :interrupt)
           (close-server)
           (format t "<error: connection lost -- killing process>~%")))
    (t
     (let ((stream nil))
       (unwind-protect
        (progn (setq stream
                     #+Lucid (lcl::make-lisp-stream :input-handle  *connection-port*
                                                    :output-handle  *connection-port*
                                                    :element-type '(unsigned-byte 16))
                     #-Lucid (warn "~S undefined in this Lisp" 'make-lisp-stream))
               (funcall handler-function stream))
        (close stream)
        (setq *connection-port* nil))))))

(defun close-server ()
  (when (and *socket* (plusp *socket*))
    (%close-socket *socket*)
    (setq *socket* nil))
  (when (and *connection-port* (plusp *connection-port*))
    (close *connection-port*)
    (setq *connection-port* nil))
  (when *connection-waiting-process*
    #+Lucid (lcl::kill-process *connection-waiting-process*)
    #-Lucid (warn "~S undefined in this Lisp" 'kill-process)
    (setq *connection-waiting-process* nil)))


;;; polling
(defvar *polling-handler* nil)
(defvar *net-connection-toplevel-polling-time* #+Lucid 200)

#+Lucid
(defun net-connection-toplevel-polling-function ()
  (when (not (null *polling-handler*))
    (let ((port (select-and-accept *socket*  *net-connection-toplevel-polling-time*)))
      (when (not (zerop port))
        (setq *socket* nil)
        (setq *connection-port* port)
        (handle-connection *polling-handler*)))))


(defun enable-polling (function)
  (setq *polling-handler* function))

(defun disable-polling ()
  (setq *polling-handler* nil))



;;; send
(defmacro with-open-port-stream ((stream-var hostname) &body body)
  `(with-open-stream
     (,stream-var (let ((fd (socket-open-and-connect ,hostname)))
                    (when (minusp fd)
                      (boxer-eval::primitive-signal-error
                       "Couldn't connect to host:" ,hostname))
                    #+Lucid (lcl:make-lisp-stream :input-handle fd
                                                  :output-handle fd
                                                  :element-type '(unsigned-byte 16))
                    #-Lucid (warn "~S undefined for this Lisp" 'MAKE-LISP-STREAM)))
     .,BODY))

;;;;
;;;; FILE: boxwin-opengl.lisp
;;;;

#+cocoa
(defvar *cocoa-boxer-interface* nil)

;; *** should be acceptable to (setf (graphics-state-pattern ...
;; the number vectors are lists of either 1 or 0
(defun make-pattern (number-vectors)
  (declare (ignore number-vectors))
  )

(eval-when (compile load eval)
  (capi:define-interface load-progress-frame ()
    ()
    (:panes
    (loadbar-pane capi::progress-bar)
    (message-pane capi:display-pane))
    (:layouts
    (progree-layout capi:column-layout
                    '(loadbar-pane message-pane)
                    :columns 1 :rows 2 :x-uniform-size-p t))
  ;  (:menus
  ;   (min-menu ""
  ;             ((:component
  ;               (
  ;                ("Quit"
  ;                 :callback 'capi:destroy))))))
    ;(:menu-bar min-menu)
    (:default-initargs
    :title "Loading Boxer..."
    :auto-menus nil
    :best-x 250 :best-y 500 :best-width 500 :best-height 50
    :window-styles '(:borderless :always-on-top :ignores-keyboard-input)))
)

;; might have to go to ignore-errors if problems continue
(defmethod incr-bar ((self load-progress-frame) percentage
                     &optional newtext (cr? T))
  (let ((loadbar-pane (slot-value self 'loadbar-pane)))
    (capi::apply-in-pane-process loadbar-pane
                                 #'(setf capi:range-slug-start)
                                 percentage loadbar-pane))
  (when (not (null newtext))
    (let* ((message-pane (slot-value self 'message-pane))
           (existing-text (capi:display-pane-text message-pane))
           (new-text (progn
                       (cond ((listp existing-text))
                             ((stringp existing-text)
                              (cond ((string= existing-text "")
                                     (setq existing-text nil))
                                    (t (setq existing-text (list existing-text))))))
                       (cond ((null existing-text)
                              (list newtext))
                             ((null cr?)
                              (append (butlast existing-text)
                                      (list
                                       (concatenate 'string (car (last existing-text))
                                                    " " newtext))))
                             (t
                              (append existing-text (list newtext)))))))
      (capi::apply-in-pane-process message-pane
                                   #'(setf capi::display-pane-text)
                                   new-text message-pane))))

(defconstant *number-of-mouse-buttons* 3)

;; 2022-01-06 A copy of window-system-specifc-boxer before major refactoring
(defun window-system-specific-start-boxer ()
  (gp:register-image-translation
    'toolbar-scratch-images
    (gp:read-external-image (merge-pathnames "./images/scratch-icons.png" boxer::*resources-dir*)))

  (let ((boot-start-time (get-internal-real-time))
        (progress-bar (make-instance 'load-progress-frame)))
    (flet ((start-boxer-progress (fstring time percentage)
             (incr-bar progress-bar percentage
                       (format nil fstring (- time boot-start-time)))))
      (capi:display progress-bar)
      (start-boxer-progress "Starting ~D" (get-internal-real-time) 10)
      (when (member "-debug" sys:*line-arguments-list* :test #'string-equal)
        (break "Start Boxer"))
      (when (boxer::box? *old-world*)
        (setf (boxer::slot-value *old-world* 'boxer::screen-objs) nil))
      (setq boxer-eval::*current-process* nil)
      (setq *old-world* boxer::*initial-box*)
      ;; extensions
      (setq boxer::*starting-directory-pathname* (lw:lisp-image-name))
      ;; sgithens TODO - Removing extensions for now March 7, 2020
      ;; (boxer::load-boxer-extensions)
      ;; (start-boxer-progress "Loaded Extensions ~D" (get-internal-real-time) 20)

      ;; load prefs if they exists
      (let ((pf (boxer::default-lw-pref-file-name)))
        (when (and pf (probe-file pf))
          (boxer::handle-preference-initializations pf)))
      (start-boxer-progress "Initialized Preferences ~D"
                            (get-internal-real-time) 30)
      ;; maybe set the size of the boxer window...
      ;; check window size prefs, they will be overidden by the following
      ;; fullscreen-window check
      (let ((screen (capi:convert-to-screen)))
        (when (> *starting-window-width* 0)
          (capi:set-hint-table *boxer-frame* (list :width *starting-window-width*)))
        (when (> *starting-window-height* 0)
          (capi:set-hint-table *boxer-frame* (list :height *starting-window-height*)))
        ;; fullscreen check AFTER prefs are loaded but BEFORE display ?
        (when *fullscreen-window-p*
          (capi:set-hint-table *boxer-frame*
                          (list :x 0 :y 0
                                :width (- (capi:screen-width screen) 10)
                                :height (- (capi:screen-height screen) 120)))))
      (start-boxer-progress "Setting Hints ~D" (get-internal-real-time) 40)
      (capi:display *boxer-frame*)
      (start-boxer-progress "Display ~D" (get-internal-real-time) 50)
      (when (member "-debug" sys:*line-arguments-list* :test #'string-equal)
        (opengl:describe-configuration *boxer-pane*))
      ;; move to inits
;     (let ((gs (gp::get-graphics-state *boxer-pane*)))
;     (setf (gp::graphics-state-foreground gs) boxer::*foreground-color*))
      ;; opengl equivalent would be...
      (opengl:rendering-on (*boxer-pane*)
                    (initialize-ogl-color-pool)
                    (boxer::initialize-gray-patterns)
                    (boxer::initialize-colors)
                    (%set-pen-color box::*foreground-color*)
                    ;; do other OpenGL inits...
                    (setq *ogl-current-color-vector* (make-ogl-color 0.0 0.0 0.0)
                          *blinker-color* (make-ogl-color .3 .3 .9 .5))
                    (opengl:gl-enable opengl:*gl-scissor-test*)
                    (opengl::gl-enable opengl::*gl-line-smooth*)
                    (opengl::gl-enable opengl::*gl-polygon-smooth*)
                    (opengl::gl-enable opengl::*gl-blend*)
                    (opengl::gl-blend-func opengl::*gl-src-alpha* opengl::*gl-one-minus-src-alpha*)
                    (opengl::gl-hint opengl::*gl-line-smooth-hint* opengl::*gl-nicest*))

      (let ((arial-12 (boxer::make-boxer-font '("Arial" 12)))
            (arial-16 (boxer::make-boxer-font '("Arial" 16)))
            (arial-16-bold (boxer::make-boxer-font '("Arial" 16 :bold))))
        (setq  boxer::*normal-font-no*           arial-16
               boxer::*default-font*             arial-16
               boxer::*box-border-label-font-no* arial-12
               boxer::*border-label-font*        arial-12
               boxer::*box-border-name-font-no*  arial-16-bold
               boxer::*border-name-font*         arial-16-bold
               boxer::*sprite-type-font-no*      arial-16-bold
               boxer::*initial-graphics-state-current-font-no* arial-16-bold
               boxer::*graphics-state-current-font-no* arial-16-bold
               boxer::*boxtop-text-font*         arial-16-bold
        ))
      ;; #+freetype-fonts
      (boxer::load-freetype-faces)
      (let ((boxer::%private-graphics-list nil))
        ;; needed by shape-box updater in the redisplay inits but not set until
        ;; (boxer-eval::setup-evaluator) farther down
        (run-redisplay-inits))

      (start-boxer-progress "RDP inits ~D" (get-internal-real-time) 60)
      (boxer::load-appdata)
      (fixup-menus)
      (setup-editor *old-world*)
      (setq *display-bootstrapping-no-boxes-yet* nil)
      (start-boxer-progress "Editor ~D" (get-internal-real-time) 70)
      (boxer-eval::setup-evaluator)
      (start-boxer-progress "Eval ~D" (get-internal-real-time) 80)
      ;; should handle double clicked files here...
      (multiple-value-bind (start-box as-world?)
          (load-startup-file)
        (when (boxer::box? start-box)
          (cond ((not (null as-world?))
                 (setup-editor start-box))
                (t (setup-editor (boxer::make-box (list (list start-box))))))))
      (unless boxer::*boxer-version-info*
        (setq boxer::*boxer-version-info*
              (format nil "~:(~A~) Boxer" (machine-instance))))
      (set-cursor-visibility *point-blinker* t)
      ;; wait a sec
      ;; now that everything is defined, we can safely run redisplay
      (resize-handler-utility)
      (update-toolbar-font-buttons)
      ;; and check for initial double clicked file box
      (when (eq :open-file (caar *pending-osx-events*))
        (safe-open-double-clicked-file (cdar *pending-osx-events*))
        (setq *pending-osx-events* nil))
      (start-boxer-progress "Starting Command Loop ~D" (get-internal-real-time) 100)
      (sleep 1)

      (update-visible-editor-panes)
      (boxer::switch-use-mouse2021 *use-mouse2021*)

      (capi:destroy progress-bar)
      (boxer-process-top-level-fn *boxer-pane*))))

;; sgithens 2022-01-06 Not sure what this *old-world* var was ever used for,
;; maybe to cache some previous instance or something, but going forward we'll
;; just pass the *initial-box* into setup-editor
(defvar *old-world* nil)

;; inside window-system-specific-start-boxer
      (when (boxer::box? *old-world*)
        (setf (boxer::slot-value *old-world* 'boxer::screen-objs) nil))

      (setq *old-world* boxer::*initial-box*)

      (setup-editor *old-world*)



(defvar *typeahead-during-eval* nil)

;; sgithens TODO this doesn't appear to be used. Looks to have been replaced by
;;               *double-click-pause-time*
(defvar *double-click-wait-interval* .3
  "Number of seconds to wait for another (possible) mouse click")

(defvar *literal-input*  nil)

(defun handle-event-internal (event &optional bits)
  (boxer-system-error-restart
;    (boxer-editor-bindings nil
    ;  Wrong place, this needs to be wrapped around the top level loop
      (catch 'boxer::boxer-editor-top-level
        (handle-boxer-input event bits)
        (setq just-redisplayed? nil)
        ;; if there is no more input, then redisplay
        (when (no-more-input?)
          (boxer::repaint)
          (setq just-redisplayed? t)
          (boxer-idle-function)))))

(defvar *suppress-event-queueing?* nil)

(defvar *blinker-alpha-value* .3)

(defun update-blinker-color ()
  #+win32
  (let ((bc (color:get-color-spec 'win32::color_highlight)))
    (setq *blinker-color* (make-ogl-color (color:color-red bc)
                                          (color:color-green bc)
                                          (color:color-blue bc)
                                          *blinker-alpha-value*))))

;; for ALT key handling
#+lispworks4
(defmethod capi::interface-keys-style ((self boxer-frame)) :emacs)

;(defun boxer-expose-window-handler (pane x y wid hei)
;  (declare (ignore pane x y wid hei))
;  (funcall *expose-window-handler-function*))

;(defun boxer-expose-window-handler (pane x y wid hei)
;  (declare (ignore pane x y))
;    (funcall *expose-window-handler-function* wid hei))

;(defun bootstrap-expose-window-function (wid hei)
;  (rendering-on (*boxer-pane*) (ogl-reshape wid hei)))

#|
(defsetf sheet-blinker-list (window) (new-list)
  `(let ((entry (box::fast-assq ,window *boxer-window-blinker-alist*)))
     (if (null entry)
   (push ,new-list *boxer-window-blinker-alist*)
   (setf (cdr entry) ,new-list))))
|#

(defmacro with-open-blinker ((blinker) &body body)
  blinker
  `(progn . ,body))

#|
(defmacro with-open-blinker ((blinker) &body body)
  `(macrolet ((draw-generic-blinker (blinker)
                `(etypecase ,blinker
                   (region-row-blinker (draw-region-row-blinker ,blinker))
                   (blinker (draw-blinker ,blinker)))))
     (boxer::without-interrupts
       (unwind-protect
         (progn (when (blinker-visibility ,blinker)
                  (box::drawing-on-window-without-prepare-sheet (*boxer-pane*)
                    (box::with-origin-at (0 0) ; invoke the quickdraw scaling
           (draw-generic-blinker ,blinker))))
                ;; Erase the blinker if it is visible
                ;; then do whatever you were going to do
                . ,body)
         (when (blinker-visibility ,blinker)
           ;; If the blinker is supposed to be visible, then redraw it
           (box::drawing-on-window-without-prepare-sheet (*boxer-pane*)
             (box::with-origin-at (0 0) ; invoke the quickdraw scaling
               (draw-generic-blinker ,blinker))))))))

(defmacro with-open-blinkers (blinker-list &body body)
  `(macrolet ((draw-generic-blinker (blinker)
     `(etypecase ,blinker
       (region-row-blinker (draw-region-row-blinker ,blinker))
       (blinker (draw-blinker ,blinker)))))
;     (box::drawing-on-window-without-prepare-sheet (*boxer-pane*) ; +++ this is probably not the right thing, fix
;; **** Almost, actually this needs to be wrapped around the blinker drawing
;; **** but NOT the body since the body will be inside a drawing-on-window
      (unwind-protect
   (progn (boxer::without-interrupts
                  (boxer::drawing-on-window-without-prepare-sheet (*boxer-pane*)
                    ;; **** this will set the value of the offset to be the
                    ;; **** origin of the *boxer-pane*
                    (boxer::with-origin-at (0 0)
                      ;; **** this will set the offset in the window system
          (dolist (b ,blinker-list)
            (when (blinker-visibility b)
                          (draw-generic-blinker b))))))
    . ,body)
       (boxer::without-interrupts
         (boxer::drawing-on-window-without-prepare-sheet (*boxer-pane*)
           ;; **** this will set the value of the offset to be the
           ;; **** origin of the *boxer-pane*
           (boxer::with-origin-at (0 0)
             ;; **** this will set the offset in the window system
       (dolist (b ,blinker-list)
         (when (blinker-visibility b)
                 (draw-generic-blinker b)))))))))

|#

#|
(defun image-to-bitmap (image)
  (unless (null image)
    (let* ((wid (gp:image-width image)) (hei (gp:image-height image))
           (bim (make-offscreen-bitmap *boxer-pane* wid hei)))
      (with-system-dependent-bitmap-drawing (bim wid hei)
        (%erase-rectangle wid hei 0 0 bim)
        (with-pen-color (boxer::*red*)
          (boxer::draw-rectangle alu-seta 10 10 10 10))
        (gp:draw-image bim image 0 0))
      (values bim wid hei))))
|#

(defun start-box-copyright-warning ()
  (boxer::make-box '(("Warning: start.box file detected")
                     ("The name of the initial box file has been changed")
                     ("to boxer-init.box"))))

(defmacro with-open-blinkers (blinker-list &body body)
  blinker-list
  `(progn . ,body))

;;; following copied from boxwin-clx.

(defun redisplayable-window? (x)
  (member x *redisplayable-windows*))

(defun kill-redisplayable-window (window)
;  (Destroy-Window window)
  (setq *redisplayable-windows* (delete window *redisplayable-windows*))
  (let ((pair (assoc window *redisplayable-window-outermost-box-alist*)))
    (unless (null pair)
      (setq *redisplayable-window-outermost-box-alist*
      (delete pair *redisplayable-window-outermost-box-alist*)))))

;;;;
;;;; FILE: capogi.lisp
;;;;

(defun allocate-capogi-char-data (capogi-char length)
;;   (setf (capogi-char-data capogi-char) capogi-char)
;;   (fli:allocate-foreign-object :type :byte :nelems length))
;;
;; (defun free-capogi-char-data (capogi-char)
;;   (fli:free-foreign-object (capogi-char-data capogi-char)))

;; array of
(defun make-glyph-array (length) (vector length))

(defvar *snap-to-font-name* nil)

;; these dimensions need to be large enough for the largest glyph
(defvar *glyph-slate-wid* 200)
(defvar *glyph-slate-hei* 150)

(capi:define-interface glyph-slate ()
    ()
  (:panes
   (glyph-pane output-pane
               :drawing-mode :compatible
              :input-model '(((:button-1 :press) show-current-font)
                             (:character show-current-font))
              ))
  (:menus
   (file-menu "File" ((:component
                       (("Open Font" :accelerator #\o :callback 'menu-open-capi-font)))
                      (:component
                       (("Save" :callback 'save-ogl-bitmap-font)))
                      (:component
                       (("Quit" :callback 'capi:destroy))))))
  (:menu-bar  file-menu)
  (:default-initargs :title "Glyph Drawing Surface"
   :width *glyph-slate-wid* :height *glyph-slate-hei*))

(defvar *glyph-window*)
(defvar *glyph-pane*)

(defun make-glyph-window ()
  (setq *glyph-window* (make-instance 'glyph-slate)
        *glyph-pane* (slot-value *glyph-window* 'glyph-pane))
  (capi:display *glyph-window*))

(defun open-capi-font (family size styles)
  (set-glyph-pane-font
   (gp:find-best-font *glyph-pane*
                      (gp:make-font-description :family family
                                                :size size
                                                :weight (if (member :bold styles)
                                                            :bold
                                                          :normal)
                                                :slant (if (member :italic styles)
                                                           :italic
                                                         :roman)))))

(defun menu-open-capi-font (&rest ignore)
  (declare (ignore ignore))
  (let ((font (capi:prompt-for-font "Font to Convert...")))
    (capi:apply-in-pane-process *glyph-pane* #'set-glyph-pane-font font)))

(defun set-glyph-pane-font (font)
  (unless (null font)
    (setf (gp::graphics-state-font (gp::get-graphics-state *glyph-pane*)) font)
    (gp:with-graphics-state (*glyph-pane*)
      (let* ((pretty-name (font-pretty-name font))
             (height (gp:get-font-height *glyph-pane*)))
        (gp:draw-rectangle *glyph-pane* 0 0 (gp:port-string-width *glyph-pane* pretty-name) (+ height 5)
                           :foreground (gp::graphics-port-background *glyph-pane*)
                           :operation alu-seta :filled t)
        (gp:draw-string *glyph-pane* pretty-name 20 height)))))

(defun show-current-font (&rest ignore)
  (declare (ignore ignore))
  (gp:with-graphics-state (*glyph-pane*)
      (let* ((pretty-name (font-pretty-name
                           (gp::graphics-state-font (gp::get-graphics-state *glyph-pane*))))
             (height (gp:get-font-height *glyph-pane*)))
        (gp:draw-string *glyph-pane* pretty-name 0 height))))

(defun font-pretty-name (gpfont &optional stream)
  (let* ((fdesc (gp:font-description gpfont))
         (attr (unless (null fdesc) (gp:font-description-attributes fdesc))))
    (unless (null attr)
      (format stream "~A ~A ~A ~D"
              (getf attr :family) (getf attr :weight) (getf attr :slant) (getf attr :size)))))

(defun make-capogi-font (&optional (pane *glyph-pane*)
                                   (font (gp:graphics-state-font (gp::get-graphics-state pane))))
  (let* ((data-array (make-array *capogi-char-count*))
         (capogi-font (%make-capogi-font :capi-font font
                                         :height (gp:get-font-height pane font)
                                         :ascent (gp:get-font-ascent pane font)
                                         :fixed-width (when (gp:font-fixed-width-p pane font)
                                                        (gp::get-font-average-width pane font))
                                         :chars data-array)))
    (gp:with-graphics-state (pane :operation alu-seta)
      ;; set font
      (dotimes (i *capogi-char-count*)
        (setf (aref data-array i)
              (new-capogi-char (code-char i))))
      (dolist (tpair *unicode-window-1252*)
        (let ((charcode (car tpair)) (glindex (cadr tpair)))
          (unless (>= glindex *capogi-char-count*)
            (setf (aref data-array glindex)
                  (new-capogi-char (code-char charcode)))))))
    capogi-font))

;; window should be made and font already setup...
;; character is drawn with the upper left corner at (cx,cy)
;; this is the inner loop function - optimize here (particularly the stew of pixels & colors)

(defun new-capogi-char (char &optional (cx 0) (cy 0) (pane *glyph-pane*))
  (gp:with-graphics-state (pane :operation alu-seta)
    (multiple-value-bind (left top right bottom)
        (gp:get-character-extent pane char)
      (let* ((wid (abs (- left right))) (hei (abs (- top bottom)))
             (new-char (make-capogi-char :char char :wid wid :hei hei))
             (hor-bytes (ceiling wid 8))
             (maxx (floor wid))
             (bytes nil))
        ;; erase
        (gp::draw-rectangle pane cx cy (+ wid 1) hei
                            :foreground (gp::graphics-port-background pane)
                            :operation alu-seta :filled t)
        ;; now draw the char
        (gp:draw-character pane char (- cx left) (- cy top))
        ;; now we translate the screen data to a list of bytes in left->right,
        ;; bottom->top order.
        ;; actually the loops are backward cause we push each byte as we calculate them
        (let* ((image (gp:make-image-from-port pane cx cy (+ wid 1) hei))
               (imax  (gp:make-image-access pane image)))
          (unwind-protect
              (progn
                (gp::image-access-transfer-from-image imax)
                (do ((y 0 (1+ y)))
                    ((>= y hei) (setf (capogi-char-data new-char) bytes))
                  (do ((x (* (1- hor-bytes) 8) (- x 8)))
                      ((< x 0))
                    (push (glyph-byte-value x y maxx imax) bytes))))
            (gp:free-image-access imax)
            (gp::free-image pane image))
        new-char)))))

;; ;; useful for debugging
(defun show-capogi-char-data (char)
  (let* ((w (capogi-char-wid char))
         (h (capogi-char-hei char))
         (data (capogi-char-data char))
         (hbytes (ceiling w 8)))
    (format t "~%~C" (capogi-char-char char))
    (do ((i (1- h) (1- i)))
        ((minusp i))
      (terpri)
      (dotimes (j hbytes) (format t "~8,'0B  " (nth (+ (* i hbytes) j)  data))))
;    (dotimes (i h)
;      (terpri)
;      (dotimes (j hbytes) (format t "~8,'0B  " (nth (+ (* i hbytes) j)  data))))
    ))

(defun glyph-byte-value (x y maxx image-access)
  (let ((return-byte 0))
    (dotimes (i 8)
      (let* ((gx (+ x i))
             (point (color:unconvert-color  *glyph-pane* (gp:image-access-pixel image-access gx y)))
             (white? (white-point? point))
             )
        (cond ((>= gx maxx) (return nil)) ;; no more valid points to check
              ((not white?) (setq return-byte (+ return-byte (ash 1 (- 7 i))))) )))
    return-byte))

(defun draw-char-for-caching (charcode &optional (x 0) (y 0) (pane *glyph-pane*))
  (gp:with-graphics-state (pane :operation alu-seta)
    (multiple-value-bind (left top right bottom)
        (gp::get-character-extent pane charcode)
      (gp::draw-rectangle pane x y (abs (- left right)) (abs (- top bottom))
                          :foreground (gp::graphics-port-background pane)
                          :operation alu-seta :filled t)
      (gp::draw-character pane charcode (- x left) (- y top))
      (values (abs (- left right)) (abs (- top bottom))))))

;; remember that the Mac app is inside a directory "bundle"
;; <bundle directory>/Contents/MacOS/<executable>
;; we can put the fonts into the bundle in the location <bundle directory>/Contents/Resources/Fonts
(defun font-directory-search ()
  (let ((testfile "Arial10.cfnt")
        (searchlist (list *capogi-font-directory*
                          #+macosx
                          (make-pathname :directory
                                         (append (butlast (pathname-directory (lw:lisp-image-name)))
                                                 '("Resources" "Fonts")))
                          (make-pathname :directory
                                         (append (pathname-directory (lw:lisp-image-name))
                                                 '("Fonts"))))))
    (dolist (folder-key '(:appdata :local-appdata :common-appdata))
      (push (make-pathname :directory (append (pathname-directory
                                               (sys::get-folder-path folder-key))
                                              '("Boxer" "Fonts")))
            searchlist))
    (dolist (sd searchlist)
      (when (probe-file (merge-pathnames testfile sd))
        (return sd)))))

(defun cfont-filename (cfont)
  (let ((fv (capi-font-values (capogi-font-capi-font cfont))))
    (make-cfont-filename (car fv) (cadr fv) (cddr fv))))

(defun dump-capogi-font (font)
  (with-capogi-font-stream (s (merge-pathnames (cfont-filename font) (capogi-font-directory))
                              :output)
    (dump-capogi-font-internal font s)))

;; clean byte stream version...
(defun dump-capogi-font-internal (font stream)
  (let ((font-values (capi-font-values (capogi-font-capi-font font)))
        (chars (capogi-font-chars font)))
    ;; capogi-font "magic" number: 2 bytes #xF0, #x3D corresponds to boxer dumper bin-op
    (write-byte #xF0 stream) (write-byte #x3D stream)
    ;; 1 byte for version number
    (write-byte *capogi-font-file-version* stream)
    ;; name string,
    (dump-simple-string (car font-values) stream)
    ;; 1 byte for size, 1 byte for styles
    ;; on mac, size is float but we are only interested in integer sizes, so this is safe
    (write-byte (round (cadr font-values)) stream)
    (write-byte (font-styles-byte (cddr font-values)) stream)
    ;; 1 byte for height, qbyte for ascent, 1 byte for fixed-width (0 means no fixed width)
    (write-byte (capogi-font-height font) stream)
    (write-byte (capogi-font-ascent font) stream)
    (let ((fw (capogi-font-fixed-width font)))
      (write-byte (if (null fw) 0 (ceiling fw)) stream))
    ;; now the chars...
    (let ((ccount (capogi-font-count font)))
      (when (> ccount 255)
        (error "~D chars is larger than the current file format supports" ccount))
      (write-byte ccount stream)
      (dotimes (i ccount) (dump-capogi-char (svref chars i) stream)))))

(defun capi-font-values (f)
  (cond ((gp::font-p f)
         (let* ((fd (gp:font-description f))
                (weight (gp:font-description-attribute-value fd :weight))
                (slant  (gp:font-description-attribute-value fd :slant)))
           (list* (gp:font-description-attribute-value fd :family)
                  (gp:font-description-attribute-value fd :size)
                  (cond ((and (eq weight :normal) (eq slant :roman)) nil)
                        ((and (eq weight :bold)   (eq slant :italic)) '(:bold :italic))
                        ((eq weight :bold) '(:bold))
                        ((eq slant :italic) '(:italic))))))
        ((listp f) f)
        (t (error "Unknown CAPI font value, ~A" f))))

;; limitation that string < 255 chars
(defun dump-simple-string (string stream)
  (let ((count (length string)))
    (if (> count 255)
        (error "~D exceeds capability for DUMP-SIMPLE-STRING" count)
      (write-byte count stream))
    (dotimes (i count) (write-byte (char-code (char string i)) stream))))

(defun dump-capogi-char (glyph stream)
  (let* ((data (capogi-char-data glyph))
         (dlength (if (listp data) (length data) (car (fli:foreign-array-dimensions data))))
         (charcode (char-code (capogi-char-char glyph))))
    (flet ((char-bytes ()
             (values (ldb (byte 8 24) charcode) (ldb (byte 8 16) charcode)
                     (ldb (byte 8  8) charcode) (ldb (byte 8  0) charcode))))
      ;; 1st dump 4 bytes of char code
      (multiple-value-bind (b1 b2 b3 b4)
          (char-bytes)
        (write-byte b1 stream) (write-byte b2 stream)
        (write-byte b3 stream) (write-byte b4 stream))
      ;; now 1 byte of width
      (write-byte (capogi-char-wid glyph) stream)
      ;; 2 bytes of data length
      (write-byte (ldb (byte 8 8) dlength) stream) (write-byte (ldb (byte 8 0) dlength) stream)
      ;; now the data
      (if (listp data)
          (dolist (b data) (write-byte b stream))
        (dotimes (i dlength)
          (write-byte (fli:foreign-aref data i) stream))))))

(defun fill-capogi-font-cache (&optional verbose? save?)
  (when (null *capogi-font-cache*)
    (setq *capogi-font-cache* (init-capogi-font-cache)))
  (make-glyph-window) ; setq's *glyph-window* and *glyph-pane*
  (let ((last-time (get-internal-real-time)))
    (dolist (font-family boxer::*font-families*)
      (dotimes (i (length boxer::*font-sizes*))
        (let ((size (svref boxer::*font-sizes* i)))
          (dolist (style '(nil (:bold) (:italic) (:bold :italic)))
            (open-capi-font font-family size style)
            (let ((fam-i (position font-family boxer::*font-families* :test #'string=))
                  (size-i (boxer::%font-size-to-idx size))
                  (style-i (boxer::%font-face-to-idx style)))
              (when verbose?
                (format t "~&~A  ~D ~{~A ~}" font-family size style))
              ;; put the capogi font into *capogi-font-cache*
              (let ((styles (aref (aref *capogi-font-cache* fam-i) size-i))
                    (new-font (make-capogi-font)))
                (setf (aref styles style-i) new-font)
                (when verbose?
                  (format t ".....Making (~A sec) " (let ((new (get-internal-real-time)))
                                                      (prog1
                                                          (/ (- new last-time) 1000.0)
                                                        (setq last-time new)))))
                (when save?
                  (dump-capogi-font new-font)
                  (when verbose?
                    (format t ".....Saving (~A sec) " (let ((new (get-internal-real-time)))
                                                        (prog1
                                                            (/ (- new last-time) 1000.0)
                                                          (setq last-time new)))))))))))))
  (when save? (save-capogi-fonts-info))
  (capi::destroy *glyph-window*))

;; it may be neccessary to do this before a delivery - especially as load-capogi-font currently
;; generates ffi data
(defun clear-capogi-font-map ()
  (unless (null *capogi-font-cache*)
    (dotimes (i (length *capogi-font-cache*))
      (let ((fam (svref *capogi-font-cache* i)))
        (dotimes (j (length fam))
          (let ((size (svref fam j)))
            (dotimes (k (length size))
              (setf (svref size k) nil))))))))

(defun save-capogi-fonts-info (&optional (other-info (format nil "Converted with white defined as unweighted RGB > ~F" *white-enuff*)))
  (let* ((cfd (capogi-font-directory))
         (infofilename (merge-pathnames "info.txt" cfd)))
    (with-open-file (s infofilename :direction :output :element-type 'character :if-exists :supersede)
      (multiple-value-bind (sec min hou date month year)
          (decode-universal-time (get-universal-time))
        (format s "OpenGL CAPI fonts converted on ~D:~D:~D  ~D/~D/~D~%" hou min sec month date year))
      (format s "Font Families: ")
      (do* ((fams boxer::*font-families* (cdr fams))
            (fam (car fams) (car fams)))
           ((null fam) (terpri s))
        (write-string fam s)
        (unless (null (cdr fams)) (write-char #\, s) (write-char #\space s)))
      (format s "Sizes: ")
      (dotimes (i (length boxer::*font-sizes*))
        (let ((size (svref boxer::*font-sizes* i)))
          (format s "~D " size)))
      (unless (null other-info) (format s "~%~A" other-info)))))

#|

;;; testing...

(Setq *gw* (make-glyph-window)) ; then Open Font

(defun test-draw-capogi-char (char x y)
  (drawing-on-window (*boxer-pane*)
                     (opengl::gl-pixel-storei opengl::*gl-unpack-alignment* 1)
                     (opengl::gl-raster-pos2-i x y)
                     (fli:with-dynamic-foreign-objects ((bm (:unsigned :byte)
                                                            :nelems (length (opengl::capogi-char-data char))
                                                            :initial-contents (opengl::capogi-char-data char)))
                       (opengl::gl-bitmap (opengl::capogi-char-wid char) (opengl::capogi-char-hei char) 0.0 0.0 0.0 0.0 bm)))
  (force-graphics-output))

(defun test-draw-capogi-string (string font x y)
  (boxer::drawing-on-window (boxer::*boxer-pane*)
    (opengl::gl-pixel-storei opengl::*gl-unpack-alignment* 1)
    (opengl::gl-raster-pos2-i x y)
    (do* ((chars (capogi-font-chars font))
          (i 0 (1+ i)))
         ((= i (length string)))
      (let ((char (svref chars (char-code (char string i)))))
        (fli:with-dynamic-foreign-objects ((bm (:unsigned :byte)
                                               :nelems (length (opengl::capogi-char-data char))
                                               :initial-contents (opengl::capogi-char-data char)))
           (opengl::gl-bitmap (opengl::capogi-char-wid char) (opengl::capogi-char-hei char) 0.0 0.0
                          (float (capogi-char-wid char)) 0.0 bm)))))
  (boxer::force-graphics-output))

|#

;;;;
;;;; FILE: chunker.lisp
;;;;

;; these next to vars are obsoleted by the above

(defvar *non-alphanumeric-characters*
  '(#\< #\> #\, #\. #\\ #\/ #\? #\; #\' #\" #\( #\)
    #\[ #\] #\# #\$ #\% #\& #\* #\- #\_ #\+ #\=
    #\~ #\` #\{ #\}))

(defvar *extended-characters*
  #+3600 `(#\Center-Dot #\Down-Arrow #\Alpha #\Beta #\And-sign
                        #\Not-sign #\Epsilon #\Pi #\Lambda #\Gamma
                        #\Delta #\Up-Arrow #\Plus-Minus #\Circle-Plus
                        #\Infinity #\Partial-Delta #\Left-Horseshoe
                        #\Right-Horseshoe #\Up-Horseshoe #\Down-Horseshoe
                        #\Universal-Quantifier #\Existential-Quantifier
                        #\Circle-X #\Double-Arrow #\Left-Arrow
                        #\Right-Arrow #\Not-Equals #\Lozenge
                        #\Less-Or-Equal #\Greater-Or-Equal
                        #\Equivalence #\Or-sign)
  #+TI    `(#\Center-Dot #\Down-Arrow #\Alpha #\Beta #\And-sign
                         #\Not-sign #\Epsilon #\Pi #\Lambda #\Gamma
                         #\Delta #\Up-Arrow #\Plus-Minus #\Circle-Plus
                         #\Infinity #\Partial-Delta #\Left-Horseshoe
                         #\Right-Horseshoe #\Up-Horseshoe
                         #\Down-Horseshoe #\Universal-Quantifier
                         #\Existential-Quantifier #\Circle-X
                         #\Double-Arrow #\Left-Arrow #\Right-Arrow
                         #\Not-Equals #\Lozenge #\Less-Or-Equal
                         #\Greater-Or-Equal #\Equivalence #\Or-sign)
  #+(or lwwin mcl SUN)   `()
  "Non ASCII characters specific to particular machine keyboards." )

#+mcl
(defun %set-chunking-syntax (char chunking-table ns)
  (let ((defined-handler
          (cdr (fast-assq ns (chunk-table-handler-translation-alist
                              chunking-table)))))
    (cond ((and (null defined-handler)
                (not (old-functionp ns)))
           (error "~A is not a valid syntax" ns))
          (t
           (setf (svref& (chunk-table-handlers chunking-table)
                         (char-code char))
                 (if (null defined-handler)
                     ns
                     defined-handler)))))
  ns)

#+mcl
(defsetf get-chunking-syntax %set-chunking-syntax)

;;;;
;;;; FILE: comdef.lisp
;;;;

;; sgithens 2022-02-08 Removing the boxer-editor-message bit from entering-region-mode
(defun entering-region-mode ()
  (let ((main-cut-name #+(or apple win32) (current-mouse-click-name 0 2)
                       #-(or apple win32) (current-mouse-click-name 0 0))
        (main-copy-name #+(or apple win32) (current-mouse-click-name 0 1)
                        #-(or apple win32 )(current-mouse-click-name 2 0)))
    (boxer-editor-message "~A to cut, or ~A to copy the region"
                          main-cut-name main-copy-name)
    (set-mouse-cursor :region-mode)
    (add-mode (region-mode))))


;; #-opengl
;; (defun track-mouse-area (hilight-fun &key x y width height)
;;   (let ((backing-store (allocate-backing-store width height)))
;;     (drawing-on-window (*boxer-pane*)
;;       (let ((min-x (-& x *border-tab-hysteresis*))
;;             (min-y (-& y *border-tab-hysteresis*))
;;             (max-x (+& x width *border-tab-hysteresis*))
;;             (max-y (+& y height *border-tab-hysteresis*))
;;             (icon-on? t))
;;         (flet ((icon-on ()
;;                  (erase-rectangle width height x y)
;;                  (funcall hilight-fun x y width height)
;;                  (force-graphics-output)
;;                  (setq icon-on? T))
;;                (icon-off ()
;;                  (repaint) ; gak !!
;;                  ;(bitblt-to-screen alu-seta width height backing-store 0 0 x y)
;;                  ;(force-graphics-output)
;;                  (setq icon-on? nil)))
;;           ;; 1st grab what's on the screen...
;;           (bitblt-from-screen alu-seta width height backing-store x y 0 0)
;;           ;; initially turn the icon on since that is how we got here in the 1st place
;;           (icon-on)
;;           (multiple-value-bind (final-x final-y)
;;               (with-mouse-tracking ((mouse-x x) (mouse-y y))
;;                 (progn
;;                   (cond ((and (null icon-on?)
;;                               ;; if the icon is off, and we move back in
;;                               (<& min-x mouse-x max-x) (<& min-y mouse-y max-y))
;;                          ;; then turn the icon back on
;;                          (icon-on))
;;                         ((and icon-on?
;;                               ;; if the icon is on and we move out
;;                               (or (not (<& min-x mouse-x max-x))
;;                                   (not (<& min-y mouse-y max-y))))
;;                          ;; then turn off the visual indicator
;;                          (icon-off)))))
;;             ;; first turn the icon off if it is on...
;;             (unless (null icon-on?) (icon-off))
;;             (deallocate-backing-store backing-store)
;;             ;; now return whether we are still on...
;;             (and (<& min-x final-x max-x) (<& min-y final-y max-y))))))))

;;;; Wrong! You should be using modes now !!!

#|
;;; leave it here until we manage to flush old code
;;; utilities for temporarily rebinding keys (like for
;;; copying/moving regions)

(defvar *saved-key-functions* nil)

;;; Note, this is saving and rebinding TOP LEVEL bindings
;;; shadowed bindings will remain unaffected
(defun save-and-rebind-key (key-name new-function)
  (let ((existing (if (boundp key-name) (caddr (symbol-value key-name)) ':unbound))
        (entry (fast-assq key-name *saved-key-functions*)))
    ;; record the old version
    (cond ((null entry) (push (cons key-name existing) *saved-key-functions*))
      (t (setf (cdr entry) existing)))
    ;; now set it to the new version
    (boxer-eval::boxer-toplevel-set-nocache
     key-name
     (boxer-eval::make-compiled-boxer-function
      :arglist nil :precedence 0 :infix-p nil :object new-function))))

;; key should look like 'bu::crap
(defun restore-saved-function (key-name)
  (let ((entry (cdr (fast-assq key-name *saved-key-functions*)))
        (vanilla (lookup-mode-key *global-top-level-mode* key-name)))
    (cond ((null entry)
           (cond ((null vanilla)
                  ;(warn "No saved function for ~A, Unbinding the key" key-name)
                  (boxer-eval::boxer-toplevel-nocache-unset key-name))
             (t
              (warn "No saved function for ~A, setting to top level value" key-name)
              (boxer-eval::boxer-toplevel-set-nocache key-name vanilla))))
      ((or (eq entry ':unbound) (eq entry boxer-eval::*novalue*))
       (boxer-eval::boxer-toplevel-nocache-unset key-name))
      ((boxer-eval::compiled-boxer-function? entry)
       (boxer-eval::boxer-toplevel-set-nocache key-name entry))
      (t
       ;; probably means the previous binding for the key was
       ;; to a box, right thing for now is to unbind the key
       (if (null vanilla)
         (boxer-eval::boxer-toplevel-nocache-unset key-name)
         (progn
          (warn "No saved function for ~A, setting to top level value" key-name)
          (boxer-eval::boxer-toplevel-set-nocache key-name vanilla)))))))
|#

#|
(defun make-generic-port (&rest foo)
  "make a generic port so the person may redirect it"
  (declare (ignore foo))
  (if (null *dummy-box*) (setq *dummy-box* (make-dummy-box)))
  (if (null *generic-port*)
      (progn (setq *generic-port* (port-to-internal *dummy-box*))
       (INSERT-CHA *POINT* *generic-port*)
       (set-mouse-cursor :retarget)
       (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0)
          #'com-redirect-generic-port)
       (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1
                                                            0 :graphics)
          #'com-redirect-generic-port)
       (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1
                                                            0 :sprite)
          #'com-redirect-generic-port)
       (add-redisplay-clue (point-row) ':insert)
       boxer-eval::*novalue*)
      (progn
  (boxer-editor-error "Use the generic port you have made.")
  boxer-eval::*novalue*)))
|#

#| ;; converted to using modes
(defun clean-mouse-port-state ()
  (reset-mouse-cursor)
  ;;; rebind the mouse-middle
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1
                                                    0 :graphics))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1
                                                    0 :sprite))
  (setq *generic-port* nil))
|#

#|
    (save-and-rebind-key main-cut-name #'com-suck-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 2 :graphics)
                         #-mcl (current-mouse-click-name 0 0 :graphics)
                         #'com-suck-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 2 :sprite)
                         #-mcl (current-mouse-click-name 0 0 :sprite)
                         #'com-suck-region)
    (save-and-rebind-key main-copy-name #'com-suck-copy-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 1 :graphics)
                         #-mcl (current-mouse-click-name 2 0 :graphics)
                         #'com-suck-copy-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 1 :sprite)
                         #-mcl (current-mouse-click-name 2 0 :sprite)
                         #'com-suck-copy-region)
|#

#|
  (restore-saved-function #+mcl (current-mouse-click-name 0 2)
                          #-mcl (current-mouse-click-name 0 0))
  (restore-saved-function #+mcl (current-mouse-click-name 0 2 :graphics)
                          #-mcl (current-mouse-click-name 0 0 :graphics))
  (restore-saved-function #+mcl (current-mouse-click-name 0 2 :sprite)
                          #-mcl (current-mouse-click-name 0 0 :sprite))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1)
                          #-mcl (current-mouse-click-name 2 0))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1 :graphics)
                          #-mcl (current-mouse-click-name 2 0 :graphics))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1 :sprite)
                          #-mcl (current-mouse-click-name 2 0 :sprite))
|#

;;;;
;;;; FILE: coms-oglmouse.lisp
;;;;

;; there is only room to display 2 digits of row #'s
;; (defun elevator-row-string (n)
;;   (format nil "~D" n))

#|  ; unused ?
(defun reconcile-region-blinker-list (region blinker-list)
  (let ((existing-blinkers (interval-blinker-list region)))
    (cond ((not (null existing-blinkers))
           (when *boxer-system-hacker*
             (error "Region, ~A, already has blinkers" region))
           (dolist (bl blinker-list) (remove-region-row-blinker bl)))
          (t
           ;; we can just set it because the region redisplay will
           ;; handle the rest-see update-row-blinker-list called by
           ;; interval-update-redisplay-all-rows (region.lisp)
           (setf (interval-blinker-list region) blinker-list)
           ;; we do have to hack the visibility flag because we know
           ;; that the blinkers are already visible
           (setf (interval-visibility region) t)))))
|#

#|
;;; Multi-purpose box size changer.
;;; Lets you get by with just one button for changing size.

;;; Normal boxes:
;;;If clicked on the left half of the box, make it full screen.
;;;If on the right half of a box, make the box smaller.
;;; Shrunken boxes:
;;;Make full screen.
;;; Full Screen boxes:
;;;If clicked on the left half, make the box shrunken.
;;;If on the right half, make it normal size.
(defboxer-command com-mouse-change-box-size (&optional (window *boxer-pane*)
                                             (x (bw::boxer-pane-mouse-x))
                                             (y (bw::boxer-pane-mouse-y))
                                             (mouse-bp
                                              (mouse-position-values x y))
                                             (click-only? t))
  "change the size of the box according to where you mouse"
  window x y click-only? ;  (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((new-box (bp-box mouse-bp))
  (new-row (bp-row mouse-bp))
  (mouse-screen-box (bp-screen-box mouse-bp))
  (new-cha-no (bp-cha-no mouse-bp)))
    (when (and (not (null new-row)) (box? new-box))
      (let ((actual-obj (screen-obj-actual-obj mouse-screen-box)))
  (cond ((eq mouse-screen-box (outermost-screen-box))
         (multiple-value-bind (row cha-no screen-box rel-x rel-y)
       (mouse-position-values x y)
     (declare (ignore row cha-no screen-box rel-y))
     (send-exit-messages new-box mouse-screen-box)
     (move-point-1 new-row new-cha-no mouse-screen-box)
     (if (< (* rel-x 2)
      (screen-object-width mouse-screen-box))
         (com-collapse-box)
         (com-shrink-box))))
        ((shrunken? actual-obj)
         (send-exit-messages
    new-box mouse-screen-box
    (eq (superior-box (point-box)) new-box))
         (move-point-1 new-row new-cha-no mouse-screen-box)
         ;; no send-exit-messages yet.
         (com-mouse-set-outermost-box window x y mouse-bp click-only?))
        (t
         (multiple-value-bind (row cha-no screen-box rel-x rel-y)
     (screen-obj-at-position x y)
     row cha-no screen-box rel-y
     (send-exit-messages new-box mouse-screen-box)
     (move-point-1 new-row new-cha-no mouse-screen-box)
     (cond ((< (* rel-x 2)
         (screen-object-width mouse-screen-box))
      (enter new-box)
      (com-expand-box))
           (t
      (com-collapse-box)))))))))
  boxer-eval::*novalue*)

|#

;;; mouse dispatch function

;;; mouse dispatch function
;(defun com-mouse-left-do-it-all (window x y mouse-bp click-only?)
;  (if #+clx (bw::mouse-on-region-being-defined-p) #-clx nil
;(com-suck-region window x y mouse-bp click-only?)
;      (com-mouse-collapse-box  window x y mouse-bp click-only?)))
;(defun com-mouse-right-do-it-all (window x y mouse-bp click-only?)
;  (if #+clx (bw::mouse-on-region-being-defined-p) #-clx nil
;      (com-suck-copy-region window x y mouse-bp click-only?)
;      (com-mouse-expand-box window x y mouse-bp click-only?)))

;;;
#| ;; old, use modes now
(defun entering-suitcase-bindings ()
  (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0)
                       #'com-bring-back-region)
  (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0 :graphics)
                       #'com-bring-back-region)
  (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0 :sprite)
                       #'com-bring-back-region))

(defun exiting-suitcase-bindings ()
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0 :graphics))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0 :sprite)))
|#

#|
(defun mouse-in-scroll-bar-internal (screen-box x y click-only?
            box-window-x box-window-y
            wid hei left top right bottom)
  (declare (ignore left))
  (let* ((inside-hei (-& hei top bottom))
   (available-shaft (-& inside-hei (*& *scroll-button-height* 2)))
   (ed-box (screen-obj-actual-obj screen-box))
   (norows (length-in-rows ed-box))
         (maxrow nil) ; a cache for the lowest desired row number
   (scroll-row (scroll-to-actual-row screen-box))
   (current-row-no (if (null scroll-row) 0
           (row-row-no ed-box scroll-row)))
   ;; mini scroll if there is more room than we want to use
   (mini-scroll? (>& available-shaft
         (* *max-scroll-grid-increment* norows)))
   (shaft-height (if mini-scroll?
         (* *max-scroll-grid-increment* norows)
         available-shaft))
   (shaft-y (if mini-scroll?
          (+& top *scroll-button-height*
        (floor (-& available-shaft
             (* *max-scroll-grid-increment* norows))
         2)
                          box-window-y)
          (+& top *scroll-button-height* box-window-y)))
   (elevator-y (+& (floor (* (/ current-row-no norows) shaft-height))
       shaft-y))
   (row-heights (make-array norows :initial-element nil))
         (x-offset (+& box-window-x (-& wid right))))
    (declare (simple-vector row-heights))
    (labels ((new-row-no (y)
               ;; make the lower limit include a full box of text
               ;; scrolling down to the last row makes it hard to use
               ;; the scrolling buttons on the resulting 1 row high box
               (let ((raw-row (round (* (/ (min& (max& 0 (-& y shaft-y)) shaft-height)
                         shaft-height)
                      (1-& norows)))))
                 (cond ((null maxrow)
                        (let ((end (ending-row-no raw-row)))
                          (cond ((>=& end norows)
                                 ;; cache should be filled by call to ending-row-no
                                 (or maxrow raw-row))
                                (t raw-row))))
                       (t (min raw-row maxrow)))))
       (get-row-height (row-no)
         (let ((entry (svref& row-heights row-no)))
     (cond ((null entry)
      (let* ((edrow (row-at-row-no ed-box row-no))
             (scrow (car (screen-objs edrow)))
             (row-height
        (if (and scrow
           (not (screen-obj-y-got-clipped?
                 scrow)))
            (screen-obj-hei scrow)
            (estimate-row-height edrow))))
        (setf (svref& row-heights row-no) row-height)
        row-height))
           (t entry))))
       (draw-scroll-grid ()
               ;; erase the box border
               (erase-rectangle *scroll-grid-width* shaft-height
                                x-offset shaft-y)
               (let* ((single-incr (/ shaft-height norows))
                      (draw-singles? (< *min-scroll-grid-increment*
                                        single-incr)))
                 (when (< *min-scroll-grid-increment* (* 10 single-incr))
                   ;; don't draw grid lines at all if they would be too dense
                   (dotimes (i norows)
                     (let ((y-offset (+& (floor (* i single-incr)) shaft-y)))
                       (cond ((zerop& (mod i 10))
                              (draw-rectangle alu-seta
                                              (-& *scroll-grid-width* 2) 2
                                              x-offset y-offset))
                             ((zerop& (mod i 5))
                              (draw-rectangle alu-seta
                                              (-& *scroll-grid-width* 4) 2
                                              (+& x-offset 2) y-offset))
                             ((not (null draw-singles?))
                              (draw-rectangle alu-seta
                                              (-& *scroll-grid-width* 6) 1
                                              (+& x-offset 3)
                                              y-offset))))))))
       (ending-row-no (starting-row)
         (do* ((last-row (length row-heights)) (height 0)
         (row starting-row (1+& row)))
        ((>=& row last-row)
                     (setq maxrow starting-row) ;; fill the maxrow cache
                     row)
     ;; need to do this AFTER last-row check
     (setq height (+& height (get-row-height row)))
     (when (>=& height inside-hei)
                   (return row))))
       (draw-line-indicator ()
               (erase-rectangle *scroll-grid-width* 10 x-offset (-& shaft-y 10))
               ;; make the line counter 1-based
               (draw-string alu-seta *box-border-label-font-no*
                            (elevator-row-string (1+ current-row-no))
                            x-offset (-& shaft-y 10))
               (erase-rectangle *scroll-grid-width* 10
                                x-offset (+& shaft-y shaft-height))
               (draw-string alu-seta *box-border-label-font-no*
                            (elevator-row-string
                             (1+ (ending-row-no current-row-no)))
                            x-offset (+& shaft-y shaft-height)))
       (draw-temp-elevator ()
         (draw-rectangle alu-xor
                               (-& *scroll-grid-width* 2)
                               (+& *scroll-button-height* 2)
             x-offset (1-& elevator-y)))
       (erase-temp-elevator ()
         (draw-rectangle alu-xor
                               (-& *scroll-grid-width* 2)
                               (+& *scroll-button-height* 2)
             x-offset (1-& elevator-y))))
      (if click-only?
    ;; don't have to do tracking, just figure out the row
    (set-scroll-to-actual-row screen-box
            (row-at-row-no ed-box (new-row-no y)))
    ;; draw the grid and track
    (drawing-on-window (*boxer-pane*)
            (with-clipping-inside (x-offset (- shaft-y 10)
                                            *scroll-grid-width* (+ 20 shaft-height))
              (draw-scroll-grid)
              ;; draw the original
              (draw-temp-elevator)
              (draw-line-indicator)
              (with-mouse-tracking ((mouse-x x) (mouse-y y))
                (let ((new (new-row-no mouse-y)))
                  (unless (=& new current-row-no)
                    (erase-temp-elevator)
                    (setq current-row-no new
                          elevator-y (+& (floor (* (/ current-row-no norows)
                                                   shaft-height))
                                         shaft-y))
                    (draw-line-indicator)
                    (draw-temp-elevator))))
              (erase-temp-elevator)
              (erase-rectangle *scroll-grid-width* shaft-height x-offset shaft-y))
            (force-graphics-output)
      ;; actually make the change
      (set-scroll-to-actual-row screen-box
                                      (new-elevator-scrolled-row ed-box
                                                                 current-row-no))
      (set-force-redisplay-infs? screen-box))))))
|#

#|
(defvar *christmas-half-time* 0.5)
(defvar *number-of-christmas-blinks* 3)

(defboxer-command com-christmas-tree ()
  "Lights up the mouse sensitive parts of a box's border"
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (drawing-on-window (*boxer-pane*)
  (let* ((screen-box (point-screen-box))
   (box-type (box-type screen-box))
   (resize-backing-store (allocate-backing-store
        *mouse-resize-corner-bitmap*))
   (toggle-view-backing-store (allocate-backing-store
             *mouse-toggle-view-bitmap*))
   (name-stub-backing-store (allocate-backing-store
           *mouse-name-tab-bitmap*))
   (toggle-type-backing-store (allocate-backing-store
             (if (eq box-type 'data-box)
                 *mouse-doit-toggle-bitmap*
                 *mouse-data-toggle-bitmap*))))
    (flet ((grab-back (store x y)
          (bitblt-from-screen alu-seta
            (offscreen-bitmap-width store)
            (offscreen-bitmap-height store)
            store x y 0 0))
     (stamp-icon (icon x y)
           (bitblt-to-screen alu-seta
           (offscreen-bitmap-width icon)
           (offscreen-bitmap-height icon)
           icon 0 0 x y)))
      (multiple-value-bind (box-window-x box-window-y)
        (xy-position screen-box)
      (multiple-value-bind (left top right bottom)
    (box-borders-widths box-type screen-box)
  (declare (ignore top))
  (multiple-value-bind (delta-x delta-y)
      (box-borders-offsets box-type screen-box)
    (let ((resize-x (-& (+& box-window-x (screen-obj-wid screen-box))
            right))
    (resize-y (-& (+& box-window-y (screen-obj-hei screen-box))
            bottom))
    (toggle-view-x (+& box-window-x delta-x))
    (toggle-view-y (+& box-window-y delta-y
           (box-borders-cached-name-tab-height
            (box-type screen-box) screen-box)))
    (toggle-type-x (+& box-window-x left))
    (toggle-type-y (-& (+& box-window-y (screen-obj-hei screen-box))
           bottom)))
      ;; first grab underlying areas
      (grab-back resize-backing-store resize-x resize-y)
      (grab-back toggle-view-backing-store toggle-view-x toggle-view-y)
      (when (fast-memq box-type '(data-box 'doit-box))
        (grab-back toggle-type-backing-store toggle-type-x toggle-type-y))
      (with-multiple-execution
    (dotimes (i *number-of-christmas-blinks*)
      (stamp-icon (let ((z (mod i 3)))
        (cond ((=& z 0) *mouse-resize-corner-bitmap*)
              ((=& z 1) *mouse-expand-corner-bitmap*)
              (t *mouse-shrink-corner-bitmap*)))
            resize-x resize-y)
      (stamp-icon *mouse-toggle-view-bitmap* toggle-view-x toggle-view-y)
      (when (fast-memq box-type '(data-box 'doit-box))
        (stamp-icon (if (eq box-type 'data-box)
            *mouse-doit-toggle-bitmap*
            *mouse-data-toggle-bitmap*)
        toggle-type-x toggle-type-y))
                  (force-graphics-output)
      (sleep *christmas-half-time*)
      (stamp-icon resize-backing-store resize-x resize-y)
      (stamp-icon toggle-view-backing-store toggle-view-x toggle-view-y)
      (when (fast-memq box-type '(data-box 'doit-box))
        (stamp-icon toggle-type-backing-store
        toggle-type-x toggle-type-y))
                  (force-graphics-output)
      (sleep *christmas-half-time*))))))))
    ;; deallocate the backing stores
    (deallocate-backing-store *mouse-resize-corner-bitmap*
            resize-backing-store)
    (deallocate-backing-store *mouse-toggle-view-bitmap*
            toggle-view-backing-store)
    (deallocate-backing-store *mouse-name-tab-bitmap*
            name-stub-backing-store)
    (deallocate-backing-store (if (eq box-type 'data-box)
          *mouse-doit-toggle-bitmap*
          *mouse-data-toggle-bitmap*)
            toggle-type-backing-store)))
  boxer-eval::*novalue*)
|#

;;;;
;;;; FILE: comsa.lisp
;;;;

#| ;; old version
(defboxer-command filling-com-space ()
  "check margin, com-space, com-return, or format-line"
  (set-a-fill-margin (point-box))
  (if (equal (/ (auto-margin) (cha-wid #\a)) (bp-cha-no *point*))
      (com-return)
      (if (> (/ (auto-margin) (cha-wid #\a)) (bp-cha-no *point*))
          (nonfilling-com-space)
          (format-line))))
|#

;; it doesn't look like anyone uses this AND it conflicts with the name of a method
;(defun delete-cha (bp &optional (force-bp-type nil))
;  (action-at-bp-internal
;    (let* ((row (bp-row bp))
;	   (row-length-in-chas (length-in-chas row))
;	   (cha-no (bp-cha-no bp)))
;      (cond ((< cha-no row-length-in-chas)
;	     (delete-cha-at-cha-no row cha-no))
;	    ((next-row row)
;	     (let* ((box (bp-box bp))
;		    (row-row-no (row-row-no box row))
;		    (row-next-row (row-at-row-no box (+ row-row-no 1))))
;	       (delete-row-at-row-no box (+ row-row-no 1))
;	       (insert-row-chas-at-cha-no row row-next-row row-length-in-chas)))))))

;;old stuff
;(DEFUN RUBOUT-OVER-VALUES (BP DIRECTION DELIMITER-CHAS)
;  (LET ((NOT-FIRST-CHA? NIL))
;    (MAP-OVER-CHAS-IN-LINE (BP DIRECTION)
;      (LET ((DELIMITER-CHA? (char-member cha delimiter-chas))
;	    (FORCE-BP-TYPE ':MOVING))
;	(COND ((AND (NULL CHA)(NULL NEXT-OR-PREVIOUS-ROW));end/beginning of box
;	       (RETURN (VALUES ROW CHA-NO)))
;	      ((AND NOT-FIRST-CHA? (NULL CHA))      ;end/beginning of the line
;	       (RETURN (VALUES ROW CHA-NO)))
;	      ((AND NOT-FIRST-CHA? DELIMITER-CHA?)           ;end of the word
;	       (RETURN (VALUES ROW CHA-NO)))
;	      ((NOT DELIMITER-CHA?)                          ;beginning of word
;	       (SETQ NOT-FIRST-CHA? T)
;	       (ACTION-AT-BP-INTERNAL
;		 (kill-buffer-push (cha-at-cha-no row (1- cha-no)) ':backward)
;		 (DELETE-CHA-AT-CHA-NO ROW (1- CHA-NO))))
;	      (T                                    ;delimiter chas before word
;	       (ACTION-AT-BP-INTERNAL
;		 (kill-buffer-push (cha-at-cha-no row (1- cha-no)) ':backward)
;		 (DELETE-CHA-AT-CHA-NO ROW (1- CHA-NO)))))))))

;;old stuff
;(DEFUN DELETE-OVER-VALUES (BP DELIMITER-CHAS)
;  (DO* ((ROW (BP-ROW BP) ROW)
;	(NEXT-ROW (UNLESS (NULL ROW) (NEXT-ROW ROW))
;		  (UNLESS (NULL ROW) (NEXT-ROW ROW)))
;	(CHA-NO (BP-CHA-NO BP)
;		(BP-CHA-NO BP))
;	(CHA (CHA-AT-CHA-NO ROW CHA-NO)
;	     (CHA-AT-CHA-NO ROW CHA-NO))
;	(NOT-FIRST-CHA?))
;       (NIL)
;    (COND ((AND (NULL NOT-FIRST-CHA?)
;		(NULL CHA)
;		(NOT-NULL NEXT-ROW))
;	   (SETQ ROW NEXT-ROW
;		 CHA-NO 0))
;	  (T (LET ((DELIMITER-CHA? (char-member cha delimiter-chas))
;		   (FORCE-BP-TYPE ':MOVING))
;	       (COND ((AND (NULL CHA) (NULL NEXT-ROW));end/beginning of the box
;		      (RETURN (VALUES ROW CHA-NO)))
;		     ((AND NOT-FIRST-CHA? (NULL CHA));end/beginning of the line
;		      (RETURN (VALUES ROW CHA-NO)))
;		     ((AND NOT-FIRST-CHA? DELIMITER-CHA?)  ;end of the word
;		      (RETURN (VALUES ROW CHA-NO)))
;		     ((NOT DELIMITER-CHA?)                 ;beginning of word
;		      (SETQ NOT-FIRST-CHA? T)
;		      (ACTION-AT-BP-INTERNAL
;			(kill-buffer-push (cha-at-cha-no row cha-no) ':forward)
;			(DELETE-CHA-AT-CHA-NO ROW CHA-NO )))
;		     (T                            ;delimiter chas before word
;		      (ACTION-AT-BP-INTERNAL
;			(kill-buffer-push (cha-at-cha-no row cha-no) ':forward)
;			(DELETE-CHA-AT-CHA-NO ROW CHA-NO)))))))))

#| ;; the old character based place naming scheme
  (status-line-display 'boxer-editor-error "Type a character to name place:")
  (multiple-value-bind (char bits)
      (get-character-input *boxer-pane* :plain-char-wanted? t)
    #-mcl (declare (ignore bits))
    (let ((existing (assoc char *recorded-place-alist* :test #'char-equal)))
      (cond ((editor-abort-char? char #+mcl bits)
             (boxer-editor-warning "Cancelled !"))
      ((null existing)
             (status-line-display 'boxer-editor-error
                (format nil "This place recorded as register:~C"
                                          (char-upcase char)))
       (push (cons char (%record-current-place)) *recorded-place-alist*))
      (t
             (status-line-display 'boxer-editor-error
                (format nil "Register ~C changed to this place"
                                          (char-upcase char)))
       (setf (cdr existing) (%record-current-place))))))
|#

#|
  (status-line-display 'boxer-editor-error
                       "Type a character to name place register:")
  (multiple-value-bind (char bits)
      (get-character-input *boxer-pane* :plain-char-wanted? t)
    #-mcl (declare (ignore bits))
    (let ((existing (assoc char *recorded-place-alist* :test #'char-equal)))
      (cond ((editor-abort-char? char #+mcl bits)
             (boxer-editor-warning "Cancelled !"))
      ((null existing)
       (boxer-editor-warning "Register ~C is undefined" (char-upcase char)))
      (t
             (status-line-display 'boxer-editor-error
                (format nil "Moving to Place in Register ~C"
                                          (char-upcase char)))
             (let ((status (move-to-place (cdr existing))))
               (when (eq status ':error)
                 (setq *recorded-place-alist*
                       (fast-delq existing *recorded-place-alist*))
                 (boxer-editor-warning "Place ~C is no longer in the editor"
                                       char)))))))
|#

#|
  (status-line-display 'boxer-editor-error "Register to Point:")
  (multiple-value-bind (char bits)
      (get-character-input *boxer-pane*)
    #-mcl (declare (ignore bits))
    (let ((existing (assoc char *recorded-place-alist* :test #'char-equal)))
      (status-line-display 'boxer-editor-error
         (format nil "Register to Point: ~C" char))
      (cond ((editor-abort-char? char #+mcl bits)
             (boxer-editor-warning "Cancelled !"))
      ((null existing)
       (boxer-editor-warning "No place in Register ~C !" char))
      (t (setq *recorded-place-alist*
                     (fast-delq existing *recorded-place-alist*))))))
|#

;;;;
;;;; FILE: comsb.lisp
;;;;

;(defboxer-COMMAND COM-UNBOXIFY ()
;  "unboxes the point-box"
;  (reset-editor-numeric-arg)
; (with-multiple-execution
;  (unless (eq (point-box) *initial-box*)
;    (let* ((bp-1 (make-bp :moving))
;	   (bp-2 (make-bp :moving))
;	   (doomed-region (make-editor-region bp-1  bp-2)))
;
;    ;;; bracket the box
;      (move-bp bp-1
;	       (BOX-FIRST-BP-VALUES (superior-box (bp-row *point*))))
;      (move-bp bp-2
;	       (BOX-LAST-BP-VALUES (superior-box (bp-row *point*))))
;      (setf (interval-box doomed-region) (point-box))
;      ;;; kill region ,exit,rubout box ,yank stuff
;      (kill-region doomed-region)
;
;      (LET ((BOX (BOX-SCREEN-POINT-IS-IN)))
;	(UNLESS (EQ BOX *INITIAL-BOX*)
;	  (EXIT BOX (SUPERIOR-SCREEN-BOX (SCREEN-BOX-POINT-IS-IN))
;		(SUPERIOR-BOX BOX) t)))
;      (add-redisplay-clue (bp-row *point*) ':insert)
;      (delete-cha-at-cha-no (point-row) (1- (point-cha-no)))
;      (yank-region *point*  doomed-region)
;      (add-redisplay-clue (bp-row *point*) ':insert)
;
;      ;;; cleanup bps, editor-arg
;      (delete-bp (bp-row bp-2) bp-2)
;      (delete-bp (bp-row bp-1) bp-1)
;      (reset-editor-numeric-arg)
;
;    )))
;  boxer-eval::*novalue*)
;
;

;;;;;;;;;;;;;;;; THIS HAS BEEN SUPERCEDED BY UNBOX-BOX
;;;;;;;;;;;;;;;; To make unbox-region and unbox-point-box things do the same thing
;;;; A cheap hack. Kill the box's contents, insert it into the above
;;;; hierarchy, and kill the box.
;(defun com-unboxify-point-box ()
;  "unboxify the point-box"
;  (with-multiple-execution
;      (unless (eq (point-box) *initial-box*)
;	(let* ((bp-1 (make-bp :moving))
;	       (bp-2 (make-bp :moving))
;	       (doomed-region (make-editor-region bp-1  bp-2)))
;	  (move-bp bp-1
;		   (BOX-FIRST-BP-VALUES (superior-box (bp-row *point*))))
;	  (move-bp bp-2
;		   (BOX-LAST-BP-VALUES (superior-box (bp-row *point*))))
;	  (setf (interval-box doomed-region) (point-box))
;	  (kill-region doomed-region)
;	  (LET ((BOX (BOX-SCREEN-POINT-IS-IN)))
;	    (UNLESS (EQ BOX *INITIAL-BOX*)
;	      (EXIT BOX (SUPERIOR-SCREEN-BOX (SCREEN-BOX-POINT-IS-IN))
;		    (SUPERIOR-BOX BOX) t)))
;	  (add-redisplay-clue (bp-row *point*) ':insert)
;	  (delete-cha-at-cha-no (point-row) (1- (point-cha-no)))
;	  (yank-region *point*  doomed-region)
;	  (add-redisplay-clue (bp-row *point*) ':insert)
;	  (delete-bp (bp-row bp-2) bp-2)
;	  (delete-bp (bp-row bp-1) bp-1)
;	  ))))

#| ;; old version
(defboxer-command com-fill-rows (&optional (region nil))
  "fill the region"
  (let* ((region-to-fill (if region region
			     (or *region-being-defined*
				 (get-current-region)))))
    (unless (null region-to-fill)
      (let ((sbp (interval-start-bp region-to-fill))
	    (ebp (interval-stop-bp region-to-fill)))
	(cond
	  ((eql (superior-box (bp-row sbp))
		(superior-box (bp-row ebp)))
	   (progn
	     (set-a-fill-margin (superior-box (bp-row ebp)))
	     (if (bp-< sbp ebp)
		 (fill-stuff sbp ebp)
		 (fill-stuff ebp sbp))))
	  (t (boxer-editor-error "Endpoints for fill must be in the same box")))
	)
      ))
  (mark-file-box-dirty (point-row))
  (reset-region)
  boxer-eval::*novalue*)

#| ;; old old version
(defboxer-command COM-FILL-BOX ()
  "fill a given boxes rows"
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (let* ((bp-1 (make-bp :moving))
	 (bp-2 (make-bp :moving))
	 (region (make-editor-region bp-1  bp-2)))
    (move-bp bp-1
	     (box-first-bp-values (superior-box (bp-row *point*))))
    (move-bp bp-2
	     (box-last-bp-values (superior-box (bp-row *point*))))
    (setf (interval-box region) (point-box))
    (com-fill-rows region)
    (delete-bp (bp-row bp-2) bp-2)
    (delete-bp (bp-row bp-1) bp-1)
    boxer-eval::*novalue*))
|#

(defboxer-command com-fill-box (&optional (box (point-box)))
  "Reformats the box's contents"
  (reset-region)
  (when (display-style-fixed-wid (display-style-list box))
    (multiple-value-bind (start-row start-cha-no)
        (box-first-bp-values box)
      (multiple-value-bind (stop-row stop-cha-no)
          (box-last-bp-values box)
        (let ((start-bp (make-bp ':fixed)) (stop-bp  (make-bp ':fixed))
              (*auto-fill-margin* (display-style-fixed-wid
                                   (display-style-list box))))
          (setf (bp-row start-bp) start-row (bp-cha-no start-bp) start-cha-no
                (bp-row stop-bp) stop-row (bp-cha-no stop-bp) stop-cha-no)
          (fill-stuff start-bp stop-bp)
          (mark-file-box-dirty (first-inferior-row box))))))
  boxer-eval::*novalue*)



;;;; various functions needed by the fill-rows routine
(defvar *auto-fill-margin* 300)
(defvar *end-row* nil)

(defun auto-margin () *auto-fill-margin*)



(defun set-a-fill-margin (box)
  (let ((sbox (car(screen-objs box))))
    (if sbox
	(setq *auto-fill-margin*
              (- (screen-obj-wid sbox) (vertical-border-width box))))))


;;;; WIDTHS OF CHARACTER/BOXES in pixels
;;;; the width of a thing (box, char)
(defun thing-wid (x)
  (cond ((null x) 0)
	((box? x) (box-wid-in-pixels x))
	(t (cha-wid x))))

;;;; loop through the box, adding up the sum of the cha widths and the borders.
;;;; need to ask ed about the width values and what is what.

(defun box-wid-in-pixels (box)
  (let ((max-wid *minimum-box-wid*))
    (do* ((r (first-inferior-row box) (next-row r))
	  ;(rwid (pix-sum r)(pix-sum r))
	  ;(dummy (if (> rwid max-wid) (setq max-wid rwid))
	  ;       (if (> rwid max-wid) (setq max-wid rwid)))
	  )
	  ((null (next-row r))))
    (if (car (screen-objs box))
	(multiple-value-bind (l-bord-wid top r-bord-wid bottom)
	    (box-borders-widths
	     (box-type (car (screen-objs box)))
	     (car (screen-objs box)))
	  (declare (ignore top bottom))
	  (+ max-wid l-bord-wid r-bord-wid))
	(+ max-wid 9 9))
    ))


;;;; what is the sum of the pix vals for the given row
(defun pix-sum (row)
    (if (eql nil (cha-at-cha-no row 0))
	0
	(do* ((index 0 (1+ index))
	      (c (cha-at-cha-no row index)
		 (cha-at-cha-no row index))
	      (pc (thing-wid c) (if c (+ pc (thing-wid c)) pc))
	      )
	     ((eql nil (cha-at-cha-no row index))
	      pc))))

(defun diagnose (row)
  (if (null row)
      nil
      (let ((ps (pix-sum row))
	    (am (auto-margin)))
	(cond
	  ((and
	    (< ps am)
	    (eql *end-row* row))
	   :too-small-leave-alone)
	  ((eql ps 0)
	   :empty-row)
	  ((< ps am)
	   :too-small)
	  ((and
	    (> ps am)
	    (eql *end-row* row))
	   :too-big-use-return)
	  ((> ps am)
	   :too-big)
	  ((eql ps am)
	   :just-right)))))


;;;; fill the specified row, return t if changes any rows, else nil
(defun fill-row (row)
      (let ((d (diagnose row)))
	(cond
	  ((null d)  (progn
			(setq *end-row* nil)
			nil))
	  ((eql d :empty-row)
	       	   (progn (erase-row row) t))
	   ((or (eql d :just-right)
		(eql d :too-small-leave-alone))
	    nil)

	  ((eql d :too-small)
	   (progn
	     (if (and (not (eql (next-row row) *end-row*))
		      (blank-row(next-row row)))
		 (erase-row (next-row row)))
	     (add-space-if-necessary row)
	     (let ((numup (where-to-break
			   (next-row row)
			   (bounded-length
			    (next-row row)
			    (- (auto-margin) (pix-sum row))))))
	       (if (numberp numup)
		   (progn
		     (pull-up-chas row numup)
		     (fill-row row)
		     t)
		   nil
		   ))))
	  ((eql d :too-big)
	   (let ((numstaying (where-to-break
			      row
			      (bounded-length row (auto-margin)))))
	     (add-space-if-necessary row)
	     (if (numberp numstaying)
		 (push-down-chas
		  row
		  (- (length-in-chas row)
		     numstaying)
		  ))
	     t))
	  ((eql d :too-big-use-return)
	   (let ((numstaying (where-to-break
			      row
			      (bounded-length row (auto-margin)))))
	     (add-space-if-necessary row)
	     (if (numberp numstaying)
		 (progn
		   (split-rows
		    row
		    numstaying)
		   (setq *end-row* (next-row row))
		   t)
		 nil)
	     )
	   )))
      )





;;;; manipulators
;;;; push the number of chas from row down to the next row
(defun push-down-chas (row num)
  (if (< (length-in-chas row) num) (break "push-down-chas-called with num too big"))
  (if (> num  0)
      (insert-row-chas-at-cha-no (next-row row)
				 (delete-chas-between-cha-nos
				  row
				  (- (length-in-chas row) num)
				  (length-in-chas row))
				 0)
      ))



;;;; pull up chas from the row's next row.
;;;;
(defun pull-up-chas (row num)
  (if (>= (length-in-chas(next-row row)) num)
      (if (> num 0)
	  (insert-row-chas-at-cha-no row
				     (delete-chas-between-cha-nos (next-row row)
								  0
								  num)
				     (length-in-chas row))
	  )
      (break "num too big"))
  )


(defun erase-row (row)
  (delete-row-at-row-no
   (superior-box row)
   (row-row-no (superior-box row) row)
   ))




;;;; add a space on the end of a row
(defun add-space-if-necessary (row)
  (if (or (eql (length-in-chas row) 0)
	  (and
	   (not (eql (cha-at-cha-no row (1- (length-in-chas row)))
		     #\Space))
	   (not (eql (cha-at-cha-no row (1- (length-in-chas row)))
		     #\-))))
      (progn
	(add-redisplay-clue row ':insert)
	(chas-array-insert-cha
	 (chas-array row)
	 (length-in-chas row)
	 #\space))))










;;;; breaking rows apart. where to do it

;;;; used to get a "correct" break at a space or hyphen
;;;; cha-num will be the upperbound of the break
;;;; return the length of the cleanly broken row, not
;;;; longer than cha-num
;;;; returns empty-row if there is no break below cha-num

(defun where-to-break (row cha-num)
  (cond ((= cha-num 0) :empty-row)
	((< (length-in-chas row) (1+ cha-num))
	 (length-in-chas row))
	(t (do* ((index (1- cha-num) (1- index))
		 (cha (cha-at-cha-no row index)(cha-at-cha-no row index))
		 )
		((or (box? cha)
		     (eql cha #\ )
		     (eql cha #\-)
		     (eql cha nil)
		     (eql index 0))
		 (cond
		   ((null cha) (progn  (print "special") cha-num))
		   ((box? cha) (1+ index))
		   ((> index 0) (1+ index))
		   ( t :empty-row)))))))

;;;; what is the greatest upper bound of the length of the row
;;;; that has a pixel count less than pcount
;;;; i.e. (bounded-length (row infinity)) is the length of the row.
;;;; (bounded-length (row 7)) should return 1 if the row's first cha is a
;;;; character


(defun bounded-length (row pcount)
  (if (null row)
      0
      (do* ((index 0 (1+ index))
	    (c (cha-at-cha-no row index)
	       (cha-at-cha-no row index))
	    (pc (thing-wid c)(+ pc (thing-wid c))))
	   ((or (eql nil (cha-at-cha-no row index))
		(> pc pcount)) index)
	)
      ))





;;;; split the row into two, with n-left-on chars in the top row
(defun split-rows (row n-left-on)
  (let ((drow (kill-chas-at-cha-no row n-left-on)))
    (insert-row-at-row-no
     (superior-box row)
     drow
     (1+ (row-row-no
	  (superior-box row)
	  row))
     )))


(defun blank-row (row)
  (cond ((null row) nil)
	(t (eql (cha-at-cha-no row 0) nil))))


;;;; do the filling, with the given rows as the borders

(defun fill-rows (start end)
  (setq *end-row* end)
  (fill-all-but-last-rows start)
  (fill-last-row))

(defun fill-all-but-last-rows (row)
  (if (or (null row) (eql row *end-row*))
      nil
      (progn
	(fill-row row)
	(fill-all-but-last-rows (next-row row)))))


(defun fill-last-row ()
  (if (blank-row *end-row*)
      (erase-row *end-row*)
      (if (fill-row *end-row*) (fill-last-row))))




;;; stuff to strip out spaces in a region

;;; kill spaces between two bp's args: start stop
(defun space-killer-iterator (bp1 bp2)
  (do ((row (bp-row bp1) (next-row row)))
      ((eql row (bp-row bp2))  (space-killer row))
    (space-killer row))
  )

(defun SPACE-KILLER (row)
  "kill xtra spaces on the given row"
  (let ((space-mode t))
    (do* ((index 0 (1+ index))
          (cha (cha-at-cha-no row index) (cha-at-cha-no row index)))
         ((eql index (length-in-chas row)))
      (cond ((eql cha #\Space)
             (if space-mode
               (progn
                 (delete-cha-at-cha-no row index)
                 (setq index (1- index)))
               (setq space-mode t)))
            (t (setq space-mode nil))))))

(defun fill-stuff (bp1 bp2)
  (let ((srow (bp-row bp1)) (erow (bp-row bp2)))
    (space-killer-iterator bp1 bp2)
    (fill-rows srow erow)))

; (defboxer-command com-fill-rows (&optional
;				  (region
;				   (or
;				    *region-being-defined*
;				    (get-current-region))
;				   ))
;   "fill the region"
;   (reset-editor-numeric-arg)
;   (unless (null region)
;     (let ((sbp (interval-start-bp region))
;	   (ebp (interval-stop-bp region)))
;       (cond
;	 ((eql (superior-box (bp-row sbp))
;	       (superior-box (bp-row ebp)))
;	  (progn
;	    (set-a-fill-margin (superior-box (bp-row ebp)))
;	    (if (bp-< sbp ebp)
;		(fill-stuff sbp ebp)
;		(fill-stuff ebp sbp))))
;	 (t
;	  (boxer-editor-error "Endpoints for fill must be in the same box")))
;       )
;     )
;   (reset-region)
;   boxer-eval::*novalue*)
;

|#


;;;;
;;;; FILE: comse.lisp
;;;;

#+mcl
(defboxer-command com-link-to-mac-file ()
  "Make a Link to a non boxer Macintosh file"
  (boxer-editor-message "Choose a file to link to...")
  (ccl::catch-cancel
    (let* ((linkfile (ccl::choose-file-dialog :button-string "Link"))
           (xbox (if (box-file? linkfile)
                     (make-file-box linkfile)
                     (make-xfile-box linkfile))))
      (when (box? xbox) (insert-cha *point* xbox))))
  (clear-editor-message)
  boxer-eval::*novalue*)

;; is (point-box) the right thing ?
#+mcl
(defboxer-command com-edit-mac-link ()
  "Change the file a link points to"
  (boxer-editor-message "Change the link's file...")
  (ccl::catch-cancel (edit-mac-file-ref (point-box)))
  (when (eq :normal (display-style (point-box))) (com-shrink-box))
  boxer-eval::*novalue*)

;;;;
;;;; FILE: comsf.lisp
;;;;

;; from set-font-style
  (cond ((or (null style) (eq style :plain))
         (%set-font-style boxer-font 0))
    (t (let* ((current-style (font-style boxer-font))
              (new-style (case style
                           (:bold (dpb& (if to-on? 1 0)
                                        '#.(byte 1 0) current-style))
                           (:italic (dpb& (if to-on? 1 0)
                                          '#.(byte 1 1) current-style))
                           (:underline (dpb& (if to-on? 1 0)
                                             '#.(byte 1 2) current-style)))))
         (%set-font-style boxer-font new-style))))


#+mcl
(progn
  (defun normal-font? (boxer-font)
    (zerop& (font-style boxer-font)))

  (defun bold-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 1))))

  (defun italic-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 2))))

  (defun underline-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 4))))

  (defun outline-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 8))))

  (defun shadow-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 16))))

  (defun condense-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 32))))

  (defun extend-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 64))))

  ;; like typep for font styles...
  (defun font-stylep (boxer-font style)
    (cond ((or (null style) (eq style :plain))
           (zerop& (font-style boxer-font)))
          (t
           (not (zerop& (logand& (font-style boxer-font)
                                 (case style
                                   (:bold 1) (:italic 2) (:underline 4)
                                   (:outline 8) (:shadow 16)
                                   (:condense 32) (:extend 64))))))))

  (defun font-styles (boxer-font)
    (let ((style-byte (font-style boxer-font))
          (return-styles nil))
      (do* ((pos 1 (ash pos 1))
            (styles '(:bold :italic :underline :outline :shadow :condense :extend)
                    (cdr styles))
            (style (car styles) (car styles)))
          ((null style))
        (unless (zerop& (logand& style-byte pos)) (push style return-styles)))
      (nreverse return-styles)))


  (defun set-font-style (boxer-font style to-on?)
    (cond ((or (null style) (eq style :plain))
           (%set-font-style boxer-font 0))
          (t (let* ((current-style (font-style boxer-font))
                    (new-style (case style
                                   (:bold (dpb& (if to-on? 1 0)
                                                '#.(byte 1 0) current-style))
                                   (:italic (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 1) current-style))
                                   (:underline (dpb& (if to-on? 1 0)
                                                     '#.(byte 1 2) current-style))
                                   (:outline (dpb& (if to-on? 1 0)
                                                   '#.(byte 1 3) current-style))
                                   (:shadow (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 4) current-style))
                                   (:condense (dpb& (if to-on? 1 0)
                                                    '#.(byte 1 5) current-style))
                                   (:extend (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 6) current-style)))))
               (%set-font-style boxer-font new-style)))))
  )

;; like typep for font styles...
(defun font-stylep (boxer-font style)
  (cond ((or (null style) (eq style :plain))
         (zerop& (font-style boxer-font)))
    (t
     (not (zerop& (logand& (font-style boxer-font)
                           (case style
                             (:bold 1) (:italic 2) (:underline 4))))))))

#|
(defmethod change-font-between-cha-nos ((row row) new-font-no
          &optional
          (start-cha-no 0)
          (stop-cha-no (length-in-chas row)))
  (let* ((last-font-no 0)
   (chas-array (slot-value row 'chas-array)))
    (cond ((null (chas-array-fds chas-array))
     ;; there are no Font Descriptors in the row so we can
     ;; just insert new ones without checking
     (if  (=& stop-cha-no (chas-array-active-length chas-array))
    ;; we don't need to insert another FD if we
    ;; are at the end of the row
    (push (make-bfd start-cha-no new-font-no)
          (chas-array-fds chas-array))
    ;; looks like we have to know when to stop
    (setf (chas-array-fds chas-array)
          (list (make-bfd start-cha-no new-font-no)
          (make-bfd stop-cha-no last-font-no)))))
    (t
     ;; looks like we may have to check and (possibly) alter
     ;; the values of intervening Font Descriptors
     (do* ((fds (chas-array-fds chas-array) (cdr fds))
     (next-fds (cdr fds) (cdr fds))
     (current-fd (car fds) (car fds))
     (next-fd (car next-fds) (car next-fds))
     (inside-new-font? nil)
     (bfd-cha-no (if (null inside-new-font?)
         start-cha-no
         stop-cha-no)))
    ((null next-fds) (setf (cdr fds) (list bfd)))
       (cond ((=& bfd-cha-no (bfd-cha-no current-fd))
        ;; if there already is a FD at the right place, bash
        ;; its values rather than splicing a new FD into the list
        (cond ((null inside-new-font?)
         (setq inside-new-font? t)
         (setq last-font-no (bfd-font-no current-fd))
         (setf (bfd-font-no current-fd)
         (logior& (bfd-font-no current-fd)
            (bfd-font-no bfd))))
        (t
         ;; if there is already a FD where we want
         ;; to stop, then we just leave it alone
         (return))))
       ((>& bfd-cha-no (bfd-cha-no current-fd))
        (cond ((null next-fd)

         (setf (cdr fds) (list bfd))
         (return))
        ((<& bfd-cha-no (bfd-cha-no next-fd))
         ;; we are in between 2 FD's so we should splice in here
         (setf (cdr fds) (cons bfd next-fds))
         (return))
        ((=& bfd-cha-no (bfd-cha-no next-fd))
         (setf (bfd-font-no next-fd)
         (logior& (bfd-font-no next-fd) (bfd-font-no bfd)))
         (return))))
       (t
        ;; record the last-font-no
        (setq last-font-no (bfd-font-no current-fd))))))







  (let ((new-bfd (make-bfd start-cha-no new-font-no)))
    ;; first, insert the Font Descriptor
    (insert-bfd row new-bfd)
    ;; now OR the new-font-no into any intervening Font Descriptors
    (dolist (bfd (remaining-bfds row start-cha-no))
      (cond ((>& (bfd-cha-no bfd) stop-cha-no)
       ;; there are still subsequent FD's down the row so we need
       ;; to add a FD which


|#

#|
(DEFBOXER-COMMAND COM-BOLDFACE-FONT-WORD ()
  "Changes the next word to be in boldface. "
  (WITH-MULTIPLE-EXECUTION
    (MOVE-POINT (BP-CHANGE-STYLE-FORWARD-WORD-VALUES *POINT* ':BOLD)))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-BOLDFACE-FONT-CHA ()
  "Change the next character to be in boldface. "
  (WITH-MULTIPLE-EXECUTION
    (LET ((OLD-CHAR (CHA-AT-CHA-NO (POINT-ROW) (POINT-CHA-NO))))
      (CHANGE-CHA-AT-CHA-NO (POINT-ROW)
      (POINT-CHA-NO)
      (MAKE-CHAR OLD-CHAR
           (CHAR-BITS OLD-CHAR) ':BOLD (CHAR-FONT-FAMILY OLD-CHAR)))
      (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-ITALICS-FONT-WORD ()
  "Changes the next word to be in italics. "
  (WITH-MULTIPLE-EXECUTION
    (MOVE-POINT (BP-CHANGE-STYLE-FORWARD-WORD-VALUES *POINT* :ITALIC)))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-ITALICS-FONT-CHA ()
  "Change the next character to be in italics. "
  (WITH-MULTIPLE-EXECUTION
    (LET ((OLD-CHAR (CHA-AT-CHA-NO (POINT-ROW) (POINT-CHA-NO))))
      (CHANGE-CHA-AT-CHA-NO (POINT-ROW)
      (POINT-CHA-NO)
      (MAKE-CHAR OLD-CHAR
           (CHAR-BITS OLD-CHAR) ':ITALIC (CHAR-FONT-FAMILY OLD-CHAR)))
      (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))))
  boxer-eval::*novalue*)


|#

#+mcl
(defvar *macl-standard-font-sizes*)

#+mcl
(defvar *current-macl-font-size* 10)

#+mcl
(defvar *macl-typeface* "courier")

#+mcl
(eval-when (eval load)
  (setq *macl-standard-font-sizes* '(9 10 12 14 18 24))
  (setf (cdr (last *macl-standard-font-sizes*)) *macl-standard-font-sizes*)
  )

#+mcl
(defun next-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (cadr (member *current-macl-font-size*
                                            *macl-standard-font-sizes*)))
        :plain))

#+mcl
(defun larger-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (max *current-macl-font-size*
                                   (cadr (member *current-macl-font-size*
                                                 *macl-standard-font-sizes*))))
        :plain))

#+mcl
(defun smaller-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (min *current-macl-font-size*
                                   (let ((prev (car *macl-standard-font-sizes*))
                                         (here *macl-standard-font-sizes*))
                                     (do* ((where (cdr here) (cdr where))
                                           (size (car where) (car where)))
                                          ((eq where here) prev)
                                       (when (= size *current-macl-font-size*)
                                         (return prev))
                                       (setq prev size)))))
        :plain))

#+mcl
(defun max-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (let ((max 0)
                                    (here *macl-standard-font-sizes*))
                                (do* ((where (cdr here) (cdr where))
                                      (size (car where) (car where)))
                                     ((eq where here) max)
                                  (when (> size max) (setq max size)))))
        :plain))

(defvar *font-size-state-var* 0)

#+mcl
(defboxer-command com-toggle-font-size ()
  "Cycles through the available font sizes for the Boxer Window"
  (bw::reinitialize-font-map (bw::sheet-font-map *boxer-pane*)
                             (next-font-spec))
  boxer-eval::*novalue*)

#-mcl
(defboxer-command com-toggle-font-size ()
  "Cycles through the available font sizes for the Boxer Window"
  (cond ((eql *font-size-state-var* 0)
   (progn
     (setq *font-size-state-var* 1)
     (com-fat)))
  ((eql *font-size-state-var* 1)
   (progn
     (setq *font-size-state-var* 2)
     (com-bloat)))
  (t
   (progn (setq *font-size-state-var* 0)
    (com-nutri-system))))
  boxer-eval::*novalue*)

;;;;; Global Fonts

(defvar *supported-font-sizes*
  #+mcl '(9 10 12 14 18 24) #+lwwin '(6 8 10 12 14 16 18 24))

(defboxer-command COM-NUTRI-SYSTEM ()
  "shrink the fonts"
  ;; mac version just resets *default-font-descriptor*
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size
                                 (reverse *supported-font-sizes*)))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the smallest supported font size"
                                 (car *supported-font-sizes*)))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

(defboxer-command COM-FAT ()
  "make-fonts bigger"
  ;; mac version just resets *default-font-descriptor*
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size *supported-font-sizes*))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the largest supported font size"
                                 (car (last *supported-font-sizes*))))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

(defboxer-command COM-BLOAT ()
  "bloat the fonts"
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size *supported-font-sizes*))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the largest supported font size"
                                 (car (last *supported-font-sizes*))))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

;;; Network stuff

(defboxer-command com-receive-boxer-send ()
  "Inserts box received from a remote Boxer user"
  (mark-file-box-dirty (point-row))
  (receive-boxer-send))

;;;;
;;;; FILE: dataprims.lisp
;;;;

(boxer-eval::defboxer-primitive bu::redirect ((boxer-eval::dont-copy port) (bu::port-to target))
  "retarget the port to the given box"
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::redirect " is no longer available"))
        (t
         (retarget-internal port target)
         boxer-eval::*novalue*)))

;;;;
;;;; FILE: disdcl.lisp
;;;;

(DEFVAR %DRAWING-FONT-MAP NIL
        "Inside of a drawing-on-window, this variable is bound to %drawing-window's
   font-map.")

;;;;
;;;; FILE: disdef.lisp
;;;;

(defvar *screen-boxes-modified* ':toplevel
  "Screen boxes modifed during eval")

;; more stuff for the top-level-eval-wrapper....
(defmacro with-screen-box-modification-tracking (&body body)
  `(let ((*screen-boxes-modified* nil))
     . ,body))

(defun queue-modified-graphics-box (gb)
  (unless (or (eq *screen-boxes-modified* ':toplevel) (eq gb ':no-graphics))
    (dolist (sb (displayed-screen-objs gb))
      (unless (fast-memq sb *screen-boxes-modified*)
        (push sb *screen-boxes-modified*)))))

;;; for systems which buffer graphics
;;; this applies equally to command buffering a la X or
;;; double buffering a la OpenGL, OSX Quickdraw
(defun force-graphics-output ()
  ;; this is the new paradigm, defined in the draw-low- files
  (flush-port-buffer))

(DEFMACRO ALTERING-REGION ((REGION) &BODY BODY)
          (WARN "ALTERING-REGION is obsolete.  Use with-open-blinker instead.")
          `(WITHOUT-INTERRUPTS
            (OPEN-BLINKER ,REGION)
            (PROGN . ,BODY)))

;;;; GRAY PATTERNS

;; the default in the code is *gray* which should be bound to one of
;; the particular grays defined below
;; These are useful for drawing gray areas on the screen.
;;
;; We break these into compile-time definitions and load-type assignments
;; so we don't have to have the window system loaded during compilation.
;;
;; The numeric grays are bound to patterns.  The other grays should
;; be set up to be bound to particular numeric grays

(defvar *gray0*)

(defvar *gray1*)

(defvar *gray2*)

(defvar *gray3*)

(defvar *gray4*)

(defvar *gray5*)

(defvar *filegray* nil)

(defvar *graphicsgray* nil)

;; Note: 0,1 and 5 look good 2,3 and 4 could use some tweaking...

(defun initialize-gray-patterns ()
  (setq *GRAY0* (boxer-window::make-pattern
                 '((1 0 0 0 0 1 0 0 0 0)
                   (0 0 1 0 0 0 0 1 0 0)
                   (0 0 0 0 1 0 0 0 0 1)
                   (0 1 0 0 0 0 1 0 0 0)
                   (0 0 0 1 0 0 0 0 1 0))))
  (setq *GRAY1* (boxer-window::make-pattern
                 '((1 0 0 0 1 0 0 0)
                   (0 1 0 0 0 1 0 0)
                   (0 0 0 1 0 0 0 1)
                   (0 0 1 0 0 0 1 0))))
  (setq *GRAY2* (boxer-window::make-pattern
                 '((1 0 0 0)
                   (0 0 1 0)
                   (0 1 0 0))))
  (setq *GRAY3* (boxer-window::make-pattern
                 '((1 0 0 0 1 0 1 0)
                   (0 1 0 1 0 0 0 1)
                   (1 0 0 0 1 0 1 0)
                   (0 1 0 1 0 0 0 1))))
  (setq *GRAY4* (boxer-window::make-pattern
                 '((1 0 1 0 1 0 1 0)
                   (0 1 0 0 0 1 0 0)
                   (1 0 1 0 1 0 1 0))))
  (setq *GRAY5* (boxer-window::make-pattern
                 '((1 0 1 0 1 0 1 0)
                   (0 1 0 1 0 1 0 1)
                   (1 0 1 0 1 0 1 0)
                   (0 1 0 1 0 1 0 1))))
  ;; finally set up *gray* to be one of the grays we just defined
  (setq *GRAY* *GRAY0*
        *filegray* (boxer-window::make-pattern '((1 1) (1 1)))
        *graphicsgray* *gray1*))

;;;;
;;;; FILE: disply.lisp
;;;;

;; sgithens 2022-04-25 These are only calling a stub function, and hopefully can be done
;; in a less complex fashion...
(defun box-border-zoom-in (new-screen-box window)
  (unless (null *zoom-step-pause-time*)
    (drawing-on-window (window)
                       (when (when (not-null new-screen-box)(visible? new-screen-box))
                         (multiple-value-bind (new-screen-box-wid new-screen-box-hei)
                                              (screen-obj-size new-screen-box)
                                              (multiple-value-bind (new-screen-box-x new-screen-box-y)
                                                                   (xy-position new-screen-box)
                                                                   (multiple-value-bind (outermost-screen-box-wid
                                                                                         outermost-screen-box-hei)
                                                                                        (outermost-screen-box-size)
                                                                                        (multiple-value-bind (outermost-screen-box-x
                                                                                                              outermost-screen-box-y)
                                                                                                             (outermost-screen-box-position)
                                                                                                             (box-borders-zoom
                                                                                                              (class-name (class-of (screen-obj-actual-obj new-screen-box)))
                                                                                                              new-screen-box
                                                                                                              outermost-screen-box-wid outermost-screen-box-hei
                                                                                                              new-screen-box-wid new-screen-box-hei
                                                                                                              outermost-screen-box-x outermost-screen-box-y
                                                                                                              new-screen-box-x new-screen-box-y
                                                                                                              20.)))))))))

(defun box-border-zoom-out (old-screen-box window)
  (unless (null *zoom-step-pause-time*)
    (drawing-on-window (window)
                       (when (when (not-null old-screen-box)(visible? old-screen-box))
                         (multiple-value-bind (old-screen-box-wid old-screen-box-hei)
                                              (screen-obj-size old-screen-box)
                                              (multiple-value-bind (old-screen-box-x old-screen-box-y)
                                                                   (xy-position old-screen-box)
                                                                   (multiple-value-bind (outermost-screen-box-wid
                                                                                         outermost-screen-box-hei)
                                                                                        (outermost-screen-box-size)
                                                                                        (multiple-value-bind (outermost-screen-box-x
                                                                                                              outermost-screen-box-y)
                                                                                                             (outermost-screen-box-position)
                                                                                                             (box-borders-zoom
                                                                                                              (class-name (class-of (screen-obj-actual-obj old-screen-box)))
                                                                                                              old-screen-box
                                                                                                              old-screen-box-wid old-screen-box-hei
                                                                                                              outermost-screen-box-wid outermost-screen-box-hei
                                                                                                              old-screen-box-x old-screen-box-y
                                                                                                              outermost-screen-box-x outermost-screen-box-y
                                                                                                              16.)))))))))

#|

(defun screen-object-new-width (screen-object)
  (when screen-object
    (if (screen-cha? screen-object)
      (cha-wid screen-object)
      (screen-obj-new-wid screen-object))))

(defun screen-object-new-height (screen-object)
  (when screen-object
    (if (screen-cha? screen-object)
      (cha-hei)
      (screen-obj-new-hei screen-object))))
|#

;; These don't seem to be used in the new OpenGL redisplay...
#|
(defun move-screen-rows (sv from-row-no delta-x delta-y &optional to-row-no no-draw?)
  (unless (>=& from-row-no (storage-vector-active-length  sv))
    (multiple-value-bind (x-offset y-offset)
                         (screen-obj-offsets (sv-nth from-row-no sv))
                         (let ((wid 0) (hei 0))
                           (do-vector-contents (screen-row sv :start from-row-no :stop to-row-no)
                             (setq wid (max wid (screen-obj-wid screen-row))
                                   hei (+  hei (screen-obj-hei screen-row)))
                             (incf (screen-obj-x-offset screen-row) delta-x)
                             (incf (screen-obj-y-offset screen-row) delta-y))
                           ;; finally do the actual moving
                           (when (null no-draw?)
                             (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))))))

(defun move-screen-chas (sv from-cha-no delta-x delta-y &optional to-cha-no)
  (multiple-value-bind (x-offset y-offset)
                       (screen-obj-offsets (sv-nth from-cha-no sv))
                       (let ((wid 0) (hei 0))
                         (do-screen-chas-with-font-info (screen-cha sv
                                                                    :start from-cha-no
                                                                    :stop to-cha-no)
                           (setq wid (+ wid (screen-object-width screen-cha))
                                 hei (max  hei (screen-object-height screen-cha)))
                           (unless (screen-cha? screen-cha)
                             (incf (screen-obj-x-offset screen-cha) delta-x)
                             (incf (screen-obj-y-offset screen-cha) delta-y)))
                         ;; finally do the actual moving
                         (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))))

;;; this is used in pass-2 to blit inferiors over to make room for character
;;; insertions, we can't use move-screen-chas because the chas we want to move
;;; no longer correspond to the cha-nos because the screen structure has been
;;; patched up in pass-1.  We iterate ONLY to look for screen-boxes so that
;;; their offsets can be updated
(defun slide-screen-chas (sv from-cha-no delta-cha-no
                             wid hei x-offset y-offset delta-x delta-y
                             &optional no-drawing fds)
  (unless (>=& (+& from-cha-no delta-cha-no)
               (storage-vector-active-length sv))
    (cond ((and (null wid) (not no-drawing))
           ;; we need to calculate the width from the chas we are about
           ;; to blit as well as adjust the offsets of any screen-boxes
           ;; that happen to be in the row
           (setq wid 0)
           (do-screen-chas-with-font-info (screen-cha
                                           sv
                                           :start (+& from-cha-no
                                                      delta-cha-no)
                                           :font-descriptors fds)
             (incf& wid (screen-object-width screen-cha))
             (unless (screen-cha? screen-cha)
               (incf& (screen-obj-x-offset screen-cha) delta-x))))
      (t
       ;; just update any offsets of boxes
       (do-vector-contents (screen-cha sv :start (+& from-cha-no
                                                     delta-cha-no))
         (unless (screen-cha? screen-cha)
           (incf& (screen-obj-x-offset screen-cha) delta-x)))))
    (unless no-drawing
      (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))))

(defun move-screen-obj (screen-obj delta-x delta-y &optional no-drawing)
  (when (not-null screen-obj)
    ;; sgithens TODO (check-screen-obj-arg screen-obj)
    (multiple-value-bind (wid hei)
                         (screen-obj-size screen-obj)
                         (multiple-value-bind (x-offset y-offset)
                                              (screen-obj-offsets screen-obj)
                                              (unless no-drawing
                                                (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))
                                              (incf (screen-obj-x-offset screen-obj) delta-x)
                                              (incf (screen-obj-y-offset screen-obj) delta-y)))))

(DEFUN MOVE-GRAPHICS-SHEET (GRAPHICS-SCREEN-SHEET DELTA-X DELTA-Y)
       (WHEN (NOT-NULL GRAPHICS-SCREEN-SHEET)
             (CHECK-GRAPHICS-SCREEN-SHEET-ARG GRAPHICS-SCREEN-SHEET)
             (LET* ((GRAPHICS-SHEET (GRAPHICS-SCREEN-SHEET-ACTUAL-OBJ
                                     GRAPHICS-SCREEN-SHEET))
                    (WID (GRAPHICS-SHEET-DRAW-WID GRAPHICS-SHEET))
                    (HEI (GRAPHICS-SHEET-DRAW-HEI GRAPHICS-SHEET))
                    (X-OFFSET (GRAPHICS-SCREEN-SHEET-X-OFFSET GRAPHICS-SCREEN-SHEET))
                    (Y-OFFSET (GRAPHICS-SCREEN-SHEET-Y-OFFSET GRAPHICS-SCREEN-SHEET)))
                   (BITBLT-MOVE-REGION WID HEI X-OFFSET Y-OFFSET DELTA-X DELTA-Y)
                   (INCF (GRAPHICS-SCREEN-SHEET-X-OFFSET GRAPHICS-SCREEN-SHEET) DELTA-X)
                   (INCF (GRAPHICS-SCREEN-SHEET-Y-OFFSET GRAPHICS-SCREEN-SHEET) DELTA-Y))))

(defun move-inferior-screen-objs (inferiors delta-x delta-y)
  (cond ((null inferiors))
    ((graphics-screen-sheet? inferiors)
     (move-graphics-sheet inferiors delta-x delta-y))
    ((screen-row? (sv-nth 0 inferiors))
     (move-screen-rows inferiors 0 delta-x delta-y))
    ((or (screen-cha? (sv-nth 0 inferiors))
         (screen-box? (sv-nth 0 inferiors)))
     (move-screen-chas inferiors 0 delta-x delta-y))
    ((screen-obj? inferiors)
     (move-screen-obj inferiors delta-x delta-y))
    (t
     (barf "Don't know how to move inferior screen object(s), ~S"
           inferiors))))

(DEFUN GRAY-SIZE-AND-OFFSETS (SCREEN-BOX)
       (let ((box-type (slot-value screen-box 'box-type)))
         (MULTIPLE-VALUE-BIND (OUTER-WID OUTER-HEI)
                              (box-borders-minimum-size box-type screen-box)
                              (MULTIPLE-VALUE-BIND (IL IT IR IB)
                                                   (box-borders-widths box-type screen-box)
                                                   (VALUES (- OUTER-WID IL IR) (- OUTER-HEI IT IB) IL IT)))))

(DEFUN MOVE-GRAY-REGION (SCREEN-BOX DELTA-X DELTA-Y)
       (MULTIPLE-VALUE-BIND (GRAY-WID GRAY-HEI GRAY-X GRAY-Y)
                            (GRAY-SIZE-AND-OFFSETS SCREEN-BOX)
                            (BITBLT-MOVE-REGION GRAY-WID GRAY-HEI GRAY-X GRAY-Y DELTA-X DELTA-Y)))

|#

;; #-lispworks6
;; (DEFUN SET-OUTERMOST-SCREEN-BOX (NEW-OUTERMOST-SCREEN-BOX
;; 				 &OPTIONAL (WINDOW *BOXER-PANE*))
;;   (WITHOUT-INTERRUPTS		      ;keep the mouse process from looking at
;;     (REDISPLAYING-WINDOW (WINDOW)     ;the screen when it is in a munged state
;;       (UNLESS (EQ NEW-OUTERMOST-SCREEN-BOX *OUTERMOST-SCREEN-BOX*)
;; 	(DECONFIGURE-SCREEN-BOX-TO-BE-OUTERMOST-BOX *OUTERMOST-SCREEN-BOX*
;; 						    WINDOW)
;; 	(CONFIGURE-SCREEN-BOX-TO-BE-OUTERMOST-BOX NEW-OUTERMOST-SCREEN-BOX
;; 						  WINDOW)
;; 	(ERASE-SCREEN-OBJ *OUTERMOST-SCREEN-BOX*)
;; 	(SETQ *OUTERMOST-SCREEN-BOX* NEW-OUTERMOST-SCREEN-BOX)))
;;     (SETQ *OUTERMOST-SCREEN-BOX* (OUTERMOST-SCREEN-BOX)) ; why ??
;;     (LET ((*COMPLETE-REDISPLAY-IN-PROGRESS?* T)
;; 	  (OLD-SCREEN-ROW (UNLESS (NULL NEW-OUTERMOST-SCREEN-BOX)
;; 			    (SCREEN-ROW NEW-OUTERMOST-SCREEN-BOX))))
;;       (WHEN (SCREEN-ROW? OLD-SCREEN-ROW)
;; 	;; we need to break up the screen-structure
;; 	(KILL-SCREEN-CHAS-FROM OLD-SCREEN-ROW 0)
;; 	(if (fast-memq (superior old-screen-row) *outermost-screen-box-stack*)
;; 	    (deallocate-inferiors (superior old-screen-row))
;; 	    (deallocate-self (superior old-screen-row))))
;;       (repaint-window window))))


#|

(defmethod xy-position ((self screen-obj))
  (multiple-value-bind (superior-x-off superior-y-off)
                       (cond ((outermost-screen-box? self)
                              (values 0 0))
                         (t
                          (xy-position (superior self))))
                       (values (+ superior-x-off (screen-obj-x-offset self))
                               (+ superior-y-off (screen-obj-y-offset self)))))
|#

;;; The OLD cursor tracker

#|
(defun bp-positions (bp)
  (check-bp-arg bp)
  (let ((box (bp-box bp))
        (row (bp-row bp))
        (screen-box (bp-screen-box bp)))
    (COND ((NULL BOX) NIL)
          ((name-row? row)
           (screen-box-name-row-bp-position screen-box row))
          ((eq ':shrunk (display-style screen-box))
           (screen-box-first-bp-position screen-box))
          ((NULL (CURRENT-SCREEN-ROW ROW))
           (SCREEN-BOX-LAST-BP-POSITION SCREEN-BOX))
          (t
           (row-point-position (current-screen-row row) screen-box)))))

(defun screen-box-first-bp-position (screen-box)
  (multiple-value-bind (x y)
                       (xy-position screen-box)
                       (multiple-value-bind (il it)
                                            (box-borders-widths (slot-value screen-box 'box-type) screen-box)
                                            (cons (+ x il) (+ y it)))))

(DEFUN SCREEN-BOX-LAST-BP-POSITION (SCREEN-BOX)
       (MULTIPLE-VALUE-BIND (X Y)
                            (XY-POSITION SCREEN-BOX)
                            (CONS (+ X (SCREEN-OBJ-WID SCREEN-BOX))
                                  (- (+ Y (SCREEN-OBJ-HEI SCREEN-BOX)) *MINIMUM-CURSOR-HEIGHT*))))

(defun screen-box-name-row-bp-position  (screen-box name-row)
  (declare (ignore name-row))
  (let ((cha-no (bp-cha-no *point*)))
    (multiple-value-bind (x y)
                         (xy-position screen-box)
                         (multiple-value-bind (tab-x tab-y)
                                              (box-borders-tab-position (slot-value screen-box 'box-type)
                                                                        screen-box x y cha-no)
                                              (cons tab-x tab-y)))))

(DEFUN ROW-POINT-POSITION (SCREEN-ROW &optional screen-box)
       (LET* ((ROW (SCREEN-OBJ-ACTUAL-OBJ SCREEN-ROW))
              (LENGTH-IN-CHAS (LENGTH-IN-CHAS ROW))
              (CHA-NO (BP-CHA-NO *POINT*)))
             (COND ((NULL (BP-SCREEN-BOX *POINT*))
                    (BARF NIL "Lost the current Screen Box"))
                   ((>= CHA-NO LENGTH-IN-CHAS)
                    (END-OF-ROW-POINT-LOCATION SCREEN-ROW))
                   (T (INSIDE-OF-ROW-POINT-LOCATION SCREEN-ROW CHA-NO)))))

(DEFUN END-OF-ROW-POINT-LOCATION (SCREEN-ROW)
       (MULTIPLE-VALUE-BIND (SCREEN-ROW-X SCREEN-ROW-Y)
                            (XY-POSITION SCREEN-ROW)
                            (CONS (+ SCREEN-ROW-X (SCREEN-OBJ-WID SCREEN-ROW)) SCREEN-ROW-Y)))

(DEFUN INSIDE-OF-ROW-POINT-LOCATION (SCREEN-ROW CHA-NO)
       (MULTIPLE-VALUE-BIND (SCREEN-ROW-X SCREEN-ROW-Y)
                            (XY-POSITION SCREEN-ROW)
                            (CONS (+ SCREEN-ROW-X
                                     (X-COORDINATE-OF-CHA-NO SCREEN-ROW CHA-NO)) SCREEN-ROW-Y)))

(DEFUN X-COORDINATE-OF-CHA-NO (ROW CHA-NO &AUX(X-COORD 0))
       (DO* ((INDEX 0 (+ INDEX 1))
             (CHA (SCREEN-CHA-AT-CHA-NO ROW INDEX)
                  (SCREEN-CHA-AT-CHA-NO ROW INDEX)))
            ((OR (NULL CHA)(= INDEX CHA-NO)) X-COORD)
            (SETQ X-COORD (+ X-COORD (SCREEN-OBJECT-WIDTH CHA)))))
|#

;;;;
;;;; FILE: draw-high-common.lisp
;;;;

;; sgithens 2022-02-24 Some fairly historic comments about clipping from the top
;; header comments
    ;;;;   Only the primitives and macros which do not respect clipping are
    ;;;;   in this file.  The files draw-high-software/hardware-clip.lisp
    ;;;;   contain the low level functions and macros (predominately used
    ;;;;   in the redisplay) which pay attention to the clipping state.

;; sgithens 2022-02-24 Removing these commented out lines from this macro:
(defmacro drawing-on-window-bootstrap-clipping-and-scaling ((x y wid hei) &body body)
  `(let* ((%origin-x-offset ,x) (%origin-y-offset ,y)
          ;; absolute clipping parameters
          (%clip-lef ,x) (%clip-top ,y)
    (%clip-rig (+& %clip-lef ,wid)) (%clip-bot (+& %clip-top ,hei))
          ;; relative clipping parameters
          (%local-clip-lef 0)    (%local-clip-top 0)
          (%local-clip-rig ,wid) (%local-clip-bot ,hei))
     %clip-rig %clip-bot %origin-x-offset %origin-y-offset ;bound but never...
     %local-clip-lef %local-clip-top %local-clip-rig %local-clip-bot
;     ;; **** since we are letting the hardware do the clipping, be sure
;     ;; to include the forms that invoke the hardware
;     (unwind-protect
;         (progn (window-system-dependent-set-origin %origin-x-offset
;                                                    %origin-y-offset)
                ,@body))
;      ;; return to some canonical state
;       (window-system-dependent-set-origin 0 0))))


;; useful for debugging erase-rectangle lossage
(defun flash-rectangle (w h x y)
  (dotimes (i 6)
    (%draw-rectangle w h x y)
    (sleep .1)))

;;; WITH-FONT-MAP-BOUND is meant to be used by all those functions
;;; (like BOX-BORDER-FN's that have to be called in an environment where the
;;; font map is supposed to be bound but nothing else (like all those
;;; wonderful drawing type things and stuff) needs to be bound

(defmacro with-font-map-bound ((window) &body body)
  `(let ((%drawing-font-map (sheet-font-map ,window)))
     %drawing-font-map				;bound but never used etc.
     . ,body))

; sgithens 2021-11-09 I don't believe the below are used anymore.

;;; +++ maybe these are supposed to be the same, maybe not
(defvar char-bits-limit 4096)

;; Support for displaying control characters
(DEFVAR *CONTROL-CHARACTER-PREFIX-TABLE* (MAKE-ARRAY CHAR-BITS-LIMIT
						     :ELEMENT-TYPE 'CHARACTER
						     :INITIAL-ELEMENT #\^))

(DEFUN CONTROL-CHARACTER-DISPLAY-PREFIX (BITS)
  (AREF *CONTROL-CHARACTER-PREFIX-TABLE* BITS))

;;;;
;;;; FILE: draw-high-hardware-clip.lisp
;;;;

;; sgithens 2022-02-24
;; Preserving top level comments from draw-high-hardware-clip.lisp before merging it
;; with draw-high-common.lisp

;;;;     This file contains the low level drawing primitives for the REDISPLAY
;;;;     which are machine independent but expect to do any clipping in software.
;;;;     The clipping calculations are done BEFORE any drawing and only unclipped
;;;;     parts are actually drawn.
;;;;
;;;;     The complement of this file is the the draw-high-software-clipping.lisp
;;;;
;;;;     All window coordinate parameters in this file are "local".  That is
;;;;     they are relative to the containing screen structure (screen-row or
;;;;     screen-box) and should only be called within the proper clipping and
;;;;     scaling macros.
;;;;
;;;;     This file should be used by on top of draw-low-xxx files which
;;;;     support fast hardware clipping.  The redisplay will setup a
;;;;     new clipping environment for EVERY level of box and row.
;;;;
;;;;     It should be possible to recompile the system after changing which
;;;;     draw-high-xxx-clipping.lisp file to use in the boxsys.lisp file
;;;;     to see which version is faster.
;;;;
;;;;     This file is meant to coexist with various
;;;;     "xxx-draw-low" files which are the machine specific primitives.
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   2/11/03 merged current LW and MCL source
;;;;   5/02/01 allow for software clipping in %bitblt ops for LWWIN in bitblt-move-region
;;;;   4/03/01 draw-string now calls %draw-string with explicit parameter %drawing-window
;;;;           this fixes bug where draw-string inside drawing-on-bitmap on PC would
;;;;           draw to the screen instead of the bitmap
;;;;   6/05/00 with-turtle-clipping changed for LW port
;;;;   5/11/98 added comment describing change to interpretation of x,y in draw-cha
;;;;   5/11/98 started logging: source = boxer version 2.3
;;;;

;;; Interface functions WINDOW-PARAMETERS-CHANGED, WITH-DRAWING.  UPDATE-WINDOW-SYSTEM-STATE
;;; must be defined by the window system code.

; **** no longer used, see draw-low-mcl for details
;(defmacro with-drawing (&body body)
;  `(progn
;     (update-window-system-state)
;     ,@body))

;;; Wrap this around the body of let forms that bind clipping variables.
;;; Now a no-op, but a more efficient implementation might make use of this.
;(defmacro with-clip-bindings (&body body)
;  `(progn ,@body))


;; Since we have hardware clipping we'll just go ahead an draw stuff even
if it is out of bounds
(defmacro x-out-of-bounds? (scaled-x)
  (declare (ignore scaled-x))
  nil)

(defmacro y-out-of-bounds? (scaled-y)
  (declare (ignore scaled-y))
  nil)

(defmacro clip-x (scaled-x) scaled-x)
(defmacro clip-y (scaled-y) scaled-y)


(defun bitblt-within-screen (alu full-wid full-hei from-x from-y to-x to-y)
  (let (;; hardware clipping is only performed on the destination
        ;; rect, so we have to make sure we don't pull in any
        ;; pixels from outside the clipping region from the source rect
        (wid (min& full-wid (-& %local-clip-rig from-x)))
        (hei (min& full-hei (-& %local-clip-bot from-y))))
    (%bitblt-in-screen alu wid hei
           %drawing-array from-x from-y to-x   to-y)))


(defun bitblt-move-region (full-wid full-hei from-x from-y delta-x delta-y)
  (let (;; hardware clipping is only performed on the destination
        ;; rect, so we have to make sure we don't pull in any
        ;; pixels from outside the clipping region from the source rect
        (wid (min& full-wid (-& %clip-rig from-x)))
        (hei (min& full-hei (-& %clip-bot from-y))))
    (unless (or (zerop full-wid) (zerop full-hei))
    (%bitblt-in-screen alu-seta wid hei
           %drawing-array from-x from-y
           (+& from-x delta-x) (+& from-y delta-y))
    ;; Now we erase the part of the screen which is no longer covered.
    (unless (zerop delta-x)
      (erase-rectangle (abs delta-x) hei
           (cond ((plusp delta-x) from-x)
           ((>& (abs delta-x) wid) from-x)
                             #+lwwin
                             ;;If the region we're moving is partly
           ;;not displayed due to clipping we have to
           ;;clear out stuff specially.  This has a
           ;;few bugs but it works better than with
           ;;out it.
                             ;; NOTE: this is because LW does software clipping for
                             ;; %bitblt ops
           ((>& (+& wid from-x  %origin-x-offset) %clip-rig)
            (+& %clip-rig delta-x (-& %origin-x-offset)))
           (t (+& from-x wid delta-x)))
           from-y))
    (unless (zerop delta-y)
      (erase-rectangle wid (abs delta-y)
           from-x
           (cond ((plusp delta-y) from-y)
           ((>& (abs delta-y) hei) from-y)
                             #+lwwin
                             ;; same software clipping stuff, doo dah doo dah...
                             ((>& (+& hei from-y %origin-y-offset) %clip-bot)
          (+& %clip-bot delta-y (-& %origin-y-offset)))
           (t (+& from-y hei delta-y))))))))

;;;;
;;;; FILE: draw-low-opengl.lisp
;;;;

;; sgithens 2022-02-26 Removing these commented out bits, putting here as a possible future
;; reference
(defun clear-window (w)
  (opengl::rendering-on (w)
    ;; sets the clearing color
    ;; shouldn't need to do this EVERY time
    (opengl::gl-clear-color (bw::ogl-color-red *background-color*)
                        (bw::ogl-color-green *background-color*)
                        (bw::ogl-color-blue *background-color*)
                        0.0)
    ;(gl-clear-depth d)
    ;(gl-clear-accum r g b alpha)
    ;(gl-clear-stencil s)
    ;; clears the screen to the clearing color
    ;; 2nd arg is logior of possible:
    ;; *gl-color-buffer-bit*
    ;; *GL-depth-BUFFER-BIT*
    ;; *GL-accum-BUFFER-BIT*
    ;; *GL-stencil-BUFFER-BIT*
    (opengl::gl-clear opengl::*gl-color-buffer-bit*)))

(defun sheet-screen-array (window) window)

;;; In the new regime, coordinates are relative to the grafport (window) rather than the pane.
(defun sheet-inside-top (window) (declare (ignore window)) 0)
(defun sheet-inside-left (window) (declare (ignore window)) 0)

;; figure out if we are using this for convert-color specs or not
(defun normalize-color-component (value) (/ value 100.0))

;; sgithens 2022-02-26 This doesn't appear to be used anywhere
(defun %pen-color= (color)
  (color= color
          (bw::ogl-current-color)))

(defmacro sign-of-no (x) `(if (plusp& ,x) 1 -1))

;; not currently used, leave here to document calling convention
(defun %draw-arc (bit-array alu x y width height th1 th2)
  (declare (ignore bit-array alu x y width height th1 th2))
#|
  (if (=& alu alu-xor)
      (gp:draw-arc bit-array x y width height (* (- 90 th1) +degs->rads+)
                   (* th2 +degs->rads+ -1)
               :operation alu :filled nil :foreground #xffffff)
    (gp:draw-arc bit-array x y width height (* (- 90 th1) +degs->rads+)
                 (* th2 +degs->rads+ -1)
               :operation alu :filled nil))
|#
)

;; not currently used, leave here to document calling convention
(defun %draw-filled-arc (bit-array alu x y width height th1 th2)
  (declare (ignore bit-array alu x y width height th1 th2))
#|
  (if (=& alu alu-xor)
      (gp:draw-arc bit-array x y width height (* (- 90 th1) +degs->rads+)
                   (* th2 +degs->rads+ -1)
               :operation alu :filled t :foreground #xffffff)
    (gp:draw-arc bit-array x y width height (* (- 90 th1) +degs->rads+)
                 (* th2 +degs->rads+ -1)
                 :operation alu :filled t))
|#
)


;;; This is called to make sure the quickdraw clipping is set up to match the boxer clipping.
(defun update-window-system-state () )


(defmacro clip-x (scaled-x) `(max& %clip-lef (min& ,scaled-x %clip-rig)))
(defmacro clip-y (scaled-y) `(max& %clip-top (min& ,scaled-y %clip-bot)))

;; sgithens 2021-05-07 This doesn't appear to be used anywhere
(defun %bitblt-in-screen (alu wid hei array fx fy tx ty)
  (declare (ignore alu array))
  (opengl::%pixblt-in-screen (round wid) (round hei)
                             (round fx) (round fy) (round tx) (round ty)))


;; sgithens 2021-05-07 This doesn't appear to be used anywhere
;; &&&& stub
(defun %bitblt-icon-to-screen (icon tx ty) (declare (ignore icon tx ty)) )

;; stub
(defun sheet-font-map (w) (declare (ignore w)) nil)

(defun max-font-size-baseline-value ()
  (- (length *font-sizes*) 7 1))

(defvar *current-font-family-index* 0)

(defun max-font-size-baseline-value ()
  (- (length *font-sizes*) 7 1))

(defvar *current-font-family-index* 0)

;;
;;  o fonts remain a fixnum but we use a caching mechanism
;;    fields of a fontspec are used as indices into simple arrays
;;
;;  o We use this indirect mechanism rather than using LW fonts
;;    directly for several reasons
;;    1) There are lots of font dependencies in the bootstrapping
;;       process and in LWWin, fonts can't be defined until AFTER
;;       the boxer window has been created. The indirection lets
;;       us refer to a font before it is actually defined.
;;    2) We need font= to be a fast operation, using fixnums makes
;;       this so.  Using LW fonts would be a field by field comparison
;;       at best.
;;    3) At some point, perhaps for speed, we might want to move to
;;       using native Windoze fonts directly.  This makes it easy to
;;       change.
;;
;;  o LW most-positive-fixnum = (1- (expt 2 29))  [previously (1- (expt 2 23))]
;;    therefore, LW boxer fonts will have to be 28 bits wide
;;
;;  o A fontspec is a list of family name, size &rest style-attributes
;;
;; The Boxer font-no is defined as:
;;
;;   o the upper 8 bits specify a font family.  On the mac, the translation between
;;     number and font name was handled by the MacOS, for LWWin, we'll just
;;     use an array of font family names, with the number being the index into
;;     the array.
;;
;;   o the style is in the next 4.  Together, the font and the face
;;     correspond to the MCL concept of the font/face code.
;;
;;   o the low 12 bits are used as a size index.  NOT the size.
;;
;;   NOTE: we can juggle the various field sizes as needed.  In particular,
;;         the face field can go down to 3 bits to support BOLD, ITALICS and
;;         UNDERLINE.  The size field needs only be able to cover the allowed
;;         (by the interface) sizes.
;;
;;  The Structure of the Font Cache
;;
;;   o an array of size %%font-family-size(8)
;;     each element of the array will point to a size array
;;
;;   o each size array will be = to the number of allowed (by the interface)
;;     sizes for each font. For now, we'll specify the defined sizes as:
;;     6,8,10,12,14,16,18,24 implying a size array with length 8
;;
;;   o each entry in the size array points to an array of specific face styles
;;     for each font, for now, it will be length 4 to support all the possible
;;     combinations of BOLD, ITALICS
;;
;;   o this leaves an extra 4 more bits, either for more sizes or families
;;
;;   Boxer Font Number
;;   +--------------+---------------+-------------+
;;   |Family index 8|  size index 12|style index 4|
;;   +------+-------+------+--------+-----+-------+
;;          |              |             |
;;          +---+          +--+          +--+
;;   Font Cache |    +-+      |             |
;;              |   0| +      |             |
;;              |    | |      |             |
;;              +-> i|++------+-> +-+       | styles
;;                   : : size |   | |       | cache
;;                   : : cache+-> |++-------+->+-+
;;                                : :       |  | |
;;                                : :       +> |++---> LW font
;;                                             : :
;;

;; Low Level Font Utilities
(defconstant %%font-family-bit-pos 16)
(defconstant %%font-family-bit-length 8)
(defconstant %%font-size-bit-pos 4)
(defconstant %%font-size-bit-length 12)
(defconstant %%font-face-bit-pos 0)
(defconstant %%font-face-bit-length 4)

(defconstant %%font-family-size (expt 2 %%font-family-bit-length))

;; doesn't fill the reserved 8-bit sized field, but keep them
;; only as big as wee need
;(defconstant %%font-size-cache-length 12)
(defconstant %%font-face-cache-length 4)

(defun %make-font-size-cache ()
  (make-array (length *font-sizes*) :initial-element nil))
; (make-array ;%%font-size-cache-length :initial-element nil))

(defun %make-font-face-cache ()
  (make-array %%font-face-cache-length :initial-element nil))

;; field extractors
(defun %font-family-idx (font-no)
  (ldb& (byte %%font-family-bit-length %%font-family-bit-pos) font-no))

(defun %font-face-idx (font-no)
  (ldb& (byte %%font-face-bit-length   %%font-face-bit-pos)   font-no))

(defun font-values (font-no)
  (declare (values family-idx size-idx face-idx))
  (values (%font-family-idx font-no)
          (%font-size-idx   font-no)
          (%font-face-idx   font-no)))

;; search the font cache for an existing match
;; returns fixnum index or NIL
(defun font-family-name-pos (string)
  (dotimes (i *current-font-family-index*)
    (let ((fam-name (svref& *font-family-names* i)))
      (when (string-equal string fam-name) ; case insensitive
        (return i)))))

(defun new-font-family (family-name)
  (setf (svref& *font-family-names* *current-font-family-index*) family-name
        (svref& *font-cache* *current-font-family-index*) (%make-font-size-cache))
  (prog1 *current-font-family-index* (incf& *current-font-family-index*)))

(defun %font-name-to-idx (string &optional create?)
  (let ((existing (font-family-name-pos string)))
    (if create? (or existing (new-font-family string)) existing)))

;; BOLD, ITALIC are documented, looks like UNDERLINE is
;; supported but not documented
(defun %font-face-to-idx (style-list)
  (let ((idx 0))
    (dolist (style style-list)
      (case style
        (:bold      (setq idx (logior #b1   idx)))
        (:italic    (setq idx (logior #b10  idx)))
        ;(:underline (setq idx (logior #b100 idx)))
        ))
    idx))

(defun cache-font (font font-no &optional (translate-size t))
  (multiple-value-bind (fam size face)
                       (font-values font-no)
                       (let ((size-cache (svref& *font-cache* fam)))
                         (when (null size-cache)
                           (setq size-cache (%make-font-size-cache))
                           (setf (svref& *font-cache* fam) size-cache))
                         (let ((face-cache (svref& size-cache (if translate-size
                                                                (font-size-to-idx size)
                                                                size))))
                           (when (null face-cache)
                             (setq face-cache (%make-font-face-cache))
                             (setf (svref& size-cache size) face-cache))
                           (setf (svref& face-cache face) font)))))

(defun fontspec->font-no (fontspec &optional create?)
  (let ((fambits (%font-name-to-idx (or (car fontspec) *default-font-family*)
                                    create?)))
    (unless (null fambits)
      (dpb& fambits
            (byte %%font-family-bit-length %%font-family-bit-pos)
            (dpb& (%font-size-to-idx (or (cadr fontspec) *default-font-size*))
                  (byte %%font-size-bit-length   %%font-size-bit-pos)
                  (%font-face-to-idx (or (cddr fontspec) *default-font-face*)))))))


(defun bfd-font-size-name (size-idx) (svref *bfd-font-size-names* (1- size-idx)))

(defun get-size-menu-names ()
  (svref *font-size-menu-names* *font-size-baseline*))

(defun set-font-family-alias (family-name local-name)
  (let ((existing (font-family-alias family-name)))
    (cond ((null existing)
           (push (cons family-name local-name) *font-family-aliases*))
      ((string= local-name (cadr existing)))
      (t (if *boxer-system-hacker*
           (error "Trying to CHANGE an alias for ~S from ~S to ~S"
                  family-name (cadr existing) local-name)
           (warn "Trying to CHANGE an alias for ~S from ~S to ~S"
                 family-name (cadr existing) local-name))))))

(defun fontspec->font-values (fontspec)
  (let ((fambits (%font-name-to-idx (or (car fontspec) *default-font-family*))))
    (unless (null fambits)
      (values fambits
              (%font-size-to-idx (cadr fontspec))
              (%font-face-to-idx (or (cddr fontspec) *default-font-face*))))))

(defvar *font-family-names* (make-array %%font-family-size :initial-element nil))

;; these next 3 take a font code and should return a new font code
(defun %set-font (boxer-font new-fname)
  (let ((new-idx (%font-name-to-idx new-fname)))
    (dpb new-idx (byte %%font-family-bit-length %%font-family-bit-pos) boxer-font))
    )

(defun %set-font-size (boxer-font new-size)
  (dpb new-size (byte %%font-size-bit-length   %%font-size-bit-pos) boxer-font))

(defun %set-font-style (boxer-font new-style-byte)
  (dpb new-style-byte (byte %%font-face-bit-length   %%font-face-bit-pos)
       boxer-font))

;; used for bootstrapping, normally we go through make-boxer-font
(defun %make-font-number-internal (fam-idx size &rest styles)
  (dpb fam-idx (byte %%font-family-bit-length %%font-family-bit-pos)
       (%set-font-size (%font-face-to-idx styles) size)))

(defun %%make-font-number-internal (fam-idx size-idx &rest styles)
  (dpb fam-idx (byte %%font-family-bit-length %%font-family-bit-pos)
       (dpb size-idx (byte %%font-size-bit-length   %%font-size-bit-pos)
            (%font-face-to-idx styles))))

(defun make-boxer-font (rawfontspec)
  (let* ((alias (font-family-alias (car rawfontspec)))
         (fontspec (if alias (list* alias (cdr rawfontspec)) rawfontspec))
         (font-no (fontspec->font-no fontspec)))
    (cond ((null font-no)
           (let ((oglfont (bw::%make-opengl-font :font-triple fontspec))
                 (new-font-no (fontspec->font-no fontspec T)))
             (or (find-cached-font new-font-no nil)
                 (cache-font oglfont new-font-no nil))
             new-font-no))
      (t
        (or (find-cached-font font-no nil)
           (cache-font (bw::%make-opengl-font :font-triple fontspec) font-no nil)
            )
       font-no))))

;; there are 2 notions of size.
;;
;; the absolute font size is used internally in the font-cache structures,
;;
;; the font size, as it appears inside of Boxer-Font-Descriptors is a relative number
;; from 1 - 7, with 3 being interpreted as "Normal" (leaving 2 smaller and 4 larger sizes)
;;
(defvar *bfd-font-size-names*
  (vector "Smallest" "Smaller" "Normal" "Larger" "Larger2" "Larger3" "Largest"))

;; these need to be in sync (used to initialize the font size menu)
;; see also font-size-menu-item-name
(defvar *bfd-font-size-values* '(1 2 3 4 5 6 7))

(defun make-size-menu-names ()
  (let* ((sl (length *font-sizes*))
         (n-sizes (- sl 7)) ;; number of possible size settings.  Changed to 7 for boxer-bugs-39
         (names-array (make-array n-sizes))
         (menu-length (length *bfd-font-size-names*)))
    (dotimes (i n-sizes)
      (let ((names (make-list menu-length)))
        (setf (svref names-array i) names)
        (dotimes (j menu-length)
          (let ((size-name (svref *bfd-font-size-names* j)))
            (setf (nth j names)
                  (format nil "~A (~D)" size-name (svref *font-sizes* (+ i j))))))))
    names-array))

(defvar *font-size-menu-names* (make-size-menu-names))

(defun font-size-menu-item-name (data)
  (nth (1- data) (svref *font-size-menu-names* *font-size-baseline*)))

(defun find-cached-font (font-no &optional (translate-size t))
  (multiple-value-bind (fam size face)
                       (font-values font-no)
                       (let ((size-cache (svref& *font-cache* fam)))
                         (unless (null size-cache)
                           (let ((face-cache (svref& size-cache (if translate-size
                                                                  (font-size-to-idx size)
                                                                  size))))
                             (unless (null face-cache) (svref& face-cache face))))))
                             )

;; sgithens Only used in make-boxer-font
;; (defun cache-font (font font-no &optional (translate-size t))
;;   ; TODO - reimplement
;;   )

;; bypass the caching mechanism because this will get called
;; at a stage where we can't open any fonts, instead, we hand
;; make the font numbers but defer actually finding fonts to
;; fill the cache until after the boxer window has been created
(defun init-bootstrapping-font-vars ()
  (setq  *normal-font-no*           (%%make-font-number-internal 0 3)
         *box-border-label-font-no* (%%make-font-number-internal 0 1)
         *box-border-name-font-no*  (%%make-font-number-internal 0 3 :bold)
         *sprite-type-font-no*      (%%make-font-number-internal 0 3 :bold)
         *boxtop-text-font*         (%%make-font-number-internal 0 3 :bold)
         ))

;; this is here so it can be kept in synch with  init-bootstrapping-font-vars
;; note that make-boxer-font is called for side effect, it fills the relevant
;; font caches with the internal representation system font
;; Note: 4/16/2010 do not calculate all the font info for every font up font
(defun fill-bootstrapped-font-caches ()
  (dolist (font-family *font-families*)
    (dotimes (i (length *font-sizes*))
      (let ((size (svref *font-sizes* i)))
        (dolist (style '(nil (:bold) (:italic) (:bold :italic))) ; leave out :underline for now
          (make-boxer-font (list* font-family size style)))))))


;; THIS is safe to do
(eval-when (load)  (init-bootstrapping-font-vars))

(defvar *default-font-size*   3)
(defvar *default-font-face* nil)

;; END MAJOR FONT REFACTORING

;; this is a stub
;; it is used by some window systems(the mac) to insure all graphics
;; commands are sent to the VIEW arg
;;
;; we seem to access the graphics state a lot so it might be advantageous to
;; bind it within this macro

(defvar %graphics-state nil)

(defmacro current-graphics-state ()
  '(or %graphics-state (gp:get-graphics-state (or %drawing-array *boxer-pane*))))

;;; See Inside Mac V-53
#| ;; unused ?
(defun pixmap? (bm-or-pm) (typep bm-or-pm 'gp::pixmap-port))
|#

;; yuck !!!
(defun window-depth (window)
  (declare (ignore window)) (capi:screen-depth (car (capi::screens))))


;;; Opengl configuration variables
;;; in theory, the GPU will be queried as a redisplay init and these
;;; parameters will be initialized.
(defvar *OpenGL-lots-o-memory-yay* t
  "Mostly having to do with how much stuff we will try and cache on GPU")

(defun configure-gpu-parameters ()
  ;; font vars
;  *opengl-font-start*  ; 0 or 32
;  *opengl-font-count*  ; 128 or 256
  )
(def-redisplay-initialization (configure-gpu-parameters))

;;; why doesn't this work ?  It's a documented function
; (gp:port-depth window))

(defun make-boxer-font-ogl (rawfontspec calculate-parameters?)
  (let* ((alias (font-family-alias (car rawfontspec)))
         (fontspec (if alias (list* alias (cdr rawfontspec)) rawfontspec))
         (sysfont (boxer-font-spec->lw-font fontspec))
         (font-no (fontspec->font-no fontspec)))
    (cond ((null font-no)
           ;; no cached font, we have to be careful here because a possible
           ;; scenario is that we are translating a mac font which could come out
           ;; as an existing PC font
           ;; wait until we have a solid native font before converting to an
           ;; opengl font
           (let* ((oglfont  (if (null calculate-parameters?)
                              (bw::register-opengl-font-from-native-font sysfont)
                              (bw::make-opengl-font-from-native-font sysfont)))
                  (localname (unless (null oglfont)
                               (gp:font-description-attribute-value
                                (gp:font-description sysfont) :family)))
                  (newfontspec (list* localname (cdr fontspec)))
                  (new-font-no (fontspec->font-no newfontspec T)))
             (unless (null localname)
               (set-font-family-alias (car rawfontspec) localname)
               (unless (member localname *font-families* :test #'string-equal)
                 (nconc *font-families* (list localname))))
             (or (find-cached-font new-font-no nil)
                 (cache-font oglfont new-font-no nil))
             new-font-no))
      (t
       (or (find-cached-font font-no nil)
           (let ((font (if (null calculate-parameters?)
                         (bw::register-opengl-font-from-native-font
                          (boxer-font-spec->lw-font fontspec))
                         (bw::make-opengl-font-from-native-font sysfont))))
             (cache-font font font-no nil)))
       font-no))))

;; the LW font internals looks like it supports :underline, but leave out for
;; now because it isn't documented
(defun boxer-font-spec->lw-font (spec)
  (let ((family (car spec))
        (size (or (cadr spec) 10))
        (styles (cddr spec)))
    (gp:find-best-font *boxer-pane*
                       (gp:make-font-description :family family
                                                 :size size
                                                 :weight (if (member :bold styles)
                                                           :bold
                                                           :normal)
                                                 :slant (if (member :italic styles)
                                                          :italic
                                                          :roman)
                                                 :underline
                                                 (not (null (member :underline
                                                                    styles)))))))

; not used ?
;(defun fast-cha-ascent () (gp:get-font-ascent %drawing-array))
;(defun fast-cha-hei ()
;  (+& (gp:get-font-ascent  %drawing-array)
;      (gp:get-font-descent %drawing-array)))

#| ;; no longer used?
(defmacro with-drawing-into-new-bitmap ((bitmap-name
           drawable bit-width bit-height
           . window-system-specific-args)
          &body body)
  (declare (ignore window-system-specific-args))
  `(let ((,bitmap-name (make-offscreen-bitmap ,drawable ,bit-width ,bit-height)))
     (drawing-on-bitmap (,bitmap-name)
       (progn
         (%erase-rectangle ,bit-width ,bit-height 0 0 ,bitmap-name)
         ,@body))
     ,bitmap-name))
|#

; gp::erase-rectangle ignores the state of the transform
; so it loses for sub boxes during redisplay
  ;(gp:clear-rectangle window x y w h)

;;;;; obsolete.... commented out 7/18/13
;(defvar *font-map* (make-array '(8)))
;(defvar *font-codes* (make-array '(8)))

;;; ???
;(defun sheet-font-map (w)
;  (declare (ignore w))
;  *font-map*)

#|
;; make a true type font, and associate it with a font number
;; font may already exists be associated
;; cases: 1) no font-no => get new ttf-font (a) fill cache or (b) not (c) cache is filled with another font
;;        2) font-no => (a) cache is filled or (b) fill cache

(defun make-boxer-font (rawfontspec &optional (calculate-parameters? T))
  (let* ((alias (font-family-alias (car rawfontspec)))
         ;; this allows boxer level font translation (eq "Geneva" => "Arial")
         (fontspec (if alias (list* alias (cdr rawfontspec)) rawfontspec))
         (font-no (fontspec->font-no fontspec)))
    (cond ((null font-no)
           ;; no assigned font
           (let* ((ttf-font (bw::register-ttf-font fontspec))
                  (ttf-fontspec (bw::ttf-fontspec ttf-font))
                  ;; register-ttf-font may map the requested font into an existing ttf-font
                  (ttf-font-no (fontspec->font-no ttf-fontspec T)))
             (when (not (string= (car fontspec) (car ttf-fontspec)))
               (set-font-family-alias (car rawfontspec) (car ttf-fontspec)))
             (unless (member (car ttf-fontspec) *font-families* :test #'string-equal)
               (nconc *font-families* (list (car ttf-fontspec))))
             (unless (find-cached-font ttf-font-no)
               (cache-font ttf-font ttf-font-no))
             (when calculate-parameters?
               (bw::cache-ttf-font *boxer-pane* :font ttf-font))
             ttf-font-no))
          (t
           (unless (find-cached-font font-no)
             (let ((ttf-font (bw::register-ttf-font fontspec)))
               (cache-font ttf-font font-no)
               (when calculate-parameters?
                 (bw::cache-ttf-font *boxer-pane* :font ttf-font))))
           font-no))))
|#

;(defun %set-font-size (boxer-font new-size)
;  (let ((new-idx (%font-size-to-idx new-size)))
;    (dpb new-idx (byte %%font-size-bit-length   %%font-size-bit-pos) boxer-font)))

#| ;; leave this here in case we need to remember how to do software clipping
(defun %bitblt-to-screen (alu wid hei from-array fx fy tx ty)
  (let* ((scaled-to-x (+& %origin-x-offset tx)) (scaled-to-y (+& %origin-y-offset ty))
         (clipped-to-x (clip-x scaled-to-x))    (clipped-to-y (clip-y scaled-to-y))
   (+wid (abs& wid))
   (+hei (abs& hei))
   (lef-overrun (max& 0 (-& scaled-to-x clipped-to-x)))
   (top-overrun (max& 0 (-& scaled-to-y clipped-to-y)))
   (rig-overrun (max& 0 (-& (+& clipped-to-x +wid)
          (clip-x (+& clipped-to-x +wid)))))
   (bot-overrun (max& 0 (-& (+& clipped-to-y +hei)
          (clip-y (+& clipped-to-y +hei)))))
   (clipped-wid (*& (sign-of-no wid)
        (max& 0 (-& +wid lef-overrun rig-overrun))))
   (clipped-hei (*& (sign-of-no hei)
        (max& 0 (-& +hei top-overrun bot-overrun)))))
    (or (zerop& clipped-wid)
        (zerop& clipped-hei)
        (gp:pixblt %drawing-array alu from-array clipped-to-x clipped-to-y
                   clipped-wid clipped-hei fx fy))))
|#

#|
(defun %bitblt-from-screen (alu wid hei to-array fx fy tx ty)
  (let* ((scaled-from-x (+& %origin-x-offset fx))
         (scaled-from-y (+& %origin-y-offset fy))
         (clipped-from-x (clip-x scaled-from-x))
         (clipped-from-y (clip-y scaled-from-y))
         (+wid (abs& wid))
   (+hei (abs& hei))
   (lef-overrun (max& 0 (-& scaled-from-x clipped-from-x)))
   (top-overrun (max& 0 (-& scaled-from-y clipped-from-y)))
   (rig-overrun (max& 0 (-& (+& clipped-from-x +wid)
                                  (clip-x (+& clipped-from-x +wid)))))
   (bot-overrun (max& 0 (-& (+& clipped-from-y +hei)
                                  (clip-y (+& clipped-from-y +hei)))))
   (clipped-wid (*& (sign-of-no wid) (max& 0 (-& +wid lef-overrun rig-overrun))))
   (clipped-hei (*& (sign-of-no hei) (max& 0 (-& +hei top-overrun bot-overrun)))))
    (or (zerop& clipped-wid)
        (zerop& clipped-hei)
        (gp:pixblt to-array alu %drawing-array tx ty
                   clipped-wid clipped-hei clipped-from-x clipped-from-y))))
|#

#|
(defun %bitblt-in-screen (alu wid hei array fx fy tx ty)
  (let* ((scaled-from-x (+& %origin-x-offset fx))
         (scaled-from-y (+& %origin-y-offset fy))
         (scaled-to-x   (+& %origin-x-offset tx))
         (scaled-to-y   (+& %origin-y-offset ty))
         (clipped-from-x (clip-x scaled-from-x))
         (clipped-from-y (clip-y scaled-from-y))
   (clipped-to-x (clip-x scaled-to-x))
   (clipped-to-y (clip-y scaled-to-y))
   (+wid (abs& wid))
   (+hei (abs& hei))
   (lef-overrun (max& 0
                            (-& scaled-from-x clipped-from-x)
                            (-& scaled-to-x clipped-to-x)))
   (top-overrun (max& 0
          (-& scaled-from-y clipped-from-y)
          (-& scaled-to-y clipped-to-y)))
   (rig-overrun (max& 0
          (-& (+& clipped-from-x +wid)
        (clip-x (+& clipped-from-x +wid)))
          (-& (+& clipped-to-x +wid)
        (clip-x (+& clipped-to-x +wid)))))
   (bot-overrun (max& 0
          (-& (+& clipped-from-y +hei)
                                (clip-y (+& clipped-from-y +hei)))
          (-& (+& clipped-to-y +hei)
        (clip-y (+& clipped-to-y +hei)))))
   (clipped-wid (*& (sign-of-no wid) (max& 0 (-& +wid lef-overrun rig-overrun))))
   (clipped-hei (*& (sign-of-no hei) (max& 0 (-& +hei top-overrun bot-overrun)))))
    (or (zerop& clipped-wid)
        (zerop& clipped-hei)
        (gp:pixblt array alu array clipped-to-x clipped-to-y
                   clipped-wid clipped-hei clipped-from-x clipped-from-y))))
|#

;; this could draws into an auxiliary buffer and then
;; transfers the bits from that buffer into an ogl-pixmap
#|
(defmacro with-system-dependent-bitmap-drawing ((bitmap &optional
                                                        bitmap-width bitmap-height)
                  &body body)
  (declare (ignore bitmap-width bitmap-height))
  `(opengl::rendering-on (*boxer-pane*)
     (unwind-protect
         (progn
           (bw::gl-draw-buffer bw::*gl-aux1*)
           (progn . ,body)
           (bw::gl-flush)
           (opengl::%pixblt-from-screen ,bitmap 0 0
                                        (opengl::ogl-pixmap-width  ,bitmap)
                                        (opengl::ogl-pixmap-height ,bitmap)
                                        0 0 bw::*gl-aux1*))
       (bw::gl-draw-buffer bw::*gl-back*))))
|#

;;;;
;;;; FILE: editor.lisp
;;;;

#|
(defun get-boxer-status-string (outermost-box-name other-string)
  (flet ((get-boxer-version-string ()
           (or *boxer-version-info*
               (system-version 'boxer))))

    (cond ((null other-string)
     (when (null outermost-box-name)
       (setq outermost-box-name (name-string (outermost-box))))
     (if (null *editor-numeric-argument*)
         (format nil "~A |         Outermost Box: ~A"
           (get-boxer-version-string) outermost-box-name)
         (format nil "~A |         Outermost Box: ~A | Arg: ~D"
           (get-boxer-version-string) outermost-box-name
           *editor-numeric-argument*)))
    (t
     (if (null *editor-numeric-argument*)
         (format nil "~A |         ~A" (get-boxer-version-string) other-string)
         (format nil "~A |         ~A | Arg: ~D" (get-boxer-version-string) other-string *editor-numeric-argument*))))))


(defun redraw-status-line (&optional new-name other-string)
  (window-system-dependent-redraw-status-line
   (get-boxer-status-string new-name other-string)
   (not (null new-name))))
|#

;;;;
;;;; FILE: ev-int.lisp
;;;;

#+symbolics
(defun Fwc (&optional exp)
  (top-level-eval-wrapper
   (when (null exp) (setq exp (eval-objs (point-row))))
   (time (boxer-eval::boxer-eval exp) t)))

#+symbolics
(defun Fwm (&optional exp)
  (top-level-eval-wrapper
   (when (null exp) (setq exp (eval-objs (point-row))))
   (meter:with-monitoring  t (boxer-eval::boxer-eval exp))))

;;;;
;;;; FILE: evalmacs.lisp
;;;;

;;;
;;; Compiler interface
;;;
(defvar *old-compilation-speed* 0)

(defmacro compile-lambda-if-possible (name lambda-form)
  `(cond ((null *compile-boxer-generated-lambda?*) ,lambda-form)
     ((eq *compile-boxer-generated-lambda?* :fast-compile)
      (proclaim '(optimize (compilation-speed 3)))
      (unwind-protect (symbol-function (compile ,name ,lambda-form))
                      (proclaim `(optimize (compilation-speed ,*old-compilation-speed*)))))
     (t
      (symbol-function (compile ,name ,lambda-form)))))

;;; POSSIBLE-EVAL-OBJECT? tells whether it is legal to look in slot 0.
(defmacro possible-eval-object? (thing)
  #+(or lucid lispworks)
  `(simple-vector-p ,thing)
  #+(or excl lispm)
  T
  #+mcl
  `(vectorp ,thing)  ; +++ I guess
  #-(or lucid lispm excl mcl lispworks)
  (warn "Check if your CLOS or PCL implementation uses vectors to make its objects~%~
         If (vectorp (make-instance <whatever>) is NIL, then~
         change the definition of ~S to simply return T" 'possible-eval-object)
  #-(or lucid lispm excl mcl lispworks)
  `(vectorp ,thing))

;;;;
;;;; FILE: gdispl.lisp
;;;;

; old (non-caching) implementation
;(defgraphics-handler (change-alu *turtle-graphics-handlers*) (trans-x
;							      trans-y
;							      cos-scale
;							      sin-scale
;							      scale)
;  ;; prevent bound but never used errors
;  ;; we can't use declare because the body is expanded in the wrong place
;  trans-x trans-y cos-scale sin-scale scale
;  (unless (=& new-alu *graphics-state-current-alu*)
;    (setq *graphics-state-current-alu* new-alu)))

;; sgithens 2022-03-10 alu version of sprite-commands-for-new-position
(defun sprite-commands-for-new-position (new-x new-y &optional (alu alu-seta))
  (list 'bu::penup 'bu::setxy new-x new-y
        (case alu
          (#.alu-xor 'bu::penxor)
          ((#.alu-seta #.alu-ior) 'bu::pendown)
          ((#.alu-setz #.alu-andca) 'bu::penerase)
          (t 'bu::pendown))))

    #-opengl
    (let ((diameter  (fixr (+ radius radius)))
          (fix-radius (fixr radius)))
      (%draw-filled-arc %drawing-array *graphics-state-current-alu*
                        (ensure-legal-window-coordinate
                        (scale-x (-& x fix-radius)))
                        (ensure-legal-window-coordinate
                        (scale-y (-& y fix-radius)))
                        diameter diameter
                        start-angle sweep-angle))

#-opengl
    (let ((diameter  (fixr (* 2 radius)))
          (fix-radius (fixr radius)))
      (%draw-arc %drawing-array *graphics-state-current-alu*
                (ensure-legal-window-coordinate (scale-x (-& x fix-radius)))
                (ensure-legal-window-coordinate (scale-y (-& y fix-radius)))
                diameter diameter
                start-angle sweep-angle))

    #-opengl
    (let ((diameter  (fixr (+ radius radius)))
          (fix-radius (fixr radius)))
      (%draw-filled-arc %drawing-array *graphics-state-current-alu*
                        (ensure-legal-window-coordinate
                        (scale-x (-& x fix-radius)))
                        (ensure-legal-window-coordinate
                        (scale-y (-& y fix-radius)))
                        diameter diameter
                        0 360))

    #-opengl
    (let ((diameter  (fixr (* 2 radius)))
          (fix-radius (fixr radius)))
      (%draw-arc %drawing-array *graphics-state-current-alu*
                (ensure-legal-window-coordinate (scale-x (-& x fix-radius)))
                (ensure-legal-window-coordinate (scale-y (-& y fix-radius)))
                diameter diameter 0 360))

          #-opengl
          (drawing-on-bitmap (new-bitmap)
                             (with-pen-color ((or (graphics-sheet-background sheet) *background-color*))
                               (draw-rectangle alu-seta new-wid new-hei 0 0)))

        #-opengl
        (drawing-on-bitmap (new-bitmap)
                           (case *boxer-graphics-box-bit-gravity*
                             (:top-right (bitblt-to-screen alu-seta
                                                           (min& old-wid new-wid)
                                                           (min& old-hei new-hei)
                                                           old-bitmap 0 0 0 0))
                             (:center (bitblt-to-screen alu-seta
                                                        (min& old-wid new-wid)
                                                        (min& old-hei new-hei)
                                                        old-bitmap
                                                        (max& 0 (round (-& old-wid new-wid) 2))
                                                        (max& 0 (round (-& old-hei new-hei) 2))
                                                        (max& 0 (round (-& new-wid old-wid) 2))
                                                        (max& 0 (round (-& new-hei old-hei) 2))))))

;;;;
;;;; FILE: file-prims.lisp
;;;;


;;; save now saves to a gensym'd name and then renames if no errors
;;; occur.  Also mv's an existing file of the same name to a backup name
#|
(boxer-eval::defboxer-primitive bu::save ((bu::port-to box) (boxer-eval::dont-copy filename))
  (catch 'cancel-boxer-file-dialog
    (save-generic (box-or-port-target box) (box-text-string filename)))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::really-save ((bu::port-to box)
             (boxer-eval::dont-copy filename))
  (catch 'cancel-boxer-file-dialog
    (save-generic (box-or-port-target box) (box-text-string filename)
                  :always-save? t))
  boxer-eval::*novalue*)
|#

;; stubbified in preparation for "UC clean" reimplentation
(defun record-file-box-place (box)
  (declare (ignore box)) ; suppress warning
  )

;; stubbified in preparation for "UC clean" reimplentation
(defun record-url-box-place (box)
  (declare (ignore box)) ;suppress warning
  )

(boxer-eval::defboxer-primitive bu::mark-for-saving ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::mark-for-saving
                                       " is no longer available, use "
                                       'bu::toggle-modified-flag " instead"))
        (t
         (when (box? boxer-eval::*lexical-variables-root*)
           (mark-file-box-dirty boxer-eval::*lexical-variables-root*))
         boxer-eval::*novalue*)))

#|
(boxer-eval::defboxer-primitive bu::mail ((boxer-eval::dont-copy to) (boxer-eval::dont-copy text))
  (let ((to-box-rows (get-box-rows to)))
    (let ((recipient (evrow-text-string (car to-box-rows) to))
    (subject (and (cdr to-box-rows)
      (evrow-text-string (cadr to-box-rows) to)))
    (message (box-text-string text))
    (mail-in-file (make-temporary-filename "mail-in"))
    (mail-out-file (make-temporary-filename "mail-out")))
      (unwind-protect
    (progn
      (with-open-file (stream mail-in-file :direction :output
            :if-exists :error)
        (and subject (format stream "~~s ~a~%" subject))
        (write-string message stream))
      #+Lucid (lcl::run-unix-program "/usr/ucb/mail"
             :arguments (list recipient)
             :input mail-in-file
             :output mail-out-file)
      (with-open-file (stream mail-out-file :direction :input
            :if-does-not-exist nil)
        (if (or (null stream)
          (zerop& (file-length stream)))
      boxer-eval::*novalue*
      (read-text-stream-internal stream))))
  (when (probe-file mail-in-file)
    (delete-file mail-in-file))
  (when (probe-file mail-out-file)
    (delete-file mail-out-file))))))
|#



;;; diagnostic tool
#+mcl
(boxer-eval::defboxer-primitive bu::show-file-info ()
  (let* ((pathname (boxer-open-file-dialog :prompt "Show File Information for:"))
         (2nd-word nil)
         (1st-word (with-open-file (s pathname :direction :input
                                      :element-type '(unsigned-byte 8.))
                     (read-file-word-from-stream s)
                     (setq 2nd-word (read-file-word-from-stream s)))))
    (make-box (list (list (namestring pathname))
                    (list (format nil "Finder File Type is: ~S"
                                  (ccl::mac-file-type pathname)))
                    (list (format nil "File Creator is: ~S"
                                  (ccl::mac-file-creator pathname)))
                    (list (format nil "Boxer File type is: ~A" (file-type pathname)))
                    (list
                     (cond ((=& 1st-word bin-op-format-version)
                            (format nil "1st word is BOFV (~X), 2nd is ~D"
                                    1st-word 2nd-word))
                           ((=& 1st-word *swapped-bin-op-format-version*)
                            (format nil "1st word is swapped BOFV (~X), 2nd is ~D"
                                    1st-word 2nd-word))
                           (t (format nil "1st word is ~4X(~D), 2nd is ~4X(~D)"
                                      1st-word 1st-word 2nd-word 2nd-word))))))))


#+mcl
(boxer-eval::defboxer-primitive bu::set-boxer-file-info ()
  (let ((pathname (boxer-open-file-dialog :prompt "Set Boxer File Info for:")))
    (ccl::set-mac-file-type pathname :boxr)
    (ccl::set-mac-file-creator pathname :boxr)))

;;;;
;;;; FILE: funs.lisp
;;;;

(defun make-boxer-primitive-internal (arglist code)
  (let ((name (gensym)))
    (proclaim `(special ,name))
    (let ((lisp-function-object
           (compile-lambda-if-possible
            name
            `(lambda ()
                     (let ,(mapcar
                            #'(lambda (u) `(,u (vpdl-pop-no-test)))
                            (reverse arglist))
                       . ,code)))))
      (boxer-toplevel-set
       name
       (make-compiled-boxer-function
        :arglist arglist
        :precedence 0
        :infix-p nil
        :object lisp-function-object)))
    name))

;;;;
;;;; FILE: ftp.lisp
;;;;
;;;; --entire-file--

;;;;    Boxer
;;;;    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                      +-Data--+
;;;;             This file is part of the | BOXER | system
;;;;                                      +-------+
;;;;
;;;;
;;;;     Current implementations for FTP
;;;;

(in-package :boxnet)

(defclass ftp-url
  (net-url)
  (;; this can be :text, :binary, :box, :directory or some other boxer
   ;; document type as defined in the boxer generic file system
   ;; see the boxer::*special-file-readers* variable
   (doc-type :initform ':text :accessor ftp-url-doc-type :initarg :doc-type))
  ;; (:metaclass block-compile-class)
  )

;;; FTP

(defmethod initialize-instance ((url ftp-url) &rest initargs)
  (call-next-method)
  ;; now set up default values for user, port and password if they
  ;; haven't been filled by the net-url method
  (when (null (slot-value url 'port))
    (setf (slot-value url 'port) 21))
  (when (null (slot-value url 'user))
    (setf (slot-value url 'user) "anonymous"))
  (when (and (null (slot-value url 'password))
             (string-equal (slot-value url 'user) "anonymous"))
    (setf (slot-value url 'password) *user-mail-address*))
  ;; now look for a possible type in the declaration
  (let* ((path (slot-value url 'path))
         (suffix (path-suffix path))
         (supplied-doc-type (getf initargs :doc-type))
         (declared-type (declared-ftp-type path)))
    ;; if there is a type declaration in the path, we need to remove it
    (unless (null declared-type)
      (setf (slot-value url 'path)
            (subseq path 0 (position #\; path :from-end t))))
    ;; this is a best guess, it may get changed later when
    ;; we actually try to access the data
    (if (not (null supplied-doc-type))
        ;; if the doc-type is in the initargs, go with it (the slot will
        ;; already have been set by shared-initialize in an earlier method)
        (setf (slot-value url 'doc-type) supplied-doc-type)
        (setf (slot-value url 'doc-type)
              (cond ((string-equal suffix "box") ':box)
                    ;; add any other recognized suffixes here
                    ((and (null declared-type) (null path)) ':directory)
                    ((null declared-type) ':text)
                    ((char-equal declared-type #\a) ':text)
                    ((char-equal declared-type #\d) ':directory)
                    ((char-equal declared-type #\i) ':binary)
                    (t ':text))))))

;;; the main method does the copying, this has to get slots which can't be
;;; inferred from the scheme-string
(defmethod copy-url ((url ftp-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'doc-type) (slot-value url 'doc-type))
    new))

(defmethod url-values ((url ftp-url))
  (let ((dt (slot-value url 'doc-type)))
    (cond ((null dt)
           (call-next-method))
          (t (append (call-next-method) (list "Document-Type" dt))))))

(defun declared-ftp-type (scheme-string)
  (let* ((semi (position #\; scheme-string))
         (type? (when semi (search "type=" scheme-string
                                   :start2 semi :test #'char-equal))))
    (when type?
      (char scheme-string (+& semi 6)))))

;; windows has some services on ports scattered below 2200 (see C:\windows\services)
(defvar *ftp-port-value* #-win32 1025 #+win32 2200)

(defun new-ftp-local-data-port ()
  (incf *ftp-port-value*))

(defun ftp-local-data-port () *ftp-port-value*)

(defvar *explicit-ftp-port?* t)

(defmethod get-password ((url ftp-url))
  (let ((url-pass (slot-value url 'password)))
    (if (null url-pass)
      ;; prompt for password here
      (boxer::get-string-from-status-line
       (format nil "Password for ~A:" (slot-value url 'user)) nil)
      url-pass)))

;; this is passed a freshly opened FTP control stream

(defmethod login-and-cd ((url ftp-url) control dirs)
  ;; read out the opening banner
  (handle-tcp-response control)
  (net-write-control-line control "USER ~A" (slot-value url 'user))
  (surf-message "Logging in as ~A..." (slot-value url 'user))
  (handle-tcp-response control)
  (net-write-control-line control "PASS ~A" (get-password url))
  (handle-tcp-response control)
  ;; now we should be logged in (or else have been error'd out)
  (dolist (dir dirs)
    (net-write-control-line control "CWD ~A" dir) (handle-tcp-response control))
  ;; now put the server in the right mode
  (when (member (slot-value url 'doc-type) '(:binary :box))
    (net-write-control-line control "TYPE I") (handle-tcp-response control))
  ;; explicitly set the PORT (eventually check *explicit-ftp-port?*)
  (let* ((addr #+mcl (ccl::%tcp-getaddr) #+lispworks (%get-ip-address control))
         (addr-str (format nil "~D,~D,~D,~D"
                           (ldb (byte 8 24) addr)
                           (ldb (byte 8 16) addr)
                           (ldb (byte 8 8) addr)
                           (ldb (byte 8 0) addr)))
         (port (new-ftp-local-data-port))
         (port-str (format nil "~D,~D"
                           (ldb (byte 8 8) port)
                           (ldb (byte 8 0) port))))
    (net-write-control-line control "PORT ~D,~D" addr-str port-str)
    (handle-tcp-response control)))

(defmethod fill-box-using-url ((url ftp-url) box)
  (unwind-protect
    (let ((control (open-tcp-stream (slot-value url 'host)
                                    (slot-value url 'port))))
      (multiple-value-bind (dirs name)
          (destructure-path (slot-value url 'path))
        (unwind-protect
          (progn
            (login-and-cd url control dirs)
            ;; now we are where we want to be
            (cond ((eq (slot-value url 'doc-type) ':directory)
                   (fill-box-from-ftp-nlst url control name box))
                  ((string= name "")
                   (setf (slot-value url 'doc-type) ':directory)
                   (fill-box-from-ftp-nlst url control name box))
                  (t (fill-box-from-ftp-retr url control name box)))
            box)
          ;; be a good citizen
          (net-write-control-line control "QUIT")
          (handle-tcp-response control nil)
          (close control))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
        (boxer::status-line-undisplay 'surf-message))))))

#-lispworks
(defmethod fill-box-from-ftp-retr ((url ftp-url) control-stream name box)
  (let ((data-stream (open-tcp-stream nil (ftp-local-data-port) ;(tcp-stream-local-port control-stream)
                                      :element-type
                                      (if (member (slot-value url 'doc-type)
                                                  '(:box :binary))
                                          '(unsigned-byte 8.)
                                          #+mcl 'base-character
                                          #-(or mcl) 'character)
                                      :timeout *ftp-data-listen-timeout*))
        (try-again :dir))
    (catch 'retry-fill
      (unwind-protect
        (let ((accepted-retr-command? nil))
          (net-write-control-line control-stream "RETR ~A" name)
          (catch 'retry-fill
            (tcp-handler-bind (("550" throw-to-retry-fill-tag))
              ;; this may signal a 550 error if the name is a directory
              (handle-tcp-response control-stream)
              (setq accepted-retr-command? t)))
          ;; see if he RETR command was accepted
          (cond
           ((null accepted-retr-command?)
            ;; if not, try NLST instead of RETR
            (fill-box-from-ftp-nlst-1 url control-stream name box data-stream)
            ;; if succesful, change the url
            (setf (slot-value url 'doc-type) :directory)
            (return-from fill-box-from-ftp-retr box))
           (t (case (slot-value url 'doc-type)
                (:box (let ((b (let ((boxer::*file-system-verbosity* T)
                                     (boxer::*FILE-STATUS-LINE-UPDATE-FUNCTION*
                                      'print-ftp-read-status))
                                 (boxer::load-binary-box-from-stream-internal
                                  data-stream))))
                        (initialize-box-from-net-box box b)
                        (set-url-flags box)))
                (:binary
                 ;; no good model for what to do with random binary data
                 ;; so grab the beginning and at least check to see if
                 ;; it might be a boxer file
                 (let ((1st-word
                        (boxer::read-file-word-from-stream data-stream)))
                   (when (or (=& 1st-word
                                 boxer::bin-op-format-version)
                             (=& 1st-word
                                 boxer::*swapped-bin-op-format-version*))
                     (setq try-again :binary-is-box)
                     (throw 'retry-fill nil))
                   ;; otherwise blah
                   (save-net-data data-stream box :binary)))
                (:text (fill-box-from-text-stream data-stream box)))
               ;; if we have made it this far, mark the try-again flag NIL
               (setq try-again nil))))
        (close data-stream)))
    (if (null try-again)
        ;; no file errors, must have filled the box...
        (return-from fill-box-from-ftp-retr box)
        ;; got a file error, try filling the box by treating the name as a dir
        (cond ((eq try-again ':dir)
               (fill-box-from-ftp-nlst url control-stream name box)
               ;; fix the URL since it was confused (but only if we get pass nlst
               (setf (slot-value url 'doc-type) :directory))
              ((eq try-again ':binary-is-box)
               ;; looks like it's box data so set the doc-type of the url
               ;; so the next try will get it right
               (setf (slot-value url 'doc-type) ':box)
               (fill-box-from-ftp-retr url control-stream name box)))))
  box)

#+lispworks
(progn
  (defvar *net-filling-box* nil)
  (defvar *net-filling-url* nil)
  (defvar *net-filling-lock* (mp:make-lock :name "Net Lock"))
  (defvar *net-filling-status* nil)

  (defmacro with-net-filling-lock (&body body)
    `(mp:with-lock (*net-filling-lock*) . ,body))

  (defmacro with-data-filling (&body body)
    ;; catch lisp errors, boxer errors and interrupts
    (let ((normal-result (gensym)))
      `(catch 'data-fill
         (handler-bind
	     ((error
	       #'(lambda (c)
                   (setq *net-filling-status* c) (throw 'data-fill nil))))
           ;; catch lisp errors and send the condition object to the boxer
           ;; process
           (let ((boxer-eval::*exception-signalled* nil)
                 (boxer-eval::*error-type* nil)
                 (boxer-eval::*error-sfun* nil)
                 (boxer-eval::*exception-args* nil)
                 ;; these are vars that boxer-eval:signal-error will frob, so we bind
                 ;; them in this process and then return them to the boxer process
                 ;; interrupt handling works by signalling a boxer error which
                 ;; will be caught by this scheme and handled generically

                 ;; also need to bind file activity vars that status line
                 ;; reporting uses
                 )
             (catch 'boxer-eval::boxer-primitive-error-signal
               (let ((,normal-result
                      (mp:with-lock (*net-filling-lock*) . ,body)))
                 (cond ((null boxer-eval::*exception-signalled*)
                        (setq *net-filling-status* nil)
                        ,normal-result)
                       (t (setq *net-filling-status* (boxer-eval::encapsulate-error))
                          nil)))))))))

  (defun prepare-net-fill-box (box &optional url)
    (setq *net-filling-box* box
          *net-filling-url* url
          *net-filling-status* nil)
    #+(or lispworks5.1 lispworks6)
    (mp:process-unlock *net-filling-lock*)
    #-(or lispworks5.1 lispworks6)
    ;; (mp:release-lock *net-filling-lock*) sgithens TODO 2020-03-28 this may now be semaphore-release
    (debugging-message "Preparing to fill box:~A" box))

  (defun release-net-fill-box ()
    (setq *net-filling-box* nil *net-filling-url* nil))

  (defun wait-for-net-fill ()
    "Returns T if the data process grabs the lock"
    (debugging-message "Waiting for Net Data Activation")
    (mp:process-wait-with-timeout
     "waiting for lock" 30
     #'(lambda () (not (null (mp:lock-owner *net-filling-lock*)))))
    (cond ((null (mp:lock-owner *net-filling-lock*))
           (debugging-message "Data Process didn't grab lock")
           nil)
          (t
           (debugging-message "Data Process successfully grabbed lock")
           t)))
  )

#+lispworks
(defmethod fill-box-from-ftp-retr ((url ftp-url) control-stream name box)
  (prepare-net-fill-box box url)
  (let* ((port (ftp-local-data-port))
         (data-process (case (slot-value url 'doc-type)
                         (:binary
                          (comm::start-up-server :function 'lw-save-binary-data
                                                 :service port))
                         (:box
                          (comm::start-up-server :function 'lw-fill-box-from-handle
                                                 :service port))
                         (otherwise
                          (comm::start-up-server :function 'lw-fill-box-from-text
                                                 :service port))))
         (trying-nlst nil)
         (accepted-retr-command? nil))
    (net-write-control-line control-stream "RETR ~A" name)
    (catch 'retry-fill
      (tcp-handler-bind (("550" throw-to-retry-fill-tag))
        ;; this may signal a 550 error if the name is a directory
        (handle-tcp-response control-stream)
        (setq accepted-retr-command? t)))
    ;; see if he RETR command was accepted
    (when (null accepted-retr-command?)
      ;; if not, try NLST instead of RETR
      (net-write-control-line control-stream "NLST")
      (handle-tcp-response control-stream)
      (setq trying-nlst t)
      (setq *net-filling-status* :use-nlst))
    ;; at this point, all possible request have been made and
    ;; we should be waiting for data...
    (cond ((wait-for-net-fill)
           ;; OK, looks like the data process is trying to do something...
           (debugging-message "Blocking until lock is released")
           (mp:process-lock *net-filling-lock* "Waiting for data"))
          (t
           ;; do nothing ?  what about warn or error ?
           ;; for now, make sure the box has an empty line
           (when (null (box::first-inferior-row box))
             (append-row box (make-row nil)))))
       (release-net-fill-box)
       (debugging-message  "Killing Data Process")
       (mp:process-kill data-process)
       ;; check status for either lisp or boxer errors
       (cond ((null *net-filling-status*)
              ;; success, we may have to fixup some URL's
              (when trying-nlst
                (setf (slot-value url 'doc-type) :directory))
              box)
             ((typep *net-filling-status* 'error)
              (signal *net-filling-status*))
             ((boxer-eval::error-state-p *net-filling-status*)
              (boxer-eval::signal-error-from-state *net-filling-status*))
             (t (error "Unknown net filling status:~A" *net-filling-status*))))
  box)

;;; ftp RETR data loading functions
#+lispworks
(defun lw-save-binary-data (handle)
  (let ((stream (make-instance 'comm:socket-stream :socket handle :direction :io
                               :element-type 'unsigned-byte)))
    (unwind-protect
        (with-data-filling
;; needs to do the binary-is-box hack
;          (let ((1st-word
;                        (boxer::read-file-word-from-stream data-stream)))
;                   (when (or (=& 1st-word
;                                 boxer::bin-op-format-version)
;                             (=& 1st-word
;                                 boxer::*swapped-bin-op-format-version*))
          (save-net-data stream *net-filling-box* :binary))
      (close stream))))

#+lispworks
(defun lw-fill-box-from-handle (handle)
  (let ((stream (make-instance 'comm:socket-stream :socket handle :direction :io
                               :element-type 'unsigned-byte)))
    (unwind-protect
        (with-data-filling
          (let ((b (let ((boxer::*file-system-verbosity* t)
                         (boxer::*FILE-STATUS-LINE-UPDATE-FUNCTION*
                          'print-ftp-read-status))
                     (boxer::load-binary-box-from-stream-internal stream))))
            (initialize-box-from-net-box *net-filling-box* b)
            (set-url-flags *net-filling-box*)))
      (close stream))))

#+lispworks
(defun lw-fill-box-from-text (handle)
  (let ((stream (make-instance 'comm:socket-stream :socket handle :direction :io
                               :element-type 'base-char)))
    (unwind-protect
        (with-data-filling
          (cond ((eq *net-filling-status* :use-nlst)
                 (lw-fill-box-from-ftp-nlst-2 stream))
                (t
                 (fill-box-from-text-stream stream *net-filling-box*))))
    (close stream))))

(defun fill-box-from-text-stream (stream box)
  (let ((bytes-read 0) (last-byte-count 0))
    (loop (multiple-value-bind (line eof? bytes)
              (net-read-line stream)
            (declare (ignore eof?))
            (cond ((or (null line) (string= "." line)) (return))
                  (t (append-row box (box::make-row-from-string line))
                     (incf bytes-read bytes)
                     ;(incf bytes-read 2) ; do we count CRLF's
                     (when (>& (-& bytes-read last-byte-count) 200)
                       (surf-message "~D bytes read" bytes-read)
                       (setq last-byte-count bytes-read)))))
          (boxer-eval::check-for-interrupt :interrupt "Stopped by User !"))
    (putprop box :text :preferred-file-format)
    box))

#-lispworks
(defmethod fill-box-from-ftp-nlst ((url ftp-url) control-stream dir box)
  (let ((data-stream (open-tcp-stream nil (ftp-local-data-port) ;(tcp-stream-local-port control-stream)
                                      :timeout 300000)))
    (fill-box-from-ftp-nlst-1 url control-stream dir box data-stream)))

(defmethod fill-box-from-ftp-nlst-1 ((url ftp-url) control-stream dir box
                                     data-stream)
  (unwind-protect
    (progn (unless (or (null dir) (string= dir ""))
             (net-write-control-line control-stream "CWD ~A" dir)
             (handle-tcp-response control-stream))
           (net-write-control-line control-stream "NLST")
           (handle-tcp-response control-stream)
           (do ((new-url-path-prefix
                 (let* ((ss (slot-value url 'scheme-string))
                        (type (search ";type=" ss :from-end t))
                        (last-pos (1-& (or type (length ss))))
                        (last-slash (char= #\/ (char ss  last-pos))))
                   (cond ((not (null last-slash)) (subseq ss 0 last-pos))
                         ((not (null type)) (subseq ss 0 type))
                         (t ss))))
                (data-line (net-read-line data-stream)
                           (net-read-line data-stream)))
               ((null data-line)
                ;; make sure the box has at least 1 empty row...
                (when (null (boxer::first-inferior-row box))
                  (append-row box (make-row nil))))
             (debugging-message "FTP dir box from: ~s" data-line)
             (append-row
              box
              (make-row
               (list
                (make-box-from-url
                 (concatenate 'string "ftp:" new-url-path-prefix "/" data-line)
                 'boxer::data-box data-line
                 ;; we can trust the string is already decoded
                 ;; because it is made up of a decode string
                 ;; plus the result of NLST which isn't url encoded
                 t))))
             (boxer-eval::check-for-interrupt :interrupt "Stopped by User !"))
           box)
      (close data-stream)))

#+lispworks
(defmethod fill-box-from-ftp-nlst ((url ftp-url) control-stream dir box)
  (prepare-net-fill-box box url)
  (let ((data-process (comm::start-up-server :function 'lw-fill-box-from-ftp-nlst-1
                                             :service (ftp-local-data-port)
                                             :process-name "NLST Wait")))
     (unwind-protect
         (progn (unless (or (null dir) (string= dir ""))
                  (net-write-control-line control-stream "CWD ~A" dir)
                  (handle-tcp-response control-stream))
           (net-write-control-line control-stream "NLST")
           (handle-tcp-response control-stream)
           (cond ((wait-for-net-fill)
                  ;; OK, looks like the data process is trying to do something...
                  (debugging-message "Blocking until lock is released")
                  (mp:process-lock *net-filling-lock* "Waiting for data"))
                 (t
                  ;; do nothing ?  what about warn or error ?
                  ;; for now, make sure the box has an empty line
                  (when (null (box::first-inferior-row box))
                    (append-row box (make-row nil))))))
       (release-net-fill-box)
       (debugging-message  "Killing Data Process")
       (mp:process-kill data-process))
     ;; check status for either lisp or boxer errors
     (cond ((null *net-filling-status*) box)
           ((typep *net-filling-status* 'error)
            (signal *net-filling-status*))
           ((boxer-eval::error-state-p *net-filling-status*)
            (boxer-eval::signal-error-from-state *net-filling-status*))
           (t (error "Unknown net filling status:~A" *net-filling-status*)))))

#+lispworks
(defun lw-fill-box-from-ftp-nlst-1 (handle)
  (let ((data-stream (make-instance 'comm:socket-stream
                                    :socket handle :direction :io
                                    :element-type 'base-char)))
    (unwind-protect
        (with-data-filling
          (lw-fill-box-from-ftp-nlst-2 data-stream))
      (close data-stream))))

#+lispworks
(defun lw-fill-box-from-ftp-nlst-2 (data-stream)
  (do ((new-url-path-prefix
        (let* ((ss (slot-value *net-filling-url* 'scheme-string))
               (type (search ";type=" ss :from-end t))
               (last-pos (1-& (or type (length ss))))
               (last-slash (char= #\/ (char ss  last-pos))))
          (cond ((not (null last-slash)) (subseq ss 0 last-pos))
                ((not (null type)) (subseq ss 0 type))
                (t ss))))
       (data-line (net-read-line data-stream)
                  (net-read-line data-stream)))
      ((null data-line)
       ;; make sure the box has at least 1 empty row...
       (when (null (boxer::first-inferior-row *net-filling-box*))
         (append-row *net-filling-box* (make-row nil))))
    (debugging-message "FTP dir box from: ~s" data-line)
    (append-row
     *net-filling-box*
     (make-row
      (list
       (make-box-from-url
        (concatenate 'string "ftp:" new-url-path-prefix "/" data-line)
        'boxer::data-box data-line
        ;; we can trust the string is already decoded
        ;; because it is made up of a decode string
        ;; plus the result of NLST which isn't url encoded
        t))))
    (boxer-eval::check-for-interrupt :interrupt "Stopped by User !")))


;;; Note: this doesn't hack the difference between /foo/bar and %2Ffoo/bar
;;; because the string has already been converted.  If this becomes a problem
;;; we will need to fix it in the initialize-url-from-string method for
;;; basic url's first

(defun destructure-path (path)
  (unless (null path)
    (let ((dirs nil)
          (pointer 0))
      (do ((next-slash (position #\/ path :start 1)
                       (position #\/ path :start pointer)))
          ((null next-slash))
        (push (subseq path pointer next-slash) dirs)
        (setq pointer (1+& next-slash)))
      (values (nreverse dirs) (subseq path pointer)))))

;;; FTP FS methods

;; we dump the doc type because that is not neccessarily
;; inferred from the scheme-string
(defmethod dump-plist-internal ((self ftp-url) stream)
  (call-next-method)
  (dump-boxer-thing :doc-type stream)
  (dump-boxer-thing (slot-value self 'doc-type) stream))

(defmethod dump-plist-length ((self ftp-url))
  (+& (call-next-method) 2))

;; Saving via FTP

(defmethod save-box-using-url ((url ftp-url) box)
  (unwind-protect
    (let ((control (open-tcp-stream (slot-value url 'host)
                                    (slot-value url 'port))))
      (multiple-value-bind (dirs name)
          (destructure-path (slot-value url 'path))
        (unwind-protect
          (progn
            (login-and-cd url control dirs)
            ;; now we are where we want to be
            (save-box-via-ftp-stor url control name box))
          ;; be a good citizen
          (net-write-control-line control "QUIT")
          (handle-tcp-response control nil)
          (close control))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
        (boxer::status-line-undisplay 'surf-message))))))

;; hacks for progress reporting

(defmethod writing-stream-position ((stream t)) (declare (ignore stream))  0)

#+mcl
(defmethod writing-stream-position ((stream ccl::tcp-stream))
  (slot-value (slot-value stream 'ccl::conn) 'ccl::bytes-transmitted))

#+carbon-compat
(defmethod writing-stream-position ((stream ccl::opentransport-tcp-stream))
  (ccl::ot-conn-bytes-written (slot-value stream 'ccl::io-buffer)))

#+carbon-compat
(defmethod writing-stream-position ((stream ccl::opentransport-binary-tcp-stream))
  (ccl::ot-conn-bytes-written (slot-value stream 'ccl::io-buffer)))

(defmethod reading-stream-position ((stream t)) (declare (ignore stream)) 0)

#+mcl
(defmethod reading-stream-position ((stream ccl::tcp-stream))
  (slot-value (slot-value stream 'ccl::conn) 'ccl::bytes-received))

#+carbon-compat
(defmethod reading-stream-position ((stream ccl::opentransport-tcp-stream))
  (ccl::ot-conn-bytes-read (slot-value stream 'ccl::io-buffer)))

#+carbon-compat
(defmethod reading-stream-position ((stream ccl::opentransport-binary-tcp-stream))
  (ccl::ot-conn-bytes-read (slot-value stream 'ccl::io-buffer)))

;; should update this to use STOU,RNFR,RNTO instead of STOR
;; also rewrite the :text clause to avoid CONSing row strings
#-lispworks
(defmethod save-box-via-ftp-stor ((url ftp-url) control-stream name box)
  (let ((data-stream (open-tcp-stream nil (ftp-local-data-port) ;(tcp-stream-local-port control-stream)
                                      :element-type
                                      (if (member (slot-value url 'doc-type)
                                                  '(:box :binary))
                                          #-lispworks '(unsigned-byte 8.)
                                          #+lispworks 'unsigned-byte

                                          #+mcl 'base-character
                                          #+lispworks 'base-char
                                          #-(or mcl lispworks) 'character)
                                      :timeout *ftp-data-listen-timeout*)))
    (unwind-protect
      (progn
        (net-write-control-line control-stream "STOR ~A" name)
        (handle-tcp-response control-stream)
        (case (slot-value url 'doc-type)
          ((:box :binary) (boxer::dump-top-level-box-to-stream box data-stream))
          (:text (boxer::do-box-rows ((row box))
                   ;(net-write-line data-stream (boxer::text-string row))
                   (format data-stream "~A" (boxer::text-string row))
                   (write-char #\CR data-stream)
                   (write-char #\LF data-stream))
                 ;; in theory, moving the force-output out of the loop
                 ;; should increase performance
                 ;; NOTE: for a 80K file between ultrix and mac via ethernet
                 ;; difference was 7.2 secs vs. 1 sec
                 (force-output data-stream))))
      (close data-stream))))

#+lispworks
(defmethod save-box-via-ftp-stor ((url ftp-url) control-stream name box)
  (prepare-net-fill-box box url)
  (let* ((port (ftp-local-data-port))
         (data-process (case (slot-value url 'doc-type)
                         ((:box :binary)
                          (comm::start-up-server :function
                                                 'lw-write-box-data-to-handle
                                                 :service port))
                         (otherwise
                          (comm::start-up-server :function
                                                 'lw-write-box-text-to-handle
                                                 :service port)))))
    (net-write-control-line control-stream "STOR ~A" name)
    (handle-tcp-response control-stream)
    ;; at this point, all possible request have been made and
    ;; we should be sending data...
    (cond ((wait-for-net-fill)
           ;; OK, looks like the data process is trying to do something...
           (debugging-message "Blocking until lock is released")
           (mp:process-lock *net-filling-lock* "Waiting for data"))
          (t
           ;; do nothing ?  what about warn or error ?
           ))
    (release-net-fill-box)
    (debugging-message  "Killing Data Process")
    (mp:process-kill data-process)
    ;; check status for either lisp or boxer errors
    (cond ((null *net-filling-status*)) ; success
          ((typep *net-filling-status* 'error)
           (signal *net-filling-status*))
          ((boxer-eval::error-state-p *net-filling-status*)
           (boxer-eval::signal-error-from-state *net-filling-status*))
          (t (error "Unknown net filling status:~A" *net-filling-status*))))
  box)

#+lispworks
(defun lw-write-box-data-to-handle (handle)
  (let ((stream (make-instance 'comm:socket-stream :socket handle :direction :io
                               :element-type 'unsigned-byte)))
    (unwind-protect
        (with-data-filling
          (boxer::dump-top-level-box-to-stream *net-filling-box* stream))
      (close stream))))

#+lispworks
(defun lw-write-box-text-to-handle (handle)
  (let ((stream (make-instance 'comm:socket-stream :socket handle :direction :io
                               :element-type 'base-char)))
    (unwind-protect
        (with-data-filling
          (boxer::do-box-rows ((row *net-filling-box*))
            ;(net-write-line data-stream (boxer::text-string row))
            (format stream "~A" (boxer::text-string row))
            (write-char #\CR stream)
            (write-char #\LF stream))
          ;; in theory, moving the force-output out of the loop
          ;; should increase performance
          ;; NOTE: for a 80K file between ultrix and mac via ethernet
          ;; difference was 7.2 secs vs. 1 sec
          (force-output stream))
      (close stream))))

#|

(defun ftp-ctest (&optional binary? (pass (capi:prompt-for-string "password:")))
  (setq cs (open-tcp-stream "soe.berkeley.edu" 21))
  (format t "OPEN:") (print (net-read-line cs t))
  (net-write-line cs "USER ehl")
  (format t "USER:") (print (net-read-line cs t))
  (net-write-line cs "PASS ~A" pass)
  (format t "PASS:") (print (net-read-line cs t))
  (let* ((addr #+mcl (ccl::%tcp-getaddr) #+lispworks (%get-ip-address cs))
         (addr-str (format nil "~D,~D,~D,~D"
                           (ldb (byte 8 24) addr)
                           (ldb (byte 8 16) addr)
                           (ldb (byte 8 8) addr)
                           (ldb (byte 8 0) addr)))
         (port (new-ftp-local-data-port))
         (port-str (format nil "~D,~D"
                           (ldb (byte 8 8) port)
                           (ldb (byte 8 0) port))))
    (net-write-control-line cs "PORT ~D,~D" addr-str port-str)
    (format t "PORT ~D" port))
  (when binary?
    (net-write-line cs "TYPE I")
    (format t "TYPE I") (print (net-read-line cs t)))
  (print (net-read-line cs t)))

(setq ds (open-tcp-stream nil (ftp-local-data-port)))

(net-write-line cs "RETR .login")

(net-read-line cs t)

(defun ftp-dtest ()
  (format t "~&DS: ~A" ds)
  (when (listen ds)
    (loop (multiple-value-bind (line eof? bytes)
              (net-read-line ds)
            (cond ((or eof? (null line)) (return))
                  (t (format t "~&~D EOF[~A] ~A" bytes eof? line)))))))

(defvar *zzz* nil)

(defun ftp-test (filename &optional binary?)
  (ftp-ctest binary?)
  ;; (mp:release-lock *tl*)  sgithens TODO 2020-03-28 this may now be semaphore-release
  (let ((data-process
         (if binary?
             (comm::start-up-server :function 'ftp-test-grab-box
                                    :service (ftp-local-data-port))
           (comm::start-up-server :function 'ftp-test-print-file
                                  :service (ftp-local-data-port)))))
    (net-write-line cs "RETR ~A" filename)
    (format t "~&Waiting for lock to be grabbed")
    (mp:process-wait-with-timeout "waiting for lock" 30
                                  #'(lambda () (not (null (mp:lock-owner *tl*)))))
    (when (null (mp:lock-owner *tl*)) (warn "Lock wasn't grabbed"))
    (format t "~&Blocking until lock is released")
    (mp:process-lock *tl* "Waiting for data")
    (format t "~&Killing process")
    (mp:process-kill data-process))
  (net-write-line cs "QUIT")
  (loop (cond ((listen cs) (print (net-read-line cs)))
              (t (return)))))

(defun ftp-test-print-file (handle)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket handle :direction :io
                               :element-type 'base-char)))
    (mp:with-lock (*tl*)
      (ftp-foo stream)
    ;(mp::process-run-function "ftp test print" nil 'ftp-foo stream)
      )
    ))

(defun ftp-foo (stream)

  (loop (let ((line (net-read-line stream)))
          (cond ((null line) (close stream) (return))
                (t ;(print line)
                 (setq *lines* (append *lines* (list line)))
                 (format *standard-output* "~&~A" line)
                 )))))

(defun ftp-test-grab-box (handle)
  (let ((stream (make-instance 'comm::socket-stream
                               :socket handle :direction :io
                               :element-type 'unsigned-byte)))
    (mp:with-lock (*tl*)
      (ftp-bar stream))))

(defun ftp-bar (stream)
  (setq *test-box* (box::load-binary-box-from-stream-internal stream)))

;; try
(setq *tl* (mp:make-lock :name "Net Lock")
      *lines* nil)

(ftp-test ".login")
;or
(ftp-test

|#



;;;;
;;;; FILE: gcmeth.lisp
;;;;

;; from defmethod stamp
      #-opengl
      (with-graphics-screen-parameters
        (let ((*currently-moving-sprite* :go-ahead-and-draw-anyway))
          ;; shouldn't need this binding anymore now that this mechanism
          ;; is limited to ONLY inside the with-sprite-prim-env macro
          (draw self ; pen-mode   need to figure out how to set up state info
                ; for the drawing of the shape....
                )))
      #+opengl

;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version stamps the rectangle in turtle coordinates
;;; NOT array/window coordinates
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod turtle-rect ((self graphics-cursor) wid hei
                                               &optional (orientation :centered))
  (declare (ignore orientation))	; a hook for later
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-centered-rectangle
        (x-position self) (y-position self)
        (coerce wid 'boxer-float) (coerce hei 'boxer-float)))
      ((not (no-graphics?))
       (let ((abs-x (absolute-x-position self))
             (abs-y (absolute-y-position self)))
         (record-boxer-graphics-command-centered-rectangle
          abs-x abs-y wid hei)
         (with-graphics-screen-parameters
           (centered-rectangle abs-x abs-y wid hei)))))))

;;;;
;;;; FILE: gcmeth.lisp
;;;;

;; sgithens 2021-07-23 This was another version of the :COMMAND-BODY section for
;;   (defstandard-graphics-handlers (line-segment 3)
    #|(let ((sx0 (scale-x x0)) (sy0 (scale-y y0))
                              (sx1 (scale-x x1)) (sy1 (scale-y y1)))
        (%draw-line (ensure-legal-window-coordinate sx0)
                    (ensure-legal-window-coordinate sy0)
                    (ensure-legal-window-coordinate sx1)
                    (ensure-legal-window-coordinate sy1)
                    *graphics-state-current-alu* t %drawing-array))|#

#|
;;; old (non caching) implementation
;;; check out the :sprite-xxx args which used to expand into calls to
;;; defgraphics-handler
(defstandard-graphics-handlers (line-segment 3)
    :COMMAND-ARGS (x0 y0 x1 y1)
    :EXTENTS-FORM (values (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1))
    :TRANSFORMATION-TEMPLATE
    (:x-transform :y-transform :x-transform :y-transform)
    :COMMAND-BODY
    (%draw-line (scale-x x0) (scale-y y0) (scale-x x1) (scale-y y1)
    *graphics-state-current-alu* t %drawing-array)
    :TRANSLATION-ARGS
    (trans-x trans-y)
    :TRANSLATION-BODY
    (progn (set-x0 (+& x0 trans-x)) (set-y0 (+& y0 trans-y))
     (set-x1 (+& x1 trans-x)) (set-y1 (+& y1 trans-y)))
    ;; translation and scaling
    :TRANSLATION-AND-SCALING-ARGS
    (trans-x trans-y scale-x scale-y)
    :TRANSLATION-AND-SCALING-BODY
    (progn (set-x0 (+& (fixr (* x0 scale-x)) trans-x))
     (set-y0 (+& (fixr (* y0 scale-y)) trans-y))
     (set-x1 (+& (fixr (* x1 scale-x)) trans-x))
     (set-y1 (+& (fixr (* y1 scale-y)) trans-y)))
    ;; sprite shape drawing
    :SPRITE-HANDLER-ARGS
    (trans-x trans-y cos-scale sin-scale scale)
    :SPRITE-HANDLER-BODY
    (line-segment (fixr (+ trans-x (+ (* cos-scale x0) (* sin-scale y0))))
      (fixr (+ trans-y (- (* sin-scale x0) (* cos-scale y0))))
      (fixr (+ trans-x (+ (* cos-scale x1) (* sin-scale y1))))
      (fixr (+ trans-y (- (* sin-scale x1) (* cos-scale y1)))))
    :SPRITE-EXTENTS-BODY
    (let ((start-x (fixr (+ trans-x (+ (* cos-scale x0) (* sin-scale y0)))))
    (start-y (fixr (+ trans-y (- (* sin-scale x0) (* cos-scale y0)))))
    (stop-x  (fixr (+ trans-x (+ (* cos-scale x1) (* sin-scale y1)))))
    (stop-y  (fixr (+ trans-y (- (* sin-scale x1) (* cos-scale y1))))))
      (values (min& start-x stop-x) (min& start-y stop-y)
        (max& start-x stop-x) (max& start-y stop-y))))

|#



;;;;
;;;; FILE: gopher.lisp
;;;;
;;;; --entire-file--

;;;;    Boxer
;;;;    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                      +-Data--+
;;;;             This file is part of the | BOXER | system
;;;;                                      +-------+
;;;;
;;;;
;;;;     Current implementations for Gopher
;;;;

(in-package :boxnet)

(defclass gopher-url
  (net-url)
  (;; This should be a keyword based on the defined gopher types (RFC 1436 pg 10)
   ;; or else :box if we can infer that it is a boxer file
   (doc-type :initform ':text :accessor gopher-url-doc-type :initarg :doc-type))
  ;; (:metaclass block-compile-class)
  )

;;;; Gopher

;; From RFC 1738 pg 9-10:
;; Gopher URL takes the form:
;;    gopher://<host>:<port>/<gopher-path>
;; where <gopher-path> is one of
;;    <gophertype><selector>
;;    <gophertype><selector>%09<search>
;;    <gophertype><selector>%09<search>%09<gopher+_string>
;; if :<port> is ommitted, the port defaults to 70.  <gophertype> is a
;; single-character field to denote the gopher type of the resource to
;; which the URL refers.  The entire <gopher-path> may also be empty, in
;; which case the delimiting "/" is also optional and the <gophertype>
;; defaults to "1".

;; The defined gopher types from RFC 1436 pg 10-11 are:
;; 0 Item is a File
;; 1 Item is a Directory
;; 2 Item is a CSO phone book server
;; 3 Error
;; 4 Item is a binHExed Macintosh file
;; 5 Item is a DOS binary archive
;; 6 Item is a UNIX uuencoded file
;; 7 Item is a index-Search server
;; 8 Item points to a text based telnet server
;; 9 Item is a binary file !
;; + Item is a redundant server
;; T Item points to a text based tn3270 session
;; g Item is a GIF format graphics file
;; I Item is some kind of image file.  Client decides how to display.
;;
;; additional types from the veronica FAQ
;;         s  -- Sound
;;         e  -- Event    (not in 2.06)
;;         M  -- MIME multipart/mixed message
;;         h  -- HTML, HyperText Markup Language

;; Characters '0' through 'Z' are reserved.


;; this should check for :binary type and ".box" in the path
;; and set the doc-type accordingly
(defmethod initialize-instance ((url gopher-url) &rest initargs)
  (call-next-method)
  (when (null (slot-value url 'port))
    (setf (slot-value url 'port) 70))
  ;; we should be able to infer the data type from the gopher path
  (let ((path (slot-value url 'path)))
    (cond ((null path)
           (setf (slot-value url 'path) "")
           (setf (slot-value url 'doc-type) :directory))
          (t
           (setf (slot-value url 'doc-type) (gopher-type (aref path 0)))
           (setf (slot-value url 'path) (subseq path 1))))))

(defun gopher-type (type-char)
  (case type-char
    (#\0 :text) (#\1 :directory) (#\2 :cso-phone-server)
    (#\3 :error) (#\4 :binhex) (#\5 :dosbin) (#\6 :uuencode)
    (#\7 :index-search) (#\8 :telnet) (#\9 :binary)
    (#\+ :redundant-server) (#\T :tn3270)
    (#\g :gif) (#\I :image)
    (otherwise :unknown)))

(defmethod copy-url ((url gopher-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'doc-type) (slot-value url 'doc-type))
    new))

(defmethod fill-box-using-url ((url gopher-url) box)
  (unwind-protect
    (let* ((doc-type (slot-value url 'doc-type))
           (stream (open-tcp-stream (slot-value url 'host) (slot-value url 'port)
                                    :element-type
                                    (if (member doc-type
                                                '(:dosbin :binary :gif :image))
                                      #-lispworks '(unsigned-byte 8)
                                      #+lispworks 'unsigned-byte

                                      #+mcl 'base-character
                                      #+lispworks 'base-char
                                      #-(or mcl lispworks) 'character))))
      (unwind-protect
        (bw::with-mouse-cursor (:file-io)
          (case doc-type
            (:text (net-write-line stream (slot-value url 'path))
                   (fill-box-from-text-stream stream box))
            (:directory (net-write-line stream (slot-value url 'path))
                        (read-gopher-directory stream box))
            (:error (append-row box (make-row '("Gopher Error")))
                    (append-row box (make-row (list (slot-value url 'host)
                                                    (slot-value url 'port))))
                    (append-row box (make-row (list (slot-value url 'path)))))
            (:index-search (cond ((find #\tab (slot-value url 'path))
                                  ;; this is a search path with a search
                                  ;; string already provided
                                  (net-write-line stream (slot-value url 'path))
                                  (read-gopher-directory stream box))
                                 (t
                                  ;; no search string specified so setup
                                  ;; a way for the user to type one in...
                                  (make-index-search-box (slot-value url 'host)
                                                         (slot-value url 'port)
                                                         (slot-value url 'path)
                                                         box))))
            ((:uuencode :binhex)
             (net-write-line stream (slot-value url 'path))
             (save-net-data stream box doc-type))
            ((:image :gif)
             (net-write-line-to-binary-stream stream (slot-value url 'path))
             (let ((path (temp-pathname doc-type)))
               (save-net-data stream box doc-type nil path)
               (start-graphics-viewer path)))
            ((:dosbin :binary)
             (net-write-line-to-binary-stream stream (slot-value url 'path))
             (save-net-data stream box doc-type))
            (otherwise (fill-unhandled-gopher-type url box))))
        (close stream)))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
        (boxer::status-line-undisplay 'surf-message))))

(defun temp-pathname (doc-type)
  (make-pathname :host "home" :name (format nil "~A~A" doc-type (gensym))))

(defun start-graphics-viewer (graphics-file)
  #+mcl
  (ccl::select-process-with-docs :JVWR graphics-file))


(defmethod fill-unhandled-gopher-type ((url gopher-url) box)
  (append-row box (make-row (list "No" "defined" "handler" "for")))
  (append-row box (make-row (list "gopher" "type" (slot-value url 'doc-type)))))

(defun read-gopher-directory (tcp-stream box)
  (do ((line (net-read-line tcp-stream) (net-read-line tcp-stream)))
      ((string= line ".") box)
    (multiple-value-bind (raw-type display-string selector-string host port)
        (decode-gopher-line line)
      (append-row box
                  (make-row
                   (list
                    (case raw-type
                      (:index-search
                       (make-index-search-box host port selector-string))
                      (otherwise
                       (make-box-from-url (format nil "gopher://~A:~D/~C~A"
                                                  host port (aref line 0)
                                                  selector-string)
                                          'boxer::data-box
                                          display-string t)))))))))

;; special types

;; for the terminal emulation types, we should should launch the
;; appropriate application
(defun make-terminal-box (host)
  (make-box `(("Entering this box will start a telnet session to:")
              (,host)
              (,(make-box `(("telnet" ,(make-box `((,host)))))
                          'boxer::doit-box "Entry-Trigger")))))

;; not quite right yet.  NCSA telnet doesn't open the connection right away
(eval-when (compile load eval)
(defboxer-primitive bu::telnet (host)
  #+mcl
  (let ((ncsa-pathname (make-pathname :host "home"
                                      :name "NCSA-telnet-temp")))
    (unwind-protect
        (progn (make-NCSA-telnet-doc ncsa-pathname (box-text-string host))
               (ccl::select-process-with-docs :NCSA ncsa-pathname))
      (delete-file ncsa-pathname)))
  boxer-eval::*novalue*)
)

(defun make-NCSA-telnet-doc (pathname host)
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (format s "commandkeys = yes~&name= \"~A\"~&host= \"~A\"~&~
               scrollback= 200~&erase = backspace~&size = {40,5,320,504}~&~
               vtwidth = 80~&tekclear = yes~&rgb0 = {0,0,0}~&~
               rgb1 = {65535,65535,65535}~&rgb2 = {0,61183,11060}~&~
               rgb3 = {61183,2079,4938}~&font = \"Monaco\"~&fsize= 9~&~
               nlines= 24~&keystop= -1~&keygo= -1~&keyip= -1~&crmap= 0~&~
               tekem= -1~&answerback= \"VT100\"~&"
            host host))
  #+mcl (ccl::set-mac-file-creator pathname :NCSA)
  #+mcl (ccl::set-mac-file-type    pathname :CONF))

;; This may change depending on how we want to handle searches
;; the box arg is provided if we come here via fill-box-using-url
(defun make-index-search-box (host port select-string &optional box)
  (let ((fun (make-box (list '(input "Search-String")
                             (list 'gopher-search (make-box `((,host))) port
                                   (make-box `((,select-string)))
                                   "Search-String"))
                       'boxer::doit-box
                       "Query-Gopher-Server")))
    (shrink fun)
    (if (null box)
        (make-box `((,fun)
                    ("Query-Gopher-Server" "Search-String:" ,(make-box '(())))))
        (progn
          (append-row box (make-row (list fun)))
          (append-row box (make-row `("Query-Gopher-Server" "Search-String:"
                                      ,(make-box '(())))))))))


(defboxer-primitive bu::gopher-search (host (boxer-eval::numberize port)
                                            select-string search-string)
  (let* ((realhost (box-text-string host))
         (real-select-string (box-text-string select-string))
         (real-search-string (box-text-string search-string))
         (stream (open-tcp-stream realhost port)))
    (unwind-protect
        (let ((return-box (boxer::make-uninitialized-box 'boxer::data-box)))
          (shared-initialize return-box t)
          (if (or (null select-string) (string= "" real-select-string))
              (net-write-line stream "~A" real-search-string)
              (net-write-line stream "~A~C~A"
                              real-select-string #\tab real-search-string))
          (read-gopher-directory stream return-box)
          return-box)
      (close stream))))

(defun decode-gopher-line (string)
  (let ((idx 1)
        (type (gopher-type (aref string 0)))
        (display nil)
        (select nil)
        (host nil))
    (setq display (subseq string idx
                          (setq idx (position #\tab string
                                              :test #'char=))))
    (incf idx) ; move the idx past the #\tab
    (setq select (subseq string idx
                         (setq idx (position #\tab string
                                             :test #'char= :start idx))))
    (incf idx)
    (setq host (subseq string idx
                         (setq idx (position #\tab string
                                             :test #'char= :start idx))))
    (incf idx)
    (when (string= "" display)
      ;; perhaps we should try and make something out of the select-string ??
      (setq display nil))
    (values type display select host (string->number (subseq string idx)))))


#|
  (do ((line (net-read-line stream) (net-read-line stream)))
      ((or (null line) (string= "." line)) box)
    (append-row box (box::make-row-from-string line)) ;; should we check for ".."
    (boxer-eval::check-for-interrupt :interrupt "Stopped by User !"))
|#

(defmethod dump-plist-internal ((self gopher-url) stream)
  (call-next-method)
  (dump-boxer-thing :doc-type stream)
  (dump-boxer-thing (slot-value self 'doc-type) stream))

(defmethod dump-plist-length ((self gopher-url))
  (+& (call-next-method) 2))


;;;;
;;;; FILE: grfdfs.lisp
;;;;

(defvar *prepared-graphics-box* nil)
(defvar *sprites-hidden* nil)


;;; just in case...

(defmacro with-sprites-hidden (draw-p &body body)
  (declare (ignore draw-p body))
  (warn
   "WITH-SPRITES-HIDDEN being called outside of WITH-GRAPHICS-VARS-BOUND")
  `(error
    "WITH-SPRITES-HIDDEN being called outside of WITH-GRAPHICS-VARS-BOUND"))

(defmacro with-sprites-hidden-always (draw-p &body body)
  (declare (ignore draw-p body))
  (warn
   "WITH-ALL-SPRITES-HIDDEN being used outside of WITH-GRAPHICS-VARS-BOUND")
  `(error
    "WITH-ALL-SPRITES-HIDDEN being used outside of WITH-GRAPHICS-VARS-BOUND"))


;; was in with-graphics-vars-bound-internal macrolet
       ;; this macro is shadowed in with-sprite-primitive-environment
       ;; they are here for the benefit of graphics functions which
       ;; which do not use the sprite interface sprite
      (with-sprites-hidden (draw-p &body body)
                    ;; mostly a stub, we might want to use the command-can-draw?
                    ;; arg in the future as a hint to propagate MODIFIED info
                    `(progn
                    ;    (when ,draw-p (queue-modified-graphics-box
                    ;                   (graphics-sheet-superior-box ,',gr-sheet)))
                       (progn . ,body)))
      ;		    (declare (ignore draw-p))
      ;		    `(unwind-protect
      ;			  (progn
      ;			    (dolist (ttl (graphics-sheet-object-list
      ;					  ,',gr-sheet))
      ;			      (when (shown? ttl) (fast-erase ttl)))
      ;			    . ,body)
      ;		       ;; All the turtles' save-unders
      ;		       ;; have to updated since the moving turtle may
      ;		       ;; have drawn on the portion of the screen
      ;		       ;; where they are.
      ;		       (dolist (ttl (graphics-sheet-object-list ,',gr-sheet))
      ;			 (save-under-turtle ttl))
      ;		       (dolist (ttl (graphics-sheet-object-list ,',gr-sheet))
      ;			 (when (shown? ttl) (draw ttl))))))


;; was in with-sprite-primitive-environment macrolet
         (WITH-SPRITES-HIDDEN (command-can-draw? &body body)
                       ;; mostly a stub, we might want to use the command-can-draw?
                       ;; arg in the future as a hint to propagate MODIFIED info
		       `(let* (
				   		; (draw-p (and ,command-can-draw?
                        ;                     (not (eq (pen ,',turtle-var) 'bu::up))))
											)
                        ;   (when draw-p (queue-modified-graphics-box ,',gboxvar))
                          (progn . ,body)))

;; 2022-04-20 with-graphics-screen-parameters-once doesn't seem to be actively used anywhere
;; anymore, only in primitives that have been archived here

;;; This is like the With-Graphics-Screen-Parameters Except that
;;; the body is only executed once (or not at all) on the "most
;;; acceptable screen-box". "Most appropriate" is defined as
;;; not clipped or if they are ALL clipped, the largest.

(defmacro with-graphics-screen-parameters-once (&body body)
  `(let ((best-screen-box nil))
     (unless (no-graphics?)
       (dolist (screen-box (get-visible-screen-objs %graphics-box))
	 (cond ((eq ':shrunk (display-style screen-box)))
               ((not (graphics-screen-box? screen-box)))
	       ((and (not (screen-obj-x-got-clipped? screen-box))
		     (not (screen-obj-y-got-clipped? screen-box)))
		(setq best-screen-box screen-box)
		(return))
	       ((null best-screen-box)
		(setq best-screen-box screen-box))
	       (t
		;; If we get to here, then both the best-screen-box
		;; and the current screen-box are clipped so pick the
		;; bigger one.  We could be a little more sophisticated
		;; about how we choose...
		(cond ((screen-obj-y-got-clipped? best-screen-box)
		       (cond ((null (screen-obj-y-got-clipped? screen-box))
			      (setq best-screen-box screen-box))
			     ((> (screen-obj-hei screen-box)
				  (screen-obj-hei best-screen-box))
			      (setq best-screen-box screen-box))))
		      ((screen-obj-y-got-clipped? screen-box)
		       ;;if the current box is clipped but the best one
		       ;; isn't, then leave things alone
		       )
		      ((screen-obj-x-got-clipped? best-screen-box)
		       (cond ((null (screen-obj-x-got-clipped? screen-box))
			      (setq best-screen-box screen-box))
			     ((> (screen-obj-wid screen-box)
				  (screen-obj-wid best-screen-box))
			      (setq best-screen-box screen-box))))))))
       (unless (null best-screen-box)
	 (drawing-on-turtle-slate best-screen-box ,@body)))))

(defvar *scrunch-factor* 1
  "the factor used to normalize the Y-coordinates so that squares really are")

;; Note 2022-04-20 This scrunch factor was used in the following versions of things
;; in grobjs.lisp. Reminds me of the Amiga which didn't have square pixels...
(defun array-coordinate-y (user-y)
  (float-minus %drawing-half-height
               (float user-y) ; (* user-y *scrunch-factor*)
               ))

(defun user-coordinate-y (array-y)
  ;(/ (- %drawing-half-height array-y) *scrunch-factor*)
  (float-minus %drawing-half-height (float array-y)))

(defun user-coordinate-fix-y (array-y)
  (declare (fixnum array-y))
  ;(/ (- %drawing-half-height array-y) *scrunch-factor*)
  (float-minus %drawing-half-height (float array-y)))

;; End scrunch-factor usage examples from grobjs.lisp

(defvar *minimum-graphics-dimension* 15
  "The smallest you can make a graphics box")

;;; When a turtle is created, all of the boxes corresponding
;;; to instance variables have also been created.  What needs
;;; to be done here is to put the boxes inthe right place
;;; At this time, that means the x-position, y-position and
;;; heading boxes are in the box proper and all the other slots
;;; live in the closet of the sprite-box.


(defvar *initially-visible-sprite-instance-slots*
	'(x-position y-position heading))


(defvar *turtle-slots-that-need-boxes*
	'(x-position y-position heading
		     shown? pen
		     home-position sprite-size shape))

(defvar *name-flashing-pause-time* 2.
  "Time in Seconds to Pause when flashing the name of a Sprite")

(defvar %new-shape nil "The new shape vectors are collected here when doing a set-shape")


(defun save-under-turtle (turtle)
  (let ((save-under (turtle-save-under turtle)))
    (cond ((eq save-under 'xor-redraw))
	  (t
	   (when (null save-under)
	     (warn "The turtle, ~S, does not have a backing store" turtle)
	     (update-save-under turtle)
	     (setq save-under (turtle-save-under turtle)))
           (multiple-value-bind (minx miny maxx maxy)
               (enclosing-rectangle turtle)
             (let* ((gs (graphics-info (slot-value turtle 'assoc-graphics-box)))
                    (mode (graphics-sheet-draw-mode gs))
                    (sub (save-under-bitmap save-under)))
               (flet ((save-corner-wrap (lefx rigx topy boty)
                        (bitblt-from-screen lefx topy sub 0 0 0 0)
                        (bitblt-from-screen (-& %drawing-width rigx) topy
                                            sub rigx 0 lefx 0)
                        (bitblt-from-screen lefx (-& %drawing-height boty)
                                            sub 0 boty 0 topy)
                        (bitblt-from-screen (-& %drawing-width rigx)
                                            (-& %drawing-height boty) sub
                                            rigx boty lefx topy))
                      (save-horiz-wrap (lefx rigx)
                        (let* ((cminy (max& miny 0))
                               (cmaxy (min& maxy %drawing-height))
                               (hei (-& cmaxy cminy)))
                          (bitblt-from-screen lefx hei sub 0 cminy 0 0)
                          (bitblt-from-screen (-& %drawing-width rigx) hei
                                              sub rigx cminy lefx 0)))
                      (save-vert-wrap (topy boty)
                        (let* ((cminx (max& minx 0))
                               ;; clipped vars...
                               (cmaxx (min& maxx %drawing-width))
                               (wid (-& cmaxx cminx)))
                          (bitblt-from-screen wid topy sub cminx 0 0 0)
                          (bitblt-from-screen wid (-& %drawing-height boty)
                                              sub cminx boty 0 topy))))
                 ;; should we check for :wrap mode ?
                 ;; what if the user moves to the edge and then changes modes ?
                 ;; for now, always save info to cover this case
                 ;;
                 ;; There are potentially 8 wrapping cases, we search for wrapping
                 ;; cases as follows
                 ;; top-left, top-right, top
                 ;; bottom-left, bottom-right, bottom
                 ;; left, then right
                 ;; also remember that some sprites can be larger than
                 ;; the graphics box itself
                 (cond ((eq mode :clip)
                        (unless (or (<=& maxx 0) (>=& minx %drawing-width)
                                    (<=& maxy 0) (>=& miny %drawing-height))
                          ;; unless completely off the graphics box
                          ;; just draw what's visible
                          (let* ((cminx (max& minx 0)) (cminy (max& miny 0))
                                 (cmaxx (min& maxx %drawing-width))
                                 (cmaxy (min& maxy %drawing-height))
                                 (cwid (-& cmaxx cminx)) (chei (-& cmaxy cminy)))
                            (unless (or (zerop& cwid) (zerop& chei))
                              (bitblt-from-screen cwid chei sub
                                                  cminx cminy 0 0)))))
                       ((minusp& miny)
                        ;; top wrap, bind useful vars and check for corners
                        (let ((topy (min& maxy %drawing-height))
                              (boty (max& 0 (+& miny %drawing-height))))
                          (cond ((minusp& minx)
                                 ;; top left corner
                                 (save-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; top right corner
                                 (save-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the top
                                 (save-vert-wrap topy boty)))))
                       ((>& maxy %drawing-height)
                        ;; bottom wrap, bind useful vars and check for corners
                        (let ((topy (min& (-& maxy %drawing-height)
                                          %drawing-height))
                              (boty (max& miny 0)))
                          (cond ((minusp& minx)
                                 ;; bottom left corner
                                 (save-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; bottom right corner
                                 (save-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the bottom
                                 (save-vert-wrap topy boty)))))
                       ((minusp& minx)
                        ;; horizontal left wrap case
                        (save-horiz-wrap (min& maxx %drawing-width)
                                         (max& 0 (+& minx %drawing-width))))
                       ((>& maxx %drawing-width)
                        ;; horizontal right wrap case
                        (save-horiz-wrap (min& (-& maxx %drawing-width)
                                               %drawing-width)
                                         (max& minx 0)))
                       (t ;; vanilla case
                        (bitblt-from-screen
                                            (-& (min& maxx %drawing-width)
                                                (max& minx 0))
                                            (-& (min& maxy %drawing-height)
                                                (max& miny 0))
                                            sub
                                            (max& minx 0) (max& miny 0)
                                            0 0))))))))))

(defun restore-under-turtle (turtle)
  (let ((save-under (turtle-save-under turtle)))
    (cond ((eq save-under 'xor-redraw))
	  (t
	   (when (null save-under)
	     (cerror "Make a Save Under"
		     "The turtle, ~S, does not have a backing store" turtle)
	     (update-save-under turtle))
           (multiple-value-bind (minx miny maxx maxy)
               (enclosing-rectangle turtle)
             (let* ((gs (graphics-info (slot-value turtle 'assoc-graphics-box)))
                    (mode (graphics-sheet-draw-mode gs))
                    (sub (save-under-bitmap save-under)))
               (flet ((restore-corner-wrap (lefx rigx topy boty)
                        (bitblt-to-screen lefx topy sub 0 0 0 0)
                        (bitblt-to-screen (-& %drawing-width rigx) topy
                                            sub lefx 0 rigx 0)
                        (bitblt-to-screen lefx (-& %drawing-height boty)
                                            sub 0 topy 0 boty)
                        (bitblt-to-screen (-& %drawing-width rigx)
                                            (-& %drawing-height boty) sub
                                            lefx topy rigx boty))
                      (restore-horiz-wrap (lefx rigx)
                        (let* ((cminy (max& miny 0))
                               (cmaxy (min& maxy %drawing-height))
                               (hei (-& cmaxy cminy)))
                          (bitblt-to-screen lefx hei sub 0 0 0 cminy)
                          (bitblt-to-screen (-& %drawing-width rigx) hei
                                              sub lefx 0 rigx cminy)))
                      (restore-vert-wrap (topy boty)
                        (let* ((cminx (max& minx 0))
                               ;; clipped vars...
                               (cmaxx (min& maxx %drawing-width))
                               (wid (-& cmaxx cminx)))
                          (bitblt-to-screen wid topy sub 0 0 cminx 0)
                          (bitblt-to-screen wid (-& %drawing-height boty)
                                              sub 0 topy cminx boty))))
                 ;; should we check for :wrap mode ?
                 ;; what if the user moves to the edge and then changes modes ?
                 ;; for now, always save info to cover this case
                 ;;
                 ;; There are potentially 8 wrapping cases, we search for wrapping
                 ;; cases as follows
                 ;; top-left, top-right, top
                 ;; bottom-left, bottom-right, bottom
                 ;; left, then right
                 ;; also remember that some sprites can be larger than
                 ;; the graphics box itself
                 (cond ((eq mode :clip)
                        (unless (or (<=& maxx 0) (>=& minx %drawing-width)
                                    (<=& maxy 0) (>=& miny %drawing-height))
                          ;; unless completely off the graphics box
                          ;; just draw what's visible
                          (let* ((cminx (max& minx 0)) (cminy (max& miny 0))
                                 (cmaxx (min& maxx %drawing-width))
                                 (cmaxy (min& maxy %drawing-height))
                                 (cwid (-& cmaxx cminx)) (chei (-& cmaxy cminy)))
                            (unless (or (zerop& cwid) (zerop& chei))
                              (bitblt-to-screen cwid chei sub
                                                0 0 cminx cminy)))))
                       ((minusp& miny)
                        ;; top wrap, bind useful vars and check for corners
                        (let ((topy (min& maxy %drawing-height))
                              (boty (max& 0 (+& miny %drawing-height))))
                          (cond ((minusp& minx)
                                 ;; top left corner
                                 (restore-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; top right corner
                                 (restore-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the top
                                 (restore-vert-wrap topy boty)))))
                       ((>& maxy %drawing-height)
                        ;; bottom wrap, bind useful vars and check for corners
                        (let ((topy (min& (-& maxy %drawing-height)
                                          %drawing-height))
                              (boty (max& miny 0)))
                          (cond ((minusp& minx)
                                 ;; bottom left corner
                                 (restore-corner-wrap (min& maxx %drawing-width)
                                                   (max& 0 (+& minx %drawing-width))
                                                   topy boty))
                                ((>& maxx %drawing-width)
                                 ;; bottom right corner
                                 (restore-corner-wrap (min& (-& maxx %drawing-width)
                                                         %drawing-width)
                                                   (min& minx %drawing-width)
                                                   topy boty))
                                (t ;; must be just the bottom
                                 (restore-vert-wrap topy boty)))))
                       ((minusp& minx)
                        ;; horizontal left wrap case
                        (restore-horiz-wrap (min& maxx %drawing-width)
                                         (max& 0 (+& minx %drawing-width))))
                       ((>& maxx %drawing-width)
                        ;; horizontal right wrap case
                        (restore-horiz-wrap (min& (-& maxx %drawing-width)
                                               %drawing-width)
                                         (max& minx 0)))
                       (t ;; vanilla case
                        (bitblt-to-screen
                                            (-& (min& maxx %drawing-width)
                                                (max& minx 0))
                                            (-& (min& maxy %drawing-height)
                                                (max& miny 0))
                                            sub
                                            0 0 (max& minx 0) (max& miny 0)))))))))))

;;; The active sprite list lives on the Plist of the Box (for now)
;(defun cache-active-sprites (box sprites)
;  ;; arg check should happen higher up at the Boxer interface level
;  (putprop box sprites 'active-sprites))

;;; should cache the sprite info in the WHO box to avoid having to
;;; walk throught the the box everytime.  Need to figure out a
;;; fast way to flush the cache (that doesn't slow down ALL boxes)

#| no longer used since WHO mechanism got flushed

(defun extract-sprites (box)
  (let ((sprites nil))
    (multiple-value-bind (rows ignore1 ignore2 vcrs)
	(if (virtual-copy? box) (vc-rows box) (virtual-copy-rows box))
      (declare (ignore ignore1 ignore2))
      (dolist (evrow rows)
	(dolist (ptr (evrow-pointers evrow))
	  (let ((value (access-evrow-element (or vcrs box) ptr)))
	    (cond ((virtual-port? value)
		   (push (vp-target value) sprites))
		  ((port-box? value)
		   (push (ports value) sprites))
		  (t (error "~A was not a port" value)))))))
    sprites))
|#

;;; this should use the pos-cache....
(defmacro with-turtle-slate-origins (screen-box &body body)
  ;; this macro sets x and y coordinates of top left of turtle array
  ;; not that the a SCREEN-SHEET may NOT have been allocated if this has
  ;; been called BEFORE Redisplay has had a chance to run
  `(let* ((screen-sheet (when ,screen-box (screen-sheet ,screen-box)))
	  (gs (graphics-screen-sheet-actual-obj screen-sheet)))
     (unless (null screen-sheet)
       (multiple-value-bind (box-x-offset box-y-offset)
	   (xy-position ,screen-box)
	 (multiple-value-bind (sheet-x sheet-y)
	     (graphics-screen-sheet-offsets screen-sheet)
	   (with-origin-at ((+& box-x-offset sheet-x)
			    (+& box-y-offset sheet-y))
	     . ,body))))))



;; switch this to use ONLY symbols in the BOXER package
(defvar *allowed-alus* '(up down erase xor :up :erase :down :xor))

(defun vlist-alu? (thing)
  ;; complain about bad alus but allow them for now
  (cond ((fast-memq thing '(:up :erase :down :xor))
	 (warn "~%The ALU, ~A, is not a valid alu, use symbols in the BOXER package" thing)
	 t)
	(t
	 (fast-memq thing *allowed-alus*))))



#|
(defmacro defsprite-update-function (name-descriptor arglist
				     (sprite-var turtle-var slot-name)
				     &body body)
  `(progn
     (when (and (symbolp ',name-descriptor)
		(not (fast-memq ',name-descriptor *sprite-update-functions*)))
       (push ',name-descriptor *sprite-update-functions*))
     (setf (get ',slot-name 'update-function-name) ',name-descriptor)
     (defsprite-function ,name-descriptor ,arglist (,sprite-var ,turtle-var)
       . ,body)))
|#

;; defined in vars.lisp
;; (defvar %learning-shape-graphics-list nil)

#| (defvar *hide-ALL-sprites-when-drawing* t) |#

;; This is bound by the erase/redraw pass in With-Sprite-Primitive-Environment
;; to the active sprite.  The possibility of the moving sprite being a
;; subsprite make the old method (checking for EQ inside of the erase all
;; other sprites loop) unworkable.  The eq check now has to be made inside
;; of the erase method (since it can be recursively invoked by superior
;; sprites)
;;

(defvar *currently-moving-sprite* nil)

    #+gl
    (setf (graphics-sheet-draw-mode new-gs) ':window)

    #+gl
    (setf (graphics-sheet-draw-mode new-gs) ':window)


;;;;
;;;; FILE: grmeth.lisp
;;;;

(defmethod update-save-under ((self button))
  (let ((shape (shape self))
	(save-under (slot-value self 'save-under))
	(scale (sprite-size self)))
    (let ((size 0)
	  (non-xor-graphics-occurred? nil)
	  (in-xor? nil))
      (with-graphics-state-bound
        (do-vector-contents (com shape)
	  ;; there really ought to be a more modular way to handle this...
	  (when (and (not non-xor-graphics-occurred?)
		     ;; no need to check if there has already been
		     ;; graphics drawn in something other than XOR
		     (=& (svref& com 0)
		        ;;  sgithens TODO 2020-03-29 Why was this read operator syntax being used?
				;; #.(boxer::graphics-command-opcode 'boxer-change-alu)))
				(boxer::graphics-command-opcode 'boxer-change-alu)))
	    (setq in-xor? (=& (svref& com 1) alu-xor)))
	  (multiple-value-bind (lef top rig bot state-change?)
	      (graphics-command-extents com)
	    (unless state-change?
	      (unless (and in-xor? (not non-xor-graphics-occurred?))
	        (setq non-xor-graphics-occurred? t))
	      (setq size
		    (max size
			 (let* ((max-x (max (abs lef) (abs rig)))
			        (max-y (max (abs top) (abs bot)))
			        (max-t (max max-x max-y)))
			   (* 2 (* max-t max-t)))))))))
      ;; size is now the largest of the sum of squares we take the square
      ;; root of size to obtain the maximimum "radius" of the shape
      ;; remember to multiply by scale since graphics-command-extents does
      ;; not scale
      (setq size (*& 2 (values (ceiling (sqrt (* scale size))))))
      (cond ((null non-xor-graphics-occurred?)
	     ;; if ALL the graphics in the shape have been drawn
	     ;; in XOR, then we can use XOR-REDRAW
	     (setf (slot-value self 'save-under) 'xor-redraw))
	    ((or (null save-under) (eq save-under 'xor-redraw))
	     ;; no existing bitmap save under so allocate a new one
	     (setf (slot-value self 'save-under)
		   (make-save-under (make-offscreen-bitmap *boxer-pane*
							   size size)
				    (floor size 2)
				    size)))
	    (t
	     ;; at this point, we know we both have and want a bitmap backing
	     ;; store now check sizes to see if what we already have is big
	     ;; enough we may want to put in a shrinking bitmap clause but
	     ;; for now, we just leave big bitmaps in place trying to
	     ;; minimize more reallocation of bitmaps--trading space (extra
	     ;; bitmap size) for speed (no need to
	     ;; reallocate bitmaps in grow/shrink/grow cases)
	     (let ((existing-bitmap (save-under-bitmap save-under)))
	       (when (and (not (null existing-bitmap))
			  ;; don't need this but be paranoid since
			  ;; this is critical code
			  (or (>& size
				  (offscreen-bitmap-width  existing-bitmap))
			      (>& size
				  (offscreen-bitmap-height existing-bitmap))))
		 (setf (slot-value self 'save-under)
		       (make-save-under (make-offscreen-bitmap *boxer-pane*
							       size size)
					(round size 2) size)))))))))

;; 2021-10-15 This was the #-opengl version of the body for `set-assoc-graphics-box`
#-opengl
(drawing-on-window (*boxer-pane*)
    (break "sgithens I don't believe this should ever be called... 13411")
    ;; need to bind the window here since the local methods no longer do
    ;; so themselves (window is usually bound around calls to top-level-doit)
    (when (not-null (slot-value self 'assoc-graphics-box))
      (with-graphics-vars-bound ((slot-value self 'assoc-graphics-box))
	(with-graphics-screen-parameters
	    (erase self))))
    (set-assoc-graphics-box-instance-var self new-box)
    (when (and (not-null new-box)
	       (absolute-shown? self))
      (with-graphics-vars-bound (new-box gr-sheet)
	(with-graphics-screen-parameters
	    (dolist (ttl (graphics-sheet-object-list gr-sheet))
	      (when (shown? ttl) (fast-erase ttl))))
	  ;; first, hide any sprites that are visible
	  ;; note that the new sprite has NOT been pushed onto the
	  ;; graphics-box's list of objects yet
	(with-graphics-screen-parameters-once
	    (save-under-turtle self))
	;; now redraw any hidden sprites
	(with-graphics-screen-parameters
	    (dolist (ttl (graphics-sheet-object-list gr-sheet))
	      (when (shown? ttl) (draw ttl)))
	  (draw self)))))

#|
(defmethod save-state-and-reset ((self graphics-object))
  (setq %turtle-state
	(list (x-position self) (y-position self)))
  (set-x-position self 0.0)
  (set-y-position self 0.0))

(defmethod restore-state ((self graphics-object))
  (set-x-position self (first %turtle-state))
  (set-y-position self (second %turtle-state)))
|#

#| ;;how did this ever work ????
(defmethod touching? ((self graphics-object) other-turtle)
  (multiple-value-bind (left1 top1 right1 bottom1)
      (enclosing-rectangle self)
    (multiple-value-bind (left2 top2 right2 bottom2)
	(enclosing-rectangle other-turtle)
      ;;; Check an edge at a time
      (or (and (inclusive-between? left1 left2 right2)
	       (or (and (>= top1 top2) (<= bottom1 top2))
		   (and (>= top1 bottom2) (<= bottom1 bottom2))))
	  (and (inclusive-between? right1 left2 right2)
	       (or (and (>= top1 top2) (<= bottom1 top2))
		   (and (>= top1 bottom2) (<= bottom1 bottom2))))
	  (and (inclusive-between? top1 top2 bottom2)
	       (or (and (>= right1 right2) (<= left1 right2))
		   (and (>= right1 left2) (<= left1 left2))))
	  (and (inclusive-between? bottom1 top2 bottom2)
	       (or (and (>= right1 right2) (<= left1 right2))
		   (and (>= right1 left2) (<= left1 left2))))
	  ;; Finally check a single point in each
	  (and (inclusive-between? left2 left1 right1)
	       (inclusive-between? top2 top1 bottom1))
	  (and (inclusive-between? left1 left2 right2)
	       (inclusive-between? top1 top2 bottom2))))))
|#

;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version does NO transformations.  Instead, the coordinates
;;; are left in turtle space and the hardware performs the transformations
;;; whenever it renders the graphics box
;;;
;;; ****************   NOTE   ****************
;;;

#+gl
(defmethod move-to ((self graphics-object) x-dest y-dest
		    &optional dont-update-box)
  (let ((x-position (slot-value self 'x-position))
	(y-position (slot-value self 'y-position))
	(pen-alu (get-alu-from-pen (pen self))))
    (cond  ((not (and (numberp x-dest) (numberp y-dest)))
	    (error "one of the args, ~s or ~s, was not a number"
		   x-dest y-dest))
	   (t
	    (unless (typep x-dest 'boxer-float)
	      (setq x-dest (coerce x-dest 'boxer-float)))
	    (unless (typep y-dest 'boxer-float)
	      (setq y-dest (coerce y-dest 'boxer-float)))
	    (cond ((not (null %learning-shape?))
		   ;; don't draw while learning shape.
		   (unless (null pen-alu)
		     (record-boxer-graphics-command-line-segment
		      (box-interface-value x-position)
		      (box-interface-value y-position)
		      x-dest y-dest))
		   ;; While in learning-shape, don't update any boxes
		   (setf (box-interface-value x-position) x-dest)
		   (setf (box-interface-value y-position) y-dest))
		  ;; Have to make fence mode work some other time
		  ((and (eq %draw-mode ':fence)
			(not (point-in-array? array-x-dest array-y-dest)))
		   (error "you hit the fence"))
		  (t
		   (cond ((no-graphics?)
			  ;; this means we can't even do any wrapping
			  ;; calculations, so just set values
			  (set-xy self x-dest y-dest dont-update-box))
			 (t
			  (multiple-value-bind (abs-x-dest abs-y-dest)
			      (make-absolute self x-dest y-dest)
			    (let ((abs-x (absolute-x-position self))
				  (abs-y (absolute-y-position self)))
			      (without-interrupts
			       (when (and (null (slot-value self
							    'superior-turtle))
					  (eq %draw-mode ':wrap))
				 (setq x-dest (wrap-x-coordinate x-dest)
				       y-dest (wrap-y-coordinate y-dest)))
			       ;; this may have to change...
			       (cond ( %mouse-usurped
				      ;; don't update boxes during follow-mouse
				      (setf (box-interface-value x-position)
					    x-dest)
				      (setf (box-interface-value y-position)
					    y-dest))
				     (
				      (set-xy self x-dest y-dest
					      dont-update-box)))
			       (when (and (not (null pen-alu))
					  (not (zerop (pen-width self))))
				 (record-boxer-graphics-command-line-segment
				  abs-x abs-y abs-x-dest abs-y-dest)
				 (with-graphics-screen-parameters
				     (line-segment
				      abs-x abs-y abs-x-dest abs-y-dest))))
			      ;; invalidate the shape and extent caches
					;(invalidate-window-shape-and-extent-caches self)
			      ))))))))))

;;;
;;; ****************   NOTE   ****************
;;;
;;; In the SGI version, only 'overlay will be supported
;;; because both XOR-REDRAW and SAVE-UNDER will be comparatively
;;; SLOW.  This should eventually be changed but there are too many
;;; places right now that depend on (not (eq 'xor-redraw)) to mean
;;; use a save-under which isn't supported in the SGI version.
;;;
;;; At some point, this needs to change to 'OVERLAY and all those
;;; other places fixed...
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod update-save-under ((button button))
  (setf (slot-value button 'save-under) 'xor-redraw))

;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version doesn't use window shape caches
;;; so this is NOOPed out
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod invalidate-window-shape-and-extent-caches ((self button))
  nil)


;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version doesn't cache the transformed shape
;;; instead, the shape list is left in turtle centered
;;; (and turtle oriented) space and we instruct the hardware
;;; to render the turtle with the appropriate transformations
;;;
;;; ****************   NOTE   ****************
;;;

#+gl
(defmethod draw ((self button))
  (unless (null (shown? self))
    (unwind-protect
	 (progn
	   ;; save away old transformation state
	   (bw::drawmode gl::overdraw)
	   (bw::pushmatrix)
	   ;; now transform
	   (bw::translate (x-position self) (y-position self) 0.0)
	   ;;   should evetually be (z-position self) -----^
	   (bw::rotate (- (* 10 (round (heading self)))) #\z)
	   (playback-graphics-list-internal (shape self))
	   (unless (eq (shown? self) ':no-subsprites)
	     (dolist (subs (slot-value self 'subsprites))
	       (draw subs))))
      (bw::popmatrix)
      (bw::drawmode gl::normaldraw))))

;;;
;;; ****************   NOTE   ****************
;;;
;;; This should actually be called slow-erase
;;; Anyways, this just blanks the overlay plane
;;; at some point, a finer grained version of this
;;; ought to be written.  ALternatively, rewrite the
;;; various macros (like with-sprite-primitive-environment
;;; in grfdfs.lisp to just blank the overlay plane
;;; once and NOT loop through the list of sprites
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod fast-erase ((turtle button))
  (bw::clear-overlay-planes))


;;;
;;; ****************   NOTE   ****************
;;;
;;; Punting on enclosing-rectangle for the SGI for now.  Need
;;; to think about what hardware resources are available and
;;; how fast they wil be.  The alternative is to use a cache
;;; like the generic version and cycle through the transformed
;;; shape except that the cache can be
;;; a lot smaller since it doesn't have to hold the window
;;; coordinate version of the sprite's shape.
;;;
;;; ****************   NOTE   ****************
;;;

#+gl
(defmethod enclosing-rectangle ((self button))
  (warn "Enclosing-Rectangle is not implemented")
  (values 0 0 0 0))

    ;; on the SGI version we don't use it because we let the hardware
    ;; draw the turtle directly from the turtle's shape
    #-gl

;;;;
;;;; FILE: grobjs.lisp
;;;;

;;; The actual def-redisplay-initialization moved to gdispl.lisp
;;; for ordering reasons
#|
;;; This has to be a redisplay init because make-turtle-shape
;;; depends upon the runtime value of *foreground-color* which
;;; is not defined until AFTER the windows are created for some
;;; window systems (like CLX)
(def-redisplay-initialization ; :turtle-shape
    (setq *default-graphics-object-shape*
    (let ((%graphics-list (make-turtle-shape 8))
    (*graphics-command-recording-mode* ':boxer))
            (record-boxer-graphics-command-change-alu alu-seta)
      (record-boxer-graphics-command-change-pen-width 1)
      (record-boxer-graphics-command-centered-rectangle
       0.0 0.0
       *default-graphics-object-size* *default-graphics-object-size*)
      %graphics-list)
    *default-turtle-shape*
    (let ((%graphics-list (make-turtle-shape 8))
    (*graphics-command-recording-mode* ':boxer))
            (record-boxer-graphics-command-change-alu alu-seta)
      (record-boxer-graphics-command-change-pen-width 1)
      ;; the base line
      (record-boxer-graphics-command-line-segment
       (- *turtle-half-base*) (- (/ *turtle-height* 3.0))
       *turtle-half-base* (- (/ *turtle-height* 3.0)))
      ;; the right side
      (record-boxer-graphics-command-line-segment
       *turtle-half-base* (- (/ *turtle-height* 3.0))
       0.0 (* 2 (/ *turtle-height* 3)))
      ;; the left side
      (record-boxer-graphics-command-line-segment
       0.0 (* 2 (/ *turtle-height* 3))
       (- *turtle-half-base*) (- (/ *turtle-height* 3.0)))
      %graphics-list)))

|#

;;; We go through these contortions in order to reduce
;;; the floating point CONSing.  Using this version seems to
;;; reduce the FP consing from about 8 DP-FP numbers per call to 2
#+lcl3.0
(defun wrap-x-coordinate (user-x)
  (let ((float-temp 0.0) (float-width (float (the fixnum %drawing-width))))
    (declare (float float-temp float-width))
    (setq float-temp (float-plus %drawing-half-width user-x))
    (float-minus (if (and (plusp float-temp) (< float-temp float-width))
         float-temp
         (let ((scratch 0.0))
           (declare (float scratch))
           (setq scratch float-temp)
           (setq float-temp
           (values (ffloor float-temp float-width)))
           (setq float-temp (float-times float-temp float-width))
           (setq float-temp (float-minus scratch float-temp))
           (if (minusp float-temp)
         (float-plus float-temp float-width)
         float-temp)))
     %drawing-half-width)))


;;; We go through these contortions in order to reduce
;;; the floating point CONSing.  Using this version seems to
;;; reduce the FP consing from about 8 DP-FP numbers per call to 2
#+lcl3.0
(defun wrap-y-coordinate (user-y)
  (let ((float-temp 0.0) (float-height 0.0))
    (declare (float float-temp float-height))
    (setq float-height (float (the fixnum %drawing-height)))
    (setq float-temp (float-minus %drawing-half-height user-y))
    (float-minus %drawing-half-height
     (if (and (plusp float-temp) (< float-temp float-height))
         float-temp
         (let ((scratch 0.0))
           (declare (float scratch))
           (setq scratch float-temp)
           (setq float-temp
           (values (ffloor float-temp float-height)))
           (setq float-temp (float-times float-temp float-height))
           (setq float-temp (float-minus scratch float-temp))
           (if (minusp float-temp)
         (float-plus float-temp float-height)
         float-temp))))))

;;;;
;;;; FILE: grprim1.lisp
;;;;

;; sgithens 2022-02-25 from defboxer-primitive set-background
#-opengl
(drawing-on-bitmap ((graphics-sheet-bit-array gs))
           (with-pen-color (pix)
       (draw-rectangle
           (graphics-sheet-draw-wid gs)
           (graphics-sheet-draw-hei gs) 0 0)))

(boxer-eval::defboxer-primitive bu::clear-graphics ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::clear-graphics
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

(boxer-eval::defboxer-primitive bu::cleargraphics ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::cleargraphics
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

(boxer-eval::defboxer-primitive bu::cg ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::cg
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

;;;;
;;;; FILE: grprim2.lisp
;;;;


;; 2022-04-21 Some wild stuff from defsprite-function stamp directly above the usage
;; of append-graphics-sheet-at
            #| ; this is now handled inside of append-graphics-sheet-at
            (when (not (null (graphics-sheet-bit-array gs)))
            ;; if the graphics box has a bitmap, stamp it also...
            (cond ((not (null tba))
            (let* ((gs-wid (graphics-sheet-draw-wid gs))
            (gs-hei (graphics-sheet-draw-hei gs))
            (gs-half-wid (floor gs-wid 2))
            (gs-half-hei (floor gs-hei 2))
            (min-x (-& turtle-array-x gs-half-wid))
            (max-x (+& turtle-array-x gs-half-wid))
            (min-y (-& turtle-array-y gs-half-hei))
            (max-y (+& turtle-array-y gs-half-hei)))
            (drawing-on-bitmap (tba)
            (bitblt-to-screen alu-seta
            (-& (min& %drawing-width max-x)
            (max& 0 min-x))
            (-& (min& %drawing-height max-y)
            (max& 0 min-y))
            (graphics-sheet-bit-array gs)
            (if (plusp& min-x) 0 (-& min-x))
            (if (plusp& min-y) 0 (-& min-y))
            (max& 0 min-x) (max& 0 min-y)))
            (with-graphics-screen-parameters
            (bitblt-to-screen alu-seta
            (-& (min& %drawing-width max-x)
            (max& 0 min-x))
            (-& (min& %drawing-height max-y)
            (max& 0 min-y))
            (graphics-sheet-bit-array gs)
            (if (plusp& min-x) 0 (-& min-x))
            (if (plusp& min-y) 0 (-& min-y))
            (max& 0 min-x) (max& 0 min-y)))))
            ;; we could have a clause here that adds the bitmap to
            ;; the display list
            (t nil)))  |#

;(defsprite-function bu::stamp-partial-bitmap ((bu::port-to graphics-box)
;					      (boxer-eval::numberize src-x)
;					      (boxer-eval::numberize src-y)
;					      (boxer-eval::numberize width)
;					      (boxer-eval::numberize height))
;  (sprite turtle)
;  (with-sprites-hidden t
;    (stamp-partial-bitmap-for-turtle ...))
;  boxer-eval::*novalue*)

;;;; mousing around...
;; #-opengl
;; (defsprite-function bu::follow-mouse () (sprite turtle)
;;   (let ((screen-box (or (car (fast-memq (bp-screen-box *mouse-bp*)
;;                                         ;; is *mouse-bp* valid ?
;;                                         (get-visible-screen-objs
;;                                          (slot-value turtle 'assoc-graphics-box))))
;;                         (car (displayed-screen-objs
;; 			      ;; this is wrong but ignore ports for the moment
;; 			      (slot-value turtle 'assoc-graphics-box))))))
;;     (multiple-value-bind (window-x-offset window-y-offset)
;; 	(xy-position screen-box)
;;       (multiple-value-bind (left top right bottom)
;; 	  (box-borders-widths (box-type screen-box) screen-box)
;; 	(let* ((min-x (+& window-x-offset left))
;; 	       (min-y (+& window-y-offset top))
;;                (superior-turtle (superior-turtle turtle))
;;                (sup-x (if (null superior-turtle) 0
;;                           (absolute-x-position superior-turtle)))
;;                (sup-y (if (null superior-turtle) 0
;;                           (absolute-y-position superior-turtle))))
;; 	  (flet ((translate-x (window-x)
;; 			      (- (user-coordinate-x (-& window-x min-x))
;;                                  sup-x))
;; 		 (translate-y (window-y)
;; 			      (- (user-coordinate-y (-& window-y min-y))
;;                                  sup-y)))
;; 	    (warp-pointer *boxer-pane*
;; 			  (+ window-x-offset left (fix-array-coordinate-x
;; 						   (absolute-x-position turtle)))
;; 			  (+ window-y-offset top  (fix-array-coordinate-y
;; 						   (absolute-y-position turtle))))
;; 	    (multiple-value-bind (final-x final-y moved?)
;; 		(let ((%mouse-usurped t))
;; 		  (with-mouse-tracking-inside ((mouse-x min-x) (mouse-y min-y)
;; 					       min-x min-y
;; 					       (-& (+& window-x-offset
;; 						       (screen-obj-wid
;; 							screen-box))
;; 						   right 1)
;; 					       (-& (+& window-y-offset
;; 						       (screen-obj-hei
;; 							screen-box))
;; 						   bottom 1)
;; 					       #+MCL :view #+MCL *boxer-pane*)
;; 		  (with-sprites-hidden t
;; 		    (move-to turtle
;; 			     (translate-x mouse-x) (translate-y mouse-y)))))
;; 	      (when moved?
;; 		(with-sprites-hidden t
;; 		  (move-to turtle
;; 			   (translate-x final-x) (translate-y final-y))))))))))
;;     boxer-eval::*novalue*)

(defsprite-function bu::stamp-wedge ((boxer-eval::numberize radius)
                                     (boxer-eval::numberize sweep-angle))
                    (sprite turtle)
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'stamp-wedge " is no longer available, use "
                                       'draw-wedge " instead"))
        (t
         (if (< radius 0)
             (boxer-eval::primitive-signal-error :sprite-error
				           "The Radius, "
				           radius
				           "Should be 0 or greater")
           (with-sprites-hidden t
	     (stamp-wedge turtle radius sweep-angle)))))
      boxer-eval::*novalue*)

(defsprite-function bu::stamp-arc ((boxer-eval::numberize radius)
                                   (boxer-eval::numberize sweep-angle))
                    (sprite turtle)
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'stamp-arc " is no longer available, use "
                                       'draw-arc " instead"))
        (t
         (if (< radius 0)
             (boxer-eval::primitive-signal-error :sprite-error
				           "The Radius, "
				           radius
				           "Should be 0 or greater")
           (with-sprites-hidden t
	     (stamp-arc turtle radius sweep-angle)))))
  boxer-eval::*novalue*)

;;;;
;;;; FILE: grprim3.lisp
;;;;

#-opengl
(boxer-eval::defboxer-primitive bu::freeze ()
  (let ((gb (get-relevant-graphics-box)))
    (if (or (null gb) (eq gb :no-graphics))
	(boxer-eval::primitive-signal-error :graphics "No graphics to FREEZE")
	(let* ((graphics-sheet
		(if (box? gb)
		    (graphics-info gb)
		    (graphics-info-graphics-sheet (vc-graphics gb))))
	       (wid (graphics-sheet-draw-wid graphics-sheet))
	       (hei (graphics-sheet-draw-hei graphics-sheet))
	       (display-list (graphics-sheet-graphics-list graphics-sheet))
	       (bitmap (graphics-sheet-bit-array graphics-sheet)))
	  (when (null bitmap)
	    ;; make sure there IS a backing store
	    (let ((new (make-offscreen-bitmap *boxer-pane* wid hei)))
	      ;; clear the bitmap if it's a new one
	      (drawing-on-bitmap (new)
	        (with-pen-color ((or (graphics-sheet-background graphics-sheet)
				     *background-color*))
		  (draw-rectangle alu-seta wid hei 0 0))
		(setf (graphics-sheet-bit-array graphics-sheet) new))))
	  ;; Now play the list into the backing store
	  (drawing-on-bitmap ((graphics-sheet-bit-array graphics-sheet))
	     (with-graphics-vars-bound-internal graphics-sheet
	       (playback-graphics-list-internal display-list)))
	  ;; now clear the display list
	  (clear-graphics-list display-list)
          ;; mark the dirty? flag
          (setf (graphics-sheet-bit-array-dirty? graphics-sheet) t)
	  (modified-graphics gb)
	  boxer-eval::*novalue*))))

;;;;
;;;; FILE: infsup.lisp
;;;;

;;; These are no longer used.  They work (6/6/88), but do much useless work,
;;; and KILL-ROW was ever called.  See KILL-BOX-CONTENTS, the more efficient
;;; replacement.
#|
(defmethod insert-box-rows-at-row-no ((self box) box row-no)
  (let ((box-first-row (kill-row box (first-inferior-row box))))
    (unless (null box-first-row)
      (let ((row-at-row-no (row-at-row-no self row-no))
      (row-bf-row-no (row-at-row-no self (- row-no 1)))
      (box-last-row (do* ((next-box-row (next-row box-first-row)
                (next-row box-row))
        (box-row box-first-row next-box-row))
             (())
          (set-superior-box box-row self)
          (do-row-chas ((c box-row)) (unless (cha? c) (insert-self-action c)))
          (if (null next-box-row) (return box-row)))))
  (set-previous-row box-first-row row-bf-row-no)
  (set-next-row box-last-row row-at-row-no)
  (set-next-row row-bf-row-no box-first-row)
  (set-previous-row row-at-row-no box-last-row)))))

(defmethod delete-rows-between-row-nos ((self box) strt-row-no stop-row-no
          &optional (check-closet t))
  (let* ((strt-row (row-at-row-no self strt-row-no))
   (stop-row (row-at-row-no self stop-row-no))
   (strt-row-prev-row (unless (null strt-row) (previous-row strt-row)))
         (stop-row-next-row (unless (null stop-row) (next-row stop-row)))
   (return-box (make-initialized-box))
   (closet-row (slot-value self 'closets)))
    (do ((row strt-row (next-row row)))
  ((null row))
      (when (or (null check-closet) (not (eq row closet-row)))
  (do-row-chas ((c row)) (unless (cha? c) (delete-self-action c)))
  (set-superior-box row nil)))
    (set-previous-row strt-row nil)
    (set-next-row strt-row nil)
    (if (null strt-row-prev-row)
  (set-first-inferior-row self stop-row-next-row)
  (set-next-row strt-row-prev-row stop-row-next-row))
    (unless (null stop-row-next-row)
      (set-previous-row stop-row-next-row strt-row-prev-row))
    (append-row return-box strt-row)
    return-box))

(defmethod delete-between-rows ((self box) strt-row stop-row
        &optional (check-closet t))
  (let ((strt-row-prev-row (when (not-null strt-row)(previous-row strt-row)))
  (stop-row-next-row (when (not-null stop-row)(next-row stop-row)))
  (return-box (make-initialized-box))
  (closet-row (slot-value self 'closets)))
    (do ((row strt-row (next-row row)))
  ((eq row stop-row-next-row))
      (when (or (null check-closet) (not (eq row closet-row)))
  (do-row-chas ((c row)) (unless (cha? c) (delete-self-action c)))
  (set-superior-box row nil)))
    (set-previous-row strt-row nil)
    (set-next-row stop-row nil)
    (if (null strt-row-prev-row)
  (set-first-inferior-row self stop-row-next-row)
  (set-next-row strt-row-prev-row stop-row-next-row))
    (when (not-null stop-row-next-row)
      (set-previous-row stop-row-next-row strt-row-prev-row))
    (set-first-inferior-row return-box strt-row)
    return-box))

(defmethod kill-rows-at-row-no ((self box) strt-row-no)
  (let ((stop-row-no (length-in-rows self)))
    (delete-rows-between-row-nos self strt-row-no stop-row-no)))

(defmethod kill-row ((self box) row)
  (kill-rows-at-row-no self (row-row-no self row)))
|#

#|  obsolete, give it 6 months and then remove from source (7/19/91)
(defun delete-rows-to-end-of-box (bp &optional (force-bp-type nil))
  (action-at-bp-internal
    (let ((box (bp-box bp))
    (row (bp-row bp)))
      (unless (null box)
  (kill-rows-at-row-no box (+ (row-row-no box row) 1))))))
|#

#|
(defun find-path-from-superior-to-inferior (superior-box inferior-box)
  (nreverse
    (with-collection
      (do ((box inferior-box (superior-box box)))
    ((eq box superior-box))
  (collect box)))))
|#

;;;;
;;;; FILE: keydef-high.lisp
;;;;

(defun current-mouse-click-name (button shift &optional place)
  (let ((button-names (input-device-mouse-string
                       *current-input-device-platform*))
        (shift-names (input-device-shift-list
                      *current-input-device-platform*)))
    (unless (or (>= button (length button-names))
                (> shift (length shift-names)))
      (mouse-click-name-string (nth button button-names)
                               (if (zerop& shift) nil
                                 (nth (1-& shift) shift-names))
                               place *current-input-device-platform*))))

;; sgithens 2021-06-14 We used to have these mouse handlers parameterized
;; by setting them on a symbol, however there has never been more than one,
;; I don't plan on adding more, and I'm trying to reduce complexity of the
;; system when we can.

;; from defun handle-boxer-input
((mouse-event? input)
          (let ((handler (get (mouse-event-type input) ':boxer-input)))
            (record-mouse-input input)
            (if (or (null handler)
                    (not (funcall handler input)))
              (unhandled-boxer-input (get-mouse-click-name input)))))

(setf (get :mouse-click :boxer-input) 'mouse-click-boxer-input-handler)
(setf (get :mouse-hold  :boxer-input) 'mouse-click-boxer-input-handler)

;;;;
;;;; FILE: keydef-lwm.lisp
;;;;

;; The test for whether a system level key code needs to be remapped is:
(defun remap-char? (char) (>= (char-code char) #xf700))

; ;; the remapping is performed by:
(defun remap-char (char)
  (code-char (+ (- (char-code char) #xf700) *boxer-function-key-code-start*)))

;; the external interface (from boxwin)
;; the key-handler in boxwin-lw uses this to generate the correct input event

(defun convert-gesture-spec-modifier (gesture)
  "This takes a gesture-spec, looks at the modifiers field on it and converts them to the
  internal boxer codes for modifier keys.
  The gesture-spec modifiers are:
      1 = SHIFT, 2 = CONTROL, 4 = META, 8 = HYPER (the apple command key)
  The internal codes we use in boxer are:
      0 = Plain Key, 1 = COMMAND, 2 = OPTION, 3 = COMMAND-OPTION
      (translating Command and Option to your modern OS key equivalents. Most likely
      Ctrl and Alt)
  "
  (format t "~%The gesture spec modifiers are: ~A" (sys:gesture-spec-modifiers gesture))
  (sys:gesture-spec-modifiers gesture)
  ; (let ((rawgm (sys:gesture-spec-modifiers gesture)))
  ;   ;; effectively ignoring the shift bit, but keeping the hyper bit distinct
  ;   ;(ash rawgm -1)
  ;   ;; we could convert the command key to control here....
  ;   ;; lookup table is fastest, we can bit shift the gesture modifiers since they are
  ;   ;; specified in powers of two
  ;   (svref #(0 1 2 3 1 1 2 3) (ash rawgm -1))
  ;   )
    )

;;;; sgithens boxer-sunrise-51 2021-11-29 Wait a build or too and make sure these
;;;; have been properly replaced, then delete them.
;;;; #+cocoa
;;;; (progn
;;;; ;  (define-lwm-function-key BU::INSERT-KEY COCOA:NS-INSERT-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::DELETE-KEY COCOA:NS-DELETE-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::BEGIN-KEY COCOA:NS-BEGIN-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::PRINT-SCREEN-KEY COCOA:NS-PRINT-SCREEN-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::SCROLL-LOCK-KEY COCOA:NS-SCROLL-LOCK-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::PAUSE-KEY COCOA:NS-PAUSE-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::SYS-REQ-KEY COCOA:NS-SYS-REQ-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::BREAK-KEY COCOA:NS-BREAK-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::RESET-KEY COCOA:NS-RESET-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::STOP-KEY COCOA:NS-STOP-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::MENU-KEY COCOA:NS-MENU-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::USER-KEY COCOA:NS-USER-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::SYSTEM-KEY COCOA:NS-SYSTEM-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::PRINT-KEY COCOA:NS-PRINT-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::CLEAR-DISPLAY-KEY COCOA:NS-CLEAR-DISPLAY-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::INSERT-LINE-KEY COCOA:NS-INSERT-LINE-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::DELETE-LINE-KEY COCOA:NS-DELETE-LINE-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::INSERT-CHAR-KEY COCOA:NS-INSERT-CHAR-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::DELETE-CHAR-KEY COCOA:NS-DELETE-CHAR-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::PREV-KEY COCOA:NS-PREV-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::NEXT-KEY COCOA:NS-NEXT-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::SELECT-KEY COCOA:NS-SELECT-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::EXECUTE-KEY COCOA:NS-EXECUTE-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::UNDO-KEY COCOA:NS-UNDO-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::REDO-KEY COCOA:NS-REDO-FUNCTION-KEY)
;;;
;;;; ;  (define-lwm-function-key BU::FIND-KEY COCOA:NS-FIND-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::HELP-KEY COCOA:NS-HELP-FUNCTION-KEY)
;;;; ;  (define-lwm-function-key BU::MODE-SWITCH-KEY COCOA:NS-MODE-SWITCH-FUNCTION-KEY))

;;;;
;;;; FILE: keys-new.lisp
;;;;
(boxer-eval::defboxer-key (bu::>-key 2) com-fat)
(boxer-eval::defboxer-key (bu::<-key 2) com-nutri-system)

#+sun (boxer-eval::defboxer-key bu::R2-key com-print-screen)

#+sun (boxer-eval::defboxer-key (bu::R2-key 2) com-print-screen-to-file)

#+apple (boxer-eval::defboxer-key (bu::return-key 1) com-doit-now) ; should be com-step

;;;;
;;;; FILE: lw-menu.lisp
;;;;

#|

(defun menu-boxtop-standard (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          (t (box::putprop spb :standard :boxtop)))))

(defun menu-boxtop-folder (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          (t (box::putprop spb :folder :boxtop)))))

(defun menu-boxtop-name-only (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          (t (box::putprop spb :name-only :boxtop)))))
|#

;;; MIME menus

;; stub these for now...

(defun boxer::edit-mime-mapping () nil)

(defun boxer::mime-type-dialog (mime-type app-sig file-type)
  (declare (ignore mime-type app-sig file-type))
  nil)

#|
(defun edit-mime-mapping ()
  (let ((entries-dialog-item
         (make-instance 'ccl::sequence-dialog-item
           :view-position #@(30 30) :view-size #@(350 100)
           :table-sequence *mime-mac-file-types*
           :visible-dimensions #@(1 8)
           :table-vscrollp t
           :table-print-function #'(lambda (entry stream)
                                     (format stream "~30A ~6A ~A"
                                             (car entry) (cadr entry)
                                             (caddr entry))))))
    (flet ((selected-entry ()
             (let ((idx (ccl::selected-cells entries-dialog-item)))
               (unless (null idx)
                  (elt (ccl::table-sequence entries-dialog-item)
                       (ccl::point-v (car idx)))))))
      (let* ((labels (ccl::make-dialog-item
                      'ccl:static-text-dialog-item #@(30 10) #@(350 15)
                      "Mail Type                     Application      File Type"))
             (new-button (ccl:make-dialog-item 'ccl:button-dialog-item
                                               #@(10 200) #@(50 20) "New"
                                               #'(lambda (di) (declare (ignore di))
                                                  (add-new-mime-type)
                                                  (ccl:set-table-sequence
                                                   entries-dialog-item
                                                   *mime-mac-file-types*))))
             (copy-button (ccl:make-dialog-item
                           'ccl:button-dialog-item #@(80 200) #@(50 20) "Copy"
                           #'(lambda (di) (declare (ignore di))
                              (let ((entry (selected-entry)))
                                (cond ((null entry) (add-new-mime-type))
                                      (t (add-new-mime-type
                                          :mime-type (car entry)
                                          :app-sig (cadr entry)
                                          :file-type (caddr entry))
                                         (ccl:set-table-sequence
                                          entries-dialog-item
                                          *mime-mac-file-types*)))))))
             (edit-button (ccl:make-dialog-item
                           'ccl:button-dialog-item #@(150 200) #@(50 20) "Edit"
                           #'(lambda (di) (declare (ignore di))
                              (let ((entry (selected-entry)))
                                (cond ((null entry) (boxer::beep))
                                      (t (edit-mime-entry entry)
                                         (ccl:set-table-sequence
                                          entries-dialog-item
                                          *mime-mac-file-types*)))))))
             (delete-button (ccl:make-dialog-item
                             'ccl:button-dialog-item #@(220 200) #@(50 20) "Delete"
                             #'(lambda (di) (declare (ignore di))
                                (let ((entry (selected-entry)))
                                  (cond ((null entry) (boxer::beep))
                                        (t (setq *mime-mac-file-types*
                                                 (delete entry
                                                         *mime-mac-file-types*))
                                           (ccl:set-table-sequence
                                            entries-dialog-item
                                            *mime-mac-file-types*)))))))
             (save-button (ccl:make-dialog-item
                           'ccl:button-dialog-item #@(290 200) #@(50 20) "Save"
                             #'(lambda (di) (declare (ignore di))
                                (save-mime-mapping))))
             (ok-button (ccl:make-dialog-item
                         'ccl:button-dialog-item #@(360 200) #@(50 20) "Done"
                                          #'(lambda (di) (declare (ignore di))
                                             (ccl::return-from-modal-dialog t))))
             (dialog (make-instance 'ccl:dialog :window-title "Mail Attachments"
                       :view-position '(:top 60) :view-size #@(420 250)
                       :close-box-p nil
                       :view-subviews (list labels entries-dialog-item
                                            new-button copy-button edit-button
                                            delete-button save-button ok-button))))
        (ccl:modal-dialog dialog)))))


(defun mime-type-dialog (mime-type app-sig file-type)
  (let* ((mime-label (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                           #@(10 10) #@(150 20) "Mail Type"))
         (mime-di (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                        #@(10 30) #@(150 20) (or mime-type "")))
         (app-label (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                          #@(170 10) #@(100 20)
                                          "Application"))
         (app-di  (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                        #@(170 30) #@(60 20) (or app-sig "????")))
         (file-label (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                          #@(290 10) #@(100 20)
                                          "File Type"))
         (file-di (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                        #@(290 30) #@(60 20) (or file-type "????")))
         (doc-box (ccl::make-dialog-item 'ccl:static-text-dialog-item
                                        #@(10 60) #@(300 20) "")))
    (let* ((like-button (ccl:make-dialog-item
                         'ccl:button-dialog-item #@(10 90) #@(100 20) "Example..."
                         #'(lambda (di)
                             (declare (ignore di))
                             (let ((file (ccl:choose-file-dialog)))
                               (ccl:set-dialog-item-text
                                app-di (String (ccl:mac-file-creator file)))
                               (ccl:set-dialog-item-text
                                file-di (string (ccl:mac-file-type file)))))))
           (ok-button (ccl:make-dialog-item
                       'ccl:button-dialog-item #@(170 90) #@(50 20) "OK"
                       #'(lambda (di)
                           (declare (ignore di))
                           (cond ((not (= (length (ccl:dialog-item-text app-di)) 4))
                                  (boxer::beep)
                                  (ccl:set-dialog-item-text
                                   doc-box "App must be 4 chars long"))
                                 ((not (= (length (ccl:dialog-item-text file-di)) 4))
                                  (boxer::beep)
                                  (ccl:set-dialog-item-text
                                   doc-box "File Types must be 4 chars long"))
                                 (t
                                  (ccl:return-from-modal-dialog
                                   (values (list (ccl:dialog-item-text mime-di)
                                                 (ccl:dialog-item-text app-di)
                                                 (ccl:dialog-item-text file-di))
                                           t)))))))
             (cancel-button (ccl:make-dialog-item
                             'ccl:button-dialog-item #@(240 90) #@(50 20) "Cancel"
                             #'(lambda (di) (declare (ignore di))
                                (ccl:return-from-modal-dialog (values nil nil)))))
             (dialog (make-instance 'ccl:dialog
                       :window-title "Mail Type Entry"
                       :view-position '(:top 390) :view-size #@(400 120)
                       :close-box-p nil
                       :view-subviews (list mime-label mime-di app-label app-di
                                            file-label file-di doc-box
                                            like-button ok-button cancel-button))))
      (multiple-value-bind (result ok?)
          (ccl:modal-dialog dialog)
        (unless (null ok?)
          (list (car result)
                (intern (cadr result) (find-package "KEYWORD"))
                (intern (caddr result) (find-package "KEYWORD"))))))))
|#

;;;;
;;;; FILE: macros.lisp
;;;;

;;; do nothing in other implementations
;;; #-mcl
(defmacro at-user-level (&body body)
  `(progn ,@body))

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

(defconstant +degs->rads+ (/ pi 180.0))

#-(or lispm tltrig)
(progn					; double precision
  (defun sind (x) (sin (float-times (float x) +degs->rads+)))

  (defun cosd (x) (cos (float-times (float x) +degs->rads+)))
  )

#+tltrig
(progn					; table lookup + linear interpolation
;;; we could phase shift instead of having 2 tables
;;; but lets not get TOO complicated unless we have to
  (defun make-sin-table ()
    (let ((table (make-array 360 :element-type 'double-float)))
      (dotimes (i 360)
        (setf (aref table i) (sin (* i +degs->rads+))))
      table))

  (defun make-cos-table ()
    (let ((table (make-array 360 :element-type 'double-float)))
      (dotimes (i 360)
        (setf (aref table i) (cos (* i +degs->rads+))))
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

;;;;
;;;; FILE: mail.lisp
;;;;
;;;; --entire-file--

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



   This file contains boxer SMTP and POP3 based mail reading utilities




Modification History (most recent at top)

12/31/11 get-mime-binary-box, smtp-send-box, send-box-mime-data, get-mime-plain-text, get-mime-binhex-box
         changed calls to xxx-mac-file-ref-xxx to xxx-xref-xxx
 8/08/05 fixed package lossage in bu::mail-document
 5/15/05 added new prim mail-document which mails the current filebox
 3/09/05 send-multipart-box-data: make sure there is only 1 border between boxes
         NOTE: receiving mailers were messing this up by converting multiple adjacent
         borders into 1 border followed by blank lines
 3/01/05 simplified write-mime-boundary
 7/21/04 generalized WITH-POP-SERVER-QUEUED-DELETION to handle message
         numbers as well as boxes
 4/05/04 removed icon caching using explicit ccl::file-icon from
         get-mime-{plain-text,binary-box}, should already be done by the calls to
         boxer::set-xref-boxtop-info
10/13/03 mail-eol? for ccl::opentransport-tcp-stream
 7/15/03 fixed bug in EOF line check in get-mime-multipart which caused
         attachments to be ignored
 7/08/03 added net-mail-read-line-from-mac-file to debug mail messages saved in mac
         files which are #\LF delineated
 4/19/03 merged current LW and MCL files
 4/07/03 rfc822-date-time changed "(mod year 100)" to just "year", non 4 digit
         years seemed to confuse some mail readers...
 3/29/03 moved "Reading.." message from POP-MSG-SIZE to GET-MESSAGE because
         pop-msg-size is used by the delete-msg routine as well
         delete-message-no: changed when => cond, function was searching for
         match even after it deleted the message !
11/11/02 added EOF handling to (fill-box-using-url (mail-message-body)),
         get-mime-multipart.
 8/30/02 filter-smtp-data-string added and used by send-smtp-data-line to fix
         bug with ~'s in outgoing mail text
 5/30/02 mime-header-line-value: handle parens as plain char inside quote
         parameter value strings
 5/01/02 debugging tools
 4/11/02 added make-string-buffer and used it various places
 4/07/01 make header-line-field-name more robust for use elsewhere (like http.lisp)
         header-line-value changed to allow passing in of value-starting index
 2/21/01 fixed bugs in row-mail-values, get-mime-plain-text
 2/16/01 mime-type-dialog & edit-mime-mapping moved to mac-menu.lisp
 2/15/01 merged current LW and MCL files
                INITIAL LW port changes
 1/29/01 mail-eol?
 1/28/01 lispworks element-type,
         net-write-line => net-write-control-line in POP commands
         net-mail-read-line
 2/15/99 unique-mime-path changed to comply with LW version of DO*
                INTERIM MCL only changes
12/13/00 added titles and labels to edit-mime-mapping, mime-type-dialog
10/18/00 fixed string coercion omission for mime-type arg in add-new-mime-type
 9/11/00 mime mapping editing functions
 9/10/00 mime mapping utilities
 9/06/00 print-smtp-write-status, bound in smtp-send-box
         added interrupt polling to smtp-send-box and also rearranged
         unwind-protect forms for cleaner behavior if abort occurs in DATA
         operation
 5/25/00 get-mime-uu-binary-data
 6/21/99 make sure unique-mime-path checks for valid pathname everywhere
 6/14/99 better s handling in mime-header-line-value
         newpath-for-mime was using original name instead of the corrected
         rawpath name.  Note:max mac file length is 31 ! (not 32)
 4/26/99 send/get-mime-box now handle link property
         *max-viewable-message-length* + other support mostly get-mime-plain-text
 3/12/99 initialize-instance for pop-url will default host to be *pop-host*
 3/11/99 get-mime-binary-box fills the xref icon cache if it can
 3/10/99 newpath-for-mime changed to use ensure-valid-filename, parameterized
         on *max-file-length* variable
 3/04/99 ensure-valid-filename checks for 32 char length and trims if neccessary
 1/21/99 smtp-do-hello transmits domain info now (some SMTP servers require this)
                Mod list before LW port
12/15/98 the MAIL primitive now precesses the editor mutation queue to
         insure that editor structure is up to date
12/13/98 added stream based mail-eol? methods for end of line checking in
         net-mail-read-line
12/10/98 finished applefile(single & double) integration
12/08/98 changed box-smtp-type to return :APPLEDOUBLE for foreign box args
12/05/98 changed MIME prologue message to be more informative and less annoying
11/28/98 append-pop-message-body changed to use mime-type-values, control logic
         changed
         get-mime-binary-box takes &optional binary-encoding arg which defaults to
         :base64, otherwise we assume 8bit data and use get-mime-8bit-binary-data
         GET-MIME-APPLEFILE  done 12/02/98
         GET-MIME-BINHEX-BOX     done 11/29/98
         GET-MIME-PMULTIPART (stubbed)
         GET-MIME-APPLEDOUBLE  done 12/03/98

11/21/98 functions which send file attachments, send-mime-text-data and
         send-mime-binary-data, now add a name parameter to Content-Type
11/20/98 unique-mime-path now takes &optional supplied-name get-mime-binary-box
         and append-pop-message-body changed (possibly) supply that arg
 8/10/98 Fixed numerous bugs in get-mime-box: names, type, graphics, closets &rest rows
 8/10/98 Started Logging Changes: source = boxer version 2.3 beta


|#

(in-package :boxnet)


(defvar *smtp-relay-host* "localhost")
(defvar *smtp-port* 25)

(defvar *pop-host*)
(defvar *pop-user*)

(defun smtp-do-hello (stream &optional (host *smtp-relay-host*))
  (net-write-line stream "HELO ~A" host)
  (handle-tcp-response stream))

(defun smtp-do-reset (stream)
  (net-write-line stream "RSET")
  (handle-tcp-response stream))

(defun smtp-do-quit (stream)
  (net-write-line stream "QUIT")
  (handle-tcp-response stream))

;;; nope
(defun smtp-do-verify (stream user)
  (net-write-line stream "VRFY ~D" user)
  (handle-tcp-response stream))

(defun smtp-do-mail-from (stream user)
  (net-write-line stream "MAIL FROM:<~A>" user)
  (handle-tcp-response stream))

(defun smtp-do-recipient-to (stream user)
  (net-write-line stream "RCPT TO:<~A>" user)
  (handle-tcp-response stream))

(defun smtp-do-data (stream)
  "Sends DATA. Returns t or nil, 2nd value is smtp response."
  (net-write-line stream "DATA")
  (handle-tcp-response stream))

(defun message-id (&optional (time (get-universal-time)))
  (format nil "<~A.~A@~A>" time (gensym) (machine-instance)))

(defun smtp-send-header (stream from  ; *user-mail-address*
                                to smtp-type
                                &key subject boundary-value filename
                                (mime-version "1.0"))
   (net-write-line stream "From: ~A" from)
   ;; does RFC822 allow parens in the To: line ?  I don't think so...
   (if (listp to)
       (net-write-line stream "To: ~A ~{, ~A~}" (car to) (cdr to))
       (net-write-line stream "To: ~A" to))
   (unless (null subject) (net-write-line stream "Subject: ~A" subject))
   (let ((now (get-universal-time)))
     (net-write-line stream "Date: ~A" (boxer::rfc822-date-time now))
     (net-write-line stream "Message-ID: ~A" (message-id now)))
   (net-write-line stream "MIME-Version: ~A" mime-version)
   ;; handle specific content types here
   (case smtp-type
     (:text (net-write-line stream "Content-Type: text/plain; charset=\"us-ascii\""))
     (:multipart (net-write-line stream
                                 "Content-Type: Multipart/mixed; boundary=\"~A\""
                                 boundary-value))
     (:appledouble (net-write-line stream
                                 "Content-Type: Multipart/appledouble; boundary=\"~A\"; name=\"~A~A\""
                                 boundary-value (pathname-name filename)
                                 (let ((type (pathname-type filename)))
                                   (if (stringp type)
                                     (format nil ".~A" type)
                                     ""))))
     (otherwise (error "Don't know how to send a type ~A message" smtp-type)))
   (net-write-line stream "X-Mailer: ~A" (system-version))
   (net-write-line stream "") ; CRLF
   )


;; need to filter for ~'s since net-write-line uses FORMAT
(defun filter-smtp-data-string (string)
  (cond ((string= string ".") "..")
        ((find #\~ string)
         (let* ((lstring (length string))
                (return-string (make-string-buffer (+ lstring 2))))
           (dotimes (i lstring)
             (let ((char (char string i)))
               (cond ((char= char #\~)
                      (vector-push-extend #\~ return-string)
                      (vector-push-extend #\~ return-string))
                     (t (vector-push-extend char return-string)))))
           return-string))
        (t string)))

(defun net-write-smtp-data-line (stream string)
  (net-write-line stream (filter-smtp-data-string string)))

(defun net-terpri (stream) (net-write-line stream ""))

;; info for use by the smtp-send-header and smtp-send-box-xxx functions
;; if there are any sub boxes, assume :multipart for now
;; the one important case is if the box is a refrence to a foreign file,
;; then we should use appledouble, rather than the generic :multipart

(defun box-smtp-type (box)
  (if (not (null (getprop box :xref)))
      :appledouble
      (let ((type :text))
        (boxer::do-box-rows ((r box))
          (boxer::do-row-chas ((c r))
            (when (box? c) (setq type :multipart) (return))))
        type)))

(defun print-smtp-write-status (stream)
  (surf-message "Sending ~D bytes" (writing-stream-position stream)))

(defun smtp-send-box (to box &optional subject
                         (host *smtp-relay-host*)
                         (from *user-mail-address*))
  (let ((smtp-stream (open-tcp-stream host *smtp-port*))
        (boxer::*FILE-STATUS-LINE-UPDATE-FUNCTION* 'print-smtp-write-status)
        (mail-sent-flag nil))
    (unwind-protect
      (let ((smtp-type (box-smtp-type box)))
        ;; get the initial connect messages
        (handle-tcp-response smtp-stream)
        ;;
        (smtp-do-hello smtp-stream)
        (smtp-do-reset smtp-stream)
        ;; now send the mail....
        ;; who from (should we put in VRFY's here ?)
        (smtp-do-mail-from smtp-stream from)
        ;; who to (should we put in VRFY's here ?)
        (if (listp to)
            (dolist (a-to to) (smtp-do-recipient-to smtp-stream a-to))
            (smtp-do-recipient-to smtp-stream to))
        ;; SMTP protocol
        ;; Last chance to poll for interrupt, after the DATA op starts
        ;; we are committed to sending something...
        (boxer-eval::check-for-interrupt :interrupt "Mail Cancelled !")
        (smtp-do-data smtp-stream)
        ;; committed, so set the flag
        (setq mail-sent-flag t)
        ;; followed by the body of the message
        (case smtp-type
          (:text
           ;; consisting of the header info
           (smtp-send-header smtp-stream from to smtp-type :subject subject)
           ;; and the box contents...
           (boxer::do-box-rows ((row box))
             (net-write-smtp-data-line smtp-stream (boxer::text-string row))))
          (:multipart
           (let ((bv (mime-separator-value)))
             (smtp-send-header smtp-stream from to smtp-type :boundary-value bv
                               :subject subject)
             (send-multipart-box-data box smtp-stream bv)))
          (:appledouble
           (let* ((bv (mime-separator-value))
                  (xref (getprop box :xref))
                  (raw-path (when xref (box::xref-pathname xref)))
                  (pathname (or raw-path (unique-mime-path))))
             (smtp-send-header smtp-stream from to smtp-type :boundary-value bv
                               :subject subject :filename pathname)
             (send-appledouble-box-data box smtp-stream bv pathname)))))
      ;; NOTE: if we are in the middle of a DATA op, we need to send "."
      (cond ((null mail-sent-flag) (smtp-do-reset smtp-stream))
            (t        ;; end the message
             (net-write-line smtp-stream ".")
             (handle-tcp-response smtp-stream)
             (smtp-do-quit smtp-stream)))
      (close smtp-stream)
      (boxer::status-line-undisplay 'surf-message)
      (boxer::boxer-editor-message "Message sent to ~A via ~A" to host))))

#|
(defun smtp-send-box-text (host from to subject text)
  (let ((stream nil) (response nil) (myText nil))
    myText
    (setf stream (smtp-open-stream host))
    (when stream
      (unwind-protect
        (progn
          ;--- logon stuff ---
          (multiple-value-setq (response myText) (smtp-do-hello stream))
          (multiple-value-setq (response myText) (smtp-do-reset stream))
          )
        (unless response (RETURN-FROM smtp-send-a-mail))

        ;--- messaging stuff
        (when (multiple-value-setq (response myText) (smtp-do-verify stream from))
          (when (multiple-value-setq (response myText) (smtp-do-verify stream to))
            (multiple-value-setq (response myText) (smtp-do-mail-from stream from))
            (multiple-value-setq (response myText) (smtp-do-recipient-to stream to))
            (multiple-value-setq (response myText)
              (smtp-send-data stream (smtp-create-mail-head from to subject) text))
            ))

        ;--- logout stuff
        (smtp-do-quit stream))
      (smtp-close-stream stream))))
|#

#|

;; testing...

(defun mime-test (box from to &optional subject (smtp-stream *standard-output*))
  (let ((bv (mime-separator-value)))
    (smtp-send-header smtp-stream from to :multipart :boundary-value bv
                      :subject subject)
    (send-multipart-box-data box smtp-stream bv)))

;; "Just call on me and I'll send it along..."
(boxnet::mime-test tb "me" "you" "with love")

|#

;;;MIME utilities

(defun make-string-buffer (initial-size)
  (make-array initial-size :element-type #+mcl 'base-character
                                         #+lispworks 'base-char
                                         #-(or mcl lispworks) 'character
                           :adjustable t :fill-pointer 0))

;; preceeded by "--" for boundaries, last boundary also has trailing  "--"
;; the == makes sure it doesn;t look like quoted printable representation
(defun mime-separator-value ()
  (format nil "==~D~C~C~C~C~C~C~C~C~C~C" (get-universal-time) (random-alpha-char)
          (random-alpha-char) (random-alpha-char) (random-alpha-char)
          (random-alpha-char) (random-alpha-char) (random-alpha-char)
          (random-alpha-char) (random-alpha-char) (random-alpha-char)))

(defun random-alpha-char ()
  (char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (random 52)))

(defun write-mime-boundary (stream boundary-value &optional last?)
  (net-write-line stream (if last? "--~A--" "--~A") boundary-value)
  ;(format t "~&boundary ~A written @  ~D" boundary-value (writing-stream-position stream))
  )

(defvar *mime-prologue-message*
  "This is a message in the MIME (Multipurpose Internet Mail Extensions) Format")

;; sub boxes are considered to be a separate type of data
(defun send-multipart-box-data (box smtp-stream bv)
  ;; first a message for the prologue
  (net-write-smtp-data-line smtp-stream *mime-prologue-message*)
  (write-mime-boundary smtp-stream bv)
  (do ((row (boxer::first-inferior-row box) (boxer::next-row row))
       (new-boundary t)
       (r1st? t nil))
      ((null row))
    (do* ((values (row-mail-values row) (cdr values))
          (v1st? t nil)
          (val (car values) (car values)))
         ((null val)) ;(format t "~&~S  @ ~D" val (writing-stream-position smtp-stream))
      (cond ((box? val)
             (unless (or (and v1st? r1st?) new-boundary)
               ;; don't write a boundary if the box is the 1st thing
               ;; or if there already is a boundary...
               (write-mime-boundary smtp-stream bv))
             ;; when a box is encountered send it as a differing type...
             (send-box-mime-data val smtp-stream)
             (unless (and (null (boxer::next-row row)) (null (cdr values)))
               ;; don't write a boundary if the box is the LAST item either
               (write-mime-boundary smtp-stream bv)
               (setq new-boundary t)))
            (t (when new-boundary (net-terpri smtp-stream))
               (net-write-smtp-data-line smtp-stream val)
               (setq new-boundary nil)))))
  ;; finally end it
  (write-mime-boundary smtp-stream bv t))

;; return a list of string(s) and/or boxes that make up the line
;; simplest case is the same as text-string of row
(defun row-mail-values (row)
  (let* ((values nil)
         (full-length (boxer::length-in-chas row))
         (current-string (make-array full-length
                                     :element-type
                                     #+lispworks 'base-char
                                     #+(or mcl symbolics) 'character
                                     #-(or mcl symbolics lispworks) 'string-char
                                     :fill-pointer 0)))
    (boxer::do-vector-contents (cha (boxer::chas-array row) :index-var-name idx)
      (cond ((and (not (zerop& (fill-pointer current-string))) (box? cha))
             ;; there is a string being formed AND we run into a box
             (setq values (nconc values (list current-string cha)))
             (setq current-string (make-array (-& full-length idx)
                                              :element-type
                                              #+lispworks 'base-char
                                              #+(or mcl symbolics) 'character
                                              #-(or mcl symbolics lispworks)
                                              'string-char
                                              :fill-pointer 0)))
            ((box? cha)
             (setq values (nconc values (list cha))))
            (t ;;it's a character
             (vector-push cha current-string))))
    ;; add the last string
    (cond ((and (zerop& (fill-pointer current-string)) (null values)) '(""))
          ((zerop& (fill-pointer current-string)) values)
          (t (nconc values (list current-string))))))


;; This just does the default thing--which is  "Content-type: application/prs.boxer"
;; eventually move to application/vnd.boxer see RFC
;; header, followed by binary box data in Base64
;; later, we will want to put some sort of dispatch here for different types of box
;; (like Content-type:image/jpeg for certain graphics boxes)
(defun send-box-mime-data (box stream)
  (let ((xref (getprop box :xref))) ; check to see if box is an external reference
    ;(format t "~&Writing box:~A  @  ~D" box (writing-stream-position stream))
    (cond ((null xref)
           (send-mime-box box stream))
          (t
           (ecase *mac-mime-file-encoding*
             ;(:binhex (send-mime-binhex-data (box::mac-file-ref-pathname xref)
             ;                               stream)
             (:appledouble
              (send-mime-appledouble-data  box (box::xref-pathname xref)
                                           stream))
             (:applesingle
              (send-mime-applesingle-data (box::xref-pathname xref)
                                          stream))))
;          (t ; dispatch...
;           (multiple-value-bind (mtype subtype)
;               (mac-file->mime-values (boxer::mac-file-ref-pathname xref))
;             ;; quickie for now...
;             (if (eq mtype :text)
;                 (send-mime-text-data
;                  mtype subtype (box::mac-file-ref-pathname xref) stream)
;                 (send-mime-binary-data
;                  mtype subtype (box::mac-file-ref-pathname xref) stream))))
          )))

;; ones we know for sure...
;; use a dialog for user to add more types.  Should use same dialog for
;; both directions.
;; this is a temporary crock (especially so in the case of the PC)
(defvar boxnet::*mac-file-mime-types*
  '((:boxr :application :boxer)
    (:jpeg :image :jpeg)
    (:|GIFf| :image :gif)
    #+lispworks
    (:box :application :boxer)
    #+lispworks
    (:jpg :image :jpeg)
    #+lispworks
    (:gif :image :gif)))

;; this should be changed to use *mime-mac-file-types*...
(defun mac-file->mime-values (pathname)
  (let* (;(creator #+mcl (ccl::mac-file-creator pathname))
         (ftype   #+mcl (ccl::mac-file-type    pathname)
                  #-mcl (intern (pathname-type pathname) (find-package "KEYWORD")))
         (mime-info (cdr (assoc ftype *mac-file-mime-types*))))
    (cond ((not (null mime-info)) (values (car mime-info) (Cadr mime-info)))
          (t (values :application :octet-stream)))))

(defvar *query-for-unknown-mime-type* t)

;; this is a list of triples: mime type string, mac creator (signature) and mac
;; file type.  Search on the 1st item to get mac file types from a mime type
;; search on the last item to get a mime type from a mac file type

(defvar *mime-mac-file-types*
  '(("application/boxer" :boxr :boxr) ("application/prs.boxer" :boxr :boxr)
    ("image/gif" :gkon :|GIFf|) ("image/jpeg" :gkon :jpeg) ("image/jpg" :gkon :jpeg)
    ("application/mac-binhex40" :bnhq :text)
    ("application/stuffit" :|SIT!| :sitd)
    ("application/pdf" :caro :|PDF |)
    ("application/msword" :mswd :wdbn)
    ("application/rft" :mswd :text)
    ))

(defun mime-values->mac-file-values (mime-type)
  (let ((existing (assoc mime-type *mime-mac-file-types* :test #'string-equal)))
    (cond ((not (null existing)) (values (cadr existing) (caddr existing)))
          ((not (null *query-for-unknown-mime-type*))
           (add-new-mime-type :mime-type mime-type))
          (t (values :???? :????)))))

#| ;; old version

(defun mime-values->mac-file-values (mime-type)
  (cond ((string-equal mime-type "image/jpeg") (values :gkon :jpeg))
        ((string-equal mime-type "image/gif")  (values :gkon :|GIFf|))
        ((string-equal mime-type "application/mac-binhex40") (values :bnhq :text))
        (t (values :???? :????))))
|#


;; Mime to mac mapping utilities
;; SAVE, READ and EDIT

(defun save-mime-mapping (&optional (pathname
                                     #+mcl (merge-pathnames
                                            "Mime-Map"
                                            (boxer::Default-mac-pref-file-name))
                                     #-mcl "~/.mime-map"))
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (dolist (entry *mime-mac-file-types*)
      (format s "~A ~A ~A~&" (car entry) (cadr entry) (caddr entry)))))

(defun read-mime-mapping (&optional (pathname
                                     #+mcl (merge-pathnames
                                            "Mime-Map"
                                            (boxer::Default-mac-pref-file-name))
                                     #-mcl "~/.mime-map"))
  (let ((entries nil) (eof-value (list 'eof)) (finished? nil))
    (when (probe-file pathname)
      (ignore-errors
       (with-open-file (s pathname :direction :input)
         (loop
           (let ((line (read-line s nil eof-value)))
             (cond ((or (eq line eof-value) (string= line ""))
                    (setq finished? t)
                    (return))
                   (t ;; should be 3 things on the line separated by spaces
                    ;; remember that the last 2 items must be 4 chars long
                    ;; even if some of those chars are spaces
                    (let ((1stspace (position #\space line)))
                      (push
                       (list (subseq line 0 1stspace)
                             (intern (subseq line (+ 1stspace 1) (+ 1stspace 5))
                                     (find-package "KEYWORD"))
                             (intern (subseq line (+ 1stspace 6) (+ 1stspace 10))
                                     (find-package "KEYWORD")))
                       entries))))))))
      (unless (null finished?)
        (setq *mime-mac-file-types* entries)))))


(defun edit-mime-entry (entry)
  (let ((new-values (boxer::mime-type-dialog (car entry) (string (cadr entry))
                                             (string (caddr entry)))))
    (cond ((null new-values))
          (t (setf (car entry)   (car new-values)
                   (cadr entry)  (cadr new-values)
                   (caddr entry) (caddr new-values))))))

(defun add-new-mime-type (&key mime-type app-sig file-type)
  (let ((new-values (boxer::mime-type-dialog (string mime-type)
                                             (when app-sig (string app-sig))
                                             (when file-type (string file-type)))))
    (cond ((null new-values))
          (t (push new-values *mime-mac-file-types*)))))

;; NOTE:mime-type-dialog & edit-mime-mapping are defined in the xxx-menu.lisp files



;; check to see if the box is a link and, if so, include the link tag
;; in the header.  If the file box has not been filled send the data
;; from the file
(defun send-mime-box (box stream)
  (let ((file (getprop box :associated-file)))
    (if (null file)
      (net-write-smtp-data-line stream "Content-Type: application/prs.boxer")
      (net-write-smtp-data-line stream
                                "Content-Type: application/prs.boxer ; link=\"T\""))
    (net-write-smtp-data-line stream "Content-Transfer-Encoding: base64")
    ;(net-write-smtp-data-line stream "Content-Description: A Boxer Box ~A" (box::name box))
    (net-terpri stream)
    (if (and (null (slot-value box 'boxer::first-inferior-row))
             (storage-chunk? box))
      ;; file box is still in file, not in editor
      (send-mime-binary-data-internal file stream)
      (with-base64-stream (binstream stream :direction :output)
        (boxer::dump-top-level-box-to-stream box binstream)))))

;; this is the hook for using quoted-printable representation
;; just writes out the lines for now....
(defun send-mime-text-data (mtype subtype file mailstream)
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Type: ~A/~A ; name=\"~A~A\""
                                    (string-downcase mtype)
                                    (string-downcase subtype)
                                    (pathname-name file)
                                    (let ((type (pathname-type file)))
                                      (if (stringp type)
                                        (format nil ".~A" type)
                                        ""))))
  ;(net-write-smtp-data-line mailstream "Content-Transfer-Encoding: quoted-printable")
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Description: ~A" (namestring file)))
  (net-terpri mailstream)
  (with-open-file (s file)
    (loop (let ((line (read-line s nil nil)))
            (if (null line) (return)
                (net-write-smtp-data-line mailstream line))))))

(defun send-mime-binary-data (mtype subtype file mailstream)
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Type: ~A/~A ; name=\"~A~A\""
                                    (string-downcase mtype)
                                    (string-downcase subtype)
                                    (pathname-name file)
                                    (let ((type (pathname-type file)))
                                      (if (stringp type)
                                        (format nil ".~A" type)
                                        ""))))
  (net-write-smtp-data-line mailstream "Content-Transfer-Encoding: base64")
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Description: ~A" (namestring file)))
  (net-terpri mailstream)
  (send-mime-binary-data-internal file mailstream))

;; send the contents of the <file> as base64 encoded binary data
;; assumes the appropriate type header has already been written out
(defun send-mime-binary-data-internal (file mailstream)
  (with-open-file (filestream file :direction :input :element-type '(unsigned-byte 8))
    (with-base64-stream (binstream mailstream :direction :output)
      (loop (let ((byte (read-byte filestream nil nil)))
              (if (null byte) (return) (write-byte byte binstream)))))))

(defboxer-primitive bu::mail ((boxer-eval::dont-copy address) (bu::port-to message))
  ;; make sure we have up to date editor structure
  (boxer::process-editor-mutation-queue-within-eval)
  (let ((to (with-collection
              (dolist (er (boxer::get-box-rows address))
                (collect (boxer::evrow-text-string er address))))))
    (when (null (cdr to)) (setq to (car to)))
    ;; should do some reality checking on the "to" arg
    (let ((subject (boxer::lookup-variable-in-box-only (box-or-port-target message)
                                                       'bu::subject nil)))
      (if (null subject)
          (smtp-send-box to (box-or-port-target message))
          (smtp-send-box to (box-or-port-target message) (box-text-string subject))))
    boxer-eval::*novalue*))


(defboxer-primitive bu::mail-document ((boxer-eval::dont-copy address))
    ;; make sure the editor structure is up to date
  (boxer::process-editor-mutation-queue-within-eval)
  (if (not (box? boxer-eval::*lexical-variables-root*))
      (boxer-eval::primitive-signal-error :file "You can only MAIL editor boxes")
      (let* ((mailbox (boxer::current-file-box))
             (to (with-collection
                   (dolist (er (boxer::get-box-rows address))
                     (collect (boxer::evrow-text-string er address)))))
             (subject (boxer::lookup-variable-in-box-only mailbox 'bu::subject nil)))
        (smtp-send-box to mailbox (unless (null subject) (box-text-string subject)))
        boxer-eval::*novalue*)))



;;;; Reading Mail using the POP3 protocol

(defvar *pop-port-number* 110) ; sometimes 109 (RFC1725 claims it should be 110)

(defvar *pop-stream* nil)

(defvar *cache-pop-password* nil)

(defvar *dele-msg-on-retr* nil
  "Should the POP client delete messages fromthe server as it retrieves them ?")

(defvar *defer-long-messages?* nil)

(defvar *max-immediate-message-length* 10000
  "POP messages longer than this will not be initially read in")

(defvar *max-viewable-message-length* 200000
  "POP messages linger than this will be saved to a file")

(defvar *delete-loaded-messages?* nil)

(defvar *report-pop-message-reading-progress?* nil)

;;  Pop classes are an extension of URL's to support a black box mechanism
;; for use with the POP reader

;; for messages which are longer than *max-immediate-message-length*, the
;; message body is a black box with a URL of mail-message-body
;; Note: this will only work if the POP3 server supports the TOP command
;; see get-pop-status

(defclass mail-message
  (url)
  ((message-number :initarg :message-number :initform 1)
   ;; should be filled either with UIDL or else, SIZE if UIDL is unsupported
   (uid :initarg :uid :initform nil))
  ;; (:metaclass block-compile-class)
  )

(defclass mail-message-body
  (mail-message)
  ()
  ;; (:metaclass block-compile-class)
  )

;; these are contained in a box with a URL of:

;; this is the URL for a mailbox.  It contains all the neccessary info to
;; open a pop connection (password can be either cached or queried, the default
;; should be query to avoid the danger of saving clear text passwords in boxer
;; files.  The dump method should take care to NEVER save out a password.
;; The pop file stream is cached here instead of a global var to allow people
;; to have multiple mailboxes
(defclass pop-url
  (url)
  ((user :initform nil :accessor pop-user)
   (password :initform nil)
   (host :initform nil :accessor pop-host)
   (port :initform *pop-port-number* :accessor pop-port)
   (stream :initform nil :accessor pop-stream)
   ;; a slot to distinguish a particular mail session, minimally, a # of messages,
   ;; size in bytes pair
   (session-id :initform nil :accessor pop-session-id))
  ;; (:metaclass block-compile-class)
  )

;; substantially the same as net-read-line except
;; we reuse the same vector to reduce CONSing because we know
;; that the line will immediately be passed to make-net-row
;; also "." hacking is handled transparently here, "." only lines return NIL
;; and lines which begin with "." but are longer than 1 char will have
;; the initial "." stripped off as per page 3 of RFC1725
(defvar *mail-line-buffer* (make-string-buffer 80))

(defvar *net-mail-read-line-hook* nil)

(defun net-mail-read-line (stream &optional (wait? t) append?)
  (if (null *net-mail-read-line-hook*)
      (net-mail-read-line-basic stream wait? append?)
      (funcall *net-mail-read-line-hook* stream wait? append?)))

(defvar *debug-mail-line-buffer* nil)

;; this has to return strings with fill-pointers for make-net-row
;; this is gross, but we'll deliberately use read-line for now because it
;; should be smart about newlines in a platform specific way...
(defun net-mail-read-line-from-file (stream wait? append?)
  (declare (ignore wait?))
  (unless append? (setf (fill-pointer *mail-line-buffer*) 0))
  (let ((rawline (read-line stream nil nil)))
    (cond ((null rawline) (values *mail-line-buffer* 0))
          (t (dotimes (i (length rawline))
               (vector-push-extend (char rawline i) *mail-line-buffer*))
             (values *mail-line-buffer* (length rawline))))))

;; need this to read "text" files onthe mac which uses #\LF only, as a line
;; separator
#+mcl
(defun net-mail-read-line-from-mac-file (stream wait? append?)
  (declare (ignore wait?))
  (unless append? (setf (fill-pointer *mail-line-buffer*) 0))
  (unless (ccl:stream-eofp stream)
    (let ((char nil))
      (do ((i 0 (1+& i)))
          ((or (null (setq char (ccl:stream-tyi stream)))
               (char= char #\LF))
           (values *mail-line-buffer* i))
        (vector-push-extend char *mail-line-buffer*)))))

;; need to hack and EOF !!!
;; callers should watch for returned NIL
(defun net-mail-read-line-basic (stream wait? append?)
  (when (or wait? (listen stream))
    ;; this is an open coded version of ccl::telnet-read-line counting added
    (unless append? (setf (fill-pointer *mail-line-buffer*) 0))
    (let ((period? nil))
      #+mcl
      (unless (ccl:stream-eofp stream)
        (let ((char nil))
          (do ((i 0 (1+& i)))
              ((or (null (setq char (ccl:stream-tyi stream)))
                   (mail-eol? stream char))
               (unless period? (values *mail-line-buffer* i)))
            (cond ((and (char= char #\.) (=& i 0)) (setq period? t))
                  (t
                   (unless (null period?) (setq period? nil))
                   (vector-push-extend char *mail-line-buffer*))))))
      #-mcl
      (let ((eof-value (list 'eof)))
        (unless (eq (peek-char nil stream nil eof-value) eof-value)
          (let ((char nil))
            (do ((i 0 (1+& i)))
                ((or (null (setq char (read-char stream nil nil)))
                     (mail-eol? stream char))
                 (unless period? (values *mail-line-buffer* i)))
              (cond ((and (char= char #\.) (=& i 0)) (setq period? t))
                    (t
                     (unless (null period?) (setq period? nil))
                     (vector-push-extend char *mail-line-buffer*)))))))
    )))

;; stream based checks for end of line,
#-mcl
(defmethod mail-eol? ((stream stream) char)
  (when (and (eq char #\CR) (eq (peek-char nil stream nil nil) #\LF))
    ;; get the LF out
    (read-char stream nil nil)
    T))

#+mcl
(defmethod mail-eol? ((stream ccl::tcp-stream) char)
  (when (and (eq char #\CR) (eq (ccl::stream-peek stream) #\LF))
    ;; get the LF out
    (ccl::stream-tyi stream)
    T))

#+carbon-compat
(defmethod mail-eol? ((stream ccl::opentransport-tcp-stream) char)
  (when (and (eq char #\CR) (eq (ccl::stream-peek stream) #\LF))
    ;; get the LF out
    (ccl::stream-tyi stream)
    T))

;; a fast specific version of make-row, that works only on adjustable
;; arrays of chars like that returned from net-mail-read-line
(defun make-net-row (char-array)
  (let ((chas-array (boxer::make-chas-array (fill-pointer char-array))))
    (boxer::with-fast-chas-array-manipulation (chas-array chas)
      (dotimes& (i (fill-pointer char-array))
        (setf (aref chas i) (aref char-array i)))
      (setf (boxer::chas-array-active-length chas-array)
            (fill-pointer char-array)))
    (boxer::make-initialized-row :chas-array chas-array)))


;;; these 5 methods are meant to be inherited by mail-message-body

(defmethod initialize-instance ((url mail-message) &rest initargs)
  (call-next-method)
  (let ((n (getf initargs :message-number))
        (id (getf initargs :uid)))
    (unless (null n) (setf (slot-value url 'message-number) n))
    (unless (null id) (setf (slot-value url 'uid) id))))

(defmethod copy-url ((url mail-message))
  (let ((new (call-next-method)))
    (setf (slot-value new 'message-number) (slot-value url 'message-number))
    new))

(defmethod dump-plist-length ((url mail-message)) (+& (call-next-method) 2))

(defmethod dump-plist-internal ((url mail-message) stream)
  (call-next-method)
  (dump-boxer-thing :message-number stream)
  (dump-boxer-thing (slot-value url 'message-number) stream))

;; scoping upward for the appropriate mail box will be the same
(defmethod get-message-mail-box ((box boxer::box))
  (let ((url (getprop box :url)))
    (cond ((typep url 'pop-url) (values box url))
          (t (let ((sup (superior-box box)))
               (when (box? sup) (get-message-mail-box sup)))))))

;;; The fill-box-using-url method is different
(defmethod fill-box-using-url ((url mail-message) box)
  (multiple-value-bind (mail-box pop-url)
      (get-message-mail-box box)
    (cond ((null mail-box)
           (boxer-eval::primitive-signal-error :net-error "Message "
                                         (slot-value url 'message-number)
                                         " not in a mail box"))
          (t (insure-pop-stream pop-url)
             (get-message-internal (pop-stream pop-url)
                                   (slot-value url 'message-number) box)))))

(defmethod fill-box-using-url ((url mail-message-body) box)
  (multiple-value-bind (mail-box pop-url)
      (get-message-mail-box box)
    (cond ((null mail-box)
           (boxer-eval::primitive-signal-error :net-error "Message "
                                         (slot-value url 'message-number)
                                         " not in a mail box"))
          (t (insure-pop-stream pop-url)
             (let ((ps (pop-stream pop-url)))
               ;; issue the RETR command
               (net-write-line ps "RETR ~D" (slot-value url 'message-number))
               (handle-pop-response ps)
               ;; skip past the header...
               (do ((line (net-mail-read-line ps) (net-mail-read-line ps)))
                   ((or (null line) (string= line ""))))
               ;; now fill the body from stream
               (do ((line (net-mail-read-line ps) (net-mail-read-line ps)))
                   ((null line))
                 (append-row box (make-net-row line))))))))

(defmethod initialize-instance ((url pop-url) &rest initargs)
  initargs
  (call-next-method)
  (multiple-value-bind (user password host port path canonicalized-scheme-string)
      (decode-net-url (slot-value url 'scheme-string))
    (declare (ignore path))
    (setf (slot-value url 'user) user
          (slot-value url 'password) password
          (slot-value url 'host) (if (and (or (null host) (string= host ""))
                                          (boundp *pop-host*))
                                     *pop-host*
                                     host)
          (slot-value url 'port) (or port *pop-port-number*))
    (unless (null canonicalized-scheme-string)
      (setf (slot-value url 'scheme-string) canonicalized-scheme-string))))

(defmethod copy-url ((url pop-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'user) (slot-value url 'user)
          ;(slot-value new 'password) (slot-value url 'password)
          (slot-value new 'host) (slot-value url 'host)
          (slot-value new 'port) (slot-value url 'port))
    new))


;; similiar to handle-tcp-response except the POP3 responses start with either
;; "+OK" or "-ERR" instead of the 3 digit TCP codes
(defun handle-pop-response (stream)
  (let* ((response (net-read-line stream t))
         (indicator (char response 0)))
    (unless (null response) (debugging-message response))
    (when (char= #\- indicator)
      (signal-tcp-error (subseq response 0 3) response))
    response))

;; Quicky version of signal-tcp-error, no checking for bound error handlers
(defun signal-pop-error (string)
  (boxer-eval::primitive-signal-error :pop-error (copy-seq string)))

;; messages to be deleted from the server
(defvar *pop-server-deletion-queue* :toplevel)

;; queued objects can be either boxes or message numbers....
(defmacro with-pop-server-queued-deletion (stream &body body)
  `(let ((*pop-server-deletion-queue* nil))
     (prog1 (progn . ,body)
       (dolist (thing *pop-server-deletion-queue*)
         (cond ((numberp thing) (pop-dele-msg ,stream thing))
               ((box? thing) (delete-msg-from-pop-server ,stream thing))
               (t (error "Unexpected object in mail deletion queue: ~A" thing)))))))

(defun queue-for-pop-server-deletion (thing)
  (unless (eq *pop-server-deletion-queue* ':toplevel)
    (push thing *pop-server-deletion-queue*)))

;; this is supposed to return a stream ready for the POP TRANSACTION state
(defmethod open-pop-stream ((self pop-url))
  (let ((stream (open-tcp-stream (slot-value self 'host)
                                 (slot-value self 'port))))
    ;; look for the greeting
    (handle-pop-response stream)
    ;; login
    (net-write-line stream "USER ~A" (slot-value self 'user))
    (handle-pop-response stream)
    ;; password (add multiple attempts using *correct-password-retries* later)
    (net-write-line stream "PASS ~A" (or (slot-value self 'password)
                                         (boxer::get-string-from-status-line
                                          "Enter password:" nil)))
    (handle-pop-response stream)
    ;; if we've got this far, it is safe to set the stream
    (setf (slot-value self 'stream) stream)
    ;; check to see if TOP is supported here ?
    stream))

;; Make sure the stream is still active since POP allows servers to close
;; idle connections after a certain timeout
(defun valid-pop-stream? (stream)
  (and (not (null stream))
       (open-stream-p stream)
       #+mcl (ccl::stream-eofp stream)
       ))

;; stubware for now, we need to store user/host in some pop object so
;; we have the info to relogin if there has been an idle timeout disconnect
(defmethod insure-pop-stream ((url pop-url))
  (unless (valid-pop-stream? (slot-value url 'stream))
    ;; do we need to make sure the old value is handled ?
    (setf (slot-value url 'stream) (open-pop-stream url))))

(defmethod fill-box-using-url ((url pop-url) box)
  (insure-pop-stream url)
  (unwind-protect
      (multiple-value-bind (nmsgs nbytes)
          (get-pop-status (slot-value url 'stream))
        (debugging-message "POP STAT ~A ~A" nmsgs nbytes)
        (surf-message "Mail Box: ~D messages, ~D bytes" nmsgs nbytes)
        (setf (slot-value url 'session-id) (cons nmsgs nbytes))
        (with-pop-server-queued-deletion (slot-value url 'stream)
          (dotimes (i nmsgs)
            (append-row box (make-row
                             (list (get-message (slot-value url 'stream)
                                                (1+ i))))))))
    (close-pop-stream (slot-value url 'stream))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
        (boxer::status-line-undisplay 'surf-message))))

;; same as above except it returns a VC instead of modifying an editor-box
(defmethod make-vc-using-url ((url pop-url))
  (insure-pop-stream url)
  (let ((vc-rows nil))
    (unwind-protect
        (multiple-value-bind (nmsgs nbytes)
            (get-pop-status (slot-value url 'stream))
          (debugging-message "POP STAT ~A ~A" nmsgs nbytes)
          (surf-message "Mail Box: ~D messages, ~D bytes" nmsgs nbytes)
          (setf (slot-value url 'session-id) (cons nmsgs nbytes))
          (with-pop-server-queued-deletion (slot-value url 'stream)
             (dotimes (i nmsgs)
               (push (boxer::make-evrow-from-entry
                      (get-message (slot-value url 'stream) (1+ i)))
                     vc-rows))))
      (close-pop-stream (slot-value url 'stream))
      ;; make sure any messages are erased
      #-mcl
      (boxer::status-line-undisplay 'surf-message)
      #+mcl
      (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
          (boxer::drawing-on-window (boxer::*boxer-pane*)
            (boxer::status-line-undisplay 'surf-message))
          (boxer::status-line-undisplay 'surf-message)))
    (make-vc (nreverse vc-rows))))

;; protocol level functions, these talk directory to the pop server stream

;; returns 2 values number of messages and total number of byte in mailbox,
;; uses the POP "STAT" command
(defun get-pop-status (pop-stream)
  (net-write-control-line pop-stream "STAT")
  (let* ((response (net-read-line pop-stream))
         (indicator (char response 0)))
    (cond ((char= indicator #\-) (signal-pop-error response))
          (t (let* ((1space (position #\space response :test #'char=))
                    (2space (position #\space response
                                      :test #'char= :start (1+ 1space)))
                    (3space (position #\space response
                                      :test #'char= :start (1+ 2space))))
               (values (string->number response :start (1+ 1space) :stop 2space)
                       (string->number response
                                       :start (1+ 2space) :stop 3space)))))))


(defun pop-msg-size (stream message-number)
  (net-write-control-line stream "LIST ~D" message-number)
  (let* ((response (net-read-line stream))
         (indicator (char response 0)))
    (cond ((char= indicator #\-) (signal-pop-error response))
          (t (let* ((1space (position #\space response :test #'char=))
                    (2space (position #\space response
                                      :test #'char= :start (1+ 1space)))
                    (3space (position #\space response
                                      :test #'char= :start (1+ 2space))))
               (string->number response :start (1+ 2space) :stop 3space))))))

(defun pop-dele-msg (stream message-number)
  (net-write-control-line stream "DELE ~D" message-number)
  (surf-message "Marking Message ~D as Deleted" message-number)
  (let ((response (net-read-line stream)))
    (when (char= (char response 0) #\-) (signal-pop-error response))))

(defun close-pop-stream (pop-stream &optional reset?)
  (when reset?
    (net-write-control-line pop-stream "RSET") (handle-pop-response pop-stream))
  (net-write-control-line pop-stream "QUIT")
  (handle-pop-response pop-stream)
  (debugging-message "Closing pop stream")
  (close pop-stream))

(defun pop-top-unsupported () (throw 'no-top nil))

;; For now, message boxes consist of a header box and a message box.  Multipart
;; MIME mail may have several boxes following the header box.  Overly large boxes
;; will use the TOP command to only read in the header.  The message box will
;; then consist of an open header box and a closed (black box) message box
;; with a MAIL-MESSAGE-BODY url.  Note that this may conflict with the a
;; multipart MIME header.  If the top command is unsupported (since it is
;; listed as optional in RFC1725), then the entire message box is left black
;; with a MAIL-MESSAGE url

(defun delete-msg-from-pop-server (stream box)
  (let ((url (getprop box :url)))
    (unless (or (null url)
                (not (typep url 'mail-message)))
      (delete-message-no url stream))))


;; assumes stream is valid and open
(defmethod delete-message-no ((url mail-message) pop-stream)
  (let* ((uid (slot-value url 'uid))
         (message-number (slot-value url 'message-number))
         (server-msg-id (pop-msg-size pop-stream message-number)))
    (cond ((= uid server-msg-id)
           ;; check to see if ID's match before sending the DELE
           (pop-dele-msg pop-stream message-number))
          (t
           ;; if the ID's don't match, loop to the beginning looking for an ID
           ;; match because the message may have drifted toward the beginning of
           ;; the mailbox because of intervening access of the mailbox with
           ;; selecting message deletes because of the nature of mail(new messages
           ;; appending), the message can only move toward the beginning of
           ;; the mailbox
           (do ((msg (1-& message-number) (1-& msg)))
               ((zerop& msg))
             (let ((msize (pop-msg-size pop-stream msg)))
               (when (and (= msize uid)
                          (boxer::status-line-y-or-n-p
                           (format
                            nil "Message ~D matches server msg ~D. Delete ~D from server ?"
                            message-number msg msg)))
                 (pop-dele-msg pop-stream msg))))))))

(defun get-message (stream message-number)
  (let* ((msize (pop-msg-size stream message-number))
         (url (make-instance 'mail-message
                :message-number message-number :uid msize))
         (*report-pop-message-reading-progress?* nil))
    ;; don't bother since many server don't support UIDL yet
    ;(net-write-line stream "UIDL ~D" message-number)
    ;(setf (slot-value url 'uid) (net-read-line stream))
    ;; if the message size is too big, don't read it in right away
    ;; WARNING: deferred messages are not fully supported as of 6/22/96
    ;; Don't change *defer-long-messages?* to T until you figure out
    ;; how to access a deferred message which has been moved out of the
    ;; mail box into a random place in the boxer world an unspecified
    ;; (possibly long) time ago
    (when (> msize *max-immediate-message-length*)
      (setq *report-pop-message-reading-progress?* t))
    (surf-message "Reading Message ~D" message-number)
    (cond ((and *defer-long-messages?*
                (> msize *max-immediate-message-length*))
           ;; just try and get the header
           (let ((header-ok? nil))
             (net-write-control-line stream "TOP ~D 0" message-number)
             (catch 'no-top
               (tcp-handler-bind (("-" pop-top-unsupported))
                 (handle-pop-response stream))
               (setq header-ok? t))
             ;; if we got the header, then make a
             (if header-ok?
               (let ((header-box (make-header-box-from-stream
                                  stream t)))
                 (make-delayed-pop-message message-number
                                           header-box))
               (make-delayed-pop-message message-number))))
          (t ;; make an entire message box
           (let ((msg-box (get-message-1 stream message-number
                                         (> msize
                                            *max-viewable-message-length*))))
             ;; if we're here, we know we've got a box without blowing out
             (putprop msg-box url :url)
             (when *delete-loaded-messages?*
               (queue-for-pop-server-deletion msg-box))
             (setf (boxer::all-new-box? msg-box) t)
             msg-box)))))

;; obsolete, here to (incompletely) support (fill-box-from-url mail-message)
(defun get-message-internal (stream message-number &optional box)
  (net-write-control-line stream "RETR ~D" message-number)
  (handle-pop-response stream)
  (multiple-value-bind (header-box mime-type mime-values header-size)
      (make-header-box-from-stream stream)
    (declare (ignore mime-type mime-values header-size))
    ;; we grab the header first because it might tell us
    ;; how we should process the rest of the message
    ;; in particular, MIME information will appear in the
    ;; header
    (make-pop-message header-box stream box)))

;; this is the core message construction function
;; It works by first, pulling out the header and returning a box with
;; header info in it.  We do it in 2 parts because the header can determine
;; how to process the body of the message.  In particular, certain MIME
;; types will require radically different types of processing.
;; the MAKE-HEADER-BOX-FROM-STREAM function returns a box, typically with
;; textual header info already at the top.  It may also contain some
;; header info as boxer structure in the closet to facilitate writing
;; mail filters in boxer.  If there is a MIME type, it is returned as
;; the 2nd value.  Additional MIME info is returned as a keyword list in
;; the 3rd value.  The most important possibilities being
;; Content-Transfer-Encoding (:base64 or :qp) and multipart boundary values
;; for multipart MIME types.  Note that make-header-box-from-stream MAY be
;; called recursively in the case of multipart messages.
;; the 4th value is # of bytes read for the benefit of progress reporting

(defun get-message-1 (stream message-number &optional save-to-file?)
  (net-write-control-line stream "RETR ~D" message-number)
  (handle-pop-response stream)
  (multiple-value-bind (header-box mime-type mime-values header-size)
      (make-header-box-from-stream stream)
    ;; we grab the header first because it might tell us
    ;; how we should process the rest of the message
    ;; in particular, MIME information will appear in the
    ;; header
    (prog1
      (append-pop-message-body header-box stream mime-type mime-values
                               header-size save-to-file?)
      ;; some of the append sub functions (especially the binary ones) can leave
      ;; unprocessed lines in the stream since they exit as soon as the binary
      ;; data ends
      (when (listen stream)
        (loop (let ((xtraline (net-mail-read-line stream nil)))
                (when (null xtraline) (return))))))))

;;; Header Line Processing
;; To DO - still need to parse character and string quoting, maybe make a general
;; header line parser at some point

;; This hacks Line-Folding
(defun get-header-line (stream)
  (multiple-value-bind (line llength)
      (net-mail-read-line stream)
    (loop (if (and (not (string= line "")) (LWSP-char? (peek-char nil stream)))
              (multiple-value-setq (line llength)
                (net-mail-read-line stream t t)) ; this will append to line
              (return (values line llength))))))

;; header fields which get their own box in the closet, should eventually
;; be user selectable and the boxes should be lightweight variables
(defvar *box-worthy-pop-msg-header-fields*
  '("Sender" "Reply-to" "From" "To" "Subject" "Date" "cc"))

(defun header-field-has-machine-part? (field-name)
  (member field-name '("From" "To" "Reply-to" "Sender") :test #'string=))

;; extracts the part of the line between "<" and ">"
;; expects the line to be unfolded, that is, after the "<" there better be a ">"
(defun header-line-machine-field (line &optional
                                       (return-field (make-string-buffer 50)))
  (let ((start (do* ((i 0 (1+ i))
                     (stop (1-(length line)))
                     (char (aref line i) (aref line i)))
                    ((or (= i stop) (char= char #\<)) (unless (= i stop) (1+ i))))))
    (if (null start)
        (header-line-value line return-field)
        (do* ((j start (1+ j))
              (char (aref line j) (aref line j)))
             ((char= char #\>) return-field)
          (vector-push-extend char return-field)))))

(defun LWSP-char? (char) (member char '(#\space #\tab)))

;; this also removes leading whitespace and ignores comments between ()'s
(defun header-line-value (line &optional
                               (return-field (make-string-buffer 50))
                               (value-start (do* ((i 0 (1+ i))
                                                  (char (aref line i)
                                                        (aref line i)))
                                                 ((char= char #\:) (+ i 1)))))
  (do* ((j value-start (1+ j))
        (stop (length line))
        (start? t)
        (in-comment? nil))
       ((>= j stop) return-field)
    (let ((char (aref line j)))
      (cond ((and start? (LWSP-char? char))
             ;; ignore leading whitespace, TAB?
             )
            (in-comment?
             ;; ignore material inside parens
             (when (char= char #\)) (setq in-comment? nil)))
            ((char= char #\() (setq in-comment? t))
            (t
             (when start? (setq start? nil))
             (vector-push-extend char return-field))))))

;; MIME fields can have additional parameters, delineated by ";" and
;; separated by "="
;; any parameter values will be returned as an additional value in a keyword list
;; NOTE: careful, parameter values sometimes are "..." quoted
(defun mime-header-line-value (line &optional
                               (return-field (make-string-buffer 50)))
  (do* ((j (do* ((i 0 (1+ i)) (char (aref line i) (aref line i)))
                ((char= char #\:) (+ i 1)))
           (1+ j))
        (stop (length line))
        (start? t)
        (in-parameter? nil)
        (parameter-string nil)
        (parameters nil)
        (in-quote? nil)
        (in-comment? nil))
       ((>= j stop)
        (when (and (eq in-parameter? :parvalue) (not (null parameter-string)))
          (setq parameters
                (append parameters (list (copy-seq parameter-string)))))
        (values (intern (string-upcase return-field) (find-package "KEYWORD"))
                parameters))
    (let ((char (aref line j)))
      (cond ((LWSP-char? char)
             ;; all whitespace, TAB?
             (when (and (eq in-parameter? :parvalue) in-quote?)
               ;; 6/14/99 only add spaces in the middle of string quotes
               (vector-push-extend char parameter-string)))
            (in-comment?
             ;; ignore material inside parens
             (when (char= char #\)) (setq in-comment? nil)))
            ((char= char #\()
             (if (and (eq in-parameter? :parvalue) in-quote?)
                 (vector-push-extend #\( parameter-string)
               (setq in-comment? t)))
            ((char= char #\;)
             (cond ((null parameter-string)
                    (setq parameter-string (make-string-buffer 30)))
                   (t ; must be a 2nd+ parameter
                    (setq parameters
                          (append parameters
                                  (list (copy-seq parameter-string))))
                    (setf (fill-pointer parameter-string) 0)))
             (setq in-parameter? :parname))
            ((and (eq in-parameter? :parname) (char= char #\=))
             (setq parameters
                   (append parameters (list (intern (string-upcase
                                                     parameter-string)
                                                    (find-package "KEYWORD")))))
             (setf (fill-pointer parameter-string) 0)
             (setq in-parameter? :parvalue))
            ((char= char #\")
             (cond ((not in-quote?) (setq in-quote? T))
                   (t               (setq in-quote? nil))))
            (t
             (when start? (setq start? nil))
             (vector-push-extend char (if (null in-parameter?)
                                          return-field
                                          parameter-string)))))))

;;; make this more robust since it is being used in other contexts
;;; like http.lisp
(defun header-line-field-name (line &optional
                                    (return-name (make-string-buffer 16)))
  (do ((maxlength (length line))
       (i 0 (1+& i)))
      ((>=& i maxlength) return-name)
    (let ((char (aref line i)))
      (cond ((char= char #\:) (return (values return-name i)))
            ; ((LWSP-char? char)) ;; to handle malformed header = token : blah
            (t (vector-push-extend char return-name))))))

;; look for either an empty line (according to RFC822, an empty line separates
;; header from body) or perhaps a "." which can occur in response to
;; a  "TOP <n> 0" command
;;
;; NOTE: flush TOP support if we decide to flush deferred message loading
;; should return a box, mime-type, mime-values (may be NIL) and bytes-read
(defun make-header-box-from-stream (stream &optional (read-to-terminator? nil))
  (let* ((header (boxer::make-uninitialized-box 'boxer::data-box))
         (visible-header-rows nil)
         (bytes-read 0)
         (closet-boxes (list header))
         ;; pre make these to reduce CONSing
         (field-name (make-string-buffer 16))
         (value-field (make-string-buffer 50))
         (mime-type :text/plain)
         (mime-values nil))
    (shared-initialize header t)
    (boxer::set-name header (boxer::make-name-row '("Header")))
    (loop (multiple-value-bind (line llength)
              (get-header-line stream) ; counts bytes and unfolds lines
            (cond ((or (null line)
                       (and (not read-to-terminator?) (string= line "")))
                   ;; add one more empty row to the header
                   (nconc visible-header-rows (list (boxer::make-row '())))
                   (return nil))
                  (t
                   (incf bytes-read llength)
                   ;; make sure the pre-made arrays are reset
                   (setf (fill-pointer field-name) 0 (fill-pointer value-field) 0)
                   (let ((header-field-name (header-line-field-name line
                                                                    field-name)))
                     (multiple-value-bind (header-row visible-row closet-box
                                                      mtype mvalues)
                         (header-field-dispatch header-field-name line value-field)
                       (unless (null visible-row)
                         (setq visible-header-rows
                               (nconc visible-header-rows (list visible-row))))
                       (unless (null closet-box) (push closet-box closet-boxes))
                       (unless (null mtype)   (setq mime-type mtype))
                       (unless (null mvalues) (setq mime-values
                                                    (nconc mime-values mvalues)))
                       (unless (null header-row)
                         (append-row header header-row))))))))
    (let ((return-header (make-box visible-header-rows)))
      (unless (null closet-boxes)
        (boxer::add-closet-row return-header (make-row closet-boxes)))
      (values return-header mime-type mime-values bytes-read))))

;; we could make this more data driven but right now, there just aren't
;; that many possibilities...
;; should return a row (to be added to the full header),
;; a visible-row (to be added to the displayed header, maybe nil), closet-box (maybe nil)
(defun header-field-dispatch (field-name line value-field)
  (let ((desired-field (car (member field-name *box-worthy-pop-msg-header-fields*
                                    :test #'string-equal)))
        (header-row (make-net-row line))
        (visible-row nil)
        (closet-box  nil)
        (mime-type   nil)
        (mime-values nil))
    (cond ((not (null desired-field))
           (setq visible-row (make-net-row line))
           (setq closet-box
                 (make-box (list (make-net-row
                                  (if (header-field-has-machine-part? desired-field)
                                    (header-line-machine-field line value-field)
                                    (header-line-value line value-field))))
                           'boxer::data-box
                           (copy-seq desired-field))))
          ((string-equal field-name "Content-Type")
           ;; parse for possible parameters...
           ;; usually boundary for multipart, possibly charset for text
           ;; applications/xxx can have somthing more specific
           (multiple-value-bind (type values)
                                (mime-header-line-value line value-field)
             (setq mime-type type)
             ;(setq header-row nil)
             (unless (null values) (setq mime-values values))))
          ;((string-equal field-name "MIME-Version")) ;treat as vanilla header row for now
          ((string-equal field-name "Content-Transfer-Encoding")
           (let ((encoding (mime-header-line-value line value-field)))
             ;(setq header-row nil)
             (setq mime-values (list :encoding encoding))))
          (t nil))
    (values header-row visible-row closet-box mime-type mime-values)))


;; NOTE: some mailers violate the spec by sending only a type
;; given a MIME type/subtype symbol,returns keyword MIME type and subtype values
(defun mime-type-values (mime-type)
  (let* ((typestring (symbol-name mime-type))
         (slashpos (position #\/ typestring)))
    (cond ((null slashpos)
           ;(error "Invalid MIME type/subtype pair ~A" mime-type)
           (values (intern typestring (find-package "KEYWORD"))
                   :default)
           )
          (t (values (intern (subseq typestring 0 slashpos)
                             (find-package "KEYWORD"))
                     (intern (subseq typestring (1+& slashpos))
                             (find-package "KEYWORD")))))))

;; this is the central MIME mail reading function.  It is
;; passed the pop-stream AFTER the header has been read out of it
;; use header information to handle MIME bodies
;; it may be called recursively for multipart messages, in which case
;; the optional boundary parameter is used when we enter append-pop-message body
;; recursively while constructing a part of a multipart message
;;
;; need to hack "application/mac-binhex40", application/applefile and
;; multipart/appledouble in addition to general mechanism for associated MIME
;; types with mac types (see mime-values->mac-file-values for how the
;; associations are created/modified
;;
;; First, specific type/subtype pairs are searched, then general types are
;; searched
;;
;; NOTE: a lot of the cases end up calling the same functions, they are separated
;;       out to help sketch out the likely set of near term possibilities
;;      SHould also change this to be more data driven (def-mime-handler)
(defun append-pop-message-body (message stream mime-type mime-values
                                        &optional (bytes-read 0) save-to-file?)
  ;; the "message" should already have the header info, and the stream should
  ;; be positioned to start making the body
  (multiple-value-bind (main-type subtype)
      (mime-type-values mime-type)
    ;; first, dispatch on main type, then look for specific subtype handlers
    (case main-type
      (:text
       (case subtype
         (:plain (get-mime-plain-text message stream bytes-read save-to-file?))
         (otherwise (get-mime-plain-text message stream
                                         bytes-read save-to-file?))))
      (:multipart
       (let* ((boundary (getf mime-values :boundary)))
         (if (null boundary)
             (get-mime-plain-text message stream
                                  bytes-read save-to-file?) ; escape clause
             (case subtype
               ((:mixed :digest)
                (get-mime-multipart message stream boundary bytes-read))
                (:appledouble
                 (get-mime-appledouble message stream boundary mime-values
                                       bytes-read))
                (:parallel
                 (get-mime-pmultipart message stream boundary bytes-read))
                (otherwise
                 (get-mime-multipart message stream boundary bytes-read))))))
      (:application
       (case subtype
         ((:boxer :prs.boxer :vnd.boxer)
          (get-mime-box message stream bytes-read (getf mime-values :link)))
         (:mac-binhex40
          (get-mime-binhex-box message stream bytes-read
                               (getf mime-values :name)))
         (:applefile
          (get-mime-applefile-box message stream bytes-read
                                  (getf mime-values :name)
                                  (getf mime-values :encoding)))
         (otherwise
          (get-mime-binary-box mime-type message stream bytes-read
                               (getf mime-values :name)
                               (getf mime-values :encoding)))))
      (:message
       (case subtype
         (:rfc822 ; handle this as plain text for now
          (get-mime-plain-text message stream bytes-read save-to-file?))
         (otherwise (get-mime-plain-text message stream bytes-read))))
;      ((:image :audio) ; binary types which should be saved and NOT shown
;       )
      (otherwise  ;same as for (:application/octet-stream)
       ;; offer to save in a file...
       ;; empty out for now...
       (if (eq (getf mime-values :encoding) :base64)
         (get-mime-binary-box mime-type message stream bytes-read
                              (getf mime-values :name))
         (get-mime-plain-text  message stream bytes-read save-to-file?)))))
  (when (null *mime-multipart-boundary-value*)
    (shrink message)) ; only shrink top level
  message)

;;;; utility functions for rendering particular MIME types

;; boxes, note that there may already be header stuff in MESSAGE
;; shouldn't have to worry about seeing a multipart boundary in here
;; NOTE: in-file? means we should save as a link
(defun get-mime-box (message stream &optional (bytes-read 0) in-file?)
  (if in-file?
    (get-mime-link-box message stream bytes-read)
    (with-base64-stream (in64 stream)
      (let* ((boxer::*file-system-verbosity* nil) ;no updates in base64 streams
             (box (boxer::load-binary-box-from-stream-internal in64)))
        ;; now we need to transfer the vitals (contents, closet, name, type(?),
        ;;  etc) of the return box into the message
        ;; very much like BOXER::INITIALIZE-BOX-FROM-BOX (loader.lisp) except we
        ;; need to merge with (possible) existing header info
        ;; name
        (let ((name (box::name-row box)))
          (unless (null name) (box::set-name message name)))
        ;; type
        (unless (or (eq (class-of message) (class-of box))
                    ;; toggling to and from sprite boxes is not handled yet.
                    ;; skip for now cause GET-MIME-BOX is called for side effect
                    (box::sprite-box? message) (box::sprite-box? box))
          (box::set-type message (class-name (class-of box))))
        ;; graphics
        (let ((gs (box::graphics-sheet box)))
          (unless (null gs) (setf (box::graphics-info message) gs)))
        ;; transparency
        (unless (null (box::exports box)) (boxer-eval::set-box-transparency message t))
        ;; flags
        (setf (slot-value message 'box::flags) (slot-value box 'box::flags))
        ;; contents & closet
        (box::move-box-internals box message)
        message))))

;; pull out the rest of a message,
(defun empty-out-mail-message (stream)
  (do ((line (net-mail-read-line stream) (net-mail-read-line stream)))
      ((null line))
       ))

;; boundary is the raw boundary stated in the content-type header.
;; need to look for leading "--" first.  Also, if we find a trailing, "--", then
;; return :END rather than just T
(defun mime-boundary-= (string boundary)
  (declare (string boundary))
  (let ((stop (length string)) (blength (length boundary)))
    (and (>& stop 2) (char= (char string 0) #\-) (char= (char string 1) #\-) ;leading "--"
         (do ((s 2 (1+& s)) (b 0 (1+& b)))
             ((>=& b blength)
              ;; made it to the end, now check for trailing "--" to decide what to return
              (if (and (>=& stop (+& blength 4))
                       (char= (char string s) #\-) (char= (char string (1+& s)) #\-))
                  :end
                  T))
           (cond ((>=& s stop) (return nil))
                 ((not (char= (char boundary b) (char string s))) (return nil)))))))

;; a stub, this should actually pick the best subpart and display only that...
(defun get-mime-pmultipart (message stream boundary &optional (bytes-read 0))
  (get-mime-multipart message stream boundary bytes-read))

(defun get-mime-multipart (message stream boundary &optional (bytes-read 0))
  (declare (ignore bytes-read))
  (let ((*mime-multipart-boundary-value* boundary)
        (*mime-multipart-boundary-encountered* nil))
    (loop (let* ((line (or *mime-multipart-boundary-encountered*
                           (net-mail-read-line stream)))
                 (boundary? (or (when (null line) :end)
                                *mime-multipart-boundary-encountered*
                                (mime-boundary-= line boundary))))
            (cond ((eq boundary? :end) ;; empty out the rest of the epilogue
                   ; 12/14/98 this is dangerous in recursive multiparts
                   ;(empty-out-mail-message stream)
                   (return))
                  (boundary? ;; we hit a boundary, call someone else to
                   ;; similiar to get-message-1 but we handle text/plain
                   ;; specially and of course, we don't need to issue & handle
                   ;; the POP commands
                   (setq *mime-multipart-boundary-encountered* nil)
                   (multiple-value-bind (header-box mime-type
                                                    mime-values header-size)
                       (make-header-box-from-stream stream)
                     ;; we grab the header first because it might tell us
                     ;; how we should process the rest of the message in
                     ;; particular, MIME information will appear in the header
                     (cond ((eq mime-type :text/plain)
                            ;; for plain text we append the rows to the
                            ;; existing box
                            (boxer::do-box-rows ((header-row header-box))
                              (append-row message header-row))
                            (append-pop-message-body message stream
                                                     mime-type mime-values
                                                     header-size))
                           (t ; for all other types, we want a box
                            (append-pop-message-body header-box stream
                                                     mime-type mime-values
                                                     header-size)
                            (append-row message (make-row (list header-box)))))))
                  (t ; we can only come here if there is some junk, so do nothing
                   nil))))))

(defvar *default-text-file-creator* :MSWD)

;; the generic text reading handler
(defun get-mime-plain-text (message stream &optional (bytes-read 0) in-file?)
  (if in-file?
    (let* ((pathname (unique-mime-path))
           (xref (boxer::make-xref :pathname pathname :mime-type :text))
           (box (make-box '(())))
           (creator *default-text-file-creator*)
           (ftype :text))
      (boxer::add-xref-closet-boxes box)
      (putprop box xref :xref)
      (boxer::set-xref-boxtop-info box creator ftype)
      (shrink box)
      (with-open-file (ostream pathname :direction :output)
        (loop
          (multiple-value-bind (line llength)
              (net-mail-read-line stream)
            (let ((boundary-value nil))
              (cond ((null line) (return nil))
                    ((and *mime-multipart-boundary-value*
                          (setq boundary-value
                                (mime-boundary-= line
                                                 *mime-multipart-boundary-value*)))
                     (setq *mime-multipart-boundary-encountered* boundary-value)
                     (return nil))
                    (t
                     (write-line line ostream)
                     (when *report-pop-message-reading-progress?*
                       (surf-message "   ~D bytes read"
                                     (incf bytes-read llength)))))))))
      #+mcl (ccl::set-mac-file-creator pathname creator)
      #+mcl (ccl::set-mac-file-type pathname ftype)
      (append-row message (make-row (list box))))
    (loop
      (multiple-value-bind (line llength)
                           (net-mail-read-line stream)
        (let ((boundary-value nil))
          (cond ((null line) (return nil))
                ((and *mime-multipart-boundary-value*
                      (setq boundary-value
                            (mime-boundary-= line *mime-multipart-boundary-value*)))
                 (setq *mime-multipart-boundary-encountered* boundary-value)
                 (return nil))
                (t
                 (append-row message (make-net-row line))
                 (when *report-pop-message-reading-progress?*
                   (surf-message "   ~D bytes read"
                                 (incf bytes-read llength))))))))))

(defun get-mime-binhex-box (message stream
                                    &optional (bytes-read 0) supplied-name)

  (declare (ignore bytes-read))
  (let* ((pathname (unique-mime-path supplied-name))
         (xref (boxer::make-xref :pathname pathname)))
    (boxer::add-xref-closet-boxes message)
    (putprop message xref :xref)
    (binhex-decode-stream stream pathname)
    (shrink message)
    #+mcl
    (let* ((creator (ccl::mac-file-creator pathname))
           (ftype (ccl::mac-file-type pathname)))
      (boxer::set-xref-boxtop-info message creator ftype)
      message)))

(defun mime-attachment-dir ()
  #+mcl (append (pathname-directory (ccl::mac-default-directory)) '("Mail Files"))
  #-mcl '())

(defun insure-attachment-dir ()
  (let ((adir (make-pathname :directory (mime-attachment-dir))))
    (when (null (probe-file adir))
      #+mcl (ccl::create-directory adir)
      #-mcl ())))

;; need to make sure that we end up with a valid filename for a particular OS
;; for example, MacOS only allows 31 letter filenames
(defun unique-mime-path (&optional supplied-name)
  (insure-attachment-dir)
  (if (null supplied-name)
      (loop (let* ((name (format nil "MIME~A" (mime-separator-value)))
                   (pathname (make-pathname :directory (mime-attachment-dir)
                                            :name name)))
              (when (null (probe-file pathname)) (return pathname))))
      (let* ((name (pathname-name supplied-name))
             (type (pathname-type supplied-name))
             (raw-pathname (make-pathname :directory (mime-attachment-dir)
                                          :name name :type type))
             ;; now make sure that the supplied path is OK
             (pathname (ensure-valid-filename raw-pathname)))
        (if (null (probe-file pathname)) pathname
            (do* ((i 0 (1+ i))
                  (newpath (newpath-for-mime name type i)
                           (newpath-for-mime name type i)))
                 (nil)
              (when (null (probe-file newpath)) (return newpath)))))))

(defvar *max-file-length* #+mcl 31 #-mcl 31)

;; should probably be moved somewhere more general (macros.lisp ?)
;; do nothing by default
#-mcl
(defun ensure-valid-filename (pathname &optional max-length)
  (declare (ignore max-length))
  pathname)

;; only check for length for now
#+mcl
(defun ensure-valid-filename (pathname &optional (max-length *max-file-length*))
  (let* ((name (pathname-name pathname)) (type (pathname-type pathname))
         (nlength (if (null name) 0 (length name)))
         (tlength (if (null type) 0 (length type)))
         (total (cond ((null name) tlength)
                      ((null type) nlength)
                      (t (+& nlength tlength 1)))))
    ;; note that either name or type can be NIL depending "." placement
    (cond ((> total max-length) ; bad, have to frob
           (cond ((null name)
                  (make-pathname :type (subseq type 0 (1- max-length))
                                 :defaults pathname))
                 ((null type)
                  (make-pathname :name (subseq name 0 (1- max-length))
                                 :defaults pathname))
                 (t
                  ;; prefer to trim name
                  (let ((trim-amount (- total max-length)))
                    (cond ((< trim-amount nlength)
                           (make-pathname :name (subseq name 0
                                                        (- nlength trim-amount))
                                          :type type
                                          :defaults pathname))
                          ((< trim-amount tlength)
                           (make-pathname :name name
                                          :type (subseq type 0
                                                        (- tlength trim-amount))
                                          :defaults pathname))
                          (t
                           ;; got to trim from both, prefer name since type will
                           ;; be getting messed up anyway
                           (make-pathname :name (subseq name 0 (1- max-length))
                                          :defaults pathname)))))))
          (t pathname))))

;; used when we've probed for, and discovered and existing file with the same name
(defun newpath-for-mime (name type vers &optional (dir (mime-attachment-dir)))
  (let ((rawpath (ensure-valid-filename
                  (make-pathname :directory dir :name name :type type)
                  (- *max-file-length*
                     ;; CONSes way less than (floor (log vers 10))
                     (length (format nil "~D" vers))))))
    (make-pathname :name (format nil "~A~D" (pathname-name rawpath) vers)
                   :defaults rawpath)))

;; generic file saving
(defun get-mime-binary-box (mime-type message stream bytes-read
                                      &optional supplied-name
                                      (binary-encoding :base64))
  (let* ((pathname (unique-mime-path supplied-name))
         (xref (boxer::make-xref :pathname pathname :mime-type mime-type)))
    (boxer::add-xref-closet-boxes message)
    (putprop message xref :xref)
    (multiple-value-bind (creator ftype)
        (mime-values->mac-file-values mime-type)
      (boxer::set-xref-boxtop-info message creator ftype)
      (shrink message)
      (cond ((eq binary-encoding :base64)
             (get-mime64-binary-data message stream bytes-read pathname))
            ((eq binary-encoding :uuencode)
             (get-mime-uu-binary-data message stream bytes-read pathname))
            (t
             (get-mime-8bit-binary-data message stream bytes-read pathname)))
      #+mcl (ccl::set-mac-file-creator pathname creator)
      #+mcl (ccl::set-mac-file-type pathname ftype)
      message)))

(defun get-mime-link-box (message stream bytes-read)
  (let* ((pathname (unique-mime-path)))
    (box::mark-box-as-file message pathname)
    (shrink message)
    (get-mime64-binary-data message stream bytes-read pathname)
    #+mcl (ccl::set-mac-file-creator pathname :boxr)
    #+mcl (ccl::set-mac-file-type pathname :boxr)
    (setf (slot-value message 'box::first-inferior-row) nil)
    message))

;; this basically just empties the text out, but
;; DOES check for a mime boundary to avoid hanging
(defun get-mime-uu-binary-data (message stream
                                        &optional (bytes-read 0)
                                        (pathname
                                         #+mcl (ccl::choose-new-file-dialog
                                                :prompt
                                                "save MIME data in file:")
                                         #+lispworks
                                         (capi:prompt-for-file
                                          "save MIME data in file:"
                                          :filter bw::*boxer-file-filters*
                                          :operation :save :if-does-not-exist :ok
                                          :owner bw::*boxer-frame*)
                                         #-(or lispworks mcl) pathname))
  (declare (ignore message))
  (with-open-file (ostream pathname :direction :output)
    (loop
      (multiple-value-bind (line llength)
          (net-mail-read-line stream)
        (let ((boundary-value nil))
          (cond ((null line) (return nil))
                ((and *mime-multipart-boundary-value*
                      (setq boundary-value
                            (mime-boundary-= line *mime-multipart-boundary-value*)))
                 (setq *mime-multipart-boundary-encountered* boundary-value)
                 (return nil))
                (t
                 (write-line line ostream)
                 (when *report-pop-message-reading-progress?*
                  (surf-message "   ~D bytes read"
                                (incf bytes-read llength))))))))))


;; If the message is in 8bit, then we just have to hack conversion from
;; char stream back to bytes
;; NOTE: need to hack EOF properly for multipart sections
(defun get-mime-8bit-binary-data (message stream
                                          &optional (bytes-read 0)
                                          (pathname
                                           #+mcl (ccl::choose-new-file-dialog
                                                  :prompt
                                                  "save MIME data in file:")
                                           #+lispworks
                                           (capi:prompt-for-file
                                            "save MIME data in file:"
                                            :filter bw::*boxer-file-filters*
                                            :operation :save :if-does-not-exist :ok
                                            :owner bw::*boxer-frame*)
                                           #-(or lispworks mcl) pathname))
  (declare (ignore message bytes-read))
  (let ((mime-boundary *mime-multipart-boundary-value*)
        (bindex 0))
    ;; these vars are bound cause they likely will be needed in a smarter
    ;; version of this function
    (declare (ignore mime-boundary bindex))
    (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede :if-does-not-exist :create)
      (let ((eof-value (list 'eof)))
        (loop (let ((b (read-char stream nil eof-value)))
                (cond ((eq b eof-value) (return nil))
                      (t (write-byte (char-code b) s)))))))))

(defun get-mime64-binary-data (message stream
                                       &optional (bytes-read 0)
                                       (pathname
                                        #+mcl (ccl::choose-new-file-dialog
                                               :prompt "save MIME data in file:")
                                        #+lispworks
                                        (capi:prompt-for-file
                                         "save MIME data in file:"
                                         :filter bw::*boxer-file-filters*
                                         :operation :save :if-does-not-exist :ok
                                         :owner bw::*boxer-frame*)
                                        #-(or lispworks mcl) pathname))
  (declare (ignore message bytes-read))
  (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                     #+mcl :fork #+mcl :data
                     :if-exists :overwrite :if-does-not-exist :create)
    (with-base64-stream (in64 stream)
      (loop (let ((b (read-byte in64 nil nil)))
              (if (null b) (return) (write-byte b s)))))))

;; obsolete, here to (incompletely) support (fill-box-from-url mail-message)
(defun make-pop-message (header stream &optional box)
  (let ((message (or box (boxer::make-uninitialized-box 'boxer::data-box)))
        (body (boxer::make-uninitialized-box 'boxer::data-box)))
    (shared-initialize message t) (shared-initialize body t)
    ;; first, empty out the rest of the stream to compose the body
    (do ((line (net-mail-read-line stream) (net-mail-read-line stream)))
        ((null line))
      (append-row body (make-net-row line)))
    ;; now make the message from the header and the body
    (append-row message (make-row (list header)))
    (shrink body)
    (append-row message (make-row (list body)))
    message))

;; this is called when the message is to large to be initially loaded,
;; if we are passed the header, use it, along with a black box with a
;; MAIL-MESSAGE-BODY URL to make up the message.  If the header is not
;; passed in, e.g. because the POP3 server does not support the TOP command,
;; then we have to make a black box with a MAIL-MESSAGE url
(defun make-delayed-pop-message (number &optional header)
  (let ((message (boxer::make-uninitialized-box 'boxer::data-box)))
    (shared-initialize message t)
    (cond ((null header) ;
           (putprop message (make-instance 'mail-message :message-number number)
                    :url)
           ;; now process the box (thick borders ?, file box?)
           (shrink message)
           ;(set-url-flags message) ; storage-chunk, RO flags
           message)
          (t (let ((body (boxer::make-uninitialized-box 'boxer::data-box)))
               (shared-initialize body t)
               (putprop body (make-instance 'mail-message-body
                                  :message-number number)
                        :url)
               ;; now process the box (thick borders ?, file box?)
               (shrink body)
               ;(set-url-flags body)
               ;; now make the message from the header and the body
               (append-row message (make-row (list header)))
               (append-row message (make-row (list body)))
               message)))))

;;; Primitives

(defboxer-primitive bu::get-mail ((boxer-eval::dont-copy mailbox) delete-messages?)
  (let ((*delete-loaded-messages?* (boxer-eval::true? delete-messages?))
        (mailbox (box-text-string mailbox)))
    (make-vc-using-url (make-instance 'pop-url
                         :scheme-string (concatenate 'string "//" mailbox)))))



(defboxer-primitive bu::delete-message ((bu::port-to message))
  message boxer-eval::*novalue*
  )

;; should this be bu::on-server?
(defboxer-primitive bu::mail-message? ((bu::port-to message))
  message boxer-eval::*novalue*
  )



;;;; Finger

(defvar *finger-port* 79)

(defun finger (host &optional (user ""))
  (let* ((*net-verbose* nil) ; suppress status mesages
         (stream (open-tcp-stream host *finger-port*)))
    (unwind-protect
        (progn
          (net-write-control-line stream user)
          ;; put the reply into a box...
          (make-vc-from-text-stream stream))
      (close stream))))

(defun make-vc-from-text-stream (stream)
  (let ((rows nil))
    (do ((line (net-read-line stream) (net-read-line stream)))
        ((null line) (make-vc (nreverse rows)))
      (push (boxer::make-evrow-from-string line) rows))))

(defboxer-primitive bu::finger (user)
  (let* ((raw-text (box-text-string (box-or-port-target user)))
         (@pos (position #\@ raw-text))
         (host (if @pos ;; if no "@", treat the entire name as a hostname
                   (subseq raw-text (1+ @pos))
                   raw-text))
         (user (if @pos (subseq raw-text 0 @pos) "")))
    (finger host user)))


 ;;; For Debugging.....

#|
;; problem mesages saved as text...

(defvar *mailstream* )

(setq *mailstream* (open
                    #+lispworks (capi::prompt-for-file "mail message:")
                    #+mcl       (ccl::choose-file-dialog)
                    #-(or lispworks mcl) (progn (format t "~&Enter a filename: ")
                                           (read-line))))

(let ((*net-mail-read-line-hook* 'net-mail-read-line-from-file))
     ;; or net-mail-read-line-from-mac-file
  (make-header-box-from-stream *mailstream*))

(let ((*net-mail-read-line-hook* 'net-mail-read-line-from-file))
  (append-pop-message-body * *mailstream* :multipart/mixed '(:boundary "Boundary_")))

(close *mailstream*)


|#

;;;;
;;;; FILE: mailfile.lisp
;;;;
;;;; --entire-file--

;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-


#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



   This file contains boxer mail file handling utilities




Modification History (most recent at top)

 7/23/05 added open-mail prim, also read-line-smart can handled lines
         in files separated by LF's (mac) as well as CR's
 7/22/05 added "From" to ">From" filtering in pop-append-message-internal
 5/01/05 mailfile-eom? changed to eliminate false positives
 7/29/04 added open-inbox, removed extra <CR> at the end of each message in
         pop-append-message-internal.
 7/20/04 started file


|#




(in-package :boxnet)


;;; mail file handling
;;; utilities for dealing with unix style mail files where messages are separated
;;; by <newline>From

;; eventually need utilities for synchronization and search

;; this should be a network preference...
(defvar *inbox-pathname* "~/inbox")

;; this needs to do "From" -> ">From" filtering....
(defmethod pop-fill-inbox ((url pop-url) &optional (mailfile *inbox-pathname*))
  (insure-pop-stream url)
  (unwind-protect
      (multiple-value-bind (nmsgs nbytes)
          (get-pop-status (slot-value url 'stream))
        (debugging-message "POP STAT ~A ~A" nmsgs nbytes)
        (surf-message "Mail Box: ~D messages, ~D bytes" nmsgs nbytes)
        (setf (slot-value url 'session-id) (cons nmsgs nbytes))
        (with-pop-server-queued-deletion (slot-value url 'stream)
          (with-open-file (mailstream mailfile :direction :output :if-exists :append
                                      :if-does-not-exist :create)
            (dotimes (i nmsgs)
              (surf-message "Downloading message ~D" (1+ i))
              (pop-append-message-internal (slot-value url 'stream)
                                           (1+ i) ;; POP3 message numbers are 1-based
                                           mailstream)))))
    (close-pop-stream (slot-value url 'stream))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
      (boxer::status-line-undisplay 'surf-message))))


;; appends messages from the network (a POP3 stream)  to the mailfile
(defun pop-append-message (pop-stream message-number mailfile)
  (with-open-file (ms mailfile :direction :output :if-exists :append)
    (pop-append-message-internal pop-stream message-number ms)))

;; >From filtering happens at this level
(defun pop-append-message-internal (stream message-number mailfile-stream)
  (net-write-control-line stream "RETR ~D" message-number)
  (handle-pop-response stream)
  (insert-mailfile-msg-separator mailfile-stream)
  (loop (let* ((line (net-mail-read-line-basic stream t nil))
               (fromline (search "From" line)))
          (cond ((null line) (return t))
                ((and fromline (zerop fromline))
                 (write-char #\> mailfile-stream)
                 (write-line line mailfile-stream))
                (t (write-line line mailfile-stream)))))
  ;; blank line at the end of each message...
  (terpri mailfile-stream)
  (when *delete-loaded-messages?*
    (queue-for-pop-server-deletion message-number)))


;; in a world of lots of time, these 2 could be extended to handle other
;; mailfile formats like Babyl files...
;; the basic format (see http://en.wikipedia.org/wiki/Mbox) is
;; From<space><other stuff>
;;  <message>
;;  <blank line>
;;
;; our parse just looks for "From " which will cover most mbox formats as well
;; as Eudora mail files where <other stuff> = "???@???"
;; and Mozilla based mail
;;
(defun insert-mailfile-msg-separator (stream)
  (format stream "From - ~A~&" (boxer::rfc822-date-time)))

(defun mailfile-eom? (rawline)
  (let ((place (search "From " rawline)))
    (and place (zerop place))))

;; adds the mail contents of a message box to the designated mail file
(defun append-message-box (msgbox mailfile)
  )

;;; for handling lines which can be delineated by CR's or LF's
(defmethod read-line-smart ((input-stream t) eof-error-p eof-value)
    "A faster way to do read-line-raw than that in streams.lisp. The speedup
     comes from avoiding with-output-to-string. Conses less too."
    (declare (optimize (speed 3) (safety 1)))
    (if (ccl::stream-eofp input-stream)
      (if eof-error-p
        ;; signal-eof-error is a macro defined in ccl:lib:streams.lisp
        (ccl::signal-eof-error input-stream)
        (values eof-value (or eof-value t)))
      (let ((char nil)
            (str (make-array 20
                             :element-type 'base-character
                             :adjustable t :fill-pointer 0)))
        (multiple-value-bind (reader reader-arg) (ccl::stream-reader input-stream)
          (ccl::while (and (setq char (funcall reader reader-arg))
                           (neq char #\newline)
                           (neq char #\linefeed))
            (when (and (not (ccl::base-character-p char))(ccl::base-string-p str))
              (setq str (ccl::string-to-extended-string str)))
            (vector-push-extend char str)))
        (values str (null char)))))

;; basically read-line + header line folding support (via append?=T)
;; and special end of message check via "From - <date>" search
(defun mailfile-read-line (stream wait? append?)
 (declare (ignore wait?))
  (unless append? (setf (fill-pointer *mail-line-buffer*) 0))
  (let ((rawline (read-line-smart stream nil nil)))
    (cond ((null rawline) nil)
          ((mailfile-eom? rawline) nil)
          (t (dotimes (i (length rawline))
               (vector-push-extend (char rawline i) *mail-line-buffer*))
             (values *mail-line-buffer* (length rawline))))))

(defun mail-file-start (stream)
  (do ((line (read-line-smart stream nil nil)  (read-line-smart stream nil nil)))
      ((null line) (error "Mail file start line not found"))
    (when (mailfile-eom? line) (return t))))

;; this is the :fill-call function
(defun fill-box-from-inbox (box &optional (inbox *inbox-pathname*))
  )

(defun read-mailbox-file (mailfile)
  (let ((*net-mail-read-line-hook* 'mailfile-read-line)
        (vc-rows nil))
    (with-open-file (stream mailfile)
      (mail-file-start stream)
      (loop (cond ((null (listen stream)) (return nil))
                  (t (push (boxer::make-evrow-from-entry
                            (get-mailfile-message stream))
                           vc-rows)))))
  (make-vc (nreverse vc-rows))))

;; this assumes that the stream will have been positioned just after
;; a line which consists of "From - <date>" which is the separator line for mail files
(defun get-mailfile-message (stream)
  (multiple-value-bind (header-box mime-type mime-values header-size)
      (make-header-box-from-stream stream)
    ;; we grab the header first because it might tell us
    ;; how we should process the rest of the message
    ;; in particular, MIME information will appear in the
    ;; header
    (prog1
      (append-pop-message-body header-box stream mime-type mime-values
                               header-size))))

;; The inverse, when we have better connections from/to other types of files
(defun save-mailbox-file (box)
  )



;;; Prims

(defboxer-primitive bu::new-get-mail ((eval::dont-copy mailbox) delete-messages?)
  (let ((*delete-loaded-messages?* (eval::true? delete-messages?))
        (mailbox (box-text-string mailbox)))
    ;; 1st, get mail into the inbox...
    (pop-fill-inbox (make-instance 'pop-url
                                   :scheme-string
                                   (concatenate 'string "//" mailbox)))
    ;; now get a box from the inbox
    (read-mailbox-file *inbox-pathname*)))

(defboxer-primitive bu::open-inbox ()
  (read-mailbox-file *inbox-pathname*))

(defboxer-primitive bu::open-mail (mailfile)
  (read-mailbox-file (box-text-string mailfile)))



;;; testing

#|

(let ((*delete-loaded-messages?* nil))
  (pop-fill-inbox (make-instance 'pop-url :scheme-string "//bug-boxer@earthlink.net")
                  (capi::prompt-for-file "Inbox file:"
                                         :if-exists :append
                                         :if-does-not-exist :create)))

|#



;;;;
;;;; FILE: makcpy.lisp
;;;;

;; From inside copy-graphics-sheet -> with-graphics-vars-bound-internal...
          #-opengl
          (drawing-on-bitmap (bm)
                             (with-pen-color ((or (graphics-sheet-background from-sheet)
                                                  *background-color*))
                               (draw-rectangle alu-seta %drawing-width %drawing-height 0 0))
                             (bitblt-to-screen alu-seta wid hei (graphics-sheet-bit-array from-sheet)
                                               0 0 0 0))
          #+opengl
          (copy-offscreen-bitmap alu-seta wid hei (graphics-sheet-bit-array from-sheet) 0 0 bm 0 0)


#|
;; Yuck, this is an example of how NOT to do this, it iterates over
;; the characters at least 4 (!) times
(defmethod text-string ((row row))
  (let* ((chas (chas row))
	(NO-OF-BOXES (COUNT-IF #'BOX? CHAS)))
    (IF (ZEROP NO-OF-BOXES)
	(let ((string (make-array (length chas)
				   :ELEMENT-TYPE
				   #-(or mcl symbolics) 'STRING-CHAR
				   #+(or mcl symbolics) 'character)))
	  (do ((i 0 (1+ i))
	       (chas chas (cdr chas)))
	      ((null chas))
	      (setf (aref string i)
		    #+(or symbolics mcl) (car chas)
		    #-(or symbolics mcl) (make-char (car chas))))
	  string)
	(let ((return-string (make-array (+& (length chas) no-of-boxes)
					 :element-type
					 #-(or mcl symbolics) 'string-char
					 #+(or mcl symbolics) 'character
					 :fill-pointer 0)))
	  (dolist (cha chas (values return-string t))
	    (cond ((cha? cha) (vector-push cha return-string))
		  (t
		   (vector-push #\[ return-string)
		   (vector-push #\] return-string))))))))

;; too much CONSing...
(defmethod text-string ((box box))
  (let ((return-string ""))
    (do-box-rows ((row box))
       (if (eq row (first-inferior-row box))
	   (setq return-string (text-string row))
	   (setq return-string (concatenate 'string
					    return-string
					    (make-string 1 :initial-element #\return)
					    (text-string row)))))
    return-string))

|#

#|  (originally from emanip.lisp ) this still needs to be converted....

(DEFUN MAKE-BOX-FROM-STRING (STRING)
  "make a box from a string.  carriage returns start new rows.  this is the inverse function
to the :TEXT-STRING method of boxes. "
  (MAKE-BOX
    (LOOP WITH START = 0
	  FOR INDEX FROM 0 TO (1- (LENGTH STRING))
	  FOR CHA = (AREF STRING INDEX)
	  WHEN (OR (CHAR= CHA #\CR) (CHAR= CHA #\LINE))
	    COLLECT (LIST (NSUBSTRING STRING START INDEX)) INTO ROWS
	  WHEN (OR (CHAR= CHA #\CR) (CHAR= CHA #\LINE))
	    DO (SETQ START (1+ INDEX))
	  FINALLY
	    (RETURN (APPEND ROWS (LIST (LIST (NSUBSTRING STRING START INDEX))))))))




;;;;BOX-EQUAL
(DEFUN BOX-EQUAL (BOX1 BOX2)
  (EQUAL BOX1 BOX2))

(DEFUN ROW-EQUAL (ROW1 ROW2)
  (EQUAL ROW1 ROW2))

;(DEFMETHOD (BOX :EQUL) (BOX)
;  (LET ((MY-LENGTH-IN-ROWS (LENGTH-IN-ROWS SELF))
;	(HE-LENGTH-IN-ROWS (LENGTH-IN-ROWS BOX)))
;    (COND ((NOT (= MY-LENGTH-IN-ROWS HE-LENGTH-IN-ROWS)) NIL)
;	  (T
;	   (DO* ((ROW-NO 0 (+ ROW-NO 1))
;		 (MY-ROW (ROW-AT-ROW-NO SELF ROW-NO) (ROW-AT-ROW-NO SELF ROW-NO))
;		 (HE-ROW (ROW-AT-ROW-NO BOX ROW-NO) (ROW-AT-ROW-NO BOX ROW-NO)))
;		((>= ROW-NO MY-LENGTH-IN-ROWS) T)
;	     (OR (EQUAL MY-ROW HE-ROW)
;		 (RETURN NIL)))))))
;
;(DEFMETHOD (ROW :EQUL) (ROW)
;  (LET ((MY-LENGTH-IN-CHAS (LENGTH-IN-CHAS SELF))
;	(HE-LENGTH-IN-CHAS (LENGTH-IN-CHAS ROW)))
;    (COND ((NOT (= MY-LENGTH-IN-CHAS HE-LENGTH-IN-CHAS)) NIL)
;	  (T
;	   (DO* ((CHA-NO 0 (+ CHA-NO 1))
;		 (MY-CHA (CHA-AT-CHA-NO SELF CHA-NO) (CHA-AT-CHA-NO SELF CHA-NO))
;		 (HE-CHA (CHA-AT-CHA-NO ROW CHA-NO) (CHA-AT-CHA-NO ROW CHA-NO)))
;		((>= CHA-NO MY-LENGTH-IN-CHAS) T)
;	     (COND ((AND (BOX? MY-CHA) (BOX? HE-CHA))
;		    (IF (NOT (EQUAL MY-CHA HE-CHA))
;			(RETURN NIL)))
;		   ((= (CHAR-CODE MY-CHA) (CHAR-CODE HE-CHA))
;		    T)
;		   (T (RETURN NIL))))))))

|#

;;;;
;;;; FILE: misc-prims.lisp
;;;;

(boxer-eval::defboxer-primitive bu::set-text-size ((bu::port-to box)
                                                   (boxer-eval::numberize width)
                                                   (boxer-eval::numberize height))
  (cond ((not (null *uc-copyright-free*))
          (boxer-eval::primitive-signal-error :copyright
                                              'bu::set-text-size
                                              " is no longer available, use "
                                              'bu::set-text-dimensions " instead"))
    (t
      (let ((realbox (box-or-port-target box)))
        (when (box? realbox)
          (let ((*current-font-descriptor* (closest-bfd
                                            (first-inferior-row realbox) 0)))
            (multiple-value-bind (font-cha-wid font-cha-hei)
                                (current-font-values)
                                (set-fixed-size realbox
                                                (* width font-cha-wid) (* height font-cha-hei))
                                ;; allow further mousing
                                (unless (bottom-right-hotspot-active? realbox)
                                  (set-bottom-right-hotspot-active? realbox t))
                                (modified realbox)))))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::set-port-text-size ((boxer-eval::dont-copy port)
                                                        (boxer-eval::numberize width)
                                                        (boxer-eval::numberize height))
  (cond ((not (null *uc-copyright-free*))
          (boxer-eval::primitive-signal-error :copyright
                                              'bu::set-port-text-size
                                              " is no longer available, use "
                                              'bu::set-port-text-dimensions
                                              " instead"))
    (t
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
                                (modified port)))))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::scroll-to-row ((bu::port-to box) (boxer-eval::numberize row))
  (cond ((not (null *uc-copyright-free*))
          (boxer-eval::primitive-signal-error :copyright
                                              'bu::scroll-to-row
                                              " is no longer available, use "
                                              'bu::set-scroll-row " instead"))
    (t
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
            :scrolling-error "You can only scroll editor boxes")))))))

;;;;
;;;; FILE: mouse.lisp
;;;;

;;; old
;(defsubst screen-boxes-in-row (screen-row)
;  (subset #'screen-box? (inferiors screen-row)))

#|
;;; **** this is now hacked in boxwin-mcl by modifying the coords
;;; **** of the mouse blip
;; make sure the returned value will be a row
;;; +++ I made these account for getting their coordinates in the window frame, parallel to
;;; mouse-position-values, above.  This is necessary to make tracking work right on the Mac
;;; version.  I have no idea what effect it will have on the Sun version. -- mt
#+mcl
(defun mouse-position-screen-row-values (global-x global-y)
  (multiple-value-bind (so local-offset position)
      (screen-obj-at (outermost-screen-box)
                     (- global-x (sheet-inside-left *boxer-pane*))
                     (- global-y (sheet-inside-top *boxer-pane*)))
    (cond ((screen-row? so)
       (values so local-offset))
      (t
       (let ((sr (screen-row so)))
         (if (not (screen-row? sr))
         (ecase position
           (:top (values (first-screen-row so) local-offset))
           (:bottom (values (last-screen-row so) local-offset)))
         (values sr local-offset)))))))

|#

;;; this is obsolete
(defmacro with-mouse-bp-bound ((x y window) &body body)
  (declare (ignore x y window body))
  (error "This macro is obsolete"))

;;; This shouldn't be consing up a BP every time ....
;(defmacro with-mouse-bp-bound ((x y window) &body body)
;  "This macro sets up an environment where MOUSE-BP is bound to a BP which
;   indicates where in the actual structure the mouse is pointing to.
;   MOUSE-SCREEN-BOX is also bound to the screen box which the mouse is
;   pointing to. "
;  `(let ((mouse-bp (make-bp ':fixed)))
;     (multiple-value-bind (mouse-row mouse-cha-no mouse-screen-box)
;	 (screen-obj-at-position ,x ,y ,window)
;     (unwind-protect
;       (progn
;	 (set-bp-row mouse-bp mouse-row)
;	 (set-bp-cha-no mouse-bp mouse-cha-no)
;	 (set-bp-screen-box mouse-bp mouse-screen-box)
;	 . ,body)
;       (when (not-null (bp-row mouse-bp))
;	 (delete-bp (bp-row mouse-bp) mouse-bp))))))



#|
;;;; RESIZE Support

(defconstant *resize-blinker-width* 1)

(defun draw-resize-blinker (x y wid hei)
  (draw-rectangle alu-xor *resize-blinker-width* hei x y)
  (draw-rectangle alu-xor
          (-& wid *resize-blinker-width*) *resize-blinker-width*
          (+& x *resize-blinker-width*) y)
  (draw-rectangle alu-xor
          *resize-blinker-width* (-& hei *resize-blinker-width*)
          (-& (+& x wid ) *resize-blinker-width*)
          (+& y *resize-blinker-width*))
  (draw-rectangle alu-xor
          (-& wid (*& 2 *resize-blinker-width*)) *resize-blinker-width*
          (+& x *resize-blinker-width*)
          (-& (+& y hei) *resize-blinker-width*)))

(defun resize-tracker (editor-box x y screen-box)
  (multiple-value-bind (box-window-x box-window-y)
      (xy-position screen-box)
    (multiple-value-bind (left top right bottom)
        (box-borders-widths (slot-value screen-box 'box-type) screen-box)
     (drawing-on-window (*boxer-pane*)
    ;; draw the resize icon on the box...
        (bitblt-to-screen alu-seta 7 7 *mouse-resize-bitmap* 0 0
              (-& (+& box-window-x (screen-obj-wid screen-box))
                  right)
              (-& (+& box-window-y (screen-obj-hei screen-box))
                  bottom))
    (let ((old-wid x) (old-hei y))
      ;; draw-the initial resize blinker
      (draw-resize-blinker box-window-x box-window-y old-wid old-hei)
    ;; now track the mouse
    (multiple-value-bind (final-x final-y)
        (with-mouse-tracking ((mouse-x box-window-x)
                  (mouse-y box-window-y))
          (let ((new-wid (-& mouse-x box-window-x))
            (new-hei (-& mouse-y box-window-y)))
        (unless (or (minusp new-wid) (minusp new-hei))
          ;; erase the previous resize blinker
          (draw-resize-blinker box-window-x box-window-y
                       old-wid old-hei)
          ;; and update the values and draw the new one
          (setq old-wid new-wid old-hei new-hei)
          (draw-resize-blinker box-window-x box-window-y
                       new-wid new-hei))))
      ;; erase the last hollow blinker
      (draw-resize-blinker box-window-x box-window-y old-wid old-hei)
      (unless (or (<& final-x box-window-x)
              (<& final-y box-window-y))
        (set-fixed-size editor-box
                (- final-x box-window-x left right)
                (- final-y box-window-y top bottom))))))))
  (modified editor-box)
  (box-first-bp-values editor-box))


(setf (get :resize-tab 'mouse-bp-values-handler) 'resize-tracker)

|#

;;;;
;;;; FILE: mousedoc.lisp
;;;;

  #+mcl "Click to Supershrink (Hold for more choices)"
  #+mcl "Click to Shrink (Hold for more choices)"
  #+mcl "Click to Expand (Hold for more choices)"
  #+mcl "Click to Expand to Fullscreen (Hold for more choices)"
  #+mcl "Flip to Graphics (Hold for more choices)"
  #+mcl "Flip to Text (Hold for more choices)"
  #+mcl "Hold for more choices"
  #+mcl "Hold for more choices"

  #-opengl
(defun mouse-doc-status-backing () (svref& *mouse-doc-status* 2))

#-opengl
(defun set-mouse-doc-status-backing (newback)
  (setf (svref& *mouse-doc-status* 2) newback))

           #-opengl
           (boxer::drawing-on-window (*boxer-pane*)
             (document-mouse-dispatch place screen-box T))

#-opengl
           (boxer::drawing-on-window (*boxer-pane*)
             (document-mouse-dispatch place screen-box))

  #+mcl
(defun popup-doc-delay ()
  (let ((original-event-id (event-id)))
    (or
     (process-wait-with-timeout "Mouse Documentation"
                                (round (* 60 *mouse-doc-wait-time*))
                                #'(lambda ()
                                    (not (= (event-id) original-event-id))))
     (boxer::drawing-on-window (*boxer-pane*)
       ;; why is this neccessarry ? shouldn't we already be in a drawing-on-window ?
       ;; perhaps the process switch messes the graphics state up ?
       (neq (mouse-place) (mouse-doc-status-place))))))

           #-opengl ; only need to "undraw" for non OpenGL
    (boxer::drawing-on-window (*boxer-pane*) (undocument-mouse-dispatch))

    #-opengl
        (when *change-mouse-on-hotspots*
          (set-mouse-cursor-internal *current-mouse-cursor*))

#-opengl
(defun undocument-mouse-dispatch ()
  (let* ((place (mouse-doc-status-place))
         (screen-box (mouse-doc-status-screen-box))
         (edbox (cond ((boxer::screen-box? screen-box)
                       (boxer::screen-obj-actual-obj screen-box))
                      ((boxer::sprite-box? screen-box)
                       screen-box)
                      (t nil)))
         (target (when edbox (boxer::box-or-port-target edbox))))
    (case place
      (:top-left     (boxer::popup-undoc-shrink
                      screen-box
                      (and (eq (boxer::display-style edbox) :shrunk)
                           (not (eq screen-box (outermost-screen-box))))))
      (:top-right    (boxer::popup-undoc-expand
                      screen-box (neq (boxer::display-style edbox) :shrunk)))
      (:bottom-left  (unless (null (slot-value target 'boxer::graphics-sheet))
                       (boxer::popup-undoc-view-flip
                        screen-box (not (boxer::graphics-screen-box? screen-box)))))
      (:bottom-right (if (boxer::bottom-right-hotspot-active? edbox)
                       (boxer::popup-undoc-resize screen-box)
                       (boxer::popup-undoc-resize screen-box t)))
      ((:type :port-target-type) (boxer::popup-undoc-toggle-type screen-box))
      ;; future expansion...
      (:name-handle)
      (:graphics (boxer::popup-undoc-graphics screen-box))
      (:sprite))))

;;;;
;;;; FILE: net-prims.lisp
;;;;
;;;; --entire-file--

;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|

 $Header: net-prims.lisp,v 1.0 90/01/24 22:15:07 boxer Exp $

 $Log:	net-prims.lisp,v $
;;;Revision 1.0  90/01/24  22:15:07  boxer
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



   This file contains all of the boxer functions which use the
   file system.


Modification History (most recent at top)

 4/21/03 merged current LW and MCL files, no diffs, updated copyright

|#

(in-package :boxer)

(boxer-eval::defboxer-primitive bu::send-box ((boxer-eval::dont-copy where)(bu::port-to box))
  (let ((hostname (box-text-string where))
	(box (get-port-target box)))
    (let ((guaranteed-editor-box (if (box? box) box (top-level-print-vc box))))
      (boxnet::with-open-port-stream (stream hostname)
	(dump-top-level-box-to-stream guaranteed-editor-box stream)))
    boxer-eval::*novalue*))

(defvar *net-boxes-to-be-inserted* nil)

;;; Two low-level implementations: polling and processes
;;; polling happens inside BOXER-COMMAND-LOOP.  If you have
;;; a brain-damaged implementation that won't let you do that,
;;; or if you feel it's really important to receive those sends
;;; while Boxer programs are running (even though you won't
;;; actually GET the sends), then use processes.
;;; Processes will probably go away, because no one can be
;;; that brain damaged and still get the sends, since we
;;; use the same mechanism.
;;;
;;; But we need the polling to happen more often at toplevel.
;;;
;;; Also, we need some error trapping around the receive thing.

;;; NIL means off.
;;; :POLL means polling
;;; :INTERRUPT means processes.
(defvar *boxer-send-server-status* nil)

;;; This function is called from setup-evaluator.  Make it
;;; do whatever you want.
(defun setup-boxer-send ()
  (enable-boxer-send-polling))

(boxer-eval::defboxer-primitive bu::enable-boxer-send-polling ()
  (enable-boxer-send-polling)
  boxer-eval::*novalue*)

(defun enable-boxer-send-polling ()
  (when (eq *boxer-send-server-status* :interrupt) (boxnet::close-server))
  (boxnet::setup-server)
  (boxnet::enable-polling 'net-interrupt-function)
  (setq *boxer-send-server-status* :poll))

(boxer-eval::defboxer-primitive bu::disable-boxer-send-polling ()
  (disable-boxer-send-polling)
  boxer-eval::*novalue*)

(defun disable-boxer-send-polling ()
  (boxnet::disable-polling)
  (boxnet::close-server)
  (setq *boxer-send-server-status* nil))

(boxer-eval::defboxer-primitive bu::enable-boxer-send-interrupts ()
  (enable-boxer-send-interrupts)
  boxer-eval::*novalue*)

(defun enable-boxer-send-interrupts ()
  (when (eq *boxer-send-server-status* :poll) (boxnet::disable-polling))
  (boxnet::setup-server)
  (boxnet::setup-connection-waiting-process 'net-interrupt-function)
  (setq *boxer-send-server-status* :interrupt))

(boxer-eval::defboxer-primitive bu::disable-boxer-send-interrupts ()
  (disable-boxer-send-interrupts)
  boxer-eval::*novalue*)

(defun disable-boxer-send-interrupts ()
  (boxnet::close-server)
  (setq *boxer-send-server-status* nil))

(boxer-eval::defboxer-primitive bu::receive-boxer-send ()
  (receive-boxer-send))

(defun receive-boxer-send ()
  (let ((box (pop *net-boxes-to-be-inserted*)))
    (when (null *net-boxes-to-be-inserted*) (status-line-undisplay 'handle-net-interrupt))
    (or box boxer-eval::*novalue*)))

;;;runs at interrupt time or at poll time, when we first read the message itself
;;; from the network.
(defun net-interrupt-function (stream)
  (let ((box (safe-load-binary-box-from-stream-internal stream)))
    (unless (null box)
      (push box *net-boxes-to-be-inserted*)
      (push 'handle-net-interrupt bw::*boxer-command-loop-event-handlers-queue*))))

;;; we don't want errors (like EOF) to trash the Boxer.  Just give up for now.
(defun safe-load-binary-box-from-stream-internal (stream)
  (let ((result (#+Lucid lcl::ignore-errors #-Lucid progn
			 (load-binary-box-from-stream-internal stream))))
    (cond ((null result)
	   ;; we would like to complain here but since we're running at an
	   ;; interrupt time we can't do a boxer-editor-error.
	   (restart-send-server)
	   nil)
	  (t result))))

;;;Runs inside BOXER-COMMAND-LOOP.
(defun handle-net-interrupt ()
  (beep) (beep)
  (status-line-display 'handle-net-interrupt
		       "*** Incoming Box Send --- Type META-R to Receive ***")
  (restart-send-server))

(defun restart-send-server ()
  (boxnet::setup-server)
  (when (eq *boxer-send-server-status* :interrupt)
    (boxnet::setup-connection-waiting-process 'net-interrupt-function)))


;;;;
;;;; FILE: new-borders.lisp
;;;;

;;;; used in various border GUI's & popup docs
;; these bitmaps are used to save *small* pieces of the screen before displaying
;; popup menus or other GUI items
;; we use an PDL allocation mechanism to prepare for a future which may include
;; recursive walking menus

(defvar *bitmap-backing-store* nil)

(defun allocate-backing-store (w h)
  (let ((existing (pop *bitmap-backing-store*)))
    (cond ((null existing) (make-offscreen-bitmap *boxer-pane* w h))
          ((or (< (offscreen-bitmap-width existing) w)
               (< (offscreen-bitmap-height existing) h))
           ;; existing bitmap is too small
           (free-offscreen-bitmap existing)
           ;; make a new one which is large enough. This should have the effect that
           ;; the one bitmap will grow big enough to handle all situations
           (make-offscreen-bitmap *boxer-pane* w h))
          (t existing))))

(defun deallocate-backing-store (bm)
  (push bm *bitmap-backing-store*))

;; stub...
(defun box-borders-zoom (box-type screen-box
          start-wid start-hei end-wid end-hei
          start-x start-y end-x end-y
          steps)
  (declare (ignore box-type screen-box start-wid start-hei end-wid end-hei
                   start-x start-y end-x end-y steps))
  )

#|
(defun scroll-up-tracking-info (screen-box)
  )

(defun scroll-down-tracking-info (screen-box)
  )

;; ???
(defun scroll-elevator-tracking-info (screen-box)
  )
|#

#|
;; old style
(defun draw-mouse-shrink-corner (x y)
  (with-pen-color (*mouse-doc-highlight-color*)
    (draw-poly alu-seta (list (cons (+ x -1) (+ y 3)) (cons (+ x 3) (+ y 3))
                              (cons (+ x 3)  (+ y -1))(cons (+ x -1) (+ y 3))))
    (draw-poly alu-seta (list (cons (+ x 4) (+ y -1)) (cons (+ x 4) (+ y 3))
                              (cons (+ x 7)  (+ y 3)) (cons (+ x 4) (+ y -1))))
    (draw-poly alu-seta (list (cons (+ x 8) (+ y 4)) (cons (+ x 4) (+ y 4))
                              (cons (+ x 4)  (+ y 8))(cons (+ x 8) (+ y 4))))
    (draw-poly alu-seta (list (cons (+ x -1) (+ y 4)) (cons (+ x 3) (+ y 4))
                              (cons (+ x 3)  (+ y 7))(cons (+ x -1) (+ y 4))))))
|#

#|
;; old style
(defun draw-mouse-expand-corner (x y)
  (let* ((last-pt (+& *border-inside-space* *basic-border-width*))
         (arrow-side (floor last-pt 2)))
    (with-pen-color (*mouse-doc-highlight-color*)
      ;; top left
      (draw-poly alu-seta (list (cons x y)   (cons x (+& y arrow-side 1))
                                (cons (+& x arrow-side 1) y) (cons x y)))
      ;; top right
      (draw-poly alu-seta (list (cons (-& (+& x last-pt) arrow-side 1) y)
                                (cons (+& x last-pt) y)
                                (cons (+& x last-pt) (+& y arrow-side))
                                (cons (-& (+& x last-pt) arrow-side 1) y)))
      ;; bottom right
      (draw-poly alu-seta (list (cons (+& x last-pt) (+& y arrow-side))
                                (cons (+& x last-pt) (+& y last-pt))
                                (cons (-& (+& x last-pt) arrow-side 1) (+& y last-pt))
                                (cons (+& x last-pt) (+& y arrow-side))))
      ;; bottom left
      (draw-poly alu-seta (list (cons (+& x arrow-side) (+& y last-pt))
                                (cons x (+& y last-pt))
                                (cons x (-& (+& y last-pt) arrow-side 1))
                                (cons (+& x arrow-side) (+& y last-pt)))))))
|#

;;;;
;;;; FILE: opengl-utils.lisp
;;;;

;; stub
(defun sheet-font-map (w) (declare (ignore w)) nil)

;; conversion from a native font to an OpenGL font
;; 1) get a LW font from a boxer font spec
;; 2) make an opengl font struct
;; 3) calculate height & width info for the font using the native font
;;    we could have opengl do it but then we have to convert formats & work
;;    in floating point
;;
;; 4) use win32::wgl-use-font to cache local font in GPU
;; *) font caching mechanism uses the DL-base-addr slot in opengl font struct
;; *) Note that the :start arg to wgl-use-font should be the same offset used
;;    when drawing chars, that's what *opengl-starting-char-index* is for

(defun register-opengl-font-from-native-font (native-font &optional (pane *boxer-pane*))
  (declare (ignore pane))
  (%make-opengl-font :native-font native-font))

(defun make-opengl-font-from-native-font (native-font &optional (pane *boxer-pane*))
  (let ((oglfont (register-opengl-font-from-native-font native-font)))
    (fill-oglfont-parameters oglfont pane)
    oglfont))

(defvar *use-capogi-fonts* t) ; want to allow option for scaleable vector OpenGL fonts in Windows

;;;;
;;;; FILE: oglscroll.lisp
;;;;
(defun draw-vertical-scroll-buttons (x y)
  (let ((left-x  (+ x *scroll-info-offset*))
        (mid-x   (+ x *scroll-info-offset* (/ *scroll-button-width* 2)))
        (right-x (+ x *scroll-info-offset* *scroll-button-width*))
        (top-button-bottom-y (+ y 1 *scroll-button-length*))
        (bottom-button-top-y (+ y 1 *scroll-button-length* 1)))
    (with-pen-color (*scroll-buttons-color*)
      ;; upper button
      (draw-poly alu-seta (list (cons mid-x (+ y 1))
                                (cons left-x top-button-bottom-y)
                                (cons right-x top-button-bottom-y)))
      ;; lower-button
      (draw-poly alu-seta (list (cons left-x bottom-button-top-y)
                                (cons right-x bottom-button-top-y)
                                (cons mid-x (+ y 1 *scroll-button-length* 1 *scroll-button-length*)))))))

(defun draw-horizontal-scroll-buttons (x y)
  (let ((top-y  (+ y *scroll-info-offset*))
        (mid-y   (+ y *scroll-info-offset* (round *scroll-button-width* 2)))
        (bottom-y (+ y *scroll-info-offset* *scroll-button-width*))
        (left-button-right-x (+ x 1 *scroll-button-length*))
        (right-button-left-x (+ x 1 *scroll-button-length* 1)))
    (with-pen-color (*scroll-buttons-color*)
      ;; upper button
      (draw-poly alu-seta (list (cons (+ x 1) mid-y)
                                (cons left-button-right-x top-y)
                                (cons left-button-right-x bottom-y)))
      ;; lower-button
      (draw-poly alu-seta (list (cons right-button-left-x top-y)
                                (cons right-button-left-x bottom-y)
                                (cons (+ x 1 *scroll-button-length* 1 *scroll-button-length*) mid-y))))))

#| ;; no longer used?
;;; These should expand into constants.  This also means that this
;;; file can only be compiled AFTER the appropriate borders file is loaded

(defmacro box-border-outside-space ()
  (cadr (assoc 'border-outside-space (get 'data-box 'box-borders-constants))))

(defmacro box-border-inside-space ()
  (cadr (assoc 'border-inside-space (get 'data-box 'box-borders-constants))))

(defmacro box-border-border-width ()
  (cadr (assoc 'border-width (get 'data-box 'box-borders-constants))))

(defmacro port-border-port-box-gap ()
  (cadr (assoc 'port-box-gap (get 'port-box 'box-borders-constants))))

(defvar *last-scrolled-box* nil)
(defvar *last-scrolled-dims* (make-array 4 :element-type 'fixnum
                                         :initial-element 0))

(defun fill-button-memory (x y wid hei
                             &optional (button-vector *last-scrolled-dims*))
  (setf (aref button-vector 0) x (aref button-vector 1) y
        (aref button-vector 2) wid (aref button-vector 3) hei))

(defun button-memory-match? (x y wid hei
                               &optional (button-vector *last-scrolled-dims*))
  (and (=& (aref button-vector 0) x) (=& (aref button-vector 1) y)
       (=& (aref button-vector 2) wid) (=& (aref button-vector 3) hei)))

|#

;; ;; don't need to show corner spots now that we have mouse tracking documentation
(defun dont-show-resize-hotspots? (screen-box)
  ; (or (eq *initial-box* (screen-obj-actual-obj screen-box)))
  (declare (ignore screen-box)) t)

;;;;
;;;; FILE: popup.lisp
;;;;

;; sgithens remove #-opengl
(defmethod menu-select ((menu popup-menu) x y)
  (multiple-value-bind (mwid mhei) (menu-size menu)
    (let* ((window-width (sheet-inside-width *boxer-pane*)); what about %clip-rig?
           (window-height (sheet-inside-height *boxer-pane*)) ; %clip-bot?
           (fit-x (-& (+& x mwid) window-width))
           (fit-y (-& (+& y mhei) window-height))
           (backing (make-offscreen-bitmap *boxer-pane* mwid mhei))
           ;; if either fit-? vars are positive, we will be off the screen
           (real-x (if (plusp& fit-x) (-& window-width mwid) x))
           (real-y (if (plusp& fit-y) (-& window-height mhei) y))
           ;; current-item is bound out here because we'll need it after the
           ;; tracking loop exits...
           (current-item nil))
      (unless (zerop& (mouse-button-state))
        ;; make sure the mouse is still down
        ;; if the original x and y aren't good, warp the mouse to the new location
        ;; a more fine tuned way is to use fit-x and fit-y to shift the current
        ;; mouse pos by the amount the menu is shifted (later...)
        (drawing-on-window (*boxer-pane*)
          (when (or *select-1st-item-on-popup* (plusp& fit-x) (plusp& fit-y))
            (warp-pointer *boxer-pane* (+& real-x 5) (+& real-y 5)))
          ;; grab the area into the backing store
          (bitblt-from-screen alu-seta mwid mhei backing real-x real-y 0 0)
          ;; now draw the menu and loop
          (unwind-protect
            (progn
              ;; draw menu
              (draw-menu menu real-x real-y)
              ;; loop
              (let ((current-y 0) (current-height 0))
                (with-mouse-tracking ((mouse-x real-x) (mouse-y real-y))
                  (let ((local-x (-& mouse-x real-x)) (local-y (-& mouse-y real-y)))
                    (if (and (<& 0 local-x mwid) (<& 0 local-y mhei))
                      ;; this means we are IN the popup
                      (multiple-value-bind (ti iy ih)
                                           (track-item menu local-y)
                        (cond ((and (null current-item)
                                    (not (slot-value ti 'enabled?)))
                               ;; no current, selected item is disabled...
                               )
                              ((null current-item)
                               ;; 1st time into the loop, set vars and then
                               (setq current-item ti current-y iy current-height ih)
                               ;; highlight
                               (draw-rectangle alu-xor (-& mwid 3) ih
                                               (1+& real-x) (+& real-y iy)))
                              ((eq ti current-item)) ; no change, do nothing
                              ((not (slot-value ti 'enabled?))
                               ;; new item is disabled but we have to erase...
                               (draw-rectangle alu-xor (-& mwid 3) current-height
                                               (1+& real-x) (+& real-y current-y))
                               (setq current-item nil))
                              (t ; must be a new item selected,
                               (draw-rectangle alu-xor (-& mwid 3) ih
                                               (1+& real-x) (+& real-y iy))
                               ;; erase old,
                               (draw-rectangle alu-xor (-& mwid 3) current-height
                                               (1+& real-x) (+& real-y current-y))
                               ;; set vars
                               (setq current-item ti current-y iy current-height ih))))
                      ;; we are OUT of the popup
                      (cond ((null current-item)) ; nothing already selected
                            (t ; erase old item
                             (draw-rectangle alu-xor (-& mwid 3) current-height
                                             (1+& real-x) (+& real-y current-y))
                             (setq current-item nil))))))
                ;; loop is done, either we are in and item or not
                ;; why do we have to do this ?
                #+carbon-compat
                (window-system-dependent-set-origin %origin-x-offset %origin-y-offset)
                (unless (null current-item)
                  ;; if we are in an item, flash and erase the highlighting
                  (dotimes (i 5)
                    (draw-rectangle alu-xor (-& mwid 3) current-height
                                    (1+& real-x) (+& real-y current-y))
                    (force-graphics-output)
                    (snooze .05)))))
            (bitblt-to-screen alu-seta mwid mhei backing 0 0 real-x real-y)
            (free-offscreen-bitmap backing)))
        ;; funcall the action (note we are OUTSIDE of the drawing-on-window
        (unless (null current-item)
          (let ((action (slot-value current-item 'action)))
            (unless (null action) (funcall action))))))))

;; use on non double-buffered window systems
#-opengl
(progn
;;; !!!! SHould use allocate-backing-store.....
;; called at the beginning and whenever the popup doc font is changed
(defun allocate-popup-backing ()
  (let ((wid 0)
        (padding (*& (+& *popup-doc-border-width* *popup-doc-padding*) 2)))
    (dolist (doc *popup-docs*)
      (setq wid (max wid (string-wid *popup-doc-font* (popup-doc-string doc)))))
    (let ((new-wid (+ padding wid))
          (new-hei (+ (string-hei *popup-doc-font*) padding)))
    (when (or (null *popup-doc-backing-store*)
              (not (= new-wid (offscreen-bitmap-width *popup-doc-backing-store*)))
              (not (= new-hei (offscreen-bitmap-height *popup-doc-backing-store*))))
      (unless (null *popup-doc-backing-store*)
        (free-offscreen-bitmap *popup-doc-backing-store*))
      (setq *popup-doc-backing-store*
            (make-offscreen-bitmap *boxer-pane* new-wid new-hei))))))

(def-redisplay-initialization (allocate-popup-backing))

)

#-opengl
(defmethod erase-doc ((self popup-doc) x y)
  (let ((total-padding (*& (+& *popup-doc-border-width* *popup-doc-padding*) 2)))
    (bitblt-to-screen alu-seta
                      (+& (string-wid *popup-doc-font* (slot-value self 'string))
                          total-padding)
                      (+& (string-hei *popup-doc-font*) total-padding)
                      *popup-doc-backing-store* 0 0 x y)
    (force-graphics-output)
    (setq *popup-doc-on?* nil)))

#-opengl (hotspot-back (allocate-backing-store *mouse-shrink-corner-bitmap*))
          #-opengl (hotwid (offscreen-bitmap-width  *mouse-shrink-corner-bitmap*))
          #-opengl (hothei (offscreen-bitmap-height *mouse-shrink-corner-bitmap*))

          handle the highlighting immediatement
          #-opengl
          (unless popup-only?
            (bw::set-mouse-doc-status-backing hotspot-back)
            (bitblt-from-screen alu-seta hotwid hothei hotspot-back
                                corner-x corner-y 0 0)
            ;; draw the hotspot
            (bitblt-to-screen alu-seta hotwid hothei *mouse-shrink-corner-bitmap*
                              0 0 corner-x corner-y))
          #+opengl

#-opengl
(defun popup-undoc-shrink (screen-box &optional supershrink?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (cond ((eq (screen-obj-actual-obj screen-box) *initial-box*)
                      :top-left-initial-box)
                     ((not (null  supershrink?))
                      :shrunken-top-left)
                     (t :top-left))))
        (backing (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-shrink-corner-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-shrink-corner-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (let ((corner-x (+& x delta-x))
              (corner-y (+& y delta-y (box-borders-cached-name-tab-height
                                       box-type screen-box))))
          (unless (or (null *popup-mouse-documentation?*)
                      (null *popup-doc-on?*))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (erase-doc doc doc-x doc-y)))
          ;; restore the hospot area
          (unless (null backing)
            (bitblt-to-screen alu-seta hotwid hothei backing 0 0 corner-x corner-y)
            (deallocate-backing-store *mouse-shrink-corner-bitmap* backing)))))))

#-opengl
(defun popup-undoc-expand (screen-box &optional fullscreen?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (cond ((eq screen-box (outermost-screen-box))
                     :top-right-outermost-box)
                    ((not (null fullscreen?))
                     :fullscreen)
                    (t :top-right))))
        (hotspot-back (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-expand-corner-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-expand-corner-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (declare (ignore delta-x))
        (multiple-value-bind (lef top rig bot)
            (box-borders-widths box-type screen-box)
          (declare (ignore lef top bot))
          (let ((corner-x (+& x (screen-obj-wid screen-box) (-& rig)))
                (corner-y (+& y delta-y (box-borders-cached-name-tab-height
                                         box-type screen-box))))
            (unless (or (null *popup-mouse-documentation?*)
                        (null *popup-doc-on?*))
              (multiple-value-bind (doc-x doc-y)
                  (popup-doc-offset-coords doc corner-x corner-y)
                (erase-doc doc doc-x doc-y)))
            (unless (null hotspot-back)
              (bitblt-to-screen alu-seta hotwid hothei hotspot-back
                                0 0 corner-x corner-y)
              (deallocate-backing-store *mouse-expand-corner-bitmap*
                                        hotspot-back))))))))

#-opengl
(defun popup-undoc-view-flip (screen-box &optional to-graphics?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (if to-graphics? :bottom-left-g :bottom-left-t)))
        (hotspot-back (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-toggle-view-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-toggle-view-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (declare (ignore delta-y))
        (multiple-value-bind (lef top rig bot)
            (box-borders-widths box-type screen-box)
          (declare (ignore lef top rig))
          (let ((corner-x (+& x delta-x))
                (corner-y (+& y (screen-obj-hei screen-box) (-& bot))))
            (unless (or (null *popup-mouse-documentation?*)
                        (null *popup-doc-on?*))
              (multiple-value-bind (doc-x doc-y)
                  (popup-doc-offset-coords doc corner-x corner-y)
                (erase-doc doc doc-x doc-y)))
            (unless (null hotspot-back)
              (bitblt-to-screen alu-seta hotwid hothei hotspot-back
                                0 0 corner-x corner-y)
              (deallocate-backing-store *mouse-toggle-view-bitmap*
                                        hotspot-back))))))))

#-opengl
(defun popup-undoc-resize (screen-box &optional is-off?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area (if is-off? :bottom-right-off :bottom-right)))
        (hotspot-back (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-resize-corner-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-resize-corner-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (lef top rig bot)
          (box-borders-widths box-type screen-box)
        (declare (ignore lef top))
        (let ((corner-x (+& x (screen-obj-wid screen-box) (-& rig)))
              (corner-y (+& y (screen-obj-hei screen-box) (-& bot))))
          (unless (or (null *popup-mouse-documentation?*)
                      (null *popup-doc-on?*))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (erase-doc doc doc-x doc-y)))
          (unless (null hotspot-back)
            (bitblt-to-screen alu-seta hotwid hothei hotspot-back
                              0 0 corner-x corner-y)
            (deallocate-backing-store *mouse-resize-corner-bitmap*
                                      hotspot-back)))))))

                                  ;; these are now all in the props dialog (mac)
                                  #-(or mcl lispworks)
                                  (make-instance 'menu-item
                                    :title "File"
                                    :action 'com-tt-toggle-storage-chunk)
                                  #-(or mcl lispworks)
                                  (make-instance 'menu-item
                                    :title "Read Only"
                                    :action 'com-tt-toggle-read-only)
                                  #-(or mcl lispworks)
                                  (make-instance 'menu-item
                                    :title "Autoload"
                                    :action 'com-tt-toggle-autoload-file)

;; frobs the items in the pop up to be relevant
;; more elegant to do this by specializing the menu-item-update method
;; on a tt-pop-up-menu-item class.  Wait until we hack the fonts to do this...
#-(or mcl lispworks)
(defun update-tt-menu (box)
  (let ((type-item  (car (menu-items *tt-popup*)))
        (store-item (find-menu-item *tt-popup* "File"))
        (read-item  (find-menu-item *tt-popup* "Read Only"))
        (autl-item  (find-menu-item *tt-popup* "Autoload")))
    (cond ((data-box? box)
           (set-menu-item-title type-item "Flip to Doit")
           (menu-item-enable type-item))
          ((doit-box? box)
           (set-menu-item-title type-item "Flip to Data")
           (menu-item-enable type-item))
          (t
           (set-menu-item-title type-item "Flip Box Type")
           (menu-item-disable type-item)))
    (cond ((storage-chunk? box)
           (set-menu-item-check-mark store-item t)
           ;; enable the other menu items in case they have been previously disabled
           (menu-item-enable read-item)
           (menu-item-enable autl-item))
          (t (set-menu-item-check-mark store-item nil)
             ;; disable remaining items because they only apply to storage-chunks
             (menu-item-disable read-item)
             (menu-item-disable autl-item)))
    ;; synchronize the remaining items, even if they are disabled, they
    ;; should still reflect the current values of the box
    (set-menu-item-check-mark read-item (read-only-box? box))
    (set-menu-item-check-mark autl-item (autoload-file? box))))


    ;; crock,
    #+carbon-compat
    (window-system-dependent-set-origin %origin-x-offset %origin-y-offset)

                ;; why do we have to do this ?
                #+carbon-compat
                (window-system-dependent-set-origin %origin-x-offset %origin-y-offset)

;;;;
;;;; FILE: prims.lisp
;;;;

(defrecursive-eval-primitive bu::any-of ((dont-copy box)
                                         (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null boxer::*uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::any-of " is no longer available, use "
                                       'bu::some " instead"))
        (t
         (cond ((not (null rest-of-line-must-be-empty))
                (signal-error
                 :any-of-bug
                 "ANY-OF statements must appear on lines by themselves."))
               ((or (numberp box) (not (fast-eval-data-box? box)))
                (signal-error :any-of-bug "expects a data box"))
               (t (let* ((code (interpreted-boxer-function-text
	                        (if (boxer::virtual-copy? box)
	                            (cached-code-virtual-copy box)
	                          (cached-code-editor-box box))))
                         (1stline (do ((line (car code) (car code)))
			              ((null code) nil)
			            (if (null line)
                                        (pop code)
                                      (return (pop code))))))
                    (if (null 1stline) *false*
                      (progn
                        (set-and-save-state-variables code)
                        (recursive-eval-invoke
                         (list* 'boxer-eval::any-of 1stline)))))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::any-of nextline)))))))

;; give helper function same name in eval package is a crock to make
;; the error message come out right
(defboxer-primitive boxer-eval::any-of ((dont-copy clause) (list-rest ignore))
  ignore ; bound but not used blah blah...
  (cond ((true? clause)
         (setq *boolean-clauses* nil) *true*)
        ((false? clause) *false*)
        (t (signal-error :any-of clause "neither true nor false"))))

(defrecursive-eval-primitive bu::all-of ((dont-copy box)
                                         (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null boxer::*uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::all-of " is no longer available, use "
                                       'bu::every " instead"))
        (t
         (cond ((not (null rest-of-line-must-be-empty))
                (signal-error
                 :all-of-bug
                 "ALL-OF statements must appear on lines by themselves."))
               ((or (numberp box) (not (fast-eval-data-box? box)))
                (signal-error :all-of-bug "expects a data box"))
               (t (let* ((code (interpreted-boxer-function-text
	                        (if (boxer::virtual-copy? box)
	                            (cached-code-virtual-copy box)
	                          (cached-code-editor-box box))))
                         (1stline (do ((line (car code) (car code)))
			              ((null code) nil)
			            (if (null line)
                                        (pop code)
                                      (return (pop code))))))
                    (if (null 1stline) *true*
                      (progn
                        (set-and-save-state-variables code)
                        (recursive-eval-invoke
                         (list* 'boxer-eval::all-of 1stline)))))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::all-of nextline)))))))

(defboxer-primitive boxer-eval::all-of ((dont-copy clause) (list-rest ignore))
  ignore
  (cond ((true? clause) *true*)
        ((false? clause) (setq *boolean-clauses* nil) *false*)
        (t (signal-error :any-of clause "neither true nor false"))))

;;;;
;;;; FILE: realprinter.lisp
;;;;

#|
;;; This is what we stick into rows for ports until we can retarget
;;; them.  The reason we need to do this is that inserting a port
;;; that doesn't have a legit target plays havoc with all the port caching
;;; code so we defer inserting ports untilthe very end when we have both
;;; a well defined hierarchy that we are inserting into and a legitimate
;;; port-target pair to insert.
;;;

;;; establish a default for these so we don't blow out
(defmethod insert-self-action ((self t) &optional superior)
  ;(declare (ignore self superior))
  nil)

(defmethod delete-self-action ((self t) &optional superior)
  ;(declare (ignore self superior))
  nil)
;; old stuff.  Using editor ports instead with the relevant info on the PLiST

;;; this ought to be just a vector but I'd like to be able to see
;;; what is what for now
(defstruct (port-retargetting-placeholder
	     (:conc-name prp-)
	     #+symbolics
	     (:print-function print-placeholder-internal))
  row
  target)

#+symbolics
(defun print-placeholder-internal (pl stream depth)
  (declare (ignore pl depth))
  (format stream ""))

|#

#|
;;; old stuff.  when vc's are targets, use real ports and record the VC
;; on the plist to be retargetted later

(defun make-port-from-vp (vp)
  ;; now reset the target
  (let ((target (vp-target vp)))
    (cond ((virtual-copy? target)
	   ;; this may have been an interior link so record it
	   ;; cause we may need to retarget it later
	   (let ((placeholder (make-port-retargetting-placeholder
				:target target)))
	     (record-port-printing placeholder)
	     placeholder))
	  ((box? target)
	   (let ((port (make-uninitialized-box 'port-box)))
	     ;; fixup some slots that the init method would have fixed
	     (setf (slot-value port 'display-style-list) (make-display-style))
	     (setf (slot-value port 'tick) (tick))
	     ;; ports to editor boxes can be made immediately
	     (set-port-to-box port target)
	     ;; record it so port caching will work later
	     (record-outlink-port port)
	     (unless (null (vp-name vp))
	       (set-name port (make-name-row (list (vp-name vp)))))
	     port))
	  (t
	   (error "Virtual Port target, ~S, is not a Box or Virtual Copy"
		  target)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Old stuff below                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(defun convert-vc-box-to-editor-box (vc)
  (let ((box (make-uninitialized-box (vc-type vc)))
	(last-row (convert-evrow-to-editor-row (car (vc-rows vc)) vc)))
    (initialize-from-defaults box)
    (insert-row-at-row-no box last-row 0)
    (dolist (evrow (cdr (vc-rows vc)))
      (let ((new-row (convert-evrow-to-editor-row evrow vc)))
	(insert-row-after-row box new-row last-row)
	(setq last-row new-row)))
    (when (vc-name vc)
      (set-name box (make-name-row `(,(vc-name vc)))))
    box))


(defun convert-evrow-to-editor-row (evrow superior-box)
  (make-row-no-spaces
    (mapcon #'(lambda (list)
		(convert-to-editor-object
		  (get-pointer-value (car list) superior-box)
		  (null (cdr list))))
	    (evrow-pointers evrow))))

(defun convert-to-editor-object (object lastp)
  (cond ((chunk-p object)
	 (let* ((left (chunk-left-format object))
		(pname (chunk-pname object))
		(right (chunk-right-format object))
		(chars (fi-chas left)))
	   (nconc (subseq chars
			  (fi-start left)
			  (fi-stop left))
		  (cond ((box? pname) (list (copy-box pname)))
			((virtual-copy? object)
                         (list (convert-vc-box-to-editor-box pname)))
			(t (subseq chars
				   (fi-start pname)
				   (fi-stop pname))))
		  (if lastp
		      (subseq chars
			      (fi-start right)
			      (fi-stop right))))))
	(t
	 (let ((result (cond ((or (numberp object)
                                  (symbolp object)
                                  (characterp object))
			      object)
			     ((virtual-copy? object)
			      (convert-vc-box-to-editor-box object))
			     ((box? object) (copy-box object))
			     (t (error "~S is not a chunk" object)))))
	   (if lastp (list result) (list result #\space))))))

;;; *** We need to rewrite convert-to-editor-object
(defun make-row-no-spaces (list)
  (let* ((new-row (make-initialized-row))
	 (ca (chas-array new-row))
	 (idx 0))
    (dolist (item list)
      (cond ((numberp item) (fast-string-into-chas-array
			     (convert-number-to-string item) ca))
	    ((stringp item) (fast-string-into-chas-array item ca))
	    ((symbolp item) (fast-string-into-chas-array (symbol-name item) ca))
	    ((characterp item) (fast-chas-array-append-cha ca item))
	    ((box? item)
	     (fast-chas-array-append-cha ca item)
	     (set-superior-row item new-row))
	    (t (error "Don't know how to make a row out of ~S" item)))
      (incf idx))
    new-row))

|#

             #+lcl3.0 (case (lcl::extreme-float-p number)
                        (:minus-infinity most-negative-long-float)
                        (:plus-infinity most-positive-long-float)
                        (t number))

;;;;
;;;; FILE: recursive-prims.lisp
;;;;

;; Some interesting pre-opengl bits from defrecursive-funcall-primitive update-shape:
(unless (null assoc-graphics-box)
               #-opengl
              (boxer::with-graphics-vars-bound (assoc-graphics-box gr-sheet)
                 ;; make sure all the other sprites are erased so we get a clean
                 ;; save-under image (i.e. no other sprite parts)
                 (let ((xor-sprites nil) (opaque-sprites nil))
                   (dolist (ttl (boxer::graphics-sheet-object-list gr-sheet))
                     (cond ((eq ttl turtle))
                           ((boxer::shown? ttl)
                            (if (eq (boxer::turtle-save-under ttl)
                                    'boxer::xor-redraw)
                              (push ttl xor-sprites)
                              (push ttl opaque-sprites)))))
                  (boxer::with-graphics-screen-parameters
                     (let ((boxer::*current-active-sprite* turtle))
                       (dolist (xs xor-sprites) (boxer::fast-erase xs)))
                    (boxer::erase turtle)
                     (let ((boxer::*current-active-sprite* turtle))
                       (dolist (os opaque-sprites) (boxer::fast-erase os)))))))

;; now we need to initialize the save under...
(unless (null assoc-graphics-box)
        #-opengl
        (boxer::with-graphics-vars-bound (assoc-graphics-box gr-sheet)
          (boxer::with-graphics-screen-parameters-once
            (unless (eq (boxer::turtle-save-under turtle)
                        'boxer::xor-redraw)
               (boxer::save-under-turtle turtle)))
                        ;; make sure all the other sprites are erased so we get a clean
              ;; save-under image (i.e. no other sprite parts)
              (let ((xor-sprites nil) (opaque-sprites nil))
                (dolist (ttl (boxer::graphics-sheet-object-list gr-sheet))
                  (cond ((eq ttl turtle))
                        ((boxer::shown? ttl)
                         (if (eq (boxer::turtle-save-under ttl)
                                 'boxer::xor-redraw)
                             (push ttl xor-sprites)
                             (push ttl opaque-sprites)))))
                (boxer::with-graphics-screen-parameters
                  (let ((boxer::*current-active-sprite* turtle))
                    (dolist (os opaque-sprites) (boxer::draw os)))
                 (when (boxer::absolute-shown? turtle) (boxer::draw turtle))
                  (let ((boxer::*current-active-sprite* turtle))
                    (dolist (xs xor-sprites) (boxer::draw xs)))))))

(unless (null assoc-graphics-box)
       ;; now we need to initialize the save under...
        #-opengl
       (boxer::with-graphics-vars-bound (assoc-graphics-box gr-sheet)
         (boxer::with-graphics-screen-parameters-once
             (unless (eq (boxer::turtle-save-under turtle) 'boxer::xor-redraw)
               (boxer::save-under-turtle turtle)))
          (let ((xor-sprites nil) (opaque-sprites nil))
            (dolist (ttl (boxer::graphics-sheet-object-list gr-sheet))
              (cond ((eq ttl turtle))
                    ((boxer::shown? ttl)
                     (if (eq (boxer::turtle-save-under ttl)
                             'boxer::xor-redraw)
                       (push ttl xor-sprites)
                       (push ttl opaque-sprites)))))
            (boxer::with-graphics-screen-parameters
              (let ((boxer::*current-active-sprite* turtle))
                (dolist (os opaque-sprites) (boxer::draw os)))
              (when (boxer::absolute-shown? turtle) (boxer::draw turtle))
              (let ((boxer::*current-active-sprite* turtle))
                (dolist (xs xor-sprites) (boxer::draw xs))))))
        )


;;; HOLDING-POSITION
(defrecursive-funcall-primitive bu::holding-position ((list-rest what))
  :STACK-FRAME-ALLOCATION (10 5 10 10)
  :STATE-VARIABLES (boxer::*current-sprite* boxer::%turtle-state)
  :BEFORE  (let* ((sprite (boxer::get-sprites))
                 (turtle (slot-value sprite 'boxer::graphics-info))) ;'boxer::associated-turtle)))
            (unless (boxer::sprite-box? sprite)
              (primitive-signal-error :not-a-sprite sprite))
            (set-and-save-state-variables
             sprite
             (boxer::return-state turtle))
            (recursive-funcall-invoke
             (make-interpreted-procedure-from-list (list what))))
  :AFTER
  (let* ((turtle (slot-value boxer::*current-sprite*
                            'boxer::graphics-info)) ;'boxer::associated-turtle))
        (assoc-graphics-box (slot-value turtle 'boxer::assoc-graphics-box)))
    (when (not (null boxer::*current-sprite*))
      (unless (null assoc-graphics-box)
       ;; now we need to initialize the save under...
       (boxer::with-graphics-vars-bound (assoc-graphics-box)
         (boxer::with-graphics-screen-parameters-once
             (unless (eq (boxer::turtle-save-under turtle)
                         'boxer::xor-redraw)
               (boxer::save-under-turtle turtle)))
         (boxer::with-graphics-screen-parameters
             (when (boxer::absolute-shown? turtle)
               (boxer::erase turtle)
               (boxer::restore-turtle-state turtle boxer::%turtle-state)
               (boxer::draw turtle))))))
    (restore-state-variables)
    nil)
  :UNWIND-PROTECT-FORM
  (let* ((turtle (slot-value boxer::*current-sprite*
                            'boxer::graphics-info)) ;'boxer::associated-turtle))
        (assoc-graphics-box (slot-value turtle 'boxer::assoc-graphics-box)))
    (when (not (null boxer::*current-sprite*))
      (unless (null assoc-graphics-box)
       ;; now we need to initialize the save under...
       (boxer::with-graphics-vars-bound (assoc-graphics-box)
         (boxer::with-graphics-screen-parameters-once
             (unless (eq (boxer::turtle-save-under turtle)
                        'boxer::xor-redraw)
               (boxer::save-under-turtle turtle)))
       (boxer::with-graphics-screen-parameters
           (when (boxer::absolute-shown? turtle)
             (boxer::erase turtle)
             (boxer::restore-turtle-state turtle boxer::%turtle-state)
             (boxer::draw turtle))))))
    (restore-state-variables)
    nil))

(defrecursive-funcall-primitive bu::with-sprites-hidden ((bu::port-to graphics-box)
                                                         (list-rest what))
  :stack-frame-allocation (10 5 10 10)
  :state-variables (boxer::*prepared-graphics-box* boxer::*sprites-hidden*)
  :before (let ((gs (boxer::graphics-sheet
                     (boxer::box-or-port-target graphics-box))))
            (if (null gs)
              (primitive-signal-error :with-sprites-hidden
                                      "No graphics in" graphics-box)
              (progn
                (set-and-save-state-variables
                 (boxer::box-or-port-target graphics-box)
                 (boxer::graphics-sheet-prepared-flag gs))
                (boxer::with-graphics-vars-bound ((boxer::box-or-port-target
                                                   graphics-box))
                  ;; make sure all sprites are erased so we get a clean
                  ;; save-under image (i.e. no other sprite parts)
                  (let ((xor-sprites nil) (opaque-sprites nil))
                    (dolist (ttl (boxer::graphics-sheet-object-list gs))
                      (when (boxer::shown? ttl)
                        (if (eq (boxer::turtle-save-under ttl) 'boxer::xor-redraw)
                          (push ttl xor-sprites)
                          (push ttl opaque-sprites))))
                   (boxer::with-graphics-screen-parameters
                      (dolist (xs xor-sprites) (boxer::fast-erase xs))
                      (dolist (os opaque-sprites) (boxer::fast-erase os)))))
                (setf (boxer::graphics-sheet-prepared-flag gs) t)
                (recursive-funcall-invoke
                 (make-interpreted-procedure-from-list (list what))))))
  :after (let ((gr-sheet (boxer::graphics-sheet boxer::*prepared-graphics-box*)))
           (boxer::with-graphics-vars-bound (boxer::*prepared-graphics-box*)
             ;; recalculate these lists because they may
             ;; have changed during the body
             (let ((xor-sprites nil) (opaque-sprites nil))
               (dolist (ttl (boxer::graphics-sheet-object-list gr-sheet))
                 (when (boxer::shown? ttl)
                   (if (eq (boxer::turtle-save-under ttl) 'boxer::xor-redraw)
                           (push ttl xor-sprites)
                           (push ttl opaque-sprites))))
                ;; update the save unders...
                (boxer::with-graphics-screen-parameters-once
                  (dolist (os opaque-sprites) (boxer::save-under-turtle os)))
               (boxer::with-graphics-screen-parameters
                  (dolist (os opaque-sprites) (boxer::draw os))
                  (dolist (xs xor-sprites) (boxer::draw xs)))))
           (setf (boxer::graphics-sheet-prepared-flag gr-sheet)
                 boxer::*sprites-hidden*)
           (restore-state-variables)
           nil)
  :unwind-protect-form
  (let ((gr-sheet (boxer::graphics-sheet boxer::*prepared-graphics-box*)))
    (when (boxer::graphics-sheet-prepared-flag gr-sheet)
      (boxer::with-graphics-vars-bound (boxer::*prepared-graphics-box*)
        ;; recalculate these lists because they may
        ;; have changed during the body
        (let ((xor-sprites nil) (opaque-sprites nil))
          (dolist (ttl (boxer::graphics-sheet-object-list gr-sheet))
            (when (boxer::shown? ttl)
              (if (eq (boxer::turtle-save-under ttl) 'boxer::xor-redraw)
                (push ttl xor-sprites)
                (push ttl opaque-sprites))))
          ;; update the save unders...
          (boxer::with-graphics-screen-parameters-once
            (dolist (os opaque-sprites) (boxer::save-under-turtle os)))
          (boxer::with-graphics-screen-parameters
            (dolist (os opaque-sprites) (boxer::draw os))
            (dolist (xs xor-sprites) (boxer::draw xs)))))
           (setf (boxer::graphics-sheet-prepared-flag gr-sheet)
                 boxer::*sprites-hidden*)
           (restore-state-variables)
           nil)))

;;;;
;;;; FILE: region.lisp
;;;;

;; sgithens TODO I don't believe this is ever called or referenced...
(defun interval-update-repaint-current-rows (region &optional
                                                      (window *boxer-pane*))
  (cond ((not (row-connected? (bp-row (interval-start-bp region))))
         ;; No BP's mean that there is not any screen structure.
         ;; Probably a region got wiped
         (dolist (blinker (interval-blinker-list region))
           (remove-region-row-blinker blinker))
         (setf (interval-blinker-list region) nil))
        (t
         (with-region-top-level-bps (region :start-bp-name region-start-bp
                                            :stop-bp-name region-stop-bp)
           ;; First we do "allocation" that is, make sure that there is a
           ;; blinker for every screen row and vice versa.  Note that
           ;; blinker list will be ordered from top to bottom
           (setf (interval-blinker-list region)
                 (update-row-blinker-list
                  (interval-blinker-list region)
                  (with-collection
                    (do-region-rows (row region)
                      (collect (current-screen-row row))))))
           (if (interval-visibility region)
               (make-interval-visible region)
               (make-interval-invisible region))
           (let ((starting-row (bp-row region-start-bp))
                 (starting-cha-no (bp-cha-no region-start-bp))
                 (stopping-row (bp-row region-stop-bp))
                 (stopping-cha-no (bp-cha-no region-stop-bp)))
             (dolist (blinker (interval-blinker-list region))
               (let* ((blinker-row (region-row-blinker-uid blinker))
                      (editor-row (screen-obj-actual-obj blinker-row)))
                 (cond ((and (eq starting-row editor-row)
                             (eq stopping-row editor-row))
                        ;; the row is both the first and last one in a
                        ;; region so we should trim both ends of it
                        (both-ends-blinker-trim blinker starting-cha-no
                                                stopping-cha-no))
                       ((eq starting-row editor-row)
                        ;; If the row is the first one in a region then it
                        ;; needs to be trimmed to correspond to where
                        ;; the BP is pointing
                        (left-half-blinker-trim blinker starting-cha-no))
                       ((eq stopping-row editor-row)
                        ;; If the row is the last one in the region, then
                        ;; it ALSO needs to be trimmed to correspond to
                        ;; where the BP is pointing
                        (right-half-blinker-trim blinker stopping-cha-no))
                       (t
                        ;; finally, take care of all the other rows
                        (update-region-row-blinker blinker)))))))))
  ;; @ this point all the blinkers are the correct size and inthe right place...
  (drawing-on-window (window)
    (dolist (blinker (interval-blinker-list region)) (draw-blinker blinker)))
  #-opengl
  (flush-port-buffer window))

;;; The fast track
#|

(defun update-tracking-blinkers (blinkers screen-box start-row stop-row
                      start-x stop-x context-x context-y)
  (let ((remaining-blinkers blinkers)
    (return-blinkers blinkers)
    (sr-x (+ context-x (screen-obj-x-offset screen-box)
                  (slot-value screen-box 'scroll-x-offset)))
    (sr-y (+ context-y (screen-obj-y-offset screen-box)
                  (slot-value screen-box 'scroll-y-offset))))
    (do-self-and-next-sv-contents (screen-row
                   (slot-value screen-box 'screen-rows)
                   start-row)
      (let ((blinker (or (car remaining-blinkers)
             ;; if we have run out, add them onto the
             ;; end of the return-blinkers list
             (let ((new (allocate-region-row-blinker screen-row)))
               (nconc return-blinkers (list new))
               new))))
    ;; next in line...
    ;; do this now because ww may RETURN from the next expression
    (setq remaining-blinkers (cdr remaining-blinkers))
    (let ((bl-y (+ sr-y (screen-obj-y-offset screen-row)))
          (bl-hei (screen-obj-hei screen-row)))
      ;; now handle specific case
      (cond ((and (eq screen-row start-row) (eq screen-row stop-row))
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row)
                        (min start-x stop-x))
                    bl-y
                    (abs (- stop-x start-x)) bl-hei)
         ;; pop out of the loop now that we have gotten to the last row
         (return))
        ((eq screen-row start-row)
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row)
                        start-x)
                    bl-y
                    (- (screen-obj-wid screen-row)
                        start-x)
                    bl-hei))
        ((eq screen-row stop-row)
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row))
                    bl-y stop-x bl-hei)
         (return))
        (t
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row))
                    bl-y
                    (screen-obj-wid screen-row) bl-hei))))))
    ;; if there are any extra blinkers in the list, turn them
    ;; off, we can stop when we have found one that is already turned off
    (dolist (bl remaining-blinkers)
      (cond ((null (region-row-blinker-visibility bl)) (return))
        (t (with-open-blinker (bl #+clx nil)
         (setf (region-row-blinker-visibility bl) nil
               (region-row-blinker-uid bl) nil)))))
    #+clx (bw::display-finish-output bw::*display*)
    return-blinkers))

(defun update-blinker-values (blinker uid x y wid hei)
  (cond ((and (eq uid (region-row-blinker-uid blinker))
          (not (null (region-row-blinker-visibility blinker)))
          (= y (region-row-blinker-y blinker))
          (= hei (region-row-blinker-hei blinker))
          (or (not (= x (region-row-blinker-x blinker)))
          (not (= wid (region-row-blinker-wid blinker)))))
     ;; check for and optimize the common case of moving
     ;; back and forth along the same row
     #+clx
     ;; this stuff really belongs in boxwin-xxx
     (progn
       ;; we know that either the wid or x or both have changed
       (unless (= (region-row-blinker-x blinker) x)
         (%draw-rectangle (abs (- (bw::blinker-x blinker) x))
                  (bw::blinker-height blinker)
                  (min x (bw::blinker-x blinker))
                  (bw::blinker-y blinker)
                  alu-xor (bw::blinker-window blinker)))
       (let ((new-right (+ x wid))
         (old-right (+ (bw::blinker-x blinker)
                (bw::blinker-width blinker))))
         (unless (= new-right old-right)
           (%draw-rectangle (abs (- new-right old-right))
                (bw::blinker-height blinker)
                (min& new-right old-right)
                (bw::blinker-y blinker)
                alu-xor (bw::blinker-window blinker))))
       (setf (region-row-blinker-x blinker) x
         (region-row-blinker-wid blinker) wid)
       )
     #-clx
     (with-open-blinker (blinker #+clx nil)
       (setf (region-row-blinker-x blinker) x
         (region-row-blinker-wid blinker) wid))
     )
     ((or (not (eq uid (region-row-blinker-uid blinker)))
          (null (region-row-blinker-visibility blinker))
          (not (=& x (region-row-blinker-x blinker)))
          (not (=& y (region-row-blinker-y blinker)))
          (not (=& wid (region-row-blinker-wid blinker)))
          (not (=& hei (region-row-blinker-hei blinker))))
      (with-open-blinker (blinker #+clx nil)
        (setf (region-row-blinker-visibility blinker) t
          (region-row-blinker-uid blinker) uid
          (region-row-blinker-x blinker) x
          (region-row-blinker-y blinker) y
          (region-row-blinker-wid blinker) wid
          (region-row-blinker-hei blinker) hei)))))
|#

;;;;
;;;; FILE: repaint.lisp
;;;;

(defun brand-new? (screen-obj) (=& (screen-obj-tick screen-obj) -1))

#|
;; unused ?
(defun force-repaint-window (&optional (window *boxer-pane*))
  (let ((*complete-redisplay-in-progress?* t))
    (repaint-window window)))
|#

;;; redisp refugees...
;; should try to remove the need for these when there's more time
;; make sure to check if the border opcode stuff is still needed

(defmethod set-force-redisplay-infs? ((self screen-obj) &rest ignore)
  (declare (ignore ignore))
  (setf (slot-value self 'force-redisplay-infs?) t)
  (set-needs-redisplay-pass-2? self t))

(defmethod set-needs-redisplay-pass-2? ((self screen-obj) new-value)
  (setf (slot-value self 'needs-redisplay-pass-2?)
        ;; try an preserve any existing border opcode
        (if (or (null new-value) (typep new-value 'fixnum))
          new-value
          (or (slot-value self 'needs-redisplay-pass-2?) new-value)))
  (when (not-null new-value)
    (let ((superior (superior self)))
      (when (screen-obj? superior)
        (set-needs-redisplay-pass-2? superior t)))))

(defmethod gray-self ((self screen-box))  *gray*)
(defmethod gray-self ((self graphics-screen-box)) *graphicsgray*)

;; sgithens 2022-01-15 from inside defmethod gray-body (self screen-box)
                             #-(or lwwin opengl)
                             (gray (if (or (and (storage-chunk? (slot-value self 'actual-obj))
                                                (null (slot-value (slot-value self 'actual-obj)
                                                                  'first-inferior-row)))
                                           (and (port-box? (slot-value self 'actual-obj))
                                                (null (ports (slot-value self 'actual-obj)))
                                                (boxnet::cross-file-port-branch-links
                                                 (slot-value self 'actual-obj))))
                                     *filegray*
                                     (gray-self self)))

;;;;
;;;; FILE: site.lisp
;;;;

(defvar *default-site-directory*
  #+unix "/usr/local/lib/boxer/"
  #+mcl  "home:")

(defvar *default-configuration-file-name* "config.text")

(defvar *site-initialization-handlers* nil)

(defmacro def-site-var (token-name value-type variable)
  (let ((handler-name (gensym)))
    `(progn
       (defun ,handler-name (value-string)
	 (let ((new-value (coerce-config-value value-string ,value-type)))
	   (unless (null *site-initialization-verbosity*)
	     (format t "~%Initializing Site Variable ~A to ~A"
		     ',variable new-value))
	   (setq ,variable new-value)))
       (let ((existing-entry (assoc ,token-name
				    *site-initialization-handlers*
				    :test #'string-equal)))
	 (if (null existing-entry)
	     (push (list ,token-name ',handler-name ',value-type)
		   *site-initialization-handlers*)
	     ;; just bash the slots in the existing entry
	     (setf (cadr existing-entry)  ',handler-name
		   (caddr existing-entry) ',value-type))))))

(defun handle-site-initializations (&optional
				    (site-file
				     (or #+lucid (system::environment-variable "SITE_DIRECTORY")
					 #+excl  (system::getenv "SITE_DIRECTORY")
					 *default-configuration-file-name*)))
  (let ((site-file (merge-pathnames site-file *default-site-directory*)))
    (if (null (probe-file site-file))
	(warn "~%The site file, ~A was not found.  ~%~
                 Site initializations will not be performed" site-file)
	(with-open-file (s site-file :direction :input)
	  (loop
	   (multiple-value-bind (valid? eof? keyword value)
	       (read-config-line s *keyword-buffer* *value-buffer*)
	     (cond (eof? (return))
		   (valid? (handle-site-initialization keyword value)))
	     (buffer-clear *keyword-buffer*)
	     (buffer-clear *value-buffer*)))))))

;;; this should take care to copy strings when appropriate since
;;; the args it is being passed are the keyword and value buffers
(defun handle-site-initialization (keyword value)
  (let ((handler-entry (assoc keyword *site-initialization-handlers*
			      :test #'string-equal)))
    (if (null handler-entry)
	(warn "~%No handler was found for the keyword: ~A" keyword)
	(funcall (cadr handler-entry) value))))

;;;;; Actual Site Inits

(def-site-var "Site-Initialization-Verbosity" :boolean
  *site-initialization-verbosity*)

(def-site-var "Postscript-Printer-Name" :string *ps-postscript-printer*)

(def-site-var "Postscript-Printer-Host" :string *ps-postscript-printer-host*)

(def-site-var "Postscript-Header-File" :string *ps-header-file*)

(def-site-var "Box-Server-Host" :string boxnet::*default-box-server-host*)

(def-site-var "Bug-Report-Address" :string *bug-report-address*)

(def-site-var "Local-Server-Directory"
    :string boxnet::*local-server-directory*)

;; also, look at the boxer preferences in sysprims.lisp

(def-site-var "Decimal-Print-Precision" :number *decimal-print-precision*)

(def-site-var "Print-Rationals" :boolean *print-rationals*)

(def-site-var "Evaluator-Helpful" :boolean *evaluator-helpful*)

(def-site-var "Default-Graphics-Box-Transparency" :boolean
  *default-graphics-box-transparency*)

(def-site-var "New-Sprites-Should-Be-Diet-Sprites?" :boolean
  *new-sprites-should-be-diet-sprites?*)

(def-site-var "Enable-Mouse-Toggle-Box-Type?" :boolean
  *enable-mouse-toggle-box-type?*)


;;;;
;;;; FILE: sysprims.lisp
;;;;

;;; use this after the site file has been edited
(boxer-eval::defboxer-primitive bu::reconfigure-system ()
                                (handle-site-initializations)
                                boxer-eval::*novalue*)


;;; should specify all available slots, punt for now
(defun empty-configuration-box () (make-box '(())))

(boxer-eval::defboxer-primitive bu::configuration-info ()
  (let* ((confile (merge-pathnames *default-configuration-file-name*
                                    *default-site-directory*))
          (conbox (if (probe-file confile)
                    (read-text-file-internal confile)
                    (empty-configuration-box))))
    (shrink conbox)
    (make-vc (list (list "Edit" "the" "following" "box:")
                    (list "Write-Text-File" conbox
                          (make-box `((,(namestring confile)))))
                    (list "You" "need" "to" "write" "out" "your" "changes"
                          "by" "evaluating" "the" "above" "line")
                    (list "and" "then" "evaluate" "the" "next" "line"
                          "to" "make" "the" "changes")
                    (list "Reconfigure-System")))))

;; Temporarily, or perhaps permanently removing this while fonts are being
;; reworked and simplified.
(defboxer-command com-show-font-info ()
  "Display font information"
  (reset-region)
  (reset-editor-numeric-arg)
  (insert-cha *point* (make-box (mapcar #'list (bw::capogi-fonts-info))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::show-font-info ()
  (virtual-copy (make-box (mapcar #'list (bw::capogi-fonts-info)))))

(boxer-eval:defboxer-primitive bu::toggle-fonts ()
  "A command for toggling between capi cfnt fonts and freetype fonts
   until we're done with the transition."
                               (if (member :freetype-fonts *features*)
                                 (setf *features* (remove :freetype-fonts *features*))
                                 (setf *features* (cons :freetype-fonts *features*)))
                               boxer-eval::*novalue*)

#+mcl
               (value ,(ecase value-type
                         (:boolean '(ccl::check-box-checked-p di))
                         (:number '(let ((ns (ccl::dialog-item-text di)))
                                     (when (numberstring? ns)
                                       (ignoring-number-read-errors
                                         (read-from-string ns nil nil)))))
                         (:string '(ccl::dialog-item-text di))
                         (:keyword '(ccl::dialog-item-text di))))

; from write-preferences
#+mcl
  (when (probe-file file)
    (ccl::set-mac-file-creator file :BOXR)
    (ccl::set-mac-file-type file :BOXP))

;; no longer needed, perhaps replace with interrupt poll count value
;#+carbon-compat
;(defboxer-preference bu::immediate-sprite-drawing (true-or-false)
;  ((*sprite-drawing-flush?* :boolean
;                            (boxer-eval::boxer-boolean *sprite-drawing-flush?*))
;   #+lwwin graphics #-lwwin graphics-settings
;   ("Should sprite graphics draw immediately with every command")
;   ("Setting this to false speeds up complicated sprite graphics"))
;  (cond ((not (null true-or-false))
;	 (setq *sprite-drawing-flush?* t))
;	(t
;	 (setq *sprite-drawing-flush?* nil)))
;  boxer-eval::*novalue*)

;; obsolete
;(defboxer-preference bu::enable-box-type-toggling-with-mouse (true-or-false)
;  ((*enable-mouse-toggle-box-type?* :boolean
;    (boxer-eval::boxer-boolean *enable-mouse-toggle-box-type?*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Should the mouse be able to toggle the box")
;   ("type by clicking on the type label ?"))
;  (setq *enable-mouse-toggle-box-type?* true-or-false)
;  boxer-eval::*novalue*)

;; removed 5/12/99 this is never encountered in practice now that we have menus
;; on the mouse corners
;(defboxer-preference bu::warn-about-disabled-commands (true-or-false)
;  ((*warn-about-disabled-commands* :boolean
;    (boxer-eval::boxer-boolean *warn-about-disabled-commands*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Should the user be informed when trying a disabled command.  ")
;   ("or should the action do nothing."))
;  (setq *warn-about-disabled-commands* true-or-false)
;  boxer-eval::*novalue*)

;; removed 5/24/01: no one is ever going to know what to do with this,
;; we should eventually replace it with an "animation speed" slider which also
;; controls things like speed of popup mouse doc unscrolling and move-to-bp
;(defboxer-preference bu::zoom-pause ((boxer-eval::numberize seconds))
;  ((*move-bp-zoom-pause-time* :number *move-bp-zoom-pause-time*)
;   #+lwwin editor #-lwwin editor-settings
;   ("How many seconds pause should there be between steps while")
;   ("zooming to a place (e.g., the target of a port)")
;   ("Setting to 0 will disable animation."))
;  (setq *move-bp-zoom-pause-time* seconds)
;  boxer-eval::*novalue*)

; removed 2/22/97
;(defboxer-preference bu::lock-all-closets (true-or-false)
;  ((*lock-all-closets* :boolean (boxer-eval::boxer-boolean *lock-all-closets*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Should all closets start out being locked"))
;  (setq *lock-all-closets* true-or-false)
;  (force-redisplay)
;  boxer-eval::*novalue*)

;; removed 9/14/98
;; used to be only-shrink-wrap-text-boxes
;(defboxer-preference bu::disable-box-resizing (true-or-false)
;  ((*only-shrink-wrap-text-boxes* :boolean (boxer-eval::boxer-boolean
;                                            *only-shrink-wrap-text-boxes*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Do not allow the size of text boxes to be fixed. If TRUE,")
;   ("boxes will automatically stretch to fit the contents"))
;  (setq *only-shrink-wrap-text-boxes* true-or-false)
;  boxer-eval::*novalue*)

;;turned off for now
;(defboxer-preference bu::slow-graphics-toggling (true-or-false)
;  ((*slow-graphics-toggle* :boolean (boxer-eval::boxer-boolean *slow-graphics-toggle*))
;   editor-settings
;   ("Require the user to confirm graphics toggling by holding the mouse")
;   ("button down for a small interval before the action will take place"))
;  (setq *slow-graphics-toggle* true-or-false)
;  boxer-eval::*novalue*)

;; removed 5/12/99 at andy's suggestion
;(defboxer-preference bu::only-scroll-current-box (true-or-false)
;  ((*only-scroll-current-box?* :boolean (boxer-eval::boxer-boolean *only-scroll-current-box?*))
;   editor-settings
;   ("Limit the ability to scroll the contents to the current box"))
;  (setq *only-scroll-current-box?* true-or-false)
;  (force-redisplay)
;  boxer-eval::*novalue*)

#| ;; removed 9/08/02
(defboxer-preference bu::fullscreen-window (true-or-false)
  ((bw::*fullscreen-window-p* :boolean
                              (boxer-eval::boxer-boolean bw::*fullscreen-window-p*))
   #+lwwin editor #-lwwin editor-settings
   ("Should the boxer window occupy the entire screen ?"))
  (setq bw::*fullscreen-window-p* true-or-false)
  boxer-eval::*novalue*)
|#

;; removed 5/12/99 at andy's suggestion
;(defboxer-preference bu::enable-egc (true-or-false)
;   ((*egc-enabled?* :boolean (boxer-eval::boxer-boolean *egc-enabled?*))
;    #+lwwin evaluator #-lwwin evaluator-settings
;    ("Should the Ephemeral Garbage Collector be turned on ?"))
;  (let ((new-value true-or-false))
;    (setq *egc-enabled?* new-value)
;    #+ccl (ccl::egc new-value)
;    #+lucid (if new-value (lcl::egc-on) (lcl::egc-off))
;    )
;  boxer-eval::*novalue*)

;; what about ALL bitmap ops...

;; removed 5/12/99 at andy's suggestion
;#+mcl
;(defboxer-preference bu::use-fast-bitmap-loaders (true-or-false)
;  ((*use-mac-fast-bitmap-loaders* :boolean
;                                  (boxer-eval::boxer-boolean
;                                   *use-mac-fast-bitmap-loaders*))
;   File-System-Settings
;   ("Use the experimental (may crash your machine) fast bitmap operations ?"))
;  (setq *use-mac-fast-bitmap-loaders* true-or-false)
;  boxer-eval::*novalue*)

;; not currently use
#|
(defboxer-preference bu::max-viewable-message-size ((boxer-eval::numberize length))
  ((boxnet::*max-viewable-message-length*
    :number boxnet::*max-viewable-message-length*)
   #+lwwin network #-lwwin network-settings
   ("What is the maximum size mail message that will appear in Boxer ?"))
  (setq boxnet::*max-viewable-message-length* length)
  boxer-eval::*novalue*)
|#

;;; temporary
#|
#+lwwin
(defboxer-preference bu::draw-icon-options ((boxer-eval::numberize new-option))
                     ((*windows-draw-icon-options* :number
                                                   *windows-draw-icon-options*)
                      #+lwwin temporary
                      ("Try different draw-icon display routines")
                      ("Should be an interger between 0 and 15"))
  (when (and (integerp new-option) (<=& 0 new-option 15))
    (setq  *windows-draw-icon-options* new-option))
  boxer-eval::*novalue*)
|#

;;
;; Stepper and Evaluator Prefs
;;

;; sgithens 2022-03-25 Removing these steppers preferences for now, since the stepper is currently
;;                     out of commission.
(defboxer-preference bu::step-wait-for-key-press (true-or-false)
  ((boxer-eval::*step-wait-for-key-press* :boolean
                                          (boxer-eval::boxer-boolean boxer-eval::*step-wait-for-key-press*))
   #+capi evaluator #-capi evaluator-settings
   ("Should the Stepper wait for a key press ")
   ("before going on to the next step ?")
   ("(The Stepper shows Boxer execution one step at a time.)"))
  (setq boxer-eval::*step-wait-for-key-press* true-or-false)
  boxer-eval::*novalue*)

(defboxer-preference bu::step-time ((boxer-eval::numberize seconds))
  ((boxer-eval::*step-sleep-time* :number boxer-eval::*step-sleep-time*)
   #+capi evaluator #-capi evaluator-settings
   ("How many seconds should the Stepper pause between steps")
   ("(The Stepper shows Boxer execution one step at a time.)"))
  (setq boxer-eval::*step-sleep-time* seconds)
  boxer-eval::*novalue*)

;; sgithens 2022-03-25 Removing this for now, since optimally we should just always been repainting. There may be some
;;                     use case for this variable in the future.
(defboxer-preference bu::update-display-during-eval (true-or-false)
  ((*repaint-during-eval?* :keyword
                           (boxer-eval::boxer-boolean boxer-eval::*warn-about-primitive-shadowing*))
   #+capi evaluator #-capi evaluator-settings
   ("Should the screen be repainted during eval ? Valid entries are ALWAYS, NEVER and CHANGED-GRAPHICS"))
  (setq *repaint-during-eval?* true-or-false)
  boxer-eval::*novalue*)

;;
;; Editor Preferences
;;

;; sgithens 2021-03-28 Removing this for now as we are consolidating keyboards for all 3 platforms. This may or may not
;;                     be useful again in the future.
;;
(defboxer-preference bu::input-device-names (machine-type)
  ((*current-input-device-platform* :keyword
                                    (make-box
                                     `((,*current-input-device-platform*))))
   #+capi editor #-capi editor-settings
   ("Which set of names should be used to refer to ")
   ("special (control) keys or mouse actions ?")
   ("(Different platforms may use different names.)"))
  (let ((canonicalized-name (intern (string-upcase machine-type)
                                    (find-package 'keyword))))
    (if (fast-memq canonicalized-name *defined-input-device-platforms*)
      (make-input-devices canonicalized-name)
      (boxer-eval::primitive-signal-error :preference
                                          "The machine-type, " machine-type
                                          ", does not have a defined set of input devices"))
    boxer-eval::*novalue*))

;;
;; Network Preferences
;;

;;; Network stuff

;; sgithens 2021-03-08 Removing these network email preferences as email support is currently broken
;;                     and we aren't sure whether we will include this functionality going forward.
;;
(defboxer-preference bu::user-mail-address (address)
  ((boxnet::*user-mail-address* :string
                                (make-box `((,boxnet::*user-mail-address*))))
   #+capi network #-capi network-settings
   ("What Internet address should identify you in various network dealings ?"))
  (let* ((newname address)
         (@pos (position #\@ newname)))
    ;; need some sort of consistency checking on the name here
    (if (null @pos)
      (boxer-eval::primitive-signal-error :preferences-error
                                          newname
                                          " Does not look like a valid address")
      (let ((user (subseq newname 0 @pos)) (host (subseq newname (1+ @pos))))
        (setq boxnet::*user-mail-address* newname
              boxnet::*pop-user* user
              boxnet::*pop-host* host)))
    boxer-eval::*novalue*))

(defboxer-preference bu::mail-relay-host (host)
  ((boxnet::*smtp-relay-host* :string (make-box `((,boxnet::*smtp-relay-host*))))
   #+capi network #-capi network-settings
   ("What computer should be responsible for ")
   ("relaying mail to the Internet ?"))
  (let ((newname host))
    ;; need some sort of consistency checking on the name here
    (setq boxnet::*smtp-relay-host* newname)
    boxer-eval::*novalue*))

;; ;; should have a hook to access the MIME type dialog

(defboxer-preference bu::query-for-unkown-mime-type (true-or-false)
  ((boxnet::*query-for-unknown-mime-type* :boolean
                                          (boxer-eval::boxer-boolean boxnet::*query-for-unknown-mime-type*))
   #+capi network #-capi network-settings
   ("Should a dialog popup if an unknown")
   ("MIME (mail attachment) type is encountered ?"))
  (setq boxnet::*query-for-unknown-mime-type* true-or-false)
  boxer-eval::*novalue*)

(defboxer-preference bu::mail-inbox-file (filename)
  ((boxnet::*inbox-pathname* :string (make-box `((,boxnet::*inbox-pathname*))))
   #+capi network #-capi network-settings
   ("Which File should new mail be placed in"))
  (let ((newpath filename))
    ;; should reality check here (at least directory should exist)
    (setq boxnet::*inbox-pathname* newpath)
    boxer-eval::*novalue*))


;;;; (Postscript) Printer Preferences (mostly unix based)

#+(and unix (not macosx))
(defboxer-preference bu::printer-name (printer-name)
  ((*ps-postscript-printer* :string (make-box `((,*ps-postscript-printer*))))
   #+capi printer #-capi printer-settings
   ("The name of the printer used for")
   ("Postscript output"))
  (let ((newname  printer-name))
    ;; need some sort of consistency checking on the name here
    (setq *ps-postscript-printer* newname)
    boxer-eval::*novalue*))

#+(and unix (not macosx))
(defboxer-preference bu::printer-host (machine-name)
  ((*ps-postscript-printer-host* :String (make-box `((,*ps-postscript-printer-host*))))
   #+capi printer #-capi printer-settings
   ("The name of the machine attached to the")
   ("printer used for Postscript output"))
  (let ((newname machine-name))
    ;; need some sort of consistency checking on the name here
    (setq *ps-postscript-printer-host* newname)
    boxer-eval::*novalue*))

#+(and unix (not macosx))
(defboxer-preference bu::printer-filename (filename)
  ((*ps-file* :string (make-box `((,*ps-file*))))
   #+capi printer #-capi printer-settings
   ("The name of the file used by Com-Print-Screen-To-File")
   ("for Postscript output"))
  (let ((newname filename))
    ;; need some sort of consistency checking on the name here
    (setq *ps-file* newname)
    boxer-eval::*novalue*))

;;;; Serial Line Preferences

#+(and unix (not macosx))
(defboxer-preference bu::newline-after-serial-writes (true-or-false)
  ((*add-newline-to-serial-writes* :boolean
                                   (boxer-eval::boxer-boolean *add-newline-to-serial-writes*))
   #+capi communication #-capi communication-settings
   ("Should extra Carriage Returns be added")
   ("at the end of each Serial-Write ? "))
  (setq *add-newline-to-serial-writes* true-or-false)
  boxer-eval::*novalue*)

#+(and unix (not macosx))
(defboxer-preference bu::serial-read-base ((boxer-eval::numberize radix))
  ((*serial-read-base* :number *serial-read-base*)
   #+capi communication #-capi communication-settings
   ("The radix that the serial line will")
   ("use to read in n (possible) numbers"))
  (setq *serial-read-base* radix)
  boxer-eval::*novalue*)

;; More unused editor prefs

;; This should be changed to :choice after the :choice pref is implemented
#+(and (not opengl) capi) ; dont offer until it works...
(defboxer-preference bu::popup-mouse-documentation (true-or-false)
  ((*popup-mouse-documentation?* :boolean
                                 (boxer-eval::boxer-boolean
                                  *popup-mouse-documentation?*))
   #+capi editor #-capi editor-settings
   ("Should mouse documentation popup after a short delay ?"))
  (setq *popup-mouse-documentation* true-or-false)
  boxer-eval::*novalue*)

;;;;
;;;; FILE: turtle.lisp
;;;;

(defmethod scale-save-under ((self turtle) scale)
  (let ((su (slot-value self 'save-under)))
    (unless (or (null su) (eq su 'xor-redraw))
      (let ((new-size (ceiling (* (save-under-size su) scale))))
	(setf (save-under-size su) new-size
	      (save-under-middle su) (round new-size 2))
        (free-offscreen-bitmap (save-under-bitmap su))
	(setf (save-under-bitmap su)
	      (make-offscreen-bitmap *boxer-pane*
				     new-size new-size))))))

;;;;
;;;; FILE: vars.lisp
;;;;
;; used in with-sprites-hidden
(define-eval-var boxer::*prepared-graphics-box* :global nil)
(define-eval-var boxer::*sprites-hidden* :global nil)

#|
;;; FOR-EACH-ITEM
(define-eval-var *for-each-item-variable-object* :global nil)
(define-eval-var *for-each-item-counter* :global nil)
(define-eval-var *for-each-item-length* :global nil)
(define-eval-var *for-each-item-box* :global nil)
(define-eval-var *for-each-item-list* :global nil)

;;; FOR-EACH-BOX
(define-eval-var *for-each-box-variable-object* :global nil)
(define-eval-var *for-each-box-counter* :global nil)
(define-eval-var *for-each-box-length* :global nil)
(define-eval-var *for-each-box-box* :global nil)
(define-eval-var *for-each-box-list* :global nil)

;;; FOR-EACH-ROW
(define-eval-var *for-each-row-variable-object* :global nil)
(define-eval-var *for-each-row-counter* :global nil)
(define-eval-var *for-each-row-length* :global nil)
(define-eval-var *for-each-row-box* :global nil)
(define-eval-var *for-each-row-list* :global nil)
|#

;;;;
;;;; FILE: virtcopy.lisp
;;;;

#|see the file eval/ev-int.lisp for the real defs
(DEFUN EVAL-BOX? (THING)
  ;; this is to avoid multiple calls to typep
  (MEMBER (TYPE-OF THING) '(VIRTUAL-COPY VIRTUAL-PORT PORT-BOX DATA-BOX DOIT-BOX)))	;etc

(DEFUN EVAL-DOIT? (THING)
  (AND (VIRTUAL-COPY? THING) (EQ (VC-TYPE THING) ':DOIT-BOX)))

(DEFUN EVAL-DATA? (THING)
  (AND (VIRTUAL-COPY? THING) (EQ (VC-TYPE THING) ':DATA-BOX)))

(DEFSUBST EVAL-PORT? (THING)
  (VIRTUAL-PORT? THING))

(DEFSUBST EVAL-NAMED? (THING)
  (NOT (NULL (BOX-NAME THING))))

|#

#|
(defun access-evrow-element (vc element
				&optional
				(need-exact-match nil) (chunk-too? nil))
  (multiple-value-bind (item exact?)
       (get-pointer-value element vc)
     (let ((answer (if chunk-too? item (access-pointer-element item))))
       (if (and need-exact-match (or (not exact?)
				     (eq exact? ':default)
				     (and (eq exact? 'single)
					  ;;; aaacckkk!!!!
					  (not (=& (length vc) 9))
					  (not
					   (vc-rows-entry-single-is-exact?
					    vc)))))
	   (let ((new-answer (virtual-copy answer :top-level? t)))
	     (append-value-to-pointer element vc
				      (vcis-creation-time vc) new-answer)
	     new-answer)
	   answer))))
|#

#|
;; the old non-generic versions
(defun get-port-target (thing)
  (if (fast-port-box? thing)
      (vp-target thing)
      (error "~S is not an Evaluator Port" thing)))

(defun box-or-port-target (thing)
  (if (and (vectorp thing) (fast-port-box? thing))
      (get-port-target thing)
      thing))
|#

#|
(defun port-to-item (ptr superior &optional keep-chunk)
  (multiple-value-bind (item exact?)
      (get-pointer-value ptr superior)
    (let* ((chunk-p (fast-chunk? item))
	   (value (if chunk-p (chunk-chunk item) item)))
      (flet ((new-chunk (new-value)
	       (if chunk-p
		   (let ((nc (copy-chunk item)))
		     (setf (chunk-chunk nc) new-value)
		     nc)
		   new-value)))
	(cond ((or (numberp value) (symbolp value))
	       item)
	      ((or (fast-eval-port-box? value)
		   (port-box? value))
	       (if keep-chunk
		   (new-chunk (virtual-copy value))
		   (virtual-copy value)))
	      ((not (null exact?))
	       (if keep-chunk
		   (new-chunk (port-to value))
		   (port-to value)))
	      ((and (box? superior)
		    (box? value))
	       (if keep-chunk
		   (new-chunk (port-to value))
		   (port-to value)))
	      (t
	       ;; need to cons a unique target
	       (let* ((target (virtual-copy value))
		      (nc (new-chunk target)))
		 (extend-pointer ptr superior (now)
				 (if chunk-p nc target))
		 (if keep-chunk
		     (new-chunk (port-to target))
		     (port-to target)))))))))
|#

#| ;; too much CONSing !!!
(defun evrow-text-string (row superior)
  (let ((entries (evrow-pointers row)))
    (flet ((chunk-string (chunk &optional last?)
	     (if (null last?)
		 (concatenate 'string
			      (formatting-info-string (chunk-left-format
						       chunk))
			      (if (formatting-info? (chunk-pname chunk))
				  (formatting-info-string (chunk-pname chunk))
				  "[]"))
		 (concatenate 'string
			      (formatting-info-string (chunk-left-format
						       chunk))
			      (if (formatting-info? (chunk-pname chunk))
				  (formatting-info-string (chunk-pname chunk))
				  "[]")
			      (formatting-info-string
			       (chunk-right-format chunk)))))
	   (value-string (value space?)
	     (cond ((or (symbolp value) (stringp value) (numberp value))
		    (format nil (if space? " ~A" "~A") value))
		   (space? " []")
		   (t "[]"))))
      (cond ((not (null entries))
	     (let ((return-string (make-string 0)))
	       (dolist (p entries return-string)
		 (let ((chunk (get-pointer-value p superior)))
		   (setq return-string
			 (concatenate
			  'string
			  return-string
			  (cond ((chunk-p chunk)
				 (chunk-string chunk
					       (eq p (car (last entries)))))
				((and (not (zerop& (length return-string)))
				      (not (char= (char return-string
							(1-& (length
							      return-string)))
						  #\space)))
				 ;; add a space if there isn't already
				 ;; one between items
				 (value-string chunk t))
				(t (value-string chunk nil)))))))))
	    ((null (evrow-row-format row))
	     "")
	    (t (let ((format-chunk (evrow-row-format row))
		     (return-string (make-string 0)))
		 (when (formatting-info? (chunk-left-format format-chunk))
		   (setq return-string
			 (concatenate 'string
				      return-string
				      (formatting-info-string
				       (chunk-left-format format-chunk)))))
		 (when (formatting-info? (chunk-pname format-chunk))
		   (setq return-string
			 (concatenate 'string
				      return-string
				      (formatting-info-string
				       (chunk-pname format-chunk)))))
		 (when (formatting-info? (chunk-right-format format-chunk))
		   (setq return-string
			 (concatenate 'string
				      return-string
				      (formatting-info-string
				       (chunk-right-format format-chunk)))))
		 return-string))))))

(defun box-text-string (box)
  (cond ((numberp box) (format nil "~A" box))
	(t
	 (let ((return-string (make-string 0)))
	   (dolist (r (get-box-rows box) (string-right-trim '(#\newline)
							    return-string))
	     (setq return-string (concatenate 'string
					      return-string
					      (evrow-text-string r box)
					      (make-string 1
							   :initial-element
							   #\newline))))))))

|#

#|
;seems to be unused as of 4/28/92

;;;; How to Make new Evrows
;; EVROW-CONSTRUCTOR is the established way of making new EVROWS used by the
;; data manipulators. If we are making an EVROW for use in a constructor e.g.
;; JOIN-RIGHT then All MP's must be disambiguated since the new row will NOT
;; be in the a box which is ONLY a direct ancestor of the original row because
;; multiple rows will be joined in some fashion which means we won't be able
;; to just extend the pedigree due to the mutiple parents

;; the thing arg should either be a evrow entry or a list of entries
;; pointer should be disambiguted by this point
;; Note that we pay a cost for CONSing new Single pointers

(defun evrow-constructor (thing box)
  (flet ((port-item-operator (item)
	   (let ((val (get-pointer-value item box)))
	     (if (box? val)	   ; should we be porting to numbers here ?
		 (port-to-item item box)
		 item)))
	 (copy-item-operator (item)
	   ;; we may have to copy here
	   item)
	 (thing-discriminator (item-op)
	   (cond ((evrow? thing)
		  (make-evrow :pointers (mapcar item-op (evrow-pointers thing))
			      :row-format (evrow-row-format thing)))
		 ((listp thing)
		  (make-evrow-from-entries (mapcar item-op thing)))
		 ((pointer? thing)
		  (make-evrow-from-pointer  (funcall item-op thing)))
		 (t (error "~S was not an EVROW, LIST or POINTER. " thing)))))
    (etypecase box
      (virtual-copy (thing-discriminator #'copy-item-operator))
      (virtual-port (thing-discriminator #'port-item-operator)))))

|#

;;;;
;;;; FILE: vrtdef.lisp
;;;;

;; sgithens This doesn't seem to be used anywhere... what do we currently look at to determine if eval is in progress???
(DEFVAR *EVAL-IN-PROGRESS* NIL
  "Bound by top level eval functions and used by the :MODIFIED message to
   decide when to flush old eval structure. ")

(DEFVAR *FUNNY-FUNCTION-ARGLIST-TABLE* (MAKE-HASH-TABLE))

(DEFVAR *EVALUATOR-COPYING-ON?* T
  "A Flag which controls the automatic copying of objects in the evaluator.  ")

(DEFVAR *EVALUATOR-COPYING-FUNCTION* 'SHALLOW-COPY-FOR-EVALUATOR)

(DEFVAR *MULTIPLE-ROW-TOP-LEVEL-UNBOX-ACTION* :FLATTEN
  "What happens when we unbox a box with multiple rows at top level. Valid
   values are :ERROR (signal an error), :TRUNCATE (use only the top row)
   and :FLATTEN (use each row sequentially). ")

(DEFVAR *TRIM-EMPTY-ROWS?* T
  "Should empty rows be removed from a box BEFORE it is returned ?")
