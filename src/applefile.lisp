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

