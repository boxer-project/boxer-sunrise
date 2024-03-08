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
;;;;   - bfsforeign.lisp and bfslocal.lisp contains the unused remnants of a Boxer File System
;;;;     that had usernames, passwords, and various types of transactional locking for apparently
;;;;     sharing box files. This was all done on a filesystem directory.
;;;;     The last bits of the boxer server and boxer file system are in archived
;;;;     client.lisp and clientmacros.lisp
;;;;   - surf.lisp mailto-url and make-message-box show the email urls and how a new box was
;;;;     created for composing a new email message.
;;;;   - grmeth.lisp flash-name seems like a useful design idea... flashed a turtles name or label?
;;;;   - gdispl.lisp (defun ,recording-function ,args... shows how we could optimize the
;;;;     record-boxer-graphics-command-xyz functions by looking backwards and not recording it if it's
;;;;     the previous item
;;;;   - oglscroll.lisp  Some old versions of mouse-in-h-scroll-bar-internal (and v) that handled the mouse
;;;;     scrolling with a scroll bar and also used maybe-move-point-after-scrolling to move to make the
;;;;     cursor visible if it had been scrolled offscreen
;;;;   - gdispl.lisp check-existing-graphics-state-entries peephole optimizer support
;;;;     would look back through a graphics command list and not add the entry (or maybe just state change)
;;;;     if it matched the previous one
;;;;   - boxwin-opengl.lisp There is a stub for a thing called a warp pointer that would have pulled the mouse
;;;;     cursor back to the side of the box if you moused outside of it.

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
;;;; FILE: bfsforeign.lisp
;;;;
;;;; --entire-file--

#|

  An implementation of the Box File System which uses local
  files.  That is, all box oriented commands will be built
  out of vanilla common lisp functions which manipulate files.

  This file contains support for access and
  modification of other server worlds.


Modification History (most recent at top)

 2/16/03 merged current LW and MCL files, no diffs, copyright updated

|#

(in-package :boxnet)




;; returns 2 values, a server type symbol (useable by make-instance) and
;; a world name string to be passed to that server

;; use colon to delimit server type
(defvar *server-world-delimiter-cha* #\:)
(defun parse-foreign-world-name (namestring)
  (let ((colpos (position *server-world-delimiter-cha* namestring)))
    (cond ((null colpos)
	   (values (default-foreign-server (get-server)) namestring))
	  (t
	   (values (case (intern (string-upcase (subseq namestring 0 colpos)))
		     ((floppy tape) 'local-floppy-server)
		     (local 'local-world-server)
		     (t (default-foreign-server (get-server))))
		   (subseq namestring (1+ colpos)))))))


;; returns 2 values specifying people and files
;; the first value can either be the keyword :ALL or a list of usernames
;; the 2nd value can either be the keyword :ALL or a list of box
;; specifiers.  A box specifier is a list whose CAR is a BId and whose
;; CDR, (possibly NULL) is a plist acceptable to make-box-from-server-spec
;; The 1st value can be NIL if the line is empty or a comment line

(defun ex-eol? (char) (or (char= char #\return) (char= char #\newline)))
(defun flush-remaining-chars-on-line (filestream)
  (do* ((eof (list 'eof))
	(char (read-char filestream nil eof) (read-char filestream nil eof)))
      ((or (eq char eof) (ex-eol? char)))))

(defvar keyword-package (find-package "KEYWORD"))

(defun string->BId (buffer)
  (let ((hi 0) (low 0) (where :start))
    (dotimes (i (length buffer))
      (let* ((char (aref buffer i))
	     (digit (digit-char-p char)))
	(cond ((and (eq where :start) (not digit)))
	      ((eq where :start) (setq where :hi hi digit))
	      ((and (eq where :hi) (not digit)) (setq where :low))
	      ((not (null digit))
	       (if (eq where :hi)
		   (setq hi (+& digit (*& hi 10)))
		   (setq low (+& digit (*& low 10))))))))
    (make-bid hi low)))

(defun handle-people-spec (spec)
  (if (and (null (cdr spec)) (string-equal (car spec) "all"))
      ':all
      spec))

(defun read-exports-file-line (filestream &optional (buffer *string-buffer*))
  (buffer-clear buffer)
  (do* ((eof (list 'eof))
	(people-spec nil) (box-specs nil) (box-spec nil) (in-box-spec? nil) ;
	(char (read-char filestream nil eof) (read-char filestream nil eof)))
       ((or (eq char eof) (ex-eol? char))
	(cond ((zerop& (fill-pointer buffer)))
		 ((null in-box-spec?)
		  (push (copy-seq buffer) people-spec) (buffer-clear buffer))
		 ((null box-spec)
		  ;; Looks like no plist, is the buffer a BId or "all"
		  (if (string-equal buffer "all")
		      (return (values (handle-people-spec people-spec)
				      ':all))
		      (push (string->BId buffer) box-specs))
		  (buffer-clear buffer))
		 (t (buffer-clear buffer)))
	(values (handle-people-spec people-spec) box-specs))
    (case char
      ((#\space #\tab) ; keep whitespace to help separate BId components
       (vector-push-extend #\space buffer))
      ((#\; #\#)
       (flush-remaining-chars-on-line filestream)
       (return (values people-spec box-specs)))
      (#\: (unless (zerop& (fill-pointer buffer))
	     (push (copy-seq buffer) people-spec)
	     (buffer-clear buffer))
	   (when (null in-box-spec?) (setq in-box-spec? t)))
      (#\( (cond ((null in-box-spec?)
		  (warn "Found an Open Paren in the user spec of ~A"
			filestream))
		 (t
		  (setq box-spec (string->BId buffer))
		  (buffer-clear buffer)
		  (push (cons box-spec
			      (read-exports-file-box-plist filestream))
			box-specs)
		  (setq box-spec nil))))
      (#\, (cond ((zerop& (fill-pointer buffer)))
		 ((null in-box-spec?)
		  (push (copy-seq buffer) people-spec) (buffer-clear buffer))
		 ((null box-spec)
		  ;; Looks like no plist, is the buffer a BId or "all"
		  (if (string-equal buffer "all")
		      (return (values (handle-people-spec people-spec)
				      ':all))
		      (push (string->BId buffer) box-specs))
		  (buffer-clear buffer))
		 (t (buffer-clear buffer))))
      (t (vector-push-extend char buffer)))))


(defun read-exports-file-box-plist (filestream &optional
					       (buffer *string-buffer*))
  (buffer-clear buffer)
  (do* ((eof (list 'eof))
	(plist nil)
	(char (read-char filestream nil eof) (read-char filestream nil eof)))
       ((or (eq char eof) (ex-eol? char) (char= char #\)))
	(unless (zerop& (fill-pointer buffer))
	  (push (copy-seq buffer) plist) (buffer-clear buffer))
	(nreverse plist))
    (case char
      ((#\space #\tab)) ; ignore whitespace
      (#\, (unless (zerop& (fill-pointer buffer))
	     (push (copy-seq buffer) plist) (buffer-clear buffer)))
      (#\: (unless (zerop& (fill-pointer buffer))
	     (push (intern (string-upcase buffer) keyword-package) plist)
	     (buffer-clear buffer)))
      (t (vector-push-extend char buffer)))))


(defmacro do-export-file-lines ((people-spec-var box-spec-var filestream)
				&body body)
  `(do* ((eof (list 'eof)) (test (peek-char nil ,filestream nil eof)
				 (peek-char nil ,filestream nil eof)))
	((eq test eof) nil)
     (multiple-value-bind (,people-spec-var ,box-spec-var)
	 (read-exports-file-line ,filestream)
       . ,body)))

(defun check-foreign-load-permissions (server username)
  (let ((permission-file
	 (make-pathname :name (format nil "~D" (slot-value server 'top-box-id))
			:type (permission-file-extension server)
			:directory (pathname-directory
				    (slot-value server 'directory)))))
    (unless (and (probe-file permission-file)
		 (with-open-file (s permission-file)
		   (do-export-file-lines (people box s)
		     (declare (ignore box))
		     (when (or (eq people :all)
			       (and (listp people)
				    (member username people
					    :test #'string-equal)))
		       (return T)))))
      (server-error "Access to ~A denied for ~A"
		    (slot-value server 'world-name) username))))


;;;; Fake file ...

(defun fake-file? (bid server)
  (probe-file (make-pathname :name (bid-filename bid)
			     :type (permission-file-extension server)
			     :defaults (slot-value server 'directory))))


(defun access-list (bid server)
  (let ((perm-file (make-pathname :name (bid-filename bid)
				  :type (permission-file-extension server)
				  :defaults (slot-value server 'directory)))
	(username (slot-value server 'login-name))
	(all-list nil) (user-list nil))
    (with-open-file (s perm-file :direction :input)
      (do-export-file-lines (user access s)
	(cond ((eq user :all) (setq all-list access))
	      ((or (eq user username)
		   (and (listp user)
			(member username user :test #'string-equal)))
	       (setq user-list access)))))
    (or user-list all-list)))

;;; these check the permission file and can  signal "permission denied" errors
(defun fake-file-info (bid info server)
  (let ((access-list (access-list bid server)))
    (cond ((null access-list)
	   (server-error "You do not have permission to access Box ~D" bid))
	  ((eq access-list :all)
	   (get-box-info-internal bid info (slot-value server 'directory)))
	  (t
	   (let ((vanilla-info (get-box-info-internal
				bid info (slot-value server 'directory))))
	     ;; change the irrelevant slots and return the new info
	     (setf (sbi-inferiors vanilla-info) (mapcar #'car access-list))
	     vanilla-info)))))

(defun fake-file-size (bid server)
  (let ((access-list (access-list bid server)))
    (cond ((null access-list)
	   (server-error "You do not have permission to access Box ~D" bid))
	  ((eq access-list :all)
	   (with-open-file (s (merge-pathnames (bid-filename bid)
					       (slot-value server 'directory))
			      :direction :input
			      :element-type '(unsigned-byte 8))
	     (file-length s)))
	  (t (length access-list)))))

;;; an access-spec is a list whose CAR is a BoxID and an
;;; optional CDR plist
(defun make-box-from-access-spec (spec)
  (let ((fake-box (box::make-box '(())
				 (or (getf (cdr spec) :type) 'boxer::data-box)
				 (getf (cdr spec) :name))))
    ;; now set the various slots and flags to make this a file box
    (setf (slot-value fake-box 'boxer::first-inferior-row) nil)
    (boxer::set-storage-chunk? fake-box t)
    (boxer::putprop fake-box (car spec) :server-box-id)
    (boxer::shrink fake-box)
    fake-box))

(defun fake-file-box-and-info (bid info server)
  (let ((access-list (access-list bid server)))
    (cond ((null access-list)
	   (server-error "You do not have permission to access Box ~D" bid))
	  ((eq access-list :all)
	   (get-box-and-info-internal bid info (slot-value server 'directory)))
	  (t
	   (let ((vanilla-info (get-box-info-internal
				bid info (slot-value server 'directory)))
		 (return-box (boxer::make-box '(()))))
	     ;; change the irrelevant slots and return the new info
	     (setf (sbi-inferiors vanilla-info) (mapcar #'car access-list))
	     ;; now synthesize a new box
	     (dolist (spec access-list)
	       (boxer::append-row return-box
				  (boxer::make-row
				   (list (make-box-from-access-spec spec)))))
	     ;; fix up attributes of the return-box
	     (set-fake-file-box-attributes return-box)
	     (values return-box vanilla-info))))))

;;;

(defun set-fake-file-box-attributes (box)
  ;; you can NEVER write out a fake file box
  (boxer::set-read-only-box? box t)
  (boxer::set-fake-file-box box t))

(defun set-foreign-server-props (box server)
  (boxer::set-foreign-server box t)
  (unless (slot-value server 'shared?) (boxer::set-read-only-box? box t))
  (boxer::putprop box server 'bfs-server))



;;;;
;;;; FILE: bfslocal.lisp
;;;;
;;;; --entire-file--

#|

  An implementation of the Box File System which uses local
  files.  That is, all box oriented commands will be built
  out of vanilla common lisp functions which manipulate files.


Modification History (most recent at top)

 2/16/03 merged current LW and MCL files, no diffs, copyright updated


|#

(in-package :boxnet)



;; This variable is a site init (see the file site.lisp for details)
(defvar *local-server-directory* "/usr/local/lib/boxer/Server/")


;;;; Utilities for reading and writing BFS file headers

(defvar *box-file-system-header-termination-string*
  "### End of Header info ### Binary Data Starts Next Line ###")

(defvar *header-buffer* (make-array 100 :element-type '(unsigned-byte 8)
				    :fill-pointer 0 :adjustable t))

(defvar *string-buffer* (make-array 64 :element-type #-lucid 'character
                                                     #+lucid 'string-char
				    :fill-pointer 0 :adjustable t))

;; remember that the stream will have been opened with an
;; element-type of (unsigned-byte 8) for the binary data that follows
;; that means we have to coerce the bytes to chars

(defun bfs-eol? (byte)
  (or (=& byte #.(char-code #\return)) (=& byte #.(char-code #\newline))))

(defun buffer-clear (buffer) (setf (fill-pointer buffer) 0))

(defun whitespace-byte? (byte)
  (or (=& byte #.(char-code #\space)) (=& byte #.(char-code #\tab))))

(defun end-of-header? (filestream &optional (skip-first-char t))
  ;; the 1st byte has already been pulled out of the filestream
  (do* ((eof (list 'eof))
	(term-length (1-& (length
			   *box-file-system-header-termination-string*)))
	(byte (read-byte filestream nil eof)
	      (read-byte filestream nil eof))
	(eoh-idx (if skip-first-char 1 0) (1+& eoh-idx))
	(eoh-char (aref *box-file-system-header-termination-string* eoh-idx)
		  (aref *box-file-system-header-termination-string* eoh-idx)))
       ((>=& eoh-idx term-length)
	;; this means we really have gotten to the end
	(unless (bfs-eol? byte) (flush-remaining-line filestream))
	t)
    (cond ((not (=& byte (char-code eoh-char)))
	   ;; if there is ever a mismatch, return nil
	   (flush-remaining-line filestream)
	   (return nil))
	  ((eq byte eof)
	   ;; if we reach the end of the file,return an additional EOF value
	   (return (values nil 'eof)))
	  ((bfs-eol? byte)
	   ;; if we reach the end of a line, return nil
	   (return nil)))))

(defun flush-remaining-line (filestream)
  (do* ((eof (list 'eof))
	(byte (read-byte filestream nil eof) (read-byte filestream nil eof)))
      ((or (eq byte eof) (bfs-eol? byte)))))

(defun read-bfs-header-line (filestream &optional (string *string-buffer*))
  (declare (values key value))
  (let ((eof (list 'eof)) (key nil))
    (flet ((comment-byte? (byte)
	     (or (=& byte #.(char-code #\#)) (=& byte #.(char-code #\;))))
	   (separator-byte? (byte) (=& byte #.(char-code #\:))))
      ;; first, clear the buffer
      (buffer-clear string)
      ;; now, try and form the key
      (do ((byte (read-byte filestream nil eof)(read-byte filestream nil eof)))
	  ((or (eq byte eof) (bfs-eol? byte))
	   (unless (zerop (length string))
	     (debugging-message "WARNING: incomplete header line [~A]" string))
	   (return-from read-bfs-header-line nil))
	(cond ((and (whitespace-byte? byte) (null key)
		    (not (zerop& (fill-pointer string))))
	       ;; 1st trailing whitespace
	       (setq key (intern string)))
	      ((whitespace-byte? byte)
	       ;; ignore all other whitespace
	       )
	      ((and (separator-byte? byte) (null key))
	       (setq key (intern string)) (return))
	      ((separator-byte? byte) (return))
	      ((comment-byte? byte)
	       ;; check to see it this is the end of header comment
	       ;; flush the rest of the line and return
	       (return-from read-bfs-header-line
		 (when (end-of-header? filestream) 'end-of-header)))
	      (t
	       (vector-push-extend (char-upcase (code-char byte)) string))))
      ;; now that the key is complete and we are at a position
      ;; immediately following the ":", use the key to get a value
      (case key
	((readdate accesslength writelength
		   appendlength inferiors forwarding_table)
	 ;; these keys want a number for their value
	 (multiple-value-bind (number end?)
	     (read-bfs-number-internal filestream)
	   (cond ((null number)
		  (error "Couldn't get a number from ~A for ~A"filestream key))
		 ((null end?)
		  (flush-remaining-line filestream)
		  (values key number))
		 (t (values key number)))))
	((owner boxname)
	 ;; these keys want a string for their value
	 (buffer-clear string)
	 (do ((byte (read-byte filestream nil eof)
		    (read-byte filestream nil eof)))
	     ((or (eq byte eof) (bfs-eol? byte))
	      (values key (copy-seq string)))
	   (unless (whitespace-byte? byte)
	     (vector-push-extend (code-char byte) string))))
	(superiorBID
	 ;; this key wants a BId for its value
	 (multiple-value-bind (number end?)
	     (read-bfs-number-internal filestream)
	   (cond ((not (null end?)) (values key number))
		 (t
		  (let ((top number))
		    (multiple-value-bind (number end?)
			(read-bfs-number-internal filestream)
		      (when (null end?) (flush-remaining-line filestream))
		      (if (null number)
			  (values key top)
			  (values key (make-bid top number)))))))))
	(otherwise
	 (debugging-message "Unhandled Key: ~A" key)
	 nil)))))

;;; specialized line readers (these also assume streams
;;; which are :element-type '(unsigned-byte 8))

;;; a BID line consists of 1 or more BID's layed out as
;;; high 32bit value followed by low 32bit value separated
;;; by whitespace.  If there is more than one BId, then they
;;; should be comma separated.  BId's with only one value will
;;; be assumed to have a high 32bit value of 0.

(defun read-bfs-header-bid-line (filestream)
  (let ((numlist nil) (top 0) (fill-top? t))
    (loop
     (multiple-value-bind (number end?)
	 (read-bfs-number-internal filestream)
       (cond ((not (null end?))
	      ;; complete the numlist for various cases
	      (cond ((and fill-top? (null number))
		     ;; numlist looks good and we have nothing to add
		     )
		    ((and (null fill-top?) (not (null number)))
		     ;; numlist needs another value and we have the last piece
		     (push (make-bid top number) numlist))
		    ((null number)
		     ;; numlist is incomplete but we don't have any
		     ;; more pieces so use what we have(assuming top is bottom)
		     (debugging-message "Incomplete BId: using ~D as low 32"
					top)
		     (push top numlist))
		    (t
		     ;; somthing to add, but looks like no bid being formed to
		     ;; add it to.  Use what we get as the entire BId
		     (debugging-message "Incomplete BId: using ~D as low 32"
					number)
		     (push number numlist)))
	      ;; finally return
	      (if (null (cdr numlist)) ; only one entry
		  (return (car numlist))
		  (progn (setq numlist (nreverse numlist)) numlist)))
	     ((null number)) ; perhaps a warning is appropriate ?
	     ((null fill-top?)
	      (push (make-bid top number) numlist)
	      (setq fill-top? nil))
	     (t
	      (setq top number)
	      (setq fill-top? nil)))))))

(defun make-bid (high low) (dpb high (byte 32. 32.) low))

(defun read-bfs-number-internal (filestream &optional (radix 16.))
  (let ((num nil) (eof (list 'eof)))
    (do ((byte (read-byte filestream nil eof) (read-byte filestream nil eof)))
	((or (eq byte eof) (bfs-eol? byte))
	 (values num t))
      (cond ((and (null num) (or (whitespace-byte? byte)
				 (=& byte #.(char-code #\,))))
	     ;; leading whitespace or delimiter chars, ignore them
	     )
	    ((or (whitespace-byte? byte) (=& byte #.(char-code #\,)))
	     ;; trailing whitespace or delimiter char, so return
	     (return num))
	    (t
	     (let ((x (digit-char-p (code-char byte) radix)))
	       (cond ((null x)
		      (error "Got ~C instead of digit while forming number"
			     (code-char byte)))
		     ((null num) (setq num x))
		     (t (setq num (+& (*& num 10) x))))))))))

;;; reads one or more strings, multiple strings are delineated by
;;;  either comma or whitespace
;;; leading whitespace is ignored

(defun read-bfs-header-string-line (filestream &optional
					       (string-buffer *string-buffer*))
  (let ((stringlist nil) (in-string? nil) (eof (list 'eof)))
    (buffer-clear string-buffer)
    (do ((byte (read-byte filestream nil eof) (read-byte filestream nil eof)))
	((or (eq byte eof) (bfs-eol? byte))
	 (cond ((and (null stringlist) in-string?)
		;; we are in the middle of making a single string
		(copy-seq string-buffer))
	       (in-string?
		;; we are in the middle of making the last of several strings
		(push (copy-seq string-buffer) stringlist)
		(nreverse stringlist)
		stringlist)
	       ((null stringlist)
		;; reached EOL or EOF before we got any strings
		nil)
	       (t (nreverse stringlist) stringlist)))
      (cond ((and in-string? (or (whitespace-byte? byte)
				 (=& byte  #.(char-code #\,))))
	     ;; end of a string
	     (push (copy-seq string-buffer) stringlist)
	     (buffer-clear string-buffer)
	     (setq in-string? nil))
	    ((whitespace-byte? byte)
	     ;; if we are not in-string? then it must be
	     ;; leading whitespace so ignore it
	     )
	    (t
	     ;; it's not whitespace so add it to the buffer
	     (vector-push-extend (code-char byte) string-buffer)
	     (when (null in-string?) (setq in-string? t)))))))

;;; the stream is an '(unsigned-byte 8) stream and is assumed to
;;; have already been positioned to the right place
(defun read-box-server-info (stream info)
  (loop (multiple-value-bind (key value)
	    (read-bfs-header-line stream)
	  (unless (null key) ; ignore empty lines
	    (case key
	      (end-of-header (return))
	      (OWNER (setf (sbi-owner info) value))
	      (READDATE (setf (sbi-read-date info) value))
	      (ACCESSLENGTH
	       ;; obsolete slot, still need to pull out the data
	       (dotimes (i value) (read-bfs-header-string-line stream)))
	      (WRITELENGTH
	       ;; obsolete slot, still need to pull out the data
	       (dotimes (i value) (read-bfs-header-string-line stream)))
	      (APPENDLENGTH
	       ;; obsolete slot, still need to pull out the data
	       (dotimes (i value) (read-bfs-header-string-line stream)))
	      (SUPERIORBID (setf (sbi-superior-box-bid info) value))
	      (INFERIORS (setf (sbi-inferiors info)
			       (let ((bids nil))
				 (dotimes (i value bids)
				   (let ((bid (read-bfs-header-bid-line
					       stream)))
				     (setq bids
					   (nconc bids (if (consp bid)
							   bid
							   (list bid)))))))))
	      (BOXNAME nil) ; do nothing (a separate clause to avoid warning)
	      (FORWARDING_TABLE
	       (setf (sbi-forwarding-table info)
		     (let ((bids nil))
		       (dotimes (i value bids)
			 (let ((bid (read-bfs-header-bid-line
				     stream)))
			   (setq bids
				 (nconc bids (if (consp bid)
						 bid
						 (list bid)))))))))
	      (OTHERWISE (debugging-message "Unhandled BFS Header Key:~A"
					    key))))))
  ;; set the write-date in the info, we get this directly from the file
  (setf (sbi-write-date info) (file-write-date stream))
  info)

;;; grab the 1st to byte of an '(unsigned-byte 8) stream
;;; and make sure they are the beginning of a header
(defun header-start? (s)
  (and (=& (read-byte s) (ldb boxer::%%bin-op-top-half
			      boxer::bin-op-box-server-info-start))
       (=& (read-byte s) (ldb boxer::%%bin-op-low-half
			      boxer::bin-op-box-server-info-start))))

;;; For use by the READ primitive which isn't interested in the header info
;;; another '(unsigned-byte 8) stream
(defun skip-box-server-info (stream)
  (loop (when (end-of-header? stream nil) (return))))

;;; these use streams with :element-type 'string-char
;;; this will work because we can open the file once, write the header,
;;; close it, then open it again with :append in a binary mode to
;;; write the data

(defun write-bsi-bid (stream bid)
  (format stream "~D ~D" (ldb (byte 32. 32.) bid) (ldb (byte 32. 0.) bid)))

(defun write-bsi-list (stream name list)
  (declare (string name) (list list))
  (format stream "~%~A: ~D" name (length list))
  (cond ((null list))
	((stringp (car list))
	 (dolist (item list)  (format stream "~%~A" item)))
	((numberp (car list))
	 (dolist (item list) (terpri stream) (write-bsi-bid stream item)))
	(t (error "The Box server list, ~A, doesn't have strings or BId's"))))

(defun write-header-preamble (stream)
  (write-char (code-char 240) stream)
  (write-char (code-char 57)  stream)
  (terpri stream))

(defun write-header-finish (stream)
  (format stream "~A~%" *box-file-system-header-termination-string*))

(defun write-box-server-info (stream info)
  ;; first, send the 16 bit header opcode
  (write-header-preamble stream)
  ;; now the header data
  (format stream "readdate: ~A~%owner: ~A"
	  (sbi-read-date info) (sbi-owner info))
  (format stream "~%boxname: ~A" (sbi-boxname info))
  (format stream "~%superiorBID: ")
  (write-bsi-bid stream (sbi-superior-box-bid info))
  (write-bsi-list stream "inferiors" (sbi-inferiors info))
  (write-bsi-list stream "forwarding_table" (sbi-forwarding-table info))
  ;; finally write out the end of header line
  (terpri stream)
  (write-header-finish stream))



;;;; User file Utilities

(defvar *line-buffer* (make-array 100 :element-type #-lucid 'character #+lucid 'string-char
				  :fill-pointer 0 :adjustable t))

;;; Searches for a particular user in the user file and
;;; returns the password, pretty-name, directory and top-box-id
(defun user-values (user)
  (declare (values password pretty-name directory top-box-id))
  (with-open-file (s (merge-pathnames "userfile" *local-server-directory*)
		     :direction :input)
    (catch 'eof
      (loop
       (multiple-value-bind (account password pretty-name directory
				     uid top-box-id)
	   (user-line-values s)
	 (when (string-equal account user)
	   (return (values password pretty-name directory
			   uid top-box-id))))))))

;;; Returns a unique user ID
;;; found by scanning the userfile
;;; for now, it just returns MAX+1, we might want to be more
;;; clever in the future by looking for gaps
(defun new-user-id ()
  (with-open-file (s (merge-pathnames "userfile" *local-server-directory*)
		     :direction :input)
    (let ((max-id 0))
      (catch 'eof
	(loop
	 (multiple-value-bind (account password pretty-name directory
				       uid top-box-id)
	     (user-line-values s)
	   (declare (ignore account password pretty-name directory top-box-id))
	   (setq max-id (max uid max-id)))))
      (1+ max-id))))

(defmacro do-server-users ((account password pretty-name
				    directory uid top-box-id)
			   &body body)
  `(with-open-file (s (merge-pathnames "userfile" *local-server-directory*)
		      :direction :input)
     (catch 'eof
       (loop
	(multiple-value-bind (,account ,password ,pretty-name ,directory
				       ,uid ,top-box-id)
	    (user-line-values s)
	  ,@body)))))

;;; stream is supposed to be a char stream and is expected to be
;;; positioned at the beginning of a line
;;; A userfile line consists of colon separated fields.  The fileds are:
;;; Username:Password:Pretty Name:Directory Name:UID:Top Level Box ID
(defun user-line-values (stream)
  (declare (values account password pretty-name directory top-box-id
		   uid-prefix))
  (let ((values (make-array 6)) (idx 0) (eof (list 'eof)))
    (setf (fill-pointer *line-buffer*) 0)
    (do ((char (read-char stream nil eof) (read-char stream nil eof)))
	((or (eq char #\newline) (eq char #\return))
	 (when (< idx (1-& (length values)))
	   (warn "Incomplete user file line"))
	 (setf (svref& values idx)
	       ;; the last value should be coerced into a Box ID
	       (string-to-bid *line-buffer*))
	 (values (svref& values 0) (svref& values 1) (svref& values 2)
		 (svref& values 3) (svref& values 4) (svref& values 5)))
      (cond ((eq char eof)
	     (throw 'eof nil))
	    ((char-equal char #\:)
	     (setf (svref& values idx)
		   (if (=& idx 4) ; the UID needs to coerced into a number
		       (string-to-number *line-buffer*)
		       (copy-seq *line-buffer*)))
	     (incf& idx)
	     (setf (fill-pointer *line-buffer*) 0))
	    (t (vector-push-extend char *line-buffer*))))))

(defun string-to-number (string &optional (idx 0) (radix 10.))
  (let ((number nil) (end-idx (1-& (length string))))
    (do ((char (aref string idx) (aref string idx)))
	((=& idx end-idx) (let ((last-digit (digit-char-p char radix)))
			    (cond ((null last-digit)
				   (values number idx))
				  ((null number)
				   (values last-digit idx))
				  (t
				   (values (+& (*& number radix) last-digit)
					   idx)))))
      (let ((digit (digit-char-p char radix)))
	(cond ((and (null digit) (null number)))
	      ;; whitespace or garbage, ignore it
	      ((null digit)
	       ;; number must be finished
	       (return (values number idx)))
	      ((null number)
	       ;; we've hit the first digit
	       (setq number digit))
	      (t
	       ;; in the middle of making a number
	       (setq number (+& (*& radix number)
				digit)))))
      (incf& idx))))

(defun string-to-bid (string)
  (multiple-value-bind (top idx)
      (string-to-number string 0 16.)
    (cond ((null top) (error "can't get a BID out of ~S" string))
	  (t (let ((bottom (string-to-number string idx 16.)))
	       (if (null bottom) (make-bid 0 top) (make-bid top bottom)))))))



;;;; Filename utilities

;;; NO Leading 0's !!!!!

(defun bid-filename (bid) (format nil "~D" bid))

(defvar *backup-file-suffix* "~")

(defun bid-backup-filename (bid) (format nil "~D~A" bid *backup-file-suffix*))

(defun backup-pathname (pathname)
  (merge-pathnames
   (format nil "~A~A" (pathname-name pathname) *backup-file-suffix*)
   pathname))

(defun bid-file-name? (pathname)
  (every #'digit-char-p (pathname-name pathname)))

(defun backup-bid-file-name? (pathname)
  (let ((name (pathname-name pathname)))
    (and (char= (char name (1-& (length name))) (char *backup-file-suffix* 0))
	 (dotimes (i (1-& (length name)) T)
	   (unless (digit-char-p (char name i)) (return nil))))))

;; probably want to make implementation specific versions of these for speed
(defun get-file-size (pathname) (with-open-file (s pathname) (file-length s)))

;;; this MUST not return until we are sure that the incremented
;;; file count has been written back
;;; This is to prevent possible duplication of allocated Box ID's
;;; in case of a crash while writing.

(defun increment-counter-file (counter-file)
  (when (null (probe-file counter-file))
    (error "The counter file, ~A, is missing" counter-file))
  (let ((old-count 0))
    (with-open-file (s counter-file :direction :input)
      (setq old-count (string-to-number (read-line s))))
    ;; now write back the new count
    (with-open-file (s (merge-pathnames "newcount" counter-file)
		       :direction :output :if-exists :supersede)
      (format s "~D~%" (1+ old-count)))
    ;; now backup the old counter file
    (rename-file counter-file (merge-pathnames "oldcount" counter-file))
    ;; install the new counter file
    (rename-file (merge-pathnames "newcount" counter-file) counter-file)
    (delete-file (merge-pathnames "oldcount" counter-file))
    ;; now if all that has worked, we can safely return the new count
    (1+ old-count)))

;;; these are for locking individual files
;;; for groups, see with-single-transaction
;;; these should use gensyms or something to be able
;;; to recognize who set the lock

(defmacro with-write-lock ((file lock-string) &body body)
  `(let ((lock-file (merge-pathnames ".LOCK" ,file)))
     (if (probe-file lock-file)
	 (server-error "The file, ~A, is locked for ~A"
		       ,file (with-open-file (ls lock-file)
			       (read-line ls nil nil)))
	 (unwind-protect
	      (progn
		(with-open-file (ls lock-file :direction :output)
		  (format ls "~A~%" ,lock-string))
		. ,body)
	   (delete-file lock-file)))))

(defmacro with-lock-check (open-file-args &body body)
  `(let ((lock-file (merge-pathnames ".LOCK" ,(cadr open-file-args))))
     (if (probe-file lock-file)
	 (server-error "The file, ~A, is locked for ~A"
		       ,(cadr open-file-args)
		       (with-open-file (ls lock-file) (read-line ls nil nil)))
	 (with-open-file ,open-file-args . ,body))))



;;; records all file activity so that it can be processed in
;;; one chunk.  Any errors in the body should leave the state
;;; of the file system untouched.

;;; File transactions run in 3 phases
;;;
;;;   o in phase 1, boxer structure is dumped and info's are updated
;;;     with any new info appearing in the temp directory.
;;;     Phase 1 transactions, including recording, occur in the body
;;;     of the code.
;;;   o in phase 2, files are moved into the main directory,
;;;     old files are backed up and files are deleted.
;;;     Phase 2 transaction play through the *current-transactions*
;;;     list and are handled in Finalize-transactions
;;;   o If phase 1 and 2 complete, then the temp directory is cleared
;;;     if we are losing, then we try and undo any transactions which
;;;     have affected the main directory.  Phase 3 transaction run in
;;;     an unwind-protect as Undo-Transactions and Clear-Transactions
;;;

(defvar *transaction-sub-directory* "temp")

(defvar *transaction-log-file* "transactions.log")

(defvar *log-transactions?* t)

(defvar *inside-transaction-body* nil)

(defvar *current-transactions* nil)

(defvar *phase-2-transactions* nil)

;; the last step of finalize-transactions should set this to T
(defvar *transaction-complete* nil)

(defun record-bfs-transaction (trans-type &rest args)
  (if *inside-transaction-body*
      (setq *current-transactions*
	    (nconc *current-transactions* (list (cons trans-type args))))
      (error "No context to record a File system transaction")))

;;; possible transaction types that this might handle are:
;;;  NEW-FILE (current-filename destination-filename)
;;;  NEW-COPY (current-filename destination-filename)
;;;  DELETE-FILE (current-filename)
(defmacro with-single-bfs-local-transaction (&body body)
  `(let ((*inside-transaction-body* t)
	 (*current-transactions* nil)
	 (*transaction-complete* nil)
	 (*phase-2-transactions* nil))
     (flet ((finalize-transactions ()
	      (debugging-message "Performing Phase 2 transactions...")
	      (dolist (trans *current-transactions*)
		(ecase (car trans)
		  (new-file
		   (when (probe-file (caddr trans))
		     ;; check for an existing file and back it up if there is
		     (rename-file (caddr trans)
				  (backup-pathname (caddr trans))))
		   (rename-file (cadr trans) (caddr trans)))
		  (new-copy
		   ;; there should NOT be an existing file
		   ; (when (probe-file (caddr trans)) (error "..."))
		   (rename-file (cadr trans) (caddr trans)))
		  (delete-file
		   (delete-bid-box-internal-1 (cadr trans))))
		;; after each successful, phase 2 operation, mark it down
		(push trans *phase-2-transactions*))
	      ;; finally mark that we are done and we can also clear teh
	      ;; phase-2-transactions list
	      (setq *transaction-complete* t
		    *phase-2-transactions* nil))
	    (undo-transactions ()
	      ;; this tries to undo the effects of any phase 2 transactions
	      ;; which may have been performed prior to an error condition
	      (dolist (trans *phase-2-transactions*)
		(ecase (car trans)
		  (new-file
		   ;; if there is a backup available, use it, otherwise
		   ;; be satisfied with removing the file
		   (let ((backup (merge-pathnames
				  (format nil "~A~A"
					  (pathname-name (caddr trans))
					  *backup-file-suffix*)
				  (caddr trans))))
		     (if (probe-file backup)
			 (rename-file backup (caddr trans))
			 (delete-file (caddr trans)))))
		  (new-copy
		   ;; just remove the file
		   (delete-file (caddr trans)))
		  (delete-file
		   ;; bring the file back from the delete dir
		   (let ((del-file (make-pathname
				    :name (pathname-name (cadr trans))
				    :directory
				    (append (pathname-directory (cadr trans))
					    (list
					     *delete-subdirectory-name*)))))
		     (if (probe-file del-file)
			 (rename-file del-file (cadr trans))
			 (warn "Could not undo transaction: ~A" trans)))))))
	    (clear-transactions ()
	      ;; this removes all temp files
	      (dolist (trans *current-transactions*)
		(ecase (car trans)
		  ((new-file new-copy) (delete-file (cadr trans)))
		  (delete-file nil)))))
     (unwind-protect
	  (progn
	    ;; perhaps we should write a descriptor file into the temp dir now
	    (progn . ,body)
	    (finalize-transactions))
       (when (null *transaction-complete*) (undo-transactions))
       (clear-transactions *current-transactions*)))))


;;; it should be a big performance boost to carefully tune
;;; this piece here for different implementations
(defun %bfs-copy-stream (in out)
  (do* ((eof (list 'eof))
	(byte (read-byte in nil eof) (read-byte in nil eof)))
       ((eq byte eof))
    (write-byte byte out)))



;;;; Top Level Utilities

(defun get-box-info-internal (bid info directory)
  (with-lock-check (s (merge-pathnames (bid-filename bid) directory)
		      :direction :input :element-type '(unsigned-byte 8))
    ;; first grab the 1st 2 bytes and make sure we have a header
    (unless (header-start? s)
      (error "~A does not have a File System Header"
	     (merge-pathnames (bid-filename bid) directory)))
    ;; we should now be positioned at the beginning of the header
    (read-box-server-info s info)
    info))

(defun set-box-info-internal (bid new-info directory)
  (let ((dest-path (merge-pathnames (bid-filename bid) directory))
	(tmp-path (if (null *inside-transaction-body*)
		      (merge-pathnames (gensym) directory)
		      (make-pathname :name (bid-filename bid)
				     :directory
				     (append (pathname-directory (pathname
								  directory))
					     (list
					      *transaction-sub-directory*))))))
    (with-write-lock (dest-path (format nil "Setting info for ~A on ~A"
					(unless (null *box-server-user*)
					  (bsui-username *box-server-user*))
					(unless (null *box-server-user*)
					  (bsui-hostname *box-server-user*))))
      (with-open-file (s dest-path :direction :input
			 :element-type '(unsigned-byte 8))
	;; first grab the 1st 2 bytes and make sure we have a header
	(unless (header-start? s)
	  (error "~A does not have a File System Header"
		 (merge-pathnames (bid-filename bid) directory)))
	;; we should now be positioned at the beginning of the header
	(skip-box-server-info s)
	;; we should now be positioned at the beginning of the data
	;; now write the new header out into the temp file name
	(with-open-file (out tmp-path :direction :output)
	  (write-box-server-info out new-info))
	;; now write the data out (the file had better be there)
	(with-open-file (out tmp-path
			     :direction :output
			     :element-type '(unsigned-byte 8)
			     :if-exists :append :if-does-not-exist :error)
	  (%bfs-copy-stream s out)))
      ;; now carefully put everything where it is supposed to go
      ;; or else record the transaction and put it there later
      (cond ((null *inside-transaction-body*)
	     (rename-file dest-path (merge-pathnames (bid-backup-filename bid)
						     directory))
	     (rename-file tmp-path dest-path))
	    (t
	     (record-bfs-transaction 'new-file tmp-path dest-path))))))

(defun get-box-and-info-internal (bid info directory)
  (let ((file (merge-pathnames (bid-filename bid) directory)))
    (when (null (probe-file file)) (box::maybe-uncompress-file file))
    ;; if it STILL isn't here, then signal an error
    (when (null (probe-file file)) (server-error "Couldn't find ~A" file))
    (with-open-file (s file :direction :input
		       :element-type '(unsigned-byte 8))
      ;; We parse the header first
      (unless (header-start? s)
	;; make sure we have a header
	(error "~A does not have a File System Header"
	       (merge-pathnames (bid-filename bid) directory)))
      ;; we should now be positioned at the beginning of the header
      (read-box-server-info s info)
      ;; the info slots should now be filled
      ;; Now we read in the box, the stream should be at the start of
      ;; binary data with a bin-op-format-version word coming next
      ;; we do it in an environment where the forwarding table is bound
      (let ((*loading-via-box-server?* T))
	(let ((newbox (with-forwarding-table-bound (bid info)
			(boxer::load-binary-box-from-stream-internal s))))
	  (values newbox info))))))


(defun set-box-and-info-internal (bid box info directory)
  (let ((dest-path (merge-pathnames (bid-filename bid) directory))
	(tmp-path (if (null *inside-transaction-body*)
		      (merge-pathnames (gensym) directory)
		      (make-pathname :name (bid-filename bid)
				     :directory
				     (append (pathname-directory (pathname
								  directory))
					     (list
					      *transaction-sub-directory*)))))
	(backup-path (merge-pathnames (bid-backup-filename bid) directory)))
    (with-write-lock (dest-path (format nil "Writing box and info for ~A on ~A"
					(unless (null *box-server-user*)
					  (bsui-username *box-server-user*))
					(unless (null *box-server-user*)
					  (bsui-hostname *box-server-user*))))
      ;; check for existence of file first
      (cond ((not (null *inside-transaction-body*))
	     (set-box-and-info-internal-1 tmp-path box info)
	     (record-bfs-transaction 'new-file tmp-path dest-path))
	    ((probe-file dest-path)
	     (unwind-protect
		  (progn
		    (set-box-and-info-internal-1 tmp-path box info)
		    ;; backup
		    (rename-file dest-path backup-path)
		    ;; install
		    (rename-file tmp-path dest-path))
	       ;; make sure the temporary file is removed
	       (when (probe-file tmp-path) (delete-file tmp-path))
	       ;; make sure that there is the original, if there isn't
	       ;; bring the backup file back if we can
	       (when (and (null (probe-file dest-path))
			  (probe-file backup-path))
		 (rename-file backup-path dest-path))))
	    (t
	     (set-box-and-info-internal-1 dest-path box info))))))

(defun set-box-and-info-internal-1 (file box info)
  ;; first, write out the header
  (with-open-file (out file :direction :output)
    (write-box-server-info out info))
  ;; and now, dump out the box
  (with-open-file (out file
		       :direction :output :element-type '(unsigned-byte 8)
		       :if-exists :append :if-does-not-exist :error)
    (box::dump-top-level-box-to-stream box out)))


;;; This needs to do the following:
;;;   o copy all inferiors keeping track of the old and new BId's
;;;   o update the header of the top level box reflecting the (possibly)
;;;     new superior BId as well as a forwarding table for the inferiors
;;;   o copy the data part of the top level box
;;;   o Returns the BID of the copy
(defun copy-bid-box-internal (bid superior-bid
				  from-directory to-directory new-uid-prefix)
  (let* ((new-bid (make-bid new-uid-prefix
			    (increment-counter-file
			     (merge-pathnames "counter" to-directory))))
	 (temp-info (%make-server-box-info))
	 (old-pathname (merge-pathnames (bid-filename bid) from-directory))
	 (new-pathname (merge-pathnames (bid-filename new-bid) to-directory))
	 (tmp-pathname (if (null *inside-transaction-body*)
			   new-pathname
			   (make-pathname :name (pathname-name new-pathname)
					  :directory
					  (append
					   (pathname-directory new-pathname)
					   (list
					    *transaction-sub-directory*)))))
	 (new-forwarding-table nil)
	 (counter-file (merge-pathnames "counter" to-directory)))
    (dolist (inf (get-all-bid-inferiors bid))
      (let ((new-inf-bid (make-bid new-uid-prefix
				   (increment-counter-file counter-file))))
	(cond ((null *inside-transaction-body*)
	       (bfs-copy-file (merge-pathnames (bid-filename inf)
					       from-directory)
			      (merge-pathnames (bid-filename new-inf-bid)
					       to-directory)))
	      (t
	       (let* ((dest (merge-pathnames (bid-filename new-inf-bid)
					       to-directory))
		      (temp (make-pathname :name (pathname-name dest)
					   :directory
					   (append
					    (pathname-directory dest)
					    (list
					     *transaction-sub-directory*)))))
		 (bfs-copy-file (merge-pathnames (bid-filename inf)
						 from-directory)
				temp)
		 (record-bfs-transaction 'new-copy temp dest))))
	(setq new-forwarding-table (append new-forwarding-table
					   (list bid new-inf-bid)))))
    ;; write out new header info
    (with-open-file (s old-pathname :direction :input
		       :element-type '(unsigned-byte 8))
      (unless (header-start? s)
	;; make sure we have a header
	(error "~A does not have a File System Header" old-pathname))
      ;; we should now be positioned at the beginning of the header
      (get-box-info-internal bid temp-info from-directory)
      ;; we are now positioned at the beginning of the data in the old file
      ;; change the relevant fields in the info
      (setf (sbi-bid temp-info)              new-bid
	    (sbi-superior-box-bid temp-info) superior-bid
	    (sbi-forwarding-table temp-info) (combine-forwarding-tables
					      (sbi-forwarding-table temp-info)
					      new-forwarding-table))
      ;; now write the new header out
      (with-open-file (out tmp-pathname :direction :output)
	(write-box-server-info out temp-info))
      ;; now copy the data over
      (with-open-file (out tmp-pathname :direction :output
			   :element-type '(unsigned-byte 8)
			   :if-exists :append :if-does-not-exist :error)
	(%bfs-copy-stream s out)))
    (unless (null *inside-transaction-body*)
      (record-bfs-transaction 'new-copy tmp-pathname new-pathname))
    new-bid))

;; recurses through inferiors
(defun get-all-bid-inferiors (bid &optional include-top?)
  (let ((all-infs (when include-top? (list bid))))
    (dolist (inf (get-bid-inferiors bid))
      (setq all-infs (append all-infs (get-all-bid-inferiors inf t))))
    all-infs))

(defun bfs-copy-file (from to)
  (with-open-file (in from :direction :input :element-type '(unsigned-byte 8))
    (with-open-file(out to :direction :output :element-type '(unsigned-byte 8))
      (%bfs-copy-stream in out))))

;;; these 2 functions are identical to FIND-BID and MERGE-FORWARDING-TABLES
;;; in the client part of the code.  We use 2 versions to preserve the
;;; client/server abstraction
(defun bfs-local-find-bid (bid table)
  (do* ((remaining-pairs table (cddr remaining-pairs))
	(current-bid (car remaining-pairs) (car remaining-pairs)))
       ((null current-bid) nil)
    (when (= current-bid bid)
      (return (cadr remaining-pairs)))))

(defun combine-forwarding-tables (old new)
  (let ((newnew nil))
    ;; loop through the old looking for an entry in the new
    (do* ((remaining-pairs old (cddr remaining-pairs))
	  (orig-bid  (car remaining-pairs) (car remaining-pairs))
	  (trans-bid (cadr remaining-pairs) (cadr remaining-pairs))
	  (new-trans (bfs-local-find-bid trans-bid new)))
	 ((null orig-bid))
      (cond ((bfs-local-find-bid orig-bid new)
	    ;; old entry is handled in new table, so do nothing
	     )
	    ((not (null new-trans))
	     ;; old translation is obsolete, convert it to a new one
	     (setq newnew (append newnew (list orig-bid new-trans))))
	    (t
	     ;; old entry has nothing to do with the new table so preserve it
	     (setq newnew (append newnew (list orig-bid trans-bid))))))
    ;; now append the new table to the built up table of
    ;; translated old references and return it
    (append newnew new)))

;; don't really delete the files, just move them to the
;; deleted files directory.  We rely on some other mechanism to
;; actually remove the files from the delete directory (an expunge primitive ?)
;; Also needs to plice the deleted BId out of the inferiors of the
;; superior file box.

(defvar *delete-subdirectory-name* ".Deleted")

;; moves the (possible) backup filename too
(defun delete-bid-box-internal-1 (file &optional dir)
  (let ((backup (backup-pathname file))
	(del-dir (append (if dir
			     (pathname-directory dir)
			     (pathname-directory file))
			 (list *delete-subdirectory-name*))))
    (when (probe-file backup)
      (rename-file backup
		   (make-pathname :name (pathname-name backup)
				  :directory del-dir)))
    (rename-file file
		 (make-pathname :name (pathname-name file)
				:directory del-dir))))

;; need to recurse through the inferiors as well
(defun delete-bid-box-internal (bid directory)
  (let ((file-to-delete (merge-pathnames (bid-filename bid) directory)))
    (unless (null (probe-file file-to-delete))
      ;; loop through the inferiors...
      (dolist (inf (get-bid-inferiors bid))
	(delete-bid-box-internal inf directory))
      (if (null *inside-transaction-body*)
	  ;; move the file right away
	  (delete-bid-box-internal-1 file-to-delete (pathname directory))
	  ;; or else record it for later handling
	  (record-bfs-transaction 'delete-file file-to-delete)))))



;;;; Ring out the old, ring in the new....
;;; Only useful for converting old Sun Files so...

#+sun
(progn

  (defvar *old-file-system-directory* "/usr/emstsun/boxer/user/bin/FILES/")

  (defun convert-old-file (bid &optional (new-directory (lcl::pwd))(level 0))
    (let* ((old-data-file-name (merge-pathnames
				(string-downcase (format nil "~16,'0X" bid))
				*old-file-system-directory*))
	   (old-head-file-name (merge-pathnames ".d" old-data-file-name))
	   (new-file-name (merge-pathnames (bid-filename bid) new-directory)))
      (cond ((and (probe-file old-data-file-name)
		  (probe-file old-head-file-name))
	     (terpri) (dotimes (i (* 3 level)) (write-char #\space))
	     (format t "Converting old ~X file to new ~D file" bid bid)
	     ;; first generate the new header
	     (with-open-file (new-s new-file-name :direction :output)
	       (write-header-preamble new-s)
	       ;; use the old info file
	       (with-open-file (old-s old-head-file-name :direction :input)
		 (do* ((eof (list 'eof))
		       (char (read-char old-s nil eof)
			     (read-char old-s nil eof)))
		      ((eq char eof))
		   (write-char char new-s)))
	       (write-header-finish new-s))
	     ;; now copy the old binary data
	     (with-open-file (new-s new-file-name :direction :output
				    :element-type '(unsigned-byte 8)
				    :if-exists :append
				    :if-does-not-exist :error)
	       (with-open-file (old-s old-data-file-name :direction :input
				      :element-type '(unsigned-byte 8))
		 (do* ((eof (list 'eof))
		       (byte (read-byte old-s nil eof)
			     (read-byte old-s nil eof)))
		      ((eq byte eof))
		   (write-byte byte new-s))))
	     t)
	    (t
	     (terpri) (dotimes (i (* 3 level)) (write-char #\space))
	     (format t "~%Data or Header file missing for ~16,'0X" bid)
	     nil))))

  ;; you have to first,
  ;; (1) make the new directory
  ;; (2) convert the top level box by hand and place it in the new directory

  ;; this uses internal functions in order to avoid having an active server

  (defun convert-user-files (top-bid &optional (new-dir (lcl::pwd))(level 0))
    (let ((info (%make-server-box-info)))
      ;; read the info
      (get-box-info-internal top-bid info new-dir)
      (let ((infs (sbi-inferiors info)))
	(dolist (bid infs)
	  (if (convert-old-file bid new-dir level)
	      (convert-user-files bid new-dir (1+ level))
	      (progn
		(format t "  ...Removing BID: ~D from inferiors" bid)
		(setf (sbi-inferiors info)
		      (delete bid (sbi-inferiors info) :test #'=)))))
	;; now write out the new info if the inferiors have changed
	(unless (equal infs (sbi-inferiors info))
	  (set-box-info-internal top-bid info new-dir)))))

) ; end of old sun specific conversion utilities




;;;; Debugging Help

(defun calculate-inferiors (box)
  (let ((infs nil))
    (box::map-over-all-inferior-boxes
     box #'(lambda (b)
	     (when (box::storage-chunk? b)
	       (let ((id (box::getprop b :server-box-id)))
		 (unless (null id) (push id infs))))))
    infs))



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

;; 2023-08-30 Removing completely unreferenced members of defstruct graphics-sheet
  (colormap nil)
  (prepared-flag nil)


(defun intern-in-boxer-user-package (symbol)
  (intern (string symbol) 'boxer-user))

;; now does the compile time check for PCL-ness
;; sgithens - 2019-11-17 This is currently used once in this file,
;; 4 times in grobjs.lisp, many times in optimize-classes.lisp,
;; and once in simple-stream.lisp. I'm not sure it will still be necessary
;; for a modern SBCL CLOS type implementation. Not quite ready to delete it
;; though...
;;
; (defvar *include-compiled-type-checking* t)
;;
;; (defmacro deftype-checking-macros (type type-string)
;;   (let ((predicate-name (intern (symbol-format nil "~a?" type)))
;; 	(check-arg-name (intern (symbol-format nil "CHECK-~a-ARG" type)))
;; 	(bcm-class (let ((class (find-class type nil)))
;; 		     (when (typep class 'block-compile-class)
;; 		       class))))
;;     `(progn
;;        (defsubst  ,predicate-name (x)
;; 	 ,(if (null bcm-class)
;; 	      `(typep x ',type)
;; 	      (#+clos expand-clos-type-check #+pcl expand-pcl-type-check
;;                #+mcl expand-mcl-type-check
;;                'x type bcm-class)))
;;        (defmacro  ,check-arg-name (x)
;; 	 ,(when *include-compiled-type-checking*
;; 	    ``(check-type ,x  (satisfies ,',predicate-name) ,,type-string))))))

;; Previously this was in a source file lw-bcm.lisp, though on modern machines
;; it seems unlikely we'll need to bring this forward. sgithens - 2019-11-17

(defclass block-compile-class (standard-class)
  ((counter :initform 0)))

;; https://stackoverflow.com/questions/19446174/sbcl-clos-why-do-i-have-to-add-a-validate-superclass-method-here
#+sbcl
(defmethod sb-mop:validate-superclass ((class block-compile-class)
                                       (super standard-class))
  t)

(DEFVAR *HIGHLIGHT-YANKED-REGION* NIL
  "Controls whether freshly yanked back region should be highlighted. ")

(DEFVAR *REDISPLAY-CLUES* NIL
  "A list of redisplay-clues. This are hints left behind by the editor
   to help the redisplay code figure out what is going on.")

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

;; stub, for now
(defun warp-pointer (window x y)
  (declare (ignore window x y))
  nil)

(defmacro with-mouse-tracking-inside (((original-x-variable original-x-value)
               (original-y-variable original-y-value)
              min-x min-y
              max-x max-y &rest keys)
              &body body)
  `(with-mouse-tracking ((,original-x-variable ,original-x-value)
                         (,original-y-variable ,original-y-value) ,@keys)
     ;; if the mouse has strayed,
     ;; then send it back
     (cond
       ((box::<& ,original-x-variable ,min-x)
  (cond ((box::<& ,original-y-variable ,min-y)
         (warp-pointer *boxer-pane* ,min-x ,min-y))
        ((box::>& ,original-y-variable ,max-y)
         (warp-pointer *boxer-pane* ,min-x ,max-y))
        (t
         (warp-pointer *boxer-pane* ,min-x ,original-y-variable))))
       ((box::>& ,original-x-variable ,max-x)
  (cond ((box::<& ,original-y-variable ,min-y)
         (warp-pointer *boxer-pane* ,max-x ,min-y))
        ((box::>& ,original-y-variable ,max-y)
         (warp-pointer *boxer-pane* ,max-x ,max-y))
        (t
         (warp-pointer *boxer-pane* ,max-x ,original-y-variable))))
       ((box::<& ,original-y-variable ,min-y)
  (warp-pointer *boxer-pane* ,original-x-variable ,min-y))
       ((box::>& ,original-y-variable ,max-y)
  (warp-pointer *boxer-pane* ,original-x-variable ,max-y))
       (t (progn . ,body)))))

(defvar *redisplayable-windows* nil
  "This is a list of all the windows which should be redisplayed when
   REDISPLAY is called." )

(defvar *redisplayable-window-outermost-box-alist* nil
  "An alist that keeps track of the outermost screen box for each
   redisplayable window in *redisplayable-windows*. ")

;;;; Blinkers, mostly copied from clx


;; This is a crock. depends too much on *point-blinker* being the correct
;; thing need to change the window representation so we can ask a window
;; which of its blinkers corresponds to the MAIN cursor.  We do this for
;; now cause the only window that need this is the *boxer-pane*

;; OpenGL note: no more with-open-blinker, just change the vars
(defun set-cursorpos (pane x y)
  (setf (boxer::blinker-x (boxer::point-blinker pane)) (round x)
        (boxer::blinker-y (boxer::point-blinker pane)) (round y)))

(defun set-cursor-size (cursor wid hei)
  (when (and (not (null boxer::*boxer-system-hacker*))
         (or (< wid 0) (< hei 0)))
      (cerror "Set Value to 0"
        "Blinker Width or Height is < 0"))
  (setf (boxer::blinker-wid  cursor) (max (round wid) 0))
  (setf (boxer::blinker-hei cursor) (max (round hei) 0)))

(defvar *point-blinker* nil)

;; if there ever is more than 1 window, this ought to become an alist
;; of windows and blinkers
;; and we should extend the blinker structure to point to the owning window...
(defvar *boxer-window-blinker-alist* nil)

(defun sheet-blinker-list (window)
  (let ((entry (box::fast-assq window *boxer-window-blinker-alist*)))
    (unless (null entry)
      (cdr entry))))

(defun %set-sheet-blinker-list (window new-list)
  (let ((entry (box::fast-assq window *boxer-window-blinker-alist*)))
     (if (null entry)
   (push new-list *boxer-window-blinker-alist*)
   (setf (cdr entry) new-list)))
  new-list)

(defsetf sheet-blinker-list %set-sheet-blinker-list)

(defun draw-region-row-blinker (blinker)
  (box::draw-rectangle
   (blinker-width blinker) (blinker-height blinker)
   (blinker-x blinker)     (blinker-y blinker)))

;;; these are used by others

;; In OpenGL, just draw, no need to erase as we are double buffering...

(defun set-cursor-visibility (blinker new-vis)
  (setf (blinker-visibility blinker) new-vis))


;; 2023-03-18 used to be set in get-boxer-input, but nowhere else or used
(defvar *literal-input?* nil)

(defun get-boxer-input (&optional (window *boxer-pane*))
  (declare (ignore window))
  (let ((*literal-input?* t))
    (mp::process-wait "Input" #'(lambda () (not (null *boxer-eval-queue*)))))
...


(defvar *boxer-frame-initial-width* 800) ;(- (screen-width (convert-to-screen)) 200)
(defvar *boxer-frame-initial-height* 600);(- (screen-height (convert-to-screen))100)

;;for debugging
(defvar *saved-keys* nil)
(defvar *save-key-length* 40)

(defun save-key (char)
  (if (> (length *saved-keys*) *save-key-length*)
      (setq *saved-keys* (nconc (cdr *saved-keys*) (list char)))
    (setq *saved-keys* (nconc *saved-keys* (list char)))))


(defun abort-event? (char)
  (and (characterp char)
       (or ;; sgithens TODO (char= char #\control-\g)
           (char= char #\escape))))
           ;; sgithens TODO (char= char #\control-\.))))

(defparameter *boxer-window-left-margin* 50)
(defparameter *boxer-window-right-margin* 50)

;; 2023-03-18 Removing unused boxer-abort-handler keydefs and defun
;((#\. :control :press) boxer-abort-handler)
;((#\g :control :press) boxer-abort-handler)

;; unused, see handlers in top level interface def
(defun boxer-abort-handler (w x y)
  (declare (ignore w x y))
  (format t "~&ABORT !!"))

;; 2023-03-18 Removing bits of expose-window-handlers that aren't used anymore, ideally because
;;            we've put all the repainting in correct locations in the pane callbacks so that
;;            things don't lock up anymore.

;; ?? is this called as a result of a display ?
;; might have to funcall through a symbol which changes
;; at the end of the graphics variable bootstrapping process
;; redisplay
;; x y wid hei define the invalidated region

(defvar *expose-window-handler-function* 'bootstrap-expose-window-function)

 ;;  :display-callback 'boxer-expose-window-handler
 (defun boxer-expose-window-handler (pane x y wid hei)
  (declare (ignore x y))
  (cond ((not (null *display-bootstrapping-no-boxes-yet*))
         ;(rendering-on (pane) (ogl-init wid hei))
         )
        ((null *suppress-expose-handler*)
         (opengl:rendering-on (pane) (ogl-init wid hei))  ;(ogl-reshape wid hei)
         (redraw-status-line)
         (boxer::repaint))
        (t nil)))

(defun bootstrap-expose-window-function (wid hei)
  (declare (ignore wid hei))
  ;; a stub
  nil)

(defun expose-window-function (wid hei)
  (declare (ignore wid hei))
;  (unless *suppress-expose-handler*
    (redraw-status-line)
    (boxer::repaint))
;)

;; 2022-06-13 This is actually coming from eval-command-loop.lisp, but historically was in
;; boxwin-opengl. We are finally retiring the old versions of these delayed mouse clicks.
;; This comment covers the removal of *double-click-pause-time*, *use-mouse2021*,
;; and maybe-unify-mouse-click.

(defvar *double-click-pause-time* 0.4 "Time to wait for a possible second click")

(defvar *use-mouse2021* t
  "Should we use the new 2021 Mouse clicks? This updates the mouse handling behavior in boxer to use
   true clicks with release, and adds events for mouse-down, mouse-up. This behavior may require some
   changes to legacy microworlds that have used mouse-click rather than mouse-down for dragging and
   other behaviors.")

;; pause and wait for another possible click
(defun maybe-unify-mouse-click (click)
  (let ((button (mouse-event-click click))
        (bits   (mouse-event-bits  click))
        (x-pos  (mouse-event-x-pos click))
        (y-pos  (mouse-event-y-pos click))
        ;(num-clicks (mouse-event-number-of-clicks click))
        ;(time  (mouse-event-last-time-stamp click))
        )
    #-win32 (declare (ignore button))
    #+lispworks (mp::process-wait-with-timeout "Double Click Wait" *double-click-pause-time*
                                   #'(lambda () (user-event-in-queue?)))
    (let ((new-input (peek-next-key-or-mouse-event)))
      (cond ((mouse-event? new-input)
             (cond ((and #+win32 (= (mod button 3) (mod (mouse-event-click new-input) 3))
                         ;; only need to button match for PC (3) button mouse
                         (= bits (mouse-event-bits new-input))
                         (boxer::<& (boxer::abs& (boxer::-& x-pos (mouse-event-x-pos
                                                                   new-input)))
                                    *double-click-wander*)
                         (boxer::<& (boxer::abs& (boxer::-& y-pos (mouse-event-y-pos
                                                                   new-input)))
                                    *double-click-wander*))
                    ;; looks like a double click
                    (cond ((> (mouse-event-number-of-clicks new-input) 1)
                           (handle-boxer-input (pop *boxer-eval-queue*)))
                          (t ;; looks like the event system recorded it as
                             ;; a pair of single clicks, throw out the second
                             ;; one and bash fields in the 1st one
                             (pop *boxer-eval-queue*)
                             (setf (mouse-event-click click)
                                   (+ (mouse-event-click click) 3)
                                   ;; if we allow wandering, should we use the pos
                                   ;; of the 1st or 2nd click
                                   (mouse-event-last-time-stamp click)
                                   (mouse-event-last-time-stamp new-input))
                             (incf (mouse-event-number-of-clicks click))
                             (handle-boxer-input click))))
                   (t (handle-boxer-input click))))
            (t (handle-boxer-input click))))))

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
;;;; FILE: client.lisp
;;;;
;;;; This is essentially the entire file, but was archived in bits and pieces so it may
;;;; not all be in the original order.
;;;;

;-*- mode:lisp; syntax:common-lisp;  package:boxnet -*-
#|

 $Header$

 $Log$

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


                    The Client side of the File system



Modification History (most recent at top)

10/18/05 filename-for-file: use enough-namestring ONLY for sorting
         directory structure, get the name and type from the filename
 8/31/05 in addition to filename, we dump out dirs,name,type in
         storage-chunk-plist-half-length & dump-storage-chunk-plist-items
 2/16/03 merged current LW and MCL file, no diffs, copyright updated
 2/12/01 merged with current MCL file
 9/02/99 fixed case in filename-for-file which was returning a pathname (which
         blew out the dumper) instead of a string
 5/25/99 storage-chunk-plist-half-length and dump-storage-chunk-plist-items
         changed to hack :file boxtop prop boxes with graphical boxtops
 9/23/98 no-inferiors-for-file? changed to make url case mirror file case
 9/23/98 Started Logging changes: source = boxer version 2.3beta+


|#

(in-package :boxnet)

;; sgithens: working around loading issue
(eval-when (compile load eval)
(defvar ID nil)
)

;;;; Utilities for server box info

(defun record-box-info (info
                        &optional (bid (sbi-bid info)) (box (sbi-box info)))
  (setf (gethash box *box-bid-table*) info)
  (let ((existing (assoc bid *bid-box-table* :test #'=)))
    (if (null existing)
        (push (cons bid info) *bid-box-table*)
        (setf (cdr existing) info)))
  info)

(defun box-server-info (box &optional (cons-info? t))
  (let ((existing-info (gethash box *box-bid-table*)) (id 0))
    (cond ((not (null existing-info))
           existing-info)
          ((and cons-info?
                (not (null (setq id (getprop box :server-box-id)))))
           ;; looks like there is no info only because the
           ;; box has not been loaded yet
           ;; so we make one
           (let ((new (get-box-info (get-server) id)))
             (setf (sbi-box new) box)
             new))
          (t nil))))

;;; We do not cons info's for BId's
(defun bid-server-info (bid)
  (cdr (assoc bid *bid-box-table* :test #'=)))

(defun mark-timestamp (box)
  (let ((info (box-server-info box)))
    (unless (null info)
      (setf (sbi-local-timestamp info) (slot-value box 'box::tick)))))

(defun refresh-box-bookkeeping (box info)
  (setf (sbi-write-date info) (get-box-write-date (sbi-bid info)))
  (mark-timestamp box))

(defun clear-bfs-state-tables ()
  (clrhash *box-bid-table*) (setq *bid-box-table* nil))


;;;; Box Info Accessors And Mutators

(defsbi-info-slot owner)
(defsbi-info-slot read-date)
(defsbi-info-slot write-date)
(defsbi-info-slot inferiors)
(defsbi-info-slot forwarding-table)
(defsbi-info-slot boxname)
(defsbi-info-slot superior-box-bid)
(defsbi-info-slot server)

(defun box-bid (box &optional (cons-info? nil))
  (let ((existing-info (box-server-info box cons-info?)))
    (if (null existing-info)
        (getprop box :server-box-id)
        (sbi-bid existing-info))))

(defun bid-box (bid)
  (let ((existing-info (bid-server-info bid)))
    (unless (null existing-info) (sbi-box existing-info))))



;;;; Dump-Hierarchical-Box-Prepass and friends...

;; returns the lowest superior box which is marked
;; as a storage chunk (or NIL)
(defun superior-storage-box (box &optional (skip-initial-box? nil))
  (do ((sb (if skip-initial-box? (superior-box box) box)
           (superior-box sb)))
      ((not (box?  sb)) nil)
    (when (storage-chunk? sb)
      (return sb))))

(defun superior-box-file-bid (box &optional (skip-initial-box? nil))
  (if (and (storage-chunk? box) (not skip-initial-box?))
      (let ((bid (box-bid box)))
        (if (null bid)
            (when (box? (superior-box box))
              (superior-box-file-bid (superior-box box)))
            bid))
      (when (box? (superior-box box))
        (superior-box-file-bid (superior-box box)))))

(defun get-current-superior-bid (box)
  (let ((sb (superior-box box)))
    (if (box? sb)
        (let ((info (box-server-info sb nil)))
          (if (null info)
              (get-current-superior-bid sb)
              (sbi-bid info)))
        (bsui-top-box-id *box-server-user*))))

;; forwarding tables
(defun find-bid (bid table)
  (do* ((remaining-pairs table (cddr remaining-pairs))
        (current-bid (car remaining-pairs) (car remaining-pairs)))
       ((null current-bid) nil)
    (when (= current-bid bid)
      (return (cadr remaining-pairs)))))

;;; returns either NIL or a new ID
;;; walks up the box hierarchy checking for an bid entry in any
;;; forwarding tables it encounters
(defun validate-server-box-id (box id)
  (cond ((not (box? box)) nil)
        (t (let* ((table (get-box-forwarding-table box))
                  (new-id (unless (null table) (find-bid id table))))
             (cond ((null new-id)
                    (validate-server-box-id (superior-box box) id))
                   (t new-id))))))



;;; Dumper/Loader utilities
;;; The next 3 functions have to be in agreement about when and what to
;;; dump.  They are used in the dumper.lisp file to control dumping of inferiors
;;; as well as what extra info needs to be dumped out so that the box can
;;; later be read in automatically
;;; There are currently three types of auto reading boxes (black boxes)
;;;    o Boxer Server Boxes: distinguished by numeric ID's in the :server-box-id
;;;      plist property for the most part, these are obsolete
;;;    o URL boxes: look for URL (Universal Resource Locator) Structs in the
;;;      plist property.
;;;    o File boxes: look for pathname (or string) in the :associated-file
;;;      property in the plist
;;;

;; the dumper walks through all the inferiors of the box being dumped.  If an
;; inferior box meets this test, then only its top level characteristics are
;; dumped but we do not recurse through the inferior box's inferiors
(defun no-inferiors-for-file? (box)
  (let ((plist (plist box)))
    (and (storage-chunk? box)
         ;; storage-chunk boxes are the granularity of delayed loading
         ;; we now test each of the three cases described above...
         ;; we could probably streamline the logic but at least this way,
         ;; the 3 cases are easily distinguished
         (or (and (getf plist :server-box-id)  ; box server test...
                  (in-bfs-environment?) (not (eq box *outermost-dumping-box*)))
             (and (getf plist :url)
                  (not (eq box *outermost-dumping-box*)))
             (and (getf plist :associated-file)
                  (not (eq box *outermost-dumping-box*)))))))
#|
(defun no-inferiors-for-file? (box)
  (and (storage-chunk? box)
       (or (not (eq box *outermost-dumping-box*))
           (getprop box :url))
       (or (in-bfs-environment?) (read-only-box? box))))
|#

;; sub box relocation table, branch file links, inf file links
(defun storage-chunk-plist-half-length (box)
  (let* ((half-length 0)
         (boxtop-prop (getprop box :boxtop))
         (boxtop (cond ((or (null boxtop-prop)
                            (eq boxtop-prop :standard)
                            (eq boxtop-prop :framed))
                        (boxer::boxtop box))
                       ((eq boxtop-prop :file)
                        (let ((bt (boxer::boxtop box)))
                          (when (boxer::graphics-sheet? bt) bt))))))
    (cond
     ((in-bfs-environment?)
      (when (and (storage-chunk? box)
                 (or (getprop box :server-box-id) (box-bid box)))
        (incf& half-length))
      ;; check for possible cross file link items
      ;; ... in contained-links
      (when (or (not (null (cross-file-contained-links box)))
                (some #'(lambda (link)
                          (and (cross-file-link? link)
                               (lowest-inferior-link? link box)))
                      (contained-links box)))
        (incf& half-length))
      ;; ... in branch links (bleagh, should iterate only once)
      (when (or (not (null (remove-if #'file-branch-link-broken
                                      (cross-file-port-branch-links box))))
                (some #'(lambda (link)
                          (and (eq (link-type link)
                                   'port-branch-link)
                               (cross-file-link? link)))
                      (branch-links box)))
        (incf& half-length))
      (when (or (not (null (remove-if #'file-branch-link-broken
                                      (cross-file-target-branch-links box))))
                (some #'(lambda (link)
                          (and (eq (link-type link)
                                   'target-branch-link)
                               (cross-file-link? link)))
                      (branch-links box)))
        (incf& half-length))
      ;; if we are dumping out as a black box, check for boxtop...
      (when boxtop (incf& half-length)))
     ((url-box? box) (incf& half-length) (when boxtop (incf& half-length)))
     ((boxer::file-box? box)
      ;(incf& half-length)
      ;; 8/31/05 in addition to filename, we dump out dirs,name,type
      (incf& half-length 4)
      (when boxtop (incf& half-length))))
    half-length))

(defmethod dump-storage-chunk-plist-items ((self boxer::box) stream)
  (let* ((boxtop-prop (getprop self :boxtop))
         (boxtop (cond ((or (null boxtop-prop)
                            (eq boxtop-prop :standard)
                            (eq boxtop-prop :framed))
                        (boxer::boxtop self))
                       ((eq boxtop-prop :file)
                        (let ((bt (boxer::boxtop self)))
                          (when (boxer::graphics-sheet? bt) bt))))))
    (cond
     ((in-bfs-environment?)
      (when (and (storage-chunk? self)
                 (or (getprop self :server-box-id)
                     (box-bid self)))
        (dump-boxer-thing :server-box-id stream)
        (dump-boxer-thing (or (getprop self :server-box-id)
                              (box-bid self))
                          stream))
      ;; some things, all boxes might have to deal with...
      ;; first check contained file links
      (let ((contained-links-to-dump (cross-file-contained-links self)))
        (dolist (cl (slot-value self 'contained-links))
          (when (and (cross-file-link? cl) (lowest-inferior-link? cl self))
            (push cl contained-links-to-dump)))
        (unless (null contained-links-to-dump)
          (debugging-message "~&Dumping cross file contained links ~A in box ~A"
                             contained-links-to-dump self)
          (dump-boxer-thing :cross-file-contained-links stream)
          (dump-list-preamble (length contained-links-to-dump) stream)
          (dolist (cl contained-links-to-dump)
            (if (%cross-file-link? contained-links-to-dump)
              (dump-boxer-thing (cross-file-link-id cl) stream)
              ;; must be an editor link
              (dump-boxer-thing (get-link-id cl) stream)))))
      ;; same sort of thing for branch links
      (let ((port-branch-links-to-dump (remove-if #'file-branch-link-broken
                                                  (cross-file-port-branch-links
                                                   self))))
        (dolist (bl (slot-value self 'branch-links))
          (when (and (eq (link-type bl) 'port-branch-link)
                     (not (eq (link-port bl) self))
                     (cross-file-link? bl))
            (push (get-link-id bl) port-branch-links-to-dump)))
        (unless (null port-branch-links-to-dump)
          (debugging-message
           "~&Dumping cross file Port Branch links ~A in box ~A"
           port-branch-links-to-dump self)
          (dump-boxer-thing :cross-file-port-branch-links stream)
          (dump-list-preamble (length port-branch-links-to-dump) stream)
          (dolist (pbl port-branch-links-to-dump)
            (if (%cross-file-link? pbl)
              (dump-boxer-thing (cross-file-link-id pbl) stream)
              (dump-boxer-thing pbl stream)))))
      (let ((target-branch-links-to-dump
             (remove-if #'file-branch-link-broken (cross-file-target-branch-links
                                                   self)))
            (terminating-target-links nil))
        (dolist (bl (slot-value self 'branch-links))
          (when (and (eq (link-type bl) 'target-branch-link)
                     (cross-file-link? bl))
            (if (eq self (link-target bl))
              ;; check for terminal link nodeness note that the port
              ;; case is handled in dump-cross-file-port-reference
              (push (get-link-id bl) terminating-target-links)
              (push (get-link-id bl) target-branch-links-to-dump))))
        (unless (null target-branch-links-to-dump)
          (debugging-message
           "~&Dumping cross file target branch links ~A in box ~A"
           target-branch-links-to-dump self)
          (dump-boxer-thing :cross-file-target-branch-links stream)
          (dump-list-preamble (length target-branch-links-to-dump) stream)
          (dolist (tbl target-branch-links-to-dump)
            (cond ((%cross-file-link? tbl)
                   (dump-boxer-thing (cross-file-link-id tbl) stream))
                  (t (dump-boxer-thing tbl stream)))))
        ;; process any terminating target links
        (unless (null terminating-target-links)
          (debugging-message "~&Dumping cross file link targets ~A in box ~A"
                             terminating-target-links self)
          (dump-boxer-thing :cross-file-target-ends stream)
          (dump-boxer-thing terminating-target-links stream)))
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     ((url-box? self)
      (dump-boxer-thing :url stream)
      (dump-box-url self stream)
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     ((boxer::file-box? self)
      (let* ((file (filename-for-file self))
             (dirs (pathname-directory file))
             (name (pathname-name      file))
             (type (or (pathname-type file) :unspecific)))
        (dump-boxer-thing :associated-file stream)
        (dump-boxer-thing file stream)
        ;; dump out the filename components as well for cross platform portability
        (dump-boxer-thing :associated-file-dirs stream)
        (dump-boxer-thing dirs stream)
        (dump-boxer-thing :associated-file-name stream)
        (dump-boxer-thing name stream)
        (dump-boxer-thing :associated-file-type stream)
        (dump-boxer-thing type stream))
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     )))

;; there are 4 case here, the relative filename flag can be on or off and
;; the filename can already be either a relative one or an absolute one
;; If the the filename matches the flag, we just return the filename otherwise
;; we'll have to do some work
(defmethod filename-for-file ((box boxer::box))
  (let* ((local-flag (boxer::relative-filename? box))
         (filename (getprop box :associated-file))
         (relative? (and filename (eq (car (pathname-directory filename))
                                      :relative))))
    (if (or (and relative? local-flag)
            (and (not relative?) (not local-flag)))
        (namestring filename) ; we have what we want
        (let* ((sup-file-box (boxer::current-file-box (superior-box box)))
               (sup-file (getprop sup-file-box :associated-file)))
          (cond ((null sup-file) ; save as absolute
                 (if relative?
                     (namestring (boxer::boxer-new-file-dialog
                                  :prompt
                                  "The Box has no superior filename to merge"
                                  :directory filename
                                  :box box))
                     (namestring filename)))
                ((and local-flag (not relative?))
                 ;; we want to save relative, but have an absolute
                 ;; use enough-namestring only for sorting directory
                 ;; structure otherwise type info can get lost
                 (namestring
                  (make-pathname :directory (pathname-directory
                                             (enough-namestring filename
                                                                sup-file))
                                 :name (pathname-name filename)
                                 :type (pathname-type filename))))
                (t ; must have relative, but want to save absolute
                 (namestring (merge-pathnames filename sup-file))))))))

(defmethod dump-cross-file-port-reference ((self boxer::port-box) stream)
  (let ((existing-id (gethash self *link-id-table*)))
    (cond ((null existing-id)
           (error "No existing cross file link id for ~A" self))
          (t
           (debugging-message "~&Dumping cross file port ~A, id:~D"
                              self existing-id)
           (dump-boxer-thing :cross-file-link-id stream)
           (dump-boxer-thing existing-id stream)))))

;;; Loader utilities

(defun load-server-box-id (box value)
  (unless (and (boundp '*loading-via-box-server?*)
               (null *loading-via-box-server?*))
    ;; should NOT record the server ID if we are READing the box in
    (let* ((trans (member value (current-forwarding-table) :test #'=))
           (bid (if (null trans) value (cadr trans))))
      (putprop box bid :server-box-id)
      (record-inferior-storage-chunk bid))))

(defun load-cross-file-contained-links (box value)
  (debugging-message "~&Loading Cross File contained-links ~A for ~A"
                     value box)
  (dolist (id value)
    (add-cross-file-contained-link box (%make-inferior-file-link :id id))))

(defun load-cross-file-port-branch-links (box value)
  (debugging-message "~&Loading Cross File port branch links ~A for ~A"
                     value box)
  (dolist (id value)
    (add-cross-file-port-branch-link
     box (%make-file-port-branch-link :id id :lowest box))))

(defun load-cross-file-target-branch-links (box value)
  (debugging-message "~&Loading Cross File target branch links ~A for ~A"
                     value box)
  (dolist (id value)
    (add-cross-file-target-branch-link
     box (%make-file-target-branch-link :id id :lowest box))))

(defun load-cross-file-target-ends (box value)
  (debugging-message "~&Loading Cross File targets ~A for ~A" value box)
  (dolist (id value)
    (add-cross-file-target-branch-link
     box (%make-file-target-branch-link :id id :box box))))

(defun load-cross-file-link-id (box value)
  (debugging-message "~&Loading Cross File port ~A" value)
  ;; also, record the ID with the port as key so we can reuse this
  ;; ID even after the link has been articulated
  (setf (gethash box *link-id-table*) value)
  (add-cross-file-port-branch-link
   box (%make-file-port-branch-link :id value :box box)))




;;; insert/delete-self support for branch links

(defun maybe-remove-file-links (inferior-link &optional top-box)
  (when (and (inferior-file-link-port-branch inferior-link)
             (file-branch-link-box (inferior-file-link-port-branch
                                    inferior-link))
             (inferior-file-link-target-branch inferior-link)
             (file-branch-link-box (inferior-file-link-target-branch
                                    inferior-link)))
    ;; if BOTH branches are fully articulated (both have
    ;; terminating boxes), then set the port target
    ;; and remove all file-links
    (debugging-message "~&Relinking Cross File Link ~D"
                       (cross-file-link-id inferior-link))
    (set-port-to-box (file-branch-link-box (inferior-file-link-port-branch
                                            inferior-link))
                     (file-branch-link-box (inferior-file-link-target-branch
                                    inferior-link)))
    (box::modified (file-branch-link-box (inferior-file-link-target-branch
                                          inferior-link)))
    ;; now remove file links from the port branch side
    (when (null top-box)
      ;; remember that at this point, we are sure to have both ends of the link
      (setq top-box (find-lowest-common-superior-box
                     (file-branch-link-box (inferior-file-link-port-branch
                                            inferior-link))
                     (file-branch-link-box (inferior-file-link-target-branch
                                            inferior-link)))))
    (do ((box (file-branch-link-box (inferior-file-link-port-branch
                                     inferior-link))
              (superior-box box)))
        ((or (not (box? box)) (eq box top-box)) )
      (delete-cross-file-port-branch-link box inferior-link))
    ;; now remove file links from the target branch side
    (do ((box (file-branch-link-box (inferior-file-link-target-branch
                                     inferior-link))
              (superior-box box)))
        ((or (not (box? box)) (eq box top-box)) )
      (delete-cross-file-target-branch-link box inferior-link))
    ;; and then the top
    (delete-cross-file-contained-link top-box inferior-link)))

;;; reads in boxes on the target side and then relinks them
;;; still needs some bullet proofing especially when faced
;;; with (possible) port crackness
(defun articulate-target-branch (plink)
  (let* ((il (file-branch-link-inferior-link plink))
         (tbl (inferior-file-link-target-branch il)))
    (do ((box (file-branch-link-lowest tbl)
              (file-branch-link-lowest tbl)))
        ((or (not (null (file-target-branch-link-box tbl)))
             (not (storage-chunk? (file-branch-link-lowest tbl))))
         ;;
         (maybe-remove-file-links il))
      (fill-box-from-server box))))

(defun relink-file-link (link)
  ;; need to calculate new common superior, then
  ;; (possibly) move the current inferior-file-link and
  ;; then adjust the branches to conform to the new
  ;; location of the inferior-file-link
  (let* ((pl (inferior-file-link-port-branch link))
         (tl (inferior-file-link-target-branch link))
         (pbox (when pl
                 (or (file-branch-link-box pl) (file-branch-link-lowest pl))))
         (tbox (when tl
                 (or (file-branch-link-box tl) (file-branch-link-lowest tl)))))
    (when (and pbox tbox
               (superior? pbox *initial-box*) (superior? tbox *initial-box*))
      ;; Both ends of the link are back in the hierarchy
      ;; walk upward to the new common superior, if there is
      ;; a broken link, unmark it, otherwise, add a link
      (let ((sup (find-lowest-common-superior-box pbox tbox)))
        (unless (null sup)
          (do* ((box pbox (superior-box box))
                (existing-link (link-match
                                link (cross-file-port-branch-links box))
                               (link-match
                                link (cross-file-port-branch-links box))))
               ((or (not (box? box)) (eq box sup)))
            (if (null existing-link)
                (add-cross-file-port-branch-link box pl)
                (setf (file-branch-link-broken existing-link) nil)))
          (do* ((box tbox (superior-box box))
                (existing-link (link-match
                                link (cross-file-target-branch-links box))
                               (link-match
                                link (cross-file-target-branch-links box))))
               ((or (not (box? box)) (eq box sup)))
            (if (null existing-link)
                (add-cross-file-target-branch-link box pl)
                (setf (file-branch-link-broken existing-link) nil)))
          (add-cross-file-contained-link sup link))))))


(defun remove-file-branch (from-box link &optional (port-branch? t))
  (do ((box from-box (superior-box box)))
      ((not (box? box)) )
    (let* ((blinks (if port-branch? (cross-file-port-branch-links box)
                       (cross-file-target-branch-links box)))
           (match? (link-match link blinks)))
      (if (null match?)
          (let*
              ;; check to see if there is a contained link we can remove
              ((cls (cross-file-contained-links box))
               (cl (link-match link cls)))
            (unless (null cl) (delete-cross-file-contained-link box cl))
            (return))
          (if port-branch?
              (delete-cross-file-port-branch-link   box link)
              (delete-cross-file-target-branch-link box link))))))

(defun break-file-links (from-box link &optional (port-branch? t))
  (do ((box from-box (superior-box box)))
      ((not (box? box)))
    (let* ((blinks (if port-branch? (cross-file-port-branch-links box)
                       (cross-file-target-branch-links box)))
           (match (link-match link blinks)))
      (if (null match)
          (return)
          (setf (file-branch-link-broken match) t)))))

(defmethod cross-file-link-insert-self-action ((self boxer::box) superior)
  (let ((sup-cl (cross-file-contained-links superior))
        (sup-pbl (cross-file-port-branch-links superior))
        (sup-tbl (cross-file-target-branch-links superior))
        (new-pbl (copy-seq (cross-file-port-branch-links superior)))
        (new-tbl (copy-seq (cross-file-target-branch-links superior))))
    (flet ((link-handler (link port-branch?)
             (cond ((not (null (file-branch-link-inferior-link link)))
                    ;; inserting a previously established link
                    (relink-file-link (file-branch-link-inferior-link link)))
                   (t
                    ;; must be the new insert case, usually occurring
                    ;; during READs of files
                    (let ((cl (link-match link sup-cl)))
                      (cond
                        ((not (null cl))
                         ;; made it to the top
                         (setf (file-branch-link-inferior-link link) cl)
                         (if port-branch?
                             (setf (inferior-file-link-port-branch cl) link)
                             (setf (inferior-file-link-target-branch cl)
                                   link))
                         (maybe-remove-file-links cl superior))
                        (t (let ((bl (link-match
                                      link (if port-branch? sup-pbl sup-tbl))))
                             (cond ((null bl)
                                    (warn "Don't know how to relink ~A" link))
                                   (t	; prefer the upper
                                    (unless (null(file-branch-link-box link))
                                      (setf (file-branch-link-box bl)
                                            (file-branch-link-box link)))
                                    (setf (file-branch-link-lowest bl)
                                          (file-branch-link-lowest link))
                                    (setf (file-branch-link-inferior-link link)
                                          (file-branch-link-inferior-link bl))
                                    (if port-branch?
                                        (setq new-pbl
                                              (nsubstitute link bl new-pbl))
                                        (setq new-tbl
                                              (nsubstitute
                                               link bl new-tbl)))
                                    ;; branch link may already be connected
                                    (unless (null
                                             (file-branch-link-inferior-link
                                              bl))
                                      (maybe-remove-file-links
                                       (file-branch-link-inferior-link
                                        bl)))))))))))))
      (dolist (bl (cross-file-port-branch-links self)) (link-handler bl t))
      (dolist (bl (cross-file-target-branch-links self)) (link-handler bl nil))
      (set-cross-file-port-branch-links   superior new-pbl)
      (set-cross-file-target-branch-links superior new-tbl))))

(defmethod cross-file-port-insert-self-action ((self boxer::box) superior)
  (let ((sup-cl (cross-file-contained-links superior))
        (sup-pbl (cross-file-port-branch-links superior))
        (link (car (cross-file-port-branch-links self))))
    (cond ((null link) )
          ((not (null (file-branch-link-inferior-link link)))
           (relink-file-link (file-branch-link-inferior-link link)))
          (t (let ((cl (link-match link sup-cl)))
               (cond ((not (null cl))
                      ;; made it to the top
                      (setf (file-branch-link-inferior-link link) cl)
                      (setf (inferior-file-link-port-branch cl) link)
                      (maybe-remove-file-links cl superior))
                     (t			; part of a branch
                      (let ((bl (link-match link sup-pbl)))
                        (cond ((null bl)
                               (warn "Unable to relink cross file  port" self))
                              (t
                               (setf (file-branch-link-box bl)
                                     (or (file-branch-link-box link) self))
                               (setf (file-branch-link-inferior-link link)
                                     (file-branch-link-inferior-link bl))
                               (delete-cross-file-port-branch-link superior
                                                                   bl)
                               (add-cross-file-port-branch-link superior link)
                               (unless (null
                                        (file-branch-link-inferior-link bl))
                                 (maybe-remove-file-links
                                  (file-branch-link-inferior-link
                                   bl)))))))))))))

;;; remove everything on this branch which is being deleted from the
;;; current box on up
;;;
;;; then mark as "broken" the lowest box in the other branch
;;; so that (possible) future inserts will have an existing
;;; link to connect to but immediate uses of FILE will not be
;;; confused and try to file this particular link
;;;
;;; superiors of the other branch should then be removed as well

(defmethod cross-file-link-delete-self-action ((self boxer::box) superior)
  (dolist (pbl (cross-file-port-branch-links self))
    (remove-file-branch superior pbl t)
    (let* ((il (file-branch-link-inferior-link pbl))
           (tl (when il (inferior-file-link-target-branch il))))
      (unless (null tl)
        (remove-file-branch (superior-box (file-branch-link-lowest tl)) tl nil)
        (break-file-links (file-branch-link-lowest tl) tl nil))))
  (dolist (tbl (cross-file-target-branch-links self))
    (remove-file-branch superior tbl nil)
    (let* ((il (file-branch-link-inferior-link tbl))
           (pl (when il (inferior-file-link-port-branch il))))
      (unless (null pl)
        (remove-file-branch (superior-box (file-branch-link-lowest pl)) pl t)
        (break-file-links (file-branch-link-lowest pl) pl t)))))

(defmethod cross-file-port-delete-self-action ((self boxer::box) superior)
  (let ((pbl (car (cross-file-port-branch-links self))))
    (unless (null pbl)
      (remove-file-branch superior pbl t)
      (let* ((il (file-branch-link-inferior-link pbl))
             (tl (when il (inferior-file-link-target-branch il))))
        ;; and the top part of the target link if it exists
        (unless (null tl)
          (remove-file-branch (superior-box (file-branch-link-lowest tl))
                              tl nil)
          (break-file-links (file-branch-link-lowest tl) tl nil))))))



;;; cross file port utilities

(defun unique-cross-file-link-id ()
  (prog1 *cross-file-link-id-counter*
    (incf *cross-file-link-id-counter*)))

;;; note that ports are unique to each link whereas targets can
;;; have many links
(defun get-link-id (link)
  (let ((existing-entry (gethash (link-port link) *link-id-table*)))
    (cond ((null existing-entry)
           (let ((new-id (unique-cross-file-link-id)))
             (setf (gethash (link-port link) *link-id-table*) new-id)
             new-id))
          (t existing-entry))))

;;; this attribute needs to be saved away in the top level
;;; world (during logout) and restored on login to insure
;;; continuing uniqueness
(defun initialize-cross-file-link-id (id)
  (setq *cross-file-link-id-counter* id))

(eval-when (eval)
(deffile-property-handler :max-cross-file-link-id id
  (debugging-message "Initializing Cross File Link Counter to ~A" id)
  (initialize-cross-file-link-id id))
)

;;; print functions
(defun  %print-file-port-branch-link (obj stream &rest ignore)
  (declare (ignore ignore))
  (format stream "#<File Port Link ~D (~A)"
          (cross-file-link-id obj) (file-branch-link-box obj)))

(defun  %print-file-target-branch-link (obj stream &rest ignore)
  (declare (ignore ignore))
  (format stream "#<File Target Link ~D (~A)"
          (cross-file-link-id obj) (file-branch-link-box obj)))

;;; this gets called at each level.  If it looks like speed is
;;; a problem, we can hash the results of the tree walk into a
;;; per transaction table to speed the search
(defun lowest-inferior-link? (inflink box)
  (eq box (find-lowest-common-superior-box (box::link-port inflink)
                                           (box::link-target inflink))))

(defun cross-file-link? (blink)
  (not (eq (superior-storage-box (link-port blink))
           (superior-storage-box (link-target blink)))))

(defun link-match (link link-list)
  (car (member link link-list :test #'file-link-=)))

(defun file-link-= (l1 l2)
  (when (and (%cross-file-link? l1) (%cross-file-link? l2))
    (= (cross-file-link-id l1) (cross-file-link-id l2))))

;;; accessors and mutators
(defun cross-file-contained-links (box)
  (unless (null box) (getprop box :cross-file-contained-links)))

(defun cross-file-port-branch-links (box)
  (unless (null box) (getprop box :cross-file-port-branch-links)))

(defun cross-file-target-branch-links (box)
  (unless (null box) (getprop box :cross-file-target-branch-links)))

(defun no-cross-file-links? (box)
  (let ((plist (box::plist box)))
    (and (null (getf plist :cross-file-contained-links))
         (null (getf plist :cross-file-port-branch-links))
         (null (getf plist :cross-file-target-branch-links)))))


(defun set-cross-file-contained-links (box newlinks)
  (unless (null box)
    (if (null newlinks)
        (removeprop box :cross-file-contained-links)
        (putprop box newlinks :cross-file-contained-links))))

(defun set-cross-file-port-branch-links (box newlinks)
  (unless (null box)
    (if (null newlinks)
        (removeprop box :cross-file-port-branch-links)
        (putprop box newlinks :cross-file-port-branch-links))))

(defun set-cross-file-target-branch-links (box newlinks)
  (unless (null box)
    (if (null newlinks)
        (removeprop box :cross-file-target-branch-links)
        (putprop box newlinks :cross-file-target-branch-links))))


(defun add-cross-file-contained-link (box link)
  (unless (null box)
    (let ((cfcl (getprop box :cross-file-contained-links)))
      (unless (member link cfcl :test #'file-link-=)
        (putprop box (nconc cfcl (list link)) :cross-file-contained-links)))))

(defun add-cross-file-port-branch-link (box link)
  (unless (null box)
    (let ((cfpbl (getprop box :cross-file-port-branch-links)))
      (unless (member link cfpbl :test #'file-link-=)
        (putprop box (nconc cfpbl (list link)) :cross-file-port-branch-links)))))

(defun add-cross-file-target-branch-link (box link)
  (unless (null box)
    (let ((cftbl (getprop box :cross-file-target-branch-links)))
      (unless (member link cftbl :test #'file-link-=)
        (putprop box (nconc cftbl (list link)) :cross-file-target-branch-links)))))


(defun delete-cross-file-contained-link (box link)
  (unless (null box)
    (let ((new (delete (cross-file-link-id link)
                       (getprop box :cross-file-contained-links)
                       :test #'(lambda (id ln)
                                 (= id (cross-file-link-id ln))))))
      (if (null new)
          (removeprop box :cross-file-contained-links)
          (putprop box new :cross-file-contained-links)))))

(defun delete-cross-file-port-branch-link (box link)
  (unless (null box)
    (let ((new (delete (cross-file-link-id link)
                       (getprop box :cross-file-port-branch-links)
                       :test #'(lambda (id ln)
                                 (= id (cross-file-link-id ln))))))
      (if (null new)
          (removeprop box :cross-file-port-branch-links)
          (putprop box new :cross-file-port-branch-links)))))

(defun delete-cross-file-target-branch-link (box link)
  (unless (null box)
    (let ((new (delete (cross-file-link-id link)
                       (getprop box :cross-file-target-branch-links)
                       :test #'(lambda (id ln)
                                 (= id (cross-file-link-id ln))))))
      (if (null new)
          (removeprop box :cross-file-target-branch-links)
          (putprop box new :cross-file-target-branch-links)))))


;; a port is a cross file port if the port and the target have
;; are inside (or in the case of the target, is) different file
;; boxes.

;; just walk up the hierarchy until we get to the containing
;; storage-chunk box, then check to see if the port is on a
;; branch-link or an inferior-link
(defun cross-file-port? (port-box)
  (flet ((in-links? (links)
           (member port-box links
                   :test #'(lambda (item list-item)
                             (eq item (box::link-port list-item))))))
    (do ((sb (superior-box port-box) (superior-box sb)))
        ((not (box? sb)) nil)
      (when (storage-chunk? sb)
        (cond ((in-links? (slot-value sb 'box::branch-links))
               (return T))
              ((in-links? (slot-value sb 'box::contained-links))
               (return NIL)))))))


;;; Allocates an info struct for a NEW file box.
;;; Fills whatever slots it can an then returns it.
;;; NOTE: It does NOT write through to the server.  It assumes that
;;; the caller will write out the slots to the server in some
;;; bundled way.  Usually with Set-Box-And-Info

(defun allocate-new-box-info (box)
  (let ((new-info (make-server-box-info (or (box::getprop box :server-box-id)
					    (get-new-bid (get-server)))
					box))
	(new-sup-bid (get-current-superior-bid box)))
    (setf (sbi-superior-box-bid new-info) new-sup-bid)
    (record-box-info new-info)
    new-info))


;;; Loading boxes from connections
;; sgithens (break "This must not be called... it's a duplicate but in the boxnet package.")
(defun load-binary-box-internal (bid &optional return-box)
  (bw::with-mouse-cursor (:file-io)
    (with-server (server)
      (let ((box::*current-file-length* (get-box-size server bid)))
	(unwind-protect
	     (let ((box::*status-line-loading-format-string*
		    (format nil "Loading Box ID ~D  ~~D (~~D %)" bid)))
	       (unless (null boxer::*file-system-verbosity*)
		 (box::status-line-display
		  'box::loading-box
		  (format nil "Loading ~D bytes from ~A for Box ~D"
			  box::*current-file-length*
			  (server-loading-string server)
			  bid)))
	       (multiple-value-bind (file-box info)
		   (get-box-and-info server bid
				     (and return-box
					  (box-server-info return-box nil)))
		 (if (null return-box)
		     (setq return-box file-box)
		     (box::initialize-box-from-box return-box file-box))
		 ;; with the box, we can finish setting the slots in the info
		 (setf (sbi-box info) return-box)
		 (record-box-info info bid return-box)
		 return-box))
	  ;; Cleanup Forms
	  (box::status-line-undisplay 'connection-error)
	  (unless (null boxer::*file-system-verbosity*)
	    (box::status-line-undisplay 'box::loading-box)))))))

#|
;; this is called on a Port Target Translation Table prior to
;; dumping the table out.  It is (currently) the only way in
;; which invalid PTTT entries can be removed

(defun prune-port-target-translation-table (table owning-box)
  (let ((new-table table)
	(pruned? nil))
    (do-pttt-entries (trans table)
      (let ((box (pttt-target trans)))
	(when (not (eq owning-box (superior-storage-box box)))
	  (setq pruned? t)
	  (setq new-table (fast-delq trans new-table)))))
    (values new-table pruned?)))

(defun check-port-target-table (port-box &optional
					 (target (box::ports port-box))
					 (superior-storage-box
					  (superior-storage-box target)))
  (unless (or (null superior-storage-box)
	      (eq superior-storage-box target))
    (let ((table (get-port-target-translation-table superior-storage-box)))
      (cond ((null table)
	     (set-port-target-translation-table superior-storage-box
						(make-pttt-table
						 (get-pttt-uid) target)))
	    ((not (null (find-target-uid target table))))
	    (t
	     (set-port-target-translation-table superior-storage-box
						(append-pttt-values
						 table
						 (get-pttt-uid)
						 target)))))))

;;; the existing arg means to use whatever pttt-table is already there
;;; a NIL existing arg means to calculate the current table
(defun cross-file-port-target-reference (port-box &optional (existing nil))
  (let* ((target (box::ports port-box))
	 (superior-storage-box (superior-storage-box target))
	 (port-uid 0))
    ;; A port UID of 0 means that the last box in
    ;; the BID chain is a the target
    (unless (eq target superior-storage-box)
      ;; get the port UID, make sure a table is there
      (let ((table (if existing
		       (get-port-target-translation-table superior-storage-box)
		       (make-pttt-table superior-storage-box))))
	(setq port-uid (find-target-uid target table))))
    (unless (null port-uid)
      (let ((return-reference (list port-uid)))
	;; now walk upward Pushing successive BID's
	(setq return-reference (nconc return-reference (bid-path target)))
	return-reference))))

(defun bid-path (target)
  (unless (or (null target) (not (box::superior? target box::*initial-box*)))
    ;; make sure we are part of the hierarchy, then walk upwards, checking
    (do* ((storage-chunk (superior-storage-box target)
			 (superior-storage-box storage-chunk T))
	  (next-bid (box-bid storage-chunk) (box-bid storage-chunk))
	  (return-path nil))
	 ((null storage-chunk) return-path)
      (cond ((null next-bid)
	     (let ((new-bid (get-new-bid (get-server))))
	       (box::putprop storage-chunk new-bid :server-box-id)
	       (push new-bid return-path)))
	    (t (push next-bid return-path))))))

;;; This is only called if time stamps would otherwise not dump a box.
;;; this checks to see if the dumped out link info is still valid.
;;; if it is, then we don't have to dump out the inferiors.  If the link
;;; info HAS changed (e.g. some intermediate file boxes have either
;;; been filed or unfiled -- changing the BID path), then this is
;;; supposed to detect it
(defun obsolete-link-info? (box)
  (let ((checked-pttts nil))
    (dolist (bl (box::branch-links box))
      (case (box::link-type bl)
	(box::port-branch-link
	 (let ((existing-ref (box::getprop (box::link-port bl)
					   :cross-file-port-target)))
	   ;; compare any dumped out info with what it should be
	   (when (or (null existing-ref)
		     ;; there is no saved out info OR
		     ;; the saved out info is not accurate
		     (not (equal existing-ref
				 (cross-file-port-target-reference
				  (box::link-port bl)))))
	     (return t))))
	(box::target-branch-link
	 (let ((sup (superior-storage-box (box::link-target bl))))
	   (unless (box::fast-memq sup checked-pttts)
	     (when (check-pttt-table sup) (return t))
	     (push sup checked-pttts))))))))

|#


;;; maps through the entire box returning several values
;;;  . A list of inferior file boxes which need to be dumped
;;;  . A list of inferior file boxes which are Read Only and won't be dumped
;;;  . A list of inferior file boxes which need to be copied
;;;  . A list of inferior file boxes whose superiors need to be set
;;;  . A list of cross file ports, in the process, updating the
;;;    Port Target Translation Tables to include the targets of
;;;    the cross file ports

(defun dump-hierarchical-box-prepass (box)
  (declare (values rw-inferiors ro-inferiors
		   new-copies new-superiors cross-file-ports))
  (let ((rw-inferiors nil)  ; RW boxes which need to be saved
	(ro-inferiors nil)  ; RO boxes which need to be saved (originals)
	(new-copies nil)
	(new-superiors nil)
	(cross-file-ports nil))
    (flet ((recursively-append-values (b)
	     (multiple-value-bind (new-rw new-ro new-new-copies
					  new-new-sups new-cross-file-ports)
		 (dump-hierarchical-box-prepass b)
	       (setq rw-inferiors (append rw-inferiors new-rw)
		     ro-inferiors (append ro-inferiors new-ro)
		     new-copies   (append new-copies new-new-copies)
		     new-superiors(append new-superiors new-new-sups)
		     cross-file-ports (append cross-file-ports
					      new-cross-file-ports)))))
      (boxer::do-box-rows ((row box))
	(boxer::do-row-chas ((b row))
	  (unless (boxer::cha? b)
	    ;; must be a box
	    (let ((flags (slot-value b 'boxer::flags)))
	      (cond
		((box::box-flag-storage-chunk? flags)
		 (let ((info (box-server-info b)))
		   (cond
		     ((box::box-flag-read-only-box? flags)
		      (cond
			((null info)
			 ;; this means its a brand new box, a copy
			 ;; of a foreign box, or a copy of a
			 ;; native box
			 (cond ((box::box-flag-copy-file? flags)
				(let* ((orig-bid
					(boxer::getprop b :copy-box-id))
				       (oinfo (bid-server-info orig-bid)))
				  (when (and (not (null oinfo))
					     (string-equal
					      (sbi-owner oinfo)
					      (bsui-username
					       *box-server-user*)))
				    ;; must be a local copy
				    (push b new-superiors)
				    ;; should local copies be
				    ;; written out separately ??
				    ))
				(setf (boxer::copy-file? b) nil))
			       (t
				;; must be a local original
				(push b ro-inferiors) (push b new-superiors))))
			    ((string-equal (sbi-owner info)
					   (bsui-username *box-server-user*))
			     ;; there is an info and its a native box
			     ;; (we do nothing about foreign RO boxes)
			     ;; check for superior file box diffs
			     (unless (= (sbi-superior-box-bid info)
					(get-current-superior-bid b))
			       (push b new-superiors)))))
		     ((box::box-flag-copy-file? flags)
		      ;; if the box is a copy, but there is some
		      ;; inferior structure, then it was probably
		      ;; read in already and we want to save out
		      ;; any modifications rather than copying the
		      ;; original
		      (if (null (slot-value b 'box::first-inferior-row))
			  (push b new-copies)
			  (progn
			    (setf (boxer::copy-file? b) nil)
					;(box::removeprop b :server-box-id)
			    (push b new-superiors)
			    (push b rw-inferiors))))
		     ((and (not (null (slot-value b 'box::first-inferior-row)))
			   (or (null info)
			       (> (slot-value b 'box::tick)
				  (sbi-local-timestamp info))))
		      ;; only add it to the inferiors list if
		      ;; it has changed
		      (push b rw-inferiors)))))
		((and (box::port-box? b) (cross-file-port? b))
		 (push b cross-file-ports))
		(t
		 ;; otherwise, recurse
		 (recursively-append-values b))))))))
    ;; New Superiors checking...
    ;; now check for any new-superiors in the rw-inferiors list
    (dolist (rwb rw-inferiors)
      (let ((box-info (box-server-info rwb)))
	(if (null box-info)
	    (push rwb new-superiors)
	    ;; now check if the superior has changed...
	    (unless (= (sbi-superior-box-bid box-info)
		       (get-current-superior-bid rwb))
	      (push rwb new-superiors)))))
    ;; now search for cross file ports
    (values rw-inferiors ro-inferiors
	    new-copies new-superiors cross-file-ports)))

(defun copy-server-box-id (box old-id superior-bid)
  (let ((info (copy-bid-box (get-server box) old-id superior-bid)))
    (setf (sbi-box info) box)
    (record-box-info info)
    ;; adjust the flags and Plist of the copy
    (setf (box::copy-file? box) nil)
    (box::removeprop box :copy-box-id)
    (box::putprop box (sbi-bid info) :server-box-id)))

;; this should only be called during logout
(defun do-server-box-deletions ()
  ;; go through the kill-buffer in case there are any storage chunks
  ;; waiting to be deallocated in the kill-buffer
  (dolist (kill box::*kill-buffer*)
    (box::queue-editor-objs-for-deallocation kill)
    (setq box::*kill-buffer* (make-list 8)))
  ;; first, make sure the editor object deallocation queue is empty
  (box::deallocate-editor-objs)
  (debugging-message "Deleting BID's ~A from the server" *box-bid-delete-list*)
  (dolist (box *box-bid-delete-list* (setq *box-bid-delete-list* nil))
    (let* ((box-info (box-server-info box nil))
	   (bid (if (null box-info) (box::getprop box :server-box-id)
		    (sbi-bid box-info)))
	   (sup-bid (or (superior-box-file-bid box t)
			(get-bid-superior-box-bid bid))))
      (cond ((and (box::box? box) (box::superior? box box::*initial-box*))
	     (warn "Tried to Delete BId ~D (~X) when Box ~A is still active"
		   bid bid box))
	    ((zerop sup-bid))
	    (t
	     (set-bid-inferiors sup-bid
				(box::fast-delq bid
						(get-bid-inferiors sup-bid)))
	     (delete-bid-box (get-server) bid))))))

(defun dump-hierarchical-box-internal (box &optional (bid (box-bid box))
					   file-attribute-list)
  (unless (getf file-attribute-list :package)
    (setf (getf file-attribute-list :package) ':boxer))
  (multiple-value-bind (rw-inferiors ro-inferiors new-copies
				     new-superiors cross-file-ports)
      (dump-hierarchical-box-prepass box)
    ;; talk about what we gots to do
    (debugging-message "~D Read/Write boxes ~A~%~
                          ~D Read Only boxes ~A~%~
                          ~D New Copies ~A~%~
                          ~D Superior Pointer need to be changed ~A~%~
                          ~D Cross File Ports ~A"
		       (length rw-inferiors) rw-inferiors
		       (length ro-inferiors) ro-inferiors
		       (length new-copies) new-copies
		       (length new-superiors) new-superiors
		       (length cross-file-ports) cross-file-ports)
    ;; first create any copies (move copying behind set-superior ?)
    (dolist (copy new-copies)
      (copy-server-box-id copy (box::getprop copy :copy-box-id)
			  (get-current-superior-bid copy)))
    ;; finally dump the top level box and any inferiors
    (debugging-message "Dumping Hierachical Box ~A to ~D" box bid)
    ;; now set/reset any superiors first because locking in the server
    ;; depends on an up to date version of hierarchical relationships
    (dolist (inf new-superiors)
      (let* ((sup-info (box-server-info box nil))
	     (existing-info (box-server-info inf))
	     (existing-bid (box-bid inf))
	     (old-sup-bid (unless (null existing-info)
			    (sbi-superior-box-bid existing-info))))
	(cond ((null existing-info) (allocate-new-box-info inf))
	      (t
	       (unless (or (null sup-info)
			   (member existing-bid (sbi-inferiors sup-info)
				   :test #'=))
		 (push existing-bid (sbi-inferiors sup-info)))
	       (unless (or (null old-sup-bid) ; no superior ?
			   ;; The superior is the top level box
			   (and (numberp old-sup-bid) (zerop old-sup-bid))
			   ;; The superior is THIS box, so the info will
			   ;; get written out when the box and info is
			   ;; dumped out at the end of this function
			   (= old-sup-bid bid))
		 (let ((old-infs (get-bid-inferiors old-sup-bid)))
		   (when (member existing-bid old-infs :test #'=)
		     ;; try and avoid a write to the server if we can
		     (let ((new-infs (delete existing-bid old-infs :test #'=)))
		       (set-bid-inferiors old-sup-bid new-infs)))))
	       ;; now the info caches for both the old and new sups are correct
	       ;; NOTE: the updated inferiors for the NEW (current) superior
	       ;; will be sent out to the server at the end of this function
	       ;; the OLD superior will have to be explicitly updated
	       ;; who knows WHERE that box has been....
	       ;;
	       ;; now update the superior
	       (if (box::fast-memq inf rw-inferiors)
		   ;; if the box is in the rw-inferiors, we can avoid
		   ;; the write to the server and just update the info
		   (setf (sbi-superior-box-bid existing-info) bid)
		   (set-bid-superior-box-bid (sbi-bid existing-info) bid))))))
    ;; we dump the inferiors before the top level box because some
    ;; of them may need to generate boxID's which should be recorded
    ;; before the higher level box is dumped out
    (dolist (inf rw-inferiors)
      (let* ((info (or (box-server-info inf) (allocate-new-box-info inf)))
	     (bid (sbi-bid info)))
	;; maybe this should be a recursive call to dump-hierarchial-box ?
	(debugging-message "Dumping Inferior Box ~A to ~D"
			   inf (box-bid inf))
	(dump-hierarchical-box-internal inf bid file-attribute-list)))
    (let ((info (box-server-info box nil)))
      (when (null (box::getprop box :server-box-id))
	;; the box may have been marked for storage but not saved out yet
	(box::putprop box bid :server-box-id))
      (with-bfs-standard-dumping-environment
	  (set-box-and-info (get-server) bid box info))
      ;; finally, update info about the box
      (refresh-box-bookkeeping box info))))

(defun dump-hierarchical-box (box &optional
				  (bid (box-bid box)) file-attribute-list)
  (bw::with-mouse-cursor (:file-io)
    (dump-hierarchical-box-internal box bid file-attribute-list)))



(defun fake-file-superior? (info)
  (let* ((sup-bid (unless (null info) (sbi-superior-box-bid info)))
         (sup-file-box (unless (null sup-bid) (bid-box sup-bid))))
    (unless (null sup-file-box)
      (boxer::fake-file-box sup-file-box))))

(defun queue-for-server-deletion (box)
  (let* ((info (box-server-info box nil))
         (bid (if (null info)
                  (box::getprop box :server-box-id)
                  (sbi-bid info))))
    (cond ((null bid))
          ((box::superior? box box::*initial-box*)
           (warn "~A is still connected to the hierarchy" box))
          ((and info
                ;; Watch out for inferiors of Read Only and Foreign Boxes
                (or (and (foreign-world-server? (sbi-server info))
                         (fake-file-superior? box))
                    (not (writeable-server? (sbi-server info)))
                    ;; if any superior is read-only, then don't delete
                    (do ((fbox box
                               ;; can't walk up box structure since it has
                               ;; already been severed
                               (let* ((info (box-server-info fbox))
                                      (sup-bid (unless (null info)
                                                 (sbi-superior-box-bid info))))
                                 (and sup-bid (bid-box sup-bid)))))
                        ((not (box? fbox)) nil)
                      (when (read-only-box? fbox) (return T))))))
          ((box::fast-memq box *box-bid-delete-list*))
          ((numberp bid)
           (unless (null box::*boxer-system-hacker*)
             (format t "~%Queueing ~A (~D, #x~X)" box bid bid))
           (push box *box-bid-delete-list*))
          (t (error "BID, ~A, is not a number" bid)))))

(defun update-file-control-interface (interface-symbol box value)
  (let ((int-box (boxer-eval::lookup-static-variable-in-box-only box
							   interface-symbol)))
    (when (box::box? int-box)
      ;; if there is an interface box then set it to the new value
      (box::bash-box-to-single-value int-box value))))

;;; Converting from old file operations
(defvar *convert-file-boxes-to-storage-chunks* t)

;; this is called after someone reads in a box in the old style
;; it marks the box as a Read/Write Storage-Chunk and allocates
;; a boxID for it
(defun read-box-postamble (box)
  (when (and *convert-file-boxes-to-storage-chunks* (box::box? box))
    (install-file-control-boxes box)
    (setf (box::storage-chunk? box) t
	  (read-only-box? box) nil)))

;; a stub for conversion from the old READ/SAVE file system
(defun save-box-postamble (box)
  (declare (ignore box))
  )

(defun storage-info-for-make-box (box info)
  (cond ((null info)
	 (let ((id (box::getprop box :server-box-id)))
	   (if (null id)
	       '(("Box is marked for Storage"))
	       `(("Box is marked for Storage")
		 (,(format nil "Box ID: ~D (#x~x)" id id))))))
	(t
	 (list
	  (if (null (sbi-write-date info))
	      '("No version of the box exists in long term storage")
	      (list (multiple-value-bind (sec min hour date month year)
			(decode-universal-time (sbi-write-date info)
					       boxer::*time-zone*)
		      (format nil "Last Written on ~D:~D:~D ~D/~D/~D"
			      hour min sec month date year))))
	  (list (format nil "Box ID: ~D (#x~x)" (sbi-bid info) (sbi-bid info)))
	  (if (load-box-on-login? box)
	      '("The box will be loaded on login")
	      '("The box will not be loaded on login"))
	  (if (read-only-box? box)
	      '("Changes made to this box will not be saved")
	      '("Changes made to this box will be saved"))))))

;; should update the file-inferiors and possibly promote the
;; cross file port target tables of the superior storage-chunk

(defmethod storage-chunk-delete-self-action ((self box::box))
  )

(defmethod storage-chunk-insert-self-action ((self box::box))
  )


;;;; Box Server Primary Interface Functions
;;;; to be used by Primitives and Editor Commands

(defun mark-box-flags-as-file (box &optional
			     (load-on-login? nil)
			     (read-only? nil))
  (setf (box::storage-chunk? box) t
	;; setup some defaults...
	(box::load-box-on-login? box) load-on-login?
	(read-only-box? box) read-only?))


(defun unmark-box-flags-as-file (box)
  (setf (box::storage-chunk? box) nil
	(box::load-box-on-login? box) nil
	(read-only-box? box) nil
	(box::copy-file? box) nil))

(defun make-file-control-box (name current-value update-fn)
  (let ((trigger (box::make-box `((,update-fn))
				'box::doit-box
				"Modified-Trigger"))
	(interface-box (box::make-box `((,current-value))
				      'box::data-box
				      name)))
    (box::add-closet-row interface-box (box::make-row `(,trigger)))
    interface-box))

(defun install-file-control-boxes (box)
  (let ((closet (box::closet-row box nil)))
    (unless (boxer-eval::lookup-static-variable-in-box-only box 'bu::load-on-login?)
      (box::append-cha closet
		       (make-file-control-box "Load-on-Login?"
					      (if (box::load-box-on-login? box)
						  "True" "False")
					      "Update-Login-Action")))
    (unless (boxer-eval::lookup-static-variable-in-box-only box 'bu::save-changes?)
      (box::append-cha closet
		       (make-file-control-box "Save-Changes?"
					      (if (read-only-box? box)
						  "False" "True")
					      "Update-Save-Changes")))
    box))

(defun remove-file-control-boxes (box)
  (declare (ignore box))
  )

;; sgithens 2022-08-30 I don't think this is ever actually called...
(defun file-box (box)
  (let* ((info (or (box-server-info box nil) ; for boxes which have been filed
		   (allocate-new-box-info box)))
	 (bid (sbi-bid info)))
    (when (not (box::storage-chunk? box))
      ;; if the box is not already a storage chunk, make it one
      (install-file-control-boxes box)
      (mark-box-flags-as-file box))
    (when (null (box::getprop box :server-box-id))
      ;; the box may have been marked for storage but not saved out yet
      (box::putprop box bid :server-box-id)
      ;; If this is a new file box, we also need to save out the
      ;; superior file box first in order to insure that there is
      ;; a pointer to the new file box.  Otherwise a crash would
      ;; orphan the new file box.  Also, the superior storage box
      ;; may also be new (and therefore needs to be filed)
      (let ((sup-box (superior-storage-box box t)))
	(unless (null sup-box)
	  (file-box sup-box)
	  ;; make sure the superior's infs include the box we are filing
	  ;; since the superior has already been filed, we know that
	  ;; there should be an info
	  (let ((sup-info (box-server-info sup-box)))
	    (unless (member bid (sbi-inferiors sup-info) :test #'=)
	      (push bid (sbi-inferiors sup-info))
	      (set-bid-inferiors (sbi-bid sup-info)
				 (sbi-inferiors sup-info)))))))
    (bw::with-mouse-cursor (:file-io)
      (with-bfs-standard-dumping-environment
	  (set-box-and-info (get-server) bid box info)))
    ;; finally, update the write date and other info
    (refresh-box-bookkeeping box info)))

;;; this must read in the box if the immediate inferiors are not
;;; there yet...
(defun unfile-box (box)
  ;; read in the inferiors
  (when (and (box::storage-chunk? box)
	     (null (slot-value box 'box::first-inferior-row)))
    (fill-box-from-server box))
  ;; now unmark the box
  (remove-file-control-boxes box)
  (unmark-box-flags-as-file box)
  ;; and queue for deletion from the server
  (queue-for-server-deletion box))

;;; Editor Interface

;; NOTE: server errors can be signalled from inside fill-box-from-server
;; to be safe, callers must either wrap it in side a with-server-errors
;; or else be underneath a primitive
(defmethod fill-box-from-bfs-server ((self box::box))
  (when (box::getprop self :server-box-id)
    (let* ((id (box::getprop self :server-box-id))
           (new-id (validate-server-box-id self id)))
      (unless (null new-id)
        (setq id new-id))
      (load-binary-box-internal id self)
      ;; need to patch up name row to avoid re-insertion of name/box into
      ;; the superior binding alist
      (when (box::name-row? (slot-value self 'box::name))
        (setf (slot-value (slot-value self 'box::name) 'box::cached-name)
              (box::get-box-name (slot-value self 'box::name))))
      (box::modified self)
      (mark-timestamp self))))

(defun record-copy-file-info (from-box to-box)
  (let ((old-file-id (boxer::getprop from-box :server-box-id)))
    (unless (null old-file-id)
      (boxer::putprop to-box old-file-id :copy-box-id))))

;;;;
;;;; FILE: clientmacros.lisp
;;;;
;;;; --entire-file--

;-*- mode:lisp; syntax:common-lisp;  package:boxnet -*-
#|

 $Header$

 $Log$

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


 Macro Definitions and Variable Declarations for the
 Client side of the File system


Modification History (most recent at top)

 2/16/03 merged current LW and MCL files, no diffs, copyright updated

|#

(in-package :boxnet)



;;;; Important Variables

(defvar *dump-out-file-box-inferiors?* t)

(defvar *box-bid-delete-list* nil)

(defvar *loading-via-box-server?* nil)

(defvar *during-login-action?* nil
  "Bound to T during the loading of the top level box")

(defvar *login-postpass-list* nil)



;;;; Debugging Support

(defvar *include-debugging-code* t)

(defvar *debug-box-client?* nil)

(defun client-debug () (setq *debug-box-client?* t))

(defun client-nodebug () (setq *debug-box-client?* nil))

(defun client-single-step-mode () (setq *debug-box-client?* :single-step))

(defvar *client-debug-stream* *trace-output*)

;;; Logging variables
(defvar *opcode-logging-enabled?* nil)
(defvar *log-file* "/tmp/boxer-client.log")

;;; The Opcodes...

(defvar *server-opcode-names* (make-array 255 :initial-element nil))

(defvar *opcode-debugging-format-string* "Sending Opcode ~D (~A)")

(defun print-debugging-message-internal (format-string &rest format-args)
  #+lucid (declare (lcl::dynamic-extent format-args))
  (when *debug-box-client?*
    (apply #'format *client-debug-stream* format-string format-args)
    (if (eq *debug-box-client?* :single-step)
	(let ((string (read-line)))
	  (cond ((string-equal string "break")
		 (break))))
	(terpri *client-debug-stream*))
    (force-output *client-debug-stream*)))


(defmacro debugging-message (format-string . format-args)
  (when *include-debugging-code*
    `(print-debugging-message-internal ,format-string . ,format-args)))





;;; net chars to lisp chars

;; these will do for now....

(defmacro netchar->char (byte)
  `(code-char ,byte))

(defmacro char->netchar (char)
  `(char-code ,char))

;;;; File Info

(defvar *box-bid-table* (make-hash-table))

(defvar *bid-box-table* nil)

;;; these info structs function as write-through caches for
;;; file info on the server
(defstruct (server-box-info (:conc-name sbi-)
			    (:predicate server-box-info?)
			    (:constructor %make-server-box-info))
  (bid 1)
  (box nil)
  (owner nil)
  (read-date 0)
  (write-date 0)
  (boxname "UnNamed")  ; not neccessarily the same as the box's name
  (inferiors nil)
  (forwarding-table nil)
  (superior-box-bid 0)
  (server nil)
  (local-timestamp -1))

(defun make-server-box-info (bid box
				 &optional
				 (write-date 0)
				 (owner (bsui-username *box-server-user*))
				 (supid 0)
				 table
				 (server (get-server)))
  (%make-server-box-info :bid bid :box box
			 :write-date write-date
			 :forwarding-table table
			 :superior-box-bid supid
			 :owner owner
			 :server server))


;;;; User Info

(defvar *box-server-user* nil)

(defvar *cache-server-password?* nil)

(defvar *default-box-server-user* "boxer")

(defstruct (box-server-user-info (:conc-name bsui-)
				 (:predicate box-server-user-info?)
				 (:constructor %make-box-server-user-info))
  (username nil)
  (pretty-name nil)
  (cached-password nil)
  (hostname (machine-instance))
  (top-box-id -1))

;; on the sparcs, we may want to eventually play a sound file here
(defun warning-sound ()
  (bw::beep))



;;;; Finding the current server

(defvar *top-level-bfs-server* nil)

(defun get-server (&optional(box boxer-eval::*lexical-variables-root*))
  (if (not (box::box? box)) *top-level-bfs-server*
      (or (box::getprop box 'bfs-server)
	  (get-server (box::superior-box box)))))

;;; Cross File Port and Forwarding Table Utilities

(defvar *cross-file-link-id-counter* 0)

(defvar *link-id-table* (make-hash-table :test #'eq))


(defstruct (cross-file-link (:predicate %cross-file-link?)
			    (:constructor %make-cross-file-link))
  (id -1))

(defstruct (inferior-file-link (:include cross-file-link)
			       (:predicate %inferior-file-link?)
			       (:constructor %make-inferior-file-link))
  (port-branch nil)
  (target-branch nil))

(defstruct (file-branch-link (:include cross-file-link)
			     (:predicate %cross-file-branch-link))
  (inferior-link nil) ; backpointer
  (box)               ; terminating box, either a target or a port
  (lowest nil)
  (broken nil))

(defstruct (file-port-branch-link (:include file-branch-link)
				  (:predicate %file-port-branch-link?)
				  (:constructor %make-file-port-branch-link)
				  (:print-function %print-file-port-branch-link))
  )

(defstruct (file-target-branch-link (:include file-branch-link)
				    (:predicate %file-target-branch-link?)
				    (:constructor
				     %make-file-target-branch-link)
				    (:print-function
				     %print-file-target-branch-link))
  )

#|

;; a port UID of 0 in the port target reference means
;; that the last box in the chain is the target

(defmacro do-pttt-entries ((var table) &body body)
  `(dolist (,var ,table) . ,body))

(defun make-pttt-table (storage-box)
  (let ((table nil)
	(tid 1))
    (dolist (bl (box::branch-links storage-box))
      (when (box::target-branch-link? bl)
	(let ((superior-storage-box (superior-storage-box
				     (box::link-target bl))))
	  (when (and (eq superior-storage-box storage-box)
		     (not (eq (box::link-target bl) storage-box)))
	    (push (cons tid (box::link-target bl)) table)
	    (incf& tid)))))
    table))

;; returns a new table and a flag indicating whether or not the
;; table had to be modifed
(defun update-pttt-table (storage-box old-table)
  (let ((tid (length old-table))
	(modified nil))
    (dolist (bl (box::branch-links storage-box))
      (when (box::target-branch-link? bl)
	(let ((superior-storage-box (superior-storage-box
				     (box::link-target bl))))
	  (when (and (eq superior-storage-box storage-box)
		     (not (eq (box::link-target bl) storage-box)))
	    (unless (rassoc (box::link-target bl) old-table :test #'eq)
	      (push (cons tid (box::link-target bl)) old-table)
	      (setq modified t)
	      (incf& tid))))))
    ;; check for obsolete entries
    (do-pttt-entries (entry old-table)
      (when (not (member (cdr entry) (box::branch-links storage-box)
			 :test #'(lambda (a b) (eq a (box::link-target b)))))
	(setq modified t)
	(setf (cdr entry) 'not-here-anymore)))
    (values old-table modified)))

;;; like update-pttt-table except it doesn't cons a new table
;;; we don't bother to remove bad entries, just look for new ones
;;; which should be there
(defun check-pttt-table (storage-box)
  (let ((old-table (get-port-target-translation-table storage-box)))
    (dolist (bl (box::branch-links storage-box))
      (when (box::target-branch-link? bl)
	(let ((superior-storage-box (superior-storage-box
				     (box::link-target bl))))
	  (when (and (eq superior-storage-box storage-box)
		     (not (eq (box::link-target bl) storage-box)))
	    (unless (rassoc (box::link-target bl) old-table :test #'eq)
	      (return t))))))))

(defsubst make-pttt-entry (uid target)
  (cons uid target))

(defsubst pttt-uid (trans) (car trans))

(defsubst pttt-target (trans) (cdr trans))

(defun find-target-uid (target table)
  (declare (list table))
  (let ((entry (rassoc target table :test #'eq)))
    (unless (null entry) (pttt-uid entry))))

(defun get-port-target-translation-table (box)
  (boxer::getprop box :port-target-translation-table))

(defun set-port-target-translation-table (box new-table)
  (boxer::putprop box new-table :port-target-translation-table))

|#

(defvar *current-forwarding-table* nil)

(defvar *new-forwarding-inferiors* nil
  "Any storage-chunk encountered during the filling of box contents
   should be pushed onto this list so that the top level box's
   forwarding table can be pushed down to the next level of inferiors")

;;; this needs to be used by the various get-box and get-box-and-info
;;; server interface functions to automagically do any neccessary forwarding
(defmacro with-forwarding-table-bound ((bid server-info) &body body)
  `(let* ((*current-forwarding-table* (unless (null ,server-info)
					(sbi-forwarding-table ,server-info)))
	  (*new-forwarding-inferiors* nil))
     (prog1 (progn . ,body)
       ;; if the body finishes without error, then the forwarding
       ;; table needs to be pushed down to the next level of
       ;; inferiors and we can reset the forwarding table for this
       ;; particular box
       (unless (null *current-forwarding-table*)
	 (dolist (inferior-bid *new-forwarding-inferiors*)
	   (set-bid-forwarding-table inferior-bid
				     (merge-forwarding-tables
				      (get-bid-forwarding-table inferior-bid)
				      *current-forwarding-table*)))
	 (set-bid-forwarding-table ,bid NIL)))))

(defmacro current-forwarding-table () '*current-forwarding-table*)

(defun record-inferior-storage-chunk (bid)
  (push bid *new-forwarding-inferiors*))

;;; ??? old entries should take precedence over new ones ??
(defun merge-forwarding-tables (old-table new-table)
  (let ((newnew nil))
    ;; loop through the old looking for an entry in the new
    (do* ((remaining-pairs old-table (cddr remaining-pairs))
	  (orig-bid  (car remaining-pairs) (car remaining-pairs))
	  (trans-bid (cadr remaining-pairs) (cadr remaining-pairs))
	  (new-trans (find-bid trans-bid new-table)))
	 ((null orig-bid))
      (cond ((find-bid orig-bid new-table)
	     ;; old entry is handled in new table, so do nothing
	     )
	    ((not (null new-trans))
	     ;; old translation is obsolete, convert it to a new one
	     (setq newnew (append newnew (list orig-bid new-trans))))
	    (t
	     ;; old entry has nothing to do with the new table so preserve it
	     (setq newnew (append newnew (list orig-bid trans-bid))))))
    ;; now append the new table to the built up table of
    ;; translated old references and return it
    (append newnew new-table)))

;; remember that the CAR is a port-target-uid and not part of the bid path
(defun translate-port-target-path (path)
  (let ((ft (current-forwarding-table)))
    (cond ((null ft) path)
	  (t
	   (let ((newpath (list (car path))))
	     (dolist (bid (cdr path))
	       (let ((trans (member bid ft :test #'=)))
		 (push (if (null trans) bid (cadr trans)) newpath)))
	     (nreverse newpath))))))




(defmacro with-server ((server-variable) &body body)
  `(let ((,server-variable (get-server)))
     . ,body))

(defmacro with-info ((info-var) &body body)
  `(let ((,info-var (%make-server-box-info)))
     #+lucid (declare (lcl::dynamic-extent ,info-var))
     . ,body))

(defmacro with-bfs-standard-dumping-environment (&body body)
  `(let ((*dump-out-file-box-inferiors?* nil))
     . ,body))

(defun in-bfs-environment? () (not *dump-out-file-box-inferiors?*))

;;; Accessor and mutator generating macro for server-box-info's
;;; remember that that the info structs are supposed to be
;;; write through caches of information on the server

(defmacro defsbi-info-slot (slot-name)
  (let ((box-accessor-name (intern (boxer:symbol-format nil "GET-BOX-~A" slot-name)))
	(box-mutator-name  (intern (boxer:symbol-format nil "SET-BOX-~A" slot-name)))
	(bid-accessor-name (intern (boxer:symbol-format nil "GET-BID-~A" slot-name)))
	(bid-mutator-name  (intern (boxer:symbol-format nil "SET-BID-~A" slot-name)))
	(slot-accessor-name (intern (boxer:symbol-format nil "SBI-~A" slot-name))))
    `(progn
       ;; Box designated accessor
       (defun ,box-accessor-name (box &optional (cons-info? nil))
	 (let ((existing-info (box-server-info box cons-info?)))
	   (unless (null existing-info) (,slot-accessor-name existing-info))))
       ;; Box designated mutator
       (defun ,box-mutator-name (box new-value &optional (cons-info? nil))
	 (let ((existing-info (box-server-info box cons-info?)))
	   (unless (null existing-info)
	     (setf (,slot-accessor-name existing-info) new-value)
	     (set-box-info (get-server)
			   (sbi-bid existing-info) existing-info))))
       ;; BId designated accessor
       (defun ,bid-accessor-name (bid)
	 (let ((existing-info (bid-server-info bid)))
	   (cond ((null existing-info)
		  ;; we have to get it from the server directly
		  (with-info (info)
		    (,slot-accessor-name
		     (get-box-info (get-server) bid info))))
		 (t
		  (,slot-accessor-name existing-info)))))
       ;; BId desginated mutator
       (defun ,bid-mutator-name (bid new-value)
	 (let ((existing-info (bid-server-info bid)))
	   (with-server (server)
	     (cond ((null existing-info)
		    ;; get it from the server
		    (with-info (info)
		      (setf (,slot-accessor-name
			     (get-box-info server bid info))
			  new-value)
		    (set-box-info server bid info)))
		   (t
		    (setf (,slot-accessor-name existing-info) new-value)
		    (set-box-info server bid existing-info)))))))))



;;;; Errors

;;; server errors can happen in both the editor and the evaluator
;;; so we need to be able to handle either


;;; used to establish a catch for server errors inside
;;; of editor functions.  Primitives have the usual
;;; evaluator error handling to use, although they might
;;; want to use this mechanism as well.

(defvar *inside-server-error-handler* nil)

(defmacro with-server-errors (&body body)
  `(let ((*inside-server-error-handler* t))
     (catch 'server-error
       . ,body)))

(defun server-error (format-string &rest format-args)
  (cond ((not (null boxer-eval::*give-lisp-errors*))
	 (apply #'error format-string format-args))
	((or (null box::*evaluation-in-progress?*)
	     (box::edit-during-eval?))
	 ;; we are editing, either at top level or recursively
	 (apply 'box::boxer-editor-error format-string format-args)
	 ;; If we can, throw
	 (when *inside-server-error-handler*
	   (throw 'server-error nil)))
	(t
	 ;; looks like we are solidly in the evaluator
	 (boxer-eval::primitive-signal-error
	  :server-error (apply #'format nil format-string format-args)))))



;;;;
;;;; FILE: comdef.lisp
;;;;

;; sgithens 2023-04-20 No longer used after cleaning up all the right click popup menu commands
(defun mouse-still-down-after-pause? (pause-time)
  (not
   (or (wait-with-timeout nil pause-time #'(lambda () (zerop& (mouse-button-state))))
       ;; one final check
       (zerop& (mouse-button-state)))))

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

;; we need to make sure that we don't leave just a single row for unfixed size
;; boxes because that makes it hard to use the scrolling machinery
;; should be smarter and estimate row heights so the lowest we go is still a boxful
;; of text
(defun new-elevator-scrolled-row (ed-box elevator-row-no)
  (let ((elevator-row (row-at-row-no ed-box elevator-row-no)))
    (cond ((and t ; (not (fixed-size? ed-box)) ;; note: Size is fixed temp, for ALL
                (eq elevator-row (last-inferior-row ed-box)))
           (or (previous-row elevator-row) elevator-row))
      (t elevator-row))))

(defun set-v-scroll-row (screen-box fraction
                                    &optional (ed-box (screen-obj-actual-obj screen-box))
                                    (no-of-rows (length-in-rows ed-box)))
  (set-scroll-to-actual-row screen-box
                            (new-elevator-scrolled-row ed-box
                                                       (floor (* fraction (1-& no-of-rows))))))

(defun last-scrolling-row (editor-box)
  (previous-row (previous-row (last-inferior-row editor-box))))

(defun last-page-top-row (box hei)
  (do ((row (last-inferior-row box) (previous-row row))
       (acc-height 0))
    ((or (null row) (>= acc-height hei))
     (if (null row) (first-inferior-row box) (next-row row)))
    (setq acc-height (+ acc-height (estimate-row-height row)))))

(defvar *initial-scroll-pause-time* .5
  "Seconds to pause after the 1st line scroll while holding the mouse")

(defvar *scroll-pause-time* 0.1
  "Seconds to pause between each line scroll while holding the mouse")

(defun mouse-line-scroll-internal (screen-box direction)
  (if (eq direction :up)
    (com-scroll-up-row screen-box)
    (com-scroll-dn-row screen-box))
  ;; do one thing, show it, then pause...
  #+lispworks (capi::apply-in-pane-process *boxer-pane* #'repaint t)
  (simple-wait-with-timeout *initial-scroll-pause-time* #'(lambda () (zerop& (mouse-button-state))))

  ;; sgithens 2021-03-11 This `if` is a temporary crash fix, as this method keeps getting called when there are
  ;; no previous rows for `last-scrolling-row`
  (if (and (previous-row (last-inferior-row (screen-obj-actual-obj screen-box)))
           (previous-row (previous-row (last-inferior-row (screen-obj-actual-obj screen-box)))))
    ;; now loop
    (let* ((edbox (screen-obj-actual-obj screen-box))
          (1st-edrow (first-inferior-row edbox))
          (last-edrow (last-scrolling-row edbox)))
      (loop (when (or (zerop& (mouse-button-state))
                      (and (eq direction :up) (eq (scroll-to-actual-row screen-box) 1st-edrow))
                      (and (eq direction :down) (row-> (scroll-to-actual-row screen-box)
                                                  last-edrow)))
              ;; stop if the mouse is up or we hit one end or the other...
              (return))
        (if (eq direction :up)
          (com-scroll-up-row screen-box)
          (com-scroll-dn-row screen-box))
        (repaint)
        (simple-wait-with-timeout *scroll-pause-time*
                                  #'(lambda () (zerop& (mouse-button-state))))))))


(defvar *only-scroll-current-box?* nil)


(defvar *smooth-scroll-pause-time* 0.005
  "Seconds to pause between each pixel scroll while holding the mouse")

;; sgithens 2023-12-20 We aren't using these types of scroll buttons anymore, and if we do again
;;                     the implemention will be factored differently

;; The scroll bar button checks from com-mouse-scroll-box, looking at the return from get-scroll-position
        (:v-up-button (if click-only?
                        (com-scroll-up-row screen-box)
                        (mouse-line-scroll-internal screen-box :up)))
        (:v-down-button (if click-only?
                          (com-scroll-dn-row screen-box)
                          (mouse-line-scroll-internal screen-box :down)))
        (:h-left-button (if click-only?
                          (h-scroll-screen-box screen-box *horizontal-click-scroll-quantum*)
                          (mouse-h-scroll screen-box :left)))
        (:h-right-button (if click-only?
                           (h-scroll-screen-box screen-box (- *horizontal-click-scroll-quantum*))
                           (mouse-h-scroll screen-box :right)))
;; and the old cond expressions from get-scroll-position in oglscroll.lisp
((and (>= x (+ box-window-x (- wid right)))
      (> y (+ v-div *scroll-button-length* 2))) ; fudge factor...
((>= x (+ box-window-x (- wid right)))
       (unless (null scroll-top) :v-up-button))                                                                                                                                          (unless last-is-top? :v-down-button))
((> x (+ h-div *scroll-button-length*))
      (unless (null (slot-value screen-box 'max-scroll-wid)) :h-right-button))
(t
      (unless (zerop (slot-value screen-box 'scroll-x-offset)) :h-left-button))

;; sgithens 2023-12-19 If you look at one of the old old windows builds we had, there was this
;;                     funky grid rendered on the scroll bars.

(defvar *max-scroll-grid-increment* 15
  "Maximum number of pixels between each tick in the scroll bar grid")

(defvar *min-scroll-grid-increment* 4
  "Minimum number of pixels between each tick in the scroll bar grid")

(defvar *scroll-grid-width* 10)

(defun mouse-page-scroll-internal (direction &rest screen-box-list)
  (if (eq direction :up)
    (com-scroll-up-one-screen-box screen-box-list)
    (com-scroll-dn-one-screen-box screen-box-list))
  (simple-wait-with-timeout *initial-scroll-pause-time*
                            #'(lambda () (zerop& (mouse-button-state))))
  (loop (when (zerop& (mouse-button-state)) (return))
    (if (eq direction :up)
      (com-scroll-up-one-screen-box screen-box-list)
      (com-scroll-dn-one-screen-box screen-box-list))
    (repaint)
    (simple-wait-with-timeout *scroll-pause-time*
                              #'(lambda ()
                                        (zerop& (mouse-button-state))))))

(defboxer-command com-mouse-page-scroll-box (&optional (window *boxer-pane*)
                                                       (x (bw::boxer-pane-mouse-x))
                                                       (y (bw::boxer-pane-mouse-y))
                                                       (mouse-bp
                                                        (mouse-position-values x y))
                                                       (click-only? t))
  "Scroll box by the page"
  window
  (reset-region)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box))
         (fixed? (not (null (display-style-fixed-wid
                             (display-style-list (screen-obj-actual-obj
                                                  screen-box)))))))
    (unless (and *only-scroll-current-box?* (neq screen-box (point-screen-box)))
      (unless fixed? ; fix the box size during scrolling
        (multiple-value-bind (wid hei)
                             (screen-obj-size screen-box)
                             (multiple-value-bind (left top right bottom)
                                                  (box-borders-widths (box-type  (screen-box-point-is-in))
                                                                      (screen-box-point-is-in))
                                                  (set-fixed-size (screen-obj-actual-obj screen-box)
                                                                  (- wid left right)
                                                                  (- hei top bottom)))))
      (case (get-scroll-position x y screen-box box-type)
        (:v-up-button (if click-only?
                        (com-scroll-up-one-screen-box (list screen-box))
                        (mouse-page-scroll-internal :up screen-box)))
        (:v-down-button (if click-only?
                          (com-scroll-dn-one-screen-box (list screen-box))
                          (mouse-page-scroll-internal :down screen-box)))
        (:h-left-button (if click-only?
                          (h-scroll-screen-box screen-box (* 2 *horizontal-click-scroll-quantum*))
                          (mouse-h-scroll screen-box :left 2)))
        (:h-right-button (if click-only?
                           (h-scroll-screen-box screen-box (* 2 (- *horizontal-click-scroll-quantum*)))
                           (mouse-h-scroll screen-box :right 2)))
        (:v-bar (mouse-in-v-scroll-bar-internal screen-box x y click-only?))
        (:h-bar (mouse-in-h-scroll-bar-internal screen-box x y)))
      ;; now restore the box, if we have fixed it before
      (unless fixed? (set-fixed-size (screen-obj-actual-obj screen-box) nil nil))))
  ;; if the cursor is in the box being scrolled (or some inferior), we
  ;; need to make sure that it gets moved to where it will become visible
  ;; The scroll-to-actual-row of the screen box is a good bet
  boxer-eval::*novalue*)

(defboxer-command com-mouse-limit-scroll-box (&optional (window *boxer-pane*)
                                                        (x (bw::boxer-pane-mouse-x))
                                                        (y (bw::boxer-pane-mouse-y))
                                                        (mouse-bp
                                                         (mouse-position-values x y))
                                                        (click-only? t))
  "To the limit..."
  window
  (reset-region)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (edbox (screen-obj-actual-obj screen-box))
         (box-type (box-type screen-box)))
    (unless (and *only-scroll-current-box?* (neq screen-box (point-screen-box)))
      (multiple-value-bind (left top right bottom)
                           (box-borders-widths box-type screen-box)
                           (declare (ignore left right))
                           (multiple-value-bind (wid hei)
                                                (screen-obj-size screen-box)
                                                (declare (ignore wid))
                                                (case (get-scroll-position x y screen-box box-type)
                                                  (:v-up-button   (set-scroll-to-actual-row screen-box (first-inferior-row edbox)))
                                                  (:v-down-button (set-scroll-to-actual-row screen-box
                                                                                            (last-page-top-row edbox (- hei top bottom))))
                                                  (:h-left-button  (h-scroll-screen-box screen-box 100000)) ; any large number will do...
                                                  (:h-right-button (h-scroll-screen-box screen-box -100000))
                                                  (:v-bar (mouse-in-v-scroll-bar-internal screen-box x y click-only?))
                                                  (:h-bar (mouse-in-h-scroll-bar-internal screen-box x y)))))))
  boxer-eval::*novalue*)

(defvar *smooth-scrolling?* nil)  ; for now...

;; pixel (as opposed to row) based scrolling
;; should we quantize on integral row on exit ??
;; no movement lines for now, presumably, disorientation should be less of a problem
;; no initial pause, start scrolling right away

(defvar *smooth-scroll-min-speed* 1)
(defvar *smooth-scroll-med-speed* 2)
(defvar *smooth-scroll-max-speed* 6) ; note must be less than (max-char-height)

(defun mouse-smooth-scroll-internal (screen-box direction)
  (drawing-on-window (*boxer-pane*)
                     (queueing-screen-objs-deallocation
                      (let* ((edbox (screen-obj-actual-obj screen-box))
                             (1st-edrow (first-inferior-row edbox))
                             (last-edrow (last-scrolling-row edbox))
                             (slow-start-time (get-internal-real-time)))
                        (multiple-value-bind (initial-mx initial-my) (mouse-window-coords)
                                             (declare (ignore initial-mx))
                                             (flet ((get-velocity ()
                                                                  (let ((ydiff (- initial-my
                                                                                  (multiple-value-bind (mx my) (mouse-window-coords)
                                                                                                       (declare (ignore mx)) my)))
                                                                        (tdiff (- (get-internal-real-time) slow-start-time)))
                                                                    (if (eq direction :up)
                                                                      (cond ((or (> ydiff 10)
                                                                                 (> tdiff (* 2 internal-time-units-per-second)))
                                                                             *smooth-scroll-max-speed*)
                                                                        ((or (> ydiff 5)
                                                                             (> tdiff internal-time-units-per-second))
                                                                         *smooth-scroll-med-speed*)
                                                                        (t *smooth-scroll-min-speed*))
                                                                      (cond ((or (< ydiff -10)
                                                                                 (> tdiff (* 2 internal-time-units-per-second)))
                                                                             (- *smooth-scroll-max-speed*))
                                                                        ((or (< ydiff -5)
                                                                             (> tdiff internal-time-units-per-second))
                                                                         (- *smooth-scroll-med-speed*))
                                                                        (t (- *smooth-scroll-min-speed*)))))))
                                                   ;; everything needs to happen inside the screen-box
                                                   (let ((bwid (screen-obj-wid screen-box))
                                                         (bhei (screen-obj-hei screen-box))
                                                         (body-time (round (* *smooth-scroll-pause-time*
                                                                              internal-time-units-per-second))))
                                                     (multiple-value-bind (sb-x sb-y) (xy-position screen-box)
                                                                          (with-drawing-inside-region (sb-x sb-y bwid bhei)
                                                                            ;; grab the initial y pos as a baseline for acceleration
                                                                            (loop (when (or (zerop& (mouse-button-state))
                                                                                            (and (eq direction :up)
                                                                                                 (or (eq (scroll-to-actual-row screen-box)
                                                                                                         1st-edrow)
                                                                                                     (null (scroll-to-actual-row screen-box)))
                                                                                                 (zerop (slot-value screen-box 'scroll-y-offset)))
                                                                                            (and (eq direction :down)
                                                                                                 (row-> (or (scroll-to-actual-row screen-box)
                                                                                                            (first-inferior-row edbox))
                                                                                                   last-edrow)))
                                                                                    (return))
                                                                              (timed-body (body-time)
                                                                                          (let ((vel (get-velocity)))
                                                                                            (setq vel (pixel-scroll-screen-box screen-box vel))
                                                                                            ;; sgithens TODO 2021-04-21 Crash fix, these don't exist anymore.
                                                                                            ;; Can we remove this entire timed-body section?
                                                                                            ;; (erase-scroll-buttons *last-scrolled-box* t)
                                                                                            ;; (scroll-move-contents screen-box vel)
                                                                                            )
                                                                                          ;; (draw-scroll-buttons screen-box t)
                                                                                          (swap-graphics-buffers)))
                                                                            ;; now maybe move the point so it is still visible after scrolling...
                                                                            (let ((scroll-row (scroll-to-actual-row screen-box)))
                                                                              (cond ((null scroll-row)
                                                                                     (move-point-1 (first-inferior-row
                                                                                                    (screen-obj-actual-obj screen-box))
                                                                                                   0 screen-box))
                                                                                ((and (not (zerop (slot-value screen-box 'scroll-y-offset)))
                                                                                      (not (null (next-row scroll-row))))
                                                                                 (move-point-1 (next-row scroll-row) 0 screen-box))
                                                                                (t (move-point-1 scroll-row 0 screen-box))))
                                                                            ;; finally cover up our mistakes...
                                                                            ;(set-force-redisplay-infs? screen-box) ; looks bad...
                                                                            )))))))))

;; sgithens 2023-04-20 Removing this old slow graphics toggle option
(defvar *slow-graphics-toggle* nil)

;; This was unused in the com-mouse-bl-corner-toggle-box-view let*
(graphics-sheet (if (port-box? box)
                           (slot-value (ports box) 'graphics-info)
                           (slot-value box 'graphics-info)))

;; This was the final cond predicate... replacing with t
(if *slow-graphics-toggle*
         (and (let ((waited? (mouse-still-down-after-pause?
                              *mouse-action-pause-time*)))
                ;; if the user has clicked, but not waited long enough,
                ;; maybe warn about how to win
                (when (and (null waited?) *warn-about-disabled-commands*)
                  (boxer-editor-warning
                   "You have to hold the mouse down for ~A seconds to confirm"
                   *mouse-action-pause-time*))
                waited?)
              (mouse-corner-tracking (:bottom-left)
                                     #'toggle-corner-fun screen-box))
         (or click-only?
             (mouse-corner-tracking (:bottom-left)
                                    #'toggle-corner-fun screen-box)))

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

#| ; old stuff
(DEFUN BP-OVER-VALUES (BP DIRECTION DELIMITER-CHAS)
  (LET ((NOT-FIRST-CHA? NIL))
    (MAP-OVER-CHAS-IN-LINE (BP DIRECTION)
      (LET ((DELIMITER-CHA? (char-member cha delimiter-chas)))
     (COND ((AND (NULL CHA)
           (NULL NEXT-OR-PREVIOUS-ROW)) ;end/beginning of the box
      (RETURN (VALUES ROW CHA-NO)))
     ((AND (NULL CHA) NOT-FIRST-CHA?) ;end/beginning of the line
      (RETURN (VALUES ROW CHA-NO)))
     ((AND NOT-FIRST-CHA? DELIMITER-CHA?) ;end of the word
      (RETURN (VALUES ROW CHA-NO)))
     ((NOT DELIMITER-CHA?)	;beginning of word
      (SETQ NOT-FIRST-CHA? T)))))))
|#

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


;; sgithens 2022-09-29 This is duplicated by defun boxer-file-contents?
(defun box-file? (pathname)
  (or #+mcl (eq (ccl:mac-file-type pathname) :BOXR)
      ;; peek at the beginning bytes...
      (let ((b0 (ldb (byte 8 0) bin-op-format-version))
            (b1 (ldb (byte 8 8) bin-op-format-version))
            f0 f1 f2 f3)
        (with-open-file (s pathname :element-type '(unsigned-byte 8))
          (setq f0 (read-byte s nil nil) f1 (read-byte s nil nil)
                f2 (read-byte s nil nil) f3 (read-byte s nil nil)))
        (unless (or (null f0) (null f1) (null f2) (null f3))
          (or
           (and (= b0 f0) (= b1 f1) (= f2 *version-number*) (= f3 0))
           ;; byte swapping version...
           (and (= b0 f1) (= b1 f0) (= f3 *version-number*) (= f2 0)))))))

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

;; this should go somewhere else (file-prims.lisp ?)
(defun make-file-box (pathname)
  (multiple-value-bind (RO world maj min btype bname)
      (boxer-file-info pathname)
    (declare (ignore RO world maj min))
    (let ((filebox (make-box '(()) btype bname)))
      (mark-box-as-file filebox pathname)
      (shrink filebox)
      (setf (first-inferior-row filebox) nil)
      filebox)))

;; sgithens Old mcl version of com-help text
;#+mcl(" press cmd-<help> or cmd-? after the name of the function.")
;; option-shift-K is the apple glyph
#+mcl(" press �-<help> or �-? after the name of the function.")
#+mcl(" press option-<help> or option-?")


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

;; sgithens 2024-03-04 this is just *boxer-pane* and was only used in like one remaining place...
(DEFVAR %DRAWING-ARRAY NIL
        "Inside of a drawing-on-window, this variable is bound to %drawing-window's
   screen-array (Note that this value is valid because drawing-on-window does
   a prepare-sheet of drawing-window.")

(defvar *redisplay-in-progress?* nil)

(defmacro redisplaying-unit (&body body)
  `(let ((*redisplay-in-progress?* t))
        (progn . ,body)))

(defun redisplay-in-progress? () *redisplay-in-progress?*)

(defvar *redisplay-id* 0)
(defvar *redisplay-encore?* nil)
(defvar *allow-redisplay-encore? nil)

(defun redisplay-id () *redisplay-id*)

(DEFVAR %DRAWING-WINDOW NIL
        "Inside of a drawing-on-window, this variable is bound to the window which
   was given as an argument to drawing-on window, makes sense right.")

;; Commented out slots that were on defclass screen-obj
   ;   (new-wid :initform 0 :accessor screen-obj-new-wid)
   ;   (new-hei :initform 0 :accessor screen-obj-new-hei)
   ;   (new-x-got-clipped? :initform nil :accessor screen-obj-new-x-got-clipped?)
   ;   (new-y-got-clipped? :initform nil :accessor screen-obj-new-y-got-clipped?)

;; Commented out slot on defclass screen-row
   ;   (out-of-synch-mark :initform nil :accessor out-of-synch-mark)


;; 2023-02-23 Removing slot from defclass screen-box and the got-repainted
;; method from repaint.lisp that used it

   ;; how much to slosh inferiors when the borders change
   ;   (inf-shift :initform nil :accessor inf-shift)  ; remove?

;(defmethod got-repainted ((self screen-box))
;  (call-next-method)
;  (setf (inf-shift self) nil))

(DEFVAR %DRAWING-FONT-MAP NIL
        "Inside of a drawing-on-window, this variable is bound to %drawing-window's
   font-map.")

;;;;
;;;; FILE: disdef.lisp
;;;;

(defmacro with-real-time (&body body)
  `(progn . ,body))

(DEFUN ACTUAL-OBJ-OF-SCREEN-OBJ (SCREEN-OBJ)
       (IF (SCREEN-CHA? SCREEN-OBJ)
           SCREEN-OBJ
           (SCREEN-OBJ-ACTUAL-OBJ SCREEN-OBJ)))

;;; right now these are flushed by the got-redisplayed
;;; method (probably not the best place)

(defvar *repaint-during-eval?* :always
  "Periodically update the screen during eval, valid values are :always,:changed-graphics, and :never")
; ; :changed-graphics simulates the old behavior


(defstruct (screen-row-rdp1-info (:type vector)
                                 (:conc-name sr-rdp1-info-))
  (action nil) ; a keyword
  (from-cha-no 0)
  (from-offset 0)
  (no-of-chas 0)
  (dist-to-move 0)
  (width-to-move nil))

; ;; we may want to resource these (but lets just cons them for now)
; ;; we shouldn't need any more than 5-10 since only those rows which have
; ;; changed will use them at any given time
(defun allocate-sr-rdp1-info (&optional (from-cha-no 0) (from-offset 0))
  (make-screen-row-rdp1-info :from-cha-no from-cha-no
                             :from-offset from-offset))

; ;; a stub for allocation
(defun free-sr-rdp1-info (info) (declare (ignore info))  nil)


(DEFMACRO CHA-WIDTH (CHA) `(CHA-WID ,CHA))

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

(DEFMACRO REDISPLAYING-BOX (SCREEN-BOX &BODY BODY)
          ;;this macro sets up the scaling for the redisplay of a particular box without having to
          ;;redisplay the entire screen.  This means that the box to be redisplayed has to be a fixed
          ;;sized box to avoid worrying about propagating changes in size to the superiors of the box.
          `(QUEUEING-SCREEN-OBJS-DEALLOCATION
            (MULTIPLE-VALUE-BIND (SUPERIOR-ORIGIN-X-OFFSET SUPERIOR-ORIGIN-Y-OFFSET)
                                 (let ((ssb (superior ,screen-box)))
                                   (if (screen-obj? ssb) (xy-position ssb) (values 0 0)))
                                 (with-origin-at ((SCALE-X SUPERIOR-ORIGIN-X-OFFSET)
                                                  (SCALE-Y SUPERIOR-ORIGIN-Y-OFFSET))
                                   ,@BODY))))

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

;;;; Utilities for Handling the screen-row-rdp1-info

;;; informs pass-2 that pass-1 erased all the characters from the
;;; optional cha-no arg to the end of the line
(defun set-rdp1-info-to-erase-to-eol (info &optional cha-no offset)
  (unless (eq (sr-rdp1-info-no-of-chas info) 'to-eol)
    (setf (sr-rdp1-info-action info) ':insert)
    (unless (null cha-no)
      (setf (sr-rdp1-info-from-cha-no info) cha-no))
    (unless (null offset)
      (setf (sr-rdp1-info-from-offset info) offset))
    (setf (sr-rdp1-info-no-of-chas  info) 'to-eol)))

(defun rdp1-info-is-eol? (info)
  (eq (sr-rdp1-info-no-of-chas info) 'to-eol))

(defun still-inside-rdp1-info (info cha-no)
  (cond ((numberp (sr-rdp1-info-no-of-chas info))
         (<= cha-no
             (+ (sr-rdp1-info-from-cha-no info)
                (sr-rdp1-info-no-of-chas  info))))
    ((eq (sr-rdp1-info-no-of-chas  info)
         'to-eol))))


;; 2023-02-23 Removing this single call from repaint.lisp:defmethod rp1-sr-punt-extra-screen-objs-from
;; and the unused definitions from disply.lisp

  ;; erase the glyphs from the screen...
  (erase-screen-chas (slot-value self 'screen-chas) no-of-first-obj-to-punt
                     x-coord y-coord)

;;; this erases ONLY characters to the end of the line
;;; NOTE: this CAN'T just iterate through the screen chas erasing
;;; characters BECAUSE the screen-chas may have been side effected
;;; HOWEVER, any boxes will still have valid offsets and widths so
;;; we use the boxes to delimit regions to erase.
;;; If there are no boxes, we simply erase to the end of the row
;;; This may actually turn out to be faster (especially in the X
;;; implementation) because of fewer graphics commands

(defun erase-chas-to-eol (cha-no screen-chas x-offset y-offset row-width
                                 &optional boxes-too)
  (let ((erase-height 0)
        (current-x-offset x-offset))
    (do-screen-chas-with-font-info (cha screen-chas :start cha-no)
      (cond ((screen-cha? cha)
             (setq erase-height (max erase-height (cha-hei))))
        ((not (null boxes-too))
         ;; we want to erase boxes as well as characters
         ;; first, adjust the size of the erasing rectangle
         (setq erase-height (max erase-height (screen-obj-hei cha)))
         ;; now do all the things erase-screen-box would have done
         ;(screen-obj-zero-size cha) ; huh ? this seems to break things
         ;; sgithens 2021-11-18 Removing these un-needed slots below.
         ;; The next question is, are any of these erase methods even
         ;; necessary anymore in the OpenGL double buffer version? TODO
         ;;  (set-needs-redisplay-pass-2? cha t)
         ;;  (set-force-redisplay-infs?   cha t)
         )
        (t
         ;; looks like we hit a box, and we want to preserve the box
         ;; erase from the current-x-offset to the beginning of the box
         (erase-rectangle (- (screen-obj-x-offset cha) current-x-offset)
                          erase-height
                          current-x-offset y-offset)
         ;; now setup the values for the next block
         (setq erase-height 0)
         (setq current-x-offset (+ (screen-obj-x-offset cha)
                                   (screen-obj-wid      cha))))))
    ;; now finish off the rest of the row
    (erase-rectangle (- row-width current-x-offset) erase-height
                     current-x-offset y-offset)))

(defun erase-screen-cha (screen-cha x-offset y-offset)
  (if (not-null screen-cha)
    (let ((wid (cha-wid screen-cha))
          (hei (cha-hei)))
      (erase-rectangle wid hei x-offset y-offset))
    (barf "null screen-cha for some reason")))

(defun erase-screen-chas (chas start-cha-no x-offset y-offset
                               &optional stop-cha-no)
  (do-screen-chas-with-font-info (cha-to-erase chas
                                               :start start-cha-no
                                               :stop stop-cha-no)
    (let (obj-wid)
      (cond ((screen-cha? cha-to-erase)
             (setq obj-wid (cha-wid cha-to-erase))
             (erase-screen-cha cha-to-erase x-offset y-offset))
        (t
         (setq obj-wid (screen-obj-wid cha-to-erase))
         (erase-screen-box cha-to-erase x-offset y-offset)))
      ;; now increment the x-offset (y-ofset doesn't change on the row)
      (setq x-offset  (+ x-offset obj-wid)))))






(DEFUN REDISPLAY-CLUE (TYPE &REST ARGS)
       (LET ((HANDLER (GET TYPE ':REDISPLAY-CLUE)))
            (IF (NOT-NULL HANDLER)
                (APPLY HANDLER TYPE ARGS)
                (BARF "~S is an unknown type of redisplay-clue." TYPE))))

(setf (get ':clear-screen ':redisplay-clue)
      #'(lambda (&rest ignore)
                (declare (ignore ignore))
                (push '(:clear-screen) *redisplay-clues*)))

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

;; origin gets reset in hardware by scaling macros so these are no ops
;; They need to be defined because other functions (usually sprite graphics)
;; will use them explicitly to convert coords.
(defmacro scale-x (x) x)
(defmacro scale-y (y) y)

;; sgithens 2024-03-04 this doesn't do much anymore...
(defmacro with-turtle-clipping ((wid hei . args) &body body)
  "This MUST use the hardware clipping regardless of speed.
It is used only around bodies which do sprite graphics
so the frequency of use is MUCH less than it is in the redisplay

this adjusts the clipping to be width and height AT the current
scaled origin"
  `(with-window-system-dependent-clipping (0 0 ,wid ,hei . ,args) . ,body))

;; sgithens 2024-03-04 no longer needed...
(defmacro with-scrolling-origin ((scroll-x scroll-y) &body body)
  ;; do we need to readjust the clip region here ????
  `(with-origin-at (,scroll-x ,scroll-y)
     . ,body))

(defun draw-filled-arc (alu x y wid hei start-angle sweep-angle)
  (%draw-filled-arc %drawing-window alu (scale-x x) (scale-y y)
                    wid hei start-angle sweep-angle))

(defun draw-arc (alu x y wid hei start-angle sweep-angle)
  (%draw-arc %drawing-window alu (scale-x x) (scale-y y)
             wid hei start-angle sweep-angle))

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

;; sgithens 2024-03-04 I don't believe this needed anymore
(defmacro prepare-sheet ((window) &body body)
  `(with-drawing-port ,window
     ;; make sure things are the way they should be
     ,@body))


;; sgithens 2024-01-08 None of these seem to be used other than emptily being
;;          bound in drawing macros
;; (defvar %local-clip-lef 0)
;; (defvar %local-clip-top 0)
;; (defvar %local-clip-rig (expt 2 15))
;; (defvar %local-clip-bot (expt 2 15))

;; (defvar %clip-total-height nil)

(defun %draw-filled-arc (bit-array alu x y width height th1 th2)
  "See the-attic for the previous lispworks GP library version of this function.
It's not clear yet whether we'll need to re-implement this for the future."
  (declare (ignore bit-array alu x y width height th1 th2)))

(defun %draw-arc (bit-array alu x y width height th1 th2)
  "See the-attic for the previous lispworks GP library version of this function.
It's not clear yet whether we'll need to re-implement this for the future."
  (declare (ignore bit-array alu x y width height th1 th2)))

;; sgithens 2023-07-10 Removing ogl-color stuff

;; Boxer represents colors as RGB triples where each component is
;; between 0 and 100 inclusively.

;;
;; use this to convert a boxer RGB component to a window system dependent
;; color component.  Domain checking should be assumed to occur at a
;; higher (in boxer proper) level but paranoia is ok too
;; in CLX,  color component is between 0.0 and 1.0
;; Mac stores them as 3 concatenated 8-bit fields of an integer
;; LWWin RGB color spec component is between 0.0 and 1.0 but we pass pixels
;; instead.
;; Get pixels from color specs via ogl-convert-color which
;; Opengl color = gl-vector of 4 floats between 0.0 and 1.0

(defmacro color-red (pixel) `(bw::ogl-color-red ,pixel))

(defmacro color-green (pixel) `(bw::ogl-color-green ,pixel))

(defmacro color-blue (pixel) `(bw::ogl-color-blue ,pixel))

(defmacro color-alpha (pixel) `(bw::ogl-color-alpha ,pixel))

;; this should return a suitable argument to set-pen-color
;; should assume that R,G,and B are in boxer values (0->100)
;; **** If the current screen is B&W, this needs to return a valid value ****
;; **** perhaps after doing a luminance calculation ****
;; **** NTSC  luminance = .299Red + .587Green + .114Blue
;; **** SMPTE luminance = .2122Red + .7013Green + .0865Blue
;; (defun %make-color (red green blue &optional alpha)

;; sgithens 2023-06-12 Only usage of this was in draw-high draw-poly
  ;; should'nt transform the points because translation is done @ hardware level in OpenGL
  ; (unless (null points)
  ;   (%draw-poly (boxer-points->window-system-points points (x x) (y y))))
(defmacro boxer-points->window-system-points (boxer-point-list (x-arg x-form)
                                                               (y-arg y-form))
  "this takes a set of boxer points and converts them into a form that
the window system desires.  The x/y-function args will be funcalled
on the coordinate as it is converted.

OpenGL expects a list of X Y pairs"
  `(macrolet ((x-handler (,x-arg) ,x-form)
              (y-handler (,y-arg) ,y-form))
             (let ((trans nil))
               (dolist (pt ,boxer-point-list (nreverse trans))
                 (push (list (x-handler (car pt)) (y-handler (cdr pt))) trans)))))

(defmacro with-blending-on (&body body)
  ;; sgithens 2023-01-17 TODO Blending is essentially always on these days. Remove this once the old
  ;; openGL immediate mode version is removed completely.
  (let ((current-blend-var (gensym)))
    `(let ((,current-blend-var nil ))
       (unwind-protect
        (progn
         . ,body)))))

;; check for the existence of auxiliary buffer so we can signal
;; an error at the right level
(defun auxiliary-buffer-count ()
  (bw::get-opengl-state opengl::*gl-aux-buffers* :signed-32))

(defun auxiliary-buffer-exists? ()
  (not (zerop (auxiliary-buffer-count))))

;; **** may have to change this depending on what offscreen-bitmap-image does
;; THIS should be SETFable
;; Image-Pixel and the SETF is supposed to work in conjuction with offscreen-bitmap-image's

(defmacro image-pixel (x y pixmap)
  `(pixmap-pixel ,pixmap ,x ,y))

;;;
(defun %set-image-pixel (x y pixmap new-pixel)
  (set-pixmap-pixel new-pixel pixmap x y))

(defsetf image-pixel %set-image-pixel)

;; copy-graphics-sheet in makcpy,  stamp-bitmap in  gcmeth ?
(defun copy-offscreen-bitmap (alu wid hei src src-x src-y dst dst-x dst-y)
  (declare (ignore alu))
  (%copy-pixmap-data wid hei src src-x src-y dst dst-x dst-y))

(defun make-offscreen-bitmap (window w h)
  (declare (ignore window))
  (make-ogl-pixmap w h))

;; gak !
(defun ogl-pixmap-depth (pm) (declare (ignore pm)) 32)


;; also yuck, but it might actually be correct
;; see window-depth for real yuck
(defun offscreen-bitmap-depth (bitmap)
  (ogl-pixmap-depth bitmap))

;;; These assume bitmap bounds are zero based
(defun offscreen-bitmap-height (bm) (ogl-pixmap-height bm))

(defun offscreen-bitmap-width (bm) (ogl-pixmap-width bm))

(defun deallocate-bitmap (bm) (opengl::ogl-free-pixmap bm))

(defun free-offscreen-bitmap (bitmap) (ogl-free-pixmap bitmap))

;; **** Look here !!! ****
;; this is supposed to return an object that you can use offscreen-pixel
;; with.  In the clx implementation, it actually brings the data over to
;; the client side in an array.  In other implementations,
;; offscreen-bitmap-image might just get the actual image data out from
;; behind any containing structures
;;
;; In the mac implementation, we just refer to the GWorld.  We may want to change
;; this to refer to the Pixmap of the GWorld in the future since we have already
;; made the semantic split in the higher level code.  The caveat would be in properly
;; interpreting the meaning of the pixel arg in the SETF version of IMAGE-PIXEL.
;;
;; In the OpenGL implementation, we have to deal with the fact that the image is
;; built from the bottom up instead of the top down.  This is done and the load/save level
;; it also means we have o pass the entire pixmap struct so we can use the width, height
;; slots to calculate the correct offset for the pixel's location

(defun offscreen-bitmap-image (bm) bm)

;; **** A hook for those implementations in which OFFSCREEN-BITMAP-IMAGE
;; returns a separate structure like X.  Probably a NOOP for any native
;; window system
(defun set-offscreen-bitmap-image (bm image)
  (declare (ignore bm image))
  nil)

(defun offscreen-pixel (x y pixmap)
   (pixmap-pixel pixmap x y))

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
;;;; FILE: dumper.lisp
;;;;

;; sgithens 2022-09-03 I believe this version was only used in the old bfs server setup
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

; from defun dump-top-level-box writing-bin-file
    #+mcl (ccl::set-mac-file-type filename "BOXR")
    #+mcl (ccl::set-mac-file-creator filename "BOXR")

; from end of defun end-bin-file
  ;; don't know about ExCl yet...
  #+(or lucid lispm)
  (and (system:file-stream-p stream) (truename stream))

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

; from defun dump-symbol
#+(or Symbolics mcl)
(canonicalize-package-name
                            (symbol-package symbol))

#+symbolics
(defun array-bits-per-element (array)
  (let ((et (si:array-type array)))
    (cdr (fast-assq et si::array-bits-per-element))))

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

#+mcl
(defun file-stream-position (stream)
  (etypecase stream
             (ccl::file-stream (file-length stream))
             (ccl::tcp-stream  (boxnet::writing-stream-position stream))))

; was under defun flags-for-dumping
#|
  (if (or (eq box *outermost-dumping-box*) (in-bfs-environment?)
          (box-flag-read-only-box? flags))
|#

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

#|  no more sprtie boxes....
(defmethod dump-self ((self sprite-box) stream)
  (write-file-word bin-op-sprite-box stream)
  (dump-box-plist self stream)
  (write-file-word bin-op-end-of-box stream))
|#




; sgithens 2022-05-10 This was inside dump-graphics-sheet

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

;;
;; sgithens 2022-05-10 While these may work, our pixmaps are hardcoded to always return a depth of 32 so these will never
;; get called. If we were going to make any pixmap saving changes, we should just start using a standard png library or
;; something.
;;

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




;;;;
;;;; FILE: editor.lisp
;;;;

;;; These are used ONLY by the comaptability loader and should
;;; eventually be flushed or moved to compat-loader.lisp

(DEFMETHOD SEMI-INIT ((SELF BOX) INIT-PLIST)
           (SETF   ;;these come from box proper
                   (FIRST-INFERIOR-ROW SELF) (GETF INIT-PLIST ':SUPERIOR-ROW)
                   (CACHED-ROWS SELF)        NIL
                   (NAME SELF)       (WHEN (GETF INIT-PLIST :NAME)
                                           (MAKE-NAME-ROW `(,(GETF INIT-PLIST :NAME))))
                   (DISPLAY-STYLE-LIST SELF) (OR (GETF INIT-PLIST ':DISPLAY-STYLE-LIST)
                                                 (DISPLAY-STYLE-LIST SELF)))
           (WHEN (NAME-ROW? (slot-value self 'name))
                 (SET-SUPERIOR-BOX (slot-value self 'name) SELF))
           (SET-TYPE SELF (OR (GETF INIT-PLIST ':TYPE) 'DOIT-BOX)))

(DEFMETHOD RETURN-INIT-PLIST-FOR-FILING ((SELF BOX))
           `(:TYPE ,(class-name (class-of SELF))
                    :DISPLAY-STYLE-LIST ,(DISPLAY-STYLE-LIST SELF)))



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
;;;; FILE: eval-command-loop.lisp
;;;;

;; 2023-03-18 This nearly exact copy of boxer-system-error-restart-loop was only used in the dribbler
;;            which is currently not active/working.
(defmacro boxer-system-error-restart (&body body)
  (let ((warned-about-error-already-gensym (gensym)))
    `(let ((,warned-about-error-already-gensym nil))
       (restart-bind
  ((BOXER-CONTINUE
    #'(lambda () (throw 'system-error-restart-loop nil))
    :report-function
    #'(lambda (stream)
        (unless (or ,warned-about-error-already-gensym
        boxer::*boxer-system-hacker*
        boxer::*inside-lisp-breakpoint-p*)
    (beep) (beep) (beep)
    ;; this mechanism is a crock.
    (setq ,warned-about-error-already-gensym t))
        (format stream "--> Return to Boxer <--")))
   (BOXER-TOP-LEVEL
    #'(lambda () (boxer::com-goto-top-level)
             (throw 'system-error-restart-loop nil))
    :report-function
    #'(lambda (stream)
        (format stream "--> GOTO Top Level then return to Boxer <--"))))
  (handler-bind
   ((error
     #'(lambda (c)
         (cond ((or ,warned-about-error-already-gensym
        (not *automagic-lisp-error-handling*))
          (invoke-debugger c))
         (t
          (dotimes (i 3) (beep))
          ;(format t "~%Lisp Error:~A" c)
                      (when *report-crash* (write-crash-report))
          (boxer::boxer-editor-error "Lisp Error:~A" c)
          (invoke-restart 'BOXER-CONTINUE))))))
    (catch 'system-error-restart-loop
      . ,body)
    (setq ,warned-about-error-already-gensym nil))))))


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

(defmacro expand-mutators-and-body (args initial-index &body body)
  (cond ((null args)
         `(progn . ,body))
    (t `(macrolet ((,(intern (symbol-format nil "SET-~A" (car args)))
                    (new-value)
                    `(setf (svref& .graphics-command. ,,initial-index)
                           ,new-value)))
                  (expand-mutators-and-body ,(cdr args)
                                             ,(incf initial-index)
                                             ,@body)))))

;; this is not smart about special forms
(defun walk-body-for-args (args body)
  (let ((mutators (mapcar #'(lambda (s) (intern (symbol-format nil "SET-~A" s)))
                          args))
        (found nil)
        (mutators-found nil))
    (cond ((symbolp body)
          (when (and (member body args) (not (member body found)))
            (push body found))
          (when (and (member body mutators) (not (member body mutators-found)))
            (push body mutators-found)))
      ((listp body)
      (dolist (thing body)
        (cond ((symbolp thing)
                (when (and (member thing args) (not (member thing found)))
                  (push thing found))
                (when (and (member thing mutators) (not (member thing mutators-found)))
                  (push thing mutators-found)))
          ((listp thing)
            (multiple-value-bind (a m)
                                (walk-body-for-args args thing)
                                (setq found (union found a)
                                      mutators-found (union mutators-found m))))))))
    (values found mutators-found)))

(defmacro with-graphics-command-slots-bound (gc-arg args body)
  (multiple-value-bind (args-used mutators-used)
                      (walk-body-for-args args body)
                      (cond ((and (null args-used) (null mutators-used)) `,body)
                        (T
                          (let ((mutators (mapcar #'(lambda (s)
                                                            (intern (symbol-format nil "SET-~A" s)))
                                                  args)))
                            ;; easier than passing a list of mutators
                            `(let ,(mapcar #'(lambda (arg)
                                                    (list arg `(svref& ,gc-arg
                                                                        ,(1+ (position arg args)))))
                                          args-used)
                              (flet ,(mapcar #'(lambda (mut)
                                                        (list mut '(new-value)
                                                              `(setf (svref& ,gc-arg
                                                                              ,(1+ (position
                                                                                    mut mutators)))
                                                                    new-value)))
                                              mutators-used)
                                      ,body)))))))


(defvar *initial-graphics-command-dispatch-table-size* 32.
  "This is the expected maximum number of DIFFERENT graphics commands.
   The actual table sizes will be twice this number with the bottom
   half for window coordinate based commands and the top half for
   boxer coodinate based commands.")

(defvar *boxer-graphics-command-mask* 32.)

;;; peephole optimizer support

;; walk back looking for entries in the graphics list that
;; can be changed rather than blindly appending to the graphics list
;; for now, we just peek at the last entry.  At some point, we may
;; want to continue backwards until we hit a non state change entry.
(defun check-existing-graphics-state-entries (opcode new-value graphics-list)
  (let ((al (storage-vector-active-length graphics-list)))
    (unless (zerop& al)
      (let ((last-entry (sv-nth (1-& al) graphics-list)))
        (cond ((=& opcode (svref& last-entry 0))
               (setf (svref& last-entry 1) new-value)
               T)
          (t nil))))))

(defun arglist-argnames (arglist)
  (let ((revargnames nil))
    (dolist (arg arglist)
      (cond ((fast-memq arg lambda-list-keywords))
        ((consp arg) (push (car arg) revargnames))
        (t (push arg revargnames))))
    (nreverse revargnames)))

(defvar *type-check-during-template-conversion* t)

(defun expand-transform-template-item (arg template-action direction)
  (ecase direction
        (:boxer->window (case template-action
                          (:x-transform (list 'fix-array-coordinate-x arg))
                          (:y-transform (list 'fix-array-coordinate-y arg))
                          (:coerce      (list 'round arg))
                          (t            arg)))
        (:window->boxer (case template-action
                          (:x-transform (list 'user-coordinate-fix-x arg))
                          (:y-transform (list 'user-coordinate-fix-y arg))
                          (:coerce      (list 'make-single-float arg))
                          (t            arg)))))


;; sgithens 2024-02-20 Final removal of the last of this macro
(defmacro defgraphics-command ((name opcode
                                    &optional (optimize-recording? nil))
                              args
                              sprite-command
                              transform-template
                              &body draw-body)
  (flet ((numeric-declaration-args ()
                                  (with-collection
                                    (do* ((list-of-args args (cdr list-of-args))
                                          (arg (car list-of-args) (car list-of-args))
                                          (template-items transform-template (cdr template-items))
                                          (template-item (car template-items) (car template-items)))
                                      ((null list-of-args))
                                      (unless (null template-item)
                                        (collect arg))))))
        (let* ((boxer-command-name (intern (symbol-format nil "BOXER-~A" name)))
              (boxer-command-opcode (+ opcode *boxer-graphics-command-mask*))
              (wstruct-name
                (intern (symbol-format nil "WINDOW-GRAPHICS-COMMAND-~A" name)))
              (wmake-name
                (intern (symbol-format nil "MAKE-WINDOW-GRAPHICS-COMMAND-~A" name)))
              (wcopy-name
                (intern (symbol-format nil "COPY-WINDOW-GRAPHICS-COMMAND-~A" name)))
              (wcopy-struct-name wcopy-name)
              (bstruct-name
                (intern (symbol-format nil "BOXER-GRAPHICS-COMMAND-~A" name)))
              (bmake-name
                (intern (symbol-format nil "MAKE-BOXER-GRAPHICS-COMMAND-~A" name)))
              (bcopy-name
                (intern (symbol-format nil "COPY-BOXER-GRAPHICS-COMMAND-~A" name)))
              (bcopy-struct-name bcopy-name)
              (window->boxer-name
                (intern (symbol-format nil "GRAPHICS-WINDOW->BOXER-~A-ALLOCATOR" name)))
              (process-function
                (intern (symbol-format nil "Process Graphics Command ~A" name))))
          `(progn
            (defstruct (,wstruct-name (:type vector)
                                      (:constructor ,wmake-name ,args)
                                      (:copier ,wcopy-struct-name))
              ;; slot 0 is used as an index into the dispatch table
              (type ,opcode)
              ,@args)
            (defstruct (,bstruct-name (:type vector)
                                      (:constructor ,bmake-name ,args)
                                      (:copier ,bcopy-struct-name))
              ;; slot 0 is used as an index into the dispatch table
              (type ,boxer-command-opcode)
              ,@args)

            Conversion functions from Window->Boxer coordinates and back
            these rely on being called within a with-graphics-vars-bound
            (defun ,window->boxer-name (window-command)
              (let ((graphics-command
                    (,bmake-name ,@(let ((idx 0))
                                      (mapcar #'(lambda (arg)
                                                        (declare (ignore arg))
                                                        (incf idx)
                                                        (expand-transform-template-item
                                                        `(svref& window-command ,idx)
                                                        (nth (1- idx) transform-template)
                                                        ':window->boxer))
                                              args)))))
                graphics-command))
            install it
            (setf (svref& *graphics-command-window->boxer-translation-table*
                          ,opcode)
                  ',window->boxer-name)

            ;; now make the function for drawing on the window (that also records)
            (defun ,name ,args
              ,@args
              (progn ,@draw-body))

            ;; finally return the name (as opposed to random returned values)
            ',name))))

(defvar *graphics-command-window->boxer-translation-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

;; sgithens 2024-02-20 older version of this function
(defun allocate-window->boxer-command (graphics-command)
  (if (< (aref graphics-command 0) 32)
    (let ((handler (svref& *graphics-command-window->boxer-translation-table*
                           (svref& graphics-command 0))))
      (if (null handler)
        (error "No translation allocator for ~A" graphics-command)
        (funcall handler graphics-command)))
    graphics-command))

(defvar *graphics-command-descriptor-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil)
  "This is useful for decoding what a graphics command is")

(defvar *graphics-command-binding-values-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-deallocation-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-dumper-dispatch-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-loader-dispatch-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-dispatch-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-size-values-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-copier-table*
    (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
                :initial-element nil))

(defvar *graphics-command-translation-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

(defvar *graphics-command-boxer->window-translation-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

(defvar *graphics-command-sprite-command-translation-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defvar *graphics-command-name-opcode-alist* nil
  "Used to map names back into their opcodes")

(eval-when (:compile-toplevel :load-toplevel :execute)
           (defun graphics-command-transform-template (graphics-command)
             (graphics-command-descriptor-transform-template
              (get-graphics-command-descriptor (svref& graphics-command 0))))
) ; eval-when

            ;; this indirection is provided because we may want to
            ;; switch to some resource scheme for these command markers
            ;; instead of just consing them up on the fly
            (defun ,recording-function ,args
              (unless (not (null *supress-graphics-recording?*))
                ,(if optimize-recording?
                  `(if (eq *graphics-command-recording-mode* ':boxer)
                      (unless (check-existing-graphics-state-entries
                              ,boxer-command-opcode ,@args %graphics-list)
                        (sv-append %graphics-list (,bmake-name ,@args)))
                      (unless (check-existing-graphics-state-entries
                              ,opcode ,@args %graphics-list)
                        (sv-append %graphics-list (,wmake-name ,@args))))
                  `(sv-append %graphics-list
                              (if (eq *graphics-command-recording-mode*
                                      ':boxer)
                                (,bmake-name ,@args)
                                (,wmake-name ,@args))))))

 (defun graphics-command-opcode (command-name)
    (let ((entry (fast-assq command-name *graphics-command-name-opcode-alist*)))
      (if (null entry)
        (error "No Graphics Command Opcode for ~S" command-name)
        (cdr entry))))

(defun graphics-command-slot-offset (descriptor slot-name)
  (let ((pos (position slot-name
                      (graphics-command-descriptor-slots descriptor))))
    (if (null pos)
      (error "The slot, ~S, does not seem to be in the Graphics Command ~S"
            slot-name (graphics-command-descriptor-name descriptor))
      (1+ pos))))

(defmacro graphics-command-values (command-name-or-opcode
                                   graphics-command &body body)
  (let ((opcode (etypecase command-name-or-opcode
                           (number command-name-or-opcode)
                           (symbol (graphics-command-opcode command-name-or-opcode)))))
    `(,(svref& *graphics-command-binding-values-table* opcode)
      ,graphics-command
       ,@body)))

(defmacro bind-graphics-handlers ((table) &body body)
  `(let ((*graphics-command-dispatch-table* ,table))
     . ,body))

(defun translate-graphics-command (graphics-command trans-x trans-y)
  (if (< (aref graphics-command 0) 32)
    (let ((handler (svref& *graphics-command-translation-table*
                           (svref& graphics-command 0))))
      (unless (null handler)
        (funcall handler graphics-command trans-x trans-y)))
    graphics-command))

(defmacro defgraphics-handler ((name &optional
                                     (table
                                      '*graphics-command-translation-table*))
                               extra-args &body body)
  (let ((handler-name (intern (symbol-format nil "~A Graphics Handler ~A"
                                             name (gensym))))
        (handler-opcode (graphics-command-opcode name)))
    ;; some of that ole' compile time error checking appeal...
    (cond ((null table)
           (error "Need a table to put the handlers in"))
      ((vectorp table)
       (when (>= handler-opcode (svlength table))
         (error "The table, ~A, is too short for an opcode of ~D"
                table handler-opcode)))
      ((symbolp table)
       (let ((value (symbol-value table)))
         (if (vectorp value)
           (when (>= handler-opcode (svlength value))
             (error "The table, ~A, is too short for an opcode of ~D"
                    table handler-opcode))
           (error "Hey, ~A doesn't look like a handler table" table))))
      (t (error "fooey !")))
    (cond ((null body)
           ;; a null body means that we should copy the default handler
           `(if (null (svref& *graphics-command-dispatch-table*
                              ,handler-opcode))
              (error "There is NO default command for ~S" ',name)
              (setf (svref& ,table ,handler-opcode)
                    (svref& *graphics-command-dispatch-table*
                            ,handler-opcode))))
      (t
       `(progn
         (defun ,handler-name (graphics-command . ,extra-args)
           ,@extra-args
           (graphics-command-values ,name graphics-command
                                     . ,body))
         (setf (svref& ,table ,handler-opcode) ',handler-name)
         ',name)))))

(defmacro process-graphics-command-marker (graphics-command &rest args)
  `(let ((handler (svref& *graphics-command-dispatch-table*
                          (svref& ,graphics-command 0))))
     (when (null handler)
       (format t "~%Handler missing for: ~A" ,graphics-command))
     (unless (null  handler)
       (funcall handler ,graphics-command ,@args))))

(defvar *graphics-command-translation-and-scaling-table*
  (make-array *initial-graphics-command-dispatch-table-size*
              :initial-element nil))

(defun translate-and-scale-graphics-command (graphics-command
                                             trans-x trans-y
                                             scale-x scale-y)
  (let ((handler (svref& *graphics-command-translation-and-scaling-table*
                         (svref& graphics-command 0))))
    (unless (null handler)
      (funcall handler graphics-command trans-x trans-y scale-x scale-y))))

(defvar *turtle-translation-table*
  (make-array (* 2 *initial-graphics-command-dispatch-table-size*)
              :initial-element nil))

(defun translate-boxer->window-command (from-graphics-command
                                        to-graphics-command
                                        trans-x trans-y
                                        cos-scale sin-scale scale)
  (let ((handler (svref& *turtle-translation-table*
                         (svref& from-graphics-command 0))))
    (unless (null handler)
      (format t "~%Handler: ~A" handler)
      (funcall handler
               from-graphics-command to-graphics-command
               trans-x trans-y cos-scale sin-scale scale)))
  (format t "~%translate-boxer->window-command:after
  from-graphics-command: ~A to-graphics-command: ~A
  trans-x: ~A trans-y: ~A cos-scale: ~A sin-scale:~A scale: ~A" from-graphics-command to-graphics-command
  trans-x trans-y cos-scale sin-scale scale)
  )

;;; Used to define arbitrary transformations between the boxer/floating
;;; representations and the window/fixnum representations
;;; translation clauses should be a list of forms.
;;; The CAR of each form should be the name of a slot in the graphics command
;;; and the CADR of each form should be a form to be called which translates
;;; the slot.  The translating form is called in an environment where the
;;; slots of the originating form as well as the EXTRA-ARGS are bound
;;;
;;; This tries to be smart and use info from the transformation-template
;;; when none is provided
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro defgraphics-translator ((name &optional
                                        (table '*turtle-translation-table*)
                                        (direction :boxer->window))
                                  extra-args translation-clauses)
  (let* ((handler-name (intern (symbol-format nil "~A Graphics Translator ~A"
                                              name (gensym))))
        (handler-opcode (if (eq direction :window->boxer)
                          (graphics-command-opcode name)
                          (+ (graphics-command-opcode name)
                              *boxer-graphics-command-mask*)))
        (command-descriptor (get-graphics-command-descriptor handler-opcode))
        (template (graphics-command-descriptor-transform-template
                    command-descriptor))
        )
    ;; some of that ole' compile time error checking appeal...
    (cond ((null table)
          (error "Need a table to put the handlers in"))
      ((vectorp table)
      (when (>= handler-opcode (svlength table))
        (error "The table, ~A, is too short for an opcode of ~D"
                table handler-opcode)))
      ((symbolp table)
      (let ((value (symbol-value table)))
        (if (vectorp value)
          (when (>= handler-opcode (svlength value))
            (error "The table, ~A, is too short for an opcode of ~D"
                    table handler-opcode))
          (error "Hey, ~A doesn't look like a handler table" table))))
      (t (error "fooey !")))
    `(progn
      (defun ,handler-name (from-gc to-gc . ,extra-args)
        ,@extra-args   ;; handle bound but never used errors
        (graphics-command-values ,handler-opcode from-gc
                                  ,@(with-collection
                                      (dolist (slot (graphics-command-descriptor-slots
                                                    command-descriptor))
                                        (let ((tform (assoc slot translation-clauses))
                                              (offset (graphics-command-slot-offset
                                                      command-descriptor slot)))
                                          (collect
                                          `(setf (svref& to-gc ,offset)
                                                  ,(if (not (null tform))
                                                    (cadr tform)
                                                    (let ((template-action (nth (1- offset)
                                                                                template)))
                                                      (expand-transform-template-item
                                                        slot template-action direction))))))))))
      (setf (svref& ,table ,handler-opcode) ',handler-name)
      ',handler-name)))

)

;;; temporary fix to keep Window systems from blowing out when
;;; some kid types FORWARD 239823094230923490
;;;
;;; In theory, this should get captured at a higher level
;;; in the STEPS-ARG-CHECK function but that doesn't deal in
;;; window coords so it can be fooled
;;;
(defun ensure-legal-window-coordinate (n)
  (cond ((< n #.(min-window-coord))
         (warn "window system coordinate ~D too small, changing to ~D"
               n #.(min-window-coord))
         #.(min-window-coord))
    ((>= n #.(max-window-coord))
     (warn "window system coordinate ~D too large, changing to ~D"
           n #.(max-window-coord))
     #.(max-window-coord))
    (t n)))

;;;; COLOR

(defstruct (boxer-color :named (:type vector)
                        (:constructor %make-boxer-color (red green blue)))
  (red   0)
  (green 0)
  (blue  0))

;;; color tables map color indices (internal fixnums) to boxer
;;; color structures.  For each window system, there should be
;;; a way to obtain an index from a color description.
;;; The index returned will be a suitable value for the turtle's pen-color



(defmacro playback-graphics-list-internal (gl &key (start 0) (graphics-canvas nil))
  `(with-graphics-state (,gl t)

     (when (and *use-opengl-framebuffers* ,graphics-canvas)
      (when (graphics-canvas-pen-color-cmd ,graphics-canvas)
        (process-graphics-command-marker (graphics-canvas-pen-color-cmd ,graphics-canvas)))
      (when (graphics-canvas-pen-size-cmd ,graphics-canvas)
        (process-graphics-command-marker (graphics-canvas-pen-size-cmd ,graphics-canvas)))
      (when (graphics-canvas-pen-font-cmd ,graphics-canvas)
        (process-graphics-command-marker (graphics-canvas-pen-font-cmd ,graphics-canvas))))

      (do-vector-contents (command ,gl :start ,start)
        (process-graphics-command-marker command) ; . ,args)

        (when (and *use-opengl-framebuffers* ,graphics-canvas)
          (cond ((equal 4 (aref command 0))
                (setf (graphics-canvas-pen-color-cmd ,graphics-canvas) command))
                ((equal 1 (aref command 0))
                (setf (graphics-canvas-pen-size-cmd ,graphics-canvas) command))
                ((equal 2 (aref command 0))
                (setf (graphics-canvas-pen-font-cmd ,graphics-canvas) command)))))))

;; sgithens 2023-07-26 bugs-54 removal

(defun translate-graphics-command-list (gl trans-x trans-y)
  (do-vector-contents (graphics-command gl)
    (translate-graphics-command graphics-command trans-x trans-y)))

(defun translate-and-scale-graphics-command-list (gl trans-x trans-y
                                                     scale-x scale-y)
  (do-vector-contents (graphics-command gl)
    (translate-and-scale-graphics-command graphics-command
                                          trans-x trans-y
                                          scale-x scale-y)))

;;; should go somewhere else eventually, doesn't hack clipping
;;; should also handle "boxer-bit-gravity" eventually , for now,
;;; the "bit gravity" will be :TOP-RIGHT but we might want to
;;; make it be :CENTER to follow the sprite coordinate system
;;; (maybe have scaling too)

(defvar *boxer-graphics-box-bit-gravity* ':center)

(defvar *scale-on-resize?* nil)

;; stuff from resize-graphics-sheets that's not needed anymore... the boxer turtle coords stay the same
    (when (not (null (graphics-sheet-graphics-list sheet)))
      (ecase *boxer-graphics-box-bit-gravity*
             (:top-right
              (when *scale-on-resize?*
                (translate-and-scale-graphics-command-list
                 (graphics-sheet-graphics-list sheet)
                 0 0 wid-scale hei-scale)))
             (:center
              (cond ((null *scale-on-resize?*)
                     (translate-graphics-command-list
                      (graphics-sheet-graphics-list sheet)
                      (round (-& new-wid old-wid) 2)
                      (round (-& new-hei old-hei) 2))
                     )
                (t
                 (translate-graphics-command-list
                  (graphics-sheet-graphics-list sheet)
                  (-& (round old-wid 2)) (-& (round old-hei 2)))
                 (translate-and-scale-graphics-command-list
                  (graphics-sheet-graphics-list sheet)
                  (round new-wid 2) (round new-hei 2)
                  wid-scale hei-scale))))))

       ;; if *scale-on-resize?* was not nil
       (t
          (dolist (obj (graphics-sheet-object-list sheet))
            (when (not (eq (graphics-sheet-draw-mode sheet) ':clip))
              (set-x-position obj (* (x-position obj) wid-scale))
              (set-y-position obj (* (y-position obj) hei-scale)))
            ))

        ;; bitmap resizing
        (case *boxer-graphics-box-bit-gravity*
          (:top-right
           (copy-pixmap-data
            (min& old-wid new-wid) (min& old-hei new-hei)
            old-bitmap 0 0 new-bitmap 0 0))
          (:center

;; sgithens 2023-07-24 bugs-54 removal
(defun allocate-boxer->window-command (graphics-command)
  (let ((handler (svref& *graphics-command-boxer->window-translation-table*
                         (let ((opcode (svref& graphics-command 0)))
                           ;; this is a crock to handle cases where
                           ;; we (maybe) need to convert old graphics
                           ;; commands from obsolete files
                           (if (>=& opcode 32) (-& opcode 32) opcode)))))
    (if (null handler)
      (error "No translation allocator for ~A" graphics-command)
      (funcall handler graphics-command))))


;;; Like a Graphics-Command-List with extra slots.  This is used as
;;; a cache for the rendering of a turtle shape at a particular
;;; location and heading.  Subsequent calls to draw the turtle
;;; can just access the cached values instead of recalculating.
;;; Commands will be of the window system/integer variety

(defstruct (turtle-window-shape (:type vector)
                                (:include graphics-command-list)
                                ;; we need to define our own versions of
                                (:copier %%copy-turtle-window-shape)
                                (:constructor %%make-turtle-window-shape))
  (valid nil)
  ;; some places to cache popular quantities
  min-graphics-x-extent
  max-graphics-x-extent
  min-graphics-y-extent
  max-graphics-y-extent)

(defsubst turtle-window-shape? (thing)
  (simple-vector-p thing))

(defun make-turtle-window-shape (&optional (shape *default-turtle-shape*))
  (let ((tws (%%make-turtle-window-shape
              :contents (allocate-c-vector
                         (storage-vector-max-length shape)))))
    (do-vector-contents (gc shape)
      (sv-append tws (allocate-boxer->window-command gc)))
    tws))

(defun flush-window-shape-cache (turtle)
  (let ((ws (slot-value turtle 'window-shape)))
    (setf (turtle-window-shape-valid ws) nil)
    ;; extents are checks separately (see sprite-at-window-point for details)
    (setf (turtle-window-shape-min-graphics-x-extent ws) nil)
    (dolist (ss (subsprites turtle)) (flush-window-shape-cache ss))))

; Commented out section from redisplay-graphics-sheet
    ;; no more save-unders in OpenGL
    ;    (dolist (turtle (graphics-sheet-object-list gs))
    ;      ;; the save-under of the moving turtle has to be filled BEFORE ANY
    ;      ;; turtles are drawn or else it might capture part of another
    ;      ;; turtle's shape
    ;      (save-under-turtle turtle))
    ;; and then any sprites


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
;;;; FILE: html-export.lisp
;;;;
;;;; --entire-file--

;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;         HTML Export
;;;;

(in-package :boxer)

;;;; HTML

(defclass html-file-class
          (foreign-file-class)
  ()
  )

(defmethod begin-foreign-stream ((ffc html-file-class)
                                 &optional (stream *standard-output*))
  (format stream "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" ~%~
                   \"http://www.w3.org/TR/html4/strict.dtd\">~%~
                  <html>~%~%"))


;; optionally <title></title>
;; maybe add boxer version number to description META tag
(defmethod write-foreign-file-header ((ffc html-file-class) box
                                      &optional (stream *standard-output*))
  (format stream
    "~%<head>~%~
       <META http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">~%~
       <META name=\"description\" content=\"Exported from the Boxer System\">~%")
  (let ((nr (name-row box)))
    (unless (null nr)
      (format stream "<TITLE>~A </TITLE>~%" (text-string nr))))
  (format stream "</head>~%"))

(defmethod write-foreign-file-box ((ffc html-file-class) box stream)
  (let* ((*ffc-depth* (min 6 (1+ *ffc-depth*))) ;; maximum header depth is 6
         (*export-properties* (merge-export-properties
                               *export-properties*
                               (get-export-format-properties box)))
         (nr (name-row box))
         (respect-line-breaks? (let ((local (getf *export-properties*
                                                  :respect-line-breaks
                                                  :not-specified)))
                                 (cond ((eq local :not-specified)
                                        (getf (slot-value ffc 'prefs)
                                              :respect-line-breaks))
                                       (t local)))))
    (cond ((null nr)
           ;; need some demarcation for a new box, <HR> or <p> ?
           )
          (t
           (format stream "~%<H~D>~A </H~D>~%~%"
                   *ffc-depth* (text-string nr) *ffc-depth*)))
    (Do ((row (first-inferior-row box) (next-row row)))
        ((null row))
      (write-foreign-file-row ffc row stream)
      (terpri stream)
      (cond ((not (null respect-line-breaks?)) (format stream "<BR>~%"))
            (t (format stream " "))))))

(defmethod write-foreign-file-graphics-box ((ffc html-file-class) box stream)
  (if (and (graphics-box? box)
           (display-style-graphics-mode? (display-style-list box)))
      (format stream "~A~%" *graphics-replacement-text*)
    (write-foreign-file-box ffc box stream)))

;; empty line should output a "<P>"
(defmethod write-foreign-file-row ((ffc html-file-class) row stream)
  (let ((cha-written? nil)
        (empty? t))
    (do-row-chas ((cha row))
      (cond ((box? cha)
             ;; if chas have been written, finish
             (when cha-written? (format stream "<BR>~%"))
             ;; CR's around boxes ?
             (if (and (graphics-box? cha)
                      (let ((ds (display-style-list cha)))
                        (when ds (display-style-graphics-mode? ds))))
                 (write-foreign-file-graphics-box ffc cha stream)
               (write-foreign-file-box ffc cha stream))
             (setq cha-written? nil empty? nil))
            (t (write-xml-char cha stream)
               (setq cha-written? t empty? nil))))
    (when empty? (format stream "~%<P>~%"))))

(defun write-xml-char (char stream)
  (let ((cc (char-code char)))
    (cond ((< cc 128) (write-char char stream))
          (t (format stream " &#~D " cc)))))

(defmethod begin-foreign-body ((ffc html-file-class)
                               &optional (stream *standard-output*))
  (format stream "~%<body>~%"))

(defmethod end-foreign-body ((ffc html-file-class)
                                 &optional (stream *standard-output*))
  (format stream "~%</body>~%"))


(defmethod end-foreign-stream ((ffc html-file-class)
                                 &optional (stream *standard-output*))
  (format stream "~%</html>~%"))

(def-export-type html-file-class "HTML" "*.htm;*.html" :respect-line-breaks nil)

;;;;
;;;; FILE: fildfs.lisp
;;;;

;;;; Pathname Construction and manipulation...

#+lispm
(fs:define-canonical-type :box "Box"	;default type for SAVE/READ
  (:tops-20 "Box")
  (:unix42 "box")
  (:vms "Box")
  (:its "Box"))

#+lispm
(defprop :box 16. :binary-file-byte-size)

;; system dependent
#+mcl
(defvar *possible-boxer-file-mac-types* (list :text :???? :****
                                              ;; OSX default for unknown
                                              (intern
                                               (make-string
                                                4 :initial-element #\Null)
                                               (find-package "KEYWORD"))))

;;;;
;;;; FILE: file-prims.lisp
;;;;

;; sgithens 2024-02-21 In MCL, this is how we used to dynamically update the top macOS menus
#| ;; this is now handled (as it should be) in the menu-update
      #+mcl ;; dynamically adjust the file menu...
      (let ((save-item (find "Save" (slot-value *boxer-file-menu* 'ccl::item-list)
                             :test #'(lambda (a b)
                                       (string-equal a (ccl::menu-item-title b)))))
            ;(save-box-as-item (find "Save Box As..." (slot-value *boxer-file-menu*
            ;                                                     'ccl::item-list)
            ;                        :test #'(lambda (a b)
            ;                                  (string-equal
            ;                                   a (ccl::menu-item-title b)))))
            )
        ;; grey out File menu items if they are redundant or not applicable
        ;; "Save" is not applicable if there is not an existing filename
        ;; or if the file box has not been modified...
        (if (or read-only? (null pathname) (not (file-modified? filebox)))
            (ccl::menu-item-disable save-item)
            (ccl::menu-item-enable  save-item))
        ;; "Save Box As..." is redundant with "Save As"if the
        ;; cursor is in the same box as the document box
;        (if (eq filebox (point-box))
;            (ccl::menu-item-disable save-box-as-item)
;            (ccl::menu-item-enable  save-box-as-item))
        )
|#

;; this is used to catch pathnames which won't even parse

(defun quote-wild-char (string quote-char)
  (let* ((slength (length string))
         (return-string (make-array (1+ slength)
                                    :element-type 'character :adjustable t
                                    :fill-pointer 0)))
    (dotimes (i slength)
      (let ((char (char string i)))
        (when (char= char #\*) (vector-push-extend quote-char return-string))
        (vector-push-extend char return-string)))
    return-string))

(defvar *max-filename-length* 50)

;; pathnames derived from other platforms wont parse into directories correctly
(defun truncated-filename (pathname)
  (unless (pathnamep pathname) (setq pathname (pathname pathname)))
  (let* ((dirs (pathname-directory pathname))
         (name (pathname-name pathname)) (type (pathname-type pathname))
         (lname (length name)) (ltype (length type))
         (ellipsis? t)
         (endsize (cond ((and (null name) (or (null type) (eq type ':unspecific)))
                         (setq ellipsis? nil) 0)
                        ((or (null type) (eq type ':unspecific))
                         (+ 3 lname))
                        ((null name) (+ 3 ltype))
                        (t (+ 3 lname 1 ltype))))
         (return-string (make-string *max-filename-length*))
         (dirstop (max& 0 (- *max-filename-length* endsize)))
         (idx 0))
    ;(declare (dynamic-extent return-string))
    (flet ((add-char (&optional (char #+mcl #\: #-mcl #\/))
             (when (= idx dirstop) (throw 'dir-exit nil))
             (setf (char return-string idx) char)
             (incf idx))
           (add-string (string &optional (throw? t))
             (let ((end (length string)))
               (do ((i 0 (1+ i)))
                   ((or (= i end) (= idx *max-filename-length*)))
                 (when (and throw? (= idx dirstop)) (throw 'dir-exit nil))
                 (setf (char return-string idx) (char string i))
                 (incf idx))))
           (add-ellipsis ()
             (dotimes (i 3) (setf (char return-string idx) #\.) (incf idx))))
      (when (eq (car dirs) :relative)  (add-char))
      (catch 'dir-exit
        (unless (zerop dirstop)
          (dolist (dir (cdr dirs) (setq ellipsis? nil))
            (add-string dir) (add-char))))
      (unless (null ellipsis?)
        (add-ellipsis))
      (cond ((zerop dirstop)
             ;; this means the name.type is already longer than the alloted space
             (cond ((null type)
                    (do ((nidx (+ (- lname *max-filename-length*) 3) (1+ nidx)))
                        ((>= idx *max-filename-length*))
                      (setf (char return-string idx) (char name nidx))
                      (incf idx)))
                   (t
                    ;; truncated name
                    (do ((nidx (+ (- lname *max-filename-length*) 3 1 ltype)
                               (1+ nidx)))
                        ((>= idx (- *max-filename-length* ltype 1)))
                      (setf (char return-string idx) (char name nidx))
                      (incf idx))
                    (add-char #\.)
                    ;; and now the type
                    (do ((i 0 (1+ i)))
                        ((or (= i ltype) (= idx *max-filename-length*)))
                      (setf (char return-string idx) (char type i))
                      (incf idx)))))
            ((and (null name) (or (null type) (eq type ':unspecific))))
            ((or (null type) (eq type ':unspecific))
             (add-string name nil))
            ((null name) (add-string type nil))
            (t (add-string name nil) (add-char #\.) (add-string type nil))))
    (if (< idx *max-filename-length*)
        (subseq return-string 0 idx)
        return-string)))

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

;; sgithens Apparently we used to have file compression that farmed
;; out it's work to a unix command line invocation
(defvar *automatic-file-compression-on* nil)
(defvar *file-compress-minimum-length* (* 64 1024)) ;empirical

;; was inside save-internal
(when *automatic-file-compression-on*
      ;; Compress the file only if it's greater than
      ;; *file-compress-minimum-length*.  Otherwise the time
      ;; it takes won't be worth it.
      (let ((length (with-open-file (stream dest-pathname :direction :input)
           (file-length stream))))
  (if (> length *file-compress-minimum-length*)
      (compress-file dest-pathname))))

;;;
;;; COMPRESS-FILE.
;;;

;;; These are for saving space on unix filesystems.  They should go away
;;; when Ed rewrites the file system make smaller files.

;;; The user can call COMPRESS-FILE.  READ will try to uncompress a file
;;; if it doesn't exist.

(defun compress-file (pathname)
  #+Unix (progn
     (make-backup-if-necessary (concatenate 'string (namestring pathname) ".Z"))
     (boxer-run-unix-program "compress" (list "-f" (namestring pathname))))
  #-Unix (progn pathname nil))

(defun uncompress-file (pathname)
  #+Unix (boxer-run-unix-program "uncompress" (list (namestring pathname)))
  #-Unix (progn pathname nil))

(defun maybe-uncompress-file (pathname)
  #+Unix (when (and (null (probe-file pathname))
        (probe-file (concatenate 'string (namestring pathname) ".Z")))
     #+Lucid (uncompress-file pathname)
     #-Lucid nil)
  #-Unix (progn pathname nil))

#-mcl
(boxer-eval::defboxer-primitive bu::compress-file ((boxer-eval::dont-copy name))
  (let ((filename (if (numberp name)
          (format nil "~D" name)
          (box-text-string name))))
    (if (null (probe-file filename))
  (boxer-eval::signal-error :FILE-NOT-FOUND (boxer-eval::copy-thing name))
  (let ((error-string (compress-file filename)))
    (if (null error-string)
        boxer-eval::*novalue*
        (boxer-eval::signal-error :COMPRESS-FILE error-string))))))

;;; This is just a crock, but at least it will work on the Suns as long
;;; as there's only one Boxer per machine.
(defun make-temporary-filename (info)
  (format nil "/tmp/boxer-~a" info))


(defun read-box-from-text-stream (stream)
  stream
  boxer-eval::*novalue*)

;;;
;;; boxer-run-unix-program
;;;


;;; Returns NIL if succesful, otherwise a list of error strings.
;;;
;;; We have to run the program asynchronously in order to get the
;;; error output into a stream.
;;; Didn't do anything about stdout, though.
(defun boxer-run-unix-program (program-name arguments)
  #+Unix (let ((error-result
    #+Lucid (multiple-value-bind
            (stream error-output-stream exit-status process-id)
          ;; We can't do both :error-output :stream
          ;; and :wait t, so we have to assume that the process
          ;; is finished when when we find out that the error-output-stream
          ;; is done.
          (system::run-program program-name
             :arguments arguments
             :wait nil
             :if-error-output-exists nil
             :error-output :stream)
        (declare (ignore stream process-id))
        (if error-output-stream
            (prog1 (do* ((string (read-line error-output-stream
                    nil
                    nil)
               (read-line error-output-stream
                    nil
                    nil))
             (result nil))
            ((null string) (nreverse result))
               (unless (null string)
           (push string result)))
        (close error-output-stream))
            (format nil "Unknown Unix Error ~D" exit-status)))
    #-Lucid "BOXER-RUN-UNIX-PROGRAM not implemented on this system"))
     (if (stringp error-result)
         (boxer-eval::primitive-signal-error :UNIX-PROGRAM-ERROR error-result)
         nil))
  #-Unix (progn program-name arguments
                "BOXER-RUN-UNIX-PROGRAM not implemented on this system"))

(defun fix-file-alus (top-box &optional (sun->mac? t))
  (labels ((fix-gl (gl)
           (setf (graphics-command-list-alu gl)
                 (convert-file-alu (graphics-command-list-alu gl) sun->mac?))
           (do-vector-contents (gc gl)
             (when (member (aref gc 0) '(0 32) :test #'=)
               ;; if it is an CHANGE-ALU command....
               (setf (aref gc 1) (convert-file-alu (aref gc 1) sun->mac?)))))
           (fix-box (box)
             (let ((graphics-sheet (graphics-sheet box)))
               (when (not (null graphics-sheet))
                 (let ((gl (graphics-sheet-graphics-list graphics-sheet)))
                   (unless (null gl) (fix-gl gl)))))
             (when (sprite-box? box)
               (let* ((turtle (slot-value box 'associated-turtle))
                      (shape (shape turtle))
                      (ws (slot-value turtle 'window-shape)))
                 (fix-gl shape) (fix-gl ws)))))
    (fix-box top-box)
    (do-for-all-inferior-boxes-fast (inf-box top-box) (fix-box inf-box))))

;; order is (sun-alu . mac-alu)
(defvar *file-conversion-alu-alist* '((2 . 3) ; alu-andca
                                      (5 . 0) ; alu-seta
                                      (6 . 2) ; alu-xor
                                      (7 . 1) ; alu-ior
                                      (0 . 11))) ; alu-setz

(defun convert-file-alu (old-alu sun->mac?)
  (if sun->mac?
    (or (cdr (assoc  old-alu *file-conversion-alu-alist* :test #'=)) 0)
    (or (car (rassoc old-alu *file-conversion-alu-alist* :test #'=)) 5)))

(boxer-eval::defboxer-primitive bu::fix-sun-file-graphics ((bu::port-to box))
  (fix-file-alus (box-or-port-target box))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::fix-mac-file-graphics ((bu::port-to box))
  (fix-file-alus (box-or-port-target box) nil)
  boxer-eval::*novalue*)

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

;; similiar to synchronize-graphics-state except we synch to erasing
;; values instead of the intrinsic values of the sprite
(defmethod synchronize-graphics-state-for-erase ((agent graphics-cursor)
                                                 erase-color)
  (let* ((pw (box-interface-value (slot-value agent 'pen-width)))
         (font (box-interface-value (slot-value agent 'type-font)))
         ;; this has to be bound explicitly here because this method can
         ;; be called INSIDE of update-shape
         ;; sgithens 2023-07-12 we are always in :boxer mode now
         ;  (*graphics-command-recording-mode* ':window)
         )
    (unless (eql *graphics-state-current-alu* alu-seta)
      (record-boxer-graphics-command-change-alu alu-seta)
      (change-alu alu-seta))
    (unless (eql *graphics-state-current-pen-width* pw)
      (record-boxer-graphics-command-change-pen-width pw)
      (change-pen-width pw))
    (unless (color= *graphics-state-current-pen-color* erase-color)
      (record-boxer-graphics-command-change-graphics-color erase-color)
      (change-graphics-color erase-color))
    (unless (eql *graphics-state-current-font-no* font)
      (record-boxer-graphics-command-change-graphics-font font)
      (change-graphics-font font))))

;; 2023-06-23 Removing this bit of window-shape update from set-shape
    ;; fixup other slots which depend on the shape...
    ; (update-window-shape-allocation self)
    ;; now we need to initialize the save under...
    (let ((assoc-graphics-box (slot-value self 'assoc-graphics-box))
          (ahead (absolute-heading self))
          (asize (absolute-size self)))
      (unless (null assoc-graphics-box)
        (with-graphics-vars-bound (assoc-graphics-box)
          (update-window-shape (box-interface-value (slot-value self 'shape))
                               (slot-value self 'window-shape)
                               (absolute-x-position self)
                               (absolute-y-position self)
                               (* (cosd ahead) asize) (* (sind ahead) asize)
                               asize)
        )))

;; ... and then from inside defmethod draw
      ;; update the turtle-window-shape
      (unless (turtle-window-shape-valid (slot-value self 'window-shape))
        ;; minimal error checking...
        (unless (= (storage-vector-active-length
                    (box-interface-value (slot-value self 'shape)))
                   (storage-vector-active-length (slot-value self
                                                             'window-shape)))
          (warn "The shape and the window-shape are out of synch, fixing...")
          (update-window-shape-allocation self))
        (update-window-shape (box-interface-value (slot-value self 'shape))
                             (slot-value self 'window-shape)
                             (absolute-x-position self)
                             (absolute-y-position self)
                             (* (cosd ahead) asize) (* (sind ahead) asize)
                             asize)
        )

;; 2023-02-22 Last bits of #-opengl bits
;; from defmethod set-shown?
#-opengl (old-value (box-interface-value slot))
#-opengl (top-guy (top-sprite self))
#-opengl
(unless (or (not explicit) (eq old-value value))
                           ;; if the shown? values are different, then
                           ;; erase (before changing the shown? values)
                           (with-graphics-screen-parameters (erase top-guy)))

#-opengl
(unless (or (not explicit) (eq old-value value))
                           ;; if the shown? values are different, then we need to redraw
                           (with-graphics-screen-parameters
                             (when (shown? top-guy) (draw top-guy))))

;; from defmethod stamp-dot
#-opengl
(with-graphics-screen-parameters
           (dot array-x array-y))

;; from defmethod turtle-rect
#-opengl
(with-graphics-screen-parameters
           (centered-rectangle array-x array-y wid hei))

;; from defmethod hollow-turtle-rect
#-opengl
(with-graphics-screen-parameters
           (hollow-rectangle array-x array-y wid hei))

;; from defmethod stamp-ellipse
#-opengl
(with-graphics-screen-parameters
           (filled-ellipse array-x array-y wid hei))

;; from defmethod stamp-hollow-ellipse
#-opengl
(with-graphics-screen-parameters
           (ellipse array-x array-y wid hei))

;; from defun new-offscreen-copy
#-opengl
(drawing-on-bitmap (new-bm)
                       (copy-offscreen-bitmap alu-seta w h ba 0 0 new-bm 0 0))

;; from defmethod stamp-bitmap
#-opengl
(with-graphics-screen-parameters
           (centered-bitmap nbitmap array-x array-y wid hei))

;; from defmethod type-box
#-opengl
(with-graphics-screen-parameters
           (ecase orientation
                  (:centered (centered-string array-x array-y
                                              (box-text-string box)))
                  (:right (right-string array-x array-y (box-text-string box)))
                  (:left (left-string array-x array-y (box-text-string box)))))

;; from defmethod move-to
#-opengl
(with-graphics-screen-parameters
                                     (line-segment array-x array-y
                                                   array-x-dest array-y-dest))

;; END 2023-02-22

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
;;;; FILE: graphics-clear.lisp
;;;;

;; from defmethod clear-box
        ;; now erase stuff on the screen...
        ;; sgithens 2023-07-22 I don't think anything in this erase box is necessary with
        ;; double buffering
        ; (dolist (screen-box (get-visible-screen-objs  self))
        ;   (unless (eq ':shrunk (display-style screen-box))
        ;     (drawing-on-turtle-slate screen-box
        ;                              (cond ((or (null bg) bitmap-p)
        ;                                     (erase-rectangle gswid gshei 0 0))
        ;                                ((color? bg)
        ;                                 ;; looks like a color so draw a rectangle of that color
        ;                                 (with-pen-color (bg)
        ;                                   (draw-rectangle gswid gshei 0 0)))
        ;                                ;; check for tiling pattern here
        ;                                )
        ;                              ;; now, if only one of the drawing sufaces has been cleared,
        ;                              ;; we need to regenerate the other surface
        ;                              (when (and (not graphics-list-p) graphics-list)
        ;                                ;; regenerate the graphics list
        ;                                ;;  (break "This is it!")
        ;                                (playback-graphics-list-internal graphics-list))
        ;                              (when (and (not bitmap-p) graphics-list-p bit-array)
        ;                                ;; regenerate the background
        ;                                (bitblt-to-screen gswid gshei bit-array
        ;                                                  0 0 0 0)))))

;;;;
;;;; FILE: grfdfs.lisp
;;;;

;; this is set to Nil for the mac because of the bug which causes
;; allocation of new bitmaps to take a LONG time
(defvar *add-new-graphics-sheet-bit-array?* #+mcl nil #-mcl t
  "Automatically allocate new bit-arrays for graphics sheets when
   they need them e.g. the STAMP primitive")

;; 2023-07-20 sgithens Old version of draw-wrap-line that took the window-system
;; integer coords
(defun draw-wrap-line (from-x from-y to-x to-y alu)
  (let* ((delta-x (- to-x from-x))
         (delta-y (- to-y from-y))
         (islope (unless (zerop delta-y) (/ delta-x delta-y)))
         (slope (unless (zerop delta-x) (/ delta-y delta-x))))
    (flet ((wrap-y-coord-top (y) (+ y %drawing-height))
           (wrap-y-coord-bottom (y) (- y %drawing-height))
           (wrap-x-coord-left (x) (+ x %drawing-width))
           (wrap-x-coord-right (x) (- x %drawing-width))
           (beyond-top? (y) (minusp y))
           (beyond-bottom? (y) (>= y %drawing-height))
           (beyond-left? (x) (minusp x))
           (beyond-right? (x) (>= x %drawing-width))
           (top-x-intercept (x y) (unless (null islope)
                                    (+ x (round (* islope (- y))))))
           (bottom-x-intercept (x y) (unless (null islope)
                                       (+ x
                                           (round (* islope (- %drawing-height
                                                                y 1))))))
           (left-y-intercept (x y) (unless (null slope)
                                     (+ y (round (* slope (- x))))))
           (right-y-intercept (x y) (unless (null slope)
                                      (+ y (round (* slope (- %drawing-width
                                                                x 1)))))))
      (declare (inline wrap-y-coord-top wrap-y-coord-bottom
                       wrap-x-coord-right wrap-x-coord-left
                       beyond-top? beyond-bottom? beyond-right? beyond-left?))

      (flet ((line-right-then-continue (y-intercept)
               (draw-line (scale-x from-x) (scale-y from-y)
                           (scale-x (1- %drawing-width))
                           (scale-y y-intercept))
               ;; now recurse
               (draw-wrap-line 0 y-intercept
                               (wrap-x-coord-right to-x) to-y alu))
             (line-top-then-continue (x-intercept)
               (draw-line (scale-x from-x) (scale-y from-y)
                                       (scale-x x-intercept) (scale-y 0))
               (draw-wrap-line x-intercept (1- %drawing-height)
                               to-x (wrap-y-coord-top to-y) alu))
             (line-left-then-continue (y-intercept)
               (draw-line (scale-x from-x) (scale-y from-y)
                                       (scale-x 0) (scale-y y-intercept))
               (draw-wrap-line (1- %drawing-width) y-intercept
                               (wrap-x-coord-left to-x) to-y alu))
             (line-bottom-then-continue (x-intercept)
               (draw-line (scale-x from-x) (scale-y from-y)
                                       (scale-x x-intercept) (scale-y (1- %drawing-height)))
               (draw-wrap-line x-intercept 0
                               to-x (wrap-y-coord-bottom to-y) alu))
             (break-line-left (y-intercept)
               (draw-wrap-line (wrap-x-coord-left from-x) from-y
                               (1- %drawing-width) y-intercept alu)
               (draw-wrap-line 0 y-intercept to-x to-y alu))
             (break-line-top (x-intercept)
               (draw-wrap-line from-x (wrap-y-coord-top from-y)
                               x-intercept (1- %drawing-height) alu)
               (draw-wrap-line x-intercept 0 to-x to-y alu))
             (break-line-right (y-intercept)
               (draw-wrap-line (wrap-x-coord-right from-x) from-y
                               0 y-intercept alu)
               (draw-wrap-line (1- %drawing-width) y-intercept to-x to-y alu))
             (break-line-bottom (x-intercept)
               (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
                               x-intercept 0 alu)
               (draw-wrap-line x-intercept (1- %drawing-height)
                               to-x to-y alu)))
        (cond ((point-in-array? from-x from-y)
               ;; check for the simple cases instead of falling
               ;; to optimize the common case
               (cond ((point-in-array? to-x to-y)
                      ;; the simple, simple case
                      (draw-line (scale-x from-x) (scale-y from-y)
                                              (scale-x to-x)   (scale-y to-y)))
                     ((beyond-right? to-x)
                      ;; note that if the line extends beyond a
                      ;; horizontal boundary, it can't be vertical
                      (let ((y-intercept (right-y-intercept from-x from-y)))
                        (cond ((y-in-array? y-intercept)
                               ;; we are sure it intersects a vertical edge
                               (line-right-then-continue y-intercept))
                              ((beyond-top? to-y) ; y-intercept ?
                               ;; must intersect with the top edge instead
                               (line-top-then-continue
                                (top-x-intercept from-x from-y)))
                              (t
                               ;; must intersect with the bottom edge
                               (line-bottom-then-continue
                                (bottom-x-intercept from-x from-y))))))
                     ((beyond-left? to-x)
                      ;; if it's not inside, or beyond the right edge,
                      ;; it must be beyond the left edge
                      (let ((y-intercept (left-y-intercept from-x from-y)))
                        (cond ((y-in-array? y-intercept)
                               ;; we are sure it intersects a vertical edge
                               (line-left-then-continue y-intercept))
                              ((beyond-top? to-y) ; y-intercept ?
                               ;; must intersect with the top edge instead
                               (line-top-then-continue
                                (top-x-intercept from-x from-y)))
                              (t
                               ;; must intersect with the bottom edge
                               (line-bottom-then-continue
                                (bottom-x-intercept from-x from-y))))))
                     ((beyond-top? to-y)
                      (line-top-then-continue (top-x-intercept from-x from-y)))
                     (t
                      (line-bottom-then-continue (bottom-x-intercept from-x
                                                                     from-y)))))
              ((beyond-right? from-x)
               (let ((right-y-intercept (right-y-intercept to-x to-y)))
                 (cond ((and (not (beyond-right? to-x))
                             (y-in-array? right-y-intercept))
                        ;; break the line on the right edge
                        (break-line-right right-y-intercept))
                       (t;; otherwise wrap, and try again...
                        (draw-wrap-line (wrap-x-coord-right from-x) from-y
                                        (wrap-x-coord-right to-x)   to-y   alu)))))
              ((beyond-left? from-x)
               (let ((left-y-intercept (left-y-intercept to-x to-y)))
                 (cond ((and (not (beyond-left? to-x))
                             (y-in-array? left-y-intercept))
                        ;; break the line on the right edge
                        (break-line-left left-y-intercept))
                       (t;; just wrap both coords and try again
                        (draw-wrap-line (wrap-x-coord-left from-x) from-y
                                        (wrap-x-coord-left to-x)   to-y   alu)))))
              ((beyond-top? from-y)
               (let ((top-x-intercept (top-x-intercept to-x to-y)))
                 (cond ((and (not (beyond-top? to-y))
                             (x-in-array? top-x-intercept))
                        (break-line-top top-x-intercept))
                       (t
                        (draw-wrap-line from-x (wrap-y-coord-top from-y)
                                        to-x   (wrap-y-coord-top to-y)   alu)))))
              (t;; from-y must be beyond the bottom line
               (let ((bottom-x-intercept (bottom-x-intercept to-x to-y)))
                 (cond ((and (not (beyond-bottom? to-y))
                             (x-in-array? bottom-x-intercept))
                        (break-line-bottom bottom-x-intercept))
                       (t
                        (draw-wrap-line from-x (wrap-y-coord-bottom from-y)
                                        to-x   (wrap-y-coord-bottom to-y) alu))))))))))

;; this tries to save/restore only the extents of the turtle rather than
;; the entire size of the allocated save-under which must take into account
;; possible rotations
;;
;; Also, clip to the containing graphics-box's dimensions

;; sgithens 2023-06-12 Completely removing remainder of save-under
(defun save-under-turtle (turtle)
  (declare (ignore turtle))
  (log:debug "Is save-under-turtle really necessary?"))

(defun restore-under-turtle (turtle)
  (declare (ignore turtle))
  (log:debug "Is restore-under-turtle really necessary?"))



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

;; sgithens 2023-08-24 I believe we can get rid of this and just use enclosing-rectangle
(defun enclosing-sprite-coords (sprite)
  (multiple-value-bind (left top right bottom)
      (enclosing-rectangle sprite)
    (unless (no-graphics?)
      (values left top right bottom))))

;; sgithens 2023-08-23 I don't think this draw-update does anything at all anymore...
;;; like draw but without the actual drawing
(defmethod draw-update ((self button))
  (let ((ahead (absolute-heading self)) (asize (absolute-size self)))
    (unless (eq (shown? self) ':no-subsprites)
      (dolist (subs (slot-value self 'subsprites))
        (draw-update subs)))))

;; sgithens 2023-07-25 no longer needed, was only used in flash-name
(defun calc-name-position-x (length left right)
  (setq left (array-coordinate-x left)
        right (array-coordinate-x right))
  (if (> (+ right length) %drawing-width)
      (fixr (- left length 3.))
      (fixr (+ right 5.))))

;; sgithens 2023-07-25 no longer needed, was only used in flash-name
(defun calc-name-position-y (height top bottom)
  (let ((center (+ (array-coordinate-y top)
                   (/ (- top bottom) 2))))
    (fixr (min (max center 0)
               (- %drawing-height height 1.)))))

;;; Drawing the turtle's name

#|
CLOSED for renovations until I fix the string/font situation

(defmethod flash-name ((self turtle))
    (let* ((print-name (name sprite-box))
           (name-length (sting-wid ))) ;;; fix this
      (multiple-value-bind (left top right bottom)
          (enclosing-rectangle self)
        (let ((x-pos (calc-name-position-x name-length left RIGHT))
              (Y-POS (CALC-NAME-POSITION-Y *FONT-HEIGHT* TOP BOTTOM)))
          (DRAW-STRING-TO-GBOX PRINT-NAME X-POS Y-POS)
          (PROCESS-SLEEP 120 "Pausing to flash name")
          (DRAW-STRING-TO-GBOX PRINT-NAME X-POS Y-POS)))))

|#

; 2023-07-17 sgithens Did we used to have a fence mode? Commented out stuff from defmethod move-to
                  ;; Have to make fence mode work some other time
;		  ((and (eq %draw-mode ':fence)
;			(not (point-in-array? array-x-dest array-y-dest)))
;		   (error "you hit the fence"))

;; 2023-06-24 Old version that used window-shape, and the old version of enclosing-sprite-coords
;; that used it.
(defmethod enclosing-rectangle ((self button))
  ;; first insure the validity of the window-shape
  (let ((ws (slot-value self 'window-shape)))
    (unless (turtle-window-shape-valid ws)
      (when *boxer-system-hacker*
        (warn "Updating window shape inside of ENCLOSING-RECTANGLE"))
      (let ((ahead (absolute-heading self))
            (asize (absolute-size self)))
        (unless (= (storage-vector-active-length
                    (box-interface-value (slot-value self 'shape)))
                   (storage-vector-active-length (slot-value self
                                                             'window-shape)))
          (warn "The shape and the window-shape are out of synch, fixing...")
          (update-window-shape-allocation self))
        (update-window-shape (box-interface-value (slot-value self 'shape))
                             ws
                             (absolute-x-position self)
                             (absolute-y-position self)
                             (* (cosd ahead) asize) (* (sind ahead) asize)
                             asize)))
    ;; now fill the extents slots if needed
    (when (null (turtle-window-shape-min-graphics-x-extent ws))
      (update-turtle-window-extents ws self
                                    ;; ?? is this neccesary ??
                                    (fix-array-coordinate-x
                                     (absolute-x-position self))
                                    (fix-array-coordinate-y
                                     (absolute-y-position self))))
    ;; finally return the values
    (values (turtle-window-shape-min-graphics-x-extent ws)
            (turtle-window-shape-min-graphics-y-extent ws)
            (turtle-window-shape-max-graphics-x-extent ws)
            (turtle-window-shape-max-graphics-y-extent ws))))

(defun enclosing-sprite-coords (sprite)
   (multiple-value-bind (left top right bottom)
       (enclosing-rectangle sprite)
     (unless (no-graphics?)
       (values (user-coordinate-x left)  (user-coordinate-y top)
               (user-coordinate-x right) (user-coordinate-y bottom)))))


;; 2023-06-23 Old version that used window-shape. Current version in
;; code uses the boxer native gcdispl in the shape slot.
(defmethod sprite-at-window-point ((self button) window-x window-y)
  ;; first insure the validity of the window-shape
  (let ((ws (slot-value self 'window-shape)))
    (unless (turtle-window-shape-valid ws)
      (when *boxer-system-hacker*
        (warn "Updating window shape inside of ENCLOSING-RECTANGLE"))
      (let ((ahead (absolute-heading self))
            (asize (absolute-size self)))
        (unless (= (storage-vector-active-length
                    (box-interface-value (slot-value self 'shape)))
                   (storage-vector-active-length (slot-value self
                                                             'window-shape)))
          (warn "The shape and the window-shape are out of synch, fixing...")
          (update-window-shape-allocation self))
        (update-window-shape (box-interface-value (slot-value self 'shape))
                             ws
                             (absolute-x-position self)
                             (absolute-y-position self)
                             (* (cosd ahead) asize) (* (sind ahead) asize)
                             asize)))
    ;; now fill the extents slots if needed
    (when (null (turtle-window-shape-min-graphics-x-extent ws))
      (update-turtle-window-extents ws self
                                    ;; ?? is this neccesary ??
                                    (fix-array-coordinate-x
                                     (absolute-x-position self))
                                    (fix-array-coordinate-y
                                     (absolute-y-position self))))
    ;; now we can use the cache values
    (when (and (<= (turtle-window-shape-min-graphics-x-extent ws)
                    window-x
                    (turtle-window-shape-max-graphics-x-extent ws))
               (<= (turtle-window-shape-min-graphics-y-extent ws)
                    window-y
                    (turtle-window-shape-max-graphics-y-extent ws)))
      ;; the point is within the sprite, now check any subsprites
      (or (dolist (sub (slot-value self 'subsprites))
            (let ((under? (sprite-at-window-point sub window-x window-y)))
              (when (not (null under?))
                (return under?))))
          self))))

(defun update-turtle-window-extents (window-shape turtle
                                                  &optional
                                                  (array-x 0) (array-y 0))
  (let ((min-x array-x) (min-y array-y) (max-x array-x) (max-y array-y)
        (visible? (shown? turtle)))
    (unless (eq visible? ':subsprites)
      (with-graphics-state-bound
        (do-vector-contents (gc window-shape)
          (multiple-value-bind (gc-min-x gc-min-y gc-max-x gc-max-y
                                         state-change?)
                               (graphics-command-extents gc)
            (unless state-change?
              (setq min-x (min gc-min-x min-x)
                    min-y (min gc-min-y min-y)
                    max-x (max gc-max-x max-x)
                    max-y (max gc-max-y max-y)))))))
    (unless (eq visible? ':no-subsprites)
      (dolist (subs (slot-value turtle 'subsprites))
        (when (absolute-shown? subs)
          (multiple-value-bind (sub-min-x sub-min-y sub-max-x sub-max-y)
            (enclosing-rectangle subs)
            (setq min-x (min min-x sub-min-x)
                  min-y (min min-y sub-min-y)
                  max-x (max max-x sub-max-x)
                  max-y (max max-y sub-max-y))))))
    (setf (turtle-window-shape-min-graphics-x-extent window-shape) min-x
          (turtle-window-shape-min-graphics-y-extent window-shape) min-y
          (turtle-window-shape-max-graphics-x-extent window-shape) max-x
          (turtle-window-shape-max-graphics-y-extent window-shape) max-y)
    (values min-x min-y max-x max-y)))

;;;; Shape Handling

;;;
;;; Note that we deliberately allocate a backing store large enough
;;; to support any possible rotation of the sprite to avoid having to
;;; continually reallocate a backing store (slow !) for any rotation
;;; of the sprite.
;;;
;;; Need to put in support for overlay planes
;;;

(defmethod update-window-shape-allocation ((self button))
  (let ((window-shape (slot-value self 'window-shape))
        (shape (box-interface-value (slot-value self 'shape))))
    (unless (null window-shape)
      (clear-graphics-list window-shape))
    (do-vector-contents (gc shape)
      (sv-append window-shape (allocate-boxer->window-command gc)))))

(defun update-window-shape (shape window-shape
                                  trans-x trans-y cos-scale sin-scale scale)
  (do-vector-contents (turtle-graphics-command shape :index-var-name idx)
    (translate-boxer->window-command turtle-graphics-command
                                     (sv-nth idx window-shape)
                                     trans-x trans-y 1.0 0.0 ;sgithens hacking cos-scale sin-scale
                                     scale))
  (setf (turtle-window-shape-valid window-shape) t))

;;; When a window shape is invalidated, we have to recurse through the
;;; inferiors as well

(defmethod invalidate-window-shape-and-extent-caches ((self button))
  (setf (turtle-window-shape-valid (slot-value self 'window-shape))
        nil
        (turtle-window-shape-min-graphics-x-extent
         (slot-value self 'window-shape))
        nil)
  ;; now recurse
  (dolist (ss (slot-value self 'subsprites))
    (invalidate-window-shape-and-extent-caches ss)))


;;; END Shape Handling

(defmethod fast-erase ((self graphics-object))
  (error "You MUST define a FAST-ERASE method for the ~S class"
         (class-name (class-of self))))

(defmethod erase ((self graphics-object))
  (error "You MUST define a ERASE method for the ~S class"
         (class-name (class-of self))))

;;; need to support overlay here...
(defmethod fast-erase ((turtle button))
  (unless (eq turtle *current-active-sprite*)
    (if (eq (turtle-save-under turtle) 'xor-redraw)
        (draw turtle)
        (restore-under-turtle turtle))))

(defmethod erase ((self button))
  (when (absolute-shown? self) (fast-erase self)))


;;; The save-under slot-of a turtle can be the symbol 'XOR-REDRAW,
;;; or an offscreen bitmap or NIL (for a freshly CONSed turtle)
;;; XOR mode is faster than save-unders so if we can, use XOR mode

;; sgithens TODO 2023-06-12 Completely removing remainder of save-under
(defmethod update-save-under ((self button))
  (declare (ignore self))
  (log:debug "Is update-save-under really necessary?")
)

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

;; 2023-06-22 Removing from defclass button
(window-shape :initform nil
                 :accessor turtle-window-shape)

;; 2023-06-12 Removing from defclass button
(save-under :initform nil
               :accessor turtle-save-under)

;; sgithens TODO 2023-06-12 Completely removing remainder of save-under
;; soon to be '(xor-redraw overlay)
(defvar *valid-save-under-keywords* '(xor-redraw))

;; sgithens TODO 2023-06-12 Completely removing remainder of save-under
(defstruct (save-under (:constructor make-save-under (bitmap middle size)))
  (bitmap nil)
  (middle 0)
  (size))

;;; Some useful variables that various types of objects need

(defconstant *default-graphics-object-height* 10.0)

(defconstant *default-graphics-object-width* 10.0)

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
(boxer-eval::defboxer-key bu::ctrl-mouse-click-on-scroll-bar        com-mouse-page-scroll-box) ;; was previously Command on MacOS
(boxer-eval::defboxer-key bu::option-mouse-click-on-scroll-bar         com-mouse-page-scroll-box)
(boxer-eval::defboxer-key bu::alt-mouse-click-on-scroll-bar         com-mouse-page-scroll-box)

(boxer-eval::defboxer-key bu::mouse-double-click-on-scroll-bar         com-mouse-limit-scroll-box)
(boxer-eval::defboxer-key bu::ctrl-mouse-right-click-on-scroll-bar com-mouse-limit-scroll-box)
(boxer-eval::defboxer-key bu::alt-mouse-right-click-on-scroll-bar  com-mouse-limit-scroll-box)
(boxer-eval::defboxer-key bu::option-mouse-double-click-on-scroll-bar  com-mouse-limit-scroll-box)
(boxer-eval::defboxer-key bu::ctrl-mouse-double-click-on-scroll-bar com-mouse-limit-scroll-box) ;; was previously Command on MacOS


(boxer-eval::defboxer-key (bu::>-key 2) com-fat)
(boxer-eval::defboxer-key (bu::<-key 2) com-nutri-system)

#+sun (boxer-eval::defboxer-key bu::R2-key com-print-screen)

#+sun (boxer-eval::defboxer-key (bu::R2-key 2) com-print-screen-to-file)

#+apple (boxer-eval::defboxer-key (bu::return-key 1) com-doit-now) ; should be com-step

;;;;
;;;; FILE: loader.lisp
;;;;

#-lispworks
(defun code-char-value (value) (code-char value))

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

;; From defun load-box
  #+mcl (ccl::update-cursor) ; hack to get moving cursor


;;;
;;; Versions of #+mcl code for using a different "fast" version of pixmap loaders
;;;

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

#+mcl
(defvar *use-mac-fast-bitmap-loaders* t)

;; From define-load-command-for-value bin-op-pixmap
       #+mcl (if *use-mac-fast-bitmap-loaders*
               (fast-mac-load-8-bit-run-length-encoded-pixmap stream)
               (load-8-bit-run-length-encoded-pixmap-by-strips stream))

       #+mcl (if *use-mac-fast-bitmap-loaders*
               (fast-mac-load-true-color-run-length-encoded-pixmap stream)
               (load-true-color-run-length-encoded-pixmap-by-strips stream))

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

;; Pre-opengl versions of code from load-8-bit-run-length-encoded-pixmap
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

;; Pre-opengl versions of code from load-true-color-run-length-encoded-pixmap
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

;;;;
;;;; FILE: lw-menu.lisp
;;;;

;; capi:prompt-for-file chokes on raw mac filenames (with ":"'s)
(defun massage-pathname (pathname)
  (make-pathname :directory (pathname-directory pathname)
                 :name      (pathname-name      pathname)
                 :type      (pathname-type      pathname)))


;; On the mac, these read/wrote into the a file's resource fork
(defun boxer::write-boxer-file-info (pathname &key read-only? world-box? flags)
  (declare (ignore pathname read-only? world-box? flags))
  nil)

(defun boxer::boxer-file-info (pathname) (declare (ignore pathname)) nil)


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

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-LIST-INTO-LIST (INTO-LIST LIST BEFORE-ITEM)
  `(SETF ,INTO-LIST (SPLICE-LIST-INTO-LIST-1 ,INTO-LIST ,LIST ,BEFORE-ITEM)))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-ITEM-INTO-LIST (INTO-LIST ITEM BEFORE-ITEM)
  `(SETF ,INTO-LIST (SPLICE-LIST-INTO-LIST-1 ,INTO-LIST `(,,ITEM) ,BEFORE-ITEM)))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
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

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-ITEM-OUT-OF-LIST (OUT-OF-LIST ITEM)
  `(SETF ,OUT-OF-LIST (DELETE ,ITEM ,OUT-OF-LIST)))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-ITEM-AND-TAIL-OUT-OF-LIST (OUT-OF-LIST ITEM)
  `(SETF ,OUT-OF-LIST (SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-1 ,OUT-OF-LIST ,ITEM)))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFUN SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-1 (OUT-OF-LIST ITEM)
  (LET ((ITEM-POSITION (POSITION ITEM OUT-OF-LIST)))
    (COND ((NULL ITEM-POSITION) OUT-OF-LIST)
          ((=& ITEM-POSITION 0) NIL)
          (T (RPLACD (NTHCDR (-& ITEM-POSITION 1) OUT-OF-LIST) NIL)
             OUT-OF-LIST))))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
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

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-LIST-INTO-LIST-AT (INTO-LIST LIST POSITION)
  `(COND ((zerop& ,POSITION)
          (SETF ,INTO-LIST (NCONC ,LIST ,INTO-LIST)))
         ((>=& ,POSITION (LENGTH ,INTO-LIST))
          (SETF ,INTO-LIST (NCONC ,INTO-LIST ,LIST)))
         (T (SETF ,INTO-LIST (NCONC (SUBSEQ ,INTO-LIST 0 ,POSITION)
                                    ,LIST
                                    (NTHCDR ,POSITION ,INTO-LIST))))))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-ITEM-INTO-LIST-AT (INTO-LIST ITEM POSITION)
  `(SPLICE-LIST-INTO-LIST-AT ,INTO-LIST `(,,ITEM) ,POSITION))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-ITEM-OUT-OF-LIST-AT (LIST POSITION)
  `(COND ((=& ,POSITION 0)
          (SETF ,LIST (CDR ,LIST)))
         ((>=& ,POSITION (LENGTH ,LIST))
          (SETF ,LIST (BUTLAST ,LIST)))
         (T (SETF ,LIST (NCONC (SUBSEQ ,LIST 0 ,POSITION)
                               (NTHCDR (+& ,POSITION 1) ,LIST))))))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
(DEFMACRO SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-FROM (LIST POSITION)
  `(COND ((>=& ,POSITION (LENGTH ,LIST)))
         (T (SETF ,LIST (SUBSEQ ,LIST 0 ,POSITION)))))

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
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

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
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

;; sgithens 2023-06-04 TODO Doesn't seem to be referenced anywhere
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
;;;; FILE: mcl-utils.lisp
;;;;

;; (defun cf (f)
;;   (let* ((sysprops (sm::system-properties (sm::find-system-named 'boxer)))
;;          (sp (getf sysprops :pathname-default))
;;          (bp (getf sysprops :binary-pathname-default)))
;;   (compile-file (merge-pathnames f sp)
;;                 :output-file (merge-pathnames (sm::make-binary-pathname f)
;;                                               (if (null bp) sp bp)))))

(defun compops ()
  (proclaim '(optimize (speed 3) (space 2) (compilation-speed 0)
              ;; these next ones happen to be the defaults
              (safety 1) (debug 1)))
  (format t "~&Compiler Options are:~&")
  #+mcl
  (ccl::declaration-information 'optimize))

(defun lcf (file-or-files &optional (load? t))
  (cond ((and (listp file-or-files) load?)
         (dolist (f file-or-files) (load (cf f))))
        ((listp file-or-files)
         (dolist (f file-or-files) (cf f)))
        (load?
         (load (cf file-or-files)))
        (t (cf file-or-files))))

;; printing help

(defun pa (x) (let ((*print-array* t)) (print x)))
(defun pp (x) (let ((*print-pretty* t)) (print x)))

;;; this is general, but I don't know where to put it
(defun whereis (command)
  (get command :on-keys))

(defun test-loop ()
  (loop
    (terpri)
    (princ "Boxer> ")
    (funcall (read))
    (repaint)
    ))

;;; elsewhere
;(setq *boxer-version-info* "Experimental Mac Boxer")

#+mcl
(defun keys-code ()
  (format t "Press the key...")
  (multiple-value-bind (message modifiers)
                       (ccl::wait-key-event)
    (let ((char-code (ldb (byte 8 0) message))
          (key-code (ldb (byte 8 8) message)))
      (format t "~&Char ~A (~C); Key ~A, Modifiers ~A" char-code
              (code-char char-code) key-code modifiers))))

;;; Editor support
#+mcl
(progn
  (defun add-def-type (definer &optional (type 'function))
      (when (boundp 'ccl::*define-type-alist*)
  (pushnew (string-downcase (subseq (symbol-name definer) 3))
     (cdr (assoc type ccl::*define-type-alist*))
             :test #'equal)))

  (add-def-type 'boxer-eval::defboxer-primitive 'function)
  (add-def-type 'boxer-eval::defrecursive-funcall-primitive 'function)
  (add-def-type 'boxer-eval::defrecursive-eval-primitive 'function)
  (add-def-type 'box::defsprite-function 'function)
  (add-def-type 'box::defboxer-command 'function)
  )

#|
;; this was for pre 4.3 LWW
#+lispworks
(progn
  (editor:define-top-level-form-parser boxer-eval::defboxer-primitive
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser boxer-eval::defrecursive-funcall-primitive
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser boxer-eval::defrecursive-eval-primitive
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser defsprite-function
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser defboxer-command
       editor::section-parse-name-only)
  ;; we have our own private version of defmethod so inform the editor
  ;; this is the result of (editor::get-parser 'clos::defmethod)
  (editor:define-top-level-form-parser defmethod
      EDITOR::SECTION-PARSE-DEFMETHOD-TLF)
)
|#

#+mcl
(eval-when (load)
  (format t "~&Changing control key mapping and Paste with styles")
  (setq #-ccl-5.0 ccl::*control-key-mapping* #-ccl-5.0 :command
        ccl::*autoload-traps* t
        ccl::*paste-with-styles* nil)
  )


;; file header parsing
(defun mod-history-start-line? (string)
  (search "Modification History" string :test #'string-equal))

(defun mod-history-end-line? (string)
  (or (search "Start logging" string :test #'string-equal)
      (search "Started " string :test #'string-equal)
      (search "in-package" string :test #'string-equal)))

(defun mod-history-date-line? (string)
  (let ((month nil) (date nil) (year nil) (field :month) (1st-char nil))
    (dotimes (i (length string))
      (let ((char (char string i)))
        (cond ((char= char #\space))
              ((digit-char-p char)
               (if (null 1st-char)
                 (setq 1st-char (digit-char-p char))
                 (case field
                   (:month (setq month (+ (* 1st-char 10) (digit-char-p char))
                                 1st-char nil))
                   (:date (setq date (+ (* 1st-char 10) (digit-char-p char))
                                1st-char nil))
                   (:year (setq year (+ (* 1st-char 10) (digit-char-p char))
                                1st-char nil)
                          (return (values T month date year))))))
              ((char= char #\/)
               (case field
                 (:month (cond ((and (null 1st-char) (null month)) (return nil))
                               ((null month)
                                (setq month 1st-char field :date))
                               (t (setq field :date))))
                 (:date (when (null date) (setq date 1st-char))
                        (setq field :year)))
               (setq 1st-char nil))
              ((eq field :month) (return nil)))))))

;; try and capture multiline entries which don't all start with a date...
(defun get-mod-history-lines (file &optional
                                   (outstream *standard-output*)
                                   (show-filename? t))
  (let ((eof-value (list 'eof))
        (date-line? nil) (start? nil) (count 0))
    (with-open-file (in file)
      (loop
       (let ((line (read-line in nil eof-value)))
         (cond ((eq line eof-value) (return nil))
               ((null start?)
                (when (mod-history-start-line? line) (setq start? t)))
               ((mod-history-end-line? line) (return count))
               ((mod-history-date-line? line)
                (setq date-line? t) (incf count)
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))
               ((not (null date-line?))
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))))))))

(defun get-mod-history-lines-after (file month date year
                                         &optional
                                         (outstream *standard-output*)
                                         (show-filename? t))
  (let ((eof-value (list 'eof))
        (start? nil)
        (date-line? nil)
        (count 0))
    (with-open-file (in file)
      (loop
       (let ((line (read-line in nil eof-value)))
         (cond ((eq line eof-value) (return nil))
               ((null start?)
                (when (mod-history-start-line? line) (setq start? t)))
               ((mod-history-end-line? line) (return count))
               ((multiple-value-bind (is-date-line? lmonth ldate lyear)
                    (mod-history-date-line? line)
                  (and is-date-line?
                       (let ((good-date?
                              (or (> (mod (+ 50 lyear) 100) (mod (+ 50 year) 100))
                                  (and (= (mod (+ 50 lyear) 100)
                                          (mod (+ 50 year) 100))
                                       (or (> lmonth month)
                                           (and (= lmonth month)
                                                (>= ldate date)))))))
                         (unless good-date? (setq date-line? nil))
                         good-date?)))
                (setq date-line? t) (incf count)
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))
               ((not (null date-line?))
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))))))
    count))

#|
;; sgithens - no longer using the 'sm' version of package defining
;; (dolist (file (sm::system-source-files (sm::find-system-named 'boxer)))
;;   (get-mod-history-lines-after file 9 1 98))

;; (with-open-file (out "C:\\Boxer\\ChangeLog" :direction :output
;;                      :if-exists :supersede :if-does-not-exist :create)
;;   (let ((total-changes 0))
;;     (dolist (file (sm::system-source-files (sm::find-system-named 'boxer)))
;;       (format out "~&     ~A   ~&" (enough-namestring file))
;;       (format out "~&     ~D entries~&"
;;               (let ((changes (get-mod-history-lines-after file 9 1 98 out nil)))
;;                 (incf total-changes changes)
;;                 changes)))
;;     (format out "~&~&Total Changes = ~D" total-changes)))

;; a histogram


(defvar *modarray* (make-array 50 :initial-element 0))

;; (dolist (file (sm::system-source-files (sm::find-system-named 'boxer)))
;;   (let ((eof-value (list 'eof))
;;         (start? nil))
;;     (with-open-file (in file)(loop
;;         (let ((line (read-line in nil eof-value)))
;;           (cond ((eq line eof-value) (return nil))
;;                 ((null start?)
;;                  (when (mod-history-start-line? line) (setq start? t)))
;;                 ((mod-history-end-line? line) (return nil))
;;                 ((multiple-value-bind (date-line? lmonth ldate lyear)
;;                      (mod-history-date-line? line)
;;                    (and date-line?
;;                         (incf (aref *modarray*
;;                                     (cond ((zerop lyear) (+ lmonth 36))
;;                                           ((< lyear 97) 0)
;;                                           ((= lyear 97) lmonth)
;;                                           ((= lyear 98) (+ lmonth 12))
;;                                           ((= lyear 99) (+ lmonth 24))))))))))))))
|#


;;; source code utilities

;; lines in PC files are CRLF terminated
(defun mcl-file->pc-file (mac-file-in pc-file-out)
  (with-open-file (in mac-file-in)
    (with-open-file (out pc-file-out :direction :output :if-exists :supersede)
      (let ((eof-value (list 'eof)))
        (loop (let ((c (read-char in nil eof-value)))
                (cond ((eq c eof-value) (return nil))
                      ((char= c #\NewLine)
                       (write-char #\NewLine out)
                       (let ((nextchar (peek-char nil in nil nil)))
                         (unless (and nextchar (char= nextchar #\LineFeed))
                           (write-char #\LineFeed out))))
                      (t (write-char c out))))))))
  #+mcl
  (ccl::set-file-write-date pc-file-out (file-write-date mac-file-in))
  )

;; lines in MCL files are only CR terminated
;; lines in PC lisp are LF terminated, NOTE: meaning of #\NewLine is platform
;; dependent !!!!
(defun pc-file->mcl-file (pc-file-in mac-file-out)
  (with-open-file (in pc-file-in)
    (with-open-file (out mac-file-out :direction :output :if-exists :supersede)
      (let ((eof-value (list 'eof)))
        (loop (let ((c (read-char in nil eof-value)))
                (cond ((eq c eof-value) (return nil))
                      ((char= c #\cr)
                       ;; make sure only 1 #\CR is issued for CRLF or CR
                       (write-char #\cr out)
                       (let ((nextchar (peek-char nil in nil nil)))
                         (when (and nextchar (char= nextchar #\LineFeed))
                           ;; if there is a LF, read it in an ignore it...
                           (read-char in))))
                      ((char= c #\lf) (write-char #\cr out))
                      (t (write-char c out))))))))
  ;; no equivalent in LW (sigh)
  ;(ccl::set-file-write-date mac-file-out (file-write-date pc-file-in)))
  )

(defun no-convert (in-file out-file)
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction ':output :if-exists ':overwrite :if-does-not-exist ':create)
      (let ((eof-value (list 'eof)))
        (loop (let ((c (read-char in nil eof-value)))
                (cond ((eq c eof-value) (return nil))
                      (t (write-char c out)))))))))

;; need special handling for  "boxer.rsrc"
(defvar *special-source-files* '("devlog" "plst.txt" "deliver-boxer.sh"))

;; (defun update-source-files (synchfile dest-dir
;;                                     &optional (file-conversion #'no-convert)
;;                                     (system (sm::find-system-named 'boxer)))
;;   (let* ((start-utime (cond ((listp synchfile)
;;                              (prog1
;;                                  (apply #'encode-universal-time (cdr synchfile))
;;                                (setq synchfile (car synchfile))))
;;                             (t
;;                              (file-write-date synchfile))))
;;          (source-dir (getf (sm::system-properties system) :pathname-default))
;;          (all-source-files (append (directory (merge-pathnames "*.lisp" source-dir))
;;                                    (with-collection
;;                                      (dolist (sf *special-source-files*)
;;                                        (collect (merge-pathnames sf source-dir))))))
;;          (source-files (remove-if #'(lambda (f)
;;                                       (let ((fwd (file-write-date f)))
;;                                         (cond ((numberp fwd) (<= fwd start-utime))
;;                                               (t (error "No write date for ~A" f)))))
;;                                   all-source-files)))
;;     (multiple-value-bind (sec min hou date month year)
;;         (decode-universal-time start-utime)
;;       (format t "~%Files changed since ~D:~D:~D on ~D/~D/~D"
;;               hou min sec month date year))
;;     (format t "~%Selected ~D of ~D files.  "
;;             (length source-files) (length all-source-files))
;;     (when (y-or-n-p "List ? ")
;;       (dolist (f source-files) (print f)))
;;     (when (y-or-n-p "Update changed files to ~S ?"
;;                     (merge-pathnames "*.lisp" dest-dir))
;;       (dolist (f source-files)
;;         (let ((newpath (merge-pathnames (make-pathname :name (pathname-name f)
;;                                                        :type (pathname-type f))
;;                                         dest-dir)))
;;           (format t "~%Updating ~S to ~S" f newpath)
;;           (funcall file-conversion f newpath)))
;;       (when (y-or-n-p "Update synch file ?")
;;         (with-open-file (sfs synchfile :direction ':output :if-exists ':append)
;;           (multiple-value-bind (sec min hou date month year)
;;               (decode-universal-time (get-universal-time))
;;             (declare (ignore sec min hou))
;;             (format sfs "~%~D/~D/~D Synched source files~%" month date year)))
;; ;        (funcall file-conversion synchfile (make-pathname :name (pathname-name synchfile)
;; ;                                                          :type (pathname-type synchfile)
;; ;                                                          :defaults dest-dir))
;;         ))))

#|


(update-source-files "/Boxer/SRC/OpenGL/SYNCHROSTAMP" "/Users/edwardlay/Public/Drop Box/")

(update-source-files "/Boxer/SRC/OpenGL/SYNCHROSTAMP" (capi::prompt-for-directory "Send Files) to: "))

;; midnight feb 9 2011
(update-source-files '("/Boxer/SRC/OpenGL/SYNCHROSTAMP" 0 0 0 9 2 2011) (capi::prompt-for-directory "Destination directory:"))

|#


;;;;
;;;; FILE: misc-prims.lisp
;;;;
#-opengl
(boxer-eval::defboxer-primitive bu::redisplay ()
                                ;(boxer-eval::reset-poll-count)
                                (process-editor-mutation-queue-within-eval)
                                (let ((*evaluation-in-progress?* nil))
                                  ;; This is checked by CLX clipping, needs to be NIL for redisplay
                                  (repaint-window *boxer-pane*))
                                (invalidate-absolute-position-caches)
                                boxer-eval::*novalue*)

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

;; for debugging
(eval-when (compile)
  (defvar *include-opengl-debugging?* nil)
)

(defmacro debug-opengl-print (format-string &rest args)
  (when *include-opengl-debugging?*
    `(format *error-output* ,format-string . ,args)))

(eval-when (compile)
           (defvar *include-font-debugging* nil)
           )

(defvar *debug-font-caching* nil)

(defmacro ogl-debug (&body forms)
  (when *include-font-debugging*
    `(when *debug-font-caching*
       . ,forms)))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun opengl::color->pixel (color)
  (dpb (float-color-to-byte-value (ogl-color-alpha color))
       opengl::*gl-rgba-rev-alpha-byte*
       (dpb (float-color-to-byte-value (ogl-color-blue color))
            opengl::*gl-rgba-rev-blue-byte*
            (dpb (float-color-to-byte-value (ogl-color-green color))
                 opengl::*gl-rgba-rev-green-byte*
                 (float-color-to-byte-value (ogl-color-red color))))))

;; sgithens 2023-07-10 Retiring the old ogl-color vectors that are no longer in use, but were still being
;; used as intermediaries
;;;; COLORS

;; we'll use opengl vectors as the primary color object, this allows us to
;; bind and pass them around as they need to be in the upper level boxer code

(defvar *ogl-color-counter* 0)
(defvar *ogl-color-freed* 0)

(defun make-ogl-color (r g b &optional (alpha 1.0))
  (incf *ogl-color-counter*)
  (box::with-stack-list (color (coerce r 'single-float)
                               (coerce g 'single-float)
                               (coerce b 'single-float)
                               (coerce alpha 'single-float))
                        (opengl::make-gl-vector :float 4 :contents color)))

(defun %make-ogl-color ()
  (incf *ogl-color-counter*)
  (opengl::make-gl-vector :float 4))

(defun free-ogl-color (color)
  (incf *ogl-color-freed*)
  (opengl::free-gl-vector color))

(defun ogl-convert-color (colorspec)
  "Takes an RGB spec vector and returns an opengl 4f vector with RGBA values, with alpha at 1.0
  Colorspec is an RGB vector like this (for orange): #(:RGB 1.0 0.6470585 0.0)"
  (make-ogl-color (svref colorspec 1)
                  (svref colorspec 2)
                  (svref colorspec 3)))

(defun ogl-color-red   (color) (opengl:gl-vector-aref color 0))
(defun ogl-color-green (color) (opengl:gl-vector-aref color 1))
(defun ogl-color-blue  (color) (opengl:gl-vector-aref color 2))
(defun ogl-color-alpha (color) (opengl:gl-vector-aref color 3))

(defun ogl-color->rgb (ogl-color)
  "Takes an openGL vector representing an RGBA color and returns a five part
  vector as: #(:rgb 1.0 1.0 1.0 1.0"
  (let ((togo (make-array '(5))))
    (setf (aref togo 0) :rgb
          (aref togo 1) (ogl-color-red ogl-color)
          (aref togo 2) (ogl-color-green ogl-color)
          (aref togo 3) (ogl-color-blue ogl-color)
          (aref togo 4) (ogl-color-alpha ogl-color))
    togo))

(defun ogl-color= (c1 c2)
  "Compares an ogl-color to one another to see if they are the same.
  For all practical purposes we're using the usual color range of 0 - 255 (a bit larger than that),
  so only that portion of a float will be considered, regardless of how
  many trailing decimal points each RGB float component as it has."
  (and (float-precision= (opengl:gl-vector-aref c1 0) (opengl:gl-vector-aref c2 0))
       (float-precision= (opengl:gl-vector-aref c1 1) (opengl:gl-vector-aref c2 1))
       (float-precision= (opengl:gl-vector-aref c1 2) (opengl:gl-vector-aref c2 2))
       ;; compare alpha values ?
       ))

(defun print-color (ogl-color)
  (format t "<OGL-Color R:~3F G:~3F B:~3F alpha:~3F>" (ogl-color-red ogl-color)
          (ogl-color-green ogl-color) (ogl-color-blue ogl-color)
          (ogl-color-alpha ogl-color)))

(defun ogl-report (&optional clear?)
  (format t "~&~D OGL colors allocated, ~D freed, ~D leaked"
          bw::*ogl-color-counter* bw::*ogl-color-freed*
          (- bw::*ogl-color-counter* bw::*ogl-color-freed*))
  (when clear? (setq bw::*ogl-color-counter* 0 bw::*ogl-color-freed* 0)))




(defun ogl-init (width height)
  ;; 2022-11-17 sgithens
  ; (opengl:gl-matrix-mode opengl:*gl-projection*)
  ; (opengl:gl-load-identity)
  ; ;; orthographic projection, 0,0 = top,left
  ; ;; Note:GL-Ortho wants double-floats as args (and insists on the mac)
  ; (opengl:gl-ortho (coerce 0.0 'double-float)            (coerce (float width) 'double-float)
  ;           (coerce (float height) 'double-float) (coerce 0.0 'double-float)
  ;           (coerce -1.0 'double-float)           (coerce 1.0 'double-float))
            )

(defvar *ogl-current-color-vector*) ; init'd in start-boxer (boxwin-opengl.lisp)


;; resource pool for gl-colors used in maintaining-ogl-color
(defvar *ogl-color-pool* nil)

(defvar *initial-ogl-color-pool-size* 20)

(defun initialize-ogl-color-pool ()
  (dotimes (i *initial-ogl-color-pool-size*) (push (%make-ogl-color) *ogl-color-pool*)))

(defun allocate-ogl-color () (or (pop *ogl-color-pool*) (%make-ogl-color)))

(defun ogl-current-color (&optional (vector *ogl-current-color-vector*))
  (get-opengl-state opengl:*gl-current-color* (:float 4) vector))

(defun deallocate-ogl-color (color) (push color *ogl-color-pool*))

;;;; for testing flags
(defun gl-enabled? (flag) (if (zerop (opengl::gl-is-enabled flag)) nil t))

;;; note that gl-begin can also be
;; modes can be: *gl-points*, *gl-lines*, *GL-LINE-LOOP*, *GL-LINE-STRIP*,
;; *GL-TRIANGLES*, *GL-TRIANGLE-STRIP*, *GL-TRIANGLE-FAN*, *GL-QUADS*,
;; *GL-QUAD-STRIP*, or *gl-polygon*

;;;;
(eval-when (compile)
  (defvar *opengl-type-checking-included?* t)
)

(defvar *opengl-type-checking-action* :coerce)

(defmacro ogl-type (arg type)
  (cond ((null *opengl-type-checking-included?*) `,arg)
    (t
     `(cond ((eq *opengl-type-checking-action* :error)
             (cond ((typep ,arg ,type) ,arg)
               (t (error "The arg, ~S, is not of type ~S" ',arg ,type))))
        ((eq *opengl-type-checking-action* :coerce)
         (coerce ,arg ,type))
        (t (error "*opengl-type-checking-action*,~S, should be :COERCE or :ERROR"
                  *opengl-type-checking-action*))))))

; the gl-viewport is conditionalized with:
;  (when #+Win32 (win32:is-window-visible
;                 (win32:pane-hwnd (capi-internals:representation canvas)))
;	#-Win32 T
; in example code

;;; State management (see Red Book appendix B for state vars)
;; Note, type can be either an atomic type of a list of type and length
;; valid types are: :SIGNED-8 :SIGNED-16 :SIGNED-32 :UNSIGNED-8 :UNSIGNED-16
;;                  :UNSIGNED-32 (:DOUBLE :DOUBLE-FLOAT) (:FLOAT :SINGLE-FLOAT)
(defmacro get-opengl-state (pname type &optional return-vector)
  (let ((gl-vect-var (gensym)))
    (flet ((canonicalize-type (raw-type)
                              (if (eq raw-type :boolean) :unsigned-8 raw-type)))
          `(let ((,gl-vect-var (cond ((and (not (null ,return-vector))
                                           (listp ',type)
                                           ; sgithens 2021-03-21 Removing this check now to get rid of
                                           ; fli dependency, all uses of get-opengl-state seem safe in
                                           ; the codebase. Will be refactored as we set up the libre
                                           ; opengl rendering.
                                           ;(fli::pointerp ,return-vector)
                                           )
                                      ,return-vector)
                                 (t
                                  (opengl::make-gl-vector ,(canonicalize-type
                                                           (if (listp type) (car type) type))
                                                          ,(if (listp type) (cadr type) 1))))))
             (unwind-protect
              (progn
               (,(ecase (if (listp type) (car type) type)
                        (:boolean 'opengl::gl-get-booleanv)
                        ((:signed-32 :integer) 'opengl::gl-get-integerv)
                        (:float 'opengl::gl-get-floatv))
                ,pname ,gl-vect-var)
               ,(cond ((and (listp type) (not (null return-vector)))
                       `,gl-vect-var)
                  ((listp type)
                   `(values ,@(let ((value-forms nil))
                                (dotimes (i (cadr type))
                                  (push `(opengl::gl-vector-aref ,gl-vect-var ,i)
                                        value-forms))
                                (nreverse value-forms))))
                  ((eq type :boolean)
                   `(let ((raw (opengl::gl-vector-aref ,gl-vect-var 0)))
                      (cond ((zerop raw) nil)
                        ((= raw 1) t)
                        (t (error "~D is not a valid boolean value" raw)))))
                  (t
                   `(opengl::gl-vector-aref ,gl-vect-var 0))))
              (when (null ,return-vector)
                (opengl::free-gl-vector ,gl-vect-var)))))))

(defstruct (ogl-graphics-state (:constructor %make-ogl-graphics-state))
  (color nil)
  (font  nil))

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

(defvar *scroll-buttons-color*)
(defvar *scroll-button-width* 10)
(defvar *scroll-button-length* 6)

(defun scroll-buttons-extent () (+ 1 *scroll-button-length* 1 *scroll-button-length* 1))

;; useful info for h-scroll tracking, returns the elevator's  min-x, max-x and
;; current left x-pos relative to the box
(defmethod h-scroll-info ((self screen-box))
  (with-slots (actual-obj wid box-type scroll-x-offset max-scroll-wid)
    self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths box-type self)
                         (declare (ignore it ib))
                         (let* ((type-label-width (border-label-width (box-type-label actual-obj)))
                                (s-start (+ type-label-width il) )
                                (s-width (- wid il ir type-label-width (scroll-buttons-extent))))
                           (values s-start
                                   (+ s-start s-width);(+ s-start (- s-width (round (* s-width (/ (- wid il ir) max-scroll-wid)))))
                                   (+ s-start (abs (round (* s-width (/ scroll-x-offset max-scroll-wid))))))))))

(defmethod v-scroll-info ((self screen-box))
  (with-slots (hei box-type)
    self
    (multiple-value-bind (il it ir ib)
                         (box-borders-widths box-type self)
                         (declare (ignore il ir))
                         (let ((s-width (- hei it ib (scroll-buttons-extent))))
                           (values it
                                   (+ it s-width))))))

;; size is expressed as a rational < 1 = amount of available space to draw the elevator in
;; pos is also expressed as a rational 0 <= pos <= 1
(defun draw-vertical-elevator (x y height size pos)
   (with-pen-size (1) (with-pen-color (*black*)
     (opengl::gl-disable opengl::*gl-line-smooth*)
    (draw-line x y x (+ y height))
    (opengl::gl-enable opengl::*gl-line-smooth*)
  ))
  (with-pen-color (*scroll-elevator-color*)
    (draw-rectangle *scroll-elevator-thickness* (round (* height size))
                    (+ x *scroll-info-offset*) (+ y (round (* pos height))))))

;; size is expressed as a rational < 1 = amount of available space to draw the elevator in
;; pos is also expressed as a rational 0 <= pos <= 1
(defun draw-horizontal-elevator (x y width size pos)
  (with-pen-color (*scroll-elevator-color*)
    (draw-rectangle (round (* width size)) *scroll-elevator-thickness*
                    (+ x (round (* pos width))) (+ y *scroll-info-offset*))))

;; Older versions of mouse-in-h-scroll-bar-internal and mouse-in-v-scroll-bar-internal for reference

;; loop:get the current elevator position, update, repaint
;; ? display info graphics ?
(defun mouse-in-h-scroll-bar-internal (screen-box x y)
  (let ((initial-scroll-pos (slot-value screen-box 'scroll-x-offset)))
    (multiple-value-bind (h-min-x h-max-x)
                         (h-scroll-info screen-box)
                         (multiple-value-bind (box-window-x box-window-y)
                                              (xy-position screen-box)
                                              ;; the "offset" is the difference between the initial mouse pos to the start if
                                              ;; the horizontal scrolling elevator which is where the new scrolling location is
                                              ;; calculated from
                                              (declare (ignore box-window-y))
                                                #+lispworks (ignore-errors
                                                  (let ((x-offset (+ box-window-x h-min-x))
                                                        (h-working-width (- h-max-x h-min-x)))
                                                    (with-mouse-tracking ((mouse-x x) (mouse-y y))
                                                      ; (declare (ignore mouse-y))
                                                      (setf (slot-value screen-box 'scroll-x-offset)
                                                            (- (round (* (min (/ (max 0 (- mouse-x x-offset)) h-working-width) 1)
                                                                        (- (slot-value screen-box 'max-scroll-wid)
                                                                            (/ (screen-obj-wid screen-box) 2))))))
                                                      (repaint t)))
                                                  (maybe-move-point-after-scrolling screen-box (if (< initial-scroll-pos
                                                                                                      (slot-value screen-box 'scroll-x-offset))
                                                                                                :left
                                                                                                :right)))))))

(defun mouse-in-v-scroll-bar-internal (screen-box x y click-only?)
  (let ((start-row (or (scroll-to-actual-row screen-box)
                       (first-inferior-row (screen-obj-actual-obj screen-box)))))
    (multiple-value-bind (v-min-y v-max-y)
                         (v-scroll-info screen-box)
                         (multiple-value-bind (box-window-x box-window-y)
                                              (xy-position screen-box)
                                              (declare (ignore box-window-x))
                                              (let ((y-offset (+ box-window-y v-min-y))
                                                    (v-working-height (- v-max-y v-min-y)))
                                                (if click-only?
                                                  (set-v-scroll-row screen-box (min (/ (max 0 (- y y-offset))
                                                                                       v-working-height)
                                                                                    1))
                                                  (let* ((eb (screen-obj-actual-obj screen-box))
                                                         (no-of-rows (length-in-rows eb)))
                                                    ;; bind these so we dont have to calculate them for each iteration
                                                    ;; of the tracking loop
                                                    #+lispworks (boxer-window::with-mouse-tracking ((mouse-x x) (mouse-y y))
                                                                                       (declare (ignore mouse-x))
                                                                                       (set-v-scroll-row screen-box
                                                                                                         (min (/ (max 0 (- mouse-y y-offset)) v-working-height) 1)
                                                                                                         eb
                                                                                                         no-of-rows)
                                                                                       (repaint t)))))))
    (maybe-move-point-after-scrolling screen-box
                                      (if (row-> start-row
                                            (or (scroll-to-actual-row screen-box)
                                                (first-inferior-row (screen-obj-actual-obj
                                                                     screen-box))))
                                        :left
                                        :right))))

(defun h-scroll-screen-box (screen-box &optional (velocity -1)) ; negative values scroll right
  (let ((new-scroll-x-offset (+ (slot-value screen-box 'scroll-x-offset) velocity)))
    (setf (slot-value screen-box 'scroll-x-offset)
          (cond ((plusp velocity)
                 (min new-scroll-x-offset 0))
            (t
             (max new-scroll-x-offset (- (if (null (slot-value screen-box 'max-scroll-wid))
                                           (floor (screen-obj-wid screen-box) 2)
                                           (slot-value screen-box 'max-scroll-wid)))))))))

(defvar *horizontal-click-scroll-quantum* 10
  "How much to scroll horizontally when a horizontal scroll button has been clicked")

(defvar *horizontal-continuous-scroll-quantum* 2)


;; should this be in coms-oglmouse ?
;; this needs to move the *point* if it is scrolled off the screen
(defun mouse-h-scroll (screen-box direction &optional (vboost 1))
  (let ((velocity (if (eq direction :right)
                    (* vboost (- *horizontal-continuous-scroll-quantum*))
                    (* vboost *horizontal-continuous-scroll-quantum*))))
    ;; do SOMETHING, for cases that should be interpreted as a slow click
    (h-scroll-screen-box screen-box velocity)
    (simple-wait-with-timeout *initial-scroll-pause-time*
                              #'(lambda () (zerop& (mouse-button-state))))
    (loop (when (or (zerop& (mouse-button-state))
                    (and (eq direction :right)
                         (or (null (slot-value screen-box 'max-scroll-wid))
                             (>= (+ (abs (slot-value screen-box 'scroll-x-offset))
                                    (round (screen-obj-wid screen-box) 2))
                                 (slot-value screen-box 'max-scroll-wid))))
                    (and (eq direction :left)
                         (zerop (slot-value screen-box 'scroll-x-offset))))
            (return))
      (h-scroll-screen-box screen-box velocity)
      (repaint t)
      (simple-wait-with-timeout *scroll-pause-time*
                                #'(lambda () (zerop& (mouse-button-state)))))
    ;; maybe adjust *point* here, otherwise (repaint) can change the scroll state
    ;; no need to adjust in the loop because we aren't calling the scroll changing version of repaint
    (maybe-move-point-after-scrolling screen-box direction)))

;; 2023-12-08 sgithens Old versions of v-scrollable? and h-scrollable?
(defmethod v-scrollable? ((self screen-box))
  (with-slots (actual-obj scroll-to-actual-row screen-rows)
    self
    (unless (symbolp screen-rows) ;;  screen-rows can be a symbol for port ellipsis
      (or (< (screen-rows-length self) (length-in-rows actual-obj))
          (and (not (null scroll-to-actual-row))
               (not (eq scroll-to-actual-row (first-inferior-row actual-obj))))))))

(defmethod h-scrollable? ((self screen-box))
  (with-slots (scroll-x-offset max-scroll-wid)
    self
    (or (not (zerop scroll-x-offset)) (not (null max-scroll-wid)))))

#|
(defmethod pixel-scroll-screen-box ((screen-box screen-box) pixels)
  (with-slots (wid hei scroll-to-actual-row scroll-y-offset) screen-box
    (let* ((top-row (or scroll-to-actual-row
                        (first-inferior-row (screen-obj-actual-obj screen-box))))
           (top-screen-row (allocate-screen-obj-for-use-in top-row screen-box))
           (top-row-height (screen-obj-hei top-screen-row))
           (move (+& scroll-y-offset pixels))
           (actual-velocity pixels))
      (multiple-value-bind (lef top rig bot)
                           (box-borders-widths (box-type screen-box) screen-box)
                           (cond ((plusp& move)
                                  ;; scrolled down beyond the extent of the current top row
                                  ;; if there is a row above this one, go to that
                                  (let ((prev-row (previous-row top-row)))
                                    (cond ((null prev-row)
                                           ;; no previous row so set to scrolled to top of box values
                                           (setq scroll-y-offset 0 scroll-to-actual-row nil)
                                           (setq actual-velocity (-& pixels move)))
                                      (t
                                       ;; setup screen structure for the prev-row
                                       (let ((psr (allocate-screen-obj-for-use-in
                                                   prev-row screen-box)))
                                         (insert-screen-row screen-box psr top-screen-row)
                                         ;; now we have to simulate a redisplay-pass-1 for the row
                                         (setf (screen-obj-x-offset psr) lef
                                               (screen-obj-y-offset psr) top)
                                         ;; note that it also sets origin and clipping like the
                                         ;; usual redisplay-pass-1-sr because inferior boxes
                                         ;; may do some erasing and the erasing should happen
                                         ;; in the right place
                                         (with-origin-at ((screen-obj-x-offset psr)
                                                          (screen-obj-y-offset psr))
                                           (update-screen-row-for-scrolling
                                            psr (-& wid lef rig) (-& hei top bot)))
                                         (setq scroll-to-actual-row prev-row
                                               scroll-y-offset
                                               (-& move (screen-obj-hei psr)))
                                         (new-1st-screen-row-for-scrolling
                                          screen-box psr
                                          (-& hei top bot scroll-y-offset)))))))
                             ((null (next-row top-row))
                              ;; nowhere to go
                              ;; should fix velocity here too....
                              (setq scroll-y-offset 0))
                             ((>=& (-& move) top-row-height)
                              ;; scrolled up beyond the extent of the current top row
                              ;; we know there must be a next row because we already checked
                              (setq scroll-to-actual-row (next-row top-row)
                                    scroll-y-offset (+& move top-row-height))
                              (remove-1st-screen-row-for-scrolling screen-box top-screen-row)
                              (check-new-screen-row-for-scrolling screen-box lef top rig bot))
                             (t
                              (setq scroll-y-offset move)
                              (unless (plusp& pixels)
                                ;; no need to check the bottom if we are scrolling upwards
                                (check-new-screen-row-for-scrolling screen-box
                                                                    lef top rig bot)))))
      ;(set-force-redisplay-infs? screen-box)
      (setf (slot-value screen-box 'needs-redisplay-pass-2?) t)
      actual-velocity)))

;; box borders widths are passed in cause we've already done the work
(defmethod check-new-screen-row-for-scrolling ((screen-box screen-box)
                                               lef top rig bot)
  (with-slots (wid hei scroll-x-offset scroll-y-offset) screen-box
    (let*  ((row-size (with-summation
                        (do-vector-contents (sr (slot-value screen-box
                                                            'screen-rows))
                          (sum (screen-obj-hei sr)))))
            (box-occupied (+& row-size scroll-y-offset))
            (last-screen-row (last-screen-row screen-box)))
      (cond ((screen-obj-y-got-clipped? last-screen-row)
             ;; no room for more rows but we should update the last
             ;; row in case the clipping state would get changed
             (with-origin-at ((+& scroll-x-offset (screen-obj-x-offset
                                                   last-screen-row))
                              (+& scroll-y-offset (screen-obj-y-offset
                                                   last-screen-row)))
               (update-screen-row-for-scrolling last-screen-row (-& wid lef rig)
                                                (+& (-& hei top bot box-occupied)
                                                    (screen-obj-hei
                                                     last-screen-row)))))
        ((>& (-& hei top bot) box-occupied)
         ;; there is room for more inferiors at the bottom of the box...
         ;; 1st see if we want more rows...
         (let ((next-unseen-row (next-row (screen-obj-actual-obj
                                           last-screen-row))))
           (unless (null next-unseen-row)
             (let ((nsr (allocate-screen-obj-for-use-in next-unseen-row
                                                        screen-box)))
               (append-screen-row screen-box nsr)
               (setf (screen-obj-x-offset nsr) lef
                     (screen-obj-y-offset nsr) (+& top row-size))
               (with-origin-at ((+& scroll-x-offset
                                    (screen-obj-x-offset nsr))
                                (+& scroll-y-offset
                                    (screen-obj-y-offset nsr)))
                 (with-clipping-inside (0 0 (-& wid lef rig)
                                          (-& hei top bot box-occupied))
                   (update-screen-row-for-scrolling
                    nsr (-& wid lef rig) (-& hei top bot
                                             box-occupied))))))))))))

;; this updates the rest of the screen-rows of a box when a new one
;; has been inserted in the front
(defmethod new-1st-screen-row-for-scrolling ((screen-box screen-box)
                                             1st-row max-hei)
  (let* ((y-inc (screen-obj-hei 1st-row))
         (acc-hei y-inc))
    (do-vector-contents (screen-row (slot-value screen-box 'screen-rows)
                                    :start 1 :index-var-name pos)
      (cond ((>& acc-hei max-hei)
             (erase-and-queue-for-deallocation-screen-rows-from
              (slot-value screen-box 'screen-rows) pos nil t)
             (kill-screen-rows-from screen-box pos)
             ;; should deallocate killed rows...
             (return nil))
        (t
         (setf (screen-obj-y-offset screen-row)
               (+& (screen-obj-y-offset screen-row) y-inc))
         (incf& acc-hei (screen-obj-hei screen-row)))))))

(defmethod remove-1st-screen-row-for-scrolling ((screen-box screen-box) 1st-row)
  (let ((1st-hei (screen-obj-hei 1st-row)))
    ;; remove the row
    (delete-screen-row screen-box 1st-row)
    ;; process it for deallocation
    (screen-obj-zero-size 1st-row)
    (set-needs-redisplay-pass-2? 1st-row t)
    (set-force-redisplay-infs?  1st-row t)
    (queue-screen-obj-for-deallocation 1st-row)
    ;; adjust the offsets for the rest
    (do-vector-contents (sr (slot-value screen-box 'screen-rows))
      (setf (screen-obj-y-offset sr) (-& (screen-obj-y-offset sr) 1st-hei)))))

|#

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

;; sgithens 2023-04-20 This old bottom-right pop-up hasn't been used in years.
;; (boxer-eval::defboxer-key bu::mouse-right-click-on-bottom-right com-mouse-br-pop-up)

;; THings are setup so that this menu appears ONLY if the hotspot is enabled
;; indicating that auto box sizing is active.  If manual box sizing is active
;; we go straight to com-mouse-resize-box
(defvar *br-popup* (make-instance 'popup-menu
                     :items (list (make-instance 'menu-item
                                    :title "Automatic Box Size"
                                    :action 'com-hotspot-unfix-box-size)
                                  (make-instance 'menu-item
                                    :title "Manual Box Size"
                                    :action 'com-mouse-toggle-br-hotspot))))

(defun update-br-menu (box)
  (declare (ignore box))
  (let ((auto-item    (car  (menu-items *br-popup*)))
        (manual-item  (cadr (menu-items *br-popup*))))
    ;; We only see the menu if the spot is "off", an "on" spot means resize
    ;; make sure "auto" is greyed out
    (menu-item-disable  auto-item)
    (menu-item-enable manual-item)))

(defboxer-command com-mouse-br-pop-up (&optional (window *boxer-pane*)
                                       (x (bw::boxer-pane-mouse-x))
                                       (y (bw::boxer-pane-mouse-y))
                                       (mouse-bp
                                       (mouse-position-values x y))
                                       (click-only? t))
  "Pop up a box attribute menu"
  window x y ;  (declare (ignore window x y))
  ;; first, if there already is an existing region, flush it
  (reset-region) (reset-editor-numeric-arg)
  (let* ((screen-box (bp-screen-box mouse-bp))
         (box-type (box-type screen-box))
         (swid (screen-obj-wid screen-box))
         (shei (screen-obj-hei screen-box))
         (edbox (screen-obj-actual-obj screen-box)))
    (cond ((bottom-right-hotspot-active? edbox)
           (if (or click-only? (not (mouse-still-down-after-pause? 0)))
               ;; hotspot is on, but we only got a click, shortcut to restore menu
               (progn
                 (set-fixed-size edbox nil nil)
                 (set-scroll-to-actual-row screen-box nil)
                 (modified edbox)
                 ;; turn the hotspot off so the menu will pop up next time
                 (set-bottom-right-hotspot-active? edbox nil))
               ;; otherwise, do the normal resize stuff
               (com-mouse-resize-box window x y mouse-bp click-only?)))
          (t ;; otherwise, update and present a menu
           (multiple-value-bind (left top right bottom)
               (box-borders-widths box-type screen-box)
             (declare (ignore left top))
             ;; will probably have to fudge this for type tags near the edges of
             ;; the screen-especially the bottom and right edges
             (multiple-value-bind (abs-x abs-y) (xy-position screen-box)
               (update-br-menu edbox)
               ;; the coms in the pop up rely on this variable
               (let ((*hotspot-mouse-box* edbox)
                     (*hotspot-mouse-screen-box* screen-box))
                 (menu-select *br-popup*
                              (- (+ abs-x swid) right)
                              (- (+ abs-y shei) bottom))))))))
  boxer-eval::*novalue*)


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

(defun allocate-region-row-blinker (screen-row)
  (let ((new-blinker (make-region-row-blinker)))
    (setf (region-row-blinker-uid new-blinker) screen-row)
    new-blinker))

;;; Accessor Macros...
(defsubst region-row-blinker-wid (region)
  (bw::blinker-width region))

(defsubst region-row-blinker-hei (region)
  (bw::blinker-height region))

(defsubst region-row-blinker-x (region)
  (bw::blinker-x region))

(defsubst region-row-blinker-y (region)
  (bw::blinker-y region))

(defsubst region-row-blinker-uid (region)
  (bw::region-row-blinker-uid region))

;;; setf's

(defsetf region-row-blinker-wid (region) (new-wid)
  `(setf (bw::blinker-width ,region) ,new-wid))

(defsetf region-row-blinker-hei (region) (new-hei)
  `(setf (bw::blinker-height ,region) ,new-hei))

(defsetf region-row-blinker-x (region) (new-x)
  `(setf (bw::blinker-x ,region) ,new-x))

(defsetf region-row-blinker-y (region) (new-y)
  `(setf (bw::blinker-y ,region) ,new-y))

(defsetf region-row-blinker-uid (region) (new-uid)
  `(setf (bw::region-row-blinker-uid ,region) ,new-uid))

;;;; mousy stuff
;;;; this function tells if the mouse is on top of the current region.
(defun mouse-on-region-being-defined-p ()
  (if (null boxer::*region-being-defined*)
      nil
      (let ((blinker-list (boxer::interval-blinker-list boxer::*region-being-defined*)))
        (multiple-value-bind (m-x m-y) (bw::mouse-window-coords)
          (dolist (b-row blinker-list)
            (if (coords-on-blinker-row m-x m-y b-row)
                (return t)))))))

(defun coords-on-blinker-row (m-x m-y b-row)
  (if (null b-row) nil)
  (let* ((x-low (region-row-blinker-x b-row))
         (x-high (+ (region-row-blinker-x b-row)
                    (bw::region-row-blinker-width b-row)))

         (y-low (region-row-blinker-y b-row))
         (y-high (+ (region-row-blinker-y b-row)
                    (bw::region-row-blinker-height b-row))))
    (and (and (< m-x x-high) (> m-x x-low))
         (and (< m-y y-high) (> m-y y-low)))))




(defun turn-on-interval (region)
  (unless (interval-visibility region)
    (make-interval-visible region))
  (setf (interval-visibility region) t)
  )

(defun turn-off-interval (region)
  (when (interval-visibility region)
    (make-interval-invisible region))
  (setf (interval-visibility region) nil)
  )

(defsubst region-row-blinker-visibility (region)
  (bw::blinker-visibility region))

(defsetf region-row-blinker-visibility (region) (new-vis)
  `(setf (bw::blinker-visibility ,region) ,new-vis))

;; these need to have with-drawing-port's wrapped around them because
;; they get called OUTSIDE of the redisplay
(defun make-interval-visible (region)
  (dolist (row-blinker (interval-blinker-list region))
    (setf (region-row-blinker-visibility row-blinker) t)))

(defun make-interval-invisible (region)
  (dolist (row-blinker (interval-blinker-list region))
    (setf (region-row-blinker-visibility row-blinker) nil)))


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

(defun repaint-with-cursor-relocation ()
  (let ((*allow-redisplay-encore? t))
    (repaint)))

;; sgithens 2023-23-08 from repaint-cursor-internal, when we were checking to see if the horiz scroll-bar
;; needed to snap back because the cursor was offscreen.
        ;; check the BP coords here, we may have to adjust the scroll-x-offset of the psb
    ;; check to see if cursor-x falls inside of PSB, if not, change the scroll-x-offset of psb
    (unless (or (null psb) (null *allow-redisplay-encore?))
      (multiple-value-bind (lef top rig bot)
                           (box-borders-widths (box-type psb) psb) (declare (ignore top bot))
                           (let* ((psb-x (- (screen-obj-absolute-coordinates psb) (slot-value psb 'scroll-x-offset)))
                                  (psb-min-x (+ psb-x lef))
                                  (psb-max-x (+ psb-x (- (screen-obj-wid psb) rig)))
                                  (move-distance (floor (slot-value psb 'wid) 2)))
                             (cond ((<= psb-min-x cursor-x psb-max-x)) ; everything is ok, cursor is already visible
                               ((< cursor-x psb-min-x) ; cursor is to left of the visible part of the box
                                                       (setf (slot-value psb 'scroll-x-offset)
                                                             (min 0 (+ (slot-value psb 'scroll-x-offset) (- psb-min-x cursor-x) move-distance)))
                                                       (setq *redisplay-encore?* t)
                                                       (throw 'scroll-x-changed t))
                               ((> cursor-x psb-max-x) ; cursor is to the right of the visible part of the box
                                                       (setf (slot-value psb 'scroll-x-offset)
                                                             (min 0 (+ (slot-value psb 'scroll-x-offset) (- psb-max-x cursor-x move-distance))))
                                                       (setq *redisplay-encore?* t)
                                                       (throw 'scroll-x-changed nil))))))
    ;; continue...

;; this has been commented out from repaint-cursor-internal for a while.
                                              ;            (cond ((and (or (eq (bp-row cursor) scroll-row)
                                              ;                            (and (null scroll-row)
                                              ;                                 (box? actual-box)
                                              ;                                 (eq (bp-row cursor)
                                              ;                                     (first-inferior-row actual-box))))
                                              ;                        (not (zerop scroll-y-offset)))
                                              ;                   (set-cursorpos *boxer-pane* cursor-x (- cursor-y scroll-y-offset))
                                              ;                   (SET-CURSOR-SIZE *POINT-BLINKER* 3 (+ (get-cursor-height cha)
                                              ;                                                         scroll-y-offset)))
                                              ;                  (t
                                              ;                   (set-cursorpos *boxer-pane* cursor-x cursor-y)
                                              ;                   (SET-CURSOR-SIZE *POINT-BLINKER* 3
                                              ;                                    (get-cursor-height cha))))


;; 2023-02-23 Removing needs-repaint-pass methods since we are always returning t
;; 1st cut, recalculate everything
;; 2nd cut, get smarter (need to hack deep active sprite problem)
;; if no deep active sprite, then use timestamps or force-repaint flag
(defmethod needs-repaint-pass-1? ((self screen-obj)
                                  &optional (max-wid nil) (max-hei nil)) t)
  ;; sgithens 2021-11-19 Experiementing with just always repainting pass 1
  ;; (cond ((or (<= (slot-value self 'tick)
  ;;                (actual-obj-tick (screen-obj-actual-obj self)))
  ;;            ;; actual obj has changed
  ;;            (not (null *complete-redisplay-in-progress?*))
  ;;            ;; got less room...
  ;;            (and (not-null max-wid) (< max-wid (slot-value self 'wid)))
  ;;            (and (not-null max-hei) (< max-hei (slot-value self 'hei)))
  ;;            ;; was clipped but got more room...
  ;;            (and (not-null (slot-value self 'x-got-clipped?))
  ;;                 (not-null max-wid) (> max-wid (slot-value self 'wid)))
  ;;            (and (not-null (slot-value self 'y-got-clipped?))
  ;;                 (not-null max-hei) (> max-hei (slot-value self 'hei)))
  ;;            ;; deep active sprite
  ;;            ;             (active-sprite-inside? self)
  ;;            )
  ;;        t)
  ;;   (t nil)))

;(defmethod needs-repaint-pass-1? ((self screen-obj)
;                                  &optional (max-wid nil) (max-hei nil))
;  (declare (ignore max-wid max-hei))
;  t)

;; if we intend to rebuild the entire screen with every key stroke,
;; this will always be T, we may want to be more selective in what we erase
;; and redraw for efficiency
;(defmethod needs-repaint-pass-2? ((self screen-obj))
;  (or (not-null (slot-value self 'needs-redisplay-pass-2?))
;      (not-null *complete-redisplay-in-progress?*)))

(defmethod needs-repaint-pass-2? ((self screen-obj)) T)



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
;;;; FILE: surf.lisp
;;;;

;; this is supposed to read a CRLF terminated line from a "clear text" connection
(defun net-read-line (stream &optional (wait? t))
  (when (or wait? (listen stream))
    ;; this is an open coded version of ccl::telnet-read-line counting added
    #+mcl
    (unless (ccl:stream-eofp stream)
      (let ((line (Make-Array 10 :Element-Type #+mcl 'base-Character
                                               #+lispworks 'base-char
                                               #-(or mcl lispworks) 'character
                                 :Adjustable T :Fill-Pointer 0))
            (count 0)
            (char nil))
        (do () ((or (null (setq char (ccl:stream-tyi stream)))
                    (and (eq char #\CR) (eq (ccl:stream-peek stream) #\LF)))
                (when char (ccl:stream-tyi stream))
                (values line (null char) count))
          (vector-push-extend char line)
          (incf& count))))
    #-mcl
    (let ((eof-value (list 'eof)))
      (unless (eq (peek-char nil stream nil eof-value) eof-value)
        (let ((line (make-array 10 :element-type 'character
                                :adjustable t :fill-pointer 0))
              (count 0)
              (char nil))
          (do () ((or (null (setq char (read-char stream nil nil)))
                      (and (eq char #\cr) (eq (peek-char nil stream nil nil) #\lf)))
                  (when char (read-char stream))
                  (values line (null char) count))
            (vector-push-extend char line)
            (incf& count)))))
    ))

;; use the suffix to try an infer some information about the content of the file
(defun path-suffix (path)
  (unless (null path)
    (let ((last-dot (position #\. path :from-end t))
          (semi (position #\; path :from-end t)))
      (when last-dot
        (subseq path (1+& last-dot) semi)))))

(defun read-hex-pair (char1 char2)
  (flet ((char->number (char)
           (case char
             (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7)
             (#\8 8) (#\9 9) ((#\a #\A) 10.) ((#\b #\B) 11.) ((#\c #\C 12.))
             ((#\d #\D) 13.) ((#\e #\E) 14.) ((#\f #\F) 15.)
             (otherwise (error "% in url's should be encoded as %25")))))
    (+& (*& 16. (char->number char1)) (char->number char2))))

;; decoding of unsafe characters is handled here
;; look for "%" character encodings and convert them to characters
(defun decode-url-string (string)
  (let* ((slength (length string))
         (decoded-string (make-array slength
                                     :element-type #+mcl 'base-character
                                                   #+lispworks 'base-char
                                                   #-(or mcl lispworks) 'character
                                     :fill-pointer 0)))
    (do ((i 0 (1+& i)))
        ((>=& i slength))
      (let ((char (char string i)))
        (cond ((char= char #\%)
               ;; encoded character....
               (vector-push (code-char (read-hex-pair (char string (+& i 1))
                                                      (char string (+& i 2))))
                            decoded-string)
               (incf& i 2))
              (t
               (vector-push char decoded-string)))))
    decoded-string))


;;; TCP implementation for lispworks....

;;now loaded in dumper.lisp
;#+lispworks
;(eval-when (load) (require "comm"))
;#+carbon-compat
;(eval-when (load) (require "OpenTransport"))


(defvar *correct-password-retries* 3
  "Number of times to retry getting a correct password in
   situations where a login is required.")

;;; This is the interface to the underlying TCP support for a particular
;;; implementation:
;;; OPEN-TCP-STREAM, NET-READ-LINE and NET-WRITE-LINE
;;; NIL host means to open a port for listening...

(defun open-tcp-stream (host port
                             &key (element-type #+mcl 'base-character
                                                #+lispworks 'base-char)
                             (timeout 30))
  #+mcl
  (let ((unresolved-host host))
    (unless (null host)
      (unless (integerp host)
        (surf-message "Looking up Host ~A" host)
        (setq host (ccl::tcp-host-address host))
        (surf-message "Looking up Host ~A ==> ~A"
                      unresolved-host (ccl::tcp-addr-to-str host))))
    (if (null host)
        (surf-message "Opening Data Connection")
        (surf-message "Opening Connection to ~A..." unresolved-host))
    (prog1
      (ccl::open-tcp-stream host port :element-type element-type
                            #+carbon-compat :connect-timeout
                            #-carbon-compat :commandtimeout timeout)
      (unless (null host)
        (surf-message "Connected to ~A" unresolved-host))))
  #+lispworks
  (cond ((null host)
         (error "No lispworks implentation for server streams"))
;         (surf-message "Opening Data Connection")
;         (cond ((member element-type '(base-char character))
;                (start-char-server-stream port))
;               ((or (eq element-type 'unsigned-byte)
;                    (equal element-type '(unsigned-byte)))
;                (start-binary-server-stream port))
;               (t
;               (error "Unhandled network stream element-type: ~S" element-type))))
        (t
         (surf-message "Opening Connection to ~A" host)
         (comm::open-tcp-stream host port :element-type element-type
                                :timeout timeout :direction :io)))
  #-(or lispworks mcl) (error "TCP services undefined for ~A" (machine-type))
  )

;; needs better error checking
#+lispworks
(defun %get-ip-address (stream)
  (comm::get-socket-address (slot-value stream 'comm::socket)))

;; yuck
;; We need to replace this mechanism that uses global *return-tcp-stream*
;; after we find out how to do symbol-value-in-process we kind bind it
;; around the calls to start-XXX-server-stream

#+lispworks
(progn
  (defvar *return-tcp-stream* nil)

  (defun start-char-server-stream (port)
    (comm::start-up-server :function 'make-server-char-stream :service port)
    (mp::process-wait-with-timeout "Net Wait" 10
                                   #'(lambda () (not (null *return-tcp-stream*))))
    (prog1 *return-tcp-stream*
      (setq *return-tcp-stream* nil)))

  (defun start-binary-server-stream (port)
    (comm::start-up-server :function 'make-server-binary-stream :service port)
    *return-tcp-stream*)

  (defun make-server-char-stream (handle)
    (let ((stream (make-instance 'comm:socket-stream
                                 :socket handle :direction :io
                                 :element-type 'base-char)))
      (setq *return-tcp-stream* stream)
      (mp:process-run-function "Net Char Data"
                               '()
                               'poll-for-close-stream stream)))

  (defun make-server-binary-stream (handle)
    (let ((stream (make-instance 'comm:socket-stream
                                 :socket handle :direction :io
                                 :element-type 'unsigned-byte)))
      (setq *return-tcp-stream* stream)
      (mp:process-run-function "Net Binary Data"
                               '()
                               'poll-for-close-stream stream)))

  (defun poll-for-close-stream (stream)
    (loop
     (cond ((not (open-stream-p stream)) (return nil))
           (t
            (mp:process-wait-with-timeout "net wait" 10
                                          #'(lambda () (listen stream)))))))


)

#|
#+lispworks
(defun make-ftp-data-stream (handle &optional (direction :io)
                                    (element-type 'common-lisp:base-char))
  (let ((stream (make-instance 'comm:socket-stream
                               :socket handle
                               :direction direction
                               :element-type element-type)))
    (mp:process-run-function "ftp-data"
                           '()
                           'check-for-net-eof stream)))

(defun check-for-net-eof (stream)
  (let ((eof-value (list 'eof)))
    (unwind-protect
        (loop for char = (peek-char nil stream nil eof-value)


(defun blah (port)
 (comm:start-up-server :function 'make-ftp-data-stream
                      :service port))

|#

;; same as below except for debugging
;; once-only the args...
(defmacro net-write-control-line (stream string &rest args)
  (let ((string-arg (gensym))
        (rest-args nil))
    (dolist (arg args (setq rest-args (reverse rest-args)))
      (push (list (gensym) arg) rest-args))
    `(let ((,string-arg ,string)
           ,@rest-args)
       (debugging-message ,string-arg . ,(mapcar #'car rest-args))
       #+mcl (ccl::telnet-write-line ,stream
                                     ,string-arg . ,(mapcar #'car rest-args))
       #-mcl (net-write-line ,stream ,string-arg . ,(mapcar #'car rest-args))
       )))

(defmacro net-write-line (stream string &rest args)
  #+mcl `(ccl::telnet-write-line ,stream ,string . ,args)
  #-mcl `(progn (format ,stream ,string . ,args)
           (write-char #\return ,stream)
           (write-char #\linefeed ,stream)
           (force-output ,stream))
  )

(defun net-write-line-to-binary-stream (stream string)
  (dotimes& (i (length string))
    (write-byte (char-code (aref string i)) stream))
  (write-byte #.(char-code #\return) stream)
  (write-byte #.(char-code #\linefeed) stream)
  (force-output stream))

;; 4/12/00 - this doesn't seem to be called by anyone?
;; removed 10/13/03, leave source until next system release just in case
;(defun tcp-stream-local-port (s)
;  #+mcl
;  (ccl::rref (ccl::conn-pb (ccl::tcp-stream-conn s)) tcpioPB.status.localPort)
;  #-mcl
;  (error "TCP stream local port undefined for ~A" (machine-type))
;  )

(defvar *tcp-error-handlers* nil)
;; like CL HANDLER-BIND except that we match on FTP error codes
;; handler-list is a list or lists of error code & handlers
;; ftp errors check the handler list for a match to a handler otherwise,
;; they signal boxer errors
;; the handlers are searched in the order given so more specific handlers should
;; appear before more general ones (see signal-tcp-error)


(defmacro tcp-handler-bind (handler-list &body body)
  `(let ((*tcp-error-handlers* ',handler-list))
     . ,body))

(defun signal-tcp-error (response-code error-string)
  (let ((matching-handler (dolist (handler *tcp-error-handlers*)
                            (let ((match? (search (car handler) response-code)))
                              (when (and match? (zerop& match?))
                                (return (cadr handler)))))))
    (if (null matching-handler)
        (boxer-eval::primitive-signal-error :net-error (copy-seq error-string))
        (funcall matching-handler))))

;;; should make this more table driven so we can specialize better
;;; for different TCP operations like SMTP vs FTP
(defun handle-tcp-response (control-stream &optional (wait? t))
  (let* ((response (net-read-line control-stream wait?))
         (code (and response (subseq response 0 3))))
    (unless (null response)
      (debugging-message response)
      (cond ((char= #\- (char response 3)) ; multi-line?
             ;; just pull out any extra lines in a multi line reply...
             (do ((next-line (net-read-line control-stream wait?)
                             (net-read-line control-stream wait?)))
                 ((and (search code next-line) (char= #\space (char next-line 3))))
               (debugging-message next-line)))
            ((member (char response 0) '(#\4 #\5) :test #'char=) ; error?
             (signal-tcp-error code response))
            ((char= #\1 (char response 0))
             ;; more messages to come
             (handle-tcp-response control-stream nil))
            ((listen control-stream)
             ;(format t "recursive HANDLE-TCP-RESPONSE:")
             (handle-tcp-response control-stream))
            ))))

(defun throw-to-retry-fill-tag () (throw 'retry-fill nil))

;; this should be adaptive (larger values for PPP connections)
(defvar *ftp-data-listen-timeout* 300000)

(defclass mailto-url
  (url)
  ((address :initform *default-mail-address* :accessor mailto-url-address))
  ;; (:metaclass block-compile-class)
  )

;; this is for URL files possibly relative to some superior URL
(defclass file-url
  (url)
  ((pathname :initform nil :accessor file-url-pathname))
  ;; (:metaclass block-compile-class)
  )

;;; Mailto

(defmethod initialize-instance ((url mailto-url) &rest initargs)
  (call-next-method)
  (setf (slot-value url 'address)
        ;; should do some reality checking here
        (slot-value url 'scheme-string)))

(defvar *mail-instruction-box*
        (make-box '(("Edit your message in this box")
                    ("Exit the box to send the mail"))
                  'boxer::Data-box
                  "Instructions"))

(defvar *include-instructions-in-new-message-boxes?* T)

(defun make-message-box (from to &optional (subject ""))
  (let ((header (make-box `(("From:" ,from)
                            ("To:" ,to)
                            ("Subject:" ,subject))
                          'boxer::data-box
                          "Header"))
        (body (make-box '(()) 'boxer::data-box "Message")))
    (if *include-instructions-in-new-message-boxes?*
        (make-box (list (make-row (list *mail-instruction-box*))
                        (make-row (list header))
                        (make-row (list body))))
        (make-box (list (make-row (list header))
                        (make-row (list body)))))))

;; still need to add a trigger for actually mailing the message
;; not to mention the actual mechanism for sending the message
(defmethod fill-box-using-url ((url mailto-url) box)
  (append-row box (make-row (make-message-box *user-mail-address*
                                              (mailto-url-address url)))))


;; these network packages are loaded here to define the network stream classes...
;; most of the normal usage for network stuff is in surf.lisp

;; they define packages and functions which will be used in dumper and base64 among others
#+lispworks
(eval-when (eval load) (require "comm"))
#+carbon-compat
(eval-when (eval load) (require "OpenTransport"))

;;;;
;;;; FILE: sysprims.lisp
;;;;

;; sgithens TODO 2022-03-30 This definately isn't needed anymore, but before I archive this preference I'd like
;;                          to look at how to cleanly remove the variable the preference is bound to.
;; (defboxer-preference bu::penerase-color-from-bit-array (true-or-false)
;;   ((*check-bit-array-color* :boolean (boxer-eval::boxer-boolean *check-bit-array-color*))
;;    graphics
;;    ("Should the backing store of a frozen box be")
;;    ("checked for the penerase color if one exists ?"))
;;   (setq *check-bit-array-color* true-or-false)
;;   boxer-eval::*novalue*)

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

;;;;
;;;; FILE: xfile.lisp
;;;;

;; come here when single clicked on a file link
;; check to see if the file is a box
#+mcl
(defun edit-mac-file-ref (box &optional (xref (getprop box :xref)))
  (when (null xref)
    (let ((new-xref (make-mac-file-ref)))
      (putprop box new-xref :xref)  (setq xref new-xref)))
  (case (mac-file-ref-dialog (mac-file-ref-pathname xref))
    (:open (cond ((null (mac-file-ref-pathname xref))
                  (boxer-eval::primitive-signal-error :mac-interface
                                                "No file to launch"))
                 ((not (probe-file (mac-file-ref-pathname xref)))
                  (boxer-eval::primitive-signal-error :mac-interface
                                                "File not Found"
                                                (mac-file-ref-pathname xref)))
                 (t
                  (ccl::open-mac-file (mac-file-ref-pathname xref)))))
    (:change
     (let ((newpath (ccl::choose-file-dialog)))
       ;; newpath can point to a boxer file which should be handled
       ;; specially i.e. change to a file box
       (cond ((box-file? newpath)
              (remove-xfile-props box)
              (mark-box-as-file box newpath))
             (t
              (setf (mac-file-ref-pathname xref) newpath)
              (set-xref-boxtop-info box)))
       (modified box)))
    (:move
     (let ((newpath (ccl::choose-new-file-dialog
                     :directory (mac-file-ref-pathname xref))))
       (rename-file (mac-file-ref-pathname xref) newpath)
       (setf (mac-file-ref-pathname xref) newpath)))
    (t ;; can come here if CANCELed from the dialog
       )))
