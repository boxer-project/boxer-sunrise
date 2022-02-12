;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-
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



   This file contains boxer net surfing utilities


 Current implementations for FTP and Gopher
 Hooks for other common URL types

ToDo:
 Hack platform specific read-array and write-array for speed
 instead of doing it byte by byte



Modification History (most recent at top)

 8/05/05 had to add back methods for #+carbon-compat {reading,writing}-stream-position
         for opentransport-binary-tcp-stream because tcp-stream comes before
         opentransport-tcp-stream in the class precedence list so the wrong method
         was being used.
 3/02/05 #+carbon-compat {reading,writing}-stream-position methods fixed & changed from
         opentransport-binary-tcp-stream to opentransport-tcp-stream
 7/19/04 fill-box-from-server: clauses reordered, added new general box filling hook
10/13/03 open-tcp-stream keyword args different for opentransport tcp-streams
         reading/writing-stream-position for ccl::opentransport-binary-tcp-stream
10/01/03 fixed and updated network package REQUIRE comments
 5/22/03 recommented (require "comm") for lispworks (done in fildfs.lisp now)
 5/21/03 uncommented (require "comm") for lispworks (needed for recompile-boxer)
 3/28/03 string->number added for use in mail.lisp
 2/16/03 merged current LW and MCL files
10/20/02 removed RECORD-URL-BOX-PLACE from FILL-BOX-FROM-URL in preparation for
         "UC clean" reimplementation
 8/07/02 added url-values method for net-url & ftp-url
 4/06/02 changed decode-net-url to use &optional start-path-with-slash
         added decode-net-url-for-url method to allow for specialized url parsing
 4/04/02 changed moved and the class def for http-url to http.lisp
 3/23/02 save-box-via-ftp-stor, lw-write-box-data-to-handle, and
         lw-write-box-text-to-handle for lispworks
 3/19/02 fill-box-from-server checks to see if we've tried to fill the box already
         for the current event
 2/19/01 new implementation of open-tcp-stream for lispworks
         save-net-data for lispworks
 2/17/01 merged current LW and MCL files
 1/29/01 base-char, unsigned-byte are the only valid lispworks network stream types
 1/25/01 lispworks open-tcp-stream
 1/23/01 added force-output to non mcl version of net-write/read-line
 9/06/00 changed reading/writing-stream-position to methods so we can modularize
         file stream status reporting.
 8/31/00 fixed typo in fill-box-from-ftp-retr
 5/12/00 added writing/reading-stream-position and print-ftp-read/write-status
         to help with upload and download status line messages
 5/11/00 added hacked MCL version of print-ftp-status and installed it in
         fill-box-from-ftp-retr
 4/12/00 initial PC implementation
 1/03/00 ftp-local-data-port now increments to avoid timeouts on stale
         connections
12/21/99 (defun ftp-local-data-port () 1747)
 6/25/99 added record-url-box-place to fill-box-from-url
 9/23/98 check for read-only removed from url-box? so it behaves like file-box?
 9/23/98 Started Logging changes: source = boxer version 2.3beta+



|#

(in-package :boxnet)



;;; TCP implementation for lispworks....

;;now loaded in dumper.lisp
;#+lispworks
;(eval-when (load) (require "comm"))
;#+carbon-compat
;(eval-when (load) (require "OpenTransport"))


;;; Defvars and Macros...
(defvar *default-mail-address* "nobody@soe.berkeley.edu")

(defvar *user-mail-address* *default-mail-address*)

(defvar *net-verbose* t)

(defvar *correct-password-retries* 3
  "Number of times to retry getting a correct password in
   situations where a login is required.")

(defmacro surf-message (string &rest args)
  `(progn
     (debugging-message ,string . ,args)
     (when *net-verbose*
       (boxer::status-line-display 'surf-message (format nil ,string . ,args)))))

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

;; 4/12/00 - this doesn't seem to be called by anyone?
;; removed 10/13/03, leave source until next system release just in case
;(defun tcp-stream-local-port (s)
;  #+mcl
;  (ccl::rref (ccl::conn-pb (ccl::tcp-stream-conn s)) tcpioPB.status.localPort)
;  #-mcl
;  (error "TCP stream local port undefined for ~A" (machine-type))
;  )




;;; Object definitions...

;; generic
(defclass url
  ()
  ((scheme-string :initform nil :accessor scheme-string :initarg :scheme-string))
  ;; (:metaclass block-compile-class)
  ;; (:abstract-class t)
  (:documentation "Bare Bones url class-not meant to be instantiated"))

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

;; this is for files on the local host, that is, files which can be accessed
;; with the usual file access mechanisms like open-file, so NFS (or Appleshare?)
;;mounted files are included in this category since their access is transparent

(defclass local-url
  (url)
  ((pathname :initform nil :accessor local-url-pathname)
   (host-type :initform nil :accessor local-url-host-type :initarg :host-type))
  ;; (:metaclass block-compile-class)
  )

;;; This is for URL's which involve the direct use of
;;; an IP-based protocol to a specific host on the internet

;; ??? is it worth breaking this into a host,port,path class and a separate
;;     user, password class ???
(defclass net-url
  (url)
  ((user :initform nil :accessor url-user)
   (password :initform nil :accessor url-password)
   (host :initform nil :accessor url-host)
   (port :initform nil :accessor url-port)
   (path :initform nil :accessor url-path))
  ;; (:metaclass block-compile-class)
  )



;;; These are the main hooks into the rest of boxer.

(defun read-internal-url (url-string)
  (let ((box (make-box-from-url url-string)))
    ;(boxer::foo)
    (fill-box-from-url box)
    box))

(defun read-only-internal (path)
  (let* ((url-string (concatenate 'string "local://" path))
         (box (make-box-from-url url-string)))
    (fill-box-from-url box)
    ;; make sure it is Read Only
    (setf (read-only-box? box) t)
    box))

;;; When an empty box is accessed via VIRTUAL-COPY-ROWS(edvc.lisp) or
;;; the ENTER method (editor.lisp), an attempt is made to fill the contents
;;; of the box using THIS function:

(defun fill-box-from-server (box)
  (let* ((plist (plist box))
         (event-id (getf plist :fill-for-event)))
    (cond ((and (not (null event-id)) (= event-id (boxer::boxer-event-id)))
           ;; if we have already tried to fill this box for the same event,
           ;; then just give up, don't keep losing
           )
          ((getf plist :associated-file)
           ;; local file, check first for speed...
           (boxer::fill-box-from-local-file box))
          ((getf plist :url)
           ;; Network (or local) url
           (fill-box-from-url box))
          ((getf plist :fill-call)
           ;; the generalized box filling hook
           (let ((fill-call (getf plist :fill-call)))
             (apply (car fill-call) (cdr fill-call))))
          ((getf plist :box-server-id)
           ;; this is the "old style" box server (make it last)
           (fill-box-from-bfs-server box))
          (t (error "Don't know how to fill the box ~A" box)))
    (putprop box (boxer::boxer-event-id) :fill-for-event)))

(defmethod url-box? ((box boxer::box))
  (and (boxer::storage-chunk? box)
       ;(read-only-box? box)
       (getf (slot-value box 'boxer::plist) :url)))

(defun url-string? (String)
  (when (stringp string)
    (let ((cc (position #\: string)))
      (when cc
        (find-class (intern (string-upcase (format nil "~A-URL"
                                                   (subseq string 0 cc)))
                            (find-package "BOXNET"))
                    nil)))))



;; Boxer FS interface...

;; this is called from dumper code
(defmethod dump-box-url ((box boxer::box) stream)
  (let ((url (getf (slot-value box 'boxer::plist) :url)))
    (if (null url)
        (error "~A does not have a url" box)
        (dump-url url stream))))

(defmethod dump-url ((url url) stream)
  ;; fake a dump list
  (dump-list-preamble (dump-plist-length url) stream)
  (dump-plist-internal url stream))

;; this is just like other boxer object dump methods
;; with more specialized versions for particular url's
;; note that the other methods should (call-next-method) first
;; so that the type symbol comes up first
(defmethod dump-plist-internal ((self url) stream)
  (dump-boxer-thing (type-of self) stream)
  (dump-boxer-thing :scheme-string stream)
  (dump-boxer-thing (slot-value self 'scheme-string) stream))

(defmethod dump-plist-length ((self url)) 3)

(defmethod file-status-line-string ((self url))
  (if (typep self 'local-url) :local :network))

;; loading
(defun load-url (url-list)
  (apply #'make-instance (car url-list) (cdr url-list)))

(defmethod protocol-string ((url url))
  (let* ((rawtype (string (type-of url)))
         (pos (search "-URL" rawtype)))
    (if (null pos) rawtype (subseq rawtype 0 pos))))

(defmethod urlstring ((url url))
  (concatenate 'string (protocol-string url) ":"
               (slot-value url 'scheme-string)))



(defmethod set-url-flags ((box boxer::box))
  (setf (slot-value box 'boxer::flags)
        (boxer::set-box-flag-storage-chunk?
         (boxer::set-box-flag-read-only-box? (slot-value box 'boxer::flags)
                                             t)
         t)))

(defun make-box-from-url (url-string &optional (type 'boxer::data-box) name no-decode)
  (let ((url (make-url-from-string url-string no-decode))
        (box (boxer::make-uninitialized-box type)))
    (shared-initialize box t)
    ;; fix up various attributes of the box
    (unless (null name)
      (boxer::set-name box (boxer::make-name-row
                            (list (if (and (typep name 'string)
                                           (not (typep name 'simple-string)))
                                      ;; adjustable strings blow out make-name-row
                                      (copy-seq name)
                                      name)))))
    (shrink box)
    (set-url-flags box)
    (putprop box url :url)
    (boxer::set-border-style box (if (not (null (boxer::exports box)))
                                   :thick-dashed
                                   :thick))
    box))

;;; Main function for making URL's
(defun make-url-from-string (string &optional no-decode)
  (let* ((place (position #\: string))
         (type (subseq string 0 place))
         (scheme-string (subseq string (1+& place)))
         (class (find-class (intern
                             (boxer::symbol-format nil "~A-URL"
                                                   (string-upcase type))
                            (find-package "BOXNET")))))
    (if (null class)
        (error "Don't know how to make a ~A url" type)
        (make-instance class
                       :scheme-string (if no-decode
                                          scheme-string
                                          (decode-url-string scheme-string))))))

(defmethod fill-box-from-url ((box boxer::box))
  (let ((url (getf (slot-value box 'plist) :url)))
    (unless (null url)
      (fill-box-using-url url box)
      ;; if there is a cached boxtop, remove it
      (let ((cb (getprop box :cached-boxtop)))
        (unless (null cb)
          (when (box::graphics-sheet? cb)
            (let ((bm (box::graphics-sheet-bit-array cb)))
              (unless (null bm) (box::deallocate-bitmap bm))))
          (removeprop box :cached-boxtop)))
      (boxer::modified box)
      (boxer::mark-file-box-clean box))))

;;; Basic required methods for all URL's
;;;
;;; ALL URL's should explicitly support the following methods:
;;;   + INITIALIZE-INSTANCE
;;;   + COPY-URL
;;;   + FILL-BOX-USING-URL is the main interface function
;;;   + DUMP-PLIST-INTERNAL and DUMP-PLIST-LENGTH for file system interface
;;;

;;; More specific methods should use the URL parsing rules for each
;;; particular URL as defined in RFC 1738 on the scheme-string slot
(defmethod initialize-instance ((url url) &rest initargs)
  (shared-initialize url T)
  (setf (slot-value url 'scheme-string) (getf initargs :scheme-string)))

;; this should be faster since we just copy slots instead of decoding
;; the scheme-string again
(defmethod copy-url ((url url))
  ;(make-instance (class-of url) :scheme-string (slot-value url 'scheme-string))
  (let ((new (allocate-instance (class-of url))))
    (setf (slot-value new 'scheme-string) (slot-value url 'scheme-string))
    new))

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

;; signals error by default, more specific classes of url's actually do the work
(defmethod fill-box-using-url ((url url) box)
  (declare (ignore box))
  (error "Don't know how to fill box from ~A " url))



;;; Local Files

(defmethod initialize-instance ((url local-url) &rest initargs)
  (call-next-method) ;; this initializes scheme-string
  (setf (slot-value url 'pathname)
        (let ((slash-pos (search "//" (slot-value url 'scheme-string))))
          (if (and slash-pos (zerop slash-pos))
              (subseq (slot-value url 'scheme-string) 2)
              (slot-value url 'scheme-string))))
  (setf (slot-value url 'host-type)
        (or (getf initargs :host-type) (machine-type))))

(defmethod copy-url ((url local-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'pathname) (slot-value url 'pathname))
    new))

(defmethod fill-box-using-url ((url local-url) box)
  (let ((filebox (boxer::read-internal-1 (slot-value url 'pathname))))
    (initialize-box-from-net-box box filebox)
    ;; now do the box/file bookkeeping,
    ;; NOTE: It has to be here AFTER the initialize-box-from-box
    (when (box? box)
      ;(boxnet::read-box-postamble box) ;(part of the Boxer Server) no longer used
      (boxer::mark-box-as-file box (slot-value url 'pathname))
      (boxer::mark-file-box-clean box))
    ;; keep track of the file properties so that we can check
    (boxer::record-boxer-file-properties
     (slot-value url 'pathname)
     (file-write-date (slot-value url 'pathname))
     (file-author (slot-value url 'pathname)) box)
    box))

(defmethod dump-plist-internal ((self local-url) stream)
  (call-next-method)
  (dump-boxer-thing :host-type stream)
  (dump-boxer-thing (slot-value self 'host-type) stream))

(defmethod dump-plist-length ((self local-url))
  (+& (call-next-method) 2))



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




;;; as defined in RFC 1738, URL's which involve the direct use of
;;; an IP-based protocol to a specific host on the internet use a
;;; common syntax for the scheme-specific data
;;; The syntax is:     //<user>:<password>@<host>:<port>/<url-path>
;;; where "<user>:<password>@", ":<password>", ":<port>" and "/<url-path>"
;;; are all optional

(defmethod initialize-instance ((url net-url) &rest initargs)
  (call-next-method)
  (multiple-value-bind (user password host port path canonicalized-scheme-string)
      (decode-net-url-for-url url (slot-value url 'scheme-string))
    (setf (slot-value url 'user) user
          (slot-value url 'password) password
          (slot-value url 'host) host
          (slot-value url 'port) port
          (slot-value url 'path) path)
    (unless (null canonicalized-scheme-string)
      (setf (slot-value url 'scheme-string) canonicalized-scheme-string))))

(defmethod copy-url ((url net-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'user) (slot-value url 'user)
          (slot-value new 'password) (slot-value url 'password)
          (slot-value new 'host) (slot-value url 'host)
          (slot-value new 'port) (slot-value url 'port)
          (slot-value new 'path) (slot-value url 'path))
    new))

;; used by make-url-storage-info-box in file-prims
(defmethod url-values ((url net-url))
  (with-collection
    (collect "URL-Type") (collect (string-capitalize (type-of url)))
    (unless (null (slot-value url 'user))
      (collect "User") (collect (slot-value url 'user)))
    (unless (null (slot-value url 'host))
      (collect "Host") (collect (slot-value url 'host)))
    (unless (null (slot-value url 'port))
      (collect "Port") (collect (slot-value url 'port)))
    (unless (null (slot-value url 'path))
      (collect "Path") (collect (slot-value url 'path)))))

;; Base 10.
(defun string->number (string &key start stop)
  (let ((*read-base* 10.))
    (read-from-string string nil nil :start start :end stop)))

(defmethod decode-net-url-for-url ((url net-url) string)
  (decode-net-url string))

;; returns (values user password host port path)
(defun decode-net-url (string &optional start-path-with-slash)
  (let ((slash-start (search "//" string))
        (fixed-scheme-string nil))
    (if (or (null slash-start) (not (zerop& slash-start)))
     (error "Bad URL scheme string (~S)" string)
     (let* ((1st-slash (position #\/ string :start 2))
            (@place (position #\@ string :start 2 :end 1st-slash))
            (1st-colon (let ((maybe (position #\: string :start 2)))
                         ;; don't be confused by dir names with :'s
                         (unless (and maybe 1st-slash (> maybe 1st-slash)) maybe)))
            (pass? (and @place 1st-colon (<& 1st-colon @place)))
            (user (unless (null @place)
                    ;; if there is no "@", then user and password are omitted
                    (subseq string 2 (if pass? 1st-colon @place))))
            (pass (when pass? (subseq string (1+& 1st-colon) @place)))
            (next-colon (if pass?
                            (position #\: string :start @place :end 1st-slash)
                            1st-colon))
            (host (subseq string (if @place (1+& @place) 2)
                          (or next-colon 1st-slash)))
            (port (unless (null next-colon)
                    (string->number string
                                    :start (1+& next-colon) :stop 1st-slash)))
            (path (unless (null 1st-slash)
                    (let ((path-string (subseq string
                                               (if start-path-with-slash
                                                   1st-slash
                                                   (1+& 1st-slash)))))
                      (if (every #'(lambda (c) (char= c #\/)) path-string)
                          (progn (setq fixed-scheme-string
                                       (subseq string 0
                                               (1+& (position #\/ string
                                                              :test-not #'char=
                                                              :from-end t))))
                                 nil)
                          ;; trailing slashes are not according to
                          ;; spec but a fairly common practice
                        path-string)))))
       (values user pass host port path fixed-scheme-string)))))


;; use the suffix to try an infer some information about the content of the file
(defun path-suffix (path)
  (unless (null path)
    (let ((last-dot (position #\. path :from-end t))
          (semi (position #\; path :from-end t)))
      (when last-dot
        (subseq path (1+& last-dot) semi)))))



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

;; update function for boxer::*FILE-STATUS-LINE-UPDATE-FUNCTION*
(defvar *ftp-message-dot-counter* 0)

(defun ftp-message-dots-string ()
  (prog1
      (cond ((= *ftp-message-dot-counter* 0) "")
            ((= *ftp-message-dot-counter* 1) ".")
            ((= *ftp-message-dot-counter* 2) "..")
            ((= *ftp-message-dot-counter* 3) "...")
            ((= *ftp-message-dot-counter* 4) "....")
            ((= *ftp-message-dot-counter* 5) ".....")
            ((= *ftp-message-dot-counter* 6) "......")
            ((= *ftp-message-dot-counter* 7) ".......")
            (t "........"))
    (setq *ftp-message-dot-counter* (mod (1+ *ftp-message-dot-counter*) 8))))

(defun print-ftp-read-status (tcp-stream)
  #+mcl
  (surf-message "Reading ~D bytes of data"
                (reading-stream-position tcp-stream))
  #-mcl
  (surf-message "Reading from TCP connection ~A"
                (ftp-message-dots-string)))

(defun print-ftp-write-status (tcp-stream)
  #+mcl
  (surf-message "Writing ~D bytes of data"
                (writing-stream-position tcp-stream))
  #-mcl
  (surf-message "Writing TCP data ~A"
                (ftp-message-dots-string)))

;; this is basically boxer::initialize-box-from-box with extra
;; handling to reconcile the slots and properties in the embedded box
;; with the slots and properties in the box file being read in.
(defmethod initialize-box-from-net-box ((box boxer::box) netbox)
  (let ((old-ds (display-style box))
        (old-flags (slot-value box 'boxer::flags)))
    (boxer::initialize-box-from-box box netbox)
    ;; the vanilla init will take the display style from the net box
    (case old-ds
      (:shrunk (shrink box))
      (:supershrunk (boxer::supershrink box)))
    ;; make sure certain flags in the existing box are preserved...
    (when (and (boxer::box-flag-storage-chunk? old-flags)
               (not (storage-chunk? box)))
      (setf (storage-chunk? box) t))
    (when (and (boxer::box-flag-read-only-box? old-flags)
               (not (read-only-box? box)))
      (setf (read-only-box? box) t))
    ;; make sure the border is (or stays) thick because
    ;; if the net box was not saved with thick borders, it will bash
    ;; the existing border style to thin
    (boxer::set-border-style box (if (not (null (boxer::exports box)))
                                   :thick-dashed
                                   :thick))
  box))

(defun save-net-data (stream box doc-type
                             &optional
                             (text-data? (member doc-type
                                                 '(:text :binhex :uuencode)))
                             pathname)
  (let ((file (or pathname
                  #+mcl
                  (ccl::choose-new-file-dialog
                   :prompt (format nil "Save ~A data in:" doc-type))
                  #+lispworks
                  (capi:prompt-for-file
                   "save MIME data in file:"
                   :filter bw::*boxer-file-filters*
                   :operation :save :if-does-not-exist :ok
                   :owner bw::*boxer-frame*)))
        (eof-value (list 'eof)))
    (if text-data?
        (with-open-file (outstream file :direction :output)
          (do ((line (net-read-line stream) (net-read-line stream)))
              ((or (null line) (string= line ".")))
            (write-line line outstream)))
        (with-open-file (outstream file :direction :output
                                   :element-type '(unsigned-byte 8))
          (do ((byte (read-byte stream nil eof-value)
                     (read-byte stream nil eof-value)))
              ((eq byte eof-value))
            (write-byte byte outstream))))
    ;; if we got here, make a box informing the user...
    (append-row box (make-row (list doc-type "data" "saved" "in")))
    (append-row box (make-row (list (namestring file))))
    file))
