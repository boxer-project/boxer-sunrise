;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-
#|


 $Header$

 $Log$


  Copyright 1994 - 1998 Regents of the University of California

 Enhancements and Modifications Copyright 1999 - 2004 PyxiSystems LLC


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

#-(or lispworks mcl lispm) (in-package 'boxnet)
#+(or lispworks mcl)       (in-package :boxnet)



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
  (:metaclass block-compile-class)
  (:abstract-class t)
  (:documentation "Bare Bones url class-not meant to be instantiated"))

(defclass mailto-url
  (url)
  ((address :initform *default-mail-address* :accessor mailto-url-address))
  (:metaclass block-compile-class))

;; this is for URL files possibly relative to some superior URL
(defclass file-url
  (url)
  ((pathname :initform nil :accessor file-url-pathname))
  (:metaclass block-compile-class))

;; this is for files on the local host, that is, files which can be accessed
;; with the usual file access mechanisms like open-file, so NFS (or Appleshare?)
;;mounted files are included in this category since their access is transparent

(defclass local-url
  (url)
  ((pathname :initform nil :accessor local-url-pathname)
   (host-type :initform nil :accessor local-url-host-type :initarg :host-type))
  (:metaclass block-compile-class))

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
  (:metaclass block-compile-class))

(defclass ftp-url
  (net-url)
  (;; this can be :text, :binary, :box, :directory or some other boxer
   ;; document type as defined in the boxer generic file system
   ;; see the boxer::*special-file-readers* variable
   (doc-type :initform ':text :accessor ftp-url-doc-type :initarg :doc-type))
  (:metaclass block-compile-class))

(defclass gopher-url
  (net-url)
  (;; This should be a keyword based on the defined gopher types (RFC 1436 pg 10)
   ;; or else :box if we can infer that it is a boxer file
   (doc-type :initform ':text :accessor gopher-url-doc-type :initarg :doc-type))
  (:metaclass block-compile-class))

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
      (dolist (sb (boxer::screen-objs box))
        (boxer::set-force-redisplay-infs? sb t))
      (boxer::modified box)
      ;; removed boxer::record-url-box-place
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

;; use the suffix to try an infer some information about the content of the file
(defun path-suffix (path)
  (unless (null path)
    (let ((last-dot (position #\. path :from-end t))
          (semi (position #\; path :from-end t)))
      (when last-dot
        (subseq path (1+& last-dot) semi)))))

(defun declared-ftp-type (scheme-string)
  (let* ((semi (position #\; scheme-string))
         (type? (when semi (search "type=" scheme-string
                                   :start2 semi :test #'char-equal))))
    (when type?
      (char scheme-string (+& semi 6)))))

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
        (eval::primitive-signal-error :net-error (copy-seq error-string))
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
           (let ((eval::*exception-signalled* nil)
                 (eval::*error-type* nil)
                 (eval::*error-sfun* nil)
                 (eval::*exception-args* nil)
                 ;; these are vars that eval:signal-error will frob, so we bind
                 ;; them in this process and then return them to the boxer process
                 ;; interrupt handling works by signalling a boxer error which
                 ;; will be caught by this scheme and handled generically

                 ;; also need to bind file activity vars that status line
                 ;; reporting uses
                 )
             (catch 'eval::boxer-primitive-error-signal
               (let ((,normal-result
                      (mp:with-lock (*net-filling-lock*) . ,body)))
                 (cond ((null eval::*exception-signalled*)
                        (setq *net-filling-status* nil)
                        ,normal-result)
                       (t (setq *net-filling-status* (eval::encapsulate-error))
                          nil)))))))))

  (defun prepare-net-fill-box (box &optional url)
    (setq *net-filling-box* box
          *net-filling-url* url
          *net-filling-status* nil)
    #+(or lispworks5.1 lispworks6)
    (mp:process-unlock *net-filling-lock*)
    #-(or lispworks5.1 lispworks6)
    (mp:release-lock *net-filling-lock*)
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
             ((eval::error-state-p *net-filling-status*)
              (eval::signal-error-from-state *net-filling-status*))
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
          (eval::check-for-interrupt :interrupt "Stopped by User !"))
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
             (eval::check-for-interrupt :interrupt "Stopped by User !"))
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
           ((eval::error-state-p *net-filling-status*)
            (eval::signal-error-from-state *net-filling-status*))
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
    (eval::check-for-interrupt :interrupt "Stopped by User !")))


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
          ((eval::error-state-p *net-filling-status*)
           (eval::signal-error-from-state *net-filling-status*))
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
(defboxer-primitive bu::telnet (host)
  #+mcl
  (let ((ncsa-pathname (make-pathname :host "home"
                                      :name "NCSA-telnet-temp")))
    (unwind-protect
        (progn (make-NCSA-telnet-doc ncsa-pathname (box-text-string host))
               (ccl::select-process-with-docs :NCSA ncsa-pathname))
      (delete-file ncsa-pathname)))
  eval::*novalue*)

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


(defboxer-primitive bu::gopher-search (host (eval::numberize port)
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
    (eval::check-for-interrupt :interrupt "Stopped by User !"))
|#

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

(defmethod dump-plist-internal ((self gopher-url) stream)
  (call-next-method)
  (dump-boxer-thing :doc-type stream)
  (dump-boxer-thing (slot-value self 'doc-type) stream))

(defmethod dump-plist-length ((self gopher-url))
  (+& (call-next-method) 2))



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
  (mp:release-lock *tl*)
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
