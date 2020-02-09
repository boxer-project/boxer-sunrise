;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-
#|



               Copyright 2001 - 2003 PyxiSystems LLC


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



   This file contains boxer http related functions & methods


Modification History (most recent at top)

 2/16/03 copied into unified (LW & MCL) source
 8/07/02 added url-values
 4/04/02 started http.lisp file

|#



(in-package :boxnet)

;;; http-url methods for
;;;   + INITIALIZE-INSTANCE
;;;   + COPY-URL
;;;   + FILL-BOX-USING-URL is the main interface function
;;;   + DUMP-PLIST-INTERNAL and DUMP-PLIST-LENGTH for file system interface
;;;

(defclass http-url
  (net-url)
  (;; used by boxer
   (doc-type :initform ':text :accessor http-url-doc-type :initarg :doc-type))
  (:metaclass block-compile-class))

(defvar *default-url-port* 80)

(defmethod initialize-instance ((url http-url) &rest initargs)
  (call-next-method)
  ;; now set up default values for user, port and password if they
  ;; haven't been filled by the net-url method
  (when (null (slot-value url 'port))
    (setf (slot-value url 'port) *default-url-port*))
  (let* ((path (slot-value url 'path))
         (suffix (path-suffix path))
         (supplied-doc-type (getf initargs :doc-type)))
    (if (not (null supplied-doc-type))
        ;; if the doc-type is in the initargs, go with it (the slot will
        ;; already have been set by shared-initialize in an earlier method)
        (setf (slot-value url 'doc-type) supplied-doc-type)
        (setf (slot-value url 'doc-type)
              (cond ((null suffix) ':text)
                    ((string-equal suffix "box") ':box)
                    ;; add any other recognized suffixes here
                    ((or (string-equal suffix "txt") (string-equal suffix "text"))
                     ':text)
                    (t ':binary))))))

(defmethod copy-url ((url http-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'doc-type) (slot-value url 'doc-type))
    new))

(defmethod url-values ((url http-url))
  (let ((dt (slot-value url 'doc-type)))
    (cond ((null dt)
           (call-next-method))
          (t (append (call-next-method) (list "Document-Type" dt))))))

;; we might want to set up some defaults here (like index.html, or .box)
(defmethod decode-net-url-for-url ((url http-url) string)
  (decode-net-url string t))



;;; http input streams

(defclass http-binary-input-stream
  #+mcl (stream) ;; maybe ccl::output-stream ?
  #+lispworks (stream::fundamental-stream)  ; ; maybe stream::stdobj-stream ?
  ((charstream :initarg :charstream)
   (length :initform most-positive-fixnum :initarg :length)
   (position :initform 0 :initarg :position)
   (close-p :initform nil))
  )

(defun make-http-binary-input-stream (charstream &optional clength)
  (let ((hbis (make-instance 'http-binary-input-stream :charstream charstream)))
    (unless (null clength) (setf (slot-value hbis 'length) clength))
    hbis))

(defmethod #+mcl ccl::stream-read-byte #+lispworks stream::stream-read-byte
  ((stream http-binary-input-stream))
  (let* ((eof-value (list 'eof))
         (char (read-char (slot-value stream 'charstream) nil eof-value)))
    (cond ((or (slot-value stream 'close-p)
               (>= (slot-value stream 'position) (slot-value stream 'length))
               (eq char eof-value)) nil)
          (t (incf (slot-value stream 'position))
             (char-code char)))))

#+mcl
(defmethod ccl::stream-eofp ((stream base64-input-stream))
  (not (null (slot-value stream 'close-p))))

(defmethod #+mcl ccl::stream-listen #+lispworks stream::stream-listen
  ((stream http-binary-input-stream))
  (listen (slot-value stream 'charstream)))

;; do nothing to the char stream for now, in theory, we
;; may want to keep open (and reuse) the http network stream for multiple
;; requests, so a close of the box loading binary stream should leave the
;; http network charstream alone.
#+mcl
(defmethod ccl::stream-close ((stream base64-input-stream))
  (setf (slot-value stream 'close-p) t))

#+lispworks
(defmethod close ((stream base64-input-stream) &key abort)
  (setf (slot-value stream 'close-p) t))

(defmethod stream-element-type ((stream http-binary-input-stream))
  '(unsigned-byte 8))



;;;; Boxer implementation of a simple version of the GET method as
;;;; defined in RFC 2616 (all page numbers reference that RFC)

#|
request headers (pg 38)

 defaults OK for now...
Accept (pg 100), Accept-Charset (pg 102)
Accept-Encoding (pg 102): identity;q=1.0   identity only (no compress or gzip)
                          chunked for text OK
Accept-Language(pg 104) we'll take whatever we get...
Authorization
Expect (pg 126) ignore
From (pg 128) could use either the user mail address for this, ignore for now
Host required when HTTP/1.1 in method line
 conditional requests, skip for now...
If-Match, If-Modified-Since, If-None-Match, If-Range, If-Unmodified-Since
Max-Forwards
Proxy-Authorization
Range
Referer
TE
User-Agent: Boxer/PC-2.5

|#

;; eventually, we will want to reuse HTTP stream which remain open for a short
;; time (~5-10 secs) rather than opening and closing the stream for each
;; transaction.  Re-use will involve resourcing, and checking to see if the stream
;; is still open, etc
(defmacro with-http-stream ((stream-var host port) &body body)
  `(let ((,stream-var (open-tcp-stream ,host ,port)))
     (unwind-protect
         (progn . ,body)
       (close ,stream-var))))

(defmethod fill-box-using-url ((url http-url) box)
  (with-http-stream (stream (slot-value url 'host) (slot-value url 'port))
      (send-http-request url stream)
      ;; now we get a reply header and parse it
      (multiple-value-bind (status encoding plist header-lines)
          (handle-http-reply stream)
        (declare (ignore header-lines)) ; we might use these later for error text
        ;; note that even for error, we may have to empty out the
        ;; rest of the message
        (string-case status
          ("200"
           (case (slot-value url 'doc-type)
             (:box (let ((b (let ((boxer::*file-system-verbosity* T)
                                  (boxer::*FILE-STATUS-LINE-UPDATE-FUNCTION*
                                   'print-ftp-read-status))
                              (boxer::load-binary-box-from-stream-internal
                               (make-http-binary-input-stream
                                stream (getf plist :content-length))))))
                     (initialize-box-from-net-box box b)
                     (set-url-flags box)))
              ((or :binary :text)
               ;; a bit of a crock....
               (save-net-data (make-http-binary-input-stream
                               stream (getf plist :content-length))
                              box
                              :binary))))
          (("301" "307") ;; redirects...
           ;; there should be a :location property in the plist for redirects
           )
          (t (handle-http-error status encoding plist stream)))))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
        (boxer::status-line-undisplay 'surf-message)))

(defun handle-http-error (status encoding plist stream)
  ;; we may have to empty out a message body from the stream 1st
  (let ((length (getf plist :content-length 0)))
    (cond ((eq encoding :chunked) (empty-chunk-data stream))
          ((> length 0)
           ;; this should make a box, (later) and pass it to the sognal-error-
           (dotimes (i length) (read-char stream))))
  (signal-http-error status (getf plist :reason-phrase "Http Error"))))

(defun signal-http-error (code reason)
  (surf-message "HTTP Error detected ~A ~A" code reason)
  (eval::primitive-signal-error :http code reason))

(defun make-box-from-http-char-stream (stream)
  )

(defmethod send-http-request ((url http-url) stream)
  (surf-message "  Sending HTTP request...")
  (net-write-control-line stream "GET ~A HTTP/1.1" (slot-value url 'path))
  (net-write-control-line stream "host: ~A" (slot-value url 'host))
 ;(net-write-control-line stream "Accept-Encoding: identity;q=1.0")
  (let* ((smps (sm::system-properties (find-system-named 'boxer::boxer)))
         (major (getf smps :major-version-number 2))
         (minor (getf smps :minor-version-number 5)))
    (net-write-control-line stream "User-Agent: Boxer/~D.~D" major minor))
  ;(net-write-control-line stream "From: ~A" *user-mail-address*)
  (net-write-control-line stream "") ; CRLF
  )

;;;; Responses (pg 39)
;;    status-line = HTTP-Version <SP> Status-Code <SP> Reason-Phrase <CRLF>
;;                    |-------------------^
;;                 1xx Informational, 2xx Success, 3xx Redirection,
;;                 4xx Client Error, 5xx Server Error
;;                  details:
;;                  100 Continue, 101 Switching Protocols
;;                  200 OK, 201 Created, 202 Accepted, 203 Non-Authoritative Info,
;;                  204 No Content, 205 Reset Content, 206 Partial Content
;;                  300 Multiple Choices, 301 Moved Permanently, 302 Found,
;;                  303 See Other, 304 Not Modified, 305 Use Proxy,
;;                  307 Temporary Redirect
;;                  400 Bad Request, 401 Unauthorized, 402 Payment Required,
;;                  403 Forbidden, 404 Not Found, 405 Method Not Allowed
;;                  406 Not Acceptable, 407 Proxy Authentication Required,
;;                  408 Request Timeout, 409 Conflict, 410 Gone,
;;                  411 Length Required, 412 Precondition failed,
;;                  413 Request Entity Too Large, 414 Request-URI Too Large,
;;                  415 Unsupported Media Type,416 Requested range not satisfiable,
;;                  417 Expectation Failed
;;                  500 Internal Server Error, 501 Not Implemented,502 Bad Gateway,
;;                  503 Service Unavailable, 504 Gateway Time-Out
;;                  505 HTTP Version not supported
;;    general-header
;;    response-header
;;    entity-header
;;      CRLF
;;      CRLF
;;    message-body
;;
;;  we should handle:
;; 200, report other 200's as errors for now
;; all 400's and 500's as errors
;; try to be smart about 301 and 307, reporting errors for other 300's
;; report 100's as errors for now


;; the most important task is to determine the status & the encoding scheme
;; (values status encoding plist header-lines)
;; Also, the reply header should be completely removed from the stream
(defun handle-http-reply (stream)
  (let* ((status-line (net-read-line stream t))
         (header-lines nil))
    (surf-message "  Handling HTTP reply header ~A" status-line)
    (cond ((null status-line) )
          (t (let* ((1st-space (position #\space status-line))
                    (http-version (subseq status-line 0 1st-space))
                    (status-code  (subseq status-line
                                          (1+ 1st-space) (+ 1st-space 4)))
                    (reason-phrase (subseq status-line (+ 1st-space 5)))
                    (encoding :identity)
                    (plist (list :http-version http-version
                                 :reason-phrase reason-phrase)))
               ;; now empty out the rest of the header....
               (do ((line (net-read-line stream) (net-read-line stream)))
                   ((string= line "")
                    (values status-code encoding plist (nreverse header-lines)))
                 (push line header-lines)
                 (multiple-value-bind (field-name colon-pos)
                     (header-line-field-name line)
                   (when (not (null colon-pos))
                     (let* ((string-buffer (make-string-buffer 50))
                            (value-string (header-line-value line string-buffer
                                                             (1+ colon-pos))))
                       (string-case field-name
                         ("Transfer-Encoding"
                          (string-case value-string
                            ("Indentity" ) ; do nothing
                            ("Chunked" (setq encoding :chunked))
                            (t (error "Unhandled content encoding: ~A"
                                      value-string))))
                         (("Etag" "Last-Modified" "Location")
                          ;; this is the common case where we just want a string
                          ;; value in the plist, with the field-name as the key
                          (setq plist (nconc plist
                                             (list (intern-keyword field-name)
                                                   value-string))))
                         ("Content-Length"
                          (let ((number (read-from-string line nil nil
                                                          :start (1+ colon-pos))))
                            (when (numberp number)
                              (setq plist
                                    (nconc plist (list :content-length
                                                       number))))))))))))))))




;;; read chunked text
;;; section 3.6.1, pg 25
;;;
;;; chunk-size CRLF
;;; (possible) chunk-extension = ";" token-name "=" chunk-ext-value
;;; chunk-data
;;; (possible) trailer NOTE:don't expect trailers because we will never indicate
;;;                         that we will handle them (using TE header field)
;;;

;; basic framework...
(defun empty-chunk-data (stream &optional errorp)
  (do* ((chunk-size-line (read-line stream nil nil) (read-line stream nil nil))
        (chunk-size (when chunk-size-line
                      (read-from-string chunk-size-line nil nil))
                    (when chunk-size-line
                      (read-from-string chunk-size-line nil nil))))
       ((not (numberp chunk-size))
        (when errorp (error "Bad chunk size: ~A" chunk-size)))
    (cond ((zerop chunk-size)
           ;; the end, read one more line, then return
           (read-line stream nil nil)
           (return))
          (t (dotimes (i chunk-size) (read-char stream nil nil))))))

(defun make-box-from-chunk-stream (stream &optional errorp)
  )



;;; testing, testing....

#|

(setq url (make-url-from-string "http://soe.berkeley.edu/~ehl/pctest.box"))
(setq hs (open-tcp-stream "soe.berkeley.edu" 80))
(send-http-request url hs)
(handle-http-reply hs)

|#
