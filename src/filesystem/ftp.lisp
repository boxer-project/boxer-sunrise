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
