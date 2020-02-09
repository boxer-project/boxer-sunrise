;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-


#|


           Copyright 2004 - 2005 PyxiSystems LLC


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
  (format stream "From - ~A~&" (rfc822-date-time)))

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
