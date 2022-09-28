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

;;; Defvars and Macros...
(defvar *default-mail-address* "nobody@soe.berkeley.edu")

(defvar *user-mail-address* *default-mail-address*)

(defvar *net-verbose* t)

(defmacro surf-message (string &rest args)
  `(progn
     (log:debug ,string . ,args)
     (when *net-verbose*
       (boxer::status-line-display 'surf-message (format nil ,string . ,args)))))

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


;;; Object definitions...


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
           ;;(fill-box-from-bfs-server box))
           (error "Trying to load an old bfs-server document."))
          (t (error "Don't know how to fill the box ~A" box)))
    (putprop box (boxer::boxer-event-id) :fill-for-event)))

;; Boxer FS interface...






(defun read-hex-pair (char1 char2)
  (flet ((char->number (char)
           (case char
             (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7)
             (#\8 8) (#\9 9) ((#\a #\A) 10.) ((#\b #\B) 11.) ((#\c #\C 12.))
             ((#\d #\D) 13.) ((#\e #\E) 14.) ((#\f #\F) 15.)
             (otherwise (error "% in url's should be encoded as %25")))))
    (+& (*& 16. (char->number char1)) (char->number char2))))



;; use the suffix to try an infer some information about the content of the file
(defun path-suffix (path)
  (unless (null path)
    (let ((last-dot (position #\. path :from-end t))
          (semi (position #\; path :from-end t)))
      (when last-dot
        (subseq path (1+& last-dot) semi)))))

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
  (surf-message "Reading from TCP connection ~A"
                (ftp-message-dots-string)))

(defun print-ftp-write-status (tcp-stream)
  (surf-message "Writing TCP data ~A"
                (ftp-message-dots-string)))

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
