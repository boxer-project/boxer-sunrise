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
  ;; (:metaclass block-compile-class)
  )

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

(defmethod fill-box-using-url ((url http-url) box)
  (multiple-value-bind
    (stream status-code headers)
    (drakma:http-request (urlstring url) :want-stream t)
    (cond ((and (eq status-code 200)
                (eq :box (slot-value url 'doc-type)))
             (let ((b (let ((boxer::*file-system-verbosity* T)
                            (boxer::*FILE-STATUS-LINE-UPDATE-FUNCTION* 'print-ftp-read-status))
                        (boxer::load-binary-box-from-stream-internal stream))))
               (initialize-box-from-net-box box b)
               (set-url-flags box)))
          ((eq status-code 200) ; :binary :text
             ;; sgithens 2020-10-21 We should be handling this the same way opening a local file
             ;; does with other file types...
             (save-net-data stream box :binary))
          (t
             (surf-message "HTTP Error detected ~A ~A" status-code "Http Error")
             (boxer-eval::primitive-signal-error :http status-code "Http Error"))))
  (boxer::status-line-undisplay 'surf-message))
