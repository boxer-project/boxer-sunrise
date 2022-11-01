;;;;
;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;

(in-package :boxnet)

;; this is for files on the local host, that is, files which can be accessed
;; with the usual file access mechanisms like open-file, so NFS (or Appleshare?)
;;mounted files are included in this category since their access is transparent

(defclass local-url
  (url)
  ((pathname :initform nil :accessor local-url-pathname)
   (host-type :initform nil :accessor local-url-host-type :initarg :host-type))
  ;; (:metaclass block-compile-class)
  )

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
