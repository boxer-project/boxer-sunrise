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

;; generic
(defclass url
  ()
  ((scheme-string :initform nil :accessor scheme-string :initarg :scheme-string))
  ;; (:abstract-class t)
  (:documentation "Bare Bones url class-not meant to be instantiated"))



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

;; signals error by default, more specific classes of url's actually do the work
(defmethod fill-box-using-url ((url url) box)
  (declare (ignore box))
  (error "Don't know how to fill box from ~A " url))

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
