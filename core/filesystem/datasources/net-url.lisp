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
   (path :initform nil :accessor url-path)))

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
