;;;-*-LISP-*-


;;; $Header: boxnet.lisp,v 1.0 90/01/24 22:07:00 boxer Exp $

;;; $Log:	boxnet.lisp,v $
;;;Revision 1.0  90/01/24  22:07:00  boxer
;;;Initial revision
;;;

#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+


Modification History (most recent at top)

 2/13/03 merged current LW and MCL files, no differences, copyright updated


|#

(in-package :boxnet)

;;; Call setup-server.
;;; Then call either SETUP-CONNECTION-WAITING-PROCESS
;;; or ENABLE-POLLING with a function which receives
;;; a stream as input.  That function should deal with the stream,
;;; but should not interact with the display or anything.
;;; It should use the bw::*boxer-command-loop-event-handlers-queue*
;;; mechanism for getting things to run later.
;;;
;;; We should macroize all that and relax the restrictions and
;;; probably just settle on polling and punt the interrupt stuff
;;; and regularize the names.

#|
(eval-when (load eval)
           #+(and (not solaris) Lucid) (progn
                                        #+sparc   (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "receive.sun4.o"))
                                        #+MC68000 (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "receive.sun3.o"))
                                        #+sparc   (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "send.sun4.o"))
                                        #+MC68000 (lcl:load-foreign-files
                                                   (merge-pathnames
                                                    (getf (sm::system-properties
                                                           (find-system-named 'boxer))
                                                          :pathname-default)
                                                    "send.sun3.o")))
           )

;;; At least remind during compilation....

(eval-when (compile)
           (format t "~%***~%NOTE:You may have to recompile the C files send.c ~%~
             and receive.c for this architecture~%***")
           )

|#

(defvar *foreign-netsocket-file*
  #+(and sparc (not solaris)) "netsocket.sun4.o"
  #+(and sparc solaris)       nil ; "netsocket.solaris.o"
  #+MC68000                   "netsocket.sun3.o")

(eval-when (eval load)
           #+lucid (unless (null *foreign-netsocket-file*)
                     (lcl::load-foreign-files
                      (merge-pathnames
                       (getf (sm::system-properties (find-system-named 'boxer))
                             :pathname-default)
                       *foreign-netsocket-file*)))
           )

;;;; compilation reminder (shouldn't the defsystem do this ?)
(eval-when (compile)
           (format t "~%***~%NOTE:You may have to recompile the C files netsocket.c ~%~
             for this architecture~%***")
           )


;;returns the file descriptor bits to give to select-and-accept.
#+(and Lucid (not solaris))
(lcl:define-c-function (socket-listen-and-bind "_socket_listen_and_bind")
                       ((port-number :integer))
                       :result-type :integer)

;; returns a unix file handle for make-lisp-stream.
#+(and Lucid (not solaris))
(lcl:define-c-function (select-and-accept "_select_and_accept")
                       ((socket-object :integer)
                        (poll-time-in-usec :integer))
                       :result-type :integer)


#+(and Lucid (not solaris))
(lcl:define-c-function (%close-socket "_close") ((fd :integer)) :result-type :integer)

;; returns a unix file handle for make-lisp-stream.
#+(and Lucid (not solaris))
(lcl:define-c-function (socket-open-and-connect "_socket_open_and_connect")
                       ((hostname :string) (port-number :integer))
                       :result-type :integer)




(defvar *socket* nil)
(defvar *boxer-port-number* 7000)

(defvar *connection-waiting-process* nil)
(defvar *connection-port* nil)

(defun setup-server ()
  (when (or (null *socket*) (not (plusp *socket*)))
    (let ((socket (socket-listen-and-bind *boxer-port-number*)))
      (when (minusp socket)
        (warn "Failed to set up for remote connection:"))
      (setq *socket* socket))))

#+Lucid
(defun setup-connection-waiting-process (handler-function)
  (setq *connection-waiting-process*
        (lcl::make-process
         :function #'handle-connection :args (list handler-function)
         :wait-function (let ((socket *socket*)
                              (process lcl::*current-process*))
                          #'(lambda ()
                                    ;; can't use global variables in the wait function.
                                    (let ((port (select-and-accept socket 100)))
                                      (if (not (zerop port))
                                        (progn
                                         (setf (lcl::symbol-process-value
                                                '*connection-port* process) port)
                                         t)
                                        nil)))))))

;;; the server process calls this on the stream when the connection is made.
(defun handle-connection (handler-function)
  (cond ((minusp *connection-port*)
         (when (eq boxer::*boxer-send-server-status* :interrupt)
           (close-server)
           (format t "<error: connection lost -- killing process>~%")))
    (t
     (let ((stream nil))
       (unwind-protect
        (progn (setq stream
                     #+Lucid (lcl::make-lisp-stream :input-handle  *connection-port*
                                                    :output-handle  *connection-port*
                                                    :element-type '(unsigned-byte 16))
                     #-Lucid (warn "~S undefined in this Lisp" 'make-lisp-stream))
               (funcall handler-function stream))
        (close stream)
        (setq *connection-port* nil))))))

(defun close-server ()
  (when (and *socket* (plusp *socket*))
    (%close-socket *socket*)
    (setq *socket* nil))
  (when (and *connection-port* (plusp *connection-port*))
    (close *connection-port*)
    (setq *connection-port* nil))
  (when *connection-waiting-process*
    #+Lucid (lcl::kill-process *connection-waiting-process*)
    #-Lucid (warn "~S undefined in this Lisp" 'kill-process)
    (setq *connection-waiting-process* nil)))


;;; polling
(defvar *polling-handler* nil)
(defvar *net-connection-toplevel-polling-time* #+Lucid 200)

#+Lucid
(defun net-connection-toplevel-polling-function ()
  (when (not (null *polling-handler*))
    (let ((port (select-and-accept *socket*  *net-connection-toplevel-polling-time*)))
      (when (not (zerop port))
        (setq *socket* nil)
        (setq *connection-port* port)
        (handle-connection *polling-handler*)))))


(defun enable-polling (function)
  (setq *polling-handler* function))

(defun disable-polling ()
  (setq *polling-handler* nil))



;;; send
(defmacro with-open-port-stream ((stream-var hostname) &body body)
  `(with-open-stream
     (,stream-var (let ((fd (socket-open-and-connect ,hostname)))
                    (when (minusp fd)
                      (boxer-eval::primitive-signal-error
                       "Couldn't connect to host:" ,hostname))
                    #+Lucid (lcl:make-lisp-stream :input-handle fd
                                                  :output-handle fd
                                                  :element-type '(unsigned-byte 16))
                    #-Lucid (warn "~S undefined for this Lisp" 'MAKE-LISP-STREAM)))
     .,BODY))



