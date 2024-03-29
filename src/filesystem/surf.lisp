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

(defun save-net-data (stream box doc-type &optional pathname)
  (let ((file (or pathname
                  #+lispworks
                  (capi:prompt-for-file
                   "save MIME data in file:"
                   :filters bw::*boxer-file-filters*
                   :operation :save :if-does-not-exist :ok
                   :owner bw::*boxer-frame*)))
        (eof-value (list 'eof)))
    (with-open-file (outstream file :direction :output
                                :element-type '(unsigned-byte 8))
      (do ((byte (read-byte stream nil eof-value)
                  (read-byte stream nil eof-value)))
          ((eq byte eof-value))
        (write-byte byte outstream)))
    ;; if we got here, make a box informing the user...
    (append-row box (make-row (list doc-type "data" "saved" "in")))
    (append-row box (make-row (list (namestring file))))
    file))


;;; Dumper/Loader utilities
;;; The next 3 functions have to be in agreement about when and what to
;;; dump.  They are used in the dumper.lisp file to control dumping of inferiors
;;; as well as what extra info needs to be dumped out so that the box can
;;; later be read in automatically
;;; There are currently three types of auto reading boxes (black boxes)
;;;    o Boxer Server Boxes: distinguished by numeric ID's in the :server-box-id
;;;      plist property for the most part, these are obsolete
;;;    o URL boxes: look for URL (Universal Resource Locator) Structs in the
;;;      plist property.
;;;    o File boxes: look for pathname (or string) in the :associated-file
;;;      property in the plist
;;;
;;; 2022-09-28 All of the below have been moved here from client.lisp in the process
;;;   of full deprecating the old Boxer Server code. Look there for the old full
;;;   versions that contain the first check above for Boxer Server Boxes. Those have
;;;   been removed from here.

;; the dumper walks through all the inferiors of the box being dumped.  If an
;; inferior box meets this test, then only its top level characteristics are
;; dumped but we do not recurse through the inferior box's inferiors
(defun no-inferiors-for-file? (box)
  (let ((plist (plist box)))
    (and (storage-chunk? box)
         ;; storage-chunk boxes are the granularity of delayed loading
         ;; we now test each of the three cases described above...
         ;; we could probably streamline the logic but at least this way,
         ;; the 3 cases are easily distinguished
         (or (and (getf plist :url)
                  (not (eq box *outermost-dumping-box*)))
             (and (getf plist :associated-file)
                  (not (eq box *outermost-dumping-box*)))))))

;; sub box relocation table, branch file links, inf file links
(defun storage-chunk-plist-half-length (box)
  (let* ((half-length 0)
         (boxtop-prop (getprop box :boxtop))
         (boxtop (cond ((or (null boxtop-prop)
                            (eq boxtop-prop :standard)
                            (eq boxtop-prop :framed))
                        (boxer::boxtop box))
                       ((eq boxtop-prop :file)
                        (let ((bt (boxer::boxtop box)))
                          (when (boxer::graphics-sheet? bt) bt))))))
    (cond
     ((url-box? box) (incf& half-length) (when boxtop (incf& half-length)))
     ((boxer::file-box? box)
      ;(incf& half-length)
      ;; 8/31/05 in addition to filename, we dump out dirs,name,type
      (incf& half-length 4)
      (when boxtop (incf& half-length))))
    half-length))

(defmethod dump-storage-chunk-plist-items ((self boxer::box) stream)
  (let* ((boxtop-prop (getprop self :boxtop))
         (boxtop (cond ((or (null boxtop-prop)
                            (eq boxtop-prop :standard)
                            (eq boxtop-prop :framed))
                        (boxer::boxtop self))
                       ((eq boxtop-prop :file)
                        (let ((bt (boxer::boxtop self)))
                          (when (boxer::graphics-sheet? bt) bt))))))
    (cond
     ((url-box? self)
      (dump-boxer-thing :url stream)
      (dump-box-url self stream)
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     ((boxer::file-box? self)
      (let* ((file (filename-for-file self))
             (dirs (pathname-directory file))
             (name (pathname-name      file))
             (type (or (pathname-type file) :unspecific)))
        (dump-boxer-thing :associated-file stream)
        (dump-boxer-thing file stream)
        ;; dump out the filename components as well for cross platform portability
        (dump-boxer-thing :associated-file-dirs stream)
        (dump-boxer-thing dirs stream)
        (dump-boxer-thing :associated-file-name stream)
        (dump-boxer-thing name stream)
        (dump-boxer-thing :associated-file-type stream)
        (dump-boxer-thing type stream))
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     )))

;; there are 4 case here, the relative filename flag can be on or off and
;; the filename can already be either a relative one or an absolute one
;; If the the filename matches the flag, we just return the filename otherwise
;; we'll have to do some work
(defmethod filename-for-file ((box boxer::box))
  (let* ((local-flag (boxer::relative-filename? box))
         (filename (getprop box :associated-file))
         (relative? (and filename (eq (car (pathname-directory filename))
                                      :relative))))
    (if (or (and relative? local-flag)
            (and (not relative?) (not local-flag)))
        (namestring filename) ; we have what we want
        (let* ((sup-file-box (boxer::current-file-box (superior-box box)))
               (sup-file (getprop sup-file-box :associated-file)))
          (cond ((null sup-file) ; save as absolute
                 (if relative?
                     (namestring (boxer::boxer-new-file-dialog
                                  :prompt
                                  "The Box has no superior filename to merge"
                                  :directory filename
                                  :box box))
                     (namestring filename)))
                ((and local-flag (not relative?))
                 ;; we want to save relative, but have an absolute
                 ;; use enough-namestring only for sorting directory
                 ;; structure otherwise type info can get lost
                 (namestring
                  (make-pathname :directory (pathname-directory
                                             (enough-namestring filename
                                                                sup-file))
                                 :name (pathname-name filename)
                                 :type (pathname-type filename))))
                (t ; must have relative, but want to save absolute
                 (namestring (merge-pathnames filename sup-file))))))))
