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
;;;;     Current implementations for Gopher
;;;;

(in-package :boxnet)

(defclass gopher-url
  (net-url)
  (;; This should be a keyword based on the defined gopher types (RFC 1436 pg 10)
   ;; or else :box if we can infer that it is a boxer file
   (doc-type :initform ':text :accessor gopher-url-doc-type :initarg :doc-type))
  ;; (:metaclass block-compile-class)
  )

;;;; Gopher

;; From RFC 1738 pg 9-10:
;; Gopher URL takes the form:
;;    gopher://<host>:<port>/<gopher-path>
;; where <gopher-path> is one of
;;    <gophertype><selector>
;;    <gophertype><selector>%09<search>
;;    <gophertype><selector>%09<search>%09<gopher+_string>
;; if :<port> is ommitted, the port defaults to 70.  <gophertype> is a
;; single-character field to denote the gopher type of the resource to
;; which the URL refers.  The entire <gopher-path> may also be empty, in
;; which case the delimiting "/" is also optional and the <gophertype>
;; defaults to "1".

;; The defined gopher types from RFC 1436 pg 10-11 are:
;; 0 Item is a File
;; 1 Item is a Directory
;; 2 Item is a CSO phone book server
;; 3 Error
;; 4 Item is a binHExed Macintosh file
;; 5 Item is a DOS binary archive
;; 6 Item is a UNIX uuencoded file
;; 7 Item is a index-Search server
;; 8 Item points to a text based telnet server
;; 9 Item is a binary file !
;; + Item is a redundant server
;; T Item points to a text based tn3270 session
;; g Item is a GIF format graphics file
;; I Item is some kind of image file.  Client decides how to display.
;;
;; additional types from the veronica FAQ
;;         s  -- Sound
;;         e  -- Event    (not in 2.06)
;;         M  -- MIME multipart/mixed message
;;         h  -- HTML, HyperText Markup Language

;; Characters '0' through 'Z' are reserved.


;; this should check for :binary type and ".box" in the path
;; and set the doc-type accordingly
(defmethod initialize-instance ((url gopher-url) &rest initargs)
  (call-next-method)
  (when (null (slot-value url 'port))
    (setf (slot-value url 'port) 70))
  ;; we should be able to infer the data type from the gopher path
  (let ((path (slot-value url 'path)))
    (cond ((null path)
           (setf (slot-value url 'path) "")
           (setf (slot-value url 'doc-type) :directory))
          (t
           (setf (slot-value url 'doc-type) (gopher-type (aref path 0)))
           (setf (slot-value url 'path) (subseq path 1))))))

(defun gopher-type (type-char)
  (case type-char
    (#\0 :text) (#\1 :directory) (#\2 :cso-phone-server)
    (#\3 :error) (#\4 :binhex) (#\5 :dosbin) (#\6 :uuencode)
    (#\7 :index-search) (#\8 :telnet) (#\9 :binary)
    (#\+ :redundant-server) (#\T :tn3270)
    (#\g :gif) (#\I :image)
    (otherwise :unknown)))

(defmethod copy-url ((url gopher-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'doc-type) (slot-value url 'doc-type))
    new))

(defmethod fill-box-using-url ((url gopher-url) box)
  (unwind-protect
    (let* ((doc-type (slot-value url 'doc-type))
           (stream (open-tcp-stream (slot-value url 'host) (slot-value url 'port)
                                    :element-type
                                    (if (member doc-type
                                                '(:dosbin :binary :gif :image))
                                      #-lispworks '(unsigned-byte 8)
                                      #+lispworks 'unsigned-byte

                                      #+mcl 'base-character
                                      #+lispworks 'base-char
                                      #-(or mcl lispworks) 'character))))
      (unwind-protect
        (bw::with-mouse-cursor (:file-io)
          (case doc-type
            (:text (net-write-line stream (slot-value url 'path))
                   (fill-box-from-text-stream stream box))
            (:directory (net-write-line stream (slot-value url 'path))
                        (read-gopher-directory stream box))
            (:error (append-row box (make-row '("Gopher Error")))
                    (append-row box (make-row (list (slot-value url 'host)
                                                    (slot-value url 'port))))
                    (append-row box (make-row (list (slot-value url 'path)))))
            (:index-search (cond ((find #\tab (slot-value url 'path))
                                  ;; this is a search path with a search
                                  ;; string already provided
                                  (net-write-line stream (slot-value url 'path))
                                  (read-gopher-directory stream box))
                                 (t
                                  ;; no search string specified so setup
                                  ;; a way for the user to type one in...
                                  (make-index-search-box (slot-value url 'host)
                                                         (slot-value url 'port)
                                                         (slot-value url 'path)
                                                         box))))
            ((:uuencode :binhex)
             (net-write-line stream (slot-value url 'path))
             (save-net-data stream box doc-type))
            ((:image :gif)
             (net-write-line-to-binary-stream stream (slot-value url 'path))
             (let ((path (temp-pathname doc-type)))
               (save-net-data stream box doc-type nil path)
               (start-graphics-viewer path)))
            ((:dosbin :binary)
             (net-write-line-to-binary-stream stream (slot-value url 'path))
             (save-net-data stream box doc-type))
            (otherwise (fill-unhandled-gopher-type url box))))
        (close stream)))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
        (boxer::status-line-undisplay 'surf-message))))

(defun temp-pathname (doc-type)
  (make-pathname :host "home" :name (format nil "~A~A" doc-type (gensym))))

(defun start-graphics-viewer (graphics-file)
  #+mcl
  (ccl::select-process-with-docs :JVWR graphics-file))


(defmethod fill-unhandled-gopher-type ((url gopher-url) box)
  (append-row box (make-row (list "No" "defined" "handler" "for")))
  (append-row box (make-row (list "gopher" "type" (slot-value url 'doc-type)))))

(defun read-gopher-directory (tcp-stream box)
  (do ((line (net-read-line tcp-stream) (net-read-line tcp-stream)))
      ((string= line ".") box)
    (multiple-value-bind (raw-type display-string selector-string host port)
        (decode-gopher-line line)
      (append-row box
                  (make-row
                   (list
                    (case raw-type
                      (:index-search
                       (make-index-search-box host port selector-string))
                      (otherwise
                       (make-box-from-url (format nil "gopher://~A:~D/~C~A"
                                                  host port (aref line 0)
                                                  selector-string)
                                          'boxer::data-box
                                          display-string t)))))))))

;; special types

;; for the terminal emulation types, we should should launch the
;; appropriate application
(defun make-terminal-box (host)
  (make-box `(("Entering this box will start a telnet session to:")
              (,host)
              (,(make-box `(("telnet" ,(make-box `((,host)))))
                          'boxer::doit-box "Entry-Trigger")))))

;; not quite right yet.  NCSA telnet doesn't open the connection right away
(eval-when (compile load eval)
(defboxer-primitive bu::telnet (host)
  #+mcl
  (let ((ncsa-pathname (make-pathname :host "home"
                                      :name "NCSA-telnet-temp")))
    (unwind-protect
        (progn (make-NCSA-telnet-doc ncsa-pathname (box-text-string host))
               (ccl::select-process-with-docs :NCSA ncsa-pathname))
      (delete-file ncsa-pathname)))
  boxer-eval::*novalue*)
)

(defun make-NCSA-telnet-doc (pathname host)
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (format s "commandkeys = yes~&name= \"~A\"~&host= \"~A\"~&~
               scrollback= 200~&erase = backspace~&size = {40,5,320,504}~&~
               vtwidth = 80~&tekclear = yes~&rgb0 = {0,0,0}~&~
               rgb1 = {65535,65535,65535}~&rgb2 = {0,61183,11060}~&~
               rgb3 = {61183,2079,4938}~&font = \"Monaco\"~&fsize= 9~&~
               nlines= 24~&keystop= -1~&keygo= -1~&keyip= -1~&crmap= 0~&~
               tekem= -1~&answerback= \"VT100\"~&"
            host host))
  #+mcl (ccl::set-mac-file-creator pathname :NCSA)
  #+mcl (ccl::set-mac-file-type    pathname :CONF))

;; This may change depending on how we want to handle searches
;; the box arg is provided if we come here via fill-box-using-url
(defun make-index-search-box (host port select-string &optional box)
  (let ((fun (make-box (list '(input "Search-String")
                             (list 'gopher-search (make-box `((,host))) port
                                   (make-box `((,select-string)))
                                   "Search-String"))
                       'boxer::doit-box
                       "Query-Gopher-Server")))
    (shrink fun)
    (if (null box)
        (make-box `((,fun)
                    ("Query-Gopher-Server" "Search-String:" ,(make-box '(())))))
        (progn
          (append-row box (make-row (list fun)))
          (append-row box (make-row `("Query-Gopher-Server" "Search-String:"
                                      ,(make-box '(())))))))))


(defboxer-primitive bu::gopher-search (host (boxer-eval::numberize port)
                                            select-string search-string)
  (let* ((realhost (box-text-string host))
         (real-select-string (box-text-string select-string))
         (real-search-string (box-text-string search-string))
         (stream (open-tcp-stream realhost port)))
    (unwind-protect
        (let ((return-box (boxer::make-uninitialized-box 'boxer::data-box)))
          (shared-initialize return-box t)
          (if (or (null select-string) (string= "" real-select-string))
              (net-write-line stream "~A" real-search-string)
              (net-write-line stream "~A~C~A"
                              real-select-string #\tab real-search-string))
          (read-gopher-directory stream return-box)
          return-box)
      (close stream))))

(defun decode-gopher-line (string)
  (let ((idx 1)
        (type (gopher-type (aref string 0)))
        (display nil)
        (select nil)
        (host nil))
    (setq display (subseq string idx
                          (setq idx (position #\tab string
                                              :test #'char=))))
    (incf idx) ; move the idx past the #\tab
    (setq select (subseq string idx
                         (setq idx (position #\tab string
                                             :test #'char= :start idx))))
    (incf idx)
    (setq host (subseq string idx
                         (setq idx (position #\tab string
                                             :test #'char= :start idx))))
    (incf idx)
    (when (string= "" display)
      ;; perhaps we should try and make something out of the select-string ??
      (setq display nil))
    (values type display select host (string->number (subseq string idx)))))


#|
  (do ((line (net-read-line stream) (net-read-line stream)))
      ((or (null line) (string= "." line)) box)
    (append-row box (box::make-row-from-string line)) ;; should we check for ".."
    (boxer-eval::check-for-interrupt :interrupt "Stopped by User !"))
|#

(defmethod dump-plist-internal ((self gopher-url) stream)
  (call-next-method)
  (dump-boxer-thing :doc-type stream)
  (dump-boxer-thing (slot-value self 'doc-type) stream))

(defmethod dump-plist-length ((self gopher-url))
  (+& (call-next-method) 2))
