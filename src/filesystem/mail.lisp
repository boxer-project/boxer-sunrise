;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXNET; -*-
#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



   This file contains boxer SMTP and POP3 based mail reading utilities




Modification History (most recent at top)

12/31/11 get-mime-binary-box, smtp-send-box, send-box-mime-data, get-mime-plain-text, get-mime-binhex-box
         changed calls to xxx-mac-file-ref-xxx to xxx-xref-xxx
 8/08/05 fixed package lossage in bu::mail-document
 5/15/05 added new prim mail-document which mails the current filebox
 3/09/05 send-multipart-box-data: make sure there is only 1 border between boxes
         NOTE: receiving mailers were messing this up by converting multiple adjacent
         borders into 1 border followed by blank lines
 3/01/05 simplified write-mime-boundary
 7/21/04 generalized WITH-POP-SERVER-QUEUED-DELETION to handle message
         numbers as well as boxes
 4/05/04 removed icon caching using explicit ccl::file-icon from
         get-mime-{plain-text,binary-box}, should already be done by the calls to
         boxer::set-xref-boxtop-info
10/13/03 mail-eol? for ccl::opentransport-tcp-stream
 7/15/03 fixed bug in EOF line check in get-mime-multipart which caused
         attachments to be ignored
 7/08/03 added net-mail-read-line-from-mac-file to debug mail messages saved in mac
         files which are #\LF delineated
 4/19/03 merged current LW and MCL files
 4/07/03 rfc822-date-time changed "(mod year 100)" to just "year", non 4 digit
         years seemed to confuse some mail readers...
 3/29/03 moved "Reading.." message from POP-MSG-SIZE to GET-MESSAGE because
         pop-msg-size is used by the delete-msg routine as well
         delete-message-no: changed when => cond, function was searching for
         match even after it deleted the message !
11/11/02 added EOF handling to (fill-box-using-url (mail-message-body)),
         get-mime-multipart.
 8/30/02 filter-smtp-data-string added and used by send-smtp-data-line to fix
         bug with ~'s in outgoing mail text
 5/30/02 mime-header-line-value: handle parens as plain char inside quote
         parameter value strings
 5/01/02 debugging tools
 4/11/02 added make-string-buffer and used it various places
 4/07/01 make header-line-field-name more robust for use elsewhere (like http.lisp)
         header-line-value changed to allow passing in of value-starting index
 2/21/01 fixed bugs in row-mail-values, get-mime-plain-text
 2/16/01 mime-type-dialog & edit-mime-mapping moved to mac-menu.lisp
 2/15/01 merged current LW and MCL files
                INITIAL LW port changes
 1/29/01 mail-eol?
 1/28/01 lispworks element-type,
         net-write-line => net-write-control-line in POP commands
         net-mail-read-line
 2/15/99 unique-mime-path changed to comply with LW version of DO*
                INTERIM MCL only changes
12/13/00 added titles and labels to edit-mime-mapping, mime-type-dialog
10/18/00 fixed string coercion omission for mime-type arg in add-new-mime-type
 9/11/00 mime mapping editing functions
 9/10/00 mime mapping utilities
 9/06/00 print-smtp-write-status, bound in smtp-send-box
         added interrupt polling to smtp-send-box and also rearranged
         unwind-protect forms for cleaner behavior if abort occurs in DATA
         operation
 5/25/00 get-mime-uu-binary-data
 6/21/99 make sure unique-mime-path checks for valid pathname everywhere
 6/14/99 better s handling in mime-header-line-value
         newpath-for-mime was using original name instead of the corrected
         rawpath name.  Note:max mac file length is 31 ! (not 32)
 4/26/99 send/get-mime-box now handle link property
         *max-viewable-message-length* + other support mostly get-mime-plain-text
 3/12/99 initialize-instance for pop-url will default host to be *pop-host*
 3/11/99 get-mime-binary-box fills the xref icon cache if it can
 3/10/99 newpath-for-mime changed to use ensure-valid-filename, parameterized
         on *max-file-length* variable
 3/04/99 ensure-valid-filename checks for 32 char length and trims if neccessary
 1/21/99 smtp-do-hello transmits domain info now (some SMTP servers require this)
                Mod list before LW port
12/15/98 the MAIL primitive now precesses the editor mutation queue to
         insure that editor structure is up to date
12/13/98 added stream based mail-eol? methods for end of line checking in
         net-mail-read-line
12/10/98 finished applefile(single & double) integration
12/08/98 changed box-smtp-type to return :APPLEDOUBLE for foreign box args
12/05/98 changed MIME prologue message to be more informative and less annoying
11/28/98 append-pop-message-body changed to use mime-type-values, control logic
         changed
         get-mime-binary-box takes &optional binary-encoding arg which defaults to
         :base64, otherwise we assume 8bit data and use get-mime-8bit-binary-data
         GET-MIME-APPLEFILE  done 12/02/98
         GET-MIME-BINHEX-BOX     done 11/29/98
         GET-MIME-PMULTIPART (stubbed)
         GET-MIME-APPLEDOUBLE  done 12/03/98

11/21/98 functions which send file attachments, send-mime-text-data and
         send-mime-binary-data, now add a name parameter to Content-Type
11/20/98 unique-mime-path now takes &optional supplied-name get-mime-binary-box
         and append-pop-message-body changed (possibly) supply that arg
 8/10/98 Fixed numerous bugs in get-mime-box: names, type, graphics, closets &rest rows
 8/10/98 Started Logging Changes: source = boxer version 2.3 beta


|#

(in-package :boxnet)


(defvar *smtp-relay-host* "localhost")
(defvar *smtp-port* 25)

(defvar *pop-host*)
(defvar *pop-user*)

(defun smtp-do-hello (stream &optional (host *smtp-relay-host*))
  (net-write-line stream "HELO ~A" host)
  (handle-tcp-response stream))

(defun smtp-do-reset (stream)
  (net-write-line stream "RSET")
  (handle-tcp-response stream))

(defun smtp-do-quit (stream)
  (net-write-line stream "QUIT")
  (handle-tcp-response stream))

;;; nope
(defun smtp-do-verify (stream user)
  (net-write-line stream "VRFY ~D" user)
  (handle-tcp-response stream))

(defun smtp-do-mail-from (stream user)
  (net-write-line stream "MAIL FROM:<~A>" user)
  (handle-tcp-response stream))

(defun smtp-do-recipient-to (stream user)
  (net-write-line stream "RCPT TO:<~A>" user)
  (handle-tcp-response stream))

(defun smtp-do-data (stream)
  "Sends DATA. Returns t or nil, 2nd value is smtp response."
  (net-write-line stream "DATA")
  (handle-tcp-response stream))

(defun message-id (&optional (time (get-universal-time)))
  (format nil "<~A.~A@~A>" time (gensym) (machine-instance)))

(defun smtp-send-header (stream from  ; *user-mail-address*
                                to smtp-type
                                &key subject boundary-value filename
                                (mime-version "1.0"))
   (net-write-line stream "From: ~A" from)
   ;; does RFC822 allow parens in the To: line ?  I don't think so...
   (if (listp to)
       (net-write-line stream "To: ~A ~{, ~A~}" (car to) (cdr to))
       (net-write-line stream "To: ~A" to))
   (unless (null subject) (net-write-line stream "Subject: ~A" subject))
   (let ((now (get-universal-time)))
     (net-write-line stream "Date: ~A" (boxer::rfc822-date-time now))
     (net-write-line stream "Message-ID: ~A" (message-id now)))
   (net-write-line stream "MIME-Version: ~A" mime-version)
   ;; handle specific content types here
   (case smtp-type
     (:text (net-write-line stream "Content-Type: text/plain; charset=\"us-ascii\""))
     (:multipart (net-write-line stream
                                 "Content-Type: Multipart/mixed; boundary=\"~A\""
                                 boundary-value))
     (:appledouble (net-write-line stream
                                 "Content-Type: Multipart/appledouble; boundary=\"~A\"; name=\"~A~A\""
                                 boundary-value (pathname-name filename)
                                 (let ((type (pathname-type filename)))
                                   (if (stringp type)
                                     (format nil ".~A" type)
                                     ""))))
     (otherwise (error "Don't know how to send a type ~A message" smtp-type)))
   (net-write-line stream "X-Mailer: ~A" (system-version 'boxer::boxer))
   (net-write-line stream "") ; CRLF
   )


;; need to filter for ~'s since net-write-line uses FORMAT
(defun filter-smtp-data-string (string)
  (cond ((string= string ".") "..")
        ((find #\~ string)
         (let* ((lstring (length string))
                (return-string (make-string-buffer (+ lstring 2))))
           (dotimes (i lstring)
             (let ((char (char string i)))
               (cond ((char= char #\~)
                      (vector-push-extend #\~ return-string)
                      (vector-push-extend #\~ return-string))
                     (t (vector-push-extend char return-string)))))
           return-string))
        (t string)))

(defun net-write-smtp-data-line (stream string)
  (net-write-line stream (filter-smtp-data-string string)))

(defun net-terpri (stream) (net-write-line stream ""))

;; info for use by the smtp-send-header and smtp-send-box-xxx functions
;; if there are any sub boxes, assume :multipart for now
;; the one important case is if the box is a refrence to a foreign file,
;; then we should use appledouble, rather than the generic :multipart

(defun box-smtp-type (box)
  (if (not (null (getprop box :xref)))
      :appledouble
      (let ((type :text))
        (boxer::do-box-rows ((r box))
          (boxer::do-row-chas ((c r))
            (when (box? c) (setq type :multipart) (return))))
        type)))

(defun print-smtp-write-status (stream)
  (surf-message "Sending ~D bytes" (writing-stream-position stream)))

(defun smtp-send-box (to box &optional subject
                         (host *smtp-relay-host*)
                         (from *user-mail-address*))
  (let ((smtp-stream (open-tcp-stream host *smtp-port*))
        (boxer::*FILE-STATUS-LINE-UPDATE-FUNCTION* 'print-smtp-write-status)
        (mail-sent-flag nil))
    (unwind-protect
      (let ((smtp-type (box-smtp-type box)))
        ;; get the initial connect messages
        (handle-tcp-response smtp-stream)
        ;;
        (smtp-do-hello smtp-stream)
        (smtp-do-reset smtp-stream)
        ;; now send the mail....
        ;; who from (should we put in VRFY's here ?)
        (smtp-do-mail-from smtp-stream from)
        ;; who to (should we put in VRFY's here ?)
        (if (listp to)
            (dolist (a-to to) (smtp-do-recipient-to smtp-stream a-to))
            (smtp-do-recipient-to smtp-stream to))
        ;; SMTP protocol
        ;; Last chance to poll for interrupt, after the DATA op starts
        ;; we are committed to sending something...
        (boxer-eval::check-for-interrupt :interrupt "Mail Cancelled !")
        (smtp-do-data smtp-stream)
        ;; committed, so set the flag
        (setq mail-sent-flag t)
        ;; followed by the body of the message
        (case smtp-type
          (:text
           ;; consisting of the header info
           (smtp-send-header smtp-stream from to smtp-type :subject subject)
           ;; and the box contents...
           (boxer::do-box-rows ((row box))
             (net-write-smtp-data-line smtp-stream (boxer::text-string row))))
          (:multipart
           (let ((bv (mime-separator-value)))
             (smtp-send-header smtp-stream from to smtp-type :boundary-value bv
                               :subject subject)
             (send-multipart-box-data box smtp-stream bv)))
          (:appledouble
           (let* ((bv (mime-separator-value))
                  (xref (getprop box :xref))
                  (raw-path (when xref (box::xref-pathname xref)))
                  (pathname (or raw-path (unique-mime-path))))
             (smtp-send-header smtp-stream from to smtp-type :boundary-value bv
                               :subject subject :filename pathname)
             (send-appledouble-box-data box smtp-stream bv pathname)))))
      ;; NOTE: if we are in the middle of a DATA op, we need to send "."
      (cond ((null mail-sent-flag) (smtp-do-reset smtp-stream))
            (t        ;; end the message
             (net-write-line smtp-stream ".")
             (handle-tcp-response smtp-stream)
             (smtp-do-quit smtp-stream)))
      (close smtp-stream)
      (boxer::status-line-undisplay 'surf-message)
      (boxer::boxer-editor-message "Message sent to ~A via ~A" to host))))

#|
(defun smtp-send-box-text (host from to subject text)
  (let ((stream nil) (response nil) (myText nil))
    myText
    (setf stream (smtp-open-stream host))
    (when stream
      (unwind-protect
        (progn
          ;--- logon stuff ---
          (multiple-value-setq (response myText) (smtp-do-hello stream))
          (multiple-value-setq (response myText) (smtp-do-reset stream))
          )
        (unless response (RETURN-FROM smtp-send-a-mail))

        ;--- messaging stuff
        (when (multiple-value-setq (response myText) (smtp-do-verify stream from))
          (when (multiple-value-setq (response myText) (smtp-do-verify stream to))
            (multiple-value-setq (response myText) (smtp-do-mail-from stream from))
            (multiple-value-setq (response myText) (smtp-do-recipient-to stream to))
            (multiple-value-setq (response myText)
              (smtp-send-data stream (smtp-create-mail-head from to subject) text))
            ))

        ;--- logout stuff
        (smtp-do-quit stream))
      (smtp-close-stream stream))))
|#

#|

;; testing...

(defun mime-test (box from to &optional subject (smtp-stream *standard-output*))
  (let ((bv (mime-separator-value)))
    (smtp-send-header smtp-stream from to :multipart :boundary-value bv
                      :subject subject)
    (send-multipart-box-data box smtp-stream bv)))

;; "Just call on me and I'll send it along..."
(boxnet::mime-test tb "me" "you" "with love")

|#

;;;MIME utilities

(defun make-string-buffer (initial-size)
  (make-array initial-size :element-type #+mcl 'base-character
                                         #+lispworks 'base-char
                                         #-(or mcl lispworks) 'character
                           :adjustable t :fill-pointer 0))

;; preceeded by "--" for boundaries, last boundary also has trailing  "--"
;; the == makes sure it doesn;t look like quoted printable representation
(defun mime-separator-value ()
  (format nil "==~D~C~C~C~C~C~C~C~C~C~C" (get-universal-time) (random-alpha-char)
          (random-alpha-char) (random-alpha-char) (random-alpha-char)
          (random-alpha-char) (random-alpha-char) (random-alpha-char)
          (random-alpha-char) (random-alpha-char) (random-alpha-char)))

(defun random-alpha-char ()
  (char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (random 52)))

(defun write-mime-boundary (stream boundary-value &optional last?)
  (net-write-line stream (if last? "--~A--" "--~A") boundary-value)
  ;(format t "~&boundary ~A written @  ~D" boundary-value (writing-stream-position stream))
  )

(defvar *mime-prologue-message*
  "This is a message in the MIME (Multipurpose Internet Mail Extensions) Format")

;; sub boxes are considered to be a separate type of data
(defun send-multipart-box-data (box smtp-stream bv)
  ;; first a message for the prologue
  (net-write-smtp-data-line smtp-stream *mime-prologue-message*)
  (write-mime-boundary smtp-stream bv)
  (do ((row (boxer::first-inferior-row box) (boxer::next-row row))
       (new-boundary t)
       (r1st? t nil))
      ((null row))
    (do* ((values (row-mail-values row) (cdr values))
          (v1st? t nil)
          (val (car values) (car values)))
         ((null val)) ;(format t "~&~S  @ ~D" val (writing-stream-position smtp-stream))
      (cond ((box? val)
             (unless (or (and v1st? r1st?) new-boundary)
               ;; don't write a boundary if the box is the 1st thing
               ;; or if there already is a boundary...
               (write-mime-boundary smtp-stream bv))
             ;; when a box is encountered send it as a differing type...
             (send-box-mime-data val smtp-stream)
             (unless (and (null (boxer::next-row row)) (null (cdr values)))
               ;; don't write a boundary if the box is the LAST item either
               (write-mime-boundary smtp-stream bv)
               (setq new-boundary t)))
            (t (when new-boundary (net-terpri smtp-stream))
               (net-write-smtp-data-line smtp-stream val)
               (setq new-boundary nil)))))
  ;; finally end it
  (write-mime-boundary smtp-stream bv t))

;; return a list of string(s) and/or boxes that make up the line
;; simplest case is the same as text-string of row
(defun row-mail-values (row)
  (let* ((values nil)
         (full-length (boxer::length-in-chas row))
         (current-string (make-array full-length
                                     :element-type
                                     #+lispworks 'base-char
                                     #+(or mcl symbolics) 'character
                                     #-(or mcl symbolics lispworks) 'string-char
                                     :fill-pointer 0)))
    (boxer::do-vector-contents (cha (boxer::chas-array row) :index-var-name idx)
      (cond ((and (not (zerop& (fill-pointer current-string))) (box? cha))
             ;; there is a string being formed AND we run into a box
             (setq values (nconc values (list current-string cha)))
             (setq current-string (make-array (-& full-length idx)
                                              :element-type
                                              #+lispworks 'base-char
                                              #+(or mcl symbolics) 'character
                                              #-(or mcl symbolics lispworks)
                                              'string-char
                                              :fill-pointer 0)))
            ((box? cha)
             (setq values (nconc values (list cha))))
            (t ;;it's a character
             (vector-push cha current-string))))
    ;; add the last string
    (cond ((and (zerop& (fill-pointer current-string)) (null values)) '(""))
          ((zerop& (fill-pointer current-string)) values)
          (t (nconc values (list current-string))))))


;; This just does the default thing--which is  "Content-type: application/prs.boxer"
;; eventually move to application/vnd.boxer see RFC
;; header, followed by binary box data in Base64
;; later, we will want to put some sort of dispatch here for different types of box
;; (like Content-type:image/jpeg for certain graphics boxes)
(defun send-box-mime-data (box stream)
  (let ((xref (getprop box :xref))) ; check to see if box is an external reference
    ;(format t "~&Writing box:~A  @  ~D" box (writing-stream-position stream))
    (cond ((null xref)
           (send-mime-box box stream))
          (t
           (ecase *mac-mime-file-encoding*
             ;(:binhex (send-mime-binhex-data (box::mac-file-ref-pathname xref)
             ;                               stream)
             (:appledouble
              (send-mime-appledouble-data  box (box::xref-pathname xref)
                                           stream))
             (:applesingle
              (send-mime-applesingle-data (box::xref-pathname xref)
                                          stream))))
;          (t ; dispatch...
;           (multiple-value-bind (mtype subtype)
;               (mac-file->mime-values (boxer::mac-file-ref-pathname xref))
;             ;; quickie for now...
;             (if (eq mtype :text)
;                 (send-mime-text-data
;                  mtype subtype (box::mac-file-ref-pathname xref) stream)
;                 (send-mime-binary-data
;                  mtype subtype (box::mac-file-ref-pathname xref) stream))))
          )))

;; ones we know for sure...
;; use a dialog for user to add more types.  Should use same dialog for
;; both directions.
;; this is a temporary crock (especially so in the case of the PC)
(defvar boxnet::*mac-file-mime-types*
  '((:boxr :application :boxer)
    (:jpeg :image :jpeg)
    (:|GIFf| :image :gif)
    #+lispworks
    (:box :application :boxer)
    #+lispworks
    (:jpg :image :jpeg)
    #+lispworks
    (:gif :image :gif)))

;; this should be changed to use *mime-mac-file-types*...
(defun mac-file->mime-values (pathname)
  (let* (;(creator #+mcl (ccl::mac-file-creator pathname))
         (ftype   #+mcl (ccl::mac-file-type    pathname)
                  #-mcl (intern (pathname-type pathname) (find-package "KEYWORD")))
         (mime-info (cdr (assoc ftype *mac-file-mime-types*))))
    (cond ((not (null mime-info)) (values (car mime-info) (Cadr mime-info)))
          (t (values :application :octet-stream)))))

(defvar *query-for-unknown-mime-type* t)

;; this is a list of triples: mime type string, mac creator (signature) and mac
;; file type.  Search on the 1st item to get mac file types from a mime type
;; search on the last item to get a mime type from a mac file type

(defvar *mime-mac-file-types*
  '(("application/boxer" :boxr :boxr) ("application/prs.boxer" :boxr :boxr)
    ("image/gif" :gkon :|GIFf|) ("image/jpeg" :gkon :jpeg) ("image/jpg" :gkon :jpeg)
    ("application/mac-binhex40" :bnhq :text)
    ("application/stuffit" :|SIT!| :sitd)
    ("application/pdf" :caro :|PDF |)
    ("application/msword" :mswd :wdbn)
    ("application/rft" :mswd :text)
    ))

(defun mime-values->mac-file-values (mime-type)
  (let ((existing (assoc mime-type *mime-mac-file-types* :test #'string-equal)))
    (cond ((not (null existing)) (values (cadr existing) (caddr existing)))
          ((not (null *query-for-unknown-mime-type*))
           (add-new-mime-type :mime-type mime-type))
          (t (values :???? :????)))))

#| ;; old version

(defun mime-values->mac-file-values (mime-type)
  (cond ((string-equal mime-type "image/jpeg") (values :gkon :jpeg))
        ((string-equal mime-type "image/gif")  (values :gkon :|GIFf|))
        ((string-equal mime-type "application/mac-binhex40") (values :bnhq :text))
        (t (values :???? :????))))
|#


;; Mime to mac mapping utilities
;; SAVE, READ and EDIT

(defun save-mime-mapping (&optional (pathname
                                     #+mcl (merge-pathnames
                                            "Mime-Map"
                                            (boxer::Default-mac-pref-file-name))
                                     #-mcl "~/.mime-map"))
  (with-open-file (s pathname :direction :output :if-exists :supersede)
    (dolist (entry *mime-mac-file-types*)
      (format s "~A ~A ~A~&" (car entry) (cadr entry) (caddr entry)))))

(defun read-mime-mapping (&optional (pathname
                                     #+mcl (merge-pathnames
                                            "Mime-Map"
                                            (boxer::Default-mac-pref-file-name))
                                     #-mcl "~/.mime-map"))
  (let ((entries nil) (eof-value (list 'eof)) (finished? nil))
    (when (probe-file pathname)
      (ignore-errors
       (with-open-file (s pathname :direction :input)
         (loop
           (let ((line (read-line s nil eof-value)))
             (cond ((or (eq line eof-value) (string= line ""))
                    (setq finished? t)
                    (return))
                   (t ;; should be 3 things on the line separated by spaces
                    ;; remember that the last 2 items must be 4 chars long
                    ;; even if some of those chars are spaces
                    (let ((1stspace (position #\space line)))
                      (push
                       (list (subseq line 0 1stspace)
                             (intern (subseq line (+ 1stspace 1) (+ 1stspace 5))
                                     (find-package "KEYWORD"))
                             (intern (subseq line (+ 1stspace 6) (+ 1stspace 10))
                                     (find-package "KEYWORD")))
                       entries))))))))
      (unless (null finished?)
        (setq *mime-mac-file-types* entries)))))


(defun edit-mime-entry (entry)
  (let ((new-values (boxer::mime-type-dialog (car entry) (string (cadr entry))
                                             (string (caddr entry)))))
    (cond ((null new-values))
          (t (setf (car entry)   (car new-values)
                   (cadr entry)  (cadr new-values)
                   (caddr entry) (caddr new-values))))))

(defun add-new-mime-type (&key mime-type app-sig file-type)
  (let ((new-values (boxer::mime-type-dialog (string mime-type)
                                             (when app-sig (string app-sig))
                                             (when file-type (string file-type)))))
    (cond ((null new-values))
          (t (push new-values *mime-mac-file-types*)))))

;; NOTE:mime-type-dialog & edit-mime-mapping are defined in the xxx-menu.lisp files



;; check to see if the box is a link and, if so, include the link tag
;; in the header.  If the file box has not been filled send the data
;; from the file
(defun send-mime-box (box stream)
  (let ((file (getprop box :associated-file)))
    (if (null file)
      (net-write-smtp-data-line stream "Content-Type: application/prs.boxer")
      (net-write-smtp-data-line stream
                                "Content-Type: application/prs.boxer ; link=\"T\""))
    (net-write-smtp-data-line stream "Content-Transfer-Encoding: base64")
    ;(net-write-smtp-data-line stream "Content-Description: A Boxer Box ~A" (box::name box))
    (net-terpri stream)
    (if (and (null (slot-value box 'boxer::first-inferior-row))
             (storage-chunk? box))
      ;; file box is still in file, not in editor
      (send-mime-binary-data-internal file stream)
      (with-base64-stream (binstream stream :direction :output)
        (boxer::dump-top-level-box-to-stream box binstream)))))

;; this is the hook for using quoted-printable representation
;; just writes out the lines for now....
(defun send-mime-text-data (mtype subtype file mailstream)
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Type: ~A/~A ; name=\"~A~A\""
                                    (string-downcase mtype)
                                    (string-downcase subtype)
                                    (pathname-name file)
                                    (let ((type (pathname-type file)))
                                      (if (stringp type)
                                        (format nil ".~A" type)
                                        ""))))
  ;(net-write-smtp-data-line mailstream "Content-Transfer-Encoding: quoted-printable")
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Description: ~A" (namestring file)))
  (net-terpri mailstream)
  (with-open-file (s file)
    (loop (let ((line (read-line s nil nil)))
            (if (null line) (return)
                (net-write-smtp-data-line mailstream line))))))

(defun send-mime-binary-data (mtype subtype file mailstream)
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Type: ~A/~A ; name=\"~A~A\""
                                    (string-downcase mtype)
                                    (string-downcase subtype)
                                    (pathname-name file)
                                    (let ((type (pathname-type file)))
                                      (if (stringp type)
                                        (format nil ".~A" type)
                                        ""))))
  (net-write-smtp-data-line mailstream "Content-Transfer-Encoding: base64")
  (net-write-smtp-data-line mailstream
                            (format nil "Content-Description: ~A" (namestring file)))
  (net-terpri mailstream)
  (send-mime-binary-data-internal file mailstream))

;; send the contents of the <file> as base64 encoded binary data
;; assumes the appropriate type header has already been written out
(defun send-mime-binary-data-internal (file mailstream)
  (with-open-file (filestream file :direction :input :element-type '(unsigned-byte 8))
    (with-base64-stream (binstream mailstream :direction :output)
      (loop (let ((byte (read-byte filestream nil nil)))
              (if (null byte) (return) (write-byte byte binstream)))))))

(defboxer-primitive bu::mail ((boxer-eval::dont-copy address) (bu::port-to message))
  ;; make sure we have up to date editor structure
  (boxer::process-editor-mutation-queue-within-eval)
  (let ((to (with-collection
              (dolist (er (boxer::get-box-rows address))
                (collect (boxer::evrow-text-string er address))))))
    (when (null (cdr to)) (setq to (car to)))
    ;; should do some reality checking on the "to" arg
    (let ((subject (boxer::lookup-variable-in-box-only (box-or-port-target message)
                                                       'bu::subject nil)))
      (if (null subject)
          (smtp-send-box to (box-or-port-target message))
          (smtp-send-box to (box-or-port-target message) (box-text-string subject))))
    boxer-eval::*novalue*))


(defboxer-primitive bu::mail-document ((boxer-eval::dont-copy address))
    ;; make sure the editor structure is up to date
  (boxer::process-editor-mutation-queue-within-eval)
  (if (not (box? boxer-eval::*lexical-variables-root*))
      (boxer-eval::primitive-signal-error :file "You can only MAIL editor boxes")
      (let* ((mailbox (boxer::current-file-box))
             (to (with-collection
                   (dolist (er (boxer::get-box-rows address))
                     (collect (boxer::evrow-text-string er address)))))
             (subject (boxer::lookup-variable-in-box-only mailbox 'bu::subject nil)))
        (smtp-send-box to mailbox (unless (null subject) (box-text-string subject)))
        boxer-eval::*novalue*)))



;;;; Reading Mail using the POP3 protocol

(defvar *pop-port-number* 110) ; sometimes 109 (RFC1725 claims it should be 110)

(defvar *pop-stream* nil)

(defvar *cache-pop-password* nil)

(defvar *dele-msg-on-retr* nil
  "Should the POP client delete messages fromthe server as it retrieves them ?")

(defvar *defer-long-messages?* nil)

(defvar *max-immediate-message-length* 10000
  "POP messages longer than this will not be initially read in")

(defvar *max-viewable-message-length* 200000
  "POP messages linger than this will be saved to a file")

(defvar *delete-loaded-messages?* nil)

(defvar *report-pop-message-reading-progress?* nil)

;;  Pop classes are an extension of URL's to support a black box mechanism
;; for use with the POP reader

;; for messages which are longer than *max-immediate-message-length*, the
;; message body is a black box with a URL of mail-message-body
;; Note: this will only work if the POP3 server supports the TOP command
;; see get-pop-status

(defclass mail-message
  (url)
  ((message-number :initarg :message-number :initform 1)
   ;; should be filled either with UIDL or else, SIZE if UIDL is unsupported
   (uid :initarg :uid :initform nil))
  ;; (:metaclass block-compile-class)
  )

(defclass mail-message-body
  (mail-message)
  ()
  ;; (:metaclass block-compile-class)
  )

;; these are contained in a box with a URL of:

;; this is the URL for a mailbox.  It contains all the neccessary info to
;; open a pop connection (password can be either cached or queried, the default
;; should be query to avoid the danger of saving clear text passwords in boxer
;; files.  The dump method should take care to NEVER save out a password.
;; The pop file stream is cached here instead of a global var to allow people
;; to have multiple mailboxes
(defclass pop-url
  (url)
  ((user :initform nil :accessor pop-user)
   (password :initform nil)
   (host :initform nil :accessor pop-host)
   (port :initform *pop-port-number* :accessor pop-port)
   (stream :initform nil :accessor pop-stream)
   ;; a slot to distinguish a particular mail session, minimally, a # of messages,
   ;; size in bytes pair
   (session-id :initform nil :accessor pop-session-id))
  ;; (:metaclass block-compile-class)
  )

;; substantially the same as net-read-line except
;; we reuse the same vector to reduce CONSing because we know
;; that the line will immediately be passed to make-net-row
;; also "." hacking is handled transparently here, "." only lines return NIL
;; and lines which begin with "." but are longer than 1 char will have
;; the initial "." stripped off as per page 3 of RFC1725
(defvar *mail-line-buffer* (make-string-buffer 80))

(defvar *net-mail-read-line-hook* nil)

(defun net-mail-read-line (stream &optional (wait? t) append?)
  (if (null *net-mail-read-line-hook*)
      (net-mail-read-line-basic stream wait? append?)
      (funcall *net-mail-read-line-hook* stream wait? append?)))

(defvar *debug-mail-line-buffer* nil)

;; this has to return strings with fill-pointers for make-net-row
;; this is gross, but we'll deliberately use read-line for now because it
;; should be smart about newlines in a platform specific way...
(defun net-mail-read-line-from-file (stream wait? append?)
  (declare (ignore wait?))
  (unless append? (setf (fill-pointer *mail-line-buffer*) 0))
  (let ((rawline (read-line stream nil nil)))
    (cond ((null rawline) (values *mail-line-buffer* 0))
          (t (dotimes (i (length rawline))
               (vector-push-extend (char rawline i) *mail-line-buffer*))
             (values *mail-line-buffer* (length rawline))))))

;; need this to read "text" files onthe mac which uses #\LF only, as a line
;; separator
#+mcl
(defun net-mail-read-line-from-mac-file (stream wait? append?)
  (declare (ignore wait?))
  (unless append? (setf (fill-pointer *mail-line-buffer*) 0))
  (unless (ccl:stream-eofp stream)
    (let ((char nil))
      (do ((i 0 (1+& i)))
          ((or (null (setq char (ccl:stream-tyi stream)))
               (char= char #\LF))
           (values *mail-line-buffer* i))
        (vector-push-extend char *mail-line-buffer*)))))

;; need to hack and EOF !!!
;; callers should watch for returned NIL
(defun net-mail-read-line-basic (stream wait? append?)
  (when (or wait? (listen stream))
    ;; this is an open coded version of ccl::telnet-read-line counting added
    (unless append? (setf (fill-pointer *mail-line-buffer*) 0))
    (let ((period? nil))
      #+mcl
      (unless (ccl:stream-eofp stream)
        (let ((char nil))
          (do ((i 0 (1+& i)))
              ((or (null (setq char (ccl:stream-tyi stream)))
                   (mail-eol? stream char))
               (unless period? (values *mail-line-buffer* i)))
            (cond ((and (char= char #\.) (=& i 0)) (setq period? t))
                  (t
                   (unless (null period?) (setq period? nil))
                   (vector-push-extend char *mail-line-buffer*))))))
      #-mcl
      (let ((eof-value (list 'eof)))
        (unless (eq (peek-char nil stream nil eof-value) eof-value)
          (let ((char nil))
            (do ((i 0 (1+& i)))
                ((or (null (setq char (read-char stream nil nil)))
                     (mail-eol? stream char))
                 (unless period? (values *mail-line-buffer* i)))
              (cond ((and (char= char #\.) (=& i 0)) (setq period? t))
                    (t
                     (unless (null period?) (setq period? nil))
                     (vector-push-extend char *mail-line-buffer*)))))))
    )))

;; stream based checks for end of line,
#-mcl
(defmethod mail-eol? ((stream stream) char)
  (when (and (eq char #\CR) (eq (peek-char nil stream nil nil) #\LF))
    ;; get the LF out
    (read-char stream nil nil)
    T))

#+mcl
(defmethod mail-eol? ((stream ccl::tcp-stream) char)
  (when (and (eq char #\CR) (eq (ccl::stream-peek stream) #\LF))
    ;; get the LF out
    (ccl::stream-tyi stream)
    T))

#+carbon-compat
(defmethod mail-eol? ((stream ccl::opentransport-tcp-stream) char)
  (when (and (eq char #\CR) (eq (ccl::stream-peek stream) #\LF))
    ;; get the LF out
    (ccl::stream-tyi stream)
    T))

;; a fast specific version of make-row, that works only on adjustable
;; arrays of chars like that returned from net-mail-read-line
(defun make-net-row (char-array)
  (let ((chas-array (boxer::make-chas-array (fill-pointer char-array))))
    (boxer::with-fast-chas-array-manipulation (chas-array chas)
      (dotimes& (i (fill-pointer char-array))
        (setf (aref chas i) (aref char-array i)))
      (setf (boxer::chas-array-active-length chas-array)
            (fill-pointer char-array)))
    (boxer::make-initialized-row :chas-array chas-array)))


;;; these 5 methods are meant to be inherited by mail-message-body

(defmethod initialize-instance ((url mail-message) &rest initargs)
  (call-next-method)
  (let ((n (getf initargs :message-number))
        (id (getf initargs :uid)))
    (unless (null n) (setf (slot-value url 'message-number) n))
    (unless (null id) (setf (slot-value url 'uid) id))))

(defmethod copy-url ((url mail-message))
  (let ((new (call-next-method)))
    (setf (slot-value new 'message-number) (slot-value url 'message-number))
    new))

(defmethod dump-plist-length ((url mail-message)) (+& (call-next-method) 2))

(defmethod dump-plist-internal ((url mail-message) stream)
  (call-next-method)
  (dump-boxer-thing :message-number stream)
  (dump-boxer-thing (slot-value url 'message-number) stream))

;; scoping upward for the appropriate mail box will be the same
(defmethod get-message-mail-box ((box boxer::box))
  (let ((url (getprop box :url)))
    (cond ((typep url 'pop-url) (values box url))
          (t (let ((sup (superior-box box)))
               (when (box? sup) (get-message-mail-box sup)))))))

;;; The fill-box-using-url method is different
(defmethod fill-box-using-url ((url mail-message) box)
  (multiple-value-bind (mail-box pop-url)
      (get-message-mail-box box)
    (cond ((null mail-box)
           (boxer-eval::primitive-signal-error :net-error "Message "
                                         (slot-value url 'message-number)
                                         " not in a mail box"))
          (t (insure-pop-stream pop-url)
             (get-message-internal (pop-stream pop-url)
                                   (slot-value url 'message-number) box)))))

(defmethod fill-box-using-url ((url mail-message-body) box)
  (multiple-value-bind (mail-box pop-url)
      (get-message-mail-box box)
    (cond ((null mail-box)
           (boxer-eval::primitive-signal-error :net-error "Message "
                                         (slot-value url 'message-number)
                                         " not in a mail box"))
          (t (insure-pop-stream pop-url)
             (let ((ps (pop-stream pop-url)))
               ;; issue the RETR command
               (net-write-line ps "RETR ~D" (slot-value url 'message-number))
               (handle-pop-response ps)
               ;; skip past the header...
               (do ((line (net-mail-read-line ps) (net-mail-read-line ps)))
                   ((or (null line) (string= line ""))))
               ;; now fill the body from stream
               (do ((line (net-mail-read-line ps) (net-mail-read-line ps)))
                   ((null line))
                 (append-row box (make-net-row line))))))))

(defmethod initialize-instance ((url pop-url) &rest initargs)
  initargs
  (call-next-method)
  (multiple-value-bind (user password host port path canonicalized-scheme-string)
      (decode-net-url (slot-value url 'scheme-string))
    (declare (ignore path))
    (setf (slot-value url 'user) user
          (slot-value url 'password) password
          (slot-value url 'host) (if (and (or (null host) (string= host ""))
                                          (boundp *pop-host*))
                                     *pop-host*
                                     host)
          (slot-value url 'port) (or port *pop-port-number*))
    (unless (null canonicalized-scheme-string)
      (setf (slot-value url 'scheme-string) canonicalized-scheme-string))))

(defmethod copy-url ((url pop-url))
  (let ((new (call-next-method)))
    (setf (slot-value new 'user) (slot-value url 'user)
          ;(slot-value new 'password) (slot-value url 'password)
          (slot-value new 'host) (slot-value url 'host)
          (slot-value new 'port) (slot-value url 'port))
    new))


;; similiar to handle-tcp-response except the POP3 responses start with either
;; "+OK" or "-ERR" instead of the 3 digit TCP codes
(defun handle-pop-response (stream)
  (let* ((response (net-read-line stream t))
         (indicator (char response 0)))
    (unless (null response) (debugging-message response))
    (when (char= #\- indicator)
      (signal-tcp-error (subseq response 0 3) response))
    response))

;; Quicky version of signal-tcp-error, no checking for bound error handlers
(defun signal-pop-error (string)
  (boxer-eval::primitive-signal-error :pop-error (copy-seq string)))

;; messages to be deleted from the server
(defvar *pop-server-deletion-queue* :toplevel)

;; queued objects can be either boxes or message numbers....
(defmacro with-pop-server-queued-deletion (stream &body body)
  `(let ((*pop-server-deletion-queue* nil))
     (prog1 (progn . ,body)
       (dolist (thing *pop-server-deletion-queue*)
         (cond ((numberp thing) (pop-dele-msg ,stream thing))
               ((box? thing) (delete-msg-from-pop-server ,stream thing))
               (t (error "Unexpected object in mail deletion queue: ~A" thing)))))))

(defun queue-for-pop-server-deletion (thing)
  (unless (eq *pop-server-deletion-queue* ':toplevel)
    (push thing *pop-server-deletion-queue*)))

;; this is supposed to return a stream ready for the POP TRANSACTION state
(defmethod open-pop-stream ((self pop-url))
  (let ((stream (open-tcp-stream (slot-value self 'host)
                                 (slot-value self 'port))))
    ;; look for the greeting
    (handle-pop-response stream)
    ;; login
    (net-write-line stream "USER ~A" (slot-value self 'user))
    (handle-pop-response stream)
    ;; password (add multiple attempts using *correct-password-retries* later)
    (net-write-line stream "PASS ~A" (or (slot-value self 'password)
                                         (boxer::get-string-from-status-line
                                          "Enter password:" nil)))
    (handle-pop-response stream)
    ;; if we've got this far, it is safe to set the stream
    (setf (slot-value self 'stream) stream)
    ;; check to see if TOP is supported here ?
    stream))

;; Make sure the stream is still active since POP allows servers to close
;; idle connections after a certain timeout
(defun valid-pop-stream? (stream)
  (and (not (null stream))
       (open-stream-p stream)
       #+mcl (ccl::stream-eofp stream)
       ))

;; stubware for now, we need to store user/host in some pop object so
;; we have the info to relogin if there has been an idle timeout disconnect
(defmethod insure-pop-stream ((url pop-url))
  (unless (valid-pop-stream? (slot-value url 'stream))
    ;; do we need to make sure the old value is handled ?
    (setf (slot-value url 'stream) (open-pop-stream url))))

(defmethod fill-box-using-url ((url pop-url) box)
  (insure-pop-stream url)
  (unwind-protect
      (multiple-value-bind (nmsgs nbytes)
          (get-pop-status (slot-value url 'stream))
        (debugging-message "POP STAT ~A ~A" nmsgs nbytes)
        (surf-message "Mail Box: ~D messages, ~D bytes" nmsgs nbytes)
        (setf (slot-value url 'session-id) (cons nmsgs nbytes))
        (with-pop-server-queued-deletion (slot-value url 'stream)
          (dotimes (i nmsgs)
            (append-row box (make-row
                             (list (get-message (slot-value url 'stream)
                                                (1+ i))))))))
    (close-pop-stream (slot-value url 'stream))
    ;; make sure any messages are erased
    #-mcl
    (boxer::status-line-undisplay 'surf-message)
    #+mcl
    (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
        (boxer::drawing-on-window (boxer::*boxer-pane*)
          (boxer::status-line-undisplay 'surf-message))
        (boxer::status-line-undisplay 'surf-message))))

;; same as above except it returns a VC instead of modifying an editor-box
(defmethod make-vc-using-url ((url pop-url))
  (insure-pop-stream url)
  (let ((vc-rows nil))
    (unwind-protect
        (multiple-value-bind (nmsgs nbytes)
            (get-pop-status (slot-value url 'stream))
          (debugging-message "POP STAT ~A ~A" nmsgs nbytes)
          (surf-message "Mail Box: ~D messages, ~D bytes" nmsgs nbytes)
          (setf (slot-value url 'session-id) (cons nmsgs nbytes))
          (with-pop-server-queued-deletion (slot-value url 'stream)
             (dotimes (i nmsgs)
               (push (boxer::make-evrow-from-entry
                      (get-message (slot-value url 'stream) (1+ i)))
                     vc-rows))))
      (close-pop-stream (slot-value url 'stream))
      ;; make sure any messages are erased
      #-mcl
      (boxer::status-line-undisplay 'surf-message)
      #+mcl
      (if (null boxer::%drawing-window) ;; not already inside of drawing-on-window
          (boxer::drawing-on-window (boxer::*boxer-pane*)
            (boxer::status-line-undisplay 'surf-message))
          (boxer::status-line-undisplay 'surf-message)))
    (make-vc (nreverse vc-rows))))

;; protocol level functions, these talk directory to the pop server stream

;; returns 2 values number of messages and total number of byte in mailbox,
;; uses the POP "STAT" command
(defun get-pop-status (pop-stream)
  (net-write-control-line pop-stream "STAT")
  (let* ((response (net-read-line pop-stream))
         (indicator (char response 0)))
    (cond ((char= indicator #\-) (signal-pop-error response))
          (t (let* ((1space (position #\space response :test #'char=))
                    (2space (position #\space response
                                      :test #'char= :start (1+ 1space)))
                    (3space (position #\space response
                                      :test #'char= :start (1+ 2space))))
               (values (string->number response :start (1+ 1space) :stop 2space)
                       (string->number response
                                       :start (1+ 2space) :stop 3space)))))))


(defun pop-msg-size (stream message-number)
  (net-write-control-line stream "LIST ~D" message-number)
  (let* ((response (net-read-line stream))
         (indicator (char response 0)))
    (cond ((char= indicator #\-) (signal-pop-error response))
          (t (let* ((1space (position #\space response :test #'char=))
                    (2space (position #\space response
                                      :test #'char= :start (1+ 1space)))
                    (3space (position #\space response
                                      :test #'char= :start (1+ 2space))))
               (string->number response :start (1+ 2space) :stop 3space))))))

(defun pop-dele-msg (stream message-number)
  (net-write-control-line stream "DELE ~D" message-number)
  (surf-message "Marking Message ~D as Deleted" message-number)
  (let ((response (net-read-line stream)))
    (when (char= (char response 0) #\-) (signal-pop-error response))))

(defun close-pop-stream (pop-stream &optional reset?)
  (when reset?
    (net-write-control-line pop-stream "RSET") (handle-pop-response pop-stream))
  (net-write-control-line pop-stream "QUIT")
  (handle-pop-response pop-stream)
  (debugging-message "Closing pop stream")
  (close pop-stream))

(defun pop-top-unsupported () (throw 'no-top nil))

;; For now, message boxes consist of a header box and a message box.  Multipart
;; MIME mail may have several boxes following the header box.  Overly large boxes
;; will use the TOP command to only read in the header.  The message box will
;; then consist of an open header box and a closed (black box) message box
;; with a MAIL-MESSAGE-BODY url.  Note that this may conflict with the a
;; multipart MIME header.  If the top command is unsupported (since it is
;; listed as optional in RFC1725), then the entire message box is left black
;; with a MAIL-MESSAGE url

(defun delete-msg-from-pop-server (stream box)
  (let ((url (getprop box :url)))
    (unless (or (null url)
                (not (typep url 'mail-message)))
      (delete-message-no url stream))))


;; assumes stream is valid and open
(defmethod delete-message-no ((url mail-message) pop-stream)
  (let* ((uid (slot-value url 'uid))
         (message-number (slot-value url 'message-number))
         (server-msg-id (pop-msg-size pop-stream message-number)))
    (cond ((= uid server-msg-id)
           ;; check to see if ID's match before sending the DELE
           (pop-dele-msg pop-stream message-number))
          (t
           ;; if the ID's don't match, loop to the beginning looking for an ID
           ;; match because the message may have drifted toward the beginning of
           ;; the mailbox because of intervening access of the mailbox with
           ;; selecting message deletes because of the nature of mail(new messages
           ;; appending), the message can only move toward the beginning of
           ;; the mailbox
           (do ((msg (1-& message-number) (1-& msg)))
               ((zerop& msg))
             (let ((msize (pop-msg-size pop-stream msg)))
               (when (and (= msize uid)
                          (boxer::status-line-y-or-n-p
                           (format
                            nil "Message ~D matches server msg ~D. Delete ~D from server ?"
                            message-number msg msg)))
                 (pop-dele-msg pop-stream msg))))))))

(defun get-message (stream message-number)
  (let* ((msize (pop-msg-size stream message-number))
         (url (make-instance 'mail-message
                :message-number message-number :uid msize))
         (*report-pop-message-reading-progress?* nil))
    ;; don't bother since many server don't support UIDL yet
    ;(net-write-line stream "UIDL ~D" message-number)
    ;(setf (slot-value url 'uid) (net-read-line stream))
    ;; if the message size is too big, don't read it in right away
    ;; WARNING: deferred messages are not fully supported as of 6/22/96
    ;; Don't change *defer-long-messages?* to T until you figure out
    ;; how to access a deferred message which has been moved out of the
    ;; mail box into a random place in the boxer world an unspecified
    ;; (possibly long) time ago
    (when (> msize *max-immediate-message-length*)
      (setq *report-pop-message-reading-progress?* t))
    (surf-message "Reading Message ~D" message-number)
    (cond ((and *defer-long-messages?*
                (> msize *max-immediate-message-length*))
           ;; just try and get the header
           (let ((header-ok? nil))
             (net-write-control-line stream "TOP ~D 0" message-number)
             (catch 'no-top
               (tcp-handler-bind (("-" pop-top-unsupported))
                 (handle-pop-response stream))
               (setq header-ok? t))
             ;; if we got the header, then make a
             (if header-ok?
               (let ((header-box (make-header-box-from-stream
                                  stream t)))
                 (make-delayed-pop-message message-number
                                           header-box))
               (make-delayed-pop-message message-number))))
          (t ;; make an entire message box
           (let ((msg-box (get-message-1 stream message-number
                                         (> msize
                                            *max-viewable-message-length*))))
             ;; if we're here, we know we've got a box without blowing out
             (putprop msg-box url :url)
             (when *delete-loaded-messages?*
               (queue-for-pop-server-deletion msg-box))
             (setf (boxer::all-new-box? msg-box) t)
             msg-box)))))

;; obsolete, here to (incompletely) support (fill-box-from-url mail-message)
(defun get-message-internal (stream message-number &optional box)
  (net-write-control-line stream "RETR ~D" message-number)
  (handle-pop-response stream)
  (multiple-value-bind (header-box mime-type mime-values header-size)
      (make-header-box-from-stream stream)
    (declare (ignore mime-type mime-values header-size))
    ;; we grab the header first because it might tell us
    ;; how we should process the rest of the message
    ;; in particular, MIME information will appear in the
    ;; header
    (make-pop-message header-box stream box)))

;; this is the core message construction function
;; It works by first, pulling out the header and returning a box with
;; header info in it.  We do it in 2 parts because the header can determine
;; how to process the body of the message.  In particular, certain MIME
;; types will require radically different types of processing.
;; the MAKE-HEADER-BOX-FROM-STREAM function returns a box, typically with
;; textual header info already at the top.  It may also contain some
;; header info as boxer structure in the closet to facilitate writing
;; mail filters in boxer.  If there is a MIME type, it is returned as
;; the 2nd value.  Additional MIME info is returned as a keyword list in
;; the 3rd value.  The most important possibilities being
;; Content-Transfer-Encoding (:base64 or :qp) and multipart boundary values
;; for multipart MIME types.  Note that make-header-box-from-stream MAY be
;; called recursively in the case of multipart messages.
;; the 4th value is # of bytes read for the benefit of progress reporting

(defun get-message-1 (stream message-number &optional save-to-file?)
  (net-write-control-line stream "RETR ~D" message-number)
  (handle-pop-response stream)
  (multiple-value-bind (header-box mime-type mime-values header-size)
      (make-header-box-from-stream stream)
    ;; we grab the header first because it might tell us
    ;; how we should process the rest of the message
    ;; in particular, MIME information will appear in the
    ;; header
    (prog1
      (append-pop-message-body header-box stream mime-type mime-values
                               header-size save-to-file?)
      ;; some of the append sub functions (especially the binary ones) can leave
      ;; unprocessed lines in the stream since they exit as soon as the binary
      ;; data ends
      (when (listen stream)
        (loop (let ((xtraline (net-mail-read-line stream nil)))
                (when (null xtraline) (return))))))))

;;; Header Line Processing
;; To DO - still need to parse character and string quoting, maybe make a general
;; header line parser at some point

;; This hacks Line-Folding
(defun get-header-line (stream)
  (multiple-value-bind (line llength)
      (net-mail-read-line stream)
    (loop (if (and (not (string= line "")) (LWSP-char? (peek-char nil stream)))
              (multiple-value-setq (line llength)
                (net-mail-read-line stream t t)) ; this will append to line
              (return (values line llength))))))

;; header fields which get their own box in the closet, should eventually
;; be user selectable and the boxes should be lightweight variables
(defvar *box-worthy-pop-msg-header-fields*
  '("Sender" "Reply-to" "From" "To" "Subject" "Date" "cc"))

(defun header-field-has-machine-part? (field-name)
  (member field-name '("From" "To" "Reply-to" "Sender") :test #'string=))

;; extracts the part of the line between "<" and ">"
;; expects the line to be unfolded, that is, after the "<" there better be a ">"
(defun header-line-machine-field (line &optional
                                       (return-field (make-string-buffer 50)))
  (let ((start (do* ((i 0 (1+ i))
                     (stop (1-(length line)))
                     (char (aref line i) (aref line i)))
                    ((or (= i stop) (char= char #\<)) (unless (= i stop) (1+ i))))))
    (if (null start)
        (header-line-value line return-field)
        (do* ((j start (1+ j))
              (char (aref line j) (aref line j)))
             ((char= char #\>) return-field)
          (vector-push-extend char return-field)))))

(defun LWSP-char? (char) (member char '(#\space #\tab)))

;; this also removes leading whitespace and ignores comments between ()'s
(defun header-line-value (line &optional
                               (return-field (make-string-buffer 50))
                               (value-start (do* ((i 0 (1+ i))
                                                  (char (aref line i)
                                                        (aref line i)))
                                                 ((char= char #\:) (+ i 1)))))
  (do* ((j value-start (1+ j))
        (stop (length line))
        (start? t)
        (in-comment? nil))
       ((>= j stop) return-field)
    (let ((char (aref line j)))
      (cond ((and start? (LWSP-char? char))
             ;; ignore leading whitespace, TAB?
             )
            (in-comment?
             ;; ignore material inside parens
             (when (char= char #\)) (setq in-comment? nil)))
            ((char= char #\() (setq in-comment? t))
            (t
             (when start? (setq start? nil))
             (vector-push-extend char return-field))))))

;; MIME fields can have additional parameters, delineated by ";" and
;; separated by "="
;; any parameter values will be returned as an additional value in a keyword list
;; NOTE: careful, parameter values sometimes are "..." quoted
(defun mime-header-line-value (line &optional
                               (return-field (make-string-buffer 50)))
  (do* ((j (do* ((i 0 (1+ i)) (char (aref line i) (aref line i)))
                ((char= char #\:) (+ i 1)))
           (1+ j))
        (stop (length line))
        (start? t)
        (in-parameter? nil)
        (parameter-string nil)
        (parameters nil)
        (in-quote? nil)
        (in-comment? nil))
       ((>= j stop)
        (when (and (eq in-parameter? :parvalue) (not (null parameter-string)))
          (setq parameters
                (append parameters (list (copy-seq parameter-string)))))
        (values (intern (string-upcase return-field) (find-package "KEYWORD"))
                parameters))
    (let ((char (aref line j)))
      (cond ((LWSP-char? char)
             ;; all whitespace, TAB?
             (when (and (eq in-parameter? :parvalue) in-quote?)
               ;; 6/14/99 only add spaces in the middle of string quotes
               (vector-push-extend char parameter-string)))
            (in-comment?
             ;; ignore material inside parens
             (when (char= char #\)) (setq in-comment? nil)))
            ((char= char #\()
             (if (and (eq in-parameter? :parvalue) in-quote?)
                 (vector-push-extend #\( parameter-string)
               (setq in-comment? t)))
            ((char= char #\;)
             (cond ((null parameter-string)
                    (setq parameter-string (make-string-buffer 30)))
                   (t ; must be a 2nd+ parameter
                    (setq parameters
                          (append parameters
                                  (list (copy-seq parameter-string))))
                    (setf (fill-pointer parameter-string) 0)))
             (setq in-parameter? :parname))
            ((and (eq in-parameter? :parname) (char= char #\=))
             (setq parameters
                   (append parameters (list (intern (string-upcase
                                                     parameter-string)
                                                    (find-package "KEYWORD")))))
             (setf (fill-pointer parameter-string) 0)
             (setq in-parameter? :parvalue))
            ((char= char #\")
             (cond ((not in-quote?) (setq in-quote? T))
                   (t               (setq in-quote? nil))))
            (t
             (when start? (setq start? nil))
             (vector-push-extend char (if (null in-parameter?)
                                          return-field
                                          parameter-string)))))))

;;; make this more robust since it is being used in other contexts
;;; like http.lisp
(defun header-line-field-name (line &optional
                                    (return-name (make-string-buffer 16)))
  (do ((maxlength (length line))
       (i 0 (1+& i)))
      ((>=& i maxlength) return-name)
    (let ((char (aref line i)))
      (cond ((char= char #\:) (return (values return-name i)))
            ; ((LWSP-char? char)) ;; to handle malformed header = token : blah
            (t (vector-push-extend char return-name))))))

;; look for either an empty line (according to RFC822, an empty line separates
;; header from body) or perhaps a "." which can occur in response to
;; a  "TOP <n> 0" command
;;
;; NOTE: flush TOP support if we decide to flush deferred message loading
;; should return a box, mime-type, mime-values (may be NIL) and bytes-read
(defun make-header-box-from-stream (stream &optional (read-to-terminator? nil))
  (let* ((header (boxer::make-uninitialized-box 'boxer::data-box))
         (visible-header-rows nil)
         (bytes-read 0)
         (closet-boxes (list header))
         ;; pre make these to reduce CONSing
         (field-name (make-string-buffer 16))
         (value-field (make-string-buffer 50))
         (mime-type :text/plain)
         (mime-values nil))
    (shared-initialize header t)
    (boxer::set-name header (boxer::make-name-row '("Header")))
    (loop (multiple-value-bind (line llength)
              (get-header-line stream) ; counts bytes and unfolds lines
            (cond ((or (null line)
                       (and (not read-to-terminator?) (string= line "")))
                   ;; add one more empty row to the header
                   (nconc visible-header-rows (list (boxer::make-row '())))
                   (return nil))
                  (t
                   (incf bytes-read llength)
                   ;; make sure the pre-made arrays are reset
                   (setf (fill-pointer field-name) 0 (fill-pointer value-field) 0)
                   (let ((header-field-name (header-line-field-name line
                                                                    field-name)))
                     (multiple-value-bind (header-row visible-row closet-box
                                                      mtype mvalues)
                         (header-field-dispatch header-field-name line value-field)
                       (unless (null visible-row)
                         (setq visible-header-rows
                               (nconc visible-header-rows (list visible-row))))
                       (unless (null closet-box) (push closet-box closet-boxes))
                       (unless (null mtype)   (setq mime-type mtype))
                       (unless (null mvalues) (setq mime-values
                                                    (nconc mime-values mvalues)))
                       (unless (null header-row)
                         (append-row header header-row))))))))
    (let ((return-header (make-box visible-header-rows)))
      (unless (null closet-boxes)
        (boxer::add-closet-row return-header (make-row closet-boxes)))
      (values return-header mime-type mime-values bytes-read))))

;; we could make this more data driven but right now, there just aren't
;; that many possibilities...
;; should return a row (to be added to the full header),
;; a visible-row (to be added to the displayed header, maybe nil), closet-box (maybe nil)
(defun header-field-dispatch (field-name line value-field)
  (let ((desired-field (car (member field-name *box-worthy-pop-msg-header-fields*
                                    :test #'string-equal)))
        (header-row (make-net-row line))
        (visible-row nil)
        (closet-box  nil)
        (mime-type   nil)
        (mime-values nil))
    (cond ((not (null desired-field))
           (setq visible-row (make-net-row line))
           (setq closet-box
                 (make-box (list (make-net-row
                                  (if (header-field-has-machine-part? desired-field)
                                    (header-line-machine-field line value-field)
                                    (header-line-value line value-field))))
                           'boxer::data-box
                           (copy-seq desired-field))))
          ((string-equal field-name "Content-Type")
           ;; parse for possible parameters...
           ;; usually boundary for multipart, possibly charset for text
           ;; applications/xxx can have somthing more specific
           (multiple-value-bind (type values)
                                (mime-header-line-value line value-field)
             (setq mime-type type)
             ;(setq header-row nil)
             (unless (null values) (setq mime-values values))))
          ;((string-equal field-name "MIME-Version")) ;treat as vanilla header row for now
          ((string-equal field-name "Content-Transfer-Encoding")
           (let ((encoding (mime-header-line-value line value-field)))
             ;(setq header-row nil)
             (setq mime-values (list :encoding encoding))))
          (t nil))
    (values header-row visible-row closet-box mime-type mime-values)))


;; NOTE: some mailers violate the spec by sending only a type
;; given a MIME type/subtype symbol,returns keyword MIME type and subtype values
(defun mime-type-values (mime-type)
  (let* ((typestring (symbol-name mime-type))
         (slashpos (position #\/ typestring)))
    (cond ((null slashpos)
           ;(error "Invalid MIME type/subtype pair ~A" mime-type)
           (values (intern typestring (find-package "KEYWORD"))
                   :default)
           )
          (t (values (intern (subseq typestring 0 slashpos)
                             (find-package "KEYWORD"))
                     (intern (subseq typestring (1+& slashpos))
                             (find-package "KEYWORD")))))))

;; this is the central MIME mail reading function.  It is
;; passed the pop-stream AFTER the header has been read out of it
;; use header information to handle MIME bodies
;; it may be called recursively for multipart messages, in which case
;; the optional boundary parameter is used when we enter append-pop-message body
;; recursively while constructing a part of a multipart message
;;
;; need to hack "application/mac-binhex40", application/applefile and
;; multipart/appledouble in addition to general mechanism for associated MIME
;; types with mac types (see mime-values->mac-file-values for how the
;; associations are created/modified
;;
;; First, specific type/subtype pairs are searched, then general types are
;; searched
;;
;; NOTE: a lot of the cases end up calling the same functions, they are separated
;;       out to help sketch out the likely set of near term possibilities
;;      SHould also change this to be more data driven (def-mime-handler)
(defun append-pop-message-body (message stream mime-type mime-values
                                        &optional (bytes-read 0) save-to-file?)
  ;; the "message" should already have the header info, and the stream should
  ;; be positioned to start making the body
  (multiple-value-bind (main-type subtype)
      (mime-type-values mime-type)
    ;; first, dispatch on main type, then look for specific subtype handlers
    (case main-type
      (:text
       (case subtype
         (:plain (get-mime-plain-text message stream bytes-read save-to-file?))
         (otherwise (get-mime-plain-text message stream
                                         bytes-read save-to-file?))))
      (:multipart
       (let* ((boundary (getf mime-values :boundary)))
         (if (null boundary)
             (get-mime-plain-text message stream
                                  bytes-read save-to-file?) ; escape clause
             (case subtype
               ((:mixed :digest)
                (get-mime-multipart message stream boundary bytes-read))
                (:appledouble
                 (get-mime-appledouble message stream boundary mime-values
                                       bytes-read))
                (:parallel
                 (get-mime-pmultipart message stream boundary bytes-read))
                (otherwise
                 (get-mime-multipart message stream boundary bytes-read))))))
      (:application
       (case subtype
         ((:boxer :prs.boxer :vnd.boxer)
          (get-mime-box message stream bytes-read (getf mime-values :link)))
         (:mac-binhex40
          (get-mime-binhex-box message stream bytes-read
                               (getf mime-values :name)))
         (:applefile
          (get-mime-applefile-box message stream bytes-read
                                  (getf mime-values :name)
                                  (getf mime-values :encoding)))
         (otherwise
          (get-mime-binary-box mime-type message stream bytes-read
                               (getf mime-values :name)
                               (getf mime-values :encoding)))))
      (:message
       (case subtype
         (:rfc822 ; handle this as plain text for now
          (get-mime-plain-text message stream bytes-read save-to-file?))
         (otherwise (get-mime-plain-text message stream bytes-read))))
;      ((:image :audio) ; binary types which should be saved and NOT shown
;       )
      (otherwise  ;same as for (:application/octet-stream)
       ;; offer to save in a file...
       ;; empty out for now...
       (if (eq (getf mime-values :encoding) :base64)
         (get-mime-binary-box mime-type message stream bytes-read
                              (getf mime-values :name))
         (get-mime-plain-text  message stream bytes-read save-to-file?)))))
  (when (null *mime-multipart-boundary-value*)
    (shrink message)) ; only shrink top level
  message)

;;;; utility functions for rendering particular MIME types

;; boxes, note that there may already be header stuff in MESSAGE
;; shouldn't have to worry about seeing a multipart boundary in here
;; NOTE: in-file? means we should save as a link
(defun get-mime-box (message stream &optional (bytes-read 0) in-file?)
  (if in-file?
    (get-mime-link-box message stream bytes-read)
    (with-base64-stream (in64 stream)
      (let* ((boxer::*file-system-verbosity* nil) ;no updates in base64 streams
             (box (boxer::load-binary-box-from-stream-internal in64)))
        ;; now we need to transfer the vitals (contents, closet, name, type(?),
        ;;  etc) of the return box into the message
        ;; very much like BOXER::INITIALIZE-BOX-FROM-BOX (loader.lisp) except we
        ;; need to merge with (possible) existing header info
        ;; name
        (let ((name (box::name-row box)))
          (unless (null name) (box::set-name message name)))
        ;; type
        (unless (or (eq (class-of message) (class-of box))
                    ;; toggling to and from sprite boxes is not handled yet.
                    ;; skip for now cause GET-MIME-BOX is called for side effect
                    (box::sprite-box? message) (box::sprite-box? box))
          (box::set-type message (class-name (class-of box))))
        ;; graphics
        (let ((gs (box::graphics-sheet box)))
          (unless (null gs) (setf (box::graphics-info message) gs)))
        ;; transparency
        (unless (null (box::exports box)) (boxer-eval::set-box-transparency message t))
        ;; flags
        (setf (slot-value message 'box::flags) (slot-value box 'box::flags))
        ;; contents & closet
        (box::move-box-internals box message)
        message))))

;; pull out the rest of a message,
(defun empty-out-mail-message (stream)
  (do ((line (net-mail-read-line stream) (net-mail-read-line stream)))
      ((null line))
       ))

;; boundary is the raw boundary stated in the content-type header.
;; need to look for leading "--" first.  Also, if we find a trailing, "--", then
;; return :END rather than just T
(defun mime-boundary-= (string boundary)
  (declare (string boundary))
  (let ((stop (length string)) (blength (length boundary)))
    (and (>& stop 2) (char= (char string 0) #\-) (char= (char string 1) #\-) ;leading "--"
         (do ((s 2 (1+& s)) (b 0 (1+& b)))
             ((>=& b blength)
              ;; made it to the end, now check for trailing "--" to decide what to return
              (if (and (>=& stop (+& blength 4))
                       (char= (char string s) #\-) (char= (char string (1+& s)) #\-))
                  :end
                  T))
           (cond ((>=& s stop) (return nil))
                 ((not (char= (char boundary b) (char string s))) (return nil)))))))

;; a stub, this should actually pick the best subpart and display only that...
(defun get-mime-pmultipart (message stream boundary &optional (bytes-read 0))
  (get-mime-multipart message stream boundary bytes-read))

(defun get-mime-multipart (message stream boundary &optional (bytes-read 0))
  (declare (ignore bytes-read))
  (let ((*mime-multipart-boundary-value* boundary)
        (*mime-multipart-boundary-encountered* nil))
    (loop (let* ((line (or *mime-multipart-boundary-encountered*
                           (net-mail-read-line stream)))
                 (boundary? (or (when (null line) :end)
                                *mime-multipart-boundary-encountered*
                                (mime-boundary-= line boundary))))
            (cond ((eq boundary? :end) ;; empty out the rest of the epilogue
                   ; 12/14/98 this is dangerous in recursive multiparts
                   ;(empty-out-mail-message stream)
                   (return))
                  (boundary? ;; we hit a boundary, call someone else to
                   ;; similiar to get-message-1 but we handle text/plain
                   ;; specially and of course, we don't need to issue & handle
                   ;; the POP commands
                   (setq *mime-multipart-boundary-encountered* nil)
                   (multiple-value-bind (header-box mime-type
                                                    mime-values header-size)
                       (make-header-box-from-stream stream)
                     ;; we grab the header first because it might tell us
                     ;; how we should process the rest of the message in
                     ;; particular, MIME information will appear in the header
                     (cond ((eq mime-type :text/plain)
                            ;; for plain text we append the rows to the
                            ;; existing box
                            (boxer::do-box-rows ((header-row header-box))
                              (append-row message header-row))
                            (append-pop-message-body message stream
                                                     mime-type mime-values
                                                     header-size))
                           (t ; for all other types, we want a box
                            (append-pop-message-body header-box stream
                                                     mime-type mime-values
                                                     header-size)
                            (append-row message (make-row (list header-box)))))))
                  (t ; we can only come here if there is some junk, so do nothing
                   nil))))))

(defvar *default-text-file-creator* :MSWD)

;; the generic text reading handler
(defun get-mime-plain-text (message stream &optional (bytes-read 0) in-file?)
  (if in-file?
    (let* ((pathname (unique-mime-path))
           (xref (boxer::make-xref :pathname pathname :mime-type :text))
           (box (make-box '(())))
           (creator *default-text-file-creator*)
           (ftype :text))
      (boxer::add-xref-closet-boxes box)
      (putprop box xref :xref)
      (boxer::set-xref-boxtop-info box creator ftype)
      (shrink box)
      (with-open-file (ostream pathname :direction :output)
        (loop
          (multiple-value-bind (line llength)
              (net-mail-read-line stream)
            (let ((boundary-value nil))
              (cond ((null line) (return nil))
                    ((and *mime-multipart-boundary-value*
                          (setq boundary-value
                                (mime-boundary-= line
                                                 *mime-multipart-boundary-value*)))
                     (setq *mime-multipart-boundary-encountered* boundary-value)
                     (return nil))
                    (t
                     (write-line line ostream)
                     (when *report-pop-message-reading-progress?*
                       (surf-message "   ~D bytes read"
                                     (incf bytes-read llength)))))))))
      #+mcl (ccl::set-mac-file-creator pathname creator)
      #+mcl (ccl::set-mac-file-type pathname ftype)
      (append-row message (make-row (list box))))
    (loop
      (multiple-value-bind (line llength)
                           (net-mail-read-line stream)
        (let ((boundary-value nil))
          (cond ((null line) (return nil))
                ((and *mime-multipart-boundary-value*
                      (setq boundary-value
                            (mime-boundary-= line *mime-multipart-boundary-value*)))
                 (setq *mime-multipart-boundary-encountered* boundary-value)
                 (return nil))
                (t
                 (append-row message (make-net-row line))
                 (when *report-pop-message-reading-progress?*
                   (surf-message "   ~D bytes read"
                                 (incf bytes-read llength))))))))))

(defun get-mime-binhex-box (message stream
                                    &optional (bytes-read 0) supplied-name)

  (declare (ignore bytes-read))
  (let* ((pathname (unique-mime-path supplied-name))
         (xref (boxer::make-xref :pathname pathname)))
    (boxer::add-xref-closet-boxes message)
    (putprop message xref :xref)
    (binhex-decode-stream stream pathname)
    (shrink message)
    #+mcl
    (let* ((creator (ccl::mac-file-creator pathname))
           (ftype (ccl::mac-file-type pathname)))
      (boxer::set-xref-boxtop-info message creator ftype)
      message)))

(defun mime-attachment-dir ()
  #+mcl (append (pathname-directory (ccl::mac-default-directory)) '("Mail Files"))
  #-mcl '())

(defun insure-attachment-dir ()
  (let ((adir (make-pathname :directory (mime-attachment-dir))))
    (when (null (probe-file adir))
      #+mcl (ccl::create-directory adir)
      #-mcl ())))

;; need to make sure that we end up with a valid filename for a particular OS
;; for example, MacOS only allows 31 letter filenames
(defun unique-mime-path (&optional supplied-name)
  (insure-attachment-dir)
  (if (null supplied-name)
      (loop (let* ((name (format nil "MIME~A" (mime-separator-value)))
                   (pathname (make-pathname :directory (mime-attachment-dir)
                                            :name name)))
              (when (null (probe-file pathname)) (return pathname))))
      (let* ((name (pathname-name supplied-name))
             (type (pathname-type supplied-name))
             (raw-pathname (make-pathname :directory (mime-attachment-dir)
                                          :name name :type type))
             ;; now make sure that the supplied path is OK
             (pathname (ensure-valid-filename raw-pathname)))
        (if (null (probe-file pathname)) pathname
            (do* ((i 0 (1+ i))
                  (newpath (newpath-for-mime name type i)
                           (newpath-for-mime name type i)))
                 (nil)
              (when (null (probe-file newpath)) (return newpath)))))))

(defvar *max-file-length* #+mcl 31 #-mcl 31)

;; should probably be moved somewhere more general (macros.lisp ?)
;; do nothing by default
#-mcl
(defun ensure-valid-filename (pathname &optional max-length)
  (declare (ignore max-length))
  pathname)

;; only check for length for now
#+mcl
(defun ensure-valid-filename (pathname &optional (max-length *max-file-length*))
  (let* ((name (pathname-name pathname)) (type (pathname-type pathname))
         (nlength (if (null name) 0 (length name)))
         (tlength (if (null type) 0 (length type)))
         (total (cond ((null name) tlength)
                      ((null type) nlength)
                      (t (+& nlength tlength 1)))))
    ;; note that either name or type can be NIL depending "." placement
    (cond ((> total max-length) ; bad, have to frob
           (cond ((null name)
                  (make-pathname :type (subseq type 0 (1- max-length))
                                 :defaults pathname))
                 ((null type)
                  (make-pathname :name (subseq name 0 (1- max-length))
                                 :defaults pathname))
                 (t
                  ;; prefer to trim name
                  (let ((trim-amount (- total max-length)))
                    (cond ((< trim-amount nlength)
                           (make-pathname :name (subseq name 0
                                                        (- nlength trim-amount))
                                          :type type
                                          :defaults pathname))
                          ((< trim-amount tlength)
                           (make-pathname :name name
                                          :type (subseq type 0
                                                        (- tlength trim-amount))
                                          :defaults pathname))
                          (t
                           ;; got to trim from both, prefer name since type will
                           ;; be getting messed up anyway
                           (make-pathname :name (subseq name 0 (1- max-length))
                                          :defaults pathname)))))))
          (t pathname))))

;; used when we've probed for, and discovered and existing file with the same name
(defun newpath-for-mime (name type vers &optional (dir (mime-attachment-dir)))
  (let ((rawpath (ensure-valid-filename
                  (make-pathname :directory dir :name name :type type)
                  (- *max-file-length*
                     ;; CONSes way less than (floor (log vers 10))
                     (length (format nil "~D" vers))))))
    (make-pathname :name (format nil "~A~D" (pathname-name rawpath) vers)
                   :defaults rawpath)))

;; generic file saving
(defun get-mime-binary-box (mime-type message stream bytes-read
                                      &optional supplied-name
                                      (binary-encoding :base64))
  (let* ((pathname (unique-mime-path supplied-name))
         (xref (boxer::make-xref :pathname pathname :mime-type mime-type)))
    (boxer::add-xref-closet-boxes message)
    (putprop message xref :xref)
    (multiple-value-bind (creator ftype)
        (mime-values->mac-file-values mime-type)
      (boxer::set-xref-boxtop-info message creator ftype)
      (shrink message)
      (cond ((eq binary-encoding :base64)
             (get-mime64-binary-data message stream bytes-read pathname))
            ((eq binary-encoding :uuencode)
             (get-mime-uu-binary-data message stream bytes-read pathname))
            (t
             (get-mime-8bit-binary-data message stream bytes-read pathname)))
      #+mcl (ccl::set-mac-file-creator pathname creator)
      #+mcl (ccl::set-mac-file-type pathname ftype)
      message)))

(defun get-mime-link-box (message stream bytes-read)
  (let* ((pathname (unique-mime-path)))
    (box::mark-box-as-file message pathname)
    (shrink message)
    (get-mime64-binary-data message stream bytes-read pathname)
    #+mcl (ccl::set-mac-file-creator pathname :boxr)
    #+mcl (ccl::set-mac-file-type pathname :boxr)
    (setf (slot-value message 'box::first-inferior-row) nil)
    message))

;; this basically just empties the text out, but
;; DOES check for a mime boundary to avoid hanging
(defun get-mime-uu-binary-data (message stream
                                        &optional (bytes-read 0)
                                        (pathname
                                         #+mcl (ccl::choose-new-file-dialog
                                                :prompt
                                                "save MIME data in file:")
                                         #+lispworks
                                         (capi:prompt-for-file
                                          "save MIME data in file:"
                                          :filter bw::*boxer-file-filters*
                                          :operation :save :if-does-not-exist :ok
                                          :owner bw::*boxer-frame*)
                                         #-(or lispworks mcl) pathname))
  (declare (ignore message))
  (with-open-file (ostream pathname :direction :output)
    (loop
      (multiple-value-bind (line llength)
          (net-mail-read-line stream)
        (let ((boundary-value nil))
          (cond ((null line) (return nil))
                ((and *mime-multipart-boundary-value*
                      (setq boundary-value
                            (mime-boundary-= line *mime-multipart-boundary-value*)))
                 (setq *mime-multipart-boundary-encountered* boundary-value)
                 (return nil))
                (t
                 (write-line line ostream)
                 (when *report-pop-message-reading-progress?*
                  (surf-message "   ~D bytes read"
                                (incf bytes-read llength))))))))))


;; If the message is in 8bit, then we just have to hack conversion from
;; char stream back to bytes
;; NOTE: need to hack EOF properly for multipart sections
(defun get-mime-8bit-binary-data (message stream
                                          &optional (bytes-read 0)
                                          (pathname
                                           #+mcl (ccl::choose-new-file-dialog
                                                  :prompt
                                                  "save MIME data in file:")
                                           #+lispworks
                                           (capi:prompt-for-file
                                            "save MIME data in file:"
                                            :filter bw::*boxer-file-filters*
                                            :operation :save :if-does-not-exist :ok
                                            :owner bw::*boxer-frame*)
                                           #-(or lispworks mcl) pathname))
  (declare (ignore message bytes-read))
  (let ((mime-boundary *mime-multipart-boundary-value*)
        (bindex 0))
    ;; these vars are bound cause they likely will be needed in a smarter
    ;; version of this function
    (declare (ignore mime-boundary bindex))
    (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                       :if-exists :supersede :if-does-not-exist :create)
      (let ((eof-value (list 'eof)))
        (loop (let ((b (read-char stream nil eof-value)))
                (cond ((eq b eof-value) (return nil))
                      (t (write-byte (char-code b) s)))))))))

(defun get-mime64-binary-data (message stream
                                       &optional (bytes-read 0)
                                       (pathname
                                        #+mcl (ccl::choose-new-file-dialog
                                               :prompt "save MIME data in file:")
                                        #+lispworks
                                        (capi:prompt-for-file
                                         "save MIME data in file:"
                                         :filter bw::*boxer-file-filters*
                                         :operation :save :if-does-not-exist :ok
                                         :owner bw::*boxer-frame*)
                                        #-(or lispworks mcl) pathname))
  (declare (ignore message bytes-read))
  (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                     #+mcl :fork #+mcl :data
                     :if-exists :overwrite :if-does-not-exist :create)
    (with-base64-stream (in64 stream)
      (loop (let ((b (read-byte in64 nil nil)))
              (if (null b) (return) (write-byte b s)))))))

;; obsolete, here to (incompletely) support (fill-box-from-url mail-message)
(defun make-pop-message (header stream &optional box)
  (let ((message (or box (boxer::make-uninitialized-box 'boxer::data-box)))
        (body (boxer::make-uninitialized-box 'boxer::data-box)))
    (shared-initialize message t) (shared-initialize body t)
    ;; first, empty out the rest of the stream to compose the body
    (do ((line (net-mail-read-line stream) (net-mail-read-line stream)))
        ((null line))
      (append-row body (make-net-row line)))
    ;; now make the message from the header and the body
    (append-row message (make-row (list header)))
    (shrink body)
    (append-row message (make-row (list body)))
    message))

;; this is called when the message is to large to be initially loaded,
;; if we are passed the header, use it, along with a black box with a
;; MAIL-MESSAGE-BODY URL to make up the message.  If the header is not
;; passed in, e.g. because the POP3 server does not support the TOP command,
;; then we have to make a black box with a MAIL-MESSAGE url
(defun make-delayed-pop-message (number &optional header)
  (let ((message (boxer::make-uninitialized-box 'boxer::data-box)))
    (shared-initialize message t)
    (cond ((null header) ;
           (putprop message (make-instance 'mail-message :message-number number)
                    :url)
           ;; now process the box (thick borders ?, file box?)
           (shrink message)
           ;(set-url-flags message) ; storage-chunk, RO flags
           message)
          (t (let ((body (boxer::make-uninitialized-box 'boxer::data-box)))
               (shared-initialize body t)
               (putprop body (make-instance 'mail-message-body
                                  :message-number number)
                        :url)
               ;; now process the box (thick borders ?, file box?)
               (shrink body)
               ;(set-url-flags body)
               ;; now make the message from the header and the body
               (append-row message (make-row (list header)))
               (append-row message (make-row (list body)))
               message)))))

;;; Primitives

(defboxer-primitive bu::get-mail ((boxer-eval::dont-copy mailbox) delete-messages?)
  (let ((*delete-loaded-messages?* (boxer-eval::true? delete-messages?))
        (mailbox (box-text-string mailbox)))
    (make-vc-using-url (make-instance 'pop-url
                         :scheme-string (concatenate 'string "//" mailbox)))))



(defboxer-primitive bu::delete-message ((bu::port-to message))
  message boxer-eval::*novalue*
  )

;; should this be bu::on-server?
(defboxer-primitive bu::mail-message? ((bu::port-to message))
  message boxer-eval::*novalue*
  )



;;;; Finger

(defvar *finger-port* 79)

(defun finger (host &optional (user ""))
  (let* ((*net-verbose* nil) ; suppress status mesages
         (stream (open-tcp-stream host *finger-port*)))
    (unwind-protect
        (progn
          (net-write-control-line stream user)
          ;; put the reply into a box...
          (make-vc-from-text-stream stream))
      (close stream))))

(defun make-vc-from-text-stream (stream)
  (let ((rows nil))
    (do ((line (net-read-line stream) (net-read-line stream)))
        ((null line) (make-vc (nreverse rows)))
      (push (boxer::make-evrow-from-string line) rows))))

(defboxer-primitive bu::finger (user)
  (let* ((raw-text (box-text-string (box-or-port-target user)))
         (@pos (position #\@ raw-text))
         (host (if @pos ;; if no "@", treat the entire name as a hostname
                   (subseq raw-text (1+ @pos))
                   raw-text))
         (user (if @pos (subseq raw-text 0 @pos) "")))
    (finger host user)))


 ;;; For Debugging.....

#|
;; problem mesages saved as text...

(defvar *mailstream* )

(setq *mailstream* (open
                    #+lispworks (capi::prompt-for-file "mail message:")
                    #+mcl       (ccl::choose-file-dialog)
                    #-(or lispworks mcl) (progn (format t "~&Enter a filename: ")
                                           (read-line))))

(let ((*net-mail-read-line-hook* 'net-mail-read-line-from-file))
     ;; or net-mail-read-line-from-mac-file
  (make-header-box-from-stream *mailstream*))

(let ((*net-mail-read-line-hook* 'net-mail-read-line-from-file))
  (append-pop-message-body * *mailstream* :multipart/mixed '(:boundary "Boundary_")))

(close *mailstream*)


|#
