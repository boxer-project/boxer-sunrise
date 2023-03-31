;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|

 $Header: file-prims.lisp,v 1.0 90/01/24 22:11:51 boxer Exp $

 $Log:	file-prims.lisp,v $
;;;Revision 1.0  90/01/24  22:11:51  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



   This file contains all of the boxer functions which use the
   file system.


Modification History (most recent at top)

12/15/11 write-text-stream-internal changed to handle the :interpolate option
         *box-encountered-during-text-file-operation* changed to :interpolate
         new function write-text-stream-for-boxy-row
12/22/05 wrap ignore-errors around parts of check-valid-filename-for-primitive
         to cleanly handle illegal pathnames (e.g. from mistyped URL protocol fields)
         READ-INTERNAL checks for possible null pathname to catch this case
 8/23/05 fill-box-from-local-file uses new probe-search-file to check for boxer files
 8/06/05 fixed truncated-filename to handle names longer than *max-filename-length*
         also boosted *max-filename-length* to 50
10/06/04 extended storage-{name,info} to handle xrefs
 8/27/04 copyright error message in mark-for-saving referred to wrong replacement name
 7/16/04 Fixed Copyright Error message in MARk-FOR-SAVING
 8/15/03 added show-file-info as diagnostic tool, set-boxer-file-info
 4/19/03 merged current LW and MCL files
 2/03/03 destructure-file-info-box, file-info-to-file, file-to-file-info &
         merge-file-info
 2/01/03 changed make-file-storage-info-box to make empty boxes for fields
         which are either null or :unspecific
10/20/02 RECORD-FILE-BOX-PLACE & RECORD-URL-BOX-PLACE stubbified and removed from
         READ-INTERNAL & FILL-BOX-FROM-LOCAL-FILE in preparation for "UC clean"
         reimplementation
 9/02/02 UC free version of MARK-FOR-SAVING, added TOGGLE-MODIFIED-FLAG
 8/27/02 make-file-storage-info-box, VC names must be BU symbols instead of strings
         make-url-storage-info-box does the same via intern-in-bu-package
 8/ 8/02 finished support functions for bu::storage-xxx
 8/ 5/02 added bu::storage-name and bu::storage-info
10/ 7/01 status-line-save-format-string for use in save-internal
 3/ 8/01 fill-box-from-local-file: relative pathname check should check for
         null dirs as well as :relative
 2/14/01 merged current LW and MCL files
 1/15/01 lispworks version for check-valid-filename-for-primitive
10/16/00 make-backup-if-necessary now checks for 31 char pathnames
10/10/00 make-backup-if-necessary for windows will keep the pathname type
11/14/99 removed file-namestring from fill-box-from-local-file because it loses
         when merging a typeless pathname.  Replace by make-pathname with the
         type component defaulting to :unspecific for null pathname-type
10/15/99 simplified fill-box-from-local-file by using file-namestring
10/14/99 save-text-file processes the modified editor object queue
         save-text-file-internal now renders VC's into editor boxes
 9/13/99 fill-box-from-local-file, don't mark the file box dirty for relative
         pathname fileboxes, unless the relative part is actually changed
 6/25/99 extracted pretty-file-box-name for use in other places
         added pretty-url-box-name (temp version), record-file-box-place,
         and record-url-box-place
         use record-file-box-place in read-internal,fill-box-from-local-file
 5/25/99 changed mark-box-as-file to not be so eager to convert to :file boxtop
 5/17/99 check for filename before saving old-file-prop in unmark-box-as-file
 5/12/99 added eval::*novalue* return value to mark-for-saving
 4/25/99 {un}mark-box-as-file now adds/removes :file :boxtop property
 4/14/99 use new calling conventions for write-boxer-file-info
 3/26/99 bu::mark-for-saving added
 2/12/99 save-generic error messages use OPEN as the name of the prim
         instead of READ
 2/11/99 (defboxer-primitive bu::unfile-box
 2/10/99 added unmark-box-as-file
12/15/98 changed various save functions to process the mutation queue before saving
10/ 1/98 fill-box-from-local-file probes for file in relative pathname clause
         also separate prompts for choose file dialog when file not found
 9/29/98 save-generic sets read-only? flag both ways instead of only to T
 9/13/98 OPEN changed to bash name slot of world boxes
 5/27/98 added autoloading case to fill-box-from-local-file for relative paths
         changed read-internal-1 to give lisp error (which should be handled)
         for missing autoload
 5/27/98 Started Logging Changes: source = boxer version 2.3alphaR1

|#

(in-package :boxer)

;;;; the prims

(boxer-eval::defboxer-primitive bu::save ()
  ;; make sure the editor structure is up to date
  (process-editor-mutation-queue-within-eval)
  (cond ((box? boxer-eval::*lexical-variables-root*)
         (com-save-document))
        (t (boxer-eval::primitive-signal-error :file "You can only SAVE editor boxes"))))

(boxer-eval::defboxer-primitive bu::quietly-save ()
  ;; make sure the editor structure is up to date
  (process-editor-mutation-queue-within-eval)
  (cond ((box? boxer-eval::*lexical-variables-root*)
         (com-save-document nil T))
        (t (boxer-eval::primitive-signal-error :file "You can only SAVE editor boxes"))))

(boxer-eval::defboxer-primitive bu::save-as ((boxer-eval::dont-copy name))
  ;; make sure the editor structure is up to date
  (process-editor-mutation-queue-within-eval)
  (if (not (box? boxer-eval::*lexical-variables-root*))
      (boxer-eval::primitive-signal-error :file "You can only SAVE-AS editor boxes")
      (let ((savestring (box-text-string name))
            (filebox (current-file-box)))
        (cond ((url-string? savestring)
               ;; remove possible associated file here
               (removeprop filebox :associated-file)
               (putprop filebox (boxnet::make-url-from-string savestring t) :url))
              (t
               (removeprop filebox :url)
               (putprop filebox (check-valid-filename-for-primitive
                                 savestring *boxer-pathname-default*)
                        :associated-file)))
        (com-save-document)))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::save-box-as ((boxer-eval::dont-copy name))
  ;; make sure the editor structure is up to date
  (process-editor-mutation-queue-within-eval)
  (if (not (box? boxer-eval::*lexical-variables-root*))
      (boxer-eval::primitive-signal-error :file "You can only SAVE-AS editor boxes")
      (let ((savestring (box-text-string name))
            (filebox boxer-eval::*lexical-variables-root*))
        (cond ((url-string? savestring)
               ;; remove possible associated file here
               (removeprop filebox :associated-file)
               (putprop filebox (boxnet::make-url-from-string savestring t) :url))
              (t
               (removeprop filebox :url)
               (putprop filebox (check-valid-filename-for-primitive
                                 savestring *boxer-pathname-default*)
                        :associated-file)))
        (com-box-save-as nil boxer-eval::*lexical-variables-root*)))
  boxer-eval::*novalue*)

;; use TELL to direct
(boxer-eval::defboxer-primitive bu::unfile-box () (com-unfile-document))

;; Returns TRUE, or a box with a defined reason or else FALSE,
;; meaning "some other reason"
(boxer-eval::defboxer-primitive bu::safe-to-save? ()
  (let* ((box (current-file-box))
         (raw-filename (getprop box :associated-file))
         ;; careful, the filename might be relative
         (existing-filename (cond ((null raw-filename) nil)
                                  ((eq (car (pathname-directory raw-filename))
                                       :absolute)
                                   raw-filename)
                                  (t ; must be relative
                                   (let* ((sup-file-box (current-file-box
                                                         (superior-box box)))
                                          (sup-filename (getprop
                                                         sup-file-box
                                                         :associated-file)))
                                     (cond ((not (null sup-filename))
                                            (merge-pathnames raw-filename
                                                             sup-filename)))))))
         (fps (get-boxer-file-properties existing-filename)))
    (cond ((null existing-filename)
           (make-vc '((bu::no-filename))))
          #-mcl ;; MACL probe-file loses on directories
          ((null (probe-file (make-pathname
                              :directory (pathname-directory existing-filename))))
           (make-vc '((bu::no-directory))))
          #+mcl ;; on the mac, you can "lock" files...
          ((and (probe-file existing-filename)
                (ccl::file-locked-p existing-filename))
           (make-vc '((bu::file-locked))))
          ;; make sure we were the last person to actually write it out and
          ;; that there isn't a conflict with another box pointing to the
          ;; same pathname
          ((and (not (null fps))
                (not (null (file-write-date existing-filename)))
                (not (= (boxer-file-creation-date fps)
                          (file-write-date existing-filename))))
           (make-vc '((bu::file-is-newer))))
          ((and (not (null fps))
                (not (eq (boxer-file-box fps) box)))
           ;; the box doesn't match with the most current box for this file
           ;; there are several possibilities for this so we break it down:
           (if (member-boxer-file-box box fps)
               (make-vc '((bu::newer-other-box)))
               ;; the distinction between these is that in the 2nd case, the box has
               ;; never been saved to that filename
               (make-vc '((bu::newer-other-box)))))
          (t boxer-eval::*true*))))

(defun check-valid-filename-for-primitive (filename &optional
                                                    defaults allow-wild)
  (declare (ignore allow-wild))
  (cond ((pathnamep filename)
         ;; If it is already a pathname leave it alone (this can happen if
         ;; the pathname has been returned from boxer-open-file-dialog)
         filename)
        (t (ignore-errors
             (if (null defaults)
                 (pathname filename)
               (merge-pathnames filename defaults))))))

;; tries to be smart about file type
(defun save-generic (box filename &key format read-only? always-save?)
  (let* ((dest-pathname (check-valid-filename-for-primitive
                         filename *boxer-pathname-default*))
         (fps (get-boxer-file-properties dest-pathname))
         (file-format (or format
                          (getprop box :preferred-file-format)
                          (file-type filename)
                          :boxer)))
    ;; first perform various kinds of reality checking
    ;;
    ;; we can only save out top level boxes for now, I suppose it
    ;; it would be possible to run the printer to generate real structure
    ;; then save THAT out but it seems to be too much work for a very
    ;; LIMITED set of circumstances
    (unless (box? box)
      (boxer-eval::primitive-signal-error
       :file-error "Sorry, you can only save out top level boxes"))
    ;; then make sure the directory exists
    #-mcl ;; MACL probe-file loses on directories
    (unless (probe-file (make-pathname :directory
                                       (pathname-directory dest-pathname)))
      (boxer-eval::primitive-signal-error :file-error
                                    "The directory, "
                                    (namestring dest-pathname)
                                    ", does not exist"))
    ;; on the mac, you can "lock" files...
    #+mcl
    (when (and (probe-file dest-pathname) (ccl::file-locked-p dest-pathname))
      (boxer-eval::primitive-signal-error :file-error
                                    "The file, " (namestring dest-pathname)
                                    ", is locked"))
    ;; make sure we were the last person to actually write it out and
    ;; that there isn't a conflict with another box pointing to the
    ;; same pathname
    (when (and (null always-save?) (not (null fps)))
      (cond ((and (not (null (file-write-date dest-pathname)))
                  (not (= (boxer-file-creation-date fps)
                          (file-write-date dest-pathname)))
                  (not (file-overwrite-ok
                        (format nil "~A, has been changed since last ~A.  Save Anyway ?"
                                (namestring dest-pathname)
                                #+mcl "Open" #-mcl "READ"))))
             (throw 'cancel-boxer-file-dialog nil)
             ;; if we are going to query, then don't error out
             #|
         (boxer-eval::primitive-signal-error
    :file-overwrite-warning
    (make-box `(("The File:")
          (,(namestring dest-pathname))
          ("has been changed since you last Read it")
          ()
          ("Use REALLY-SAVE to save out changes"))))|#
             )
            ((not (eq (boxer-file-box fps) box))
             ;; the box doesn't match with the most current box for this file
             ;; there are several possibilities for this so we break it down:
             (unless (if (member-boxer-file-box box fps)
                       (file-overwrite-ok
                        "Another version of this box exists and has been saved more recently.  Save anyway?")
                       (file-overwrite-ok
                        (format nil "The file, ~A, is associated with a different box.  Save Anyway ?"
                                (namestring dest-pathname))))
               ;; dont error out if we bother to ask...
               (throw 'cancel-boxer-file-dialog nil)
               #|
     (boxer-eval::primitive-signal-error
      :file-overwrite-warning
      "The filename, " (namestring dest-pathname)
      ", is associated with a different box")|#
               ))))
    (setf (read-only-box? box) read-only?)
    ;; this should be changed to be more data driven like the file readers
    ;; need to make the appropriate file type save functions more uniform 1st
    (case file-format
      (:boxer (save-internal box dest-pathname))
      (:application/box (save-internal box dest-pathname))
      (:text (save-text-file-internal box dest-pathname))
      (:text/plain (save-text-file-internal box dest-pathname))
      (:application/boxer.document (save-box-to-boxer-document-format-zipped box dest-pathname))
      (t (save-internal box dest-pathname)))
    ;; if everything worked, then update the file properties so we don't
    ;; get an error the next time we try and save out the file
    (record-boxer-file-properties dest-pathname
                                  (file-write-date dest-pathname)
                                  (file-author dest-pathname) box)
    (mark-box-as-file box dest-pathname)
    ;; mark the box as unmodified
    (mark-file-box-clean box)))

;; got to be careful, the pathname might have "~"'s (especially from backup files)
;; also, try to keep the length limited to avoid odd window resizing bug in LWW
(defun status-line-save-format-string (dest-pathname)
  (let* ((*max-file-pathname-length* 50) ; should be adaptive to width of name-pane
         (pretty-path (pretty-file-box-name dest-pathname)))
    (format nil "Saving ~A ~~D" (cond ((find #\~ pretty-path)
                                       (quote-tilde pretty-path))
                                      (t pretty-path)))))

(defun quote-tilde (string)
  (let* ((lstring (length string))
         (newstring (make-array (+& 1 lstring) :element-type 'character
                                :adjustable t :fill-pointer 0)))
    (dotimes (i lstring)
      (let ((char (char string i)))
        (cond ((char= char #\~)
               (vector-push-extend #\~ newstring)
               (vector-push-extend #\~ newstring))
              (t (vector-push-extend char newstring)))))
    newstring))

(defun save-internal (box dest-pathname)
  (let* ((tmp-file-string (symbol-name (gensym)))
   (tmp-dest (merge-pathnames tmp-file-string
            dest-pathname)))
   (unwind-protect
   (let ((*status-line-saving-format-string*
                (status-line-save-format-string dest-pathname)))
           ;; feedback
           (status-line-display 'saving-box
                                (format nil "Saving ~A..." dest-pathname))
     ;; actually dump the box
     (dump-top-level-box box tmp-dest)
     ;; now check to see if we need to make a backup
     (make-backup-if-necessary dest-pathname)
     ;; now move the freshly save file from the tmp-name to the dest name
     (rename-file tmp-dest dest-pathname))
     (status-line-undisplay 'saving-box)
     (boxer-editor-message "~A box saved to ~A" (name-string box)(namestring dest-pathname))
     ;; make sure the tmp-dest is gone
     (unless (null (probe-file tmp-dest)) (delete-file tmp-dest)))
   ;; now handle file name stickyness if we have to
   (unless (null *sticky-file-defaulting?*)
      ;; set the default pathname to be the current pathname ONLY after
      ;; we make sure that the current pathname has all the "important"
      ;; components, otherwise the default can become damaged in a way
      ;; that leaves it unchangeable (e.g. pathname-name becomes NIL
      ;; if the "default" was ".login")
      (if (and (not (null (pathname-name dest-pathname)))
         (not (null (pathname-type dest-pathname))))
    (setq *boxer-pathname-default* dest-pathname)
    ;; otherwise, use as much of the dest-pathname as we can to set
    ;; the default
    (setq *boxer-pathname-default*
    (make-pathname :directory (pathname-directory dest-pathname)
             :defaults *boxer-pathname-default*))))
    ))

(defun make-backup-if-necessary (dest-pathname)
  (when (probe-file dest-pathname)
    ;; looks like there already is a file with that name
    (rename-file dest-pathname
                 ;; on windows systems, we have to preserve the pathname type
                 #+win32
                 (merge-pathnames (concatenate 'string
                                               (pathname-name dest-pathname)
                                               *file-backup-suffix*)
                                  dest-pathname)
                 #+mcl
                 (let ((old-name (file-namestring dest-pathname)))
                   ;; have to check for possible 31 char length
                   (cond ((>= (length old-name) 31)
                          (merge-pathnames
                           (concatenate 'string
                                        (subseq old-name 0 30)
                                        *file-backup-suffix*)
                           dest-pathname))
                         (t
              (concatenate 'string
                     (namestring dest-pathname)
                                       *file-backup-suffix*))))
                 #-(or mcl win32)
     (concatenate 'string
            (namestring dest-pathname)
            *file-backup-suffix*)
                 #+mcl :if-exists #+mcl :supersede)))


;; used to be READ
(boxer-eval::defboxer-primitive bu::open ((boxer-eval::dont-copy file))
  (let* ((filename (box-text-string file))
         (box (read-internal filename)))
    ;; if the box was a world file, need to fix the NAME slot
    (when (and (stringp (slot-value box 'name))
               (string= (slot-value box 'name) "WORLD"))
      (setf (slot-value box 'name) nil))
    box))

(boxer-eval::defboxer-primitive bu::READ ((boxer-eval::dont-copy filename))
  filename
  (boxer-eval::primitive-signal-error :file "READ is an obsolete function, use OPEN instead"))

#|
(boxer-eval::defboxer-primitive bu::read-only ((boxer-eval::dont-copy filename))
  (boxnet::read-only-internal (box-text-string filename)))
|#

;; this can handle url strings as well as filenames...
(defun read-internal (name)
  (if (url-string? name)
      (read-internal-url name)
    ;; can still be a bad filename here (bad URL spelling) (e.g. "htTtp://foo.bar")
      (let* ((pathname (check-valid-filename-for-primitive
                        name *boxer-pathname-default*))
             ;; note, we resolve the full pathname here instead of letting
             ;; read-internal-1 do the work so that we can have a complete
             ;; pathname available for the bookeeping operations which follow
             ;; a successful read-internal-1
             (box (unless (null pathname) (read-internal-1 name pathname))))
        ;; now do the box/file bookkeeping,
        ;; NOTE: It has to be here AFTER the initialize-box-from-box
        (cond ((box? box)
               ;(boxnet::read-box-postamble box) ;(Boxer Server stuff) no longer used
               (mark-box-as-file box pathname)
               (mark-file-box-clean box)
               ;; keep track of the file properties so that we can check
               (record-boxer-file-properties pathname
                                             (file-write-date pathname)
                                             (file-author pathname) box)
               box)
              (t
               (boxer-eval::primitive-signal-error :file-error "Unable to READ " name))))))


;; Note: read-internal-1 does NOT do the bookeeping ops because some of
;; the bookkeeping operations rely on the EQness of the box and there
;; are some functions (like fill-box-from-server) which USE the result of
;; read-internal-1 to initialize another box in the editor.  It is that
;; other box in the editor that the bookeeping operations need to keep
;; track of and it is in those higher up functions where the bookeeping must occur
(defun read-internal-1 (filename &optional
                                 (pathname (check-valid-filename-for-primitive
                                filename *boxer-pathname-default*)))
  (unless (probe-file pathname)
    (if *in-autoload-environment*
        (error "Autoload File not found: ~A" (namestring pathname))
        (boxer-eval::primitive-signal-error :file-or-directory-not-found (namestring pathname))))
  (prog1
    (let* ((ftype (file-type pathname))
           ;; dispatch to special readers when appropriate
           ;; ALL special reader take a filename arg and return a box
           ;; ALL implementations should handle :boxer and :text types
           (box (let ((special-file-reader-function
                       (get-special-file-reader ftype)))
                  (cond ((null special-file-reader-function)
                         (cond ((not (null *error-on-unknown-file-type*))
                                (boxer-eval::primitive-signal-error
                                 :unknown-file-type
                                 "Don't know how to load a " ftype "file"))
                               ((not (status-line-y-or-n-p
                                      (format nil "Unknown file type, ~A, load as Text ?"
                                              ftype)))
                                (boxer-eval::primitive-signal-error
                                 :unknown-file-type
                                 "Don't know how to load a " ftype "file"))
                               (t
                                (read-text-file-internal pathname))))
                        (t
                         (funcall special-file-reader-function pathname))))))
      box)
    ;; now handle file stickyness if we want to
    (unless (null *sticky-file-defaulting?*)
      ;; set the default pathname to be the current pathname ONLY after
      ;; we make sure that the current pathname has all the "important"
      ;; components, otherwise the default can become damaged in a way
      ;; that leaves it unchangeable (e.g. pathname-name becomes NIL
      ;; if the "default" was ".login")
      (if (and (not (null (pathname-name pathname)))
               (not (null (pathname-type pathname))))
        (setq *boxer-pathname-default* pathname)
        ;; otherwise, use as much of the pathname as we can to set
        ;; the default
        (setq *boxer-pathname-default*
              (make-pathname :directory (pathname-directory pathname)
                             :defaults *boxer-pathname-default*))))))

(boxer-eval::defboxer-primitive bu::probe-file ((boxer-eval::dont-copy filename))
  (let ((pathname (check-valid-filename-for-primitive
       (box-text-string filename) *boxer-pathname-default*)))
    (boxer-eval::boxer-boolean (probe-file pathname))))

(boxer-eval::defboxer-primitive bu::toggle-modified-flag ()
  (when (box? boxer-eval::*lexical-variables-root*)
    (cond ((file-box-dirty? boxer-eval::*lexical-variables-root*)
           (mark-file-box-clean boxer-eval::*lexical-variables-root*))
          (t (mark-file-box-dirty boxer-eval::*lexical-variables-root*)))
    (modified boxer-eval::*lexical-variables-root*)
    boxer-eval::*novalue*))

;;;  DIRECTORY

(boxer-eval::defboxer-primitive bu::directory ((boxer-eval::dont-copy file-pattern))
  (make-vc (or (mapcar #'(lambda (p)
         (make-evrow-from-entry (intern-in-bu-package
               (namestring p))))
           (directory-internal (box-text-string file-pattern)))
         (list (make-empty-evrow)))))

(defvar *default-directory-type* "box")

(defun directory-internal (pattern-string)
  (let* ((pattern-pathname (check-valid-filename-for-primitive
          pattern-string
          (make-pathname
           :name :wild :type #-sun :unspecific #+sun *default-directory-type*
           :defaults *boxer-pathname-default*) t))
   (directory-name (make-pathname
        :directory (pathname-directory pattern-pathname)
                          ;; mac seems to need this, bare directory pathnames
                          ;; return NIL
                          #+mcl :name #+mcl ':wild)))
    (file-pattern-match?
     (make-pathname :name (pathname-name pattern-pathname)
        :type (pathname-type pattern-pathname))
     (directory directory-name))))


;; returns a list of pattern specs for file-pattern-match?
;; expects the pattern string to already have the directory
;; components stripped off
;; a pattern-spec is a CONS composed of a keyword type as the CAR
;; and a string as the CDR
(defun parse-pathname-pattern (pp)
  (values (parse-pattern-component (pathname-name pp))
    (parse-pattern-component (pathname-type pp))))

(defun parse-pattern-component (pattern)
  (let ((return-specs nil) (max-wild-place (when (stringp pattern)
               (1-& (length pattern)))))
    (cond ((or (eq pattern :wild) (eq pattern :unspecific)
         (= max-wild-place -1))
     (push (cons :wild "") return-specs))
    ((stringp pattern)
     (let ((wild-place 0))
       (loop
        (let ((new-wild-place (position #\* pattern
                :start wild-place)))
    (cond ((and (null new-wild-place) (zerop wild-place))
           ;; this means no wild cards
           (push (cons :whole pattern) return-specs)
           (return))
          ((null new-wild-place)
           ;; no more wild cards, but some have
           ;; already been processed
           (push (cons :end (subseq pattern wild-place))
           return-specs)
           (return))
          ;; at this point we know we have found a wild card
          ;; somewhere in the pattern
          ((and (zerop wild-place) (zerop new-wild-place))
           ;; the wild card has been found at the beginning
           (setq wild-place 1))
          ((zerop wild-place)
           ;; just starting but wild card has been found
           (push (cons :begin
           (subseq pattern 0 new-wild-place))
           return-specs)
           (if (= new-wild-place max-wild-place)
         (return)
         (setq wild-place (1+& new-wild-place))))
          ((= wild-place new-wild-place)
           ;; this can happen if someone type "**" in the pattern
           (if (= new-wild-place max-wild-place)
         (return)
         (setq wild-place (1+& new-wild-place))))
          (t
           (push (cons :inside
           (subseq pattern wild-place new-wild-place))
           return-specs)
           (if (= new-wild-place max-wild-place)
         (return)
         (setq wild-place (1+& new-wild-place)))))))))
    (t
     (error "Pattern spec, ~A, is not a string or :WILD" pattern)))
    (nreverse return-specs)))


;; returns the subset of file-list which matches the pattern-string
(defun file-pattern-match? (pattern file-list)
  (let ((return-list nil))
    (multiple-value-bind (name-specs type-specs)
  (parse-pathname-pattern pattern)
      (dolist (file file-list)
  (when (and (component-pattern-match? name-specs (pathname-name file))
       (component-pattern-match? type-specs (pathname-type file)))
    (push file return-list)))
      (nreverse return-list))))


(defun component-pattern-match? (pattern-specs comp)
  (let ((start 0) (pattern-match? t))
    (dolist (spec pattern-specs)
      (case (car spec)
  (:wild (return))
  (:whole (unless (string= comp (cdr spec)) (setq pattern-match? nil))
    (return))
  (:begin (let ((place (search (cdr spec) comp)))
      (cond ((and place (zerop& place))
       (setq start (length (cdr spec))))
      (t
       (setq pattern-match? nil) (return)))))
  (:end   (let ((place (search (cdr spec) comp
             :from-end t :start2 start)))
      (unless (and place
             (=& place
           (-& (length comp)
               (length (cdr spec)))))
        (setq pattern-match? nil) (return))))
  (:inside (let ((place (search (cdr spec) comp :start2 start)))
       (cond ((null place)
        (setq pattern-match? nil) (return))
       (t
        (setq start (+ (length (cdr spec)) start))))))))
    ;; if the pattern-match? flag is still true after all of
    ;; the pattern-specs have run, push the file onto the return list
    pattern-match?))


(boxer-eval::defboxer-primitive bu::delete-file ((boxer-eval::dont-copy filename))
  (let* ((filestring (box-text-string filename))
   (pathname (check-valid-filename-for-primitive
        filestring *boxer-pathname-default*)))
    (unless (probe-file pathname)
      (boxer-eval::primitive-signal-error :file-or-directory-not-found
            (namestring pathname)))
    (delete-file pathname)
    boxer-eval::*novalue*))





;;;; TEXT File Operations

;;; connect with the outside world.  These may turn out to be useful
;;; for a longer period of time than the actual boxer file operations
;;;
;;; These should eventually be rewritten to use a string buffer for
;;; read-line and write-line so they don't keep CONSing string
;;;

(boxer-eval::defboxer-primitive bu::read-text-file ((boxer-eval::dont-copy filename))
  (let* ((filestring (box-text-string filename))
   (pathname (make-pathname :directory
          (pathname-directory
           (check-valid-filename-for-primitive
            filestring *boxer-pathname-default*))
         :defaults
         (check-valid-filename-for-primitive
          filestring))))
    (unless (probe-file pathname)
      (boxer-eval::primitive-signal-error :file-or-directory-not-found
            (namestring pathname)))
    (prog1
  (read-text-file-internal pathname)
      ;; now handle file stickyness if we want to
    (unless (null *sticky-file-defaulting?*)
      ;; set the default pathname to be the current pathname ONLY after
      ;; we make sure that the current pathname has all the "important"
      ;; components, otherwise the default can become damaged in a way
      ;; that leaves it unchangeable (e.g. pathname-name becomes NIL
      ;; if the "default" was ".login")
      (if (and (not (null (pathname-name pathname)))
         (not (null (pathname-type pathname))))
    (setq *boxer-pathname-default* pathname)
    ;; otherwise, use as much of the pathname as we can to set
    ;; the default
    (setq *boxer-pathname-default*
    (make-pathname :directory (pathname-directory pathname)
             :defaults *boxer-pathname-default*)))))))

;;; a useful hack...
(boxer-eval::defboxer-primitive bu::read-text-box-shrink ((dont-copy filename))
  (boxer-eval::vpdl-push filename)
  (let ((result (bu::read-text-file)))
    (shrink result)
    result))


;; we are guaranteed that pathname is valid
(defun read-text-file-internal (pathname)
  (with-open-file (s pathname :direction ':input)
    (let ((return-box (read-text-stream-internal s)))
      (putprop return-box :text :preferred-file-format)
      return-box)))

(defun read-text-stream-internal (stream)
  (let ((first-line (read-line stream nil nil)))
    (if (null first-line)
  (make-box '(()))
  (let* ((return-box (make-box (list (make-row-from-string first-line))))
         (prev-row (first-inferior-row return-box)))
    (do ((current-line (read-line stream nil nil)
           (read-line stream nil nil)))
        ((null current-line) return-box)
      (let ((new-row (make-row-from-string current-line)))
        (insert-row-after-row return-box new-row prev-row)
        (setq prev-row new-row)))))))


;;; ':error means blow out, nil means do nothing (insert "[]")
;;; perhaps one day we should have ':interpolate

(defvar *box-encountered-during-text-file-operation* :interpolate)


(boxer-eval::defboxer-primitive bu::write-text-file ((bu::port-to box)
           (boxer-eval::dont-copy filename))
  box filename ;; silence bound but not used compiler warnings
  (boxer-eval::primitive-signal-error
   :obsolete-function
   "WRITE-TEXT-FILE is an obsolete function, use SAVE-TEXT-FILE instead"))

(boxer-eval::defboxer-primitive bu::save-text-file ((bu::port-to box)
           (boxer-eval::dont-copy filename))
  (process-editor-mutation-queue-within-eval)
  (save-text-file-internal (box-or-port-target box) (box-text-string filename)))

(defun save-text-file-internal (box filestring)
  (let* ((tmp-file-string (symbol-name (gensym))) ;should do something better
   (dest-pathname (make-pathname :directory
               (pathname-directory
          (check-valid-filename-for-primitive
           filestring *boxer-pathname-default*))
               :defaults
               (check-valid-filename-for-primitive
          filestring)))
   (tmp-dest (merge-pathnames tmp-file-string dest-pathname)))
    ;; make sure we have an editor object
    (cond ((box? box))
          ((virtual-copy? box)
           (setq box (make-editor-box-from-vc box)))
          (t (boxer-eval::primitive-signal-error :file-error "You can only save boxes")))
    ;; actually dump the box
    (write-text-file-internal box tmp-dest)
    ;; now check to see if we need to make a backup
    (make-backup-if-necessary dest-pathname)
    ;; now move the freshly save file from the tmp-name to the dest name
    (rename-file tmp-dest dest-pathname)
    ;; now handle file name stickyness if we have to
    (unless (null *sticky-file-defaulting?*)
      ;; set the default pathname to be the current pathname ONLY after
      ;; we make sure that the current pathname has all the "important"
      ;; components, otherwise the default can become damaged in a way
      ;; that leaves it unchangeable (e.g. pathname-name becomes NIL
      ;; if the "default" was ".login")
      (if (and (not (null (pathname-name dest-pathname)))
         (not (null (pathname-type dest-pathname))))
    (setq *boxer-pathname-default* dest-pathname)
    ;; otherwise, use as much of the dest-pathname as we can to set
    ;; the default
    (setq *boxer-pathname-default*
    (make-pathname :directory (pathname-directory dest-pathname)
             :defaults *boxer-pathname-default*))))
    boxer-eval::*novalue*))

(defun write-text-file-internal (box pathname)
  (with-open-file (fs pathname :direction ':output)
    (write-text-stream-internal box fs (namestring pathname))))

;;; Warn Option should be at Boxer User level instead of Lisp level
(defun write-text-stream-internal (box fs &optional pathname-string)
  (do-box-rows ((row box))
    (multiple-value-bind (string box-p)
        (text-string row)
      (if (null box-p)
          (write-line string fs)
        (case *box-encountered-during-text-file-operation*
          (:interpolate (write-text-stream-for-boxy-row row fs))
          (:warn (warn "Box encountered during text operation, writing \"[]\"")
           (write-line string fs))
          (:error (boxer-eval::primitive-signal-error
                   :box-encountered-while-writing-text-file
                   pathname-string))
          (t (write-line string fs)))))))

(defun write-text-stream-for-boxy-row (row fs)
  (do-row-chas ((cha row))
    (cond ((box? cha)
           (terpri fs)
           (write-char #\[ fs)
           (write-text-stream-internal cha fs)
           (write-char #\] fs)
           (terpri fs))
          (t (write-char cha fs))))
  (terpri fs))




;;; all of this is being replaced by the box server
;;; leave here for possible inclusion in configurations
;;; where the server is not resident

(boxer-eval::defboxer-primitive bu::com-enter-box ()
  (com-enter-box))



;; maybe we should put some sort of reality checking into this ?
(boxer-eval::defboxer-primitive bu::set-current-directory ((boxer-eval::dont-copy directory))
  (flet ((directory? (pathname)
     (and (probe-file pathname)
    (string-equal (directory-namestring pathname)
            (namestring pathname)))))
    (let* ((raw-pathname (check-valid-filename-for-primitive
        (box-text-string directory)))
     (dirs (pathname-directory raw-pathname))
     (name (pathname-name raw-pathname))
     (pathname (if (null name) raw-pathname
       (make-pathname :directory
          (append dirs (list name))))))
      (if (directory? pathname)
    (setq *boxer-pathname-default*
    (make-pathname :directory (pathname-directory pathname)
             :defaults *boxer-pathname-default*))
    (boxer-eval::primitive-signal-error :not-a-directory
          (namestring pathname)))))
  boxer-eval::*novalue*)


(boxer-eval::defboxer-primitive bu::current-directory ()
  (make-vc (list (make-evrow-from-entry
      (intern-in-bu-package
       (let ((string (namestring *boxer-pathname-default*)))
         (subseq string 0
           (search (pathname-name *boxer-pathname-default*)
             string :from-end t))))))))

(boxer-eval::defboxer-primitive bu::default-directory ()
  (boxer-eval::primitive-signal-error
   :obsolete-function
   "DEFAULT-DIRECTORY is an obsolete function, use CURRENT-DIRECTORY instead"))



;;;

(defmethod file-box? ((box box))
  (getf (slot-value box 'plist) :associated-file))

(defmethod mark-box-as-file ((box box) &optional filename)
  (setf (storage-chunk? box) t)
  (unless (null filename) (putprop box filename :associated-file))
  ;; make sure other storage chunking properties are removed
  (when (getprop box :url) (removeprop box :url))
  (when (null (getprop box :boxtop)) (putprop box :file :boxtop))
  ;; clear the modified flag
  (set-border-style box (if (not (null (slot-value box 'exports)))
                            :thick-dashed
                            :thick)))

(defmethod unmark-box-as-file ((box box))
  (setf (storage-chunk? box) nil)
  (let ((filename (getprop box :associated-file)))
    ;; save away the filename in case we change our mine and
    ;; want to make it a file box again
    (unless (null filename) (putprop box filename :old-associated-file))
    (removeprop box :associated-file)
    (when (eq (getprop box :boxtop) :file) (removeprop box :boxtop))
    (set-border-style box (if (not (null (slot-value box 'exports)))
                            :dashed
                            nil))))

(defun current-file-box (&optional (where (if (box? boxer-eval::*lexical-variables-root*)
                                            boxer-eval::*lexical-variables-root*
                                            (point-box))
                                          #|(outermost-box)|#))
  (cond ((not (box? where)) nil)
        ((eq where *initial-box*) where)
        ((storage-chunk? where) where)
        (t (let ((sup (superior-box where)))
             (when (box? sup) (current-file-box sup))))))

(defvar *max-file-pathname-length* 20)

(defun pretty-file-box-name (filename)
  (let* ((name (pathname-name filename))
         (type (pathname-type filename))
         (dirs (pathname-directory filename))
         (rawhost (pathname-host filename))
         (prettyhost (if (null rawhost) "" (format nil "~A:" rawhost)))
         (name-length (+ (length name) (if (typep type 'string)
                                         (length type)
                                         0)))
         (ellipsis "")
         (included-dirs (with-collection
                          (when (listp dirs) ; might be :unspecific
                            (dolist (d (cdr dirs))
                              (incf name-length (length d))
                              (if (> name-length
                                     *max-file-pathname-length*)
                                (progn (setq ellipsis
                                             #+mcl "...:" #-mcl ".../")
                                       (return))
                                (collect d)))))))
    (cond ((and (typep type 'string) (not (null included-dirs)))
           (format nil
                   #+mcl "~A~{~A:~}~A~A.~A"
                   #-mcl "~A~{~A/~}~A~A.~A"
                   prettyhost  included-dirs ellipsis name type))
          ((typep type 'string)
           (format nil "~A.~A" name type))
          ((not (null included-dirs))
           (format nil #+mcl "~A {~{~A:~}~A}" #-mcl "~A {~{~A/~}~A}"
                   name included-dirs ellipsis))
          (t ; no type, no dirs
           (format nil "~A" name)))))

;; placeholder, should put in trimming to *max-file-pathname-length*
(defun pretty-url-box-name (url)
  (slot-value url 'boxnet::Scheme-string))


(defun current-file-box-name (&optional (where (point-box) #|(outermost-box)|#))
  (let ((cfb (current-file-box where)))
    (cond ((null cfb))
          (t (let ((file (getprop cfb :associated-file)))
               (cond ((null file) "")
                     (t (values (pretty-file-box-name file) file))))))))

(defvar *terse-file-status* t)

(defvar *initial-file-status-string-length* 70)

(defvar *file-status-string* (make-array *initial-file-status-string-length*
                                         :element-type 'character
                                         :fill-pointer 0))

(defun add-string-to-file-status (string)
  (let ((l (length string))
        (max (-& (array-total-size *file-status-string*) 3))) ; room for "..."
    (dotimes (i l l)
      (if (=& (fill-pointer *file-status-string*) max)
          (dotimes (i 3 (return t)) (vector-push #\. *file-status-string*))
          (vector-push (char string i) *file-status-string*)))))

(defun clear-file-status-string () (setf (fill-pointer *file-status-string*) 0))

; get box file props
;; returns (values origin-type file-format read-only?)
(defun get-box-file-props (filebox)
  (let* ((file-format (getprop filebox :preferred-file-format))
         (url (getprop filebox :url)))
    (values (if (not (null url))
                (boxnet::file-status-line-string url)
                :disk)
            (or file-format ':box)
            (read-only-box? filebox)
            (file-modified? filebox))))


(defun current-file-status (box &optional vanilla?)
  (let ((filebox (current-file-box box)))
    (if (not (box? filebox))
      "??????"
  (let* ((raw-name (slot-value filebox 'name))
         (box-name (cond ((stringp raw-name) raw-name)
                         ((null raw-name) "No Name")
                         ((name-row? raw-name) (text-string raw-name))
                         (t "???")))
         (url (getprop filebox :url))
         (pathname (or (getprop filebox :associated-file)
                       (when (and url (typep url 'boxnet::local-url))
                         (boxnet::local-url-pathname url)))))
    (multiple-value-bind (origin-type file-format read-only? fmodified?)
        (get-box-file-props filebox)
#| ;; this is now handled (as it should be) in the menu-update
      #+mcl ;; dynamically adjust the file menu...
      (let ((save-item (find "Save" (slot-value *boxer-file-menu* 'ccl::item-list)
                             :test #'(lambda (a b)
                                       (string-equal a (ccl::menu-item-title b)))))
            ;(save-box-as-item (find "Save Box As..." (slot-value *boxer-file-menu*
            ;                                                     'ccl::item-list)
            ;                        :test #'(lambda (a b)
            ;                                  (string-equal
            ;                                   a (ccl::menu-item-title b)))))
            )
        ;; grey out File menu items if they are redundant or not applicable
        ;; "Save" is not applicable if there is not an existing filename
        ;; or if the file box has not been modified...
        (if (or read-only? (null pathname) (not (file-modified? filebox)))
            (ccl::menu-item-disable save-item)
            (ccl::menu-item-enable  save-item))
        ;; "Save Box As..." is redundant with "Save As"if the
        ;; cursor is in the same box as the document box
;        (if (eq filebox (point-box))
;            (ccl::menu-item-disable save-box-as-item)
;            (ccl::menu-item-enable  save-box-as-item))
        )
|#
      ;; print into the *file-status-string*
      (clear-file-status-string)
      (catch 'status-string-end
        (flet ((add-string (string)
                 (let ((result (add-string-to-file-status string)))
                   (when (eq result t) (throw 'status-string-end nil)))))
          ;; first (maybe) indicate file modified  status
          (when (and (not vanilla?) fmodified?)
            (add-string #+mcl "� " #-mcl "* "))
          ;;; print +mcl the box name...
          (add-string "File Box: ")
          (add-string box-name)
          (add-string  #+mcl " � " #-mcl " | ")
          ;; now add info about the where the box came from...
          (add-string "From: ")
          (unless (and (null pathname) (not (eq origin-type :network)))
            ;; leave the where field blank if we dont have a name...
            (let ((adjectives nil))
              (when (and read-only? (not (eq origin-type :network)))
                (add-string (if *terse-file-status* "(RO) " "Read Only "))
                (setq adjectives t))
              (case origin-type
                (:network (if *terse-file-status*
                              (add-string "(net) ")
                              (add-string "Network "))
                          (setq adjectives t))
                (:disk (when *terse-file-status* (add-string "(disk) "))))
              (unless (eq file-format :box)
                (add-string (string-capitalize file-format))
                (add-string " ")
                (setq adjectives t))
              (when (and adjectives (not *terse-file-status*))(add-string "file "))
              (cond ((not (null pathname))
                     (add-string (pathname-name pathname))
                     (let ((type (pathname-type pathname)))
                       (unless (or (null type) (eq type :unspecific))
                         (add-string ".") (add-string type)))
                     (add-string " ")
                     (unless *terse-file-status*
                       ;; skip directory if terse but print something...
                       (let ((raw-dirs (pathname-directory pathname)))
                         (add-string "{")
                         (when (listp raw-dirs) ; could be :unspecific
                           (dolist (dir (cdr raw-dirs))
                             (add-string dir)
                             (add-string #+mcl ":" #-mcl "/")))
                         (add-string "}"))))
                    ((eq origin-type :network)
                     (add-string (boxnet::scheme-string url))))))))
      *file-status-string*)))))

;; walk up (backwards up the list) the path looking for a match in the
;; superior dirs
;; NOTE: not quite right, we should modify the sup-pathname as well to prune lower level
;;       directories which do not match
(defun dmerge-dirs (path sup-path)
  (let ((merged-dirs nil)
        (sup-dirs (cdr (pathname-directory sup-path))))
    (do* ((dirs (reverse (cdr (pathname-directory path))) (cdr dirs))
          (dir (car dirs) (car dirs)))
         ((null dir) nil)
      (if (member dir sup-dirs :test #'string-equal)
          (return (append '(:relative) merged-dirs))
          (push dir merged-dirs)))))

;; see if the file exists.
;; when not being strict check for ".box" or no "".box"
(defun probe-search-file (pathname)
  (cond ((null *strict-box-paths*)
         (or (probe-file pathname)
             (probe-file (make-pathname :type
                                        (let ((current-type (pathname-type pathname)))
                                          (cond ((or (null current-type)
                                                     (eq current-type :unspecific))
                                                 "box")
                                                ((string-equal current-type "box")
                                                 :unspecific)
                                                (t current-type)))
                                        :defaults pathname))))
        (t
         (probe-file pathname))))

;; need to hack relative pathnames here...
(defun fill-box-from-local-file (box)
  (catch 'cancel-boxer-file-dialog
    (let* ((raw-pathname (getprop box :associated-file))
           (raw-dirs (and raw-pathname (pathname-directory raw-pathname)))
           (sup-box (superior-box box))
           (sup-file-box (current-file-box sup-box))
           (sup-pathname (getprop sup-file-box :associated-file))
           (pathname (cond ((and (eq (car raw-dirs) :absolute)
                                 (probe-search-file raw-pathname)))
                           ((and (or (eq (car raw-dirs) :relative)
                                     ;; if the relative path is just a name,
                                     ;; raw-dirs will be null
                                     (null raw-dirs))
                                 (or *autoloading-namestring* sup-pathname)
                                 (let ((merged-pathname (merge-pathnames
                                                         (make-pathname
                                                          :directory raw-dirs
                                                          :name (pathname-name
                                                                 raw-pathname)
                                                          :type (or (pathname-type
                                                                     raw-pathname)
                                                                    :unspecific))
                                                         (or *autoloading-namestring*
                                                             sup-pathname))))
                                   (probe-search-file merged-pathname))))
                           ;; now try various simple guesses
                           ((let ((merged-pathname (unless (null sup-pathname)
                                                    (merge-pathnames
                                                     (make-pathname
                                                      :name (pathname-name
                                                             raw-pathname)
                                                      :type (or (pathname-type
                                                                 raw-pathname)
                                                                :unspecific))
                                                     sup-pathname))))
                              (and merged-pathname
                                   (probe-search-file merged-pathname))))
                           ((let ((autoload-pathname
                                  (unless (null *autoloading-namestring*)
                                    (merge-pathnames
                                     (make-pathname :name (pathname-name
                                                           raw-pathname)
                                                    :type (or (pathname-type
                                                               raw-pathname)
                                                              :unspecific))
                                     *autoloading-namestring*))))
                              (and autoload-pathname
                                   (probe-search-file autoload-pathname))))
                           ;; directory merging guesses...
                           ((let ((dmerged-pathname (unless (null sup-pathname)
                                                      (merge-pathnames
                                                       (make-pathname
                                                        :directory
                                                        (dmerge-dirs raw-pathname
                                                                     sup-pathname)
                                                        :name (pathname-name
                                                               raw-pathname)
                                                        :type (pathname-type
                                                               raw-pathname))
                                                       sup-pathname))))
                              (and dmerged-pathname
                                   (probe-search-file dmerged-pathname))))
                           ((let ((admerged-pathname (unless (null *autoloading-namestring*)
                                                       (merge-pathnames
                                                        (make-pathname
                                                         :directory
                                                         (dmerge-dirs raw-pathname
                                                                      *autoloading-namestring*)
                                                         :name (pathname-name
                                                                raw-pathname)
                                                         :type (or (pathname-type
                                                                    raw-pathname)
                                                                   :unspecific))
                                                        *autoloading-namestring*))))
                              (and admerged-pathname
                                   (probe-search-file admerged-pathname))))
                           ;; guesses are all wrong, so ask the user
                           ((and *prompt-for-server-file-not-found*
                                 (not (null *autoloading-namestring*)))
                              (boxer-open-lost-file-dialog
                               :prompt "(While Autoloading) Can't find"
                               :directory raw-pathname))
                           (*prompt-for-server-file-not-found*
                            (boxer-open-lost-file-dialog
                             :prompt "Can't find"
                             :directory raw-pathname))
                           ;; nothing works, return the original pathname for error message
                           (t raw-pathname)))
           ;; allow an out if cached pathname is not valid
           (filebox (read-internal-1 pathname))
           (old-ds (display-style box)))
      (initialize-box-from-box box filebox)
      ;; the vanilla init will take the display style from the file box
      ;; if we are autoloading, we want to use the file's display style
      (unless (autoload-file? box)
        (case old-ds
          (:shrunk (shrink box))
          (:supershrunk (boxer::supershrink box))))
      ;; if the box is transparent, export it's bindings
      (unless (null (exports box))
        ;; this is the relevant guts of boxer-eval::set-exports without the checks
        ;; and setting of the exports slots (which are already set from the file)
        ;; will break if we ever use a selective exporting mechanism which
        ;; doesn't look too probably at the moment
        (let ((superior-box (superior-box box)))
          (unless (null superior-box)
            (boxer-eval::propagate-all-exported-bindings box superior-box)
            (boxer-eval::export-inferior-properties box superior-box))))
      ;; if there is a cached boxtop, remove it in favor of the one being
      ;; read in from the file
      (let ((cb (getprop box :cached-boxtop)))
        (unless (null cb)
          (when (graphics-sheet? cb)
            (let ((bm (graphics-sheet-bit-array cb)))
              (unless (null bm) (ogl-free-pixmap bm))))
          (removeprop box :cached-boxtop)))
      ;; this the old style boxtop caching.... (convert to new style)
      (let ((bp (getprop box :boxtop)))
        (cond ((graphics-sheet? bp)
               (putprop box :standard :boxtop))
              ((null bp)
               ;; add :file boxtop here when converting from old style.  THis
               ;; seems more economical than hairing up dump or load
               (putprop box :file :boxtop))))
      ;; now do the box/file bookkeeping,
      ;; NOTE: It has to be here AFTER the initialize-box-from-box
      (when (box? box)
        ;(boxnet::read-box-postamble box) ;(part of the Boxer Server) no longer used
        (mark-box-as-file box pathname)
        (if (and (not (equal pathname raw-pathname))
                 ;; don't mark dirty if we just articulated a relative pathname
                 ;; unless the directory path has actually changed
                 (not (and (or (null (pathname-directory raw-pathname))
                               (eq (pathname-directory raw-pathname) :relative))
                           (subsetp (cdr (pathname-directory raw-pathname))
                                    (cdr (pathname-directory pathname))))))
          (mark-file-box-dirty box)
          (mark-file-box-clean box)))
      ;; record in place menu ?
         ; removed for UC "clean" implementation
      ;; keep track of the file properties so that we can check
      (record-boxer-file-properties pathname
                                    (file-write-date pathname)
                                    (file-author pathname) box)
      box)))




;;; In case redisplay breaks,. people can still do...
(defun salvage-boxer-world (&optional (save-name "salvaged-world") (verbose t))
  (let ((world-save-name (merge-pathnames save-name *boxer-pathname-default*)))
    ;; first make sure the name is ok
    (loop (when (yes-or-no-p "~%Save Your Boxer World in ~A ? "
           world-save-name)
      ;; we are happy with the name, then save it away
      ;; otherwise, prompt for a new name
      (format t "~%New File Name to Save in: ")
      (setq world-save-name
      (merge-pathnames (read-line) world-save-name))))
    ;; then save it away verbosely
    (when verbose (format t "~%Dumping boxer world to ~A" world-save-name))
    (dump-top-level-box *initial-box* world-save-name)
    ;; now verify
    (when verbose (format t "~%Verifying Saved file"))
    (load-binary-box-internal world-save-name)
    (format t "~%Boxer World Saved to ~A" world-save-name)
    (values)))

;;; spread it around
(import 'salvage-boxer-world (find-package 'user))
(import 'salvage-boxer-world (find-package :boxer-window))
(import 'salvage-boxer-world (find-package :boxer-eval))
;; might as well be paranoid...
(import 'salvage-boxer-world (find-package :boxer-user))



;;;
(boxer-eval::defboxer-primitive bu::storage-name ((bu::port-to box))
  (let ((target (box-or-port-target box)))
    (cond ((box? target)
           (let ((file (getprop target :associated-file))
                 (url  (getprop target :url))
                 (xref (getprop target :xref)))
             (cond ((not (null file))
                    (make-vc (list (make-evrow-from-string
                                    (namestring file)))))
                   ((not (null url))
                    (make-vc (list (make-evrow-from-string
                                    (boxnet::urlstring url)))))
                   ((not (null xref))
                    (let ((path #+mcl (mac-file-ref-pathname xref)
                                #-mcl nil))
                      (cond ((null path) (make-empty-vc))
                            (t (make-vc (list (make-evrow-from-string
                                               (namestring path))))))))
                   (t (make-empty-vc)))))
          (t (make-empty-vc)))))

(boxer-eval::defboxer-primitive bu::storage-info ((bu::port-to box))
  (let ((target (box-or-port-target box)))
    (cond ((box? target)
           (let ((file (getprop target :associated-file))
                 (url  (getprop target :url))
                 (xref (getprop target :xref)))
             (cond ((not (null file))
                    (make-file-storage-info-box file))
                   ((not (null url))
                    (make-url-storage-info-box url))
                   ((not (null xref))
                    (let ((path #+mcl (mac-file-ref-pathname xref)
                                #-mcl nil))
                      (cond ((null path) (make-empty-vc))
                            (t (make-file-storage-info-box path)))))
                   (t (make-empty-vc)))))
          (t (make-empty-vc)))))

(defun destructure-pathname (pathname)
  (values (pathname-host pathname)
          (pathname-device pathname)
          (pathname-directory pathname)
          (pathname-name pathname)
          (pathname-type pathname)))

;; consider making empty boxes for null fields instead of omitting entirely
;; 2/1/03 We should have empty boxes since people will be using TELL to
;; grab fields and we shoudl have something for the names to avoid errors
(defun make-file-storage-info-box (pathname)
  (multiple-value-bind (host device directory name type)
      (destructure-pathname pathname)
    (make-file-storage-info-box-internal host device
                                         (car directory) (cdr directory)
                                         name type)))

(defun make-file-storage-info-box-internal (host device dir-type dir name type)
    (make-vc (list (make-vc (unless (or (null host) (eq host :unspecific))
                              (list (make-evrow-from-string host)))
                            'data-box 'bu::Host)
                   (make-vc (unless (or (null device) (eq device :unspecific))
                              (list (make-evrow-from-string device)))
                            'data-box 'bu::Device)
                   (make-vc (list (make-evrow-from-string dir-type))
                            'data-box 'bu::directory-type)
                   (make-vc (mapcar #'(lambda (d)
                                        (make-evrow-from-string d))
                                    dir)
                            'data-box 'bu::directory)
                   (make-vc (unless (null name)
                              (list (make-evrow-from-string name)))
                            'data-box 'bu::name)
                   (make-vc (unless (or (null type) (eq type :unspecific))
                              (list (make-evrow-from-string type)))
                            'data-box 'bu::type))
             'data-box))

(defun make-url-storage-info-box (url)
  (make-vc (let ((url-info-sub-boxes nil))
             (do* ((value-pairs (boxnet::url-values url) (cddr value-pairs))
                   (name  (car value-pairs)  (car value-pairs))
                   (value (cadr value-pairs) (cadr value-pairs)))
                  ((null value-pairs))
               (push (make-vc (list (make-evrow-from-string value))
                              'data-box (intern-in-bu-package
                                         (string-upcase name)))
                     url-info-sub-boxes))
             (nreverse url-info-sub-boxes))))

;; like destructure-pathname, returns (values host device directory name type)
(defun destructure-file-info-box (file-info-box)
  (let* ((box      (box-or-port-target file-info-box))
         (host     (lookup-variable-in-box-only box 'bu::host))
         (device   (lookup-variable-in-box-only box 'bu::device))
         (dir-type (lookup-variable-in-box-only box 'bu::directory-type))
         (dir      (lookup-variable-in-box-only box 'bu::directory))
         (name     (lookup-variable-in-box-only box 'bu::name))
         (type     (lookup-variable-in-box-only box 'bu::type)))
    (values (if (empty-box? host)   :unspecific (box-text-string host))
            (if (empty-box? device) :unspecific (box-text-string device))
            (cond ((string-equal (box-text-string dir-type) "RELATIVE")
                   :relative)
                  (t :absolute))
            (with-collection
              (dolist (er (get-box-rows dir))
                (collect (evrow-text-string er dir))))
            (box-text-string name)
            (if (empty-box? type)   :unspecific (box-text-string type)))))

(boxer-eval::defboxer-primitive bu::file-info-to-file (file-info)
  (multiple-value-bind (host device dir-type dir name type)
      (destructure-file-info-box file-info)
    (make-vc (list (make-evrow-from-string
                    (namestring (make-pathname :host host
                                               :device device
                                               :directory (list* dir-type dir)
                                               :name name
                                               :type type))))
             'data-box)))

(boxer-eval::defboxer-primitive bu::file-to-file-info (filename)
  (let ((pathname-string (box-text-string filename)))
    (make-file-storage-info-box pathname-string)))

(boxer-eval::defboxer-primitive bu::merge-file-info (info1 info2)
  (multiple-value-bind (host1 dev1 dir-type1 dir1 name1 type1)
      (destructure-file-info-box info1)
    (multiple-value-bind (host2 dev2 dir-type2 dir2 name2 type2)
        (destructure-file-info-box info2)
      (make-file-storage-info-box-internal
       (if (or (null host1) (eq host1 :unspecific)) host2 host1)
       (if (or (null dev1)  (eq dev1  :unspecific)) dev2  dev1)
       (if (or (eq dir-type1 :absolute) (eq dir-type2 :absolute))
         :absolute :relative)
       (cond ((eq dir-type1 :absolute) dir1)
             ((eq dir-type2 :absolute) (append dir2 dir1))
             (t (append dir2 dir1)))
       (if (null name1) name2 name1)
       (if (or (null type1) (eq type1 :unspecific)) type2 type1)))))
