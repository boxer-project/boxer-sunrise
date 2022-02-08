;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER; Base:8. -*-

#|


 $Header: comse.lisp,v 1.0 90/01/24 22:09:04 boxer Exp $


 $Log:	comse.lisp,v $
;;;Revision 1.0  90/01/24  22:09:04  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


  This file contains Editor Commands which are useful in writing and Running
  BOXER Code.

Evaluation:
  COM-DOIT COM-DOIT-NOW
Closets:
  COM-OPEN-CLOSETS COM-CLOSE-CLOSETS
Prompting:
  COM-PROMPT
Stepping:
  COM-STEP-NOW


Modification History (most recent at the top)

 1/17/07 #-opengl(add-redisplay-clue.... in doit-print-returned-value, com-prompt
         com-insert-key-name
 4/21/03 merged current LW and MCL files
 1/05/02 bind font to default values around construction of help boxes
         in com-document-kep and com-help
         added hook, *help-font-descriptor* to customize this...
 2/13/01 merged current LW and MCL files.  Also commented out
         COM-DOIT because it is unused and calls nonexistent com-mark-row
         added "RO" to ignore declares in make-file-box
 4/07/00 added clear-editor-message to com-link-to-mac-file
10/29/99 doit-print-returned-value binds *current-font-descriptor* to
         *default-font-descriptor* so the returned value isn't influenced
         by the font of the line being eval'd
 6/04/99 box-file? changed to handle files shorter than 4 bytes
 5/19/99 box-file? now peeks at 1st 4 bytes of file
 4/14/99 make-file-box


|#

(in-package :boxer)





;;; Interaction between the editor and the programming environment
;;; Utilities for Prompting, Documentation, Help among other things


;;; evaluation

#| ;; com-mark-row no longer exists and this is unused...
(defboxer-command com-doit ()
  "calls the evaluator on the
current region.  If there is no
current region, marks the current
row instead. "
  (let ((region (or *region-being-defined* (get-current-region))))
    (cond ((not-null region)
     (doit-internal))
    (t
     (com-mark-row))))
  boxer-eval::*novalue*)
|#

(defboxer-command com-doit-now ()
  "calls the evaluator on the current row"
  (doit-internal)
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defboxer-command com-recursive-doit ()
  "Calls the evaluator on the current row during a recusive edit"
  (setq boxer-eval::*recursive-doit?* t)
  (throw 'boxer-editor-top-level (eval-objs (point-row))))

(defboxer-command com-exit-edit-box ()
  "Finish a call to the EDIT-BOX function"
  (exit-edit-box-internal)
  boxer-eval::*novalue*)


#|
(defvar *inside-doit-crock* nil)
(defun doit-internal ()
  (if *inside-doit-crock*
      (boxer-editor-error "Already inside COM-DOIT")
      (let ((*inside-doit-crock* t))
  (unwind-protect
       (unless (name-row? (point-row))
         (top-level-eval-wrapper
    (doit-print-returned-value (boxer-eval::eval-point-row))))
    (reset-editor-numeric-arg)))))
|#

;;; Look for a | returned-value comment.  If there is one, delete the text
;;; to the right of it, and the | too, if the value is not to be printed.
;;; If there is no | and the value is to be printed, then make one.
;;; If the value is to be printed, print it.
(defun doit-print-returned-value (returned-value)
  (let* ((boxer-eval::*primitive-shadow-warning-on?* nil) ; don't warn while printing
         (*current-font-descriptor* *default-font-descriptor*)
   (processed-returned-value
    (cond ((eq returned-value boxer-eval::*novalue*) nil)
    (t (cond ((numberp returned-value)
        (top-level-print-number returned-value))
       ((virtual-copy? returned-value)
        (top-level-print-vc returned-value))
       ((virtual-port? returned-value)
        (top-level-print-vp returned-value))
       ;((fast-memq returned-value '(bu::true bu::false))
       ; (make-box `((,returned-value))))
       ((data-box? returned-value)
         ;Error Boxes and READ
        returned-value)
       ((doit-box? returned-value)
         ;Stepper Boxes
        returned-value)
       ((port-box? returned-value)
        (port-to-internal (ports returned-value)))
                         ((typep returned-value 'foreign-data)
                          (make-editor-box-from-foreign-data returned-value))
       (t (format nil "Weird object: ~a" returned-value))))))
   (bp (make-bp ':moving))
   (row (bp-row *point*))
   (row-chas (chas-array row))
   (existing-vertical-bar-cha-no
     (position #\| (chas-array-chas row-chas) :test #'equal
         :end (chas-array-active-length row-chas))))
    (unwind-protect
   (progn
     (cond ((not-null existing-vertical-bar-cha-no)
      (dolist (bp (bps row))
        (setf (bp-cha-no bp)
        (min existing-vertical-bar-cha-no
             (bp-cha-no bp))))
      (move-bp-1 bp row
           (+ existing-vertical-bar-cha-no
        (if (not (null processed-returned-value))
            1
            0)))
      (queue-editor-objs-for-deallocation
                   (delete-chas-to-end-of-row bp ':fixed)))
     ((not (null processed-returned-value))
      (move-bp bp (row-last-bp-values row))
      ;; flush trailing spaces (but not if there's
      ;; an old value there)
      (do ((i (1-& (length-in-chas row)) (1-& i)))
          ((not (equal (cha-at-cha-no row i) #\Space)))
        (delete-cha-at-cha-no row i))
      (insert-row-chas bp (make-row '("   |")) :moving)))
     (when (not (null processed-returned-value))
       #-opengl (add-redisplay-clue (bp-row bp) ':insert)
       (insert-row-chas bp (make-row `(,processed-returned-value)))))
      ;; make sure the bp used in printing gets deallocated
      (deallocate-bp bp))))



;;;; Closets

(defboxer-command com-open-closets ()
  "Open the Closet Row For Editing"
  (reset-region)
  (multiple-value-bind (closet-row new?)
      (closet-row (box-point-is-in))
    ;; There is now a closet-row to edit so we need to
    ;; splice it into the row structure.
    ;; Closet rows should aways appear at the top of the box
    ;; If we are scrolled, then we need to splice it in somewhere in
    ;; the middle, otherwise, the first row is just fine
    (cond ((row-row-no (box-point-is-in) closet-row)) ; the row is already there
          ((null (scroll-to-actual-row (screen-box-point-is-in)))
           (insert-row-at-row-no (box-point-is-in)
                                 closet-row
                                 0
                                 (not new?))
           (modified (box-point-is-in))
           ;; now put the point in the closet
           (move-point (row-first-bp-values closet-row)))
          (t
           ;; we must be partially scrolled, so insert the
           ;; closet-row as the 1st VISISBLE row
           (insert-row-at-row-no (box-point-is-in)
                                 closet-row
                                 (row-row-no (box-point-is-in)
                                             (scroll-to-actual-row
                                              (screen-box-point-is-in)))
                                 (not new?))
           (set-scroll-to-actual-row (screen-box-point-is-in)
                                     closet-row)
           (modified (box-point-is-in)))))
  boxer-eval::*novalue*)

(defboxer-command com-close-closets ()
  "Close any closets that exist"
  (reset-region)
  (let ((closet-row (slot-value (box-point-is-in) 'closets)))
    (flet ((in-closet-row? ()
       (do ((row (point-row) (and (superior-box row)
          (superior-row (superior-box row)))))
     ((null row) nil)
         (when (eq row closet-row)
     (return t)))))
      (cond ((null closet-row))
      ((fast-memq closet-row (rows (box-point-is-in)))
       (let ((cr-no (row-row-no (box-point-is-in) closet-row))
       (dest-row (or (next-row closet-row)
         (previous-row closet-row))))
         (cond ((null dest-row)
          ;; we're going to remove the only row in the box
          ;; so we better put one back
          (let ((new-row (make-row '())))
      (append-row (box-point-is-in) new-row)
      (delete-row-at-row-no (box-point-is-in) cr-no)
      (when (in-closet-row?)
        (move-point (row-first-bp-values new-row)))
      (unless (null (scroll-to-actual-row
               (screen-box-point-is-in)))
        (set-scroll-to-actual-row
          (screen-box-point-is-in) nil))))
         (t
          (delete-row-at-row-no (box-point-is-in) cr-no)
          (when (in-closet-row?)
          (move-point (row-first-bp-values dest-row)))
          (unless (null (scroll-to-actual-row
             (screen-box-point-is-in)))
      (set-scroll-to-actual-row
        (screen-box-point-is-in) dest-row)))))
       (modified (box-point-is-in))))))
  boxer-eval::*novalue*)


;;; NOTE: The redisplay problem is fixed in a crockish way, this should be
;;; re-examined when the allocation buyg in the redisplay is fixed (you know,
;;; the one where screen-boxes are NEVER deallocated...)

(defboxer-command com-toggle-closets (&optional (box (box-point-is-in))
                                                (screen-box
                                                 (screen-box-point-is-in)))
  "Open the closet if it is closed and
   close the closet if it is open."
  (reset-region)
  (let ((closet-row (slot-value box 'closets)))
    (flet ((in-closet-row? ()
       (do ((row (point-row) (and (superior-box row)
          (superior-row (superior-box row)))))
     ((null row) nil)
         (when (eq row closet-row) (return t)))))
      (cond ((or (null closet-row)
                 (not (row-row-no box closet-row)))
             ;; there is no open closet so open it...
             (multiple-value-bind (closet-row new?)
                      (closet-row box)
               ;; There is now a closet-row to edit so we need to
               ;; splice it into the row structure.
               (when (graphics-screen-box? screen-box)
                 ;; first make sure we are in a text mode....
                 (com-toggle-box-view (screen-obj-actual-obj screen-box)))
               ;; Closet rows should aways appear at the top of the box
               ;; If we are scrolled, then we need to splice it in somewhere
               ;; in the middle, otherwise, the first row is just fine
               (cond
                ((null (scroll-to-actual-row screen-box))
                 (insert-row-at-row-no box closet-row 0 (not new?))
                 (modified box)
                 ;; now put the point in the closet, If there is only one box
                 (when (eq box (box-point-is-in))
                   (move-point (row-first-bp-values closet-row))))
                (t
                 ;; we must be partially scrolled, so insert the
                 ;; closet-row as the 1st VISISBLE row
                 (insert-row-at-row-no box closet-row
                                       (row-row-no
                                        box (scroll-to-actual-row screen-box))
                                       (not new?))
                 (set-scroll-to-actual-row screen-box closet-row)
                 (modified box)))))
            ((row-row-no box closet-row)
             ;; the closet row must be visible so we should remove it
             ;; moving the *point* out of the way if we have to...
             (let ((cr-no (row-row-no box closet-row))
                   (dest-row (or (next-row closet-row)
                                 (previous-row closet-row))))
               (cond ((null dest-row)
                      ;; we're going to remove the only row in the box
                      ;; so we better put one back
                      (let ((new-row (make-row '())))
                        (append-row box new-row)
                        (delete-row-at-row-no box cr-no)
                        (when (in-closet-row?)
                          (move-point (row-first-bp-values new-row)))
                        (unless (null (scroll-to-actual-row screen-box))
                          (set-scroll-to-actual-row screen-box nil))))
                     (t
                      (delete-row-at-row-no box cr-no)
                      (when (in-closet-row?)
                        (move-point (row-first-bp-values dest-row)))
                      (unless (null (scroll-to-actual-row screen-box))
                        (set-scroll-to-actual-row screen-box
                                                  dest-row)))))
             ;; if the closet row is empty, then we remove the closet
             (when (zerop (length-in-chas closet-row))
               ;; should we check for empty spaces as well as NO chars
               (setf (slot-value box 'closets) nil))
             (modified box)))))
  boxer-eval::*novalue*)




;; this will lose in the presence of labels, and half a dozen other situations.

(defboxer-command COM-PROMPT ()
  "prompts for the inputs of the function at or before the cursor"
  (reset-region)
  (reset-editor-numeric-arg)
  (unless (name-row? (point-row))
    (let ((first-chunk (chunk-at-cha-no (point-row) 0)))
      (cond ((null first-chunk)
       (boxer-editor-error "Can't find a function near the cursor. "))
      ((member (chunk-chunk (get-pointer-value first-chunk nil))
         *symbols-for-input-line* :test #'eq)
       (do ((arglist (cdr (eval-objs (point-row)))
         (cdr arglist))
      (chunk-no 1 (1+& chunk-no)))
     ((null arglist))
         (when (and (not (eq (car arglist)
           'boxer-eval::*ignoring-definition-object*))
        (not (boxer-eval::flavored-input-marker? (car arglist))))
     #-opengl (add-redisplay-clue (point-row) ':insert)
     (boxer-eval::step-replace-token
      chunk-no (let ((new-box (make-box '(()))))
           (set-name new-box
               (make-name-row
          (list (car arglist)))))))))
      (t
       (let ((chunk (prompter-chunk-before-point))) ;
         (if (null chunk)
       (boxer-editor-error
        "Can't find a function near the cursor. ")
       (let ((arglist
        (arglist-for-prompter
         (chunk-chunk (get-pointer-value chunk nil)))))
         (unless (null arglist)
           #-opengl (add-redisplay-clue (point-row) ':insert)
           (insert-prompter-arglist
      arglist
      (end-of-chunk-cha-no (point-row) chunk)))))))))
    boxer-eval::*novalue*))


(defun insert-prompter-arglist (list function-end-cha-no)
  (MOVE-POINT-1 (point-row) function-end-cha-no (point-screen-box))
  (dolist (item list)
    (map nil #'(lambda (cha)
     (insert-cha *point* cha :moving)) (format nil " ~a:" item)))
  (MOVE-POINT-1 (POINT-ROW)
    (+& function-end-cha-no
        (length (format nil "~a" (car list)))
        2)
    (POINT-SCREEN-BOX)))


(defun prompter-chunk-before-point ()
  (let ((cha-no (if (zerop& (bp-cha-no *point*))
        0
        (1-& (bp-cha-no *point*)))))
    (chunk-at-cha-no (point-row) cha-no T)))



;;;
;;;Stepper
;;;
(defboxer-command com-step ()
  "Like COM-DOIT except shows a movie of how Boxer evaluates the expression."
  (boxer-eval::step-point-row)
  boxer-eval::*novalue*)

;;;
;;; key documentation crock
;;;

(defboxer-command com-document-key ()
  "Read a character and document it."
  (status-line-display 'com-document-key "Press a key or click the mouse...")
  (let* ((input (get-boxer-input *boxer-pane*))
         (key-name (lookup-input-name input))
         (value (boxer-eval::boxer-symeval key-name))
         (*current-font-descriptor* (or *help-font-descriptor*
                                        *default-font-descriptor*)))
    (insert-cha
     *point*
     (make-box
      (append (list (list key-name))
              (if (eq value boxer-eval::*novalue*)
                '((""))
                (cond ((boxer-eval::possible-eval-object? value)
                       (cond ((boxer-eval::compiled-boxer-function? value)
                              (let ((fun
                                     (boxer-eval::compiled-boxer-function-object
                                      value)))
                                (if (symbolp fun)
                                    (let ((doc (justify-editor-doc-string
                                                (get fun 'editor-documentation)))
                                          (1stline
                                           (list "Runs" "the" "command"
                                                 (pretty-edfun-string fun))))
                                      (cond ((null doc)
                                             (list 1stline (list fun)))
                                            ((not (listp doc))
                                             (list 1stline (list doc)))
                                            (t ; multi line doc
                                             (push 1stline doc)
                                             doc)))
                                    (let ((doc (justify-editor-doc-string
                                                (get key-name
                                                     'editor-documentation))))
                                      (cond ((null doc)
                                             '(("a system primitive")))
                                            ((not (listp doc))
                                             (list (list doc)))
                                            (t doc))))))
                             ((fast-eval-data-box? value)
                              '(("Names" "a" "Data" "box")))
                             ((virtual-port? value)
                              '(("Names" "a" "Port")))
                             ((fast-eval-doit-box? value)
                              '(("Runs" "a" "Doit" "box"
                                 "with" "this" "name")))))
                      ((doit-box? value)
                       '(("Runs" "a" "Doit" "box" "with" "this" "name")))
                      ((data-box? value)
                       '(("Names" "a" "Data" "box")))
                      ((sprite-box? value)
                       '(("Names" "a" "Sprite" "box")))
                      ((port-box? value)
                       '(("Names" "a" "Port")))
                      (t '("")))))))
    (mark-file-box-dirty (point-row))
    (status-line-undisplay 'com-document-key))
  boxer-eval::*novalue*)

(defun pretty-edfun-string (x)
  (let* ((raw-string (string x))
         (place (search "COM-" raw-string)))
    (if (and place (zerop place))
        (subseq raw-string 4)
        raw-string)))

;; looks for and handles CR's in the raw doc string
(defun justify-editor-doc-string (docstring)
  (unless (null docstring)
    (let ((rows nil)
          (substringstart 0)
          (1st-cr (position #\newline docstring)))
      (if (null 1st-cr)
        docstring
        (do* ((crpos 1st-cr (position #\newline docstring :start (1+ crpos)))
              (substring (subseq docstring substringstart crpos)
                         (subseq docstring substringstart crpos)))
             ((null crpos) (push (list substring) rows) (nreverse rows))
          (push (list substring) rows)
          (setq substringstart (1+ crpos)))))))

(defvar *help-font-descriptor* nil)


(defboxer-command com-help ()
  "To level Help Function"
  (reset-region)
  (reset-editor-numeric-arg)
  (if (name-row? (point-row))
      (boxer-editor-error "Can't insert help box while on a name")
      (insert-cha *point*
                  (let ((*current-font-descriptor*
                         (or *help-font-descriptor* *default-font-descriptor*)))
                    (make-box '(("To see inputs for a function")
                                #-mcl(" press ctrl-<help> or ctrl-? after the name of the function.")
                              ;#+mcl(" press cmd-<help> or cmd-? after the name of the function.")
                                ;; option-shift-K is the apple glyph
                                #+mcl(" press �-<help> or �-? after the name of the function.")
                                ()
                                ("To get help on a key stroke or mouse action")
                                #-mcl(" press meta-<help> or meta-?")
                                #+mcl(" press option-<help> or option-?")
                                ()
                                ("To get help on the name or spelling of a command")
                                ("use \"name-help <string>\", where string is a sequence")
                                ("of letters in the command."))))))
  boxer-eval::*novalue*)

;;;
;;; key input crock
;;;

(defboxer-command com-insert-key-name ()
  "inserts the Boxer function name for the next keyboard character,
followed by a return."
  (reset-region)
  (status-line-display 'com-insert-key-name "Press any key...")
  (let* ((input (get-boxer-input *boxer-pane*))
   (key-name (lookup-input-name input))
   (key-string (string key-name)))
    (status-line-undisplay 'com-insert-key-name)
    (unless (null key-name)
      (dotimes& (i (length key-string))
  (insert-cha *point* (aref key-string i))))
    #-opengl (add-redisplay-clue (point-row) ':insert))
  (com-return)
  boxer-eval::*novalue*)



#+mcl
(defboxer-command com-link-to-mac-file ()
  "Make a Link to a non boxer Macintosh file"
  (boxer-editor-message "Choose a file to link to...")
  (ccl::catch-cancel
    (let* ((linkfile (ccl::choose-file-dialog :button-string "Link"))
           (xbox (if (box-file? linkfile)
                     (make-file-box linkfile)
                     (make-xfile-box linkfile))))
      (when (box? xbox) (insert-cha *point* xbox))))
  (clear-editor-message)
  boxer-eval::*novalue*)

(defun box-file? (pathname)
  (or #+mcl (eq (ccl:mac-file-type pathname) :BOXR)
      ;; peek at the beginning bytes...
      (let ((b0 (ldb (byte 8 0) bin-op-format-version))
            (b1 (ldb (byte 8 8) bin-op-format-version))
            f0 f1 f2 f3)
        (with-open-file (s pathname :element-type '(unsigned-byte 8))
          (setq f0 (read-byte s nil nil) f1 (read-byte s nil nil)
                f2 (read-byte s nil nil) f3 (read-byte s nil nil)))
        (unless (or (null f0) (null f1) (null f2) (null f3))
          (or
           (and (= b0 f0) (= b1 f1) (= f2 *version-number*) (= f3 0))
           ;; byte swapping version...
           (and (= b0 f1) (= b1 f0) (= f3 *version-number*) (= f2 0)))))))

;; this should go somewhere else (file-prims.lisp ?)
(defun make-file-box (pathname)
  (multiple-value-bind (RO world maj min btype bname)
      (boxer-file-info pathname)
    (declare (ignore RO world maj min))
    (let ((filebox (make-box '(()) btype bname)))
      (mark-box-as-file filebox pathname)
      (shrink filebox)
      (setf (first-inferior-row filebox) nil)
      filebox)))

;; is (point-box) the right thing ?
#+mcl
(defboxer-command com-edit-mac-link ()
  "Change the file a link points to"
  (boxer-editor-message "Change the link's file...")
  (ccl::catch-cancel (edit-mac-file-ref (point-box)))
  (when (eq :normal (display-style (point-box))) (com-shrink-box))
  boxer-eval::*novalue*)
