;;;-*- mode:lisp; syntax:common-lisp; package:boxer -*-

#|

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                    +-Data--+
           This file is part of the | BOXER | system
                                    +-------+

  MCL utilities for boxer.


Modification History (most recent at the top)

 8/01/11 update-source-files prints out better info, handles list arg for synchfile
         as args to encode-universal-time
10/22/05 fixed #+lispworks editor meta-. support
         also #+mcl'd call to ccl::set-file-write-date in mcl-file->pc-file

10/19/05 removed platform dependencies from file conversion routines.  Both platforms
         can now convert either way

 2/01/05 fixed pc-file->mcl-file after discovering that the meaning of
         #\NewLine is platform dependent (duh!)
 1/17/04 Converted Source file utilities for PC->mcl. Dspecs for lispworks 4.3
12/10/04 update-pc-files uses *special-source-files* var to capture non lisp source files
         when transferring to/from PC
 4/18/04 mc-file->pc-file, update-pc-files utilities for source sharing
 8/03/03 doc-self-inserting: ignore-errors is now part of common lisp
         leave ccl::*control-key-mapping* alone for ccl5.0 cause it doesn't work
 4/22/03 merged current LW and MCL files, then added useful things
         from the unix-utils file
 2/14/02 added #+lispworks support for finding boxer def forms
 8/29/01 added boundp check to add-def-type
 2/24/01 conditionized for general (lispworks) use
         new modification list hacking capabilities,
 2/16/01 merged current LW and MCL files
11/20/00 fixed bug in mod-history-date-line? that ignored 2 digit months
11/09/00 added modification history file header line utilities
11/09/00 Start logging changes: source = boxer version 2.4


|#

(in-package :boxer)

(defun show-redisplay-inits ()
  (do* ((funs (reverse *redisplay-initialization-list*) (cdr funs))
        (codes (reverse *redisplay-related-initializations*) (cdr codes))
        (fun (car funs) (car funs))
        (code (car codes) (car codes)))
       ((or (null fun) (null code)))
    (print fun)
    (pprint code)
    (read-char)))

#+lispworks
(progn

(dspec:define-form-parser (boxer-eval::defboxer-primitive (:alias defun)))
(dspec:define-form-parser (boxer-eval::defrecursive-funcall-primitive (:alias defun)))
(dspec:define-form-parser (boxer-eval::defrecursive-eval-primitive (:alias defun)))
(dspec:define-form-parser (boxer-eval::defsprite-function (:alias defun)))
(dspec:define-form-parser (defboxer-command (:alias defun)))
(dspec:define-form-parser (defmethod (:alias CLOS::DEFMETHOD)))

  )

;;;; useful material from unix-utils.lisp

;;; Prints out a table of editor command keys.
;;; Maybe also print out a table of primitives.

(defvar *sorted-command-key-alist* nil) ; boxer::*boxer-command-key-alist*
                                        ; isn't defvar'd yet...

(defvar *page-width* 80)


(defun make-keys-report (&optional (file "keychart.txt"))
  (with-open-file (stream file :direction :output)
     (print-keys-report-1 stream)))

(defun print-keys-report-1 (&optional (stream *standard-output*))
  (when (null boxer::*boxer-command-key-alist*)
    (setq boxer::*boxer-command-key-alist*
      (sort (copy-seq boxer::*boxer-command-key-alist*)
      #'string-lessp
      :key #'car)))
  (dolist (key-pair *sorted-command-key-alist*)
    (unless (or (doc-self-inserting? (car key-pair) (cdr key-pair))
    (doc-key-undefined? (car key-pair) (cdr key-pair)))
      (format stream
        "~A: ~14T~A ~39T~*~{~42T~A~%~}"
        (doc-pretty-key-name (car key-pair))
        (doc-pretty-key-function-name (car key-pair) (cdr key-pair))
        "Ohm-mani-padme-hum"
        (doc-pretty-function-doc (cdr key-pair) (- *page-width* 42))))))


(defun doc-pretty-key-name (name)
  (let ((string (symbol-name name)))
    (string-capitalize
     (substitute #\space #\-
     (remove-mouse-and-key string)))))

(defun remove-mouse-and-key (string)
  (let ((key-pos (search "-KEY" string))
  (mouse-pos (search "MOUSE-" string)))
    (cond ((not (null key-pos)) (subseq string 0 key-pos))
    ((not (null mouse-pos))
     (concatenate 'string
      (subseq string 0 mouse-pos)
      (subseq string (+ mouse-pos (length "MOUSE-")))))
    (t string))))

(defun doc-pretty-key-function-name (name obj)
  (declare (ignore name))
  (cond ((symbolp obj)
   (if (eq obj 'ignore)
       "Undefined"
     (string-capitalize
      (substitute #\space #\-
      (subseq (symbol-name obj)
       (length "COM-"))))))
  (t "???")))


(defun doc-pretty-function-doc (obj &optional maximum-length)
  (flet ((split-if-necessary
    (string)
    (if (and maximum-length (> (length string) maximum-length))
        (split-string-at-n string maximum-length)
      string)))
  (let ((doc (documentation obj 'function)))
    (if (null doc) ""
      (let ((segments (split-if-necessary (string-capitalize
             doc
             :end 2))))

        (if (consp segments)
      (let ((result nil))
        (dolist (string segments (nreverse result))
          (push string result)))
    (list segments)))))))

(defun split-string-at-n (string n &optional (start 0))
  (if (> (+ start n) (length string))
      (cons (substitute #\space #\newline
      (string-trim " " (subseq string start))) nil)
    (let ((likely-place (or (position #\space string
              :from-end t
              :start start
              :end (+ start n))
          (+ start n))))
      (cons (substitute #\space #\newline
      (string-trim " " (subseq string start likely-place)))
      (split-string-at-n string n likely-place)))))

(defun doc-self-inserting? (name function-obj)
  (and (not (symbolp function-obj))
       (let ((string (symbol-name name)))
   (or (or (< (position #\- string) 2)
     (search "CAPITAL-" string))
       (let ((try (ignore-errors
       (progn (read-from-string
         (concatenate
          'string
          "#\\"
          (subseq string 0 (position #\- string))))
        :no-read-error))))
         (eq (car try) :no-read-error))))))


(defun doc-key-undefined? (name obj)
  (declare (ignore name))
  (eq obj 'ignore))




;;; print out primitives

(defvar *input-flavors-to-ignore* '(boxer-eval::dont-copy))

(defun pretty-print-boxer-function-arglist (arglist)
  (if (null arglist)
      "()"
      (mapcar #'(lambda (arg)
      (if (and (consp arg)
         (fast-memq (car arg) *input-flavors-to-ignore*))
          (cadr arg)
          arg))
        arglist)))


(defun print-boxer-primitives (&optional (stream *standard-output*))
  (do-symbols (s (find-package 'bu))
    (when (and (boundp s)
         (not (or (search "-KEY" (symbol-name s))
      (search "MOUSE" (symbol-name s))))
         (consp (symbol-value s))
         (boxer-eval::compiled-boxer-function?
    (boxer-eval::static-variable-value (symbol-value s))))
      (let ((fun (boxer-eval::static-variable-value (symbol-value s))))
  (format stream "~%~A  ~A"
    s
    (pretty-print-boxer-function-arglist
     (boxer-eval::boxer-function-arglist fun)))))))

;;; calculating sizes....

(defstruct (size-stat (:type vector))
  (boxes 0)
  (rows 0)
  (chas 0)
  (sprites 0)
  (graphics 0)
  (ports    0))

(defvar *size-stats* (make-size-stat))

(defun clear-size-stats (&optional (stats *size-stats*))
  (setf (size-stat-boxes stats) 0
  (size-stat-rows  stats) 0
  (size-stat-chas  stats) 0
  (size-stat-sprites stats) 0
  (size-stat-graphics stats) 0
  (size-stat-ports stats) 0))

(defun report-size-stats (&optional (stats *size-stats*))
  (format t "~%Number of Boxes:    ~D~
             ~%Number of Rows :    ~D~
             ~%Number of Chas :    ~D~
             ~%Number of Sprites:  ~D~
             ~%Number of Graphics: ~D"
    (size-stat-boxes stats)
    (size-stat-rows stats)
    (size-stat-chas stats)
    (size-stat-sprites stats)
    (size-stat-graphics stats)
    (size-stat-ports stats)))

(defmethod calculate-size ((box box) &optional (stats *size-stats*))
  (incf (size-stat-boxes stats))
  (let ((gi (graphics-info box)))
    (cond ((null gi))
          ((graphics-sheet? gi) (incf (size-stat-graphics stats)))
          (t (incf (size-stat-sprites stats)))))
  ;; now recurse
  (do-box-rows ((row box))
    (calculate-size row stats)))

(defmethod calculate-size ((box port-box) &optional (stats *size-stats*))
  (incf (size-stat-ports stats)))

(defmethod calculate-size ((row row) &optional (stats *size-stats*))
  (incf (size-stat-rows stats))
  (do-row-chas ((cha row))
    (if (box? cha)
  (calculate-size cha stats)
  (incf (size-stat-chas stats)))))

;; left out function size calculation because it was very lucid specific



;; this is old stuff but leave it in for now as a possible seed for new
;; version (need still exists)
;;;; Manual hacking

(defvar *manual-text-file* "~boxer/doc/manual-print.text")

(defun add-manual-text (box subject-string)
  (with-open-file (stream *manual-text-file*
        :direction :output :if-does-not-exist :create :if-exists :append)
    (format stream "~%~A~%~%" subject-string)
    (process-manual-box box stream)
    (format stream "~%~A~%" #\page)))


;; look for entries
;; An entry is characterized by having  "Description" and "Command" sub boxes
(defun is-entry? (box)
  (and (boxer-eval::lookup-static-variable-internal box 'bu::command)
       (boxer-eval::lookup-static-variable-internal box 'bu::description)))

(defun print-entry (box stream indent-level)
  (let ((comm (caddr (boxer-eval::lookup-static-variable-internal box 'bu::command)))
  (desc (caddr (boxer-eval::lookup-static-variable-internal box 'bu::description))))
    (do-box-rows ((row comm))
      (dotimes (i indent-level) (format stream " "))
      (do-row-chas ((cha row))
  (format stream "~A" cha))
      (format stream "~&"))
    (do-box-rows ((row desc))
      (dotimes (i indent-level) (format stream " "))
      (do-row-chas ((cha row))
  (format stream "~A" cha))
      (format stream "~&"))))


;; otherwise, print the sub heading and recurse
(defun process-manual-box (box stream &optional (indent-level 0))
  (do-box-rows ((row box))
    (let ((1st-cha (cha-at-cha-no row 0)))
      (when (and (not (box? 1st-cha)) (not (char= 1st-cha #\space)))
  ;; looks like a valid row, so print the text with the current indentation
  (dotimes (i indent-level) (format stream " "))
  (do-row-chas ((cha row))
    (cond ((cha? cha) (format stream "~A" cha))
    ((is-entry? cha)
     (format stream "~%~%")
     (print-entry cha stream indent-level)
     (format stream "~%")
     (return))
    (t ;; recurse
     (format stream "~%~%")
     (process-manual-box cha stream (1+ indent-level))
     (format stream "~%")
     (return))))))))
