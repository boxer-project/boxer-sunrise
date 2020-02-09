;;;-*- mode:lisp; syntax:common-lisp; package:boxer -*-

#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

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

#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)



(in-package :boxer)

(defun cf (f)
  (let* ((sysprops (sm::system-properties (sm::find-system-named 'boxer)))
         (sp (getf sysprops :pathname-default))
         (bp (getf sysprops :binary-pathname-default)))
  (compile-file (merge-pathnames f sp)
                :output-file (merge-pathnames (sm::make-binary-pathname f)
                                              (if (null bp) sp bp)))))

(defun compops ()
  (proclaim '(optimize (speed 3) (space 2) (compilation-speed 0)
              ;; these next ones happen to be the defaults
              (safety 1) (debug 1)))
  (format t "~&Compiler Options are:~&")
  #+mcl
  (ccl::declaration-information 'optimize))

(defun lcf (file-or-files &optional (load? t))
  (cond ((and (listp file-or-files) load?)
         (dolist (f file-or-files) (load (cf f))))
        ((listp file-or-files)
         (dolist (f file-or-files) (cf f)))
        (load?
         (load (cf file-or-files)))
        (t (cf file-or-files))))

;; printing help

(defun pa (x) (let ((*print-array* t)) (print x)))
(defun pp (x) (let ((*print-pretty* t)) (print x)))

;;; this is general, but I don't know where to put it
(defun whereis (command)
  (get command :on-keys))

(defun test-loop ()
  (loop
    (terpri)
    (princ "Boxer> ")
    (funcall (read))
    #+opengl (repaint)
    #-opengl (redisplay)
    ))

(defun show-redisplay-inits ()
  (do* ((funs (reverse *redisplay-initialization-list*) (cdr funs))
        (codes (reverse *redisplay-related-initializations*) (cdr codes))
        (fun (car funs) (car funs))
        (code (car codes) (car codes)))
       ((or (null fun) (null code)))
    (print fun)
    (pp code)
    (read-char)))

;;; elsewhere
;(setq *boxer-version-info* "Experimental Mac Boxer")

#+mcl
(defun keys-code ()
  (format t "Press the key...")
  (multiple-value-bind (message modifiers)
                       (ccl::wait-key-event)
    (let ((char-code (ldb (byte 8 0) message))
          (key-code (ldb (byte 8 8) message)))
      (format t "~&Char ~A (~C); Key ~A, Modifiers ~A" char-code
              (code-char char-code) key-code modifiers))))

;;; Editor support
#+mcl
(progn
  (defun add-def-type (definer &optional (type 'function))
      (when (boundp 'ccl::*define-type-alist*)
	(pushnew (string-downcase (subseq (symbol-name definer) 3))
		 (cdr (assoc type ccl::*define-type-alist*))
             :test #'equal)))

  (add-def-type 'eval::defboxer-primitive 'function)
  (add-def-type 'eval::defrecursive-funcall-primitive 'function)
  (add-def-type 'eval::defrecursive-eval-primitive 'function)
  (add-def-type 'box::defsprite-function 'function)
  (add-def-type 'box::defboxer-command 'function)
  )

#|
;; this was for pre 4.3 LWW
#+lispworks
(progn
  (editor:define-top-level-form-parser eval::defboxer-primitive
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser eval::defrecursive-funcall-primitive
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser eval::defrecursive-eval-primitive
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser defsprite-function
       editor::section-parse-name-only)
  (editor:define-top-level-form-parser defboxer-command
       editor::section-parse-name-only)
  ;; we have our own private version of defmethod so inform the editor
  ;; this is the result of (editor::get-parser 'clos::defmethod)
  (editor:define-top-level-form-parser defmethod
      EDITOR::SECTION-PARSE-DEFMETHOD-TLF)
)
|#

#+lispworks
(progn

(dspec:define-form-parser (eval::defboxer-primitive (:alias defun)))
(dspec:define-form-parser (eval::defrecursive-funcall-primitive (:alias defun)))
(dspec:define-form-parser (eval::defrecursive-eval-primitive (:alias defun)))
(dspec:define-form-parser (eval::defsprite-function (:alias defun)))
(dspec:define-form-parser (defboxer-command (:alias defun)))
(dspec:define-form-parser (defmethod (:alias CLOS::DEFMETHOD)))

  )
#+mcl
(eval-when (load)
  (format t "~&Changing control key mapping and Paste with styles")
  (setq #-ccl-5.0 ccl::*control-key-mapping* #-ccl-5.0 :command
        ccl::*autoload-traps* t
        ccl::*paste-with-styles* nil)
  )


;; file header parsing
(defun mod-history-start-line? (string)
  (search "Modification History" string :test #'string-equal))

(defun mod-history-end-line? (string)
  (or (search "Start logging" string :test #'string-equal)
      (search "Started " string :test #'string-equal)
      (search "in-package" string :test #'string-equal)))

(defun mod-history-date-line? (string)
  (let ((month nil) (date nil) (year nil) (field :month) (1st-char nil))
    (dotimes (i (length string))
      (let ((char (char string i)))
        (cond ((char= char #\space))
              ((digit-char-p char)
               (if (null 1st-char)
                 (setq 1st-char (digit-char-p char))
                 (case field
                   (:month (setq month (+ (* 1st-char 10) (digit-char-p char))
                                 1st-char nil))
                   (:date (setq date (+ (* 1st-char 10) (digit-char-p char))
                                1st-char nil))
                   (:year (setq year (+ (* 1st-char 10) (digit-char-p char))
                                1st-char nil)
                          (return (values T month date year))))))
              ((char= char #\/)
               (case field
                 (:month (cond ((and (null 1st-char) (null month)) (return nil))
                               ((null month)
                                (setq month 1st-char field :date))
                               (t (setq field :date))))
                 (:date (when (null date) (setq date 1st-char))
                        (setq field :year)))
               (setq 1st-char nil))
              ((eq field :month) (return nil)))))))

;; try and capture multiline entries which don't all start with a date...
(defun get-mod-history-lines (file &optional
                                   (outstream *standard-output*)
                                   (show-filename? t))
  (let ((eof-value (list 'eof))
        (date-line? nil) (start? nil) (count 0))
    (with-open-file (in file)
      (loop
       (let ((line (read-line in nil eof-value)))
         (cond ((eq line eof-value) (return nil))
               ((null start?)
                (when (mod-history-start-line? line) (setq start? t)))
               ((mod-history-end-line? line) (return count))
               ((mod-history-date-line? line)
                (setq date-line? t) (incf count)
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))
               ((not (null date-line?))
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))))))))

(defun get-mod-history-lines-after (file month date year
                                         &optional
                                         (outstream *standard-output*)
                                         (show-filename? t))
  (let ((eof-value (list 'eof))
        (start? nil)
        (date-line? nil)
        (count 0))
    (with-open-file (in file)
      (loop
       (let ((line (read-line in nil eof-value)))
         (cond ((eq line eof-value) (return nil))
               ((null start?)
                (when (mod-history-start-line? line) (setq start? t)))
               ((mod-history-end-line? line) (return count))
               ((multiple-value-bind (is-date-line? lmonth ldate lyear)
                    (mod-history-date-line? line)
                  (and is-date-line?
                       (let ((good-date?
                              (or (> (mod (+ 50 lyear) 100) (mod (+ 50 year) 100))
                                  (and (= (mod (+ 50 lyear) 100)
                                          (mod (+ 50 year) 100))
                                       (or (> lmonth month)
                                           (and (= lmonth month)
                                                (>= ldate date)))))))
                         (unless good-date? (setq date-line? nil))
                         good-date?)))
                (setq date-line? t) (incf count)
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))
               ((not (null date-line?))
                (if show-filename?
                    (format outstream "~&~A: ~A" (file-namestring  file) line)
                  (format outstream "~&~A" line)))))))
    count))

#|
(dolist (file (sm::system-source-files (sm::find-system-named 'boxer)))
  (get-mod-history-lines-after file 9 1 98))

(with-open-file (out "C:\\Boxer\\ChangeLog" :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
  (let ((total-changes 0))
    (dolist (file (sm::system-source-files (sm::find-system-named 'boxer)))
      (format out "~&     ~A   ~&" (enough-namestring file))
      (format out "~&     ~D entries~&"
              (let ((changes (get-mod-history-lines-after file 9 1 98 out nil)))
                (incf total-changes changes)
                changes)))
    (format out "~&~&Total Changes = ~D" total-changes)))

;; a histogram


(defvar *modarray* (make-array 50 :initial-element 0))

(dolist (file (sm::system-source-files (sm::find-system-named 'boxer)))
  (let ((eof-value (list 'eof))
        (start? nil))
    (with-open-file (in file)(loop
        (let ((line (read-line in nil eof-value)))
          (cond ((eq line eof-value) (return nil))
                ((null start?)
                 (when (mod-history-start-line? line) (setq start? t)))
                ((mod-history-end-line? line) (return nil))
                ((multiple-value-bind (date-line? lmonth ldate lyear)
                     (mod-history-date-line? line)
                   (and date-line?
                        (incf (aref *modarray*
                                    (cond ((zerop lyear) (+ lmonth 36))
                                          ((< lyear 97) 0)
                                          ((= lyear 97) lmonth)
                                          ((= lyear 98) (+ lmonth 12))
                                          ((= lyear 99) (+ lmonth 24))))))))))))))
|#



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

(defvar *input-flavors-to-ignore* '(eval::dont-copy))

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
	       (eval::compiled-boxer-function?
		(eval::static-variable-value (symbol-value s))))
      (let ((fun (eval::static-variable-value (symbol-value s))))
	(format stream "~%~A  ~A"
		s
		(pretty-print-boxer-function-arglist
		 (eval::boxer-function-arglist fun)))))))

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
  (and (eval::lookup-static-variable-internal box 'bu::command)
       (eval::lookup-static-variable-internal box 'bu::description)))

(defun print-entry (box stream indent-level)
  (let ((comm (caddr (eval::lookup-static-variable-internal box 'bu::command)))
	(desc (caddr (eval::lookup-static-variable-internal box 'bu::description))))
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



;;; source code utilities

;; lines in PC files are CRLF terminated
(defun mcl-file->pc-file (mac-file-in pc-file-out)
  (with-open-file (in mac-file-in)
    (with-open-file (out pc-file-out :direction :output :if-exists :supersede)
      (let ((eof-value (list 'eof)))
        (loop (let ((c (read-char in nil eof-value)))
                (cond ((eq c eof-value) (return nil))
                      ((char= c #\NewLine)
                       (write-char #\NewLine out)
                       (let ((nextchar (peek-char nil in nil nil)))
                         (unless (and nextchar (char= nextchar #\LineFeed))
                           (write-char #\LineFeed out))))
                      (t (write-char c out))))))))
  #+mcl
  (ccl::set-file-write-date pc-file-out (file-write-date mac-file-in))
  )

;; lines in MCL files are only CR terminated
;; lines in PC lisp are LF terminated, NOTE: meaning of #\NewLine is platform
;; dependent !!!!
(defun pc-file->mcl-file (pc-file-in mac-file-out)
  (with-open-file (in pc-file-in)
    (with-open-file (out mac-file-out :direction :output :if-exists :supersede)
      (let ((eof-value (list 'eof)))
        (loop (let ((c (read-char in nil eof-value)))
                (cond ((eq c eof-value) (return nil))
                      ((char= c #\cr)
                       ;; make sure only 1 #\CR is issued for CRLF or CR
                       (write-char #\cr out)
                       (let ((nextchar (peek-char nil in nil nil)))
                         (when (and nextchar (char= nextchar #\LineFeed))
                           ;; if there is a LF, read it in an ignore it...
                           (read-char in))))
                      ((char= c #\lf) (write-char #\cr out))
                      (t (write-char c out))))))))
  ;; no equivalent in LW (sigh)
  ;(ccl::set-file-write-date mac-file-out (file-write-date pc-file-in)))
  )

(defun no-convert (in-file out-file)
  (with-open-file (in in-file)
    (with-open-file (out out-file :direction ':output :if-exists ':overwrite :if-does-not-exist ':create)
      (let ((eof-value (list 'eof)))
        (loop (let ((c (read-char in nil eof-value)))
                (cond ((eq c eof-value) (return nil))
                      (t (write-char c out)))))))))

;; need special handling for  "boxer.rsrc"
(defvar *special-source-files* '("devlog" "plst.txt" "deliver-boxer.sh"))

(defun update-source-files (synchfile dest-dir
                                    &optional (file-conversion #'no-convert)
                                    (system (sm::find-system-named 'boxer)))
  (let* ((start-utime (cond ((listp synchfile)
                             (prog1
                                 (apply #'encode-universal-time (cdr synchfile))
                               (setq synchfile (car synchfile))))
                            (t
                             (file-write-date synchfile))))
         (source-dir (getf (sm::system-properties system) :pathname-default))
         (all-source-files (append (directory (merge-pathnames "*.lisp" source-dir))
                                   (with-collection
                                     (dolist (sf *special-source-files*)
                                       (collect (merge-pathnames sf source-dir))))))
         (source-files (remove-if #'(lambda (f)
                                      (let ((fwd (file-write-date f)))
                                        (cond ((numberp fwd) (<= fwd start-utime))
                                              (t (error "No write date for ~A" f)))))
                                  all-source-files)))
    (multiple-value-bind (sec min hou date month year)
        (decode-universal-time start-utime)
      (format t "~%Files changed since ~D:~D:~D on ~D/~D/~D"
              hou min sec month date year))
    (format t "~%Selected ~D of ~D files.  "
            (length source-files) (length all-source-files))
    (when (y-or-n-p "List ? ")
      (dolist (f source-files) (print f)))
    (when (y-or-n-p "Update changed files to ~S ?"
                    (merge-pathnames "*.lisp" dest-dir))
      (dolist (f source-files)
        (let ((newpath (merge-pathnames (make-pathname :name (pathname-name f)
                                                       :type (pathname-type f))
                                        dest-dir)))
          (format t "~%Updating ~S to ~S" f newpath)
          (funcall file-conversion f newpath)))
      (when (y-or-n-p "Update synch file ?")
        (with-open-file (sfs synchfile :direction ':output :if-exists ':append)
          (multiple-value-bind (sec min hou date month year)
              (decode-universal-time (get-universal-time))
            (declare (ignore sec min hou))
            (format sfs "~%~D/~D/~D Synched source files~%" month date year)))
;        (funcall file-conversion synchfile (make-pathname :name (pathname-name synchfile)
;                                                          :type (pathname-type synchfile)
;                                                          :defaults dest-dir))
        ))))

#|


(update-source-files "/Boxer/SRC/OpenGL/SYNCHROSTAMP" "/Users/edwardlay/Public/Drop Box/")

(update-source-files "/Boxer/SRC/OpenGL/SYNCHROSTAMP" (capi::prompt-for-directory "Send Files) to: "))

;; midnight feb 9 2011
(update-source-files '("/Boxer/SRC/OpenGL/SYNCHROSTAMP" 0 0 0 9 2 2011) (capi::prompt-for-directory "Destination directory:"))

|#
