;;;;  -*- Mode:Lisp; Syntax: Common-Lisp;package:Boxer; -*-
;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                           +-Data--+
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;
;;;;        This file contains the interface between Boxer Editor Structure and
;;;;        the Virtual copy mechanism.  The file contains the code for
;;;;        converting Editor structure to Virtual Copy Structure (referred to
;;;;        as the CHUNKER).
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;  10/ 2/12 get-dots-value gets substring's element type from the string instead of guessing
;;;;   1/ 8/12 new %get-chunking-syntax handles non-ascii chars for use in chunk-handle-char & get-chunking-syntax
;;;;           SETF for get-chunking-syntax warns if charcode > table size
;;;;   7/13/09 removed #+lispworks stanrdard-char from get-dots-values to enter the unicode world..
;;;;   2/15/03 merg current LW and MCL file
;;;;   2/12/01 merged with current MCL file
;;;;   1/23/01 added #+lispworks to get-dots-values
;;;;   4/06/00 new and improved #+ccl ignoring-number-read-errors
;;;;   2/15/99 started Harlequin Lispworks changes (LW reader doesn't support #,)
;;;;   9/02/98 moved *test-box* to commented out test code to delay 1st call to make-box
;;;;   9/02/98 Started Logging changes: source = boxer version 2.3beta
;;;;
(in-package :boxer)



;;;; TO DO:

;;; The 2 things that we can bum for speed in this file are
;;; READ-FORMATTING-INFO and CHUNK. The best thing to do for
;;; READ-FORMATTING-INFO is to get rid of all the type conversions we need to
;;; do in order to keep the Lisp READER happy.  The better solution is to fix
;;; CHUNK to call READ directly but we need to teach the READER about
;;; SIMPLE-STREAMs which is going to be quite implementation specific

;;;; The   CHUNKER

;; The Chunker is responsible for converting Boxer Editor Structure into the
;; Structures used by the Virtual Copy Mechanism.  The Chunker should do all
;; of the following:
;;  1) Characters need to be chunked into Objects which are meaningful to
;;     the Evaluator and the Data Manipulators
;;  2) Formatting information such as CaSE and   Spacing needs to be preserved
;;  3) Additional Information used by either the Evaluator or the Data
;;     Manipulators alone, needs to be associated with the Objects.  These
;;     are things like !'s, @'s and LABEL's
;;  4) Build up several Types of Data Structure in Parallel (??)
;;  5) Ideally, the internal representation should closely follow the
;;     external representation for efficiency reasons
;;
;; The difference between the BOXER CHUNKER, and a Lisp READER, is that the
;; Lisp READER is oriented towards providing objects for the evaluator (that
;; is, program structure) and suppressing all other details (such as
;; formatting) while the BOXER CHUNKER is oriented towards providing objects
;; for the Data Manipulators AS WELL AS the evaluator (that is, data structure
;; AND program structure).  In particular, the CHUNKER has to be tolerant of
;; all sorts of syntactic horors because we are chunking arbitrary
;; datastructure which is NOT guaranteed to have any semantics and therefore
;; is not constrained to be syntactically correct.
;; We are NOT using the Lisp READER because it is too difficult to get it to
;; preserve the formatting information correctly, it puts too much of a
;; constraint on the format of the objects, it doesn't allow us to build up
;; several different structures in parallel, it has too many "LISPy" things
;; wired into it (like .'s), it doesn't hack fonts, and it doesn't encourage
;; machine independence.  We WILL use the READER to Chunk Symbols and
;; Numbers for us.  This limited capability should exist on any implementation.

(defvar *chunker-readtable* (copy-readtable))

(defvar *comment-style* ':italics "The character style for comments.")

(defvar *initial-character-type-table nil)

(defvar *ignore-fonts* nil)

;;; these are sort of like READTABLES
(defvar *initial-chunking-syntax-table* nil
  "Sacred. Do not touch.")

(defvar *chunking-syntax-table* nil
  "The default chunking table")

(defvar *standard-whitespace-characters*
  '(#\space #\newline #\tab #\page #\linefeed))

(defvar *implementation-dependent-largest-reasonable-char-code*
  (min 256 char-code-limit)
)

(defvar *initial-unbox-char*   #\@)
(defvar *initial-eval-char*    #\!)
(defvar *initial-previous-tell-environment-char* #\^)
(defvar *initial-comment-char* #\;)
(defvar *initial-returned-value-char* #\|)
(defvar *initial-label-char* #\:)
(defvar *initial-dot-char* #\.)




;;; this is like a readtable...

(defstruct (chunk-table (:predicate chunk-table?)
                        (:constructor make-chunk-table)
                        (:constructor %make-chunk-table
                            (handlers handler-translation-alist))
                        (:copier nil))
  (handlers (make-array
             *implementation-dependent-largest-reasonable-char-code*))
  (handler-translation-alist '((constituent . constituent-character-handler)
                               (whitespace . whitespace-character-handler))))

(defun copy-chunk-table (table)
  (%make-chunk-table (copy-seq (chunk-table-handlers table))
                     (chunk-table-handler-translation-alist table)))

(defmacro %get-chunking-syntax (char table)
  (let ((ccodevar (gensym))
        (tablevar (gensym)))
    `(let ((,ccodevar (char-code ,char))
           (,tablevar (chunk-table-handlers ,table)))
       (cond ((<& ,ccodevar (length ,tablevar))
              (svref& ,tablevar ,ccodevar))
             (t 'constituent-character-handler)))))

(defmacro chunk-handle-char (char &rest args)
  `(funcall (%get-chunking-syntax ,char *chunking-syntax-table*) . ,args))

(defun get-chunking-syntax (char
                            &optional (chunking-table *chunking-syntax-table*))
  (let* ((raw-handler (%get-chunking-syntax char chunking-table))
         (raw-symbol #-lucid raw-handler
                     #+lucid (if (typep raw-handler 'sys::procedure)
                                 (sys:procedure-name raw-handler)
                                 raw-handler))
         (pretty-name (rassoc raw-symbol
                              (chunk-table-handler-translation-alist
                               chunking-table))))
    (cond ((not (null pretty-name))
           (car pretty-name))
          (t raw-symbol))))

;; AppGen lossage with complex form of defsetf...

(defsetf get-chunking-syntax (char &optional
                                     (chunking-table *chunking-syntax-table*))
    (ns)
  `(let ((defined-handler
           (cdr (fast-assq ,ns (chunk-table-handler-translation-alist
                                ,chunking-table)))))
     (cond ((and (null defined-handler)
                 (not (old-functionp ,ns)))
            (error "~A is not a valid syntax" ,ns))
           ((>= (char-code ,char) (length (chunk-table-handlers ,chunking-table)))
            (warn "Unable to SET the syntax for ~C (code=~D)" ,char (char-code ,char)))
           (t
            (setf (svref& (chunk-table-handlers ,chunking-table)
                          (char-code ,char))
                  (if (null defined-handler)
                      ,ns
                      defined-handler))))))


(defun set-chunking-syntax-from-char (for-char from-char
                                      &optional
                                        (for-chunking-table
                                         *chunking-syntax-table*)
                                        (from-chunking-table
                                         *chunking-syntax-table*))
  (setf (get-chunking-syntax for-char for-chunking-table)
        (get-chunking-syntax from-char from-chunking-table)))



#|
Oldness and Oldossity.....


;;;; Types of Characters
;;  The CHUNKER understands several different types of characters.  These
;;  characters are contained in mutually exclusive lists in a
;;  CHARACTER-TYPE-TABLE organized by the different syntax of each character
;;  type.  A WITH-CHARACTER-TYPES is a Macro which binds the value
;;  of each type based on their entry in the  CHARACTER-TYPE-TABLE.
;;
;;  The way a particular character is treated is not based soley upon its
;;  position in the CHARACTER-TYPE-TABLE.  A character's Style may change its
;;  behavior (for example, the comment font).  Individual characters may also
;;  have special behaviors assigned to them via the
;;  character macro facility.
;;
;;; Character Syntax
;;
;;  Constituent-     symbols and numbers are made of these
;;  Comment-         if we have fat chars AND we select a FONT to be a comment
;;                   font, then characters of that font will be of this type
;;  Start Comment-   like ";" or font change characters for the comment font
;;                   in an editor implementation that doesn't use fat chars
;;  End Comment-     change font character when the previous font is the
;;                   comment font (non fat char implementation only)
;;  Whitespace-      Spaces and anything else that doesn't have a meaning
;;  Evaluator Macro- characters that are treated specially by the
;;                   evaluator (like unbox)
;;  Label Marker-    character used to delineate labels
;;  Box Specific-    depending on how we implement our streams, this can be
;;                   either a box or characters like *STRT-BOX-CHARACTER*
;;  Macro-           A user defined handler.  We pass it the stream and let it
;;                   do what it wants. Character macros are supposed to side
;;                   effect the STREAM and the value of CURRENT-STRING before
;;                   returning.  See the COMMENT-HANDLER macro for an example
;;                   Character Macros assume an arglist of:
;;                  (STREAM STUFF-RFP? LFP PNAME CHUNK RFP LABEL EVAL-PROPERTIES).
;;                   The actual macro character is left in the stream.
;;
;;  The tables should probably support font/style attributes as well but for
;;  now, we handle them with global variables
;;
;; NOTE: we really ought to make the syntax types be small fixnums and use
;; them as an index into a table of handlers for CHUNK to use (instead of
;; the COND which it is using now) but I need to decide on a standard calling
;; sequence for handlers first...
;; Actually, then EVERTHING would be like char macros, hmmmmmm
;;

(defstruct (character-type-table (:predicate character-type-table?)
                 (:copier nil))
  (legal-character-types '(constituent whitespace))
  (syntax (make-array #+symbolics 256. #-symbolics char-code-limit
              :element-type 'symbol ; potential win if we use fixnums ?
              ))
  macro-alist)

(defun copy-character-type-table (table)
  (make-character-type-table :legal-character-types
                 (character-type-table-legal-character-types table)
                 :syntax
                 (let ((old-syntax (character-type-table-syntax
                        table)))
                   (make-array (array-dimensions old-syntax)
                       :element-type
                       (array-element-type old-syntax)
                       :initial-contents
                       old-syntax))
                 :macro-alist
                 (character-type-table-macro-alist table)))


;; more character madness
(defun get-chunking-syntax (char
                &optional (chunking-table *chunking-syntax-table*))
  (aref (character-type-table-syntax chunking-table) (char-code char)))

(defsetf get-chunking-syntax (char &optional
                   (chunking-table *chunking-syntax-table*))
  (ns)
  `(progn
     (unless (member ,ns
             (character-type-table-legal-character-types
              ,chunking-table))
       (warn "Setting the syntax for ~C to ~A" ,char ,ns))
     (setf (aref (character-type-table-syntax ,chunking-table)
         (char-code ,char)) ,ns)))

;; This is exactly analogous to CL's SET-SYNTAX-FROM-CHAR
(defun set-chunking-syntax-from-char (for-char from-char
                       &optional
                       (for-chunking-table
                    *chunking-syntax-table*)
                       (from-chunking-table
                    *chunking-syntax-table*))
  (setf (get-chunking-syntax for-char for-chunking-table)
    (get-chunking-syntax from-char from-chunking-table)))

|#




;;;  Formatting Information Functions

(defun make-formatting-info-from-simple-stream (stream)
  (let ((initial-pos (stream-position stream)))
    (make-formatting-info :chas (formatting-array stream)
                          :start initial-pos
                          :stop initial-pos)))


(defun formatting-info-length (fi)
  (if (numberp fi) fi (-& (fi-stop fi) (fi-start fi))))

(defun formatting-info-string (fi)
  (if (numberp fi)
      (make-string fi :initial-element #\space)
      (let ((string (make-string (- (fi-stop fi) (fi-start fi))))
            (idx 0))
        (do-fi-chas (cha fi)
          (setf (aref (the simple-string string) idx) cha)
          (incf& idx))
        string)))


(defun untouched-fi? (fi)
  (unless (numberp fi)
    (minusp& (fi-start fi))))

(defun make-whitespace (length)
  length
                                        ;  (make-formatting-info :chas (make-array length :initial-element #\Space)
                                        ;			:start 0
                                        ;			:stop length)
  )



;;;; READER Stuff
;;  We use the Lisp Reader to chunk symbols and numbers for us (and nothing
;;  else) Everything is Alphabetic and Numbers are interpreted as being in
;;  Decimal. We leave the syntax for .'s alone to be able to handle
;;  floating point

;;; Make Everything Alphabetic
(eval-when (load)
  (set-syntax-from-char #\( #\A *chunker-readtable*)
  (set-syntax-from-char #\) #\A *chunker-readtable*)
  (set-syntax-from-char #\; #\A *chunker-readtable*)
  (set-syntax-from-char #\# #\A *chunker-readtable*)
  (set-syntax-from-char #\\ #\A *chunker-readtable*)
  (set-syntax-from-char #\, #\A *chunker-readtable*)
  (set-syntax-from-char #\` #\A *chunker-readtable*)
  (set-syntax-from-char #\' #\A *chunker-readtable*)
  (set-syntax-from-char #\| #\A *chunker-readtable*)
  (set-syntax-from-char #\: #\A *chunker-readtable*)
  (set-syntax-from-char #\" #\A *chunker-readtable*)
  ;; these can be in Pnames now so the reader should ignore them
  (set-syntax-from-char #\! #\Space *chunker-readtable*)
  (set-syntax-from-char #\^ #\Space *chunker-readtable*)
  (set-syntax-from-char #\@ #\Space *chunker-readtable*)
  )

;;; Symbols and Numbers
;;; Fix to "." and "..." problems, for now. (Leigh)
;;; loses on the empty string which it shouldn't be getting
(defun safe-read-from-string (string)
  (warn "safe-read-from-string is obsolete, use read-chunk-string instead")
  (read-chunk-string string))


;;  #-lispworks '#,(find-package 'BU))) except it would still blow out the reader
(defun read-formatting-info (fi)
  (let ((*read-base* 10.)
        (*readtable* *chunker-readtable*)
        (*package* (find-package :bu)))
    (read-chunk-string
     #-symbolics
     (let* ((stop (fi-stop fi))
            (start (fi-start fi))
            (length (-& stop start))
            (st (make-string length :element-type 'character))
            (chas (fi-chas fi)))
       (do ((i 0 (1+& i)) (j start (1+& j)))
           ((>=& i length) st)
         (setf (aref (the simple-string st) i)
               (char-upcase (svref& chas j)))))
     #+symbolics
     (nstring-upcase
      (make-array
       (- (fi-stop fi) (fi-start fi))
       :element-type 'character
       :initial-contents
       (subseq (fi-chas fi)
               (fi-start fi)
               (fi-stop fi)))))))




;;;; Character Predicates

(defsubst constituent-char? (char)
  (eq (get-chunking-syntax char) 'constituent))

(defsubst whitespace-char? (char)
  (eq (get-chunking-syntax char) 'whitespace))

(defun ignore-char? (char)
  (or (whitespace-char? char)
      ;;      (comment-char-style? (char-style char))
      ))

;;fonts are no longer associated with characters
                                        ;(defun comment-char-style? (style)
                                        ;  (eql style *comment-style*))

(defsubst label-char? (char)
  (eq (get-chunking-syntax char) 'label))




;;;; Top Level Chunking
;;;  The main entry point into the chunker is CHUNK-ROW.

(defun chunk-row (row &optional pointers-only?)
  (declare (values pointers eval-objects))
  (let ((stream (make-simple-row-stream-from-row row)))
    (with-local-formatting-info (stream)
      (chunk-top-level stream pointers-only?))))


(defun chunk-row-for-eval (row)
  (multiple-value-bind (chunks evalchunks)
      (chunk-top-level (make-simple-row-stream-from-row row))
    (declare (ignore chunks))
    evalchunks))

;;;  CHUNK-ROW relies directly upon CHUNK-TOP-LEVEL which wants a simple
;;   stream as an arg.  At the moment, the simple row stream is based on the
;;   full fledged streams defined in STREAMS.  They should probably have
;;   their own representation....
;;
;;   Calling this function on a stream returns two values, a list representing
;;   the chunked Data structure and a list that the EVALUATOR can use
;;   Its main job is to coordinate
;;   chunks since they are supposed to share adjacent formatting
;;   properties (i.e. one chunk's ceiling is another chunk's floor)
;;   For now, adjacent chunk's common formatting property will be EQ.  This
;;   may not be the right thing but we are doing enough Consing as it is...
;;

(defun chunk-top-level (stream &optional pointers-only?)
  (let (chunked-list eval-chunks old-lfp)
    (do ()
        ((null (simple-peek-char t stream))
         (values (let* ((ptrs (nreverse chunked-list))
                        (last-ptr (car (last ptrs)))
                        (last-chunk (unless (null last-ptr)
                                      (get-pointer-value last-ptr nil))))
                   (if (and (not (null (cdr ptrs)))
                            (only-formatting-chunk? last-chunk))
                       ;; if there is more than one chunks AND the last
                       ;; chunk is a formatting chunk, coalesce them
                       (let* ((valid-ptrs (nbutlast ptrs))
                              (new-last-chunk (get-pointer-value
                                               (car (last valid-ptrs)) nil)))
                         (setf (chunk-right-format new-last-chunk)
                               (coalesce-fi (chunk-right-format new-last-chunk)
                                            (chunk-left-format last-chunk)
                                            (chunk-pname last-chunk)
                                            (chunk-right-format last-chunk)))
                         valid-ptrs)
                       ptrs))
                 (when (null pointers-only?)
                   (process-chunks-for-eval (nreverse eval-chunks)))))
      (multiple-value-bind (lfp pname chunk rfp label eval-props)
          (chunk stream old-lfp)
        (cond ((and (null chunk) (null eval-props))
               (push (make-pointer (make-only-formatting-chunk
                                    lfp pname chunk rfp label eval-props))
                     chunked-list))
              ((and (fast-memq 'bu::imbedded-dots eval-props)
                    (numberp chunk)) ; should this be floatp ?
               ;; a crock to get the chunk plist out of floating point chunks
               (let ((chunk (make-chunk lfp pname chunk rfp label nil)))
                 (push (make-pointer chunk) chunked-list)
                 (push chunk eval-chunks)
                 (setq old-lfp (chunk-right-format chunk))))
              (t
               (let ((chunk (make-chunk lfp pname chunk rfp label eval-props)))
                 (push (make-pointer chunk) chunked-list)
                 (push chunk eval-chunks)
                 (setq old-lfp (chunk-right-format chunk)))))))))

(defun make-only-formatting-chunk (lfp pname chunk rfp label eval-props)
  (declare (ignore chunk))
  (make-chunk lfp pname *format-only-chunk-marker* rfp label eval-props))

(defun only-formatting-chunk? (thing)
  (and (chunk-p thing) (eq (chunk-chunk thing) *format-only-chunk-marker*)))




;;; In order to cut down CONSing in the chunker and to decouple
;;; chunk values from the pnames, we use a string resource
;;; which is reset on invocations to chunk
;;;
;;; The chunk string is what gets passed to INTERN (or perhaps BOXER-INTERN
;;; if we have our own symbols).  We need to be careful with the different
;;; implementations of INTERN (including our own).  The string we are passing
;;; Must Be Copied.  Lucid 3.0 and ExCl 3.0 both have this behaviour
;;; inside INTERN.
;;;
(defvar *default-chunk-string-length* 16.)

(defvar *chunk-strings* (list (make-array *default-chunk-string-length*
                                          :fill-pointer 0
                                          :adjustable t
                                          :element-type
                                          #+(or excl lucid) 'string-char
                                          #+(or lispworks mcl symbolics sbcl ecl) 'character)))

(defun chunk-string ()
  (let ((cs (or (pop *chunk-strings*)
                (make-array *default-chunk-string-length*
                            :fill-pointer 0
                            :adjustable t
                            :element-type
                            #+(or excl lucid) 'string-char
                            #+(or lispworks mcl symbolics sbcl ecl) 'character))))
    (setf (fill-pointer cs) 0)
    cs))

(defun deallocate-chunk-string (cs)
  (push cs *chunk-strings*))

(defmacro with-chunk-string ((var) &body body)
  `(let ((,var (chunk-string)))
     (unwind-protect
          (progn . ,body)
       (deallocate-chunk-string ,var))))

(defun chunk-string-append (new-char chunk-string)
  (when (>=& (fill-pointer chunk-string) (array-dimension chunk-string 0))
    (adjust-array chunk-string (+& (array-dimension chunk-string 0) 16.)))
  (setf (aref chunk-string (fill-pointer chunk-string)) new-char)
  (incf& (fill-pointer chunk-string)))

(defun chunk-string-clear (chunk-string)
  (setf (fill-pointer chunk-string) 0)
  chunk-string)

(defun numberstring? (string &optional (start 0) (stop (length string)))
  (declare (fixnum start stop))
  (unless (=& start stop)
    (or (digit-char-p (aref string start))
        ;; might be a negative number, a decimal,
        ;; or an explicitly positive number like "+5"
        (and (or (char= (aref string start) #\-)
                 (char= (aref string start) #\+)
                 (char= (aref string start) #\.))
             (>& stop (1+& start))
             (or (digit-char-p (aref string (1+& start)))
                 ;; handles "-.3" cases
                 (and (or (char= (aref string start) #\-)
                          (char= (aref string start) #\+))
                      (char= (aref string (1+& start)) #\.)
                      (>& stop (+& start 2))
                      (digit-char-p (aref string (+& start 2)))))))))

;; this should be smarter about actually returning reasonable values for
;; common cases like 223e2323 +>

#+lcl3.0
(defmacro ignoring-number-read-errors (&body body)
  `(lcl::ignore-errors . ,body))

;; NOTE: 4/5/00 avoiding ccl::ignore-errors because it returns NIL for the chunk
;; value which causes the caller to coalesce tokens
#+ccl
(defmacro ignoring-number-read-errors (&body body)
  `(catch 'chunk-read-error
     (ccl::handler-bind ((ccl::error
                           #'(lambda (c)
                               (let ((s (if (typep c 'ccl::simple-error)
                                            (slot-value c 'ccl::format-string)
                                            "")))
                                 (cond ((string= s "Exponent overflow.")
                                        ;; not quite right, could be negative...
                                        (throw 'chunk-read-error
                                          most-positive-long-float))
                                       ((String= s "Exponent underflow.")
                                        (throw 'chunk-read-error 0))
                                       (t (throw 'chunk-read-error
                                            'chunking-error-value)))))))
       . ,body)))

#+lispworks
(defmacro ignoring-number-read-errors (&body body)
  `(catch 'chunk-read-error
     (handler-bind ((error
                      #'(lambda (c)
                          (let ((s (if (typep c 'simple-error)
                                       (slot-value c 'hcl::format-string)
                                       "")))
                            (cond ((string= s "Exponent overflow.")
                                   ;; not quite right, could be negative...
                                   (throw 'chunk-read-error
                                     most-positive-long-float))
                                  ((String= s "Exponent underflow.")
                                   (throw 'chunk-read-error 0))
                                  (t (throw 'chunk-read-error
                                       'chunking-error-value)))))))
       . ,body)))

#-(or lcl3.0 ccl lispworks)
(defmacro ignoring-number-read-errors (&body body)
  `(progn . ,body))

;;; at this point, the string is either a number or else it is a symbol
(defun read-chunk-string (string &optional
                                   (start 0)
                                   (stop (length string) stop-supplied))
  (let ((*read-base* 10.)
        (*readtable* *chunker-readtable*)
        (*package* (find-package :BU)))
    (cond ((or (zerop& (length string))
               (>=& start stop))
           nil)
          ((numberstring? string start stop)
           ;; Must be a number, bum this later, just
           ;; call read-from-string for now
           (ignoring-number-read-errors
            (if *boxer-system-hacker*
                (read-from-string string t   t   :start start :end stop)
                (read-from-string string nil nil :start start :end stop))))
          ((and (zerop& start)
                (not stop-supplied))
           ;; must be a symbol
           (intern-in-bu-package string))
          (t
           (if *boxer-system-hacker*
               (read-from-string string t   t   :start start :end stop)
               (read-from-string string nil nil :start start :end stop))))))





(defun chunk (stream &optional left-formatting-property)
  (declare (values lfp pname chunk rfp label eval-props))
  (let (;; The values that will be returned
        (lfp (or left-formatting-property 0))
        pname
        chunk
        (rfp 0)
        (label nil)
        (eval-properties nil))
    (let ((stuff-rfp? nil);; fill whitespace on the left or right ?
          (terminate? nil))
      (with-chunk-string (chunk-string)
        (flet ((fixup-chunker-returned-values ()
                 (values lfp pname
                         (if (null chunk)
                             (read-chunk-string  chunk-string)
                             chunk)
                         rfp label eval-properties)))
          (catch 'end-of-chunk
            (do ((current-char (simple-peek-char t stream nil 'eof)
                               (simple-peek-char t stream nil 'eof)))
                ((or terminate?
                     (eq current-char 'eof))
                 (if (null terminate?)
                     ;; this means we are exiting abnormally so we may have to
                     ;; fix up the returned values
                     (fixup-chunker-returned-values)
                     ;; otherwise exit normally
                     (values lfp pname chunk rfp label eval-properties)))
              (cond ((characterp current-char)
                     (multiple-value-setq (lfp pname chunk rfp
                                               label eval-properties
                                               terminate? stuff-rfp?)
                       (chunk-handle-char
                        current-char stream chunk-string stuff-rfp?
                        lfp pname chunk rfp label eval-properties)))
                    ((box? current-char)
                     ;; must be a box
                     (multiple-value-setq (lfp pname chunk rfp
                                               label eval-properties
                                               terminate? stuff-rfp?)
                       (box-chunk-handler
                        current-char stream chunk-string stuff-rfp?
                        lfp pname chunk rfp label eval-properties)))
                    (t
                     (error "Unchunkable thing, ~A is not a character or a box"
                            current-char))))))))))





;;;; Character Handlers

;;; each of these has to return (values lfp pname chunk rfp
;;;                                     label eval-properties
;;;                                     terminate? stuff-rfp?)
;;;

(defun constituent-character-handler (stream chunk-string stuff-rfp?
                                      lfp pname chunk rfp
                                      label eval-properties)
  ;; the char is a CONSTITUENT, so there are 3 possibilities...
  (cond ((not (null stuff-rfp?))
         ;; we've been stuffing whitespace into the RFP in
         ;; which case, we are done
         (throw 'end-of-chunk
           (values lfp pname chunk rfp label eval-properties)))
        ((null pname)
         ;; there is no PNAME so we make one
         ;; set the starting/stopping point of the PNAME
         ;; then move on to the next character
         (setq pname (make-local-formatting-info (stream-position stream)))
         ;; add the character to the chunk-string
         (chunk-string-append (char-upcase (simple-read-char stream))
                              chunk-string)
         (setf (fi-stop  pname) (stream-position stream))
         (values lfp pname chunk rfp label eval-properties nil stuff-rfp?))
        (t
         ;; There Is a PNAME so we just have to
         ;; move on to the next character and
         ;; set the stopping point of the PNAME
         (chunk-string-append (char-upcase (simple-read-char stream))
                              chunk-string)
         (setf (fi-stop pname) (stream-position stream))
         (values lfp pname chunk rfp label eval-properties nil stuff-rfp?))))

(defun update-fp (fp stream)
  (let ((current-char (simple-read-char stream)))
    (cond ((and (numberp fp) (eql #\space current-char))
           (1+& fp))
          ((numberp fp)
           (make-local-formatting-info (- (stream-position stream) fp)
                                       (stream-position stream)))
          (t (setf (fi-stop fp) (stream-position stream))
             fp))))

(defun whitespace-character-handler (stream chunk-string stuff-rfp?
                                     lfp pname chunk rfp
                                     label eval-properties)
  (cond ((and (not (null pname)) (null stuff-rfp?))
         ;; This is the transition case.  We've constructed
         ;; our chunk and we should now switch over to making
         ;; the RFP.  We have to push the char into the RFP,
         ;; set the RFP flag, and set the CHUNK field
         (values lfp pname (unless (=& (length chunk-string) 0)
                             (read-chunk-string chunk-string))
                 (if (eql (simple-read-char stream) #\space)
                     1
                     (make-local-formatting-info
                      (1-& (stream-position stream))
                      (stream-position stream)))
                 label eval-properties nil t))
        ((null stuff-rfp?)
         ;; and now the simple cases, either the char goes
         ;; into the RFP or the LFP
         (values (update-fp lfp stream) pname chunk
                 rfp label eval-properties nil stuff-rfp?))
        (t
         (values lfp pname chunk (update-fp rfp stream)
                 label eval-properties nil stuff-rfp?))))


(defun box-chunk-handler (current-char stream chunk-string stuff-rfp?
                          lfp pname chunk rfp
                          label eval-properties)
  (declare (ignore current-char))
  ;; If the char is a box, then either...
  (cond ((not (null stuff-rfp?))
         ;; we've been stuffing whitespace into the RFP in
         ;; which case, we are done
         (throw 'end-of-chunk
           (values lfp pname chunk rfp label eval-properties)))
        ((not (null pname))
         (cond ((not (zerop& (length chunk-string)))
                ;; the pname is not null which means we have
                ;; been making a symbol or number and then we
                ;; have run into a box. the symbol is now
                ;; complete so return it
                (throw 'end-of-chunk
                  (values lfp pname (read-chunk-string chunk-string)
                          rfp label eval-properties)))
               ((box? pname)
                (throw 'end-of-chunk
                  (values lfp pname pname rfp label eval-properties)))
               (t
                ;; pname must have eval-properties or labels
                ;; in it so make the chunk from the box
                (let ((box (simple-read-char stream)))
                  (values lfp box box rfp label eval-properties nil t)))))
        (t
         (let ((box (simple-read-char stream)))
           (values lfp box box rfp label eval-properties nil t)))))





;;;; Character Macros
;;  There are two types of Macros, Terminating and Non-Terminating.
;;  Terminating macros are ones which finish the chunk in progress.  The
;;  standard comment handler is an example of a Terminating macro.  The label
;;  character macro is an example of a non-terminating type of macro.  There
;;  is more of the chunk to accumalate AFTER the macro is finished with its
;;  processing.  The eval property macros (UNBOX and EVAL) are somewhere in
;;  between since they either signal the end of a chunk (without appending
;;  the char) or else they add themselves to the LFP and return depending upon
;;  the value of STUFF-RFP?.  These macros are called with an arglist of:
;;  (STREAM STUFF-RFP? LFP PNAME CHUNK RFP LABEL EVAL-PROPERTIES) and with
;;  the macro char still in the stream.
;;  There are 2 ways to return from a character macro call, the initial call
;;  is from within a MULTIPLE-VALUE-SETQ which is waiting to
;;  SETQ (LFP PNAME CHUNK RFP LABEL EVAL-PROPERTIES TERMINATE? STUFF-RFP?).
;;  The other method used primarily for EOF handling or terminating Macros
;;  is to (THROW 'END-OF-CHUNK
;;               (VALUES LFP PNAME CHUNK RFP LABEL EVAL-PROPERTIES))

;; This comment handler is for font-encoded
;; characters (and NOT for font change chars)

(defun comment-character-macro (stream chunk-string stuff-rfp?
                                lfp pname chunk rfp
                                label eval-properties)
  (declare (ignore stuff-rfp?))
  ;; either we stuff chars into the lfp or the rfp
  ;; can't tell from stuff-rfp? though since we may have been in the middle of
  ;; building a PNAME token when we hit the comment char
  (cond ((null pname)
         ;; everything goes into the lfp
         (when (numberp lfp)
           (setq lfp
                 (make-local-formatting-info (- (stream-position stream) lfp)
                                             (stream-position stream))))
         ;; empty out the stream
         ;; this is not quite the right thing but will suffice as long as
         ;; we ONLY have test-streams and row-streams
         (do ((char (simple-read-char stream nil 'end)
                    (simple-read-char stream nil 'end)))
             ((eq char 'end)))
         (setf (fi-stop  lfp) (stream-position stream))
         (throw 'END-OF-CHUNK
           (values lfp pname chunk 0 label eval-properties)))
        (t
         (when (numberp rfp)
           (setq rfp
                 (make-local-formatting-info (- (stream-position stream) rfp)
                                             (stream-position stream))))
         ;; empty out the stream
         ;; this is not quite the right thing but will suffice as long as
         ;; we ONLY have test-streams and row-streams
         (do ((char (simple-read-char stream nil 'end)
                    (simple-read-char stream nil 'end)))
             ((eq char 'end)))
         (setf (fi-stop rfp) (stream-position stream))
         (throw 'END-OF-CHUNK
           (values lfp pname (or chunk
                                 (if (box? pname)
                                     pname
                                     (read-chunk-string chunk-string)))
                   rfp label eval-properties)))))

;;; Eval Properties
;; there are 2 possibilities for handling these props, either there is
;; whitespace after the character or else there isn't.  If there is
;; whitespace, then the character needs to be chunked by itself.  If there
;; is a constituent character (or box) after the macro character, then the
;; character needs to appear in the PNAME of the chunk and the eval-props of
;; the chunk as well, the value of the chunk will be whatever comes after
;; the macro char

(defun unbox-character-macro (stream chunk-string stuff-rfp?
                              lfp pname chunk rfp
                              label eval-properties)
  (cond ((not (null stuff-rfp?))
         ;; if we are in the process of making the RFP, then we should
         ;; terminate the chunk leaving the macro char for the next chunk
         (throw 'END-OF-CHUNK
           (values lfp pname (or chunk
                                 (if (box? pname)
                                     pname
                                     (read-chunk-string chunk-string)))
                   rfp label eval-properties)))
        ((and (not (null pname))
              (not (zerop& (length chunk-string))))
         ;; no whitespace between the thing currently being built and the "@"
         ;; according to Leigh's proposal, this should chunk into a separate
         ;; kind of eval object to allow for user program construction of
         ;; symbols.
         ;; for now, we make it behave like the case above, and terminate the
         ;; chunk.
         (throw 'END-OF-CHUNK
           (values lfp pname (or chunk
                                 (if (box? pname)
                                     pname
                                     (read-chunk-string chunk-string)))
                   rfp label eval-properties)))
        (t
         ;; this means that we are in the process of making
         ;; an LFP and we should start making a PNAME
         (when (null pname)
           (setq pname (make-local-formatting-info (stream-position stream))))
         (simple-read-char stream)
         (setf (fi-stop  pname) (stream-position stream))
         (values lfp pname chunk rfp
                 label (append eval-properties '(bu::@)) nil stuff-rfp?))))

(defun eval-it-character-macro (stream chunk-string stuff-rfp?
                                lfp pname chunk rfp
                                label eval-properties)
  (cond ((not (null stuff-rfp?))
         ;; if we are in the process of making the RFP, then we should
         ;; terminate the chunk leaving the macro char for the next chunk
         (throw 'END-OF-CHUNK
           (values lfp pname (or chunk
                                 (if (box? pname)
                                     pname
                                     (read-chunk-string chunk-string)))
                   rfp label eval-properties)))
        ((and (not (null pname))
              (not (zerop& (length chunk-string))))
         ;; no whitespace between the thing currently being built and the "!"
         ;; according to Leigh's proposal, this should chunk into a separate
         ;; kind of eval object to allow for user program construction of
         ;; symbols.
         ;; for now, we make it behave like the case above, and terminate the
         ;; chunk.
         (throw 'END-OF-CHUNK
           (values lfp pname (or chunk
                                 (if (box? pname)
                                     pname
                                     (read-chunk-string chunk-string)))
                   rfp label eval-properties)))
        (t
         ;; this means that we are in the process of making
         ;; an LFP and we should start making a PNAME
         (when (null pname)
           (setq pname (make-local-formatting-info (stream-position stream))))
         (simple-read-char stream)
         (setf (fi-stop  pname) (stream-position stream))
         (values lfp pname chunk rfp
                 label (append eval-properties '(bu::eval-it))
                 nil stuff-rfp?))))


;;; ^
(defun previous-tell-environment-character-macro (stream chunk-string stuff-rfp?
                                                  lfp pname chunk rfp
                                                  label eval-properties)
  (cond ((not (null stuff-rfp?))
         ;; if we are in the process of making the RFP, then we should
         ;; terminate the chunk leaving the macro char for the next chunk
         (throw 'END-OF-CHUNK
           (values lfp pname (or chunk
                                 (if (box? pname)
                                     pname
                                     (read-chunk-string chunk-string)))
                   rfp label eval-properties)))
        ((and (not (null pname))
              (not (zerop& (length chunk-string))))
         ;; unlike !, it's ok for a^b to terminate the
         ;; chunk.
         (throw 'END-OF-CHUNK
           (values lfp pname (or chunk
                                 (if (box? pname)
                                     pname
                                     (read-chunk-string chunk-string)))
                   rfp label eval-properties)))
        (t
         ;; this means that we are in the process of making
         ;; an LFP and we should start making a PNAME
         (when (null pname)
           (setq pname (make-local-formatting-info (stream-position stream))))
         (simple-read-char stream)
         (setf (fi-stop  pname) (stream-position stream))
         (values lfp pname chunk rfp
                 label (append eval-properties '(bu::previous-tell-environment))
                 nil stuff-rfp?))))




;;; Dots

;; this is basically like a constituent character handler except
;; that there is additional processing for a field in the eval-properties
;; a preceeding DOT or a trailing DOT causes the entire chunk to be treated
;; as a pname of a symbol.
(defun dot-character-macro (stream chunk-string stuff-rfp?
                            lfp pname chunk rfp
                            label eval-properties)
  ;; the char is a CONSTITUENT, so there are 3 possibilities...
  (cond ((not (null stuff-rfp?))
         ;; we've been stuffing whitespace into the RFP in
         ;; which case, we are done
         (throw 'end-of-chunk
           (values lfp pname chunk rfp label eval-properties)))
        ((null pname)
         ;; there is no PNAME so we make one
         ;; set the starting/stopping point of the PNAME
         ;; then move on to the next character
         (setq pname (make-local-formatting-info (stream-position stream)))
         ;; add the character to the chunk-string
         (chunk-string-append (char-upcase (simple-read-char stream))
                              chunk-string)
         (setf (fi-stop  pname) (stream-position stream))
         ;; if there is a leading dot, record this info
         (values lfp pname chunk rfp label
                 (append eval-properties '(bu::starting-dot)) nil stuff-rfp?))
        (t
         ;; There Is a PNAME so we just have to
         ;; move on to the next character and
         ;; set the stopping point of the PNAME
         (when (zerop& (length chunk-string))
           ;; we STILL need to check for a leading dot EVEN when there is
           ;; a pname because the pname might only include things like
           ;; @'s or labels
           (setq eval-properties (append eval-properties '(bu::starting-dot))))
         (chunk-string-append (char-upcase (simple-read-char stream))
                              chunk-string)
         (setf (fi-stop pname) (stream-position stream))
         (values lfp pname chunk rfp label
                 (let ((next-char (simple-peek-char t stream nil 'eof)))
                   (cond ((or (fast-memq 'bu::starting-dot eval-properties)
                              (eq next-char 'eof)
                              (box? next-char)
                              (not (eq (get-chunking-syntax next-char)
                                       'constituent)))
                          ;; if we have either a leading or trailing dot
                          ;; then we need to remove any dots properties
                          (fast-delq 'bu::imbedded-dots eval-properties))
                         ((fast-memq 'bu::imbedded-dots eval-properties)
                          eval-properties)
                         (t
                          ;; otherwise, we record finding imbedded dots
                          (append eval-properties '(bu::imbedded-dots)))))
                 nil stuff-rfp?))))



;;; Labels

(defun label-character-macro (stream chunk-string stuff-rfp?
                              lfp pname chunk rfp
                              label eval-properties)
  (cond ((null pname)
         ;; no label has been seen yet, this can only occur if the label char
         ;; comes before any constituent in the line.  We treat this situation
         ;; as having a NULL label, stuff the char into the LFP and continue
         (update-fp lfp stream)
         (values lfp pname chunk rfp label eval-properties nil stuff-rfp?))
        (t
         ;; Major rearranging is needed.
         ;; get the label from the chunk string
         (if (null label)
             (setq label (if (box? pname) pname
                             (read-chunk-string chunk-string)))
             ;; how to handle multiple labels, that is the question...
             (setq label (intern-in-bu-package
                          (concatenate 'string
                                       (cond ((numberp label)
                                              (format nil "~A" label))
                                             (t (string label)))
                                       chunk-string))))
         ;; clear the chunk-string
         (chunk-string-clear chunk-string)
         (simple-read-char stream)	;read the label char out
         ;; now we need to concatenate the label and the already existing LFP
         (if (numberp lfp)
             (setq lfp (make-local-formatting-info
                        (- (stream-position stream)
                           lfp
                           (if (box? pname) 1 (fi-length pname)) 1)
                        (stream-position stream)))
             (setf (fi-stop lfp) (stream-position stream)))
         (values lfp nil nil rfp label eval-properties nil nil))))





;;;; Converting to Evaluator Expression
;;   The basic operation is to return the CHUNK.  If there is an Evaluator
;;    Property, then it is converted into an evaluator property structure
;;   Multiple Properties are parsed into: ?????


                                        ; old stuff
                                        ;(defun parse-entries-for-eval (entries)
                                        ;  (mapcar #'chunk-for-eval entries))

(defvar *inhibit-chunker-style-warnings?* nil)

;;; this might want to be both more informative to the user and
;;; print somewhere where it can be seen (like the status line)
;;; this is for things like "!@" which is probably NOT what anyone wants
;;; as opposed to "@!" which is perfectly reasonable (I think)
(defmacro chunker-style-warning (format-string &rest args)
  `(unless *inhibit-chunker-style-warnings?*
     (warn ,format-string . ,args)))

(defun get-dots-values (symbol)
  (flet ((substring (string start &optional (stop (length string)))
           (declare (fixnum start stop))
           ;; have to be careful and see if this will
           ;; work in every implementation
           #-ccl-3
           (make-array (-& stop start)
                       :element-type (array-element-type string)
                                        ;		       #-(or mcl symbolics lispworks) 'string-char
                                        ;		       #+(or mcl symbolics lispworks) 'character
                       :displaced-to string
                       :displaced-index-offset start)
           #+ccl-3
           (subseq string start stop)))
    (cond ((not (symbolp symbol))
           (when *boxer-system-hacker*
             (cerror "oh well, keep going..." "~S isn't a symbol" symbol))
           (list symbol))
          (t
           (let ((string (symbol-name symbol))
                 (old-pos 0))
             (let ((values-list nil))
               (do ((dot-pos (position #\. string)
                             (position #\. string :start old-pos)))
                   ((null dot-pos)
                    (let ((ls (substring string old-pos)))
                      (if (digit-char-p (aref ls 0))
                          (setq values-list (list symbol))
                          (push (intern-in-bu-package ls) values-list))))
                 (let ((ss (substring string old-pos dot-pos)))
                   (cond ((string= ss ""))
                         ((digit-char-p (aref ss 0))
                          (setq values-list (list symbol))
                          (return))
                         (t
                          (push (intern-in-bu-package ss) values-list))))
                 (setq old-pos (1+& dot-pos)))
               (nreverse values-list)))))))

(defvar *eval-props-to-ignore* '(bu::starting-dot))

;;; this is also used inside of build
(defun handle-eval-props (eval-props value &optional (excl-prop? nil))
  (cond ((null eval-props)
         (if (and (null value) (not (null excl-prop?)))
             'bu::something-was-supposed-to-follow-an-excl
             value))
        ((eq (car eval-props) 'bu::@)
         (when (not (null excl-prop?))
           (chunker-style-warning "An @ was seen after a !"))
         (make-eval-prop 'bu::@
                         (handle-eval-props
                          (cdr eval-props)
                          (coerce-object-for-evaluator value)
                          t)))
        #|
        (push 'bu::@ eval-objs)
        (handle-eval-props (cdr eval-props) value excl-prop?)
        |#
        ((eq (car eval-props) 'bu::eval-it)
         (make-eval-prop 'bu::eval-it
                         (handle-eval-props (cdr eval-props)
                                            value
                                            t)))
        ((eq (car eval-props) 'bu::previous-tell-environment)
         (make-eval-prop 'bu::previous-tell-environment
                         (handle-eval-props (cdr eval-props)
                                            value
                                            t)))
        ((eq (car eval-props) 'bu::imbedded-dots)
         (if (numberp value)
             value
             (make-eval-prop 'bu::dots-list
                             (get-dots-values value))))
        (t
         ;; at worse, ignore the offending property an maybe warn the user
         (when (and *boxer-system-hacker*
                    (not (member (car eval-props) *eval-props-to-ignore*)))
           (warn "~A was an unrecognized eval property" (car eval-props)))
         value)))

(defun process-chunks-for-eval (chunks)
  (let ((eval-objs nil))
    (do* ((chunks-to-go chunks (cdr chunks-to-go))
          (chunk (car chunks-to-go) (car chunks-to-go))
          (next-chunk (cadr chunks-to-go) (cadr chunks-to-go)))
         ((null chunks-to-go) (nreverse eval-objs))
      (let ((chunk-eval-props (getf (chunk-plist chunk) :eval-prop))
            (value (chunk-chunk chunk)))
        (cond ((only-formatting-chunk? chunk))
              ((null chunk-eval-props)
               (push (coerce-object-for-evaluator (chunk-chunk chunk))
                     eval-objs))
              ;; handle NIL values that the chunker might return when there
              ;; are eval properties delimited by whitespace
              ((and (null value)
                    (not (null next-chunk)))
               (setf (getf (chunk-plist next-chunk) :eval-prop)
                     (append chunk-eval-props
                             (getf (chunk-plist next-chunk) :eval-prop))))
              ;; now handle various permutations of eval properties
              (t (push (handle-eval-props chunk-eval-props
                                          (chunk-chunk chunk))
                       eval-objs)))))))

;;; this is just like process-chunks-for-eval except that it doesn't
;;; assume the list it is passed will ONLY be chunks

(defun process-pointer-values-for-eval (ptr-vals)
  (let ((eval-objs nil))
    (do* ((ptr-vals-to-go ptr-vals (cdr ptr-vals-to-go))
          (value (car ptr-vals-to-go) (car ptr-vals-to-go))
          (next-value (cadr ptr-vals-to-go) (cadr ptr-vals-to-go)))
         ((null ptr-vals-to-go) (nreverse eval-objs))
      (let* ((chunk-p (chunk-p value))
             (chunk-eval-props (when chunk-p
                                 (getf (chunk-plist value) :eval-prop)))
             (value (if chunk-p (chunk-chunk value) value)))
        (cond ((and chunk-p (only-formatting-chunk? value)))
              ((and (simple-vector-p value)
                    (eq (svref value 0) 'boxer-eval::SPECIAL-EVAL-TOKEN))
               (push value eval-objs))
              ((null chunk-eval-props)
               (push (coerce-object-for-evaluator value) eval-objs))
              ;; handle NIL values that the chunker might return when there
              ;; are eval properties delimited by whitespace
              ((numberp value)
               ;; a crock to skip chunk-plist processing if we have a number
               (push value eval-objs))
              ((and (null value)
                    (not (null next-value)))
               (setf (getf (chunk-plist next-value) :eval-prop)
                     (append chunk-eval-props
                             (getf (chunk-plist next-value) :eval-prop))))
              ;; now handle various permutations of eval properties
              (t (push (handle-eval-props chunk-eval-props value)
                       eval-objs)))))))

;;; this digests any editor structure and insures that the evaluator NEVER
;;; sees an  editor box.  (Actually, the evaluator does see them, but
;;; only in places that it decides to)
(defun coerce-object-for-evaluator (object)
  (cond ((or (symbolp object) (numberp object)) object)
        ((and (boxer-eval::eval-object? object)
              (or (fast-eval-data-box? object)
                  (fast-eval-doit-box? object)
                  (fast-eval-port-box? object)))
         object)
        ;;	((doit-box? object) (if (null (name-row object))
        ;;				(boxer-eval::convert-doit-to-eval-object object)
        ;;				'boxer-eval::*ignoring-definition-object*))
        ;;	((sprite-box? object) (port-to object)) ; now hacked in the evaluator
        ((port-box? object) (if (null (name-row object))
                                (make-virtual-port-from-editor-port object)
                                'boxer-eval::*ignoring-definition-object*))
        ((box? object) (if (null (name-row object))
                           (top-level-virtual-copy-editor-box object nil t)
                           'boxer-eval::*ignoring-definition-object*))
        (t (error "Don't know how to coerce ~A for the Evaluator" object))))

#| (defun chunk-for-eval (chunk)
  (let ((props (getf (chunk-plist chunk) ':eval-prop)))
    (if (null props)
    (coerce-object-for-evaluator (chunk-chunk chunk))
    (make-eval-prop-thing props
                  (coerce-object-for-evaluator
                   (chunk-chunk chunk)))))) |#



;;;; Messages to EDITOR OBJECTS about Virtual Copy Structure
;;
;;   Virtual Copy Structures in the editor are arranged in a 2 level cache.
;;   The first level occurs at the level of EDITOR-ROWS and caches the results
;;   of calling the CHUNKER on the editor row.  The CHUNKER generates 2
;;   representations of a row.  One is a data structure representation which
;;   contains all the information (whitespace, CasE, comments, etc) needed to
;;   reconstruct the visual representation of the row.  The other
;;   representation is a semantic representation which contains ONLY the
;;   information in the row neccessary for the Evaluator (and also arithmetic)
;;   to process the row. These are flushed whenever the EDITOR-ROW is modified.
;;
;;   The second level of caching occurs at the level of Boxes (both
;;   EDITOR-BOX's and Virtual Copies).  An EDITOR-BOX retains a cache of the
;;   different versions of its inferior EVROWs. The diferent versions are
;;   indexed by timestamp and the entire cache can be flushed when
;;   the EDITOR-BOX is modified, but ONLY when we know that we are NOT in the
;;   Middle of an Evaluation (because there may be outstanding virtual copies
;;   to either the Box itself or its superiors during an Evaluation).  When a
;;   Virtual Copy is made of a box, the Virtual Copy caches the semantic
;;   representation of the inferior rows.  This info is cached at the
;;   Box (Virtual Copy) level because only at this level are we guaranteed
;;   Uniqueness of the immediate inferiors.  This cache is flushed whenever
;;   the Virtual Copy is Modified [Perhaps the cache should be fixed in some
;;   cases--needs more analysis of the time tradeoffs involved here (speeding
;;   up all matrix arithmetic and RUN versus slowing down every
;;   datamanipulator)]
;;
;;
;; NOTES:
;;
;; what about funs that let you edit live structure in the middle of an EVAL
;; (concrete IO) ?
;; --EXIT or ACTIVATE or whatever, needs to call CHANGE-VC-ROWS and we can't
;; use the MODIFIED message to flush the cache anymore because we may have
;; active VC's in the middle
;; of an edit which sends lots of MODIFIED's
;;
;; what about REDISPLAY in the middle of an EVAL
;;
;; The top level Evaluation process needs to BIND the variable
;; *EVAL-IN-PROGRESS* in order to flush obsolete copies.
;; the MODIFIED method also flushes but we need to patch the editor structure
;; mutation methods to prevent the cache from being flushed IF we turn
;; *MUTATE-EDITOR-BOXES-DURING-EVAL?* on.  For now,
;; *MUTATE-EDITOR-BOXES-DURING-EVAL?* will be T because so we don't have
;; to worry about this.
;;



;;; Row methods

;; the cached? slot can have 3 possible values,
;; NIL (nothing is cached), 'ALL (everything is cached), and 'ONLY-CHUNKS

(defmethod cache-chunk-result ((self row) &optional pointers-only?)
  (multiple-value-bind (chunks eval-objs)
      (chunk-row self pointers-only?)
    (setf (slot-value self 'cached-chunks) chunks)
    (cond ((null pointers-only?)
           (dolist (obj (slot-value self 'cached-eval-objs))
             ;; there may be copies of non GC-able structures in the
             ;; eval-objs cache, we need a chance to deallocate them to
             ;; plug various memory leaks
             (when (virtual-copy? obj)
               (let ((gr (vc-graphics obj)) (gs nil))
                 (when (and gr (setq gs (graphics-info-graphics-sheet gr))
                            (graphics-sheet-bit-array gs))
                   (ogl-free-pixmap (graphics-sheet-bit-array gs))))))
           (setf (slot-value self 'cached-eval-objs) eval-objs)
           (setf (slot-value self 'cached?) 'all))
          (t
           (setf (slot-value self 'cached?) 'only-chunks)))))

(defmethod chunks ((row row) &optional pointers-only?)
  (if (cached? row)
      (slot-value row 'cached-chunks)
      (progn
        (cache-chunk-result row pointers-only?)
        (slot-value row 'cached-chunks))))

(defmethod eval-objs ((row row))
  (if (eq (slot-value row 'cached?) 'all)
      (slot-value row 'cached-eval-objs)
      (progn
        (cache-chunk-result row)
        (slot-value row 'cached-eval-objs))))





;;;; Tests
;;   A standardized set of simple tests for the chunker so we can easily
;;   check out modifications.  A Neccessary but insufficient set...

(defvar *print-chunker-test-times-only?* nil)

(defun print-chunker-test-results (description time chunks eval-objs contents
                                   &optional (stream *standard-output*))
  (format stream "~%~%~A took ~A ~A,"
          description time #+lucid "Microseconds" #+excl milliseconds
          #-(or excl lucid) 'internal-time-units)
  (unless *print-chunker-test-times-only?*
    (format stream "~%The contents are:   ~S" contents)
    (format stream  "~%The chunks are:")
    (pretty-print-chunks chunks stream)
    (format stream "~%The Eval Objects are: ~A" eval-objs)))

(defmacro defchunkertest (description-string category &rest contents)
  (let ((name (gensym)))
    `(progn
       (defun ,name ()
         (let ((stream (make-test-stream . ,contents))
               (time 0))
           (with-local-formatting-info (stream)
             (let ((start-time (get-internal-real-time)))
               (multiple-value-bind (chunks eval-objs)
                   (chunk-top-level stream)
                 (setq time (- (get-internal-real-time) start-time))
                 (print-chunker-test-results ,description-string
                                             time chunks eval-objs
                                             ',contents))))))
       (push ',name (get 'general 'chunker-tests))
       ,(if (consp category)
            `(dolist (cat ',category)
               (push ',name (get cat 'chunker-tests)))
            `(push ',name (get ',category 'chunker-tests))))))

(defun test-chunker (&rest contents)
  (let ((stream (apply #'make-test-stream contents))
        (time 0))
    (with-local-formatting-info (stream)
      (let ((start-time (get-internal-real-time)))
        (multiple-value-bind (chunks eval-objs)
            (chunk-top-level stream)
          (setq time (- (get-internal-real-time) start-time))
          (print-chunker-test-results "testing"
                                      time chunks eval-objs contents))))))

(defun pretty-print-chunks (pointers stream)
  (dolist (ptr pointers)
    (pretty-print-chunk (pointer-value-internal ptr)stream)))


(defvar *pretty-printing-chunk-separator* "~%")

(defun pretty-print-chunk (c &optional (stream *standard-output*))
  (format stream *pretty-printing-chunk-separator*)
  ;; start with the left formatting information
  (format stream "\"")
  (do-fi-chas (cha (chunk-left-format c)) (format stream "~A" cha))
  ;; then the chunk and it's pname
  (cond ((only-formatting-chunk? c)
         (format stream "FORMATTING ONLY CHUNK"))
        (t
         (if (formatting-info? (chunk-pname c))
             (do-fi-chas (cha (chunk-pname c)) (format stream "~A" cha))
             (format stream "~A" (chunk-pname c)))
         (format stream "(~A)" (chunk-chunk c))))
  ;; then the right formatting info
  (do-fi-chas (cha (chunk-right-format c)) (format stream "~A" cha))
  (format stream "\"")
  ;; then the Plist if there is one
  (unless (null (chunk-plist c))
    (format stream "  Plist: ~A" (chunk-plist C))))

(defun do-chunker-tests (&optional (category 'general))
  (dolist (test (reverse (get category 'chunker-tests))) (funcall test)))



;;; Set up the initial chunking table
(eval-when (load)
  (setq *initial-chunking-syntax-table* (make-chunk-table))

  ;; Set syntax for the standard whitespace characters
  (dolist (w *standard-whitespace-characters*)
    (setf (get-chunking-syntax w *initial-chunking-syntax-table*)
          'whitespace))

  ;; Anything that is not WHITESPACE is CONSTITUENT (or MACRO but macros
  ;; are handled separately from, and BEFORE the syntax table is referenced)
  (dotimes (i *implementation-dependent-largest-reasonable-char-code*)
    (unless (member i *standard-whitespace-characters*
                    :test #'(lambda (x c) (= x (char-code c))))
      (setf (get-chunking-syntax (code-char i) *initial-chunking-syntax-table*)
            'constituent)))

  ;; now define the standard character macros
  (setf (get-chunking-syntax *initial-comment-char*
                             *initial-chunking-syntax-table*)
        'comment-character-macro)
  (setf (get-chunking-syntax *initial-returned-value-char*
                             *initial-chunking-syntax-table*)
        'comment-character-macro)
  (setf (get-chunking-syntax *initial-eval-char*
                             *initial-chunking-syntax-table*)
        'eval-it-character-macro)
  (setf (get-chunking-syntax *initial-previous-tell-environment-char*
                             *initial-chunking-syntax-table*)
        'previous-tell-environment-character-macro)
  (setf (get-chunking-syntax *initial-unbox-char*
                             *initial-chunking-syntax-table*)
        'unbox-character-macro)
  (setf (get-chunking-syntax *initial-label-char*
                             *initial-chunking-syntax-table*)
        'label-character-macro)
  (setf (get-chunking-syntax *initial-dot-char*
                             *initial-chunking-syntax-table*)
        'dot-character-macro)

  ;; Finally copy the untouchable table to one that everyone uses
  (setq *chunking-syntax-table*
        (copy-chunk-table *initial-chunking-syntax-table*))

  )


;;; Some tests

#|

(defvar *test-box* (make-box '(())))



;;; basics
(defchunkertest "Symbol no spaces" basic "foobar")
(defchunkertest "Symbol preceeding spaces" basic "  foobar")
(defchunkertest "Symbol trailing spaces" basic "foobar ")
(defchunkertest "Symbol bounding spaces" basic " foobar  ")

(defchunkertest "2 symbols" basic "foo bar")
(defchunkertest "2 symbols (a)" basic "foo   bar")
(defchunkertest "2 symbols (b)" basic " foo bar ")

(defchunkertest "1 box" basic *test-box*)
(defchunkertest "2 boxes" basic *test-box* *test-box*)
(defchunkertest "2 boxes + space " basic *test-box* " " *test-box*)

(defchunkertest "box, symbol (a)" basic *test-box* " asd")
(defchunkertest "box, symbol (b)" basic *test-box* "asd")
(defchunkertest "symbol, box (a)" basic "asf " *test-box*)
(defchunkertest "symbol, box (b)" basic "asf" *test-box*)



(DEFCHUNKERTEST "Beginning label no spaces" LABEL "Foo:bar")
(DEFCHUNKERTEST "Beginning label trailing space" LABEL "Foo: bar ")
(DEFCHUNKERTEST "Beginning label preceding space" LABEL "Foo :bar ")
(DEFCHUNKERTEST "Beginning Label all preceeding & trailing spaces" LABEL
  "Foo : Bar")
(DEFCHUNKERTEST "Beginning Label Multiple spaces" LABEL " Foo  :  bar")
(DEFCHUNKERTEST "Beginning Label, box element, no spaces" LABEL
  " Foo:" *test-box*)
(DEFCHUNKERTEST "Beginning Label & box element" LABEL
  "Foo :" *test-box* " ")
(DEFCHUNKERTEST "Beginning label, box, multiple spaces" LABEL
  "foo  :  " *test-box*)
(DEFCHUNKERTEST "Trailing label" LABEL *test-box* " wow label:stuff")
(DEFCHUNKERTEST "middle label, box no spaces" LABEL
  "asd " *test-box* "label:stuff end")
(DEFCHUNKERTEST "Box label" LABEL "as " *test-box* " :stuff asd")
(DEFCHUNKERTEST "Box label & element" LABEL
  "asfda " *test-box* ":" *test-box* " end")
(DEFCHUNKERTEST "Box label & element, multiple spaces" LABEL
  "foo   :   end")
(DEFCHUNKERTEST "Label with no element" LABEL "stuff label: ")
(DEFCHUNKERTEST "Colon, No label" LABEL "  : element")
(DEFCHUNKERTEST "Initial colon" LABEL ": element")

(DEFCHUNKERTEST "Comment no spaces" COMMENT " foo;comment")
(DEFCHUNKERTEST "Label and comment" (LABEL COMMENT)
  "asd label: wow ; and a comment")
(DEFCHUNKERTEST "comment with box" COMMENT
  "a box ; in the " *test-box* " comment")
(DEFCHUNKERTEST "Label & comment No spaces" (LABEL COMMENT)
  "label:item;comment")

(DEFCHUNKERTEST "Multiple properties, no spaces" EVAL-PROP "!@foo")
(DEFCHUNKERTEST "Multiple properties, multiple spaces" EVAL-PROP " !  @ wow ")
(DEFCHUNKERTEST "Multiple properties, intervening label" (LABEL EVAL-PROP)
  "! label : @ foo ")
(DEFCHUNKERTEST "Multiple properties with label" (LABEL EVAL-PROP)
  "!@label:stuff")
(DEFCHUNKERTEST "Label, then multiple properties" (LABEL EVAL-PROP)
  "label : @! foo ")


|#
