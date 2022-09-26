;-*- mode:lisp; syntax:common-lisp;  package:boxer -*-
#|

 $Header: fildfs.lisp,v 1.0 90/01/24 22:11:44 boxer Exp $

 $Log:	fildfs.lisp,v $
;;;Revision 1.0  90/01/24  22:11:44  boxer
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


    Macro Definitions and Variable Declarations for the BOXER File system


Modification History (most recent at the top)

10/02/13 added bin-op-capogi-font
11/02/09 boxer-file-contents? handles possibility of an empty file
         deffile-word-reader has to check and pass eof-errorp, value to make it work
 4/01/09 constants for UTF-8 codec utilities
 9/06/05 platform specific network REQUIRE's moved here from dumper
 8/23/05 added *strict-box-paths*
10/27/03 with-hilited-box added
10/07/03 added 4 x #\Null to *possible-boxer-file-mac-types* so file-type can
         detect boxer files on OSX since the null type seems to be OSX default
         also check for pathname-type = "box" since that is the accepted OSX
         paradigm
 2/16/03 merged current LW and MCL files
 2/14/01 merged current LW and MCL files
 8/30/99 changed internal-dumping-error to accept an optional reason
 4/20/99 added *warn-about-outlink-ports*
 5/27/98 with-post-load-autoloading ignores errors in autoloading sub boxes so
         that, at least the superior box will get loaded
 5/26/98 added *dumping-font-alist* to writing-bin-stream to support font
         spec dumping
 5/18/98 Start logging changes: source = boxer version 2.2r4

|#

(in-package :boxer)




;******************************************************************************
;*                              TOP  LEVEL  DEFINITIONS                       *
;******************************************************************************

;;;; Pathname Construction and manipulation...

#+lispm
(fs:define-canonical-type :box "Box"	;default type for SAVE/READ
  (:tops-20 "Box")
  (:unix42 "box")
  (:vms "Box")
  (:its "Box"))

#+lispm
(defprop :box 16. :binary-file-byte-size)

;;;; Resources

(defmacro define-resource (name parameters &rest plist)
  (if (oddp (length plist))
    (error "Odd number of elements in the Plist, ~S" plist)
    (let ((con (getf plist :constructor))
          (dummy-name (gensym))
          (init (getf plist :initializer)))
      (when (null con)
        (warn "There isn't a :constructor defined here, ~
                 assuming (MAKE-~A) is the right thing"
              name)
        (setq con (list (intern (symbol-format nil "MAKE-~A" name)))))
      `(progn
        (setf (get ',name 'resource) t)
        (defun ,dummy-name ()
          (,@con ,@parameters))
        (setf (get ',name 'resource-maker) #',dummy-name)
        (setf (get ',name 'initializer) ,init)
        (setf (get ',name 'available-resources)
              (let ((ar nil))
                (dotimes (i ,(or (getf plist :initial-copies) 1))
                  (push (funcall (get ',name 'resource-maker)) ar))
                ar))))))

(defsubst allocate-resource (name)
  (if (get name 'resource)
    (let ((r (or (pop (get name 'available-resources))
                 (funcall (get name 'resource-maker))))
          (i (get name 'initializer)))
      (unless (null i) (funcall i r))
      r)
    (error "~S is not a defined resource" name)))

(defsubst deallocate-resource (name old-resource)
  (push old-resource (get name 'available-resources)))

(defmacro using-resource ((var resource) &body body)
  `(let ((.resource-thing. (allocate-resource ',resource)))
     (unwind-protect
      (let ((,var .resource-thing.))
        . ,body)
      (deallocate-resource ',resource .resource-thing.))))



;;initializations...

(defvar *boxer-pathname-default* (user-homedir-pathname)
  "Default directory pathname for saving boxer files")

(defvar *init-file-specifier* (merge-pathnames "boxer-init.box"
                                               *boxer-pathname-default*)
  "The default name of the initial Boxer world load. ")

(defvar *sticky-file-defaulting?* t
  "A switch to make the default filename the last pathname that was used. ")

;; be like EMACS when we backup files
(defvar *file-backup-suffix* "~")


(defvar *file-system-verbosity* t)

(defvar *current-file-length* 0)

(defvar *file-status-line-update-function* nil
  "A hook for printing stuff in the status line during loads. Should be
   a function with one arg, the stream the box is being loaded from")

(defvar *current-dumping-box* nil
  "Used to generate a context for reporting errors'")

(defvar *file-properties-table* (make-hash-table :test #'equal))

;;; BINARY file format...
;;; Commands are in the form of 16. bit numbers
;;; The top four bits in a command make up a limited number of immediate
;;; op-codes in which the next 12. bits make up an immediate argument for
;;; that 16 bit word. The four bit command code can escape to a more specific
;;; box commands and another four bit sequence escapes to general commands
;;; in the next word

;******************************************************************************
;*                                    DEFINITIONS                             *
;******************************************************************************

;;; UTF-8 Constants
(defconstant %utf-8-2byte-start #b11000000)
(defconstant %utf-8-3byte-start #b11100000)
(defconstant %utf-8-4byte-start #b11110000)
(defconstant %utf-8-more-bytes  #b10000000)

(defvar %utf-8-last-byte-bytespec (byte 6 0))
(defvar %utf-8-2byte-1stbyte-src-bytespec (byte 5 6))
(defvar %utf-8-2byte-1stbyte-dst-bytespec (byte 5 0))

(defvar %utf-8-3byte-1stbyte-src-bytespec (byte 4 12))
(defvar %utf-8-3byte-1stbyte-dst-bytespec (byte 4 0))
(defvar %utf-8-3byte-2ndbyte-src-bytespec (byte 6 6))
(defvar %utf-8-3byte-2ndbyte-dst-bytespec (byte 6 0))

(defvar %utf-8-4byte-1stbyte-src-bytespec (byte 3 18))
(defvar %utf-8-4byte-1stbyte-dst-bytespec (byte 3 0))
(defvar %utf-8-4byte-2ndbyte-src-bytespec (byte 6 12))
(defvar %utf-8-4byte-2ndbyte-dst-bytespec (byte 6 0))
(defvar %utf-8-4byte-3rdbyte-src-bytespec (byte 6 6))
(defvar %utf-8-4byte-3rdbyte-dst-bytespec (byte 6 0))

(defvar %utf-8-1byte-id-bytespec (byte 1 7))
(defvar %utf-8-2byte-id-bytespec (byte 3 5))
(defvar %utf-8-3byte-id-bytespec (byte 4 4))
(defvar %utf-8-4byte-id-bytespec (byte 5 3))
(defvar %utf-8-more-byte-id-bytespec (byte 2 6))

(defconstant %utf-8-1byte-id-value 0)
(defvar %utf-8-2byte-id-value (ldb %utf-8-2byte-id-bytespec %utf-8-2byte-start))
(defvar %utf-8-3byte-id-value (ldb %utf-8-3byte-id-bytespec %utf-8-3byte-start))
(defvar %utf-8-4byte-id-value (ldb %utf-8-4byte-id-bytespec %utf-8-4byte-start))
(defvar %utf-8-more-byte-id-value (ldb %utf-8-more-byte-id-bytespec
                                            %utf-8-more-bytes))

;;; Opcode definitions
(eval-when (compile load eval)
(defvar %%bin-op-high (byte #o4 #o14))
)
(defvar %%bin-op-low (byte #o14 #o0))

;;; other useful byte specifiers
(defvar %%bin-op-low-half (byte #o10 #o0))
(defvar %%bin-op-top-half (byte #o10 #o10))
(defvar %%bin-im-arg-msb (byte #o1 #o13))

(defvar %%bin-op-char-code (byte #o10 #o0))
(defvar %%bin-op-im-cha-bits (byte #o4 #o10))
;; should be able to crank the control bits up to 4
;; now that we've flushed fonts from characters
;(defconstant %%bin-op-im-cha-style (byte #o2 #o12))
;(defconstant %%bin-op-fat-cha-bits (byte #o2 #o10))
;(defconstant %%bin-op-fat-cha-style (byte #o3 #o12))
;(defconstant %%bin-op-fat-cha-family (byte #o3 #o15))

(defconstant %%bin-op-odd-char-placeholder-code 255)

(defvar %%bin-op-im-arg-size (ash 1 12.))
(defvar %%bin-op-arg-size (ash 1 16.))
(defvar %%bin-op-im-number-size (ash 1 11.))

;;; Currently supported version number
(defvar *version-number* 13.)

;;; Dumping variables

(defvar *bin-dump-table*)
(defvar *bin-dump-index*)
(defvar *bin-dump-package*)
(defvar *dumped-box-table*)

(defvar *outermost-dumping-box* NIL
  "The top level box which is being dumped. ")

;;; is there ever a reason to use this ?
(defvar *restore-turtle-state* t
  "Determines if the state of turtle boxes should be saved. ")

(defmacro make-bin-op-dispatch-table ()
  `(make-array #o100))

(defmacro bin-op-dispatch (table number)
  `(aref ,table ,number))

(defmacro store-bin-op-dispatch (value table number)
  `(setf (aref ,table ,number) ,value))

;; AppGen lossage for complex form of defsetf
#+mcl
(defun %set-bin-op-dispatch (table number value)
  (store-bin-op-dispatch value table number)
  value)

#+mcl
(defsetf bin-op-dispatch %set-bin-op-dispatch)

#-mcl
(defsetf bin-op-dispatch (table number) (value)
  `(store-bin-op-dispatch ,value ,table ,number))

;; so we can get the commands from their number format and vice versa
(defvar *bin-op-command-name-table* (make-bin-op-dispatch-table))

(defmacro define-bin-op (name value index)
  `(progn
     (defconstant ,name ,value)
     (setf (bin-op-dispatch *bin-op-command-name-table* ,index) ',name)))

(defun decode-bin-op (bin-op-number)
  (aref *bin-op-command-name-table* bin-op-number))



;;; immediate commands.  The meaning of the 16 bit arg is
;;; specified in the comment
(defmacro define-immediate-bin-op (name value)
  `(define-bin-op ,name ,value ,value))

(define-immediate-bin-op bin-op-number-immediate #o0)	   ;<number>
(define-immediate-bin-op bin-op-table-fetch-immediate #o1) ;<table address>
(define-immediate-bin-op bin-op-cha-immediate #o2)	   ;<character>
(define-immediate-bin-op bin-op-box-immediate #o3)	   ;<box type>
(define-immediate-bin-op bin-op-string-immediate #o4)      ;<string length>
(define-immediate-bin-op bin-op-list-immediate #o5)	   ;<list length>
(define-immediate-bin-op bin-op-array #o6)	           ;number of options
(define-immediate-bin-op bin-op-row-immediate #o7)   	   ;number of chas
(define-immediate-bin-op bin-op-name-and-input-row-immediate #o10) ; # of chas
(define-immediate-bin-op bin-op-name-row-immediate #o11)
(define-immediate-bin-op bin-op-multi-simple-chas #o12)    ; number of chars
(define-immediate-bin-op bin-op-command-immediate #o17)	   ;<command>

;;; specific box commands
(defmacro define-box-bin-op (name value)
  `(define-bin-op ,name
       ,(dpb bin-op-box-immediate %%bin-op-high value)
     ,value))

(define-box-bin-op bin-op-doit-box #o20)
(define-box-bin-op bin-op-data-box #o21)
(define-box-bin-op bin-op-port-box #o22)

;; #o24 and #o25 were used for old style turtle boxes

;; #o26 was for local library boxes
;;(define-box-bin-op bin-op-ll-box #o26)

;; #o23 and #o31 were for graphics boxes and graphics-data-boxes

;; sprite boxes are now obsolete, but leave this hear for old file compatibility
(define-box-bin-op bin-op-sprite-box #o32)

;; for compatibility with pre version 4.0 files
;; (define-box-bin-op bin-op-ll-box-prescence-marker #o27)

;;; Other commands
(defmacro define-command-bin-op (name value)
  `(define-bin-op ,name ,(dpb bin-op-command-immediate %%bin-op-high value) ,value))

(define-command-bin-op bin-op-table-fetch #o35)
(define-command-bin-op bin-op-end-of-box #o36)
(define-command-bin-op bin-op-string #o37)
(define-command-bin-op bin-op-symbol #o40)
(define-command-bin-op bin-op-package-symbol #o41)

(define-command-bin-op bin-op-positive-fixnum #o42)
(define-command-bin-op bin-op-negative-fixnum #o43)
(define-command-bin-op bin-op-positive-float #o44)
(define-command-bin-op bin-op-negative-float #o45)

(define-command-bin-op bin-op-row #o46)
(define-command-bin-op bin-op-list #o47)

(define-command-bin-op bin-op-initialize-and-return-array #o50)
(define-command-bin-op bin-op-initialize-and-return-numeric-array #o51)

(define-command-bin-op bin-op-format-version #o52)
(define-command-bin-op bin-op-eof #o53)

(define-command-bin-op bin-op-file-property-list #o54)

(define-command-bin-op bin-op-table-store #o55)

(define-command-bin-op bin-op-simple-cons #o56)
(define-command-bin-op bin-op-name-and-input-row #o57)
(define-command-bin-op bin-op-name-row #o60)

;;graphics stuff
(define-command-bin-op bin-op-graphics-sheet #o61)
(define-command-bin-op bin-op-graphics-object #o62)
(define-command-bin-op bin-op-turtle #o63)
;; bit arrays for graphics (obsolete but we keep it around to be able to load old files)
(define-command-bin-op bin-op-picture #o67)

;;; misc
(define-command-bin-op bin-op-fat-cha #o64)
(define-command-bin-op bin-op-rational #o65)
(define-command-bin-op bin-op-complex #o66)
(define-command-bin-op bin-op-font-row #o70)

;;; file system
(define-command-bin-op bin-op-box-server-info-start #o71)

;;; multimedia
(define-command-bin-op bin-op-pixmap #o72)
(define-command-bin-op bin-op-sound  #o73)
;; what the hell...
(define-command-bin-op bin-op-video  #o74)

;; reserve for possible later inclusion
(define-command-bin-op bin-op-capogi-font #o75)




(define-resource dump-hash-table ()
  :constructor (make-hash-table :test #'eq)
  :initializer #'clrhash
  :initial-copies #+ppc-target 1 #-ppc-target 0)

(define-resource dumped-box-hash-table ()
  :constructor (make-hash-table :test #'eq)
  :initializer #'clrhash
  :initial-copies #+ppc-target 1 #-ppc-target 0)

(defmacro writing-bin-file ((box stream file) &body body)
  `(with-open-file (,stream ,file :direction ':output
                             :element-type '(unsigned-byte 8.))
     (writing-bin-stream (,box ,stream)
                         . ,body)))

(defvar *dumping-font-alist* nil)

(defmacro writing-bin-stream ((box stream) &body body)
  `(progn
    (using-resource (*bin-dump-table* dump-hash-table)
                    (using-resource (*dumped-box-table* dumped-box-hash-table)
                                    (start-bin-file ,stream)
                                    (let ((*bin-dump-index* 0)
                                          (*bin-dump-package* *package*)
                                          (*file-word-writer-function*
                                           *file-word-writer-function*)
                                          (*dumping-font-alist* nil)
                                          (*current-dumping-box* ,box)
                                          (*outermost-dumping-box* ,box))
                                      . ,body))
                    (end-bin-file ,stream))))


(defun internal-dumping-error (&optional reason)
  (if boxer-eval::*give-lisp-errors*
    (error "~A while trying to dump the box, ~A"
           (or reason "An error occurred") *current-dumping-box*)
    (boxer-eval::primitive-signal-error
     :file (format nil "~A trying to dump the box"
                   (or reason "An error occurred"))
     *current-dumping-box*)))


;*******************************************************************************
;*                                LOADING   DEFINITIONS                        *
;*******************************************************************************

;;; Loading variables
(define-resource bin-load-table ()
  :constructor (make-array #o1000 :adjustable t))

(defvar *no-value-marker* (list 'no-value))
(defvar *bin-next-command-function*)

(defvar *bin-load-table*)
(defvar *bin-load-index*)
(defvar *load-package*)
(defvar *file-bin-version*)
(defvar *circular-ports* nil) ; Old flush soon
(defvar *post-load-relink-ports* nil)

(defvar *bin-op-load-command-table* (make-bin-op-dispatch-table))

(defvar *supported-obsolete-versions* '(5 6 7 8 9 10 11 12))

(defvar *status-line-loading-format-string* "Loading ~D (~D %)")

(defvar *status-line-saving-format-string* nil)

(defvar *in-autoload-environment* nil)
(defvar *autoload-list* nil)
(defvar *autoloading-namestring* nil)

(defvar *warn-about-outlink-ports* t)

(defmacro with-post-load-autoloading ((fs) &body body)
  `(let ((*in-autoload-environment* t)
         (*autoload-list* nil)
         (*autoloading-namestring* (when (typep ,fs 'file-stream)
                                     (namestring ,fs))))
     (prog1 (progn . ,body)
            (with-lisp-error-reporting
              (dolist (ab *autoload-list*)
                (when (null (first-inferior-row ab)) (fill-box-from-server ab)))))))

(defvar *prompt-for-server-file-not-found* T)

(defmacro bin-next-command (&rest args)
  `(funcall *bin-next-command-function* . ,args))

(defmacro loading-bin-file ((stream next-command-function)
                            &body body)
  `(let* ((*bin-next-command-function* ,next-command-function)
          (*bin-load-index* 0)
          (*file-bin-version* 0)
          (*circular-ports* nil)
          (*post-load-relink-ports* nil)
          (*file-word-reader-function* *file-word-reader-function*))
     (using-resource (*bin-load-table* bin-load-table)
                     (bin-load-start ,stream)
                     (prog1 (progn . ,body)
                            (dolist (p *circular-ports*)
                              (unless (or (not (numberp (cdr p))) (zerop (cdr p)))
                                (set-port-to-box (car p)
                                                 (direct-circular-port-target
                                                  (car p) (cdr p)))))
                            (dolist (p *post-load-relink-ports*)
                              (unless (not (numberp (cdr p)))
                                (let ((target (aref *bin-load-table* (cdr p))))
                                  (if (box? target)
                                    (set-port-to-box (car p) target)
                                    (progn (warn "Can't Relink Port to ~A" target)
                                           (set-port-to-box
                                            (car p)
                                            (make-box
                                             '(("Was a Bad Target")))))))))))))

;;;Load command definitions...
;;;There are three types of commands

(defmacro define-bin-command-op (op-name defining-function table
                                         function-prefix arglist
                                         &body definition)
  (declare (ignore defining-function))
  (let ((function-name (intern (concatenate 'string function-prefix
                                            (string op-name)))))
    `(progn
      (setf (bin-op-dispatch ,table (ldb %%bin-op-low ,op-name))
            ',function-name)
      #+lispm
      (si::record-source-file-name ',op-name ',defining-function)
      (defun ,function-name ,arglist . ,definition))))

;;; A command that may return a value, but does not store it in the table
(defmacro define-load-command (op-name arglist &body body)
  `(define-bin-command-op ,op-name define-load-command
     *bin-op-load-command-table* "LOAD-" ,arglist
     . ,body))

;;; A command that does not return a value at all
(defmacro define-load-command-for-effect (op-name arglist &body body)
  `(define-bin-command-op ,op-name define-load-command-for-effect
     *bin-op-load-command-table* "LOAD-" ,arglist
     ,@body
     *no-value-marker*))

;;; A command that returns a value stored in the next slot in the table
(defmacro define-load-command-for-value (op-name arglist &body body)
  `(progn
    (setf (get ',op-name 'bin-table-loading-command) t)
    (define-bin-command-op ,op-name define-load-command-for-value
      *bin-op-load-command-table* "LOAD-" ,arglist
      (enter-bin-load-table (progn . ,body)))))

(defmacro enter-bin-load-table (value)
  `(let ((.index. *bin-load-index*))
     (incf *bin-load-index*)
     (enter-bin-load-table-internal ,value .index.)))


;;; This is the counterpart of Dump-List-Preamble
;;;
;;; Note that the object index MUST be incremented
;;; (by using Enter-Bin-Load-Table)
;;; but that it can't happen until AFTER ALL the elements of thelist
;;; have been loaded otherwise the indexes for the objects IN the list
;;; will be off by one

(defun load-list-values (stream)
  ;; sanity check, should look like a LIST
  (declare (values length dotted?))
  (multiple-value-bind (opcode extra-arg)
                       (decode-bin-opcode (bin-next-byte stream))
                       (cond ((=& opcode bin-op-list-immediate)
                              (values extra-arg (bin-next-value stream)))
                         ((=& opcode bin-op-list)
                          (values (bin-next-value stream) (bin-next-value stream)))
                         (t (error "Expected a LIST opcode, either ~D or ~D got ~D instead"
                                   bin-op-list bin-op-list-immediate opcode)))))

;;; The body MUST side effect the stream by removing ONE AND ONLY ONE
;;; item from the stream each time it is called
(defmacro with-loading-list ((stream &optional
                                     (idx-var (gensym))
                                     (dotted?-var (gensym) dotted-var-supplied-p))
                             &body body)
  `(multiple-value-bind (length ,dotted?-var)
                        (load-list-values ,stream)
                        ,(when (null dotted-var-supplied-p) `(declare (ignore ,dotted?-var)))
                        (enter-bin-load-table 'non-consed-list)
                        (dotimes (,idx-var length) . ,body)))

;;; file property list handlers

(defmacro deffile-property-handler (keyword value-arg &body body)
  (let ((name (intern (symbol-format nil "~A-FILE-PROPERTY-HANDLER" keyword)
                      (find-package 'boxer))))
    `(progn
      (defun ,name (,value-arg) ,@body)
      (setf (get ',keyword 'file-property-list-handler) ',name))))


(defun handle-file-property-list (plist)
  (do* ((remaining plist (cddr remaining))
        (key (car remaining) (car remaining))
        (value (cadr remaining) (cadr remaining)))
    ((null remaining))
    (cond ((eq key :package)) ; handled explicitly in old code
      (t (let ((handler (get key 'file-property-list-handler)))
           (if (null handler)
             (warn "Unhandled File property, ~S" key)
             (funcall handler value)))))))

;; try and cut down on unneccessary string CONSing by using a buffer
;; especially now, (= *file-bin-version* 12), that we use strings as part
;; of every row
(defvar *load-string-buffer* (make-array 80 :element-type 'character
                                         :fill-pointer 0 :adjustable t))

(defvar *use-load-string-buffer* nil)

(defmacro with-buffered-string-loading (&body body)
  `(let ((*use-load-string-buffer* t)) . ,body))





;;;; Reading and writing words of data

;;; The following comments are obsolete. The implementation now
;;; reads files in as bytes and assembles them into 16 bit words
;;; we will assume a default of low-byte first

;;; Different implementations have different ways of packing 16 bit words
;;; when dealing with UNIX (bytes only filesystems)
;;; In particular, Lucid packs them high-byte first, while Symbolics machines
;;; stores 16-bit words low-byte first.  While they conform to CLtL, within the
;;; implementation, it is not possible to share binary files between
;;; implementations because of this difference IF we stick to the CL functions
;;; READ-BYTE and WRITE-BYTE.  Therefore, we use these functions to pre-swap
;;; high and low bytes for certain implementations.
;;;
;;; We will arbitrarily decide that low-byte 1st is preferred because it makes
;;; it easy to read strings (given the way strings are currently dumped out)
;;;
;;; The easiest way of testing a given implementation is to use test-dump
;;; to dump a few strings and then use EMACS (or some other editor that
;;; doesn't barf on binaries) to look at the file.  The strings should be
;;; readable.
;;;

(defvar *swapped-bin-op-format-version*
  (dpb (ldb %%bin-op-low-half bin-op-format-version)
       %%bin-op-top-half
       (ldb %%bin-op-top-half bin-op-format-version)))

(defvar *file-reader-word-swapping-toggle-alist* nil)

(defmacro deffile-word-reader (name-suffix arglist
                                           &key normal-form swapping-form)
  (let ((normal-name (intern (symbol-format nil "READ-FILE-WORD-~A" name-suffix)))
        (swap-name (intern (symbol-format nil "READ-FILE-SWAP-WORD-~A" name-suffix))))
    `(progn
      (defun ,normal-name ,arglist ,normal-form)
      (defun ,swap-name ,arglist ,swapping-form)
      (let ((entry (assoc ',normal-name
                          *file-reader-word-swapping-toggle-alist*))
            (rentry (assoc ',swap-name
                           *file-reader-word-swapping-toggle-alist*)))
        (if (null entry)
          (setq *file-reader-word-swapping-toggle-alist*
                (push (cons ',normal-name ',swap-name)
                      *file-reader-word-swapping-toggle-alist*))
          (setf (cdr entry) ',swap-name))
        (if (null rentry)
          (setq *file-reader-word-swapping-toggle-alist*
                (push (cons ',swap-name ',normal-name)
                      *file-reader-word-swapping-toggle-alist*))
          (setf (cdr rentry) ',normal-name))))))

(defvar *file-writer-word-swapping-toggle-alist* nil)

(defmacro deffile-word-writer (name-suffix arglist
                                           &key normal-form swapping-form)
  (let ((normal-name (intern (symbol-format nil "WRITE-FILE-WORD-~A" name-suffix)))
        (swap-name (intern (symbol-format nil "WRITE-FILE-SWAP-WORD-~A" name-suffix))))
    `(progn
      (defun ,normal-name ,arglist ,normal-form)
      (defun ,swap-name ,arglist ,swapping-form)
      (let ((entry (assoc ',normal-name
                          *file-writer-word-swapping-toggle-alist*))
            (rentry (assoc ',swap-name
                           *file-writer-word-swapping-toggle-alist*)))
        (if (null entry)
          (setq *file-writer-word-swapping-toggle-alist*
                (push (cons ',normal-name ',swap-name)
                      *file-writer-word-swapping-toggle-alist*))
          (setf (cdr entry) ',swap-name))
        (if (null rentry)
          (setq *file-writer-word-swapping-toggle-alist*
                (push (cons ',swap-name ',normal-name)
                      *file-writer-word-swapping-toggle-alist*))
          (setf (cdr rentry) ',normal-name))))))

(deffile-word-reader from-stream (fs &optional (eof-errorp t) eof-value)
  :normal-form
  (let ((low (read-byte fs eof-errorp eof-value))
        (hi  (read-byte fs eof-errorp eof-value)))
    (cond ((and (not eof-errorp)
                (or (eq low eof-value) (eq hi eof-value)))
           eof-value)
      (t
       (dpb& hi %%bin-op-top-half low))))
  :swapping-form
  (let ((hi  (read-byte fs eof-errorp eof-value))
        (low (read-byte fs eof-errorp eof-value)))
    (cond ((and (not eof-errorp)
                (or (eq low eof-value) (eq hi eof-value)))
           eof-value)
      (t
       (dpb& hi %%bin-op-top-half low)))))

(deffile-word-writer to-stream (word fs)
  :normal-form
  (progn (write-byte (ldb& %%bin-op-low-half word) fs)
         (write-byte (ldb& %%bin-op-top-half word) fs))
  :swapping-form
  (progn (write-byte (ldb& %%bin-op-top-half word) fs)
         (write-byte (ldb& %%bin-op-low-half word) fs)))

(defvar *file-word-reader-function* 'read-file-word-from-stream)

(defvar *file-word-writer-function* 'write-file-word-to-stream)

(defun toggle-reader-byte-swapping ()
  (let ((entry (assoc *file-word-reader-function*
                      *file-reader-word-swapping-toggle-alist*)))
    (if (null entry)
      (error "Cannot toggle file word reading from ~A"
             *file-word-reader-function*)
      (setq *file-word-reader-function* (cdr entry)))))

(defun toggle-writer-byte-swapping ()
  (let ((entry (assoc *file-word-writer-function*
                      *file-writer-word-swapping-toggle-alist*)))
    (if (null entry)
      (error "Cannot toggle file word writing from ~A"
             *file-word-writer-function*)
      (setq *file-word-writer-function* (cdr entry)))))

(defmacro read-file-word (word &rest args)
  `(funcall *file-word-reader-function* ,word . ,args))

(defmacro write-file-word (word &rest args)
  `(funcall *file-word-writer-function* ,word . ,args))


;; this is (potentially) forgiving about ".box"
(defvar *strict-box-paths* nil
  "Whether to be forgiving about files ending with \".box\"")


;;;; special file readers
;; these can be platform specific (i.e. :pict for the mac) or
;; they can work across platforms for defined standards like GIF
;; EVERYTHING must support the types :boxer and :text

(defun boxer-file-contents? (filename)
  (with-open-file (s filename :direction :input :element-type '(unsigned-byte 8.))
    (let ((1st-word (read-file-word-from-stream s nil nil)))  ; file can be empty
      (when 1st-word
        (or (= 1st-word bin-op-format-version)
            (= 1st-word *swapped-bin-op-format-version*))))))

;; system dependent
#+mcl
(defvar *possible-boxer-file-mac-types* (list :text :???? :****
                                              ;; OSX default for unknown
                                              (intern
                                               (make-string
                                                4 :initial-element #\Null)
                                               (find-package "KEYWORD"))))

;; this should eventually use /etc/magic
(defun file-type (filename)
  (if (boxer-file-contents? filename) :boxer :text))

(defvar *error-on-unknown-file-type* nil)

(defvar *special-file-readers* nil)

;; TYPE is a keyword returned by the file-type function
;; FUNCTION is a function that takes 1 arg, a filename, and should return a box
(defmacro deffile-type-reader (type function)
  `(progn
    (unless (fast-memq ',type *special-file-readers*)
      (push ',type *special-file-readers*))
    (setf (get ',type 'file-type-reader-function) ',function)))

(defun get-special-file-reader (type)  (get type 'file-type-reader-function))

;; the basic file readers...
(deffile-type-reader :boxer load-binary-box-internal)

(deffile-type-reader :text   read-text-file-internal)



;;; Hooks for file system interactions for loadable modules
;;; in general, function/methods get pushed onto these lists by the defs in
;;; each loadable module

(defvar *dump-plist-length-hook* nil
  "functions take 1 box arg and should return the number of extra items in the dump plist")

(defvar *dump-plist-internal-hook* nil
  "functions talke a box and stream arg and should dump a keyword and the special
   item to the stream")

(defvar *load-module-init-keywords* nil)



(defmacro with-hilited-box ((box) &body body)
  (let ((screen-box (gensym))
        (screen-box-x (gensym))   (screen-box-y (gensym))
        (screen-box-wid (gensym)) (screen-box-hei (gensym)))
    `(drawing-on-window (*boxer-pane*)
                        (let* ((,screen-box (or (car (displayed-screen-objs ,box))
                                                (when (superior? (outermost-box) ,box)
                                                  (outermost-screen-box))))
                               ,screen-box-wid ,screen-box-hei)
                          (multiple-value-bind (,screen-box-x ,screen-box-y)
                                               (when (screen-box? ,screen-box)
                                                 (setq ,screen-box-wid (screen-obj-wid ,screen-box)
                                                        ,screen-box-hei (screen-obj-hei ,screen-box))
                                                 (xy-position ,screen-box))
                                               (unwind-protect
                                                (progn
                                                 (unless (null ,screen-box-x)
                                                   (with-pen-color (bw::*blinker-color*)
                                                     (box::with-blending-on
                                                      (draw-rectangle
                                                                      ,screen-box-wid ,screen-box-hei
                                                                      ,screen-box-x ,screen-box-y)))
                                                   (swap-graphics-buffers))
                                                 . ,body)
                                                (unless (null ,screen-box-x)
                                                  (repaint)
                                                  (swap-graphics-buffers))))))))

;;;

