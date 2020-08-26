;;; -*- Syntax: Common-Lisp; Base: 10; Package: EVAL -*-

;;;
;;; $Header: errors.lisp,v 1.0 90/01/24 22:11:03 boxer Exp $
;;;
;;; $Log:	errors.lisp,v $
;;;Revision 1.0  90/01/24  22:11:03  boxer
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

10/26/08 nope, need clean separation between kbd polling and periodic eval
10/23/08 poll-internal now also performs periodic eval actions
10/14/06 changed call to name to name-string in crock-boxer-fun-box-name-string
10/21/03 metering-poll-internal & friends
 1/06/02 make-error-box binds font to default
11/17/01 added error-states and related functions
 2/14/01 merged current LW and MCL files
 9/08/99 fixed blowout in crock-boxer-fun-name-string-try-hard when path to
          the top has no named boxes
 9/08/99 started logging changes

|#

;;;

(in-package :boxer-eval)

;;;
;;; Polling
;;;

(defvar *internal-polling-function* nil)

(defun default-poll-internal ()
  (or *last-interrupt-char*
      (let ((abort? (bw::keyboard-interrupt? boxer::*boxer-pane*)))
        (cond ((null abort?)
               nil)
          (t
           (setq *last-interrupt-char* abort?)
           t)))))

;; metering poll function & friends
(defvar *last-poll-time* 0)
(defvar *poll-times* nil)

(defun metering-poll-internal ()
  (unless (zerop *last-poll-time*)
    (push (- (get-internal-real-time) *last-poll-time*) *poll-times*))
  (setq *last-poll-time* (get-internal-real-time))
  (or *last-interrupt-char*
      (let ((abort? (bw::keyboard-interrupt? boxer::*boxer-pane*)))
        (cond ((null abort?)
               nil)
          (t
           (setq *last-interrupt-char* abort?)
           t)))))

(defun poll-internal ()
  (boxer::vcgc-check)
  (funcall (or *internal-polling-function* #'default-poll-internal)))

(defun reset-poll-count ()
  (setq *poll-count* 1)) ;; (poll) will decrement BEFORE testing for 0


;;;
;;; Notices
;;;

(defun evaluator-notice (string &rest args)
  (when boxer::*evaluator-helpful*
    (apply #'boxer::boxer-editor-warning string args)))



;;;
;;; Errors
;;;

;;; this is used to encapsulate error state so it can be passed between processes
(defstruct error-state
  (exception 'error-exception)
  (type nil)
  (sfun nil)
  (args nil))

(defun encapsulate-error ()
  (make-error-state :type *error-type*
                    :sfun *error-sfun*
                    :args *exception-args*))

;;;
;;; Signalling Errors
;;;

(defun primitive-signal-error (error-type &rest error-args)
  (apply #'signal-error error-type error-args)
  (throw 'boxer-primitive-error-signal nil))

(defun signal-error (error-type &rest error-args)
  (when (not (null *exception-signalled*))
    (warn "An error was signalled when an error was yet unhandled."))
  (setq *exception-signalled* 'error-exception
        *error-type* error-type
        *error-sfun* *executing-sfun*
        *exception-args* (copy-seq error-args)))

(defun signal-error-from-state (error-state)
  (when (not (null *exception-signalled*))
    (warn "An error was signalled when an error was yet unhandled."))
  (setq *exception-signalled* 'error-exception
        *error-type* (error-state-type error-state)
        *error-sfun* (error-state-sfun error-state)
        *exception-args* (error-state-args error-state))
  (throw 'boxer-primitive-error-signal nil))


(defvar *enable-interrupt-polling-in-editor* nil)

(defmacro report-eval-errors-in-editor (&body body)
  `(let ((error-signalled? t)
         (*enable-interrupt-polling-in-editor* t))
     (prog1 (catch 'boxer-primitive-error-signal
              (prog1 (progn . ,body)
                     (setq error-signalled? nil)))
            (when error-signalled?
              (boxer::boxer-editor-error
               (if *error-sfun* "In primitive ~s, ~s ~s" "~s~* ~s")
               *error-type* *error-sfun*
               (mapcar
                #'(lambda (u)
                          (cond
                            ((and (typep u 'simple-vector) (boxer-function? u))
                             (if (compiled-boxer-function? u)
                               (compiled-boxer-function-name u)
                               (format
                                nil
                                "[~a...]"
                                (car (interpreted-boxer-function-text u)))))
                            (t u)))
                *exception-args*)
               (clear-error-variables))))))


;;;
;;; Interrupts
;;;

;;; Signal interrupts by setting *last-interrupt-char*.

;;;
;;; Handling Interrupts
;;;
(defun handle-interrupt-char ()
  (let ((char *last-interrupt-char*))
    (setq *last-interrupt-char* nil) ;we know it's ^G of some sort.
    (if (member char bw::*noisy-abort-key-chars*)
      (signal-error :STOPPED!)
      (signal-error :STOPPED))))

;; this is for use INSIDE of primitives (like READ)
(defun check-for-interrupt (error-type &rest error-args)
  (when (poll-internal)
    ;; if there is an interrupt, reset the interrupt-char and throw
    (setq *last-interrupt-char* nil)
    (apply #'primitive-signal-error error-type error-args)))

;;;
;;; Pausing
;;;

(defun pause (type)
  (break "Boxer Pause: ~S" type))


;;;
;;; Handling Errors
;;;

(defvar *give-lisp-errors* nil)

(defvar *special-token-type-symbol-alist-for-error*
  '((bu::eval-it . bu::!)
    (bu::previous-tell-environment . bu::^)
    (bu::@ . bu::@)))

(defun handle-error ()
  (setq *lexical-variables-root* (boxer::point-box))
  (unwind-protect
   (if *give-lisp-errors*
     (give-lisp-error)
     (let ((result (make-error-result)))
       (cond (*stepping*
              (stepper-signal-error result))
         ((not (eq *current-process* *doit-key-process*))
          ;; if the current process is a background process,
          ;; then we need to setup the error and then
          ;; tell the user where to find it
          (unprintable-background-error-box-warning result)
          ;; since we aren't going to THROW out of the
          ;; evaluator (because there may be other processes
          ;; waiting to be run), we need to do the cleanup
          ;; forms explicitly here
          (unwind-to-top-level)
          (reset-stacks)
          :background-error)
         (t
          (throw 'boxer-error (values result t))))))
   (clear-error-variables)))

(defun unprintable-background-error-box-warning (error)
  ;; save away the error box
  error
  (boxer::unprintable-sound)
  (boxer::status-line-display 'boxer::boxer-editor-error
                              "Error in Background Process"))

(defun clear-error-variables ()
  ;; these variables aren't in any frames or anything, so they
  ;; don't get unwound.
  (setq *executing-sfun* nil)
  (setq *exception-signalled* nil)
  (setq *exception-args* nil)
  (setq *error-type* nil)
  (setq *error-sfun* nil))

(defun make-error-result ()
  ;; :STOPPPED is the code for quiet abort.
  ;; :STOPPED! is the code for noisy abort.
  ;; we want to be quiet if the error type is quiet abort.
  (if (eq *error-type* :stopped)
    *novalue*
    (make-error-box)))

(defun make-error-box ()
  (let ((boxer::*current-font-descriptor* boxer::*default-font-descriptor*))
    (boxer::make-box
     `(("Error:" ,.(error-report-error-string))
       ,.(error-report-error-sfun)
        ,.(error-report-exception-args)
        ,.(error-report-executing-line)
        .,(error-report-executing-box)))))

(defun error-report-error-string ()
  (list
   (or
    (case *error-type*
      (:ifs-format "")
      (:sprite-error "")
      (:not-true-or-false "TRUE or FALSE expected")
      (:doesn\'t-output "The primitive does not output a value")
      (:unbound-variable "Can't find a box named")
      (:infix-out-of-place
       "The arithmetic operator listed below needs something before it")
      (:insufficient-args-error
       "The box or primitive needs more inputs")
      (:excess-args-error
       "There are too many things on the line after the special command")
      (:no-such-procedure-to-stop
       "There is no such procedure to STOP")
      (:not-a-sprite
       "The input was not a sprite")
      (:didnt-output
       "The box or primitive did not output a value, but one was expected"))
    *error-type*)))



(defun error-report-error-sfun ()
  (if *error-sfun*
    (list (list "Primitive"
                (canonicalize-primitive-name-for-error
                 (compiled-boxer-function-name *error-sfun*))))
    nil))

(defvar *canonical-primitive-names*
  '((%ifs-internal-1 . IFS)
    (%novalue-internal . "trigger")
    (*ignoring-definition-object* . "{Named Box}")
    (%NEGATIVE-INTERNAL . -)))

(defun canonicalize-primitive-name-for-error (name)
  (or (cdr (assoc name *canonical-primitive-names*))
      name))

(defun error-report-exception-args ()
  (if *exception-args*
    (list (mapcan
           #'canonicalize-weird-object
           *exception-args*))))

(defun error-report-executing-line ()
  (if (and *executing-line* (consp (car *executing-line*)))
    (list (cons "in line"
                (mapcan #'canonicalize-weird-object
                        (car *executing-line*))))))

(defun error-report-executing-box ()
  (let ((function-box
         (and *executing-function*
              (interpreted-boxer-function-backpointer *executing-function*))))
    (if (boxer::box? function-box)
      (let* ((function-box-named? (boxer::name-row function-box))
             (backtrace-list (backtrace-list nil))
             (nontrivial-backtrace? (not (null backtrace-list))))
        (nconc
         (list (list* "of box"
                      (let ((port
                             (boxer::top-level-print-vp
                              (boxer::port-to
                               function-box))))
                        (boxer::supershrink port)
                        port)
                      (if (and (not function-box-named?)
                               nontrivial-backtrace?)
                        (list "inside"
                              (crock-boxer-fun-name-string-try-hard
                               function-box nil))
                        nil)))
         (and nontrivial-backtrace?
              (list (list "called by"
                          (stringify-backtrace-list backtrace-list)))))))))

;; Returns a list of believable Boxer objects and strings representing the
;; argument.  makes sure no boxes are inserted without being copied.
(defun canonicalize-weird-object (u)
  (cond ((numberp u) (list u))
    ((stringp u) (list u))
    ((null u) (list ""))
    ((symbolp u) (list (canonicalize-primitive-name-for-error u)))
    ((boxer-function? u)
     (cond ((compiled-boxer-function? u)
            (list (canonicalize-primitive-name-for-error
                   (compiled-boxer-function-name u))))
       ((doit-box?
          (interpreted-boxer-function-backpointer u))
        (list (boxer::copy-box
               (interpreted-boxer-function-backpointer u))))
       ;; these next 2 cases fall out of RUN
       ;; RUN of a data box or...
       ((data-box?
         (interpreted-boxer-function-backpointer u))
        (list (boxer::copy-box
               (interpreted-boxer-function-backpointer u))))
       ;; RUN of a Virtual copy
       ((and (simple-vector-p
              (interpreted-boxer-function-backpointer u))
             (fast-eval-data-box?
              (interpreted-boxer-function-backpointer u)))
        (list (boxer::top-level-print-vc
               (interpreted-boxer-function-backpointer u))))
       ((and (simple-vector-p
              (interpreted-boxer-function-backpointer u))
             (fast-eval-doit-box?
              (interpreted-boxer-function-backpointer u)))
        (list (boxer::top-level-print-vc
               (interpreted-boxer-function-backpointer u))))
       ((null (interpreted-boxer-function-backpointer u))
        (list (format nil "[Unamed-Function: ~A]"
                      (interpreted-boxer-function-text u))))
       (t (list (format nil "Strange object: ~S" u)))))
    ((typep u 'boxer::foreign-data)
     (boxer::make-editor-box-from-foreign-data u))
    ((special-token? u)
     (cond ((dots-list-token? u)
            (let* ((symbols (special-token-item u))
                   (string (symbol-name (car symbols))))
              (dolist (S (cdr symbols) (list string))
                (setq string
                      (concatenate 'string string "." (symbol-name s))))))
       (t
        (cons (cdr (fast-assq
                    (special-token-type u)
                    *special-token-type-symbol-alist-for-error*))
              (canonicalize-weird-object (special-token-item u))))))
    ((boxer::virtual-copy? u) (list (boxer::top-level-print-vc u)))
    ((boxer::virtual-port? u) (list (boxer::top-level-print-vp u)))
    ((boxer::box? u) (list (boxer::copy-box u)))
    ((consp u) (mapcar #'canonicalize-weird-object u))
    ((compiled-boxer-function? u)
     (list (compiled-boxer-function-name u)))
    ;; give up on representing the list per se.
    (t (list u))))

(defun give-lisp-error ()
  (error (if *error-sfun*
           "In primitive ~s, ~s ~s"
           "~s~* ~s")
         *error-type*
         *error-sfun*
         (mapcar
          #'(lambda (u)
                    (cond
                      ((boxer-function? u)
                       (if (compiled-boxer-function? u)
                         (compiled-boxer-function-name u)
                         (format
                          nil
                          "[~a...]"
                          (car (interpreted-boxer-function-text u)))))
                      (t u)))
          *exception-args*)))

;; used all over.
;; terrible name
(defun crock-boxer-fun-box-name-string (boxer-function)
  (and boxer-function
       (let ((obj (interpreted-boxer-function-backpointer boxer-function)))
         (cond ((boxer::box? obj)
                (let ((name-row (boxer::name-row obj)))
                  (and name-row (boxer::name-string obj))))
           ((boxer::virtual-copy? obj)
            (let ((name (boxer::vc-name obj)))
              (or (and name (string name))
                  "[]")))
           ((boxer::virtual-port? obj)
            (let ((name (boxer::vp-name obj)))
              (or (and name (string name))
                  "<>")))
           (t nil)))))

(defun crock-boxer-fun-box-name (boxer-function)
  (and boxer-function
       (let ((obj (interpreted-boxer-function-backpointer boxer-function)))
         (cond ((boxer::box? obj)
                (let ((name-row (boxer::name-row obj)))
                  (and name-row (boxer::cached-name name-row))))
           ((boxer::virtual-copy? obj)
            (boxer::vc-name obj))
           ((boxer::virtual-port? obj)
            (boxer::vp-name obj))
           (t nil)))))


;; takes a doit box or a function object and walks up the editor
;; hierarchy until it finds a function name.
(defun crock-boxer-fun-name-string-try-hard (fun &optional (include-self? t))
  (labels ((crock-boxer-fun-name-internal (fun)
                                          (cond ((null fun) nil)
                                            ((and (boxer-function? fun)
                                                  (interpreted-boxer-function? fun))
                                             (let ((name (crock-boxer-fun-box-name-string fun)))
                                               (if (not (null name))
                                                 (list name)
                                                 (crock-boxer-fun-name-internal
                                                  (interpreted-boxer-function-backpointer
                                                   fun)))))
                                            ((boxer::box? fun)
                                             (let ((name-row (boxer::name-row fun)))
                                               (if name-row
                                                 (list (boxer::name fun))
                                                 (let ((superior (boxer::superior-box
                                                                  fun)))
                                                   (if (null superior) nil
                                                     (cons nil
                                                           (crock-boxer-fun-name-internal
                                                            superior)))))))
                                            (t (error "unknown thing ~S in crock-boxer-fun-name" fun)))))
          (let* ((string-list (crock-boxer-fun-name-internal fun))
                 (munged-string-list (nreverse
                                      (if (and (not include-self?)
                                               (null (car string-list)))
                                        (cdr string-list)
                                        string-list))))
            (and munged-string-list
                 (reduce #'(lambda (a b) (concatenate 'string (or a "[]")
                                                      "."
                                                      (or b "[]")))
                         munged-string-list)))))

(defun backtrace-list (&optional (include-current? t))
  (let ((list nil))
    (flet ((record-function (object)
                            (let ((name (crock-boxer-fun-name-string-try-hard object)))
                              (unless (null name)
                                (push name list)))))
          (if include-current? (record-function *executing-function*))
          (do ((frame *pdl* (stack-frame-next-frame frame)))
            ((null frame) list)
            (cond ((eval-args-frame-p frame))
              ((ufun-frame-p frame)
               (record-function
                (dbg-ufun-frame-*executing-function* frame)))
              ((doit-port-funcall-frame-p frame))
              ((tell-special-frame-p frame))
              ((repeat-special-frame-p frame))
              (t nil))))))


(defvar *maximum-backtrace-bottom* 15)
(defvar *minimum-backtrace-top* 3)

(defun stringify-backtrace-list (list)
  (let ((length (length list)))
    (cond ((null list) "")
      ((<=& length *maximum-backtrace-bottom*)
       (reduce #'(lambda (a b) (concatenate 'string a "->" b)) list))
      (t (let ((top (subseq list 0 *minimum-backtrace-top*))
               (bottom (subseq list
                               (boxer::max&
                                (- length *maximum-backtrace-bottom*)
                                0))))
           (reduce #'(lambda (a b)
                             (concatenate 'string a "->" b))
                   (nconc top (list "...") bottom)))))))


