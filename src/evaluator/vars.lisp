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
;;;;                                        +-------+
;;;;               This file is part of the | Boxer | System
;;;;                                        +-Data--+
;;;;
;;;;
;;;;  Modification History (most recent at the top)
;;;;
;;;;   1/29/08 added boxer::%private-graphics-list
;;;;   3/05/07 *periodic-eval-action* set to boxer::repaint-in-eval
;;;;  10/21/03 *periodic-eval-action/times* carbon values set for 700 MHz G4 imac
;;;;   2/11/03 merged current LW and MCL files
;;;;  12/16/01 added *error-handler-list* and *error-signalled?*
;;;;  12/16/01 started logging changes: source = boxer version 2.5
;;;;

(in-package :boxer-eval)

;;; Preamble

;;; When you add a :global variable or change an initializer
;;; on a :global you need to recompile
;;;  PROCESS and PROCESS-PRIMS.
;;; When you add a :local variable or change an initializer
;;; on a :local, you need to recompile
;;; PROCESS and PROCESS-PRIMS and also
;;; EVAL, EVAL-EVAL, FAST-EVAL, and STEPPER-EVAL.

(begin-eval-var-definitions)


;;; Precedences
(defvar *infix-symbol-list* nil)
(defconstant *default-precedence* 5)
(defconstant *default-single-arg-precedence* 5)
(defconstant *default-multi-arg-precedence* 0)

;;; Novalue
(defvar *novalue* (list '*novalue*))

;;; VPDL
(eval-when (compile load eval)
(define-eval-var *vpdl-grow-length* :global *default-vpdl-grow-length*)
)
(define-eval-var *vpdl-index*       :global 0)
(define-eval-var *vpdl*             :global (make-vpdl))
(define-eval-var *vpdl-size*        :global *default-vpdl-size*)

;;; PDL
(define-eval-var *pdl* :global nil)

;;; Variable Stacks
(define-eval-var *lexical-variables-root* :global (boxer::point-box))

;;; Dynamic Variables
(define-eval-var *dynamic-variables-names-array*
    :global (make-dynamic-variables-names-array))
(define-eval-var *dynamic-variables-values-array*
    :global (make-dynamic-variables-values-array))
(define-eval-var *dynamic-variables-bottom*            :global 0)
(define-eval-var *dynamic-variables-top*               :global 0)
(define-eval-var *dynamic-variables-array-grow-length* :global 50)

;;; Polling/Interrupts
(defvar *last-interrupt-char* nil)
;; bump this higher for better performance (especially in the X implementation
;; where the poll is very expensive).  The knee of the performance curve
;; for fib 20 is around 1000
(defvar *initial-poll-count* 200)

;;; we rebind the polling variables when running background processes
;;; because keyboard response is more important than execution speed in
;;; that context
(defvar *background-poll-count* 20)

(defvar *periodic-eval-action* 'boxer::repaint-in-eval
  "A hook for actions to be called inside the evaluator more often than
the polling function, for now, we use it to repain the screen during eval.
Can be nil if no action is desired.")

;; this number should be less than *initial-poll-count*
(defvar *periodic-eval-times*  '(100))

;;; Evaluator State Variables

;;; General Local evaluator variables
(define-eval-var *executing-line*     :global nil)
(define-eval-var *function*           :local :undefined)
(define-eval-var *thing-pointer*      :local :undefined)
(define-eval-var *executing-function* :global nil)
(define-eval-var *arglist*            :global nil)
;; this needs to be set before any section of the evaluator which
;; might pause the currently running process
(define-eval-var *pc*                 :local :eval-loop)
(define-eval-var *without-scheduling* :global nil)


;;; General Global evaluator state
(define-eval-var *executing-pointer* :global nil)
(define-eval-var *current-function*  :global nil)

;;; Eval/special primitive communication
(define-eval-var *returned-value*                       :global *novalue*)
(define-eval-var *sfun-continuation*                    :global nil)
(define-eval-var *run-list-sfun-epilog-handler*         :global nil)
(define-eval-var *ufuncall-sfun-epilog-handler*         :global nil)
(define-eval-var *ufuncall-sfun-unwind-protect-handler* :global nil)
(define-eval-var *run-list-unwind-protect-handler*      :global nil)

;;; Initialized by init-process-state-vector.
(define-eval-var *current-process* :global *current-process*)
(define-eval-var *process-state* :global ':TOPLEVEL)
(define-eval-var *process-doit-cursor-position*
    :global (init-process-doit-cursor-position))

;;; Debugger variables
(define-eval-var *executing-sfun*      :global nil)
(define-eval-var *exception-signalled* :global nil)
(define-eval-var *exception-args*      :global nil)
(define-eval-var *error-type*          :global nil)
(define-eval-var *error-sfun*          :global nil)

;;; More Polling
(define-eval-var *poll-count*          :global *initial-poll-count*)

;;; Trigger variables
(define-eval-var *trigger-list-to-run* :global nil)

;;; Stepper variables
(define-eval-var *stepping*             :global nil)

(defvar *stepper-initial-poll-count* 1)




;;; Compiler
(defvar *compile-boxer-generated-lambda?* nil)

;;; Processes
(defvar *boxer-processes* nil)

(defvar *initial-process* nil)

;;; Recursive-eval/funcall Primitives State Variables
;;; Every state variable must be declared here as either
;;; a define-eval-var or a defvar.  We can't automatically
;;; declare them anymore because they all have to be defined
;;; before any process accessing macros can be compiled.
;;; Of course, none of them can be :LOCAL.

;;; TELL
(define-eval-var *old-dynamic-variables-bottom* :global nil)
(define-eval-var *old-lexical-variables-root* :global nil)
(define-eval-var *old-tell-special-frame* :global nil)

;;; IFS
(define-eval-var *ifs-list* :global nil)

;; ANY-OF and ALL-OF
(define-eval-var *boolean-clauses* :global nil)

;;; REPEAT
(define-eval-var *repeat-list* :global nil)
(define-eval-var *repeat-count* :global nil)

;;; FOR-EACH-***
;;; FOR-EACH-ITEM
(define-eval-var *for-each-variable-object* :global nil)
(define-eval-var *for-each-counter* :global nil)
(define-eval-var *for-each-length*  :global nil)
(define-eval-var *for-each-box*     :global nil)
(define-eval-var *for-each-list*    :global nil)

;;; RECURSIVE DOIT
(define-eval-var *recursive-doit?* :global nil)

;;; Graphics: UPDATE-SHAPE, HOLDING-POSITION, WITHOUT-RECORDING
(define-eval-var boxer::%learning-shape? :global nil)
(define-eval-var boxer::%turtle-state    :global nil)
(define-eval-var boxer::%learning-shape-graphics-list :global nil)
(define-eval-var boxer::%private-graphics-list :global nil)
(define-eval-var boxer::*graphics-command-recording-mode* :global ':window)
(define-eval-var boxer::*current-sprite* :global nil)

(defvar boxer::*supress-graphics-recording?* nil)
;; this needs to be defined with a value of NIL for the benefit of other
;; (unrelated) redisplay initializers
(define-eval-var boxer::*supress-graphics-recording?* :global nil)

;; error handling
(define-eval-var *error-handler-list* :global nil)
(define-eval-var *error-signalled?* :global nil)

;;; Per Process memory allocation:

;;; commonly used stack frames
(define-eval-var *eval-args-frames* :global (allocate-eval-args-frame-cache))

(define-eval-var *ufun-frames* :global (allocate-ufun-frame-cache))

(define-eval-var *doit-port-funcall-frames*
    :global (allocate-doit-port-funcall-frame-cache))

;;; less commonly used frames (like those associated with specific
;;; primitives or the stepper) are accessed indirectly via:

(define-eval-var *special-eval-frames*
    :global (allocate-special-eval-frame-caches))


;;; Time Synchronization variables
(define-eval-var *internal-time-stamp* :global (get-internal-real-time))

;;;
;;; end of definitions:
;;;
(end-eval-var-definitions)


