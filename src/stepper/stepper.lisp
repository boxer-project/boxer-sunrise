;;; -*-Mode: Lisp; Package: Eval-*-
#|


 $Header: stepper.lisp,v 1.0 90/01/24 22:17:51 boxer Exp $

 $Log:	stepper.lisp,v $
;;;Revision 1.0  90/01/24  22:17:51  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                   +------+
          This file is part of the |Boxer | System
                                   +-Data-+



Modification History (most recent at top)

 2/15/03 merged current LW and MCL files, no diffs, copyright updated

|#

(in-package :boxer-eval)

(defvar *stepping* t)

(defvar *stepping-box* nil)

;;;
;;;Main Entry Point
;;;
(defun step-point-row ()
  (let ((*stepping* t)
        (*stepper-item-no* 0)
        (*stepper-row-no* 0)
        (*initial-poll-count* *stepper-initial-poll-count*)
        (*poll-count* *stepper-initial-poll-count*)
        (boxer::*propagate-modified-messages?* t)
        (*stepping-box* (initial-stepping-print)))
    (catch 'stepper-error
      (boxer::top-level-eval-wrapper
       (let ((result (boxer-step-eval (boxer::eval-objs (boxer::point-row)))))
         (step-redisplay)
         (step-function-return-sequence result)
         (when (eq result *novalue*)
           (step-redisplay)
           (boxer::com-beginning-of-row)
           (boxer::doit-print-returned-value result)
           (boxer::com-end-of-row)))))))

(defun top-level-print-eval-object (thing)
  (cond ((or (numberp thing) (symbolp thing)) thing)
        ((fast-eval-port-box? thing) (boxer::top-level-print-vp thing))
        ((or (fast-eval-data-box? thing)
             (fast-eval-doit-box? thing))
         (boxer::top-level-print-vc thing))
        (t thing)))

(defun initial-stepping-print ()
  (boxer::com-beginning-of-row)
  (boxer::doit-print-returned-value *novalue*)
  (let ((stepping-box
         (boxer::make-box (list nil) 'boxer::doit-box)))
    (boxer::insert-row-at-row-no stepping-box
                                 (boxer::copy-row (boxer::point-row))
                                 0)
    (boxer::doit-print-returned-value stepping-box)
    (boxer::redisplay)
    (boxer::com-end-of-row)
    (boxer::com-enter-box)
    (boxer::com-beginning-of-row)
    (boxer::redisplay)
    stepping-box))

(defvar *step-wait-for-key-press* t)
(defvar *step-sleep-time* 0.3)

(defun step-redisplay ()
  ;; process the mutation queue first
  (boxer::process-editor-object-mutation-queue
   boxer::*editor-objects-to-be-modified*)
  (setq boxer::*editor-objects-to-be-modified* nil)
  (dolist (sb (boxer::screen-objs *stepping-box*))
    (setf (slot-value sb 'boxer::force-redisplay-infs?) t))
  (boxer::redisplay)
  (if (null *step-wait-for-key-press*)
      (sleep *step-sleep-time*)
      (boxer::get-character-input boxer::*boxer-pane*)))

;;;
;;; The STEP-* handlers.
;;;

(defun step-advance-token ()
  (when-debugging (format t "~%step advance token"))
  (incf *stepper-item-no*)
  (move-point-to-item-no *stepper-item-no*))

(defun step-previous-token ()
  (when-debugging (format t "~%step previous token"))
  (decf *stepper-item-no*)
  (move-point-to-item-no *stepper-item-no*))

(defun step-insert-token (new-token &optional
                                    (START-CHA-NO
                                     (safe-beginning-of-nth-chunk-cha-no
                                      (boxer::point-row)
                                      *stepper-item-no*)))
  (flet ((insert-string
          (s)
          (map nil
               #'(lambda (cha) (boxer::insert-cha boxer::*point* cha :moving))
               s)))
    (when-debugging (format t "~%step insert token ~S" new-token))
    (cond ((boxer::box? new-token)
           (boxer::insert-cha boxer::*point* new-token :moving))
          (t
           (boxer::move-point (values (boxer::point-row) start-cha-no))
           (cond ((numberp new-token)
                  (insert-string (format nil "~D" new-token)))
                 ((eq new-token *novalue*)
                  (boxer::insert-cha boxer::*point* (boxer::make-box '(()) 'boxer::doit-box)))
                 ((symbolp new-token) (insert-string (string new-token)))
                 ((special-token? new-token)
                  (if (squid-token? new-token)
                      (step-insert-token (boxer::make-box
                                          (mapcar
                                           #'(lambda (row)
                                               (mapcan #'canonicalize-weird-object row))
                                           (special-token-item new-token))
                                          'boxer::doit-box)))
                  (progn
                    (insert-string (symbol-name
                                    (cdr (assoc
                                          (special-token-type new-token)
                                          *special-token-type-symbol-alist-for-error*))))
                    (step-insert-token (special-token-item new-token))))
                 (t (boxer::insert-cha boxer::*point*
                                       (top-level-print-eval-object new-token)
                                       :moving)))))))

(defun step-restore-cursor-position ()
  (when-debugging (format t "~%restoring to col ~d" *stepper-row-no*))
  (let ((row (boxer::row-at-row-no (boxer::point-box) *stepper-row-no*)))
    (when row
      (boxer::move-point (values row 0))
      (move-point-to-item-no *stepper-item-no*))))

;returns start-cha-no of old token
(defun step-delete-token (item-no)
  (let ((start-cha-no
         (safe-beginning-of-nth-chunk-cha-no
          (boxer::point-row)
          item-no)))
    (boxer::move-point (values (boxer::point-row) start-cha-no))
    (dotimes (i (- (safe-end-of-nth-chunk-cha-no
                    (boxer::point-row)
                    item-no)
                   start-cha-no))
      (boxer::simple-delete-cha boxer::*point* :moving))
    start-cha-no))

(defun step-delete-excess-whitespace ()
  (do ((i (boxer::bp-cha-no boxer::*point*)))
      (nil)
      (let ((cha (boxer::cha-at-cha-no (boxer::point-row) i)))
        (if (equal cha #\Space)
            (boxer::delete-cha-at-cha-no (boxer::point-row) i)
            (progn
              (boxer::insert-cha-at-cha-no (boxer::point-row) #\Space i)
              (return nil))))))


(defun step-replace-token (item-no new-token)
  (when-debugging (format t "~%Step replace token, item no ~D, new token ~S, old token ~S"
                          item-no new-token
                          (boxer::cha-at-cha-no (boxer::point-row) *stepper-item-no*)))
  (if (boxer::virtual-copy? new-token)
      (setq new-token (top-level-print-eval-object new-token)))
  (let ((start-cha-no (step-delete-token item-no)))
    (step-insert-token new-token start-cha-no)))

(defun step-advance-line ()
  (incf *stepper-row-no*)
  (setq *stepper-item-no* 0)
  (unless (= *stepper-row-no* (boxer::length-in-rows (boxer::point-box)))
    (boxer::com-next-row)
    (boxer::com-beginning-of-row)))

(defun step-handle-list-rest-arg ()
  (step-delete-to-end-of-line))

;; is this right?
(defun step-handle-box-rest-arg ()
  (step-delete-to-end-of-line))

(defun step-delete-to-end-of-line ()
  (boxer::delete-chas-to-end-of-row boxer::*point* :fixed))


(defun step-self-evaluating-object (object)
 (if (boxer::virtual-copy? object)
     (setq object (top-level-print-eval-object object)))
; (when (boxer::box? object)
;   (boxer::set-name object nil))
 (step-replace-token *stepper-item-no* object)
 (step-advance-token))

(defun step-maybe-copy-data-box (object)
  (if (boxer::box? object)
      (boxer::copy-box object)
      object))

(defun step-start-function-sequence (function)
  (when-debugging (format t "~%Step start function sequence ~S" function))
  (cond ((interpreted-boxer-function? function)
         (step-replace-token *stepper-item-no*
          ;; put int the name, too.
          (copy-thing (interpreted-boxer-function-backpointer
                       function))))
        (t (step-replace-token *stepper-item-no*
            (make-primitive-box function)))))

(defun stepper-enter-box (function-item-no)
  (when-debugging (format t "~%stepper enter box: ~D" function-item-no))
  (move-point-to-item-no function-item-no)
  (boxer::com-enter-box)
  (boxer::com-beginning-of-row))

;; call this when all the args have been evaluated.
(defun step-finish-eval-args (final-arg-position)
  (when-debugging
   (format t
           "~%step finish eval args, final arg position ~d, current+1 ~d"
           final-arg-position
           (1+& *stepper-item-no*)))
  (let ((begin-pos
         (safe-end-of-nth-chunk-cha-no
          (boxer::point-row)
          *stepper-item-no*))
        (end-pos
         (safe-end-of-nth-chunk-cha-no
          (boxer::point-row)
          (1-& final-arg-position))))
    (boxer::move-point (values (boxer::point-row) begin-pos))
    (dotimes (i (- end-pos begin-pos))
      (boxer::simple-delete-cha boxer::*point* :moving))
    (boxer::modified (boxer::point-box)))) ; ???

(defun step-function-return-sequence (value)
  (when-debugging (format t "~%step function return sequence, value=~s" value))
  (boxer::com-exit-box)
  (boxer::com-rubout)
  (stepper-replace-previous-expression value))

(defun step-handle-run-list-continuation (list)
  (when-debugging (format t "~%step-handle-run-list-continuation, value=~s" list))
  (boxer::com-exit-box)
  (boxer::com-rubout)
  (stepper-replace-previous-expression (car list))
  (stepper-replace-rest-of-line (cdr list)))

;; the spacing here isn't quite right
;; some of these whitespace things are clearly in the wrong place
(defun step-handle-infix-function (infix-function next-token)
  (step-delete-token *stepper-item-no*)
  (step-delete-excess-whitespace)
  (step-previous-token)
  (step-delete-excess-whitespace)
  (step-insert-token next-token)
  (step-delete-excess-whitespace)
  (step-start-function-sequence infix-function))

(defun step-update-number-to-box (box)
  (step-replace-token *stepper-item-no* box))

(defun make-primitive-box (boxer-function)
  (let ((arglist
         (mapcan #'(lambda (u)
                     (if (consp u)
                         (if (eq (symbol-package (car u))
                                 boxer::pkg-bu-package)
                             (list (string (car u)) (string (cadr u)))
                             (list (string (cadr u))))
                         (list (string u))))
                 (compiled-boxer-function-arglist boxer-function)))
        (guts (concatenate
               'string
               "<"
               (string (compiled-boxer-function-name
                        boxer-function))
               "-primitive"
               ">")))
    (boxer::make-box
     (cond ((null arglist) `((,guts)))
           ((null (cdr arglist)) `(("input" . ,arglist) (,guts)))
           (t `(("inputs" . ,arglist) (,guts))))
     'boxer::doit-box)))

(defun stepper-replace-previous-expression (value)
  (when-debugging
   (format t "~%stepper replace previous expression, value ~s" value))
  (step-insert-token
   (if (eq value *novalue*)
       (boxer::make-box (list nil) 'boxer::doit-box)
       value)))

(defun stepper-replace-rest-of-line (list)
  (when-debugging (format t "~%stepper-replace-rest-of-line: ~S" list))
  (boxer::com-kill-to-end-of-row)
  (dolist (item list)
    (step-insert-token item)
    (boxer::insert-cha boxer::*point* #\space)))

(defun move-point-to-item-no (n)
  (boxer::move-point (values
                      (boxer::point-row)
                      (safe-beginning-of-nth-chunk-cha-no (boxer::point-row) n))))

(defun safe-beginning-of-nth-chunk-cha-no (row n)
  (if (zerop n) 0
      (or (boxer::beginning-of-nth-chunk-cha-no row n)
          (1-& (boxer::length-in-chas row)))))

(defun safe-end-of-nth-chunk-cha-no (row n)
  (if (minusp n) 0
    (or (boxer::end-of-nth-chunk-cha-no row n)
        (1-& (boxer::length-in-chas row)))))

(defun step-replace-input-line-with-values (arglist)
  (when-debugging (format t "~%step replace input line: arglist=~s" arglist))
  (boxer::com-beginning-of-row)
  (boxer::com-kill-to-end-of-row)
;  (boxer::redisplay)
  (dolist (name arglist)
    (if (consp name) (setq name (cadr name)))
    (let ((value (copy-thing (boxer-symeval name))))
      (if (numberp value)
          (setq value (boxer::data-boxify value)))
      (boxer-set-internal
       name
       (setq value
             (top-level-print-eval-object value)))
      (step-insert-token value)
      (let ((name-row (boxer::make-name-row (list name))))
        (boxer::set-name value name-row)
        (boxer::update-bindings name-row))
      (boxer::insert-cha boxer::*point* #\space :moving)))
  ;; Remove the dynamic bindings.  We must leave an empty frame,
  ;; though.
  (dynamically-unbind-variables)
  (dynamically-bind-variables-and-locals nil nil))

(defun stepper-signal-error (error-box)
  (boxer::move-point (values (boxer::last-inferior-row *stepping-box*) 0))
  (step-insert-token error-box)
  (step-restore-cursor-position)
  (throw 'stepper-error t))


