;;; -*- Mode:lisp;Syntax:Common-Lisp; Package:BOXER; Base:10.-*-

#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                          +-Data--+
                 This file is part of the | BOXER | system
                                          +-------+


 This file contains utilies for creating and using Modes


Modification History (most recent at top)
 2/15/03 merged current LW and MCL files, no diffs, updated copyright

|#

(in-package :boxer)

(defvar *active-modes* nil)

(defun add-mode (mode)
  (unless (fast-memq mode *active-modes*)
    (push mode *active-modes*)))

(defun remove-mode (mode)
  (setq *active-modes* (fast-delq mode *active-modes*)))

(defun reset-modes ()
  (setq *active-modes* nil))


;; we need to record all existing comtabs in case of key name changes
(defvar *existing-comtabs* nil)

(defun make-comtab ()
  (let ((new-comtab (make-hash-table)))
    (push new-comtab *existing-comtabs*)
    new-comtab))

(defun remove-comtab (comtab)
  (setq *existing-comtabs* (fast-delq comtab *existing-comtabs*)))


(defclass basic-mode
    ()
  ((name :initform nil :initarg :name)
   (comtab :initform (make-comtab))))

;; more specific behaviors for other mode
;; for example, search mode should splice itself out of
;; the mode list if it the lookup is unsuccessful
(defmethod lookup-mode-key ((self basic-mode) key-name)
  (gethash key-name (slot-value self 'comtab)))

(defvar *global-top-level-mode* (make-instance 'basic-mode
					       :name `global-top-level))

(defun record-vanilla-key (name fun)
  (let ((vanilla-comtab (when *global-top-level-mode*
			  (slot-value *global-top-level-mode* 'comtab))))
    (if (null vanilla-comtab)
	(warn "The Vanilla comtab is not defined yet")
	(setf (gethash name vanilla-comtab)
	      (boxer-eval::encapsulate-key-function fun)))))

(defmacro defboxer-mode-key (key-spec mode function)
  (let* ((shift-bits (if (listp key-spec) (cadr key-spec) 0))
	 (key-name
	  (if (zerop shift-bits)
	      key-spec
	      (boxer::intern-in-bu-package
	       (symbol-format nil "~A-~A"
			      (get-shift-names shift-bits)
			      (car key-spec))))))
    `(if (not (typep ,mode 'basic-mode))
      (error "~S is not a defined editor mode" ,mode)
      (setf (gethash ',key-name (slot-value ,mode 'comtab))
       (boxer-eval::encapsulate-key-function ',function)))))

(defun defboxer-mode-key-internal (key-name mode function)
  (setf (gethash key-name (slot-value mode 'comtab))
	(boxer-eval::encapsulate-key-function function)))

;; loop through the list of current modes looking for key-name
;; return NIL

(defun mode-key (key-name)
  (dolist (mode *active-modes*)
    (let ((key-binding (lookup-mode-key mode key-name)))
      (unless (null key-binding) (return key-binding)))))


;; keydefs should consist of keyspec, function pairs
(defmacro defbasic-mode (mode-name &rest keydefs)
  (let ((mode-var (gensym)))
    `(progn
       (defclass ,mode-name
         (basic-mode)
         ()
         )
       (defvar ,mode-var (make-instance ',mode-name))
       (defun ,mode-name () ,mode-var)
       . ,(with-collection
            (dolist (keyfunpair keydefs)
              (collect `(defboxer-mode-key ,(car keyfunpair) (,mode-name)
                          ,(cadr keyfunpair))))))))




