;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



  Foreign data in the evaluator

  Interface to foreign data types in the evaluator consists of:
   1) classes of objects built from the FOREIGN-DATA class
   2) The following Methods for those classes:
    foreign-data-set <fd> <new-value>      ; used by CHANGE (default=error)
    virtual-copy-foreign-data <fd>         ; used by virtual-copy (default=error)
    make-editor-box-from-foreign-data <fd> ; used by the printer (default=error)
    port-to-foreign-data <fd>              ; used by port-to (default OK)
    lookup-variable-in-foreign-data <fd> <var> ; used by TELL (default OK)


Modification History (most recent at top)
 2/11/03 merged current LW and MCL files


|#

(in-package :boxer)

(defclass foreign-data
  ()
  ()
  (:metaclass block-compile-class)
  (:documentation "A Mixin for Foreign data types inthe boxer evaluator"))

;; called from CHANGE.  <new-value> will be standard boxer data, possibly a
;; port-to other foreign-data
(defmethod foreign-data-set ((fd foreign-data) new-value)
  (declare (ignore new-value))
  (boxer-eval::primitive-signal-error "No set method defined for "
                                (type-of fd)
                                " type of foreign data"))

;; converts foreign data to a boxer object
(defmethod virtual-copy-foreign-data ((fd foreign-data))
  (boxer-eval::primitive-signal-error "No Virtual Copy method defined for "
                                (type-of fd)
                                " type of foreign data"))

;; how to convert foreign data back to boxer editor structure
(defmethod make-editor-box-from-foreign-data ((fd foreign-data))
  (error "No print method defined for ~A" fd))

;; a hook for any special handling during port-to. Default just makes a port
(defmethod port-to-foreign-data ((fd foreign-data))
  (make-virtual-port :target fd))


;; if TELL is passed some foreign data, it will call this generic function
(defmethod lookup-variable-in-foreign-data ((fd foreign-data) var)
  (declare (ignore var))
  nil)

(defmethod boxer-eval::boxer-symeval-dots-list-fd (error-symbol
                                             (fd foreign-data) list)
  (declare (ignore list))
  (boxer-eval::signal-error :dots-variable-lookup "in" error-symbol))





