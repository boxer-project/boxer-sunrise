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
;;;;       Vars and functions for keeping track of mouse location and movement
;;;;
(in-package :boxer)

;; mouse motion
(defvar *track-mouse-x* 0
  "Original x position in the window pane.")
(defvar *track-mouse-y* 0
  "Original y position in the window pane.")

(defvar *document-mouse-x* 0
  "Document x position based on transforming *track-mouse-x* with the current zoom and scroll values.")
(defvar *document-mouse-y* 0
  "Document y position based on transforming *track-mouse-y* with the current zoom and scroll values.")

(defvar *mouse-down-p* nil
  "If true, the primary mouse button is currently depressed (usually the left mouse button).")

(defun boxer-pane-mouse-down? ()
  *mouse-down-p*)

(defun boxer-pane-mouse-position ()
  ;; must allow track mouse handler in the interface(boxer) process to run
  #+lispworks (mp::process-allow-scheduling)
  ;; (values *track-mouse-x* *track-mouse-y*)
  (values *document-mouse-x* *document-mouse-y*)
  )

(defmacro with-mouse-tracking (((original-x-variable original-x-value)
        (original-y-variable original-y-value)
        &key
                                event-skip timeout action
                                (body-function-name (gensym)))
             &body body)
  (declare (ignore event-skip timeout))
  `(let ((,original-x-variable ,original-x-value)
         (,original-y-variable ,original-y-value)
         (moved-p nil))
     (flet ((,body-function-name () . ,body))
       (with-mouse-cursor (,action)
         (do ((last-mouse-x -1) (last-mouse-y -1))
             ((not (boxer-pane-mouse-down?))
              (values ,original-x-variable ,original-y-variable moved-p))
           (multiple-value-setq  (,original-x-variable ,original-y-variable)
               (boxer-pane-mouse-position))
           (unless moved-p
             (unless (and (= ,original-x-variable ,original-x-value)
                          (= ,original-y-variable ,original-y-value))
               (setq moved-p t)))
           (unless (and (= ,original-x-variable last-mouse-x)
                        (= ,original-y-variable last-mouse-y))
             (setq last-mouse-x ,original-x-variable last-mouse-y ,original-y-variable)
             (,body-function-name)))))))
