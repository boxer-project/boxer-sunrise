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
;;;;     Debugging tree views for Boxer Development
;;;;
(in-package :boxer-window)


(defun outline-children-function (x)
  (let ((togo '()))
    (cond ((box? x)
           (do ((row (boxer::first-inferior-row x) (boxer::next-row row)))
               ((null row))
             (print row)
             (setf togo (cons row togo))))
          ((boxer::row? x)
           (boxer::do-row-chas ((cha x))
             (when (box? cha)
               (setf togo (cons cha togo))))
          )
          (t nil))
    (reverse togo)))

(defun outline-print-function (x)
  (cond ((box? x)
         (box::name x))
        (t
         (format nil "~A" x)))
)

(capi:define-interface boxer-outline-tree ()
  ()
  (:panes
   (tree capi:tree-view
         :roots (list boxer::*initial-box*)
         :children-function #'outline-children-function
        ;  :image-lists (list :normal *my-image-list*)
        ;  :image-function #'(lambda (x) (mod (1- x) 4))
         :visible-min-width 200
         :visible-min-height 200
         :retain-expanded-nodes t
         :print-function #'outline-print-function
         :selection-callback #'(lambda (item self)
                                 (declare (ignore self))
                                 (format t "~&Select item ~S~%" item))
         :action-callback #'(lambda (item self)
                              (declare (ignore self))
                              (format t "~&Activate item ~S~%" item)
                              (when (box? item)
                                (boxer::set-outermost-box item)))
         :delete-item-callback #'(lambda (self item)
                                   (declare (ignore self))
                                   (format t "~&Delete item ~S~%" item))))
  (:layouts
   (default-layout
    capi:simple-layout
    '(tree)))
  (:default-initargs
   :title "Outline View"))

(defun show-outline-tree ()
  (capi:display (make-instance 'boxer-outline-tree :title "Outline")))
