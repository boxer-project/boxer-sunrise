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
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
(in-package :boxer)

(defmacro defmethod-box-godot-sync (name &rest args)
  "Like DEFMETHOD, but the method body only runs when *box-godot-sync* is T."
  (let* ((pos (position-if #'listp args))          ; first list = the lambda list
         (qualifiers (subseq args 0 pos))
         (lambda-list (nth pos args))
         (body (nthcdr (1+ pos) args))
         (header '()))
    ;; Keep docstring and declarations outside the conditional
    (loop while (and body
                     (or (and (stringp (first body)) (rest body))
                         (and (consp (first body))
                              (eq (caar body) 'declare))))
          do (push (pop body) header))
    `(defmethod ,name ,@qualifiers ,lambda-list
       ,@(nreverse header)
       (format t "~%method: ~A" (quote ,name))
       (when (eq *box-godot-sync* t)
         ,@body))))

;;;
;;; chas
;;;

(defmethod fast-chas-array-set-cha :after (chas-arr cha-no cha)
  (let ((row (chas-array-parent-row chas-arr)))
    (when row
      (map-over-screen-objs row #'(lambda (scr-obj)
                                    (let ((godot-row (fetch-godot-obj scr-obj)))
                                      (godot-insert-cha-signal godot-row cha cha-no)))))))
