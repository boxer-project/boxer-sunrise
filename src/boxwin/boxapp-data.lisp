;;;;
;;;;    Boxer
;;;;    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;        Support for recently opened files

(in-package :boxer)

(defvar *boxer-appdata* '(
        (:boxer-appdata . "Required for correct cl-json serialization")
        (:recent-files . ()))
    "Data structure that will contain recently opened files, pinned files,
    and similar items.")

(defvar *max-number-of-recent-files* 10
    "How many recent files to store by default")

(defun load-recent-files (&optional stream)
)

(defun save-recent-files (&optional stream)
)

(defun reset-recent-files ()
    (setf (cdr (assoc :recent-files *boxer-appdata*)) '()))

(defun get-recent-files ()
    (cdr (assoc :recent-files *boxer-appdata*)))

(defun add-recent-file (path &optional label)
    ;; First remove any duplicates...
    (setf (cdr (assoc :recent-files *boxer-appdata*))
          (remove-if #'(lambda (x)
                         (equal path (cdr (assoc :path x))))
                     (get-recent-files)))

    ;; Then add it to the top of the list
    (push `((:path . ,path)
            (:label . ,path)) (cdr (assoc :recent-files *boxer-appdata*)))
    (assoc :recent-files *boxer-appdata*)
)
