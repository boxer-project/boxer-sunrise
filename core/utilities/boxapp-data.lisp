;;;;
;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
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

(defvar *boxer-appdata-template* '(
        (:boxer-appdata . "Required for correct cl-json serialization")
        (:recent-files . ())))

(defvar *boxer-appdata* *boxer-appdata-template*
  "Data structure that will contain recently opened files, pinned files,
  and similar items.")

(defvar *max-number-of-recent-files* 10
    "How many recent files to store by default")

(defun get-boxapp-data-filepath ()
  "This function retrieves the path on disk where a users appdata should be
  stored. This is OS specific, on MacOS it's typically something like
  `~/Library/Application Support/Boxer-data.lisp` and on Windows in the users local
  appdata directory"
  (merge-pathnames "Boxer/boxapp-data.lisp"
    #+(or macosx windows) (sys:get-folder-path :appdata)
    #+linux (uiop:xdg-config-home)))


(defun load-appdata (&optional (filepath (get-boxapp-data-filepath)))
  "If this appdata file for boxer exists, we'll load the data from that, otherwise we'll
  use the default template aleady in *boxer-appdata*"
  ;; TODO: Think about, and add tests for if the data is corrupted or invalid.
  (if (probe-file filepath)
    (ignore-errors
      (with-open-file (s filepath :direction :input)
        (setf *boxer-appdata* (read s))))))

(defun save-appdata (&optional (filepath (get-boxapp-data-filepath)))
  "Saves the working Boxer data back to it's entry in the OS specific appdata location."
  (ensure-directories-exist filepath)
  (with-open-file (s filepath :direction :output :if-does-not-exist :create :if-exists :supersede)
    (write *boxer-appdata* :stream s)))

(defun reset-recent-files ()
    (setf (cdr (assoc :recent-files *boxer-appdata*)) '())
    (save-appdata))

(defun get-recent-files ()
  ;; TODO Remove files that may not exist according to probe-file
  (handler-case
    (let ((recent-files (cdr (assoc :recent-files *boxer-appdata*))))
      (subseq recent-files 0 (if (< (length recent-files)  *max-number-of-recent-files*)
                                 (length recent-files)
                                 *max-number-of-recent-files*)))
    (type-error (e)
      (format t "Error parsing recent files: ~A~%" e)
      (setf *boxer-appdata* *boxer-appdata-template*)
      '())))

(defun add-recent-file (path &optional label)
    ;; First remove any duplicates...
    (setf (cdr (assoc :recent-files *boxer-appdata*))
          (remove-if #'(lambda (x)
                         (uiop:pathname-equal path (cdr (assoc :path x))))
                     (cdr (assoc :recent-files *boxer-appdata*))))

    ;; Then add it to the top of the list
    (push `((:path . ,path)
            (:label . ,path)) (cdr (assoc :recent-files *boxer-appdata*)))
    (assoc :recent-files *boxer-appdata*)
    (save-appdata))
