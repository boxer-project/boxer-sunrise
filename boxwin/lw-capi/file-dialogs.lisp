;;;;        Boxer
;;;;        Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;        Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;        used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;        Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;        https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                             +-Data--+
;;;;                    This file is part of the | BOXER | system
;;;;                                             +-------+
;;;;

(in-package :boxer-window)

;;; File System Helpers

(defvar *boxer-file-filters* '("Box Files" "*.box;*.box~;*.box#;*.boxer" "All Files" "*.*"))
(defvar *boxer-save-file-filters*
  '("Box File 98-2022" "*.box"
    "Boxer Document" "*.boxer"
    "All Files" "*.*")
  "For saving files, we want to keep .box and .boxer separate so the user can choose
  between them."
  )


;; mac legacy vars, see boxer-new-file-dialog for usage
(defvar *save-file-format* :BOXR)
(defvar *dialog-ro* 0)

(defun boxer::boxer-open-file-dialog (&key (prompt "Open File:") directory)
  (multiple-value-bind (path success?)
      (capi:prompt-for-file prompt :filters *boxer-file-filters*
                            :pathname (merge-pathnames
                                         (or directory "")
                                         boxer::*boxer-pathname-default*)
                            :operation :open :owner *boxer-frame*)
    (if (null success?)
        (throw 'boxer::cancel-boxer-file-dialog nil)
      path)))

(defun boxer::boxer-open-lost-file-dialog (&key (prompt "Can't find") directory)
  (let ((missing-text (or (and directory
                               (format nil "~A ~A. Please locate file:"
                                       prompt (file-namestring directory)))
                          (format nil "~A box file. Please locate file:" prompt))))
    (multiple-value-bind (path success?)
        (capi:prompt-for-file missing-text :filters *boxer-file-filters*
                              :operation :open
                              :pathname (merge-pathnames
                                         (or directory "")
                                         boxer::*boxer-pathname-default*)
                              :owner *boxer-frame*)
      (if (null success?)
          (throw 'boxer::cancel-boxer-file-dialog nil)
          path))))

;; on the mac, *save-file-format* and *dialog-ro* can be side-effected by the
;; custom dialog
(defun boxer::boxer-new-file-dialog (&key (prompt "Save As") directory box
                                   no-read-only?)
  (unless (null box)
    (setq *save-file-format* (or (boxer::getprop box :preferred-file-format)
                                 :boxer)
          *dialog-ro* (if (and (null no-read-only?) (boxer::read-only-box? box))
                          1 0)))
  (multiple-value-bind (path success?)
        (capi:prompt-for-file prompt
                              :filters *boxer-save-file-filters*
                              :pathname (boxer::untitled-filename
                                          (cl-fad:merge-pathnames-as-directory
                                            (or directory
                                                (cl-fad:merge-pathnames-as-directory boxer::*boxer-pathname-default*))))
                              :operation :save
                              :if-does-not-exist :ok :owner *boxer-frame*)
    (if (null success?)
          (throw 'boxer::cancel-boxer-file-dialog nil)
          path)))
