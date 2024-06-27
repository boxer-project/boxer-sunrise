;;;;  ;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
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
;;;;
;;;;      This continues utilities for various types Copy, Cut, Paste, Kill, Yank, Redo,
;;;;      and Undo support.  We want to continue supporting (for at least a while), emacs
;;;;      Kill/Yank, OS style Cut/Copy/Paste, and some level of Undo/Redo support.
;;;;
(in-package :boxer)

;; TODO Refactor contents of defboxer-command COM-ROTATE-KILL-BUFFER, so it's *kill-buffer*
;; manipulations are in this file.

;for control-y
(DEFMACRO KILL-BUFFER-TOP ()
          '(CAR *KILL-BUFFER*))

;;; Used for modern OS style Cut/Copy/Paste
(defvar *current-paste-item* nil
  "This is a single item, and is the same as member you push on the the *kill-buffer*.
  Other than only being a single item (rather than a list), this is different in that
  will only track full copy/paste operations on it.  The kill ring tracks all sort
  of extra items such as single character deletes.")

(defvar *kill-buffer-last-direction* nil)

(defvar *kill-buffer-length* 8)

(defvar *kill-buffer* (make-list *kill-buffer-length*))

(defvar *number-of-non-kill-commands-executed* 0)

(defun kill-buffer-push (item direction &key (include-paste nil))
  "Using :include-paste t, will also put the item on the paste clipboard in addition
  to the kill-buffer."
  (when (null item)
    (setq item :newline))

  (if (<= *number-of-non-kill-commands-executed* 1)
      (if (eq direction *kill-buffer-last-direction*)
          (cond ((eq direction ':forward)
                 (ensure-list item)
                 (ensure-list (car *kill-buffer*))
                 (setf (car *kill-buffer*)
                       (nconc (car *kill-buffer*) item)))
                ((eq direction ':backward)
                 (ensure-list (car *kill-buffer*))
                 (setf (car *kill-buffer*)
                       (cons item (car *kill-buffer*)))))
          (push item *kill-buffer*))
      (push item *kill-buffer*))

  (when include-paste
    (setf *current-paste-item* item))
  ;; We don't want every deleted char to be in the clipboard, so adding to the
  ;; clipboard is now limited to the kill-region and copy-region commands
                                        ;  (write-system-scrap (car *kill-buffer*))
  (when (> (length *kill-buffer*) *kill-buffer-length*)
    (let ((objs-for-deallocation (car (nthcdr *kill-buffer-length* *kill-buffer*))))
      (rplacd (nthcdr (1- *kill-buffer-length*) *kill-buffer*) nil)
      (queue-editor-objs-for-deallocation objs-for-deallocation)))
  (setq *kill-buffer-last-direction* direction)
  (setq *number-of-non-kill-commands-executed* 0)
  *kill-buffer*)
