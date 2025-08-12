;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER-*-

#|

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+

  searching commands


Modification History (most recent at top)

 4/21/03 merged current LW and MCL files, no diffs, updated copyright

|#

(in-package :boxer)


;; these are hung on the vanilla comtab

(defboxer-command COM-SEARCH-FORWARD ()
  "Incrementally search forward"
  (reset-search-mode (search-mode))
  (new-current-position (search-mode) *point*)
  (push-bp (search-mode) *point*)
  (setf (slot-value (search-mode) 'stop-box) (point-box))
  (setf (slot-value (search-mode) 'direction) 1)
  (add-mode (search-mode))
  (display-pattern-message)
  boxer-eval::*novalue*)


(defboxer-command COM-SEARCH-BACKWARD ()
  "Incrementally search backward"
  (reset-search-mode (search-mode))
  (new-current-position (search-mode) *point*)
  (push-bp (search-mode) *point*)
  (setf (slot-value (search-mode) 'stop-box) (point-box))
  (setf (slot-value (search-mode) 'direction) -1)
  (add-mode (search-mode))
  (display-pattern-message "Searching backward")
  boxer-eval::*novalue*)


;;; These commands are inside of the search-mode comtab

(defboxer-command COM-SEARCH-CHAR (char)
  "Adds character to the search pattern, then searches for the pattern"
  (add-pattern-char char)
  (display-pattern-message (when (minusp (slot-value (search-mode) 'direction))
                             "Searching backward"))
  (multiple-value-bind (row cha-no offset)
      (search-mode-search (search-mode))
    (cond ((null row)
           (beep)
           (display-pattern-message "Failing Search"))
          (t
           (move-with-offset (search-mode) row cha-no offset)
           ;; handle screen-box
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
  boxer-eval::*novalue*)

(defboxer-command COM-SEARCH-NEWLINE ()
  "Adds a carriage return to the search pattern, then searches for the pattern"
  (com-search-exit-named-box)
  (add-pattern-cr)
  (display-pattern-message (when (minusp (slot-value (search-mode) 'direction))
                             "Searching backward"))
  (multiple-value-bind (row cha-no offset)
      (search-mode-search (search-mode))
    (cond ((null row)
           (beep)
           (display-pattern-message "Failing Search"))
          (t
           (move-with-offset (search-mode) row cha-no offset)
           ;; handle screen-box
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
  boxer-eval::*novalue*)

(defboxer-command COM-SEARCH-BOX (type)
  "Adds a box of particular type to the search pattern"
  (com-search-exit-named-box)
  (add-pattern-box (make-box-pattern :type type))
  (display-pattern-message (when (minusp (slot-value (search-mode) 'direction))
                             "Searching backward"))
  (multiple-value-bind (row cha-no offset)
      (search-mode-search (search-mode))
    (cond ((null row)
           (beep)
           (display-pattern-message "Failing Search"))
          (t
           (move-with-offset (search-mode) row cha-no offset)
           ;; handle screen-box
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
  boxer-eval::*novalue*)

(defboxer-command COM-SEARCH-NAMED-BOX ()
  "Add a named box to the search pattern"
  (let ((name (make-storage-vector)))
    (add-pattern-box (make-box-pattern :name name))
    (setf (slot-value (search-mode) 'box-pattern) name))
  (display-pattern-message (when (minusp (slot-value (search-mode) 'direction))
                             "Searching backward"))
  (multiple-value-bind (row cha-no offset)
      (search-mode-search (search-mode))
    (cond ((null row)
           (beep)
           (display-pattern-message "Failing Search"))
          (t
           (move-with-offset (search-mode) row cha-no offset)
           ;; handle screen-box
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
  boxer-eval::*novalue*)


(defboxer-command COM-SEARCH-EXIT-NAMED-BOX ()
  "Stops appending search characters into the named box section of the search pattern"
  (unless (null (slot-value (search-mode) 'box-pattern))
    ;; eliminate zero length vectors
    (let ((pattern-box (sv-nth (1-& (storage-vector-active-length
                                     (search-mode-pattern)))
                               (search-mode-pattern))))
      (when (and (box-pattern-p pattern-box)
                 (not (null (box-pattern-name pattern-box)))
                 (zerop& (storage-vector-active-length
                          (box-pattern-name pattern-box))))
        (setf (box-pattern-name pattern-box) nil)))
    (setf (slot-value (search-mode) 'box-pattern) nil))
  boxer-eval::*novalue*)


(defboxer-command COM-SEARCH-DATA-BOX ()
  "Adds a data box to the search pattern, then searches"
  (com-search-box 'data-box))

(defboxer-command COM-SEARCH-DOIT-BOX ()
  "Adds a doit box to the search pattern, then searches"
  (com-search-box 'doit-box))

(defboxer-command COM-SEARCH-SPRITE-BOX ()
  "Adds a Sprite box to the search pattern, then searches"
  (com-search-box 'sprite-box))

(defboxer-command COM-SEARCH-PORT ()
  "Adds a Port to the search pattern, then searches"
  (com-search-box 'port-box))

(defun move-with-offset (search-mode row cha-no offset)
  (set-current-position-values search-mode row cha-no)
  (let ((*move-bp-zoom-pause-time* 0))
    (cond ((or (null offset) (minusp (slot-value search-mode 'direction)))
           (move-to-bp-values *point* (values row cha-no)))
          ((numberp offset)
           (move-to-bp-values *point* (values row (+ cha-no offset))))
          ((consp offset)
           (let ((dest-row (do ((drow row)
                                (i (abs (car offset)) (1-& i)))
                               ((or (null drow) (zerop& i)) drow)
                             (setq drow (if (minusp (car offset))
                                            (previous-row row)
                                            (next-row row))))))
             (if (null dest-row)
                 (move-to-bp-values *point* (values row cha-no))
                 (move-to-bp-values *point*
                                    (values dest-row
                                            (+ cha-no (cdr offset)))))))
          (t (error "Don't know how to handle search offset: ~A"
                    offset)))))

;;; Try moving downward 1st
(defun calculate-new-screen-box ()
  (dolist (screen-box (screen-objs (point-box))
           (car (displayed-screen-objs (point-box))))
    (when (superior? screen-box (point-screen-box))
      (return screen-box))))


(defboxer-command COM-DELETE-SEARCH-CHAR ()
  "Removes the last character from the search pattern"
  (multiple-value-bind (last-char exit-name?)
      (delete-pattern-char)
    (display-pattern-message
     (when (minusp (slot-value (search-mode) 'direction))
       "Searching backward"))
    (when exit-name? (com-search-exit-named-box))
    (cond ((eq last-char :empty) (beep))
          ((eq last-char :search)
           ;; pop the Bp-Stack and move there
           (let ((last-bp (pop (slot-value (search-mode) 'bp-stack))))
             (if (minusp (slot-value (search-mode) 'direction))
                 (move-with-offset (search-mode)
                                   (bp-row last-bp) (bp-cha-no last-bp) nil)
                 (move-with-offset (search-mode)
                                   (bp-row last-bp) (bp-cha-no last-bp)
                                   (pattern-offset (slot-value (search-mode)
                                                               'pattern)))))
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))
          ;; check to see if the pattern is empty now
          ((zerop& (storage-vector-active-length (search-mode-pattern)))
           (let ((last-bp (car (slot-value (search-mode) 'bp-stack))))
             (unless (null last-bp)
               (move-with-offset (search-mode)
                                 (bp-row last-bp) (bp-cha-no last-bp) nil)
               (unless (eq (screen-obj-actual-obj (point-screen-box))
                           (point-box))
                 (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
          (t
           (multiple-value-bind (row cha-no offset)
               (search-mode-search (search-mode))
             (cond ((null row)
                    (beep)
                    (display-pattern-message "Failing Search"))
                   (t
                    (move-with-offset (search-mode) row cha-no offset)
                    ;; handle screen-box
                    (unless (eq (screen-obj-actual-obj (point-screen-box))
                                (point-box))
                      (setf (bp-screen-box *point*)
                            (calculate-new-screen-box)))))))))
  boxer-eval::*novalue*)


(defboxer-command COM-END-SEARCH ()
  "Exit search mode, leaving the cursor where it is"
  (remove-mode (search-mode))
  (save-search-mode (search-mode))
  (reset-search-mode (search-mode))
  (clear-all-pattern-messages)
  boxer-eval::*novalue*)

(defboxer-command COM-ABORT-SEARCH ()
  "Stop searching and return the cursor to its original location"
  (let ((*move-bp-zoom-pause-time* 0))
    (move-to-bp-values *point*
                       (bp-values (or (car (last (slot-value (search-mode)
                                                             'bp-stack)))
                                      (slot-value (search-mode)
                                                  'current-position)))))
  (remove-mode (search-mode))
  (reset-search-mode (search-mode))
  (clear-all-pattern-messages)
  (boxer-editor-warning "Search Cancelled !")
  boxer-eval::*novalue*)

(defboxer-command COM-QUOTE-SEARCH-CHAR ()
  "adds the next typed char to the search pattern"
  (add-pattern-char (get-character-input *boxer-pane*))
  (display-pattern-message (when (minusp (slot-value (search-mode) 'direction))
                             "Searching backward"))
  (multiple-value-bind (row cha-no offset)
      (search-mode-search (search-mode))
    (cond ((null row)
           (beep)
           (display-pattern-message "Failing Search"))
          (t
           (move-with-offset (search-mode) row cha-no offset)
           ;; handle screen-box
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
  boxer-eval::*novalue*)

(defboxer-command COM-EXPAND-SEARCH ()
  "Expands the search space to include the superior box"
  (com-search-exit-named-box)
  (let ((stop-box (slot-value (search-mode) 'stop-box)))
    (cond ((eq stop-box *initial-box*)
           (display-pattern-message "Search is at top level"))
          (t
           (let ((sup (superior-box stop-box)))
             (cond ((null sup))
                   (t
                    (if (eq sup *initial-box*)
                        (display-pattern-message "Searching top level")
                        (display-pattern-message "Expanding Search"))
                    (setf (slot-value (search-mode) 'stop-box) sup)
                    (multiple-value-bind (row cha-no offset)
                        (search-mode-search (search-mode) *point*)
                      (cond ((null row)
                             (beep)
                             (display-pattern-message "Failing Search"))
                            (t
                             (move-with-offset (search-mode) row cha-no offset)
                             ;; handle screen-box
                             (unless (eq (screen-obj-actual-obj
                                          (point-screen-box))
                                         (point-box))
                               (setf (bp-screen-box *point*)
                                     (calculate-new-screen-box))))))))))))
  boxer-eval::*novalue*)

(defboxer-command COM-SEARCH-FORWARD-AGAIN ()
  "Search forward again using the existing pattern"
  (com-search-exit-named-box)
  ;; check for empty pattern
  (cond ((empty-pattern? (search-mode))
         (restore-search-mode (search-mode)))
        (t
         (add-pattern-char :search)
         ;; now push onto the bp-stack
         (push-bp (search-mode) (slot-value (search-mode) 'current-position))))
  ;; need to do this because we MAY be going backward at the time
  (setf (slot-value (search-mode) 'direction) +1)
  (display-pattern-message)
  (multiple-value-bind (row cha-no offset)
      (search-mode-search (search-mode) *point*)
    (cond ((null row)
           (display-pattern-message "Failing Search"))
          (t
           (move-with-offset (search-mode) row cha-no offset)
           ;; handle screen-box
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
  boxer-eval::*novalue*)

(defboxer-command COM-SEARCH-BACKWARD-AGAIN ()
  "Search backward again using the existing pattern"
  (com-search-exit-named-box)
  (cond ((empty-pattern? (search-mode))
         (restore-search-mode (search-mode)))
        (t
         (add-pattern-char :search)
         ;; now push onto the bp-stack
         (push-bp (search-mode) *point*)))
  ;; need to do this because we MAY be going backward at the time
  (setf (slot-value (search-mode) 'direction) -1)
  (display-pattern-message "Searching backward")
  (multiple-value-bind (row cha-no offset)
      (search-mode-search (search-mode)
                          (let ((rbp (make-bp :fixed)))
                            (if (zerop& (point-cha-no))
                                (let ((prow (previous-row (point-row))))
                                  (if (null prow)
                                      (setf (bp-row rbp)
                                            (superior-row (point-box))
                                            (bp-cha-no rbp)
                                            (1-& (cha-cha-no
                                                  (superior-row (point-box))
                                                  (point-box))))
                                      (setf (bp-row rbp) prow
                                            (bp-cha-no rbp)
                                            (length-in-chas prow))))
                                (setf (bp-row rbp) (point-row)
                                      (bp-cha-no rbp) (1-& (point-cha-no))))
                            rbp))
    (cond ((null row)
           (display-pattern-message "Failing Search"))
          (t
           (move-with-offset (search-mode) row cha-no offset)
           ;; handle screen-box
           (unless (eq (screen-obj-actual-obj (point-screen-box)) (point-box))
             (setf (bp-screen-box *point*) (calculate-new-screen-box))))))
  boxer-eval::*novalue*)

(defvar *status-line-search-help*
  #-mcl
  "ESC=end, CTRL-.=abort, CTRL-F=resume, |=named box, ^=expands, MAKE-BOX-KEY= box type"
  #+mcl
  (format nil "ESC=end, ~C-.=abort, ~C-F=resume, |=named box, ^=expands, MAKE-BOX-KEY= box type"
          #\commandmark #\commandmark)
  )

(defboxer-command COM-SEARCH-HELP ()
  "Displays search mode key bindings in the status line"
  (status-line-display 'search *status-line-search-help*)
  boxer-eval::*novalue*)


(defmacro defsearch-mode-key (key-name function)
  `(defboxer-mode-key ,key-name (search-mode) ,function))
