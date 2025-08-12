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
;;;;         JSON Export
;;;;

(in-package :boxer)

(defclass json-file-class
  (foreign-file-class)
  ())

(defmethod begin-foreign-stream ((ffc json-file-class)
                                 &optional (stream *standard-output*))
  (format stream "["))

(defmethod write-foreign-file-header ((ffc json-file-class)
                                      box
                                      &optional (stream *standard-output*)))

(defun symbol-from-box-type (box)
  "Return a simple symbol of the box type. This should probably be
implemented as a defmethod on box subclasses."
  (cond ((sprite-box? box)
         :spritebox)
    ((graphics-box? box)
     :graphicsbox)
    ((doit-box? box)
     :doitbox)
    ((data-box? box)
     :databox)
    ((port-box? box)
     :portbox)
    (t :not-a-box)))

(defmethod write-foreign-file-graphics-list ((ffc json-file-class) graphics-list stream)
  (format stream "\"graphics-commands\": [~%")
  (let* ((commands (aref graphics-list 0))
         (at-least-one-written? nil))
    (loop for x across (remove nil commands)
      for y from 1
      when (> y 1)
      do (format stream ", ")
      do (if x (progn (format stream "[")
                      (loop for i across x
                        for j from 1
                        when (> j 1)
                        do (format stream ", ")
                        do (format stream "~A" i))
                      (format stream "]")))))
  (format stream "]~%"))

(defmethod write-foreign-file-graphics-sheet ((ffc json-file-class) box stream)
  (let ((sheet (slot-value box 'graphics-info)))
    ;; draw-wid, draw-hei, draw-mode
    (format stream "\"draw-wid\": ~A,~%" (graphics-sheet-draw-wid sheet))
    (format stream "\"draw-hei\": ~A,~%" (graphics-sheet-draw-hei sheet))
    (format stream "\"draw-mode\": \"~A\",~%" (graphics-sheet-draw-mode sheet))

    ;; graphics-lists
    (format stream "\"graphics-list\": {~%")
    (write-foreign-file-graphics-list ffc (slot-value sheet 'graphics-list) stream)
    (format stream "}~%")))

(defmethod write-foreign-file-boxflags ((ffc json-file-class) box stream &optional (show-all? nil))
  (let ((flags (slot-value box 'flags))
        (at-least-one-written? nil)) ;; Keeping track of commas
    (dotimes (i (length *defined-box-flags*))
      (let ((name (svref *defined-box-flags* i)))
        (cond ((null name))
          (t (let ((flag-set? (not (zerop (ldb (byte 1 i) flags)))))
               (cond ((and (null show-all?) (null flag-set?)))
                 (t (format t "~&~A: ~A" name
                            (if flag-set? "true" "false"))))

               (cond ((and (not show-all?) (null flag-set?)))
                 (t (if at-least-one-written?
                      (format stream ",~%")
                      (format stream "~%"))
                    (setf at-least-one-written? t)
                    (cond ((null flag-set?)
                           (format stream "\"~A\": null" name))
                      (flag-set?
                       (format stream "\"~A\": true" name))
                      (t
                       (format stream "\"~A\": false" name))))))))))))

(defun get-export-namerow-string (box)
  "Finds the name row of a box and returns the appropriate text string to render in the export, taking in to
  consideration if it's the initial-box, or nil."
  (let ((nr (name-row box)))
    (if nr
      (text-string nr)
      (if (equal box *initial-box*)
        "WORLD"
        ""))))

(defmethod write-foreign-file-box ((ffc json-file-class) box stream)
  (let* ((*ffc-depth* (1+ *ffc-depth*))
         (*export-properties* (merge-export-properties
                               *export-properties*
                               (get-export-format-properties box)))
         (nr (name-row box))
         (indent-factor (or (getf *export-properties* :indent-factor)
                            (getf (slot-value ffc 'prefs) :indent-factor)
                            0))
         (*current-indent* (make-string (* (max 0 (1- *ffc-depth*))
                                           indent-factor)
                                        :initial-element #\space)))
    (format stream "~A" *current-indent*)
    (if nr
      (format stream "{\"name\": \"~a\"," (text-string nr))
      (if (equal box *initial-box*)
        (format stream "{\"name\": \"WORLD\",")
        (format stream "{\"name\": \"\",")))
    (format stream "~A\"type\": \"~A\",~%" *current-indent* (symbol-from-box-type box))
    ;; exports
    (if (slot-value box 'exports)
      (format stream "\"exports\": \"~A\",~%" (slot-value box 'exports)))
    ;; box flags
    (format stream "\"box-flags\": {")
    (write-foreign-file-boxflags ffc box stream)
    (format stream "},~%") ; End box-flags
    (when (graphics-box? box)
      (format stream "\"graphics-sheet\": {")
      (write-foreign-file-graphics-sheet ffc box stream)
      (format stream "},~%"))
    (format stream "\"rows\": [ " *current-indent*)
    (do ((row (first-inferior-row box) (next-row row)))
      ((null row))
      (format stream "~A" *current-indent*)
      (write-foreign-file-row ffc row stream)
      ;; (format stream "")
      (if (next-row row)
        (format stream ",~%")))
    (format stream "~A ]}" *current-indent*)))

(defmethod write-foreign-file-row ((ffc json-file-class) row stream)
  (let ((cha-written? nil)
        (empty? t)
        (insert-comma? nil)
        (inside-chas? nil)) ;; Are we currently writing out a string? Keeping track of ""'s
    (format stream "[")
    (do-row-chas ((cha row))

      (cond ((box? cha)
             ;; End the chas string with a " if we're inside one.
             (if inside-chas? (format stream "\""))
             (if insert-comma? (format stream ", "))
             (setf insert-comma? nil)
             ;; Either way, if it's a box we're no longer in a string of chas
             (setf inside-chas? nil)

             (write-foreign-file-box ffc cha stream)

             (setq cha-written? nil empty? nil))
        (t
         ;; If we aren't in a string of chas yet, start with a semicolon
         (if (and insert-comma? (not inside-chas?)) (format stream ","))
         (setf insert-comma? nil)
         (if (not inside-chas?) (format stream "\""))
         ;; Either way, we are now inside a string of chas...
         (setf inside-chas? t)
         (format stream "~a" cha)
         (setq cha-written? t empty? nil)))
      (setf insert-comma? t))

    ;; If we're still in a string of chas, end it with the double quote
    (if inside-chas? (format stream "\""))
    (when empty? (format stream "\"\""))
    (format stream "]")))

(defmethod end-foreign-stream ((ffc json-file-class)
                               &optional (stream *standard-output*))
  (format stream "]"))

(def-export-type json-file-class "JSON" "*.json" :respect-line-breaks t)
