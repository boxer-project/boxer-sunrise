#|

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+

   File Export and Import mechanism hooks for Boxer
   This is for reading and writing other file formats in & out of Boxer
   Particular export types will have their own files

To Do: handle char style conversion in HTML docs
       box level export-info for special handling
       "import..." @ least for text format
       exporting graphics boxes..
       OPML can use shrunk box info to setup expansion state
       for imports hack file format so SAVE box automagically uses exporting format

Modification History (most recent at top)

11/12/13 write-xml-char to handle cases where charcode > 128, used in html, opml ffc's
 2/ 1/12 com-export-box-as moved to coms-fs.lisp so system can recompile cleanly
 3/26/10 added file-filters to basic class to support file-filter options in
         com-export-box-as - fixes file menu prompting bug
 3/21/10 added/finished 1st draft of tabbed text and html exporters
12/20/06 Started file

|#

(in-package :boxer)

;; these are also used by the menu system for the "export..." and
;; "import..." file menu items

(defvar *foreign-file-class-exports* nil
  "A list of foreign-file-classes formats which boxer can export to")

(defvar *foreign-file-class-imports* nil
  "A list of foreign-file-classes formats which boxer can import from")

(defvar *default-ffc* nil)

(defvar *ffc-depth* 0)

(defvar *current-indent* "")

(defvar *export-graphics-as-text?* nil)

(defvar *graphics-replacement-text* "[GRAPHICS]")

(defvar *foreign-file-filters* '("All Files" "*.*"))

(defmacro def-export-type (type pretty-name file-filter &rest prefs)
  `(let* ((new (make-instance ',type
                              :pretty-name ,pretty-name
                              :file-filter ,file-filter
                              :prefs ',prefs))
          (old (car (member new *foreign-file-class-exports*
                            :test #'(lambda (a b) (eq (class-of a) (class-of b)))))))
     (if (null old)
         (push new *foreign-file-class-exports*)
       (nsubstitute new old *foreign-file-class-exports*))
     (cond ((member ,pretty-name *foreign-file-filters* :test #'string-equal)
            ;; need to check if ,file-filter has changed...
            )
           (t (push ,file-filter *foreign-file-filters*)
              (push ,pretty-name *foreign-file-filters*)))))


;;; Foreign File Types

;;; each type gets added to *export-types* & *import-types*
;;; each type (can) handle(s) special handling lookup,
;;; type prefs-persistence, reading/writing

(defclass foreign-file-class
          ()
  ((pretty-name :initform "" :initarg :pretty-name :accessor ffc-pretty-name)
   (file-filter :initform "*.*" :initarg :file-filter :accessor ffc-file-filter)
   (prefs :initform nil :initarg :prefs :accessor ffc-prefs)) ;; a Plist
  )

(defclass basic-text-file-class
          (foreign-file-class)
  ()
  )

;;; prefs define 3 possible text formats
;; flat, tabbed, imbedded
;; comment header field to describe content

;;; Text prefs
; tabs = fixnum
; interpolate subbox (separate type)
;(defmacro def-foreign-file-pref (ff-class pref-name default-value)

;(defmethod initialize-instance ((self foreign-file-class) &rest init-plist)
;  (shared-initialize self t)


;; OPML (see opml.lisp)

;;; future
;; LateX-file-type
;; #+lispworks JPG-file-type
;; RTF (minimally, capture fonts)
;; HTML (minimally, capture fonts, titles)

;;;; Basic methods....
;; prefs handling
;; prefs are Plists in the basic foreign-file-class, more specific methods
;; can append or override the basic method

;; foreign files are written in the following protocol using a foreign-file-class

#|
writing-foreign-file (stream-var ffc filename)
  begin-foreign-stream ffc stream
  write-foreign-file-header  ffc box stream
   begin-foreign-body
    write-foreign-file-box (hook for special handling)
     write-foreign-file-row
     ...
   end-foreign-body
  end-foreign-stream   ffc stream
|#


(defmacro writing-foreign-file ((stream-var ffc filename) &body body)
  `(with-open-file (,stream-var ,filename
                                :direction ':output
                                :if-does-not-exist :create
                                :if-exists :supersede
                                :element-type (foreign-file-element-type ,ffc))
     . ,body))

#|
;; unicode version
(defmacro writing-foreign-file ((stream-var ffc filename) &body body)
  `(with-open-file (,stream-var ,filename
                                :direction ':output
                                :external-format :utf-8
                                :element-type :default)
     . ,body))
|#

;;; utilities

(defun rfc822-date-time (&optional (time (get-universal-time)))
  (multiple-value-bind (sec min hou day mon yea dow dst time-zone)
      (decode-universal-time time)
    (format nil "~a, ~a ~a ~a ~2,'0d:~2,'0d:~2,'0d ~a"
            (ut-day dow nil) day (ut-month mon nil) yea ;(mod yea 100)
            hou min sec (ut-tz  time-zone dst))))

(defmethod text-only-row? ((row row))
  (not (do-row-chas ((cha row)) (when (box? cha) (return t)))))

;; plist is generated by looking for an "export-propertiess" box
;; named boxes in "export-properties" become keyword/value pairs

(defvar *export-properties* nil)

(defun get-export-format-properties (box)
  (let* ((props-box (boxer-eval::static-variable-value
                     (boxer-eval::lookup-static-variable box 'bu::export-properties)))
         (bindings (when (box? props-box)
                     (slot-value props-box 'boxer-eval::static-variables-alist))))
    (cond ((null bindings) nil)
          (t (with-collection
               (dolist (binding bindings)
                 (collect (intern-keyword (Car binding)))
                 (collect (export-format-value (caddr binding)))))))))

;; for now, only look for true-false or numbers
(defun export-format-value (box)
  (or (numberize-or-nil box) (boxer-eval::true? box)))

;; we can't rely on scoping to get the correct properties because a lower
;; "export-properties" without a certain property will obscure a higher
;; binding with that property
(defun merge-export-properties (old-props new-props)
  (cond ((null new-props) old-props)
        ((eq old-props new-props) old-props)
        (t (do* ((props new-props (cddr props))
                 (indicator (car props) (car props))
                 (value     (cadr props)(cadr props)))
                ((null props) old-props)
             (setf (getf old-props indicator) value)))))

;;; Default methods

(defmethod foreign-file-element-type ((ffc foreign-file-class)) 'character)

(defmethod begin-foreign-stream ((ffc foreign-file-class)
                                 &optional (stream *standard-output*))
  ;; file format info like <html> for html files
  (declare (ignore stream))
  )

(defmethod write-foreign-file-header ((ffc foreign-file-class) box
                                      &optional (stream *standard-output*))
  ;; file level header info like date, author etc belong here....
  (declare (ignore box stream))
  )

(defmethod begin-foreign-body ((ffc foreign-file-class)
                                 &optional (stream *standard-output*))
  ;; file format info like <body> for html files
  (declare (ignore stream))
  )

(defmethod end-foreign-body ((ffc foreign-file-class)
                                 &optional (stream *standard-output*))
  ;; file format info like </body> for html files
  (declare (ignore stream))
  )

(defmethod end-foreign-stream ((ffc foreign-file-class)
                               &optional (stream *standard-output*))
  ;; file format info like </html> for html files
  (declare (ignore stream))
  )


#|
;; generic lookup for box specific handling
(defmethod foreign-file-bindings ((ffc foreign-file-class) box)
  (let ((boxer-eval::lookup-static-variable-in-box-only box 'bu::foreign-file-info))
    ;; returns a list of variable value pairs which are bound in with-foreign-file
    ;; typically will include constants as well as info from the class's prefs

|#



;;; Box & Row writing methods must be specific to formats so no defaults for those...

;; Basic Text methods

;; plain text does not need file specific file begin, end or header
(defmethod write-foreign-file-box ((ffc basic-text-file-class) box stream)
  ;; 1st check for any  special handling....
  ;; handle box's name...
  (let* ((*ffc-depth* (1+ *ffc-depth*)) ;; support for indenters
         (*export-properties* (merge-export-properties
                               *export-properties*
                               (get-export-format-properties box)))
         (nr (name-row box))
         (indent-factor (or (getf *export-properties* :indent-factor)
                            (getf (slot-value ffc 'prefs) :indent-factor)
                            0))
         ;; we check for indent info @ the box level because we may want a
         ;; box level way to change it (inside export-info)
         (*current-indent* (make-string (* (max 0 (1- *ffc-depth*))
                                           indent-factor)
                                        :initial-element #\space)))
    (terpri stream)
    (unless (null nr)
      (format stream "~A** ~A **~%" *current-indent* (text-string nr)))
    (do-box-rows ((row box))
      (write-foreign-file-row ffc row stream))))


(defmethod write-foreign-file-graphics-box ((ffc basic-text-file-class) box stream)
  (if (null *export-graphics-as-text?*)
      (format stream "~A~A~%" *current-indent* *graphics-replacement-text*)
    (write-foreign-file-box ffc box stream)))

;; can exit early if a box is encountered, 2nd value is T if end is reached
(defmethod write-foreign-file-row ((ffc basic-text-file-class) row stream)
  (format stream "~A" *current-indent*)
  (let ((box-handled? nil))
    (do-row-chas ((cha row))
      (cond ((box? cha)
             ;; CR's around boxes ?
             (if (and (graphics-box? cha)
                      (let ((ds (display-style-list cha)))
                        (when ds (display-style-graphics-mode? ds))))
                 (write-foreign-file-graphics-box ffc cha stream)
               (write-foreign-file-box ffc cha stream))
             (setq box-handled? t))
            (t (when box-handled? (format stream "~A" *current-indent*))
               (write-char cha stream)
               (setq box-handled? nil))))
    (unless box-handled? (terpri stream))))

(def-export-type basic-text-file-class "Tabbed Text" "*.txt" :indent-factor 3)

;; now that tabbed text is defined...
(eval-when (load)
  (setq *default-ffc* (find 'basic-text-file-class *foreign-file-class-exports*
                            :test #'(lambda (a b) (eq a (type-of b)))))
  )

;;; menu stuff....

(defun export-box-internal (box ffc stream)
  (begin-foreign-stream ffc stream)
  (write-foreign-file-header ffc box stream)
  (begin-foreign-body ffc stream)
  (write-foreign-file-box ffc box stream)
  (end-foreign-body ffc stream)
  (end-foreign-stream ffc stream))

(defun file-export-items-function (interface)
  (declare (ignore interface))
  (mapcar #'ffc-pretty-name *foreign-file-class-exports*))

(defun file-export-menu-action (data int)
  (declare (ignore int))
  (let ((ffc (find-if  #'(lambda (ffc) (string-equal data (ffc-pretty-name ffc)))
                       *foreign-file-class-exports*)))
    (unless (null ffc)
      (com-export-box-as ffc))))
