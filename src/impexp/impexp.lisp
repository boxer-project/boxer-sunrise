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




;;;; HTML

(defclass html-file-class
          (foreign-file-class)
  ()
  )

(defmethod begin-foreign-stream ((ffc html-file-class)
                                 &optional (stream *standard-output*))
  (format stream "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" ~%~
                   \"http://www.w3.org/TR/html4/strict.dtd\">~%~
                  <html>~%~%"))


;; optionally <title></title>
;; maybe add boxer version number to description META tag
(defmethod write-foreign-file-header ((ffc html-file-class) box
                                      &optional (stream *standard-output*))
  (format stream
    "~%<head>~%~
       <META http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">~%~
       <META name=\"description\" content=\"Exported from the Boxer System\">~%")
  (let ((nr (name-row box)))
    (unless (null nr)
      (format stream "<TITLE>~A </TITLE>~%" (text-string nr))))
  (format stream "</head>~%"))

(defmethod write-foreign-file-box ((ffc html-file-class) box stream)
  (let* ((*ffc-depth* (min 6 (1+ *ffc-depth*))) ;; maximum header depth is 6
         (*export-properties* (merge-export-properties
                               *export-properties*
                               (get-export-format-properties box)))
         (nr (name-row box))
         (respect-line-breaks? (let ((local (getf *export-properties*
                                                  :respect-line-breaks
                                                  :not-specified)))
                                 (cond ((eq local :not-specified)
                                        (getf (slot-value ffc 'prefs)
                                              :respect-line-breaks))
                                       (t local)))))
    (cond ((null nr)
           ;; need some demarcation for a new box, <HR> or <p> ?
           )
          (t
           (format stream "~%<H~D>~A </H~D>~%~%"
                   *ffc-depth* (text-string nr) *ffc-depth*)))
    (Do ((row (first-inferior-row box) (next-row row)))
        ((null row))
      (write-foreign-file-row ffc row stream)
      (terpri stream)
      (cond ((not (null respect-line-breaks?)) (format stream "<BR>~%"))
            (t (format stream " "))))))

(defmethod write-foreign-file-graphics-box ((ffc html-file-class) box stream)
  (if (and (graphics-box? box)
           (display-style-graphics-mode? (display-style-list box)))
      (format stream "~A~%" *graphics-replacement-text*)
    (write-foreign-file-box ffc box stream)))

;; empty line should output a "<P>"
(defmethod write-foreign-file-row ((ffc html-file-class) row stream)
  (let ((cha-written? nil)
        (empty? t))
    (do-row-chas ((cha row))
      (cond ((box? cha)
             ;; if chas have been written, finish
             (when cha-written? (format stream "<BR>~%"))
             ;; CR's around boxes ?
             (if (and (graphics-box? cha)
                      (let ((ds (display-style-list cha)))
                        (when ds (display-style-graphics-mode? ds))))
                 (write-foreign-file-graphics-box ffc cha stream)
               (write-foreign-file-box ffc cha stream))
             (setq cha-written? nil empty? nil))
            (t (write-xml-char cha stream)
               (setq cha-written? t empty? nil))))
    (when empty? (format stream "~%<P>~%"))))

(defun write-xml-char (char stream)
  (let ((cc (char-code char)))
    (cond ((< cc 128) (write-char char stream))
          (t (format stream " &#~D " cc)))))

(defmethod begin-foreign-body ((ffc html-file-class)
                               &optional (stream *standard-output*))
  (format stream "~%<body>~%"))

(defmethod end-foreign-body ((ffc html-file-class)
                                 &optional (stream *standard-output*))
  (format stream "~%</body>~%"))


(defmethod end-foreign-stream ((ffc html-file-class)
                                 &optional (stream *standard-output*))
  (format stream "~%</html>~%"))



;;;; OPML stuff, move this when it starts to get more complicated

(defvar *xml-indent-factor* 3)

(defclass opml-file-class
          (foreign-file-class)
  ()
  )

(defmethod begin-foreign-stream ((ffc opml-file-class)
                                &optional (stream *standard-output*))
  (format stream "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>~%~
                  <opml version=\"2.0\">~%"))

(defmethod write-foreign-file-header ((ffc opml-file-class) box
                                      &optional (stream *standard-output*))
  (format stream "   <head>~%")
  (let ((nr (name-row box)))
    (unless (null nr) (format stream "    <title>~A</title>~%" (text-string nr))))
  ;; eventually look for created from (possibly) reading in file
  (let ((current-date-string (boxnet::rfc822-date-time)))
    (format stream "    <dateCreated>~A</dateCreated>~%" current-date-string)
    (format stream "    <dateModified>~A</dateModified>~%" current-date-string))
  ;; expansion control
  ;<expansionState>1, 2, 3 </expansionState>
  ;<vertScrollState>1</vertScrollState>
  ;<ownerName>Dave Winer</ownerName>
  (format stream "    <ownerEmail>~A</ownerEmail>~%" boxnet::*user-mail-address*)
  (format stream "   </head>~%"))

(defmethod begin-foreign-body ((ffc opml-file-class)
                                 &optional (stream *standard-output*))
  ;; file format info like <html> for html files
  (format stream "  <body>~%"))

(defmethod write-foreign-file-box ((ffc opml-file-class) box stream)
  (let ((*ffc-depth* (1+ *ffc-depth*)) ;; support for indenters
        (nr (name-row box))
        (1st-row (first-inferior-row box))
        (*current-indent* (make-string (* *ffc-depth* *xml-indent-factor*)
                                       :initial-element #\space)))
    ;; box's 1st text attribute
    (format stream "~A<outline text=\"~A\">~%"
            *current-indent*
            (cond ((not (null nr)) (text-string nr))
                  ;; no name, so use 1st row of box
                  ((null 1st-row)
                   ;; imbedded file boxes can have a null first-inferior-row
                   "[Empty]")
                  ((text-only-row? 1st-row)
                   (text-string 1st-row))
                  (t ;; row with boxes....
                     ;; note: box can be 1st cha, just do something reasonable
                     ;; have the box be blank named, then set nr to be non null
                     ;; so the row will get properly exported in the body...
                     (setq nr " "))))
    (do ((r (if (null nr) (and 1st-row (next-row 1st-row)) 1st-row)
            (next-row r)))
        ((null r))
      (write-foreign-file-row ffc r stream))
    (format stream "~A</outline>~%" *current-indent*)))

(defmethod write-foreign-file-graphics-box ((ffc opml-file-class) box stream)
  (if (null *export-graphics-as-text?*)
      (format stream "~A<outline text=\"~A\">~%"
              *current-indent* *graphics-replacement-text*)
    (write-foreign-file-box ffc box stream)))

(defmethod write-foreign-file-row ((ffc opml-file-class) row stream)
  (let ((cha-written? nil))
    (do-row-chas ((cha row))
      (cond ((box? cha)
             ;; if chas have been written, finish
             (when cha-written? (format stream "\"/>~%"))
             ;; CR's around boxes ?
             (if (and (graphics-box? cha)
                      (let ((ds (display-style-list cha)))
                        (when ds (display-style-graphics-mode? ds))))
                 (write-foreign-file-graphics-box ffc cha stream)
               (write-foreign-file-box ffc cha stream))
             (setq cha-written? nil))
            (t (unless cha-written? ; starting new outline element...
                 (format stream "~A<outline text=\"" *current-indent*))
               (write-xml-char cha stream)
               (setq cha-written? t))))
    (when cha-written? (format stream "\"/>~%"))))


(defmethod end-foreign-body ((ffc opml-file-class)
                                 &optional (stream *standard-output*))
  ;; file format info like <html> for html files
  (format stream "   </body>~%"))

(defmethod end-foreign-stream ((ffc opml-file-class)
                                &optional (stream *standard-output*))
  (format stream "~%</opml>~%"))



;;;

(def-export-type basic-text-file-class "Tabbed Text" "*.txt" :indent-factor 3)

(def-export-type opml-file-class "OPML" "*.opml")

(def-export-type html-file-class "HTML" "*.htm;*.html" :respect-line-breaks nil)

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

#+mcl
(progn

(setq *export-file-menu*
  (make-instance 'ccl::menu :menu-item-title "Export"
                 :menu-items
                 (mapcar #'(lambda (ffc)
                             (make-instance 'ccl::menu-item
                               :menu-item-title (ffc-pretty-name ffc)
                               :menu-item-action #'(lambda ()
                                                     (bw::queue-event
                                                      (list 'com-export-box-as
                                                            ffc)))))
                         *foreign-file-class-exports*)))

(let* ((fmis (ccl::menu-items *boxer-file-menu*))
       (bottom-items (nthcdr 10 fmis)))
  (dolist (bottom-item bottom-items)
    (ccl:remove-menu-items *boxer-file-menu* bottom-item))
  (dolist (new (list* *export-file-menu* bottom-items))
    (ccl:add-menu-items *boxer-file-menu* new)))


)

