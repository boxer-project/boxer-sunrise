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
;;;;         OPML Export
;;;;

(in-package :boxer)

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
  (let ((current-date-string (rfc822-date-time)))
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

(def-export-type opml-file-class "OPML" "*.opml")
