;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$


                   Copyright 2004 Pyxisystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    Deep Printing (as opposed to surface, screen shots)



Modification History (most recent at the top)

10/14/06 calls to name changed to name-string
 8/30/04 started file

|#



(in-package :boxer)

;;;; The theory:
;;;;     we want to do a tree walk of boxer structure issuing print commands
;;;;   for each element of boxer structure.
;;;;     The tree walking framework should remain constant but specific print
;;;;   commands can be issued for various printing types.  We start with a
;;;;   "text" type (ASCII, no fonts) but we should be able to add things
;;;;   like RTF, PDF etc at a later date
;;;;
;;;; Hardcopy sequence of events should be:
;;;;   0) selecting a box to be printed (not covered in this file)
;;;;   1) make an instance of a particular hardcopy class
;;;;   2) initialize that instance, for text files, this would be opening
;;;;      a filestream (perhaps prompting for a filename.  For actual
;;;;      printing, a print dialog for selecting a printer and/or printing
;;;;      options
;;;;   3) tree walk boxer structure
;;;;   4) allow each box to have a "print-hint" named box
;;;;      for special handling (like treating the box as a table)
;;;;      these should be extensible keywords (need some mechanism for targeting
;;;;      specific keywords to particular hardcopy classes....
;;;;      print hints can come in 2 flavors:
;;;;        a) IMMEDIATE, affecting only the box such as "no-print" or ???
;;;;        b)   , affecting all the inferiors of the box


(defvar *deep-hardcopy-version* 0.1)

;; define hardcopy-methods on these hardcopy classes
(defclass plain-text-hardcopy
  ()
  ((output-stream :initform nil :initarg :output-stream))
  (:documentation "Hardcopy Class for output to plain text files"))

(defclass html-hardcopy
  ()
  ((output-stream :initform nil :initarg :output-stream))
  (:documentation "Hardcopy class for html output"))

#+mcl
(defclass Mac-hardcopy
  ()
  ((output-stream :initform nil))
  (:documentation "Hardcopy class for mac printers"))



;; this should get moved somewhere else, check out boxer-new-file-dialog
;; which does much of the same work but has too much boxerish stuff in it
(defun new-file-dialog ()
  #+mcl
  (ccl::catch-cancel
    (ccl::choose-new-file-dialog :prompt "Convert box to text file:"))
  #+pc
  (capi:prompt-for-file "Convert box to text file:"
                        :operation :save :owner *boxer-frame*)
  )

(defmethod deep-hardcopy-preamble ((hc plain-text-hardcopy) &optional box)
  (declare (ignore box))
  )

(defmethod deep-hardcopy-epilog ((hc plain-text-hardcopy))
  )

(defmethod handle-print-hint ((hc plain-text-hardcopy) hint box)
  (with-slots (output-stream) hc
    (when (or (box? hint) (virtual-copy? hint))
      (string-case (box-text-string hint)
        ("noprint" (terpri output-stream))))))


(defmethod deep-hardcopy-box ((hc plain-text-hardcopy) box &optional (depth 0))
  (with-slots (output-stream) hc
    (let ((hint (eval::lookup-static-variable-in-box-only box 'bu::print-hint)))
      (cond ((graphics-box? box)  (format output-stream "Graphics Box~&"))
            ((not (null hint))   (handle-print-hint hc hint box))
            ;; special cases are done, now handle the simple case
            (t (dotimes (i (+ (* depth 2) 5))(format output-stream " "))
               (if  (null (name-row box))
                 (format output-stream "------------~&")
                 (format output-stream "---- ~A ----~&" (name-string box)))
               (do-box-rows ((r box)) (deep-hardcopy-row hc r depth)))))))


(defmethod deep-hardcopy-row ((hc plain-text-hardcopy) row &optional (depth 0))
  (with-slots (output-stream) hc
    (flet ((indent () (dotimes (i (* depth 2)) (format output-stream " "))))
      (indent)
      (let ((box? nil))
        (do-row-chas ((cha row))
          (cond ((box? cha)
                 (terpri output-stream)
                 (deep-hardcopy-box hc cha (1+ depth))
                 (setq box? t))
                ((not (null box?)) (indent) (format output-stream "~C" cha))
                (t (format output-stream "~C" cha))))
        (terpri output-stream)))))



;;;; HTML methods

;; these should be format strings
(defvar *html-deep-print-meta-info*
  (format nil "    <meta name=\"GENERATOR\" content=\"~A HTML Printer v~A\""
          (system-version 'boxer) *deep-hardcopy-version*))

(defmethod deep-hardcopy-preamble ((hc html-hardcopy) &optional box)
  (with-slots (output-stream) hc
    (format output-stream "<html>~&")
    (format output-stream "   <head>~&~A~&    <title>~A</title>~&   <head>"
            *html-deep-print-meta-info* (if (box? box)
                                          (name-string box)
                                          "Boxer HTML conversion"))
    (format output-stream "<body>~&")))

(defmethod deep-hardcopy-epilog ((hc html-hardcopy))
  (with-slots (output-stream) hc
    (format output-stream "~&</body>~&</html>")))

(defmethod handle-print-hint ((hc html-hardcopy) hint box)
  (with-slots (output-stream) hc
    (when (or (box? hint) (virtual-copy? hint))
      (string-case (box-text-string hint)
         ("noprint" (terpri output-stream))))))

(defun html-header-string (depth)
  (case depth
    (0 "<p><H1>~A</H1>")
    (1 "<p><H2>~A</H2>")
    (2 "<p><H3>~A</H3>")
    (3 "<p><H4>~A</H4>")
    (4 "<p><H5>~A</H5>")
    (otherwise "<p><H^>~A</H6>")))

(defmethod deep-hardcopy-box ((hc html-hardcopy) box &optional (depth 0))
  (with-slots (output-stream) hc
    (let ((hint (eval::lookup-static-variable-in-box-only box 'bu::print-hint)))
      (cond ((graphics-box? box)  (format output-stream "Graphics Box~&"))
            ((not (null hint))   (handle-print-hint hc hint box))
            ;; special cases are done, now handle the simple case
            (t (dotimes (i (+ (* depth 2) 5))(format output-stream " "))
               (if  (null (name-row box))
                 (format output-stream "<p>~&")
                 (format output-stream (html-header-string depth) (name-string box)))
               (do-box-rows ((r box)) (deep-hardcopy-row hc r depth)))))))


;; this should hack fonts
(defmethod deep-hardcopy-row ((hc html-hardcopy) row &optional (depth 0))
  (with-slots (output-stream) hc
    (flet ((indent () (dotimes (i (* depth 2)) (format output-stream " "))))
      (indent)
      (let ((box? nil))
        (do-row-chas ((cha row))
          (cond ((box? cha)
                 (terpri output-stream)
                 (deep-hardcopy-box hc cha (1+ depth))
                 (setq box? t))
                ((not (null box?)) (indent) (format output-stream "~C" cha))
                (t (format output-stream "~C" cha))))
        (terpri output-stream)))))



(defboxer-command com-deep-hardcopy-as-plain-text ()
  "Render the contents of the current box as plain text"
  (let ((pathname (new-file-dialog)))
    (with-open-file (s pathname :direction :output)
      (let ((hc (make-instance 'plain-text-hardcopy :output-stream s)))
        (deep-hardcopy-preamble hc)
        (deep-hardcopy-box hc (point-box))
        (deep-hardcopy-epilog hc))))
  eval::*novalue*)

(defboxer-command com-deep-hardcopy-as-html ()
  "Render the contents of the current box as plain text"
  (let ((pathname (new-file-dialog)))
    (with-open-file (s pathname :direction :output)
      (let ((hc (make-instance 'html-hardcopy :output-stream s)))
        (deep-hardcopy-preamble hc)
        (deep-hardcopy-box hc (point-box))
        (deep-hardcopy-epilog hc))))
  eval::*novalue*)
