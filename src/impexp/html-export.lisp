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
;;;;         HTML Export
;;;;

(in-package :boxer)

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

(def-export-type html-file-class "HTML" "*.htm;*.html" :respect-line-breaks nil)
