;; -*- Mode:LISP; Syntax:Common-Lisp; Package:boxer ; -*-





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


Modification History (most recent at the top)

 4/21/03 merged current LW and MCL files
 7/15/00 started logging changes: *site-initialization-verbosity* changed
         to nil for non mcl systems

  This file contains code for site specific customizations

|#

(in-package :boxer)

(defvar *site-initialization-verbosity* nil)

;;; Strings MUST BE COPIED !!!!
(defun coerce-config-value (value-string type)
  (ecase type
    (:string (copy-seq value-string))
    (:symbol (intern (string-upcase value-string) (find-package "BOXER")))
    (:keyword (intern (string-upcase value-string) (find-package "KEYWORD")))
    (:boolean (not (null
                    (or (string-equal value-string "T")
		        (string-equal value-string "True")))))
    (:number (if (numberstring? value-string)
                 (ignoring-number-read-errors
                   (read-from-string value-string nil nil))
                 (progn (warn "Bad Number value for ~A, using 0" value-string)
                        0))
             #| ; only handles decimal positive integers
             (let ((acc 0) (l (length value-string)))
	       (dotimes (i l acc)
		 (let ((val (digit-char-p (aref value-string i))))
		   (cond ((null val)
			  (warn "Bad Number value for ~A, using ~A"
				value-string acc)
			  (return acc))
			 (t (setq acc (+ (* acc 10) val))))))) |#
             )))

;;; buffers + utilities
;;; if we wanted to, we could rewrite this using read/write-array
;;; but this is portable and speed is not a priority here

(defvar *keyword-buffer* (make-array 255
				     :element-type #-lucid 'character #+lucid 'string-char
				     :fill-pointer 0
				     :adjustable t))

(defvar *value-buffer* (make-array 255
				   :element-type #-lucid 'character #+lucid 'string-char
				   :fill-pointer 0
				   :adjustable t))

(defun buffer-clear (buffer)
  (setf (fill-pointer buffer) 0))

#|

 The format of the site configuration file is as follows:

   o Comments are preceeded by the # character
   o Each line should consist of a token name
     followed by a colon and then a value for that token
   o Whitespace is (should be) ignored
   o Case is not important in the token names but it may be
     important in the values
   o Number values are in decimal

|#

(defun config-file-comment? (char)
  (or (char-equal char #\#) (char-equal char #\;)))

(defvar *config-file-white-space-chars* '(#\space #\tab))

(defun config-file-whitespace? (char)
  (member char *config-file-white-space-chars* :test #'char-equal))

(defun config-file-eol? (char)
  (or (char-equal char #\return) (char-equal char #\newline)))

(defun config-file-separator? (char)
  (char-equal char #\:))

;; valid config lines can be whitespace, comments, or a keyword value pair
(defun read-config-line (filestream keyword-buffer value-buffer)
  (declare (values valid-pair? eof? keyword value))
  (let ((eof (list 'eof)) (stop-now? nil)
	(current-buffer keyword-buffer))
    (flet ((flush-remaining-line ()
	     (do ((char (read-char filestream nil eof)
			(read-char filestream nil eof)))
		 ((or (eq char eof) (config-file-eol? char))))))
      (do ((char (read-char filestream nil eof)
		 (read-char filestream nil eof)))
	  ((or (eq char eof) (config-file-eol? char) stop-now?)
	   (if (and (>& (length keyword-buffer) 0)
		    (>& (length value-buffer)   0))
	       (values t (eq char eof) keyword-buffer value-buffer)
	       (values nil (eq char eof))))
	(cond ((config-file-whitespace? char))
	      ((config-file-comment? char) (flush-remaining-line) (return nil))
	      ((config-file-separator? char)
	       (cond ((eq current-buffer value-buffer)
		      (warn "Extra \":\" seen in line with ~A ~A..."
			    keyword-buffer value-buffer)
		      (flush-remaining-line)
		      (setq stop-now? t))
		     ((>& (length current-buffer) 0)
		      (setq current-buffer value-buffer))
		     (t (warn "Empty keyword, ignoring line")
			(flush-remaining-line)
			(return nil))))
	      (t
	       ;; must be a valid character
	       (vector-push-extend char current-buffer)))))))
