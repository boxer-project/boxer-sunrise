#|

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+




Modification History (most recent at the top)

11/22/11  started file

file-type
applescript return values

launch-application
manage xref <=> player documents, (tell application "Quicktime Player" exists?)
player running ?
close-movie


|#

(in-package :boxer)

;;;; Basic Applescript stuff

(defvar *applescript-stream* nil
  "Stream interface to a shell for sending applescript commands and receiving the replies")

(defvar *applescript-running?* nil)

(defvar *apple-script-response-max-wait-time* 5
  "Maximum amount of time (in seconds) to wait for an answer from a script")

(defvar *apple-script-response-poll-period* 0.1)

(defvar *applescript-log* nil)

(defvar *applescript-log-length* 20)

(defun log-applescript-text (string &optional (query? t))
  (when (>= (length *applescript-log*) *applescript-log-length*)
    (setq *applescript-log* (cdr *applescript-log*)))
  (setq *applescript-log* (nconc *applescript-log*
                                 (list (format nil "~A ~A" (if query? "<== " "==> ") string)))))



(defun startup-applescript ()
  #+lispworks (setq *applescript-stream*  (sys:open-pipe nil :direction :io)
        *applescript-running?* T))

(defun stop-applescript ()
  (close *applescript-stream*)
  (setq *applescript-running?* nil
        *applescript-stream* nil))

;;; Stub, should return a suitable applescript application name
;;; which can open the file
(defun get-applescript-app (file) (declare (ignore file)) "Quicktime Player")

(defun applescript-close-xref (xref)
  (unless (null (xref-active-info xref))
    (applescript-command :string (format nil "tell ~A" (xref-active-info xref)) "close" "end tell")
    (setf (xref-active-info xref) nil)))

(defun as-quote (string)
  (format nil "\\\"~A\\\"" string))

(defun applescript-open-xref (xref)
  (when (null (xref-active-info xref))
    ;; xref is not already opened in the app
    (unless *applescript-running?* (startup-applescript))
    (let* ((applescript-app (get-applescript-app (xref-pathname xref)))
           (quoted-app-name (as-quote applescript-app))
           (ai (applescript-command :string
                                    (format nil "tell app \"Finder\" to open POSIX file \"~A\""
                                      (namestring (xref-pathname xref)))
                                    ;; TODO In-Progress Boxer-bugs-52 Add various filename tests.
                                    ;; (format nil "tell application ~A" quoted-app-name)
                                    ;; (format nil "open ~A" (as-quote (namestring (xref-pathname xref))))
                                    )
                                    ))
      (cond ((search "document " ai)
             (setf (xref-active-info xref)
                   (format nil "~A of application ~A"
                           (as-quote (subseq ai #.(length "document "))) quoted-app-name)))
            (t ;; signal an error?
               nil)))))


;; hook for when the pathname of an xref gets changed, minimally,
;; should CLOSE the original pathname if active
(defun applescript-new-path-for-xref (xref newpath)
  (unless (null (xref-active-info xref))
    ;; xref is opened in the app
    )
  (setf (xref-pathname xref) newpath)
  )

;; hook for when the file gets moved to a different place in the file
;; hierarchy (maybe close then open again ?)
(defun applescript-moved-path-for-xref (xref newpath)
  (unless (null (xref-active-info xref))
    ;; xref is opened in the app
    )
  (setf (xref-pathname xref) newpath)
  )

;; each command = line in script
;; rtype is a keyword specifying the type of the returned value, NIL means no value is expected
;; returns 2 values, the rtype coaerced value and the unprocessed answer
;; specialty 1st values will be :novalue and :error?
(defun applescript-command (rtype &rest commands)
  (unless *applescript-running?* (startup-applescript))
  ;; flush any old output before issuing any commands
  (loop (if (listen *applescript-stream*) (read-line *applescript-stream* nil nil) (return)))
  (let ((command-string "/usr/bin/osascript "))
    (dolist (command commands)
      (setq command-string (concatenate 'string command-string " -e " "'" command "'")))
    (log-applescript-text command-string)
    (write-line command-string *applescript-stream*)
    (force-output *applescript-stream*))
  (cond ((null rtype) boxer-eval::*novalue*)
        (t
         #+lispworks (mp:process-wait-local-with-timeout-and-periodic-checks "Applescript Response"
                                                                 *apple-script-response-max-wait-time*
                                                                 *apple-script-response-poll-period*
                                                                 #'(lambda () (listen *applescript-stream*)))
         (let ((answerstring (when (listen *applescript-stream*)
                               (read-line *applescript-stream* nil nil))))
           (log-applescript-text answerstring nil)
           (cond ((search "error" answerstring)
                  ;; check for error in the answer string
                  (values answerstring :error?))
                 (t
                  (ecase rtype
                    (:integer (read-from-string answerstring nil nil))
                    (:float   (read-from-string answerstring nil nil))
                    (:string  answerstring)
                    (:boolean (if (string-equal answerstring "true") t nil))
                    (:list (make-list-from-applescript answerstring)))))))))

;; Applescript lists are delimited by "{}"s and items are comma separated
;; we'll return the items as lists of strings and let the caller do any further parsing
(defun make-list-from-applescript (string)
  (let ((items nil)
        (last-comma 0))
    (do ((commapos (position #\, string :start last-comma)
                   (position #\, string :start last-comma)))
        ((null commapos)
         (push (subseq string last-comma) items)
         (nreverse items))
      (push (subseq string last-comma commapos) items)
      (setq last-comma (+ commapos 2)))))  ; +2 because output items are separated by COMMA,SPACE

;; other valid entries would be :all or nil
(defvar *as-activate-window-action* :default)

;; commands will be string
;; which strings warrant bringing the window to the front...
(defun applescript-active-command? (command)
  (case *as-activate-window-action*
    (:default  (member command '("play" "start") :test #'string-equal))
    (:all  T)
    (t nil)))

;; now we
;; activate option
(defun quicktime-command (rtype command &rest command-args)
  (let ((xref (get-xref)))
    (cond ((null xref))
          (t
           (applescript-open-xref xref)
           (when (applescript-active-command? command)
             (applescript-command nil
                                  (format nil "tell window named ~A" (xref-active-info xref))
                                  "activate"
                                  "end tell"))
           ;; make sure the doc is opened in the app, need to check for error here
           (multiple-value-bind (raw-value error?)
               (applescript-command rtype
                                    (format nil "tell document ~A" (xref-active-info xref))
                                    (format nil "~A~{ ~A~}" command command-args)
                                    "end tell")
             (declare (ignore error?)) ; for now....
             (case rtype
               ((nil) raw-value)
               ((:integer :float) raw-value)
               (:boolean (boxer-eval::boxer-boolean raw-value))
               (:list (make-vc (list raw-value)))
               (t (make-vc (list (list raw-value))))))))))

(defun applescript-activate ()
  (let ((xref (get-xref)))
    (cond ((null xref))
          (t
           (applescript-open-xref xref)
           (applescript-command nil
                                (format nil "tell window named ~A" (xref-active-info xref))
                                "activate"
                                "end tell")))))

(defun quicktime-movie-property (rtype property)
  (quicktime-command rtype (format nil "get ~A" property)))

;; should type check & coerce? new-value to rtype
;; new-value is passed from the evaluator and can be a box or even a virtual copy of a box
(defun set-quicktime-movie-property (rtype property new-value)
  (let ((new-avalue (ecase rtype
                      (:string (box-text-string new-value))
                      (:list (let ((items (flat-box-items new-value)))
                               (cond ((null items) "{}")
                                     ((null (cdr items)) (format nil "{~A}" (car items)))
                                     (t (format nil "{~A~{, ~A~}}" (car items) (cdr items))))))
                      (:float (numberize-or-error new-value))
                      (:integer (round (numberize-or-error new-value)))
                      (:boolean (let ((items (flat-box-items new-value)))
                                  (cond ((equal items '(bu::true)) "true")
                                        ((equal items '(bu::false)) "false")
                                        (t (boxer-eval::primitive-signal-error :applescript
                                                                         new-value
                                                                         " should be TRUE or FALSE"))))))))
    (quicktime-command nil (format nil "set ~A to ~A" property new-avalue))))



;;; The Top Level Interface
;; not quite right but close...
(boxer-eval::defboxer-primitive bu::qt-active? () (boxer-eval::boxer-boolean *applescript-running?*))

(boxer-eval::defboxer-primitive bu::qt-movie-open? ()
    (let ((xref (get-xref)))
      (boxer-eval::boxer-boolean
       (cond ((null xref) nil)
             (t (not (null (xref-active-info xref))))))))

(boxer-eval::defboxer-primitive bu::qt-open-movie ()
  (let ((xref (get-xref)))
    (unless (null xref) (applescript-open-xref xref))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::qt-close-movie ()
  (let ((xref (get-xref)))
    (unless (null xref) (applescript-close-xref xref))
    boxer-eval::*novalue*))

;; (boxer-eval::defboxer-primitive bu::qt-save ()

(boxer-eval::defboxer-primitive bu::qt-activate () (applescript-activate) boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::qt-play () (quicktime-command nil "play") boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::qt-start () (quicktime-command nil "start") boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::qt-pause () (quicktime-command nil "pause") boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::qt-resume () (quicktime-command nil "resume") boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::qt-stop () (quicktime-command nil "stop") boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::qt-step-backward ((boxer-eval::numberize steps))
  (let ((n (round steps)))
    (quicktime-command nil "step backward by" n)
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::qt-step-forward ((boxer-eval::numberize steps))
  (let ((n (round steps)))
    (quicktime-command nil "step forward by" n)
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::qt-trim ((boxer-eval::numberize from) (boxer-eval::numberize to))
  (let ((tfrom (round from)) (tto (round to)))
    (quicktime-command nil "trim" "from" tfrom "to" tto)
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::qt-present () (quicktime-command nil "present") boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::show-applescript-log ()
  (make-vc (mapcar #'list *applescript-log*)))



;;;; Properties
(defvar *quicktime-properties* nil)

(defmacro define-qt-property (boxer-name applescript-name data-type settable?)
  `(progn
     (unless (member ',boxer-name *quicktime-properties*) (push ',boxer-name *quicktime-properties*))
     (setf (get ',boxer-name 'applescript-name) ,applescript-name
           (get ',boxer-name 'adata-type) ',data-type
           (get ',boxer-name 'asettable?) ',settable?)
     ',boxer-name))

(defun get-applescript-name (prop) (get prop 'applescript-name))
(defun get-adata-type (prop) (get prop 'adata-type))
(defun get-applescript-settable? (prop) (get prop 'asettable?))

(defun qt-get-internal (pbox)
  (let* ((property (intern-in-bu-package (box-text-string pbox)))
         (aname (get-applescript-name property))
         (atype (get-adata-type property)))
    (quicktime-movie-property atype aname)))

(defun qt-set-internal (pbox new-value)
  (let* ((property (intern-in-bu-package (box-text-string pbox)))
         (aname (get-applescript-name property))
         (atype (get-adata-type property))
         (settable? (get-applescript-settable? property)))
    (cond (settable?
           (set-quicktime-movie-property atype aname new-value))
          (t (boxer-eval::primitive-signal-error :applescript "Cannot SET the property, " property)))))

(boxer-eval::defboxer-primitive bu::qt-get ((bu::datafy property))
  (qt-get-internal property))

(boxer-eval::defboxer-primitive bu::qt-set ((bu::datafy property) new-value)
  (qt-set-internal property new-value))

;;; properties from the Standard Suite
(define-qt-property bu::name "name" :string nil)
(define-qt-property bu::modified? "modified" :boolean nil)
(define-qt-property bu::file "file" :string nil)

;;; documented properties from Quicktime Dictionary
(define-qt-property bu::audio-volume "audio volume" :float t)
(define-qt-property bu::current-time "current time" :float t)
(define-qt-property bu::data-rate "data rate" :integer nil)
(define-qt-property bu::data-size "data size" :integer nil)
(define-qt-property bu::duration "duration" :float nil)
(define-qt-property bu::loop? "looping" :boolean t)
(define-qt-property bu::muted? "muted" :boolean t)
(define-qt-property bu::natural-dimensions "natural dimensions" :list nil)
(define-qt-property bu::playing? "playing" :boolean nil)
(define-qt-property bu::rate "rate" :float t)
(define-qt-property bu::presenting "presenting" :boolean t)
;; fairly useless for now...
;(define-qt-property bu::current microphone "current microphone"
;(define-qt-property bu::current camera "current camera"
;(define-qt-property bu::current movie compression "current movie compression"
;(define-qt-property bu::current screen compression "current screen compression"




