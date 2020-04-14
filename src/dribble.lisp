;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER; Base:8.-*-
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



   This file contains utilities for recording and playback of
   key press and mouse click input


Modification History (most recent at top)

 4/21/03 merged current LW and MCL files, no diffs, updated copyright

|#

(in-package :boxer)


;;; Dribble File support

(eval-when (compile load eval)
(defvar *dribble-handler-names* (make-array 10 :initial-element nil))
)
(defvar *dribble-write-handlers* (make-array 10 :initial-element nil))
(defvar *dribble-read-handlers* (make-array 10 :initial-element nil))
(defvar *dribble-print-handlers* (make-array 10 :initial-element nil))

(defvar *dribble-file-stream* nil)
;; this is used as the :if-exists option to OPEN in dribble-on
(defvar *dribble-file-exists-action* :supersede) ; also could be :append

(defvar *current-mouse-state* (make-array 3 :initial-element 0))


(defvar *dribble-playback* nil
  "Bound to T inside playback of dribble files so mouse state functions
   will know when to do the right thing")

(defvar *dribble-playback-stream* nil
  "Bound to the dribble stream during playback")

(defvar *dribble-loop-pause-time* 0
  "Amount of time to pause between handling each input event during playback")

(defvar *dribble-explicit-pause-time* .1
  "Amount of time to pause for each dribble wait event")

(defvar *mouse-track-pause-during-playback* t)


(defconstant *dribble-handler-prefix* 0)

;; READER-FUNCTION is used inside of PLAYBACK-DRIBBLE-FILE
;; WRITER-FUNCTION is used to dribble non character input out to the file
;; and PRINT-FUNCTION is used by SHOW-DRIBBLE-FILE

(defmacro def-dribble-handlers ((name opcode) &key
                                read-args read-form
                                write-args write-form
                                print-args print-form)
  (let ((reader-function (gensym))
        (writer-function (gensym))
        (print-function  (gensym)))
     `(progn
        (setf (aref *dribble-handler-names* ,opcode) ',name)
        (defun ,reader-function ,read-args  ,read-form)
        (defun ,writer-function ,write-args
          (write-byte *dribble-handler-prefix* ,(car write-args))
          (write-byte ,opcode ,(car write-args))
          ,write-form)
        (defun ,print-function ,print-args ,print-form)
        (setf (aref *dribble-read-handlers*  ,opcode) ',reader-function)
        (setf (aref *dribble-print-handlers*  ,opcode) ',print-function)
        (setf (aref *dribble-write-handlers* ,opcode) ',writer-function))))

(defmacro dribble-write (name stream &rest values)
  (let ((opcode (position name *dribble-handler-names*)))
    (if (null opcode)
        (error "No handler defined for ~A" name)
        `(funcall (aref *dribble-write-handlers* ,opcode) ,stream . ,values))))

(defmacro dribble-read (opcode stream)
  (let ((var (gensym)))
    `(let* ((,var ,opcode)
            (fun (aref *dribble-read-handlers* ,var)))
       (if (null fun)
         (error "No handler defined for ~A" ,var)
         (funcall fun ,stream)))))

(defmacro dribble-print (opcode stream)
  (let ((var (gensym)))
    `(let* ((,var ,opcode)
            (fun (aref *dribble-print-handlers* ,var)))
       (if (null fun)
         (error "No handler defined for ~A" ,var)
         (funcall fun ,stream)))))

(def-dribble-handlers (shift 0)
  :read-args (stream)
  :read-form (let ((bits (read-byte stream)))
               (declare (special dribble-playback-bits))
               (setq dribble-playback-bits bits))
  :print-args (stream)
  :print-form (let* ((bits (read-byte stream))
                     (sl (input-device-shift-list *current-input-device-platform*))
                     (pstring (if (>& bits (length sl))
                                (format nil "Shift(~D)" bits)
                                (nth (1- bits) sl))))
                (format t "~A-" pstring)
                (length pstring))
  :write-args (stream shift-bits)
  :write-form (progn (write-byte shift-bits stream)))

(def-dribble-handlers (mouse 1)
  :read-args (stream)
  :read-form (let ((me (bw::make-mouse-event)))
               (setf (mouse-event-type me) (if (zerop& (read-byte stream))
                                               ':mouse-click ':mouse-hold)
                     (mouse-event-bits me) (read-byte stream)
                     (mouse-event-click me) (read-byte stream)
                     (bw::mouse-event-number-of-clicks me) (read-byte stream)
                     (mouse-event-x-pos me) (dpb& (read-byte stream)
                                                  %%bin-op-top-half
                                                  (read-byte stream))
                     (mouse-event-y-pos me) (dpb& (read-byte stream)
                                                  %%bin-op-top-half
                                                  (read-byte stream)))
               (handle-boxer-input me)
               (redisplay))
  :print-args (stream)
  :print-form (let ((type (if (zerop& (read-byte stream)) ':mouse-click ':mouse-hold))
                    (name (let ((bits (read-byte stream))
                                (click (read-byte stream)))
                            (lookup-click-name click bits)))
                    (num-click (read-byte stream))
                    (x (dpb& (read-byte stream)
                             %%bin-op-top-half (read-byte stream)))
                    (y (dpb& (read-byte stream)
                             %%bin-op-top-half (read-byte stream))))
                (declare (ignore num-click))
                (format t "#<~A ~A (~D, ~D) > " type name x y)
                40)
  :write-args (stream mouse-event)
  :write-form (progn (write-byte (if (eq (mouse-event-type mouse-event)
                                         ':mouse-click)
                                     0 1)
                                 stream)
                     (write-byte (mouse-event-bits  mouse-event) stream)
                     (write-byte (mouse-event-click mouse-event) stream)
                     (write-byte (bw::mouse-event-number-of-clicks mouse-event) stream)
                     (let ((x (mouse-event-x-pos mouse-event))
                           (y (mouse-event-y-pos mouse-event)))
                       (write-byte (ldb& %%bin-op-top-half x) stream)
                       (write-byte (ldb& %%bin-op-low-half x) stream)
                       (write-byte (ldb& %%bin-op-top-half y) stream)
                       (write-byte (ldb& %%bin-op-low-half y) stream))))

(def-dribble-handlers (mouse-state 2)
  :read-args (stream)
  :read-form (let ((buttons (read-byte stream))
		    (x (dpb& (read-byte stream)
                             %%bin-op-top-half (read-byte stream)))
                    (y (dpb& (read-byte stream)
                             %%bin-op-top-half (read-byte stream))))
	       (setf (svref *current-mouse-state* 0) buttons
		     (svref *current-mouse-state* 1) x
		     (svref *current-mouse-state* 2) y))
  :print-args (stream)
  :print-form (let ((buttons (read-byte stream))
		    (x (dpb& (read-byte stream)
                             %%bin-op-top-half (read-byte stream)))
                    (y (dpb& (read-byte stream)
                             %%bin-op-top-half (read-byte stream))))
                (format t "#<Mouse State ~A (~D, ~D) > " buttons x y)
                30)
  :write-args (stream buttons x y)
  :write-form (progn (write-byte (dribble-encode-mouse-buttons buttons) stream)
		     (write-byte (ldb& %%bin-op-top-half x) stream)
		     (write-byte (ldb& %%bin-op-low-half x) stream)
		     (write-byte (ldb& %%bin-op-top-half y) stream)
		     (write-byte (ldb& %%bin-op-low-half y) stream)))

(defun dribble-encode-mouse-buttons (buttons)
  buttons)

(defun dribble-decode-mouse-buttons (buttons)
  buttons)

(defun update-dribble-mouse-state ()
  (when (and *dribble-playback* (streamp *dribble-playback-stream*))
    (let ((byte1 (read-byte *dribble-playback-stream*)))
      (if (not (=& byte1 *dribble-handler-prefix*))
          (warn "Bad byte(~D) read in mouse state update (should be ~D)"
                 byte1 *dribble-handler-prefix*)
          (let ((byte2 (read-byte *dribble-playback-stream*)))
            (if (not (=& byte2 2))
                (warn "Bad byte(~D) read in mouse state update (should be 2)"
                       byte2)
                (progn (dribble-read byte2 *dribble-playback-stream*)
                       ;; the state vector should be filled now
                       (when *mouse-track-pause-during-playback*
                         (sleep *dribble-explicit-pause-time*))
                       (warp-pointer *boxer-pane*
                                     (dribble-mouse-state-x)
                                     (dribble-mouse-state-y)))))))))

;; substs would be best but mcl doesn't currently support them
;; these are meant to be called after the vector has been filled
(defun dribble-mouse-state-buttons ()
  (aref (the (simple-vector 3) *current-mouse-state*) 0))

(defun dribble-mouse-state-x ()
  (aref (the (simple-vector 3) *current-mouse-state*) 1))

(defun dribble-mouse-state-y ()
  (aref (the (simple-vector 3) *current-mouse-state*) 2))

(def-dribble-handlers (extended-char 3)
  :read-args (stream)
  :read-form (let ((input (+ 256 (read-byte stream))))
               (declare (special dribble-playback-bits))
               (handle-boxer-input input dribble-playback-bits)
               (redisplay))
  :print-args (stream)
  :print-form (let* ((char-code (+ 256 (read-byte stream)))
                     (fancy-name
                      (when (<& char-code (array-dimension *key-names* 0))
                        (symbol-name (aref *key-names* char-code 0)))))
                 (if (not (null fancy-name))
                     (let ((fancy-char-string (subseq fancy-name 0
                                                      (search "-KEY"
                                                              fancy-name))))
                       (format t "~A " fancy-char-string)
                       (1+ (length fancy-char-string)))
                     (progn (format t "#\~D " char-code) 6)))
  :write-args (stream char-code)
  :write-form (progn (write-byte (- char-code 256) stream)))

;; store some info about the environment
(def-dribble-handlers (dribble-preamble 4)
  :read-args (stream)
  :read-form (let ((ww (dpb& (read-byte stream)
                              %%bin-op-top-half (read-byte stream)))
                   (wh (dpb& (read-byte stream)
                             %%bin-op-top-half (read-byte stream))))
               (multiple-value-bind (cww cwh)
                   (window-inside-size *boxer-pane*)
                 (unless (and (= ww cww) (= wh cwh))
                   (boxer-editor-warning "Dribble file window size (~D,~D) not = to current size (~D, ~D)"
                         ww wh cww cwh))))
  :print-args (stream)
  :print-form (let ((ww (dpb& (read-byte stream)
                              %%bin-op-top-half (read-byte stream)))
                    (wh (dpb& (read-byte stream)
                              %%bin-op-top-half (read-byte stream))))
                (format t "~&Dribble File Window Size was (~D, ~D)~&" ww wh)
                0)
  :write-args (stream)
  :write-form (multiple-value-bind (ww wh)
                  (window-inside-size *boxer-pane*)
                (write-byte (ldb& %%bin-op-top-half ww) stream)
                (write-byte (ldb& %%bin-op-low-half ww) stream)
                (write-byte (ldb& %%bin-op-top-half wh) stream)
                (write-byte (ldb& %%bin-op-low-half wh) stream)))

(def-dribble-handlers (dribble-pause 5)
  :read-args (stream) :read-form (progn stream ; get rid of unused var warnings
                                        (sleep *dribble-explicit-pause-time*))
  :print-args (stream) :print-form (progn stream ; get rid of unused var warnings
                                          (format t "#<PAUSE> ") 9)
  :write-args (stream) :write-form nil)

(def-dribble-handlers (mouse-move 6)
  :read-args (stream)
  :read-form (let ((x (dpb& (read-byte stream) %%bin-op-top-half (read-byte stream)))
                   (y (dpb& (read-byte stream) %%bin-op-top-half (read-byte stream))))
               (warp-pointer *boxer-pane* x y))
  :print-args (stream)
  :print-form (let ((x (dpb& (read-byte stream) %%bin-op-top-half (read-byte stream)))
                    (y (dpb& (read-byte stream) %%bin-op-top-half (read-byte stream))))
                (format t "#<Mouse Move (~D, ~D) > " x y) 26)
  :write-args (stream x y)
  :write-form (progn (write-byte (ldb& %%bin-op-top-half x) stream)
		     (write-byte (ldb& %%bin-op-low-half x) stream)
		     (write-byte (ldb& %%bin-op-top-half y) stream)
		     (write-byte (ldb& %%bin-op-low-half y) stream)))

(defun dribble-write-char (char-code stream)
  (cond ((<=& 0 char-code 255) (write-byte char-code stream))
        ((<=& 256 char-code 511) (dribble-write extended-char stream char-code))
        (t (error "Input Code too large (~D)" char-code))))

(defun record-key-input (char &optional bits)
  (cond ((null *record-keystrokes*))
        ((and (eq *record-keystrokes* ':dribble)
              (streamp *dribble-file-stream*)
              #+mcl (open-stream-p *dribble-file-stream*)
	      #+lcl (output-stream-p *dribble-file-stream*))
         (let ((code (if (numberp char) char (char-code char)))
               (bits (or bits (char-bits char))))
           (cond #+mcl
                 ((and (=& code 270) (=& bits 3))
                  ;; on the mac, 270 is the F15 (Pause) key
                  (dribble-write dribble-pause *dribble-file-stream*))
                 #+mcl
                 ((and (=& code 269) (=& bits 3))
                  ;; on the mac, 269 is the F14 (Scroll lock) key
                  (multiple-value-bind (mx my) (mouse-window-coords)
                    (dribble-write mouse-move *dribble-file-stream* mx my)))
                 ((zerop bits) (dribble-write-char code *dribble-file-stream*))
                 (t (dribble-write shift *dribble-file-stream* bits)
                    (dribble-write-char code *dribble-file-stream*)))))
        (t (push char *boxer-keystroke-history*))))

(defboxer-command com-noop (&rest ignore)
  "Nothing Nothing Nothing"
  ignore boxer-eval::*novalue*)

(defun record-mouse-input (mouse-event)
  (cond ((null *record-keystrokes*))
        ((and (eq *record-keystrokes* ':dribble)
              (streamp *dribble-file-stream*)
              #+mcl (open-stream-p *dribble-file-stream*)
	      #+lcl (output-stream-p *dribble-file-stream*))
         (dribble-write mouse *dribble-file-stream* mouse-event))
        (t (push mouse-event *boxer-keystroke-history*))))

(defun record-mouse-state (buttons x y)
  (cond ((or (null *record-keystrokes*) (not (null *dribble-playback*))))
	((and (eq *record-keystrokes* ':dribble)
              (streamp *dribble-file-stream*)
              #+mcl (open-stream-p *dribble-file-stream*)
	      #+lcl (output-stream-p *dribble-file-stream*))
	 (dribble-write mouse-state *dribble-file-stream* buttons x y))))

(defun record-pause-state ()
  (cond ((or (null *record-keystrokes*) (not (null *dribble-playback*))))
	((and (eq *record-keystrokes* ':dribble)
              (streamp *dribble-file-stream*)
              #+mcl (open-stream-p *dribble-file-stream*)
	      #+lcl (output-stream-p *dribble-file-stream*))
	 (dribble-write dribble-pause *dribble-file-stream*))))

(defun record-mouse-move (x y)
  (cond ((or (null *record-keystrokes*) (not (null *dribble-playback*))))
	((and (eq *record-keystrokes* ':dribble)
              (streamp *dribble-file-stream*)
              #+mcl (open-stream-p *dribble-file-stream*)
	      #+lcl (output-stream-p *dribble-file-stream*))
	 (dribble-write mouse-move *dribble-file-stream* x y))))

(defun dribble-on (pathname)
  (setq *dribble-file-stream*
        (open pathname :direction :output :if-exists *dribble-file-exists-action*
              :if-does-not-exist :create
              :element-type '(unsigned-byte 8.)))
  #+mcl (ccl::set-mac-file-type pathname :BOXD)
  #+mcl (ccl::set-mac-file-creator pathname :BOXR)
  (dribble-write dribble-preamble *dribble-file-stream*)
  (setq *record-keystrokes* ':dribble))

(defun dribble-off ()
  (close *dribble-file-stream*)
  (setq *dribble-file-stream* nil)
  (setq *record-keystrokes* nil))

(defvar *events-per-line-for-printing* 40.)

(defvar *dribble-print-width* 70)

(defun show-dribble-file (pathname)
  (let ((col-counter 0) (eof-value (list 'eof)))
    (with-open-file (s pathname :element-type '(unsigned-byte 8))
      (do ((byte (read-byte s nil eof-value) (read-byte s nil eof-value)))
          ((eq byte eof-value))
        (cond ((zerop byte)
               ;; call the special handler to do the job, returned value is
               ;; the width of the printed event
               (incf col-counter (dribble-print (read-byte s) s)))
              (t
               ;; must be a character
               (let ((char (code-char byte))
                     (fancy-name (symbol-name (aref *key-names* byte 0))))
                 (if (or (null char) (not (graphic-char-p char)))
                     (let ((fancy-char-string (subseq fancy-name 0
                                                      (search "-KEY"
                                                              fancy-name))))
                       (format t "~A " fancy-char-string)
                       (incf col-counter (1+ (length fancy-char-string))))
                     (progn
                       (format t "~C " (code-char byte))
                       (incf col-counter 2))))))
        (when (>& col-counter *dribble-print-width*)
          (terpri t)
          (setq col-counter 0))))))

(defun playback-dribble-file (pathname)
  #+mcl ;; yuck...
  (when (null (pathname-type pathname))
    (setq pathname (make-pathname :defaults pathname :type :unspecific)))
  #-mcl
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (playback-dribble-stream s))
  #+mcl ;; mac version has trouble with recursive edit/redisplay
  (bw::queue-event (list 'playback-dribble-file-1 pathname))
  boxer-eval::*novalue*)

#+mcl
(defun playback-dribble-file-1 (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (playback-dribble-stream s)))

(defun playback-dribble-stream (s)
  (let ((eof-value (list 'eof))
        (*dribble-playback* t)
        (*dribble-playback-stream* s)
        (set-bits nil)
        (dribble-playback-bits nil))
    (declare (special dribble-playback-bits))
    (do ((byte (read-byte s nil eof-value) (read-byte s nil eof-value)))
        ((eq byte eof-value))
      (bw::boxer-system-error-restart
        (catch 'boxer-editor-top-level
          (cond ((zerop byte)
                 ;; call the special handler to do the job
                 (dribble-read (read-byte s) s))
                (t
                 ;; a simple character
                 (handle-boxer-input (code-char byte) dribble-playback-bits)
                 (redisplay)))))
      (cond ((and dribble-playback-bits set-bits)
             (setq set-bits nil dribble-playback-bits nil))
            ((not (null dribble-playback-bits))
             (setq set-bits t))))))

#+mcl
(deffile-type-reader :boxd playback-dribble-file)

(defun decode-input-for-printing (input &optional (stream nil))
  (cond ((characterp input)
	 ;; must be a keystroke
	 (format stream "~c " input))
	((bw::mouse-event? input)
	 ;; some sort of BLIP, probably from the mouse
	 (format stream "Mouse:~A " input))))

(defun print-keystrokes (&optional (last-n (length *boxer-keystroke-history*))
			           (stream *standard-output*))
    (dotimes (index last-n)
      (when (zerop (mod index *events-per-line-for-printing*))
	(terpri stream))
      (decode-input-for-printing
	(nth (- last-n index 1) *boxer-keystroke-history*)
	stream)))

