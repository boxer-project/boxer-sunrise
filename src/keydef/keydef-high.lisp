;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER; Base:8.-*-
;;;;
;;;;        Boxer
;;;;        Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;        Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;        used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;        Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;        https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                             +-Data--+
;;;;                    This file is part of the | BOXER | system
;;;;                                             +-------+
;;;;
;;;;
;;;;
;;;;       This file contains utilities for defining keys and how to handle them.
;;;;
;;;;    Modification History (most recent at top)
;;;;
;;;;     7/14/06 all #+lwwin changed to #+lispworks for opengl port
;;;;     8/20/03 added :initial-element nil to make-mouse-click-name-translation-table,
;;;;             initialize-lookup-arrays
;;;;     4/21/03 merged current LW and MCL files
;;;;    10/18/02 INITIALIZE-INPUT-LOOKUP-ARRAYS *INITIAL-PLATFORM*
;;;;             *KEY-NAMES* *ALTERNATE-KEY-NAMES*
;;;;             *ALTERNATE-MOUSE-CLICK-NAME-TRANSLATION-TABLE*
;;;;             HANDLE-BOXER-INPUT changed to pass keycode and bits to specific functions
;;;;             so that alternate-key/click name handling works faster and conses less
;;;;             MOUSE-CLICK-BOXER-INPUT-HANDLER passes CLICK BITS AREA for the same reason
;;;;    10/09/02 code review: existing platform specific input device functions predate
;;;;             initial PC port so no need to excise specific functions before rewrite
;;;;     3/19/02 *boxer-event-id*, boxer-event-id, next-boxer-event
;;;;             used in handle-boxer-event
;;;;     7/12/00 *key-name-lookup-array-size* changed to 230 to handle full range of F keys
;;;;     1/03/00 mouse names changed to maximize compatibility with mac code
;;;;    12/18/99 mouse names
;;;;    11/27/99 function key handling
;;;;     8/03/99 added lwwin chages in handle-boxer-input
;;;;     5/27/98 Started Logging Changes: source = Boxer version 2.3 alphaR1
;;;;

(in-package :boxer)

;;;;KEY-NAMES

;;; This file defines :BOXER-FUNCTION names for the various keystrokes and
;;; mouse clicks the user can type. This file just defines names for those
;;; keys, other files (COMx) should do DEFBOXER-FUNCTIONs to define what
;;; those keys should do. In terms of ZWEI, this file is below the level
;;; of COMTABs (more at the level of kbd-convert-to-software-char) and the
;;; files which do the DEFBOXER-FUNCTIONs are at the level of COMTABs.

;;; In order to provide fast conversion of LISPM character codes to
;;; BOXER key names, we use an array to look them up in. This is kind
;;; of like ZWEI.

(defvar *initial-platform* #+macosx  :lwm
                           #+win32   :ibm-pc
                           #+linux   :ibm-pc)

(defconstant *key-name-lookup-array-size* 328
  "For most implementations, this really ought to be based on char-code-limit
  TODO sgithens Review why we need this, the currenc char-code-limit on linux is 1114112")

(defvar *key-names* nil
  "KEY-NAMES is an art-q array of dimensions 170. by 16.. It is used
   to assign symbol names to keys on the keyboard. An array is used so
   that when a key is pressed the symbol name for the key can be found
   more quickly.")

(defvar *alternate-key-names*)

;;; look for the analogous mouse array in the keydef-low files

(defvar *boxer-keystroke-history* nil
  "A list of all the keys pressed. ")

(defvar *record-keystrokes* nil)

(defvar *boxer-command-key-alist* nil
  "An association list of key names and command names. ")

(defvar *key-name-out-of-range-warning?* nil)

(defvar *boxer-event-id* 0)
(defun boxer-event-id () *boxer-event-id*)
(defun next-boxer-event () (incf *boxer-event-id*))

(defun define-key-name (key-name key-code &optional (bits 0))
  (cond ((numberp key-code)
         (if (<& key-code (car (array-dimensions *key-names*)))
           (setf (aref *key-names* key-code bits) key-name)
           (when *key-name-out-of-range-warning?*
             (warn
              "~D is beyond the range of the current key-array" key-code))))
    ((symbolp key-code)
     (error
      "~%Can't store symbols in key-names.~
           ~%In order to teach Boxer how to handle a new kind of symbol~
           ~%in its input buffer you should define a function to handle~
           ~%the symbol on the symbol's :BOXER-INPUT property. When Boxer~
           ~%sees that symbol in its input buffer it will call that function~
           ~%with the symbol as its only argument."))
    ((listp key-code)
     (error
      "~%Can't store blips in key-names.~
           ~%In order to teach the editor how to handle a new kind of blip in~
           ~%in its input buffer you should define a function to handle the~
           ~%blip on the :BOXER-INPUT property of the symbol which is the car~
           ~%of the blip. When Boxer sees a blip with that symbol as its car~
           ~%in its input buffer it will call that function with the blip as~
           ~%its only argument."))
    (t
     (error "~S is a completely unknown type of Boxer Input." key-code))))


(defun lookup-key-name (key-code key-bits)
  (and (array-in-bounds-p *key-names* key-code key-bits)
       (aref *key-names* key-code key-bits )))


(defun define-key-and-all-its-shifted-key-names (key-name key-code platform)
  (if (and (< key-code char-code-limit)
           (upper-case-p (code-char key-code)))
    ;; for some reason MCL considers some undefined chars to be uppercase
    (define-key-name (intern-in-bu-package (symbol-format nil "CAPITAL-~A"
                                                          key-name))
      key-code)
    (define-key-name key-name     key-code))
  (do* ((bit 1 (1+ bit))
        (shift-list (input-device-shift-list platform) (cdr shift-list))
        (shift-name (car shift-list) (car shift-list))
        (shifted-key-name (intern-in-bu-package
                           (symbol-format nil "~A-~A" shift-name key-name))
                          (intern-in-bu-package
                           (symbol-format nil "~A-~A" shift-name key-name))))
    ((null shift-list))
    (define-key-name shifted-key-name key-code bit)))

;; used to propagate bindings for old key names to new key names
(defun check-key-rebinding (old-name new-name)
  ;; first check the global table
  (when (boundp old-name)
    (boxer-eval::boxer-toplevel-set-nocache new-name
                                            (boxer-eval::static-variable-value
                                             (symbol-value old-name))))
  ;; now check all the comtabs
  (dolist (ct *existing-comtabs*)
    (let ((old-value (gethash old-name ct)))
      (unless (null old-value) (setf (gethash new-name ct) old-value)))))


;;; Special Keys for each type of machine
(defvar *keyboard-special-keys*
  '((BU::SPACE-KEY        #\SPACE)
    (BU::RETURN-KEY       #\RETURN)
    (BU::DELETE-KEY       #\DELETE)
    (BU::ESCAPE-KEY       #\ESC)
    (BU::TAB-KEY          #\TAB)))


;;;; define known input devices platforms

(eval-when (eval load)
           ;; assume the default is more likely to be some kind of unix machine
           ;; with a 3 button mouse, ignore possible function keys
           (define-input-devices :default
             ("CTRL" "META" "CTRL-META")
             ("LEFT" "MIDDLE" "RIGHT" "LEFT-TWICE" "MIDDLE-TWICE" "RIGHT-TWICE")
             nil)

           (define-input-devices :lwm  ; mac's under lispworks opengl port
             ("CTRL" "ALT" "CTRL-ALT" "COMMAND" "OPTION" "COMMAND-OPTION" )
            ;;  ("CLICK" "DOUBLE-CLICK")
            ("CLICK" "MIDDLE-CLICK" "RIGHT-CLICK" "DOUBLE-CLICK" "DOUBLE-MIDDLE-CLICK" "DOUBLE-RIGHT-CLICK")
             *lwm-keyboard-key-name-alist*)

           ;; pick names to maximize compatibility with mac code
           ;; assign to left button to be plain "CLICK"
           (define-input-devices :ibm-pc
             ("CTRL" "ALT" "CTRL-ALT")
             ("CLICK" "MIDDLE-CLICK" "RIGHT-CLICK" "DOUBLE-CLICK" "DOUBLE-MIDDLE-CLICK" "DOUBLE-RIGHT-CLICK")
             *lwm-keyboard-key-name-alist*)
)


;;;; Now, setup the keyboard...
;; This is split into 2 parts, a common part which defines alphanumeric
;; keys which should be common to all keyboard and a different part which
;; defines keys (like function keys) which are specific to particular
;; kinds of keyboards

;; first, common alphanumeric keys and their shifted names...
(defun define-basic-keys (platform)
  ;; Give names to all the standard character keys.  Alphabetic
  ;; keys are by given the lowercase meaning, with "CAPITAL"
  ;; defined as a shift key.
  (do* ((key-code #o101 (1+ key-code))
        (key-name (intern-in-bu-package
                   (format nil "~A-KEY" (string-upcase
                                         (format nil "~:@C"
                                                 (code-char key-code)))))
                  (intern-in-bu-package
                   (format nil "~A-KEY" (string-upcase
                                         (format nil "~:@C"
                                                 (code-char key-code)))))))
    ((= key-code #o133))
    (define-key-and-all-its-shifted-key-names key-name key-code platform)
    (define-key-and-all-its-shifted-key-names key-name (+ key-code #o40) platform))

  ;; Now give names to all the rest of the keys that we can use the format ~C
  ;; directive to get a name for. Basically these are all the random single
  ;; symbol things on the keyboard like ! @ # ~ : etc.

  (do* ((key-code 0 (1+ key-code)))
    ((= key-code #o177))
    (unless (or (and (>= key-code #o101) (<= key-code #o132))
                (and (>= key-code #o141) (<= key-code #o172)))
      (define-key-and-all-its-shifted-key-names
        (intern-in-bu-package
         (format nil "~A-KEY" (string-upcase
                               (format nil "~:@C" (code-char key-code)))))
        key-code platform)))

  ;; Give names to all the keys that we can't use the format ~C directive
  ;; to get a name for. Basically these are keys like SPACE, DELETE etc.
  ;; Now I know that there is a place in zwei, where it knows how to do
  ;; this, and that I could use that if I wanted to, but I would like this
  ;; to work in the next system release.

  (dolist (key-that-format-~c-loses-on  *keyboard-special-keys*)
    (define-key-and-all-its-shifted-key-names
      (car  key-that-format-~c-loses-on)
      (char-code (cadr key-that-format-~c-loses-on)) platform))
)

;;; This is different from define-basic-keys in that we want to loop through
;;; the existing array looking for possible translations rather than making
;;; and filling a new array, because the size of the array is determined by the
;;; capabilities of the window system
(defun reset-keys (platform)
  (let ((new-shifts (input-device-shift-list platform)))
    (dotimes (i (array-dimension *key-names* 0))
      (let ((unshifted-name (aref *key-names* i 0)))
        ;; the unshifted names should be the same regardless of keyboard
        (unless (null unshifted-name)
          (do* ((shifts new-shifts (cdr shifts))
                (shift-name (car shifts) (car shifts))
                (bit 1 (1+ bit))
                ;; get rid of possible "CAPITAL-" in the shifted names
                (vanilla-name (if (search "CAPITAL-"
                                          (symbol-name unshifted-name))
                                (subseq (symbol-name unshifted-name) 8)
                                unshifted-name)))
            ((null shifts))
            (let ((new-name (intern-in-bu-package
                             (symbol-format nil "~A-~A"
                                            shift-name vanilla-name))))
              (check-key-rebinding (aref *key-names* i bit) new-name)
              (setf (aref *key-names* i bit) new-name))))))))


;; this exists as a separate function because it may have to called
;; at startup time (e.g. for the X-Windows implementation, the particulars
;; of the keyboard are not generally known until then)

(defun configure-for-keyboard (platform)
  ;; finally, handle any machine/window system specific stuff
  ;; that doesn't fit very well into common lisp chars
  (dolist (special-key (input-device-special-keys platform))
    (define-key-and-all-its-shifted-key-names
      (car special-key) (cadr special-key) platform)))


;;; support for self-inserting non-standard characters

(defmacro defself-inserting-key (key-name char)
  `(progn
    (boxer-eval::defboxer-key-internal ',key-name
                                       #'(lambda ()
                                                 (with-multiple-execution
                                                   #-opengl (add-redisplay-clue (point-row) ':insert)
                                                   (insert-cha *point* ,char :moving))
                                                 (mark-file-box-dirty (point-row))
                                                 boxer-eval::*novalue*))
    (boxer-command-define ',key-name
                          (format nil "Insert the ~C character at the cursor." ,char))))



;;;; Mice


;;;; Click ==> name translation
(defvar *default-mouse-click-name-translation-table*)

(defvar *alternate-mouse-click-name-translation-table*)

(defconstant *maximum-number-of-desired-mouse-clicks* 2)

(defsubst maximum-mouse-button-encoding ()
  (-& (*& *number-of-mouse-buttons* *maximum-number-of-desired-mouse-clicks*) 1))

(defun make-mouse-click-name-translation-table (platform)
  (make-array `(,(length (input-device-mouse-string platform))
                ,(1+ (length (input-device-shift-list platform))))
              :initial-element nil))

(defun mouse-click-name-string (button shift place platform)
  (intern-in-bu-package
   (cond ((and (null place) (null shift))
          (symbol-format nil "MOUSE-~A" button))
     ((null place)
      (symbol-format nil "~A-MOUSE-~A" shift button))
     ((null shift)
      (case platform
        ((:mac :ibm-pc :lwm) (symbol-format nil "MOUSE-~A-ON-~A" button place))
        (t    (symbol-format nil "~A-MOUSE-~A" place button))))
     (t
      (case platform
        ((:mac :ibm-pc :lwm) (symbol-format nil "~A-MOUSE-~A-ON-~A" shift button place))
        (t (symbol-format nil "~A-~A-MOUSE-~A" shift place button)))))))

(defun current-mouse-click-name (button shift &optional place)
  (let ((button-names (input-device-mouse-string
                       *current-input-device-platform*))
        (shift-names (input-device-shift-list
                      *current-input-device-platform*)))
    (unless (or (>= button (length button-names))
                (> shift (length shift-names)))
      (mouse-click-name-string (nth button button-names)
                               (if (zerop& shift) nil
                                 (nth (1-& shift) shift-names))
                               place *current-input-device-platform*))))

(defun setup-mouse-translation-table (platform
                                      &optional
                                      place-name
                                      (table
                                       (make-mouse-click-name-translation-table
                                        platform))
                                      (unbound-name-help-message-p
                                       (or (null place-name)
                                           (member place-name
                                                   '(:graphics :sprite)))))
  ;; first fill the slots in the table...
  ;; starting with the unshifted names
  (do* ((button 0 (1+ button))
        (buttons (input-device-mouse-string platform) (cdr buttons))
        (button-name (car buttons) (car buttons)))
    ((null buttons))
    (let ((name (mouse-click-name-string button-name nil place-name platform)))
      (setf (aref table button 0) name)
      (when unbound-name-help-message-p
        (setf (get name 'unbound-name-help-message)
              "and its shifted names, are generated by clicking the mouse"))))
  ;; followed by the shifted names
  (do* ((bit 1 (1+ bit))
        (shift-list (input-device-shift-list platform) (cdr shift-list))
        (shift-name (car shift-list) (car shift-list)))
    ((null shift-list))
    (do* ((button 0 (1+ button))
          (buttons (input-device-mouse-string platform) (cdr buttons))
          (button-name (car buttons) (car buttons)))
      ((null buttons))
      (setf (aref table button bit)
            (mouse-click-name-string button-name shift-name
                                     place-name platform))))
  ;; now install the table where we can find it
  (if (null place-name)
    (setq *default-mouse-click-name-translation-table* table)
    (setf (get place-name 'click-translation-table) table)))

;; the fundamental between setup-mouse-translation-table and
;; reset-mouse-translation-table is that the reset version tries to
;; use the existing translation arrays because the initial translation arrays
;; will be hardware dependent.  Rather than allocating a new array which may
;; not fit the inputs which the current window system is capable of generating,
;; we iterate through the existing array looking for possible translations
;; for each entry, entries with no translation are left alone and the input
;; will continue to generate the old name
(defun reset-mouse-translation-table (platform
                                      &optional
                                      place-name
                                      (table
                                       (make-mouse-click-name-translation-table
                                        platform)
                                       table-supplied?))
  (when (not table-supplied?)
    (warn "Reset-mouse-translation-table:table was not supplied, making one...")
    ;; if we are making new ones, better install them
    (if (null place-name)
      (setq *default-mouse-click-name-translation-table* table)
      (setf (get place-name 'click-translation-table) table)))
  ;; more checking
  (let ((current-buttons (array-dimension table 0))
        (old-names (input-device-mouse-string *current-input-device-platform*)))
    ;; this loses when switching back, we do need some sort of reality
    ;; check here eventually
    ;    (when (not (= current-buttons (length old-names)))
    ;      (error "Mismatch between array size and..."))
    (do* ((bit 0 (1+ bit))
          (shift-list (cons nil (input-device-shift-list platform))
                      (cdr shift-list))
          (shift-name (car shift-list) (car shift-list)))
      ((>=& bit (array-dimension table 1)))
      (do* ((button 0 (1+& button))
            (buttons old-names (cdr buttons))
            (unshifted-translation (unshifted-click-translation
                                    (aref table button 0) platform)))
        ((>=& button current-buttons))
        (unless (null unshifted-translation)
          (let ((new-name (mouse-click-name-string unshifted-translation
                                                   shift-name place-name platform)))
            (unless (fast-memq platform *bound-input-device-platforms*)
              (check-key-rebinding (aref table button bit) new-name))
            (setf (aref table button bit) new-name)))))))

;;; the right fix is to add enough info to DEFINE-INPUT-DEVICES to
;;; enable a programmatic solution
;;; this is crocked up to do the following:

;; left :mac => nil
;; middle :mac => click
;; right :mac => nil
;; click :sun = > middle
;; plus the double click versions
(defun unshifted-click-translation (current-name new-platform)
  (case new-platform
    (:mac (cond ((eq current-name (mouse-click-name-string "MIDDLE" nil
                                                           nil :default))
                 "CLICK")
            ((eq current-name (mouse-click-name-string "MIDDLE-TWICE" nil
                                                       nil :default))
             "DOUBLE-CLICK")))
    (t    (cond ((eq current-name (mouse-click-name-string "CLICK" nil
                                                           nil :mac))
                 "MIDDLE")
            ((eq current-name (mouse-click-name-string "DOUBLE-CLICK" nil
                                                       nil :mac))
             "MIDDLE-TWICE")))))

(defun lookup-click-name (click bits &optional border-area)
  (cond ((>& click  (maximum-mouse-button-encoding)) 'bu::mouse-lots-o-clicks)
    ((or (null border-area)
         (eq border-area ':inside) (eq border-area ':outside))
     (aref *default-mouse-click-name-translation-table* click bits))
    (t (let ((table (get border-area 'click-translation-table)))
         (cond ((null table)
                (warn "No mouse click name translation table for ~A~
                           ~%Perhaps you still need to call ~
                            (setup-mouse-translation-table ~S ~S)"
                      border-area
                      *current-input-device-platform* border-area)
                'bu::generic-mouse-click)
           (t (aref table click bits)))))))


(defun set-mouse-translation-table (platform rebind?)
  ;; setup the default click table...
  (if rebind?
    (reset-mouse-translation-table platform nil
                                   *default-mouse-click-name-translation-table*)
    (setup-mouse-translation-table platform nil))
  ;; now setup any tables for specific parts of the box borders
  (dolist (name (defined-mouse-border-areas))
    (if rebind?
      (reset-mouse-translation-table platform name
                                     (get name 'click-translation-table))
      (setup-mouse-translation-table platform name))))

;;; putting it all together
(defun make-input-devices (platform &optional (rebind? t))
  (set-mouse-translation-table platform rebind?)
  (cond (rebind? (reset-keys platform))
    (t (define-basic-keys platform)
       (configure-for-keyboard platform)))
  (push platform *bound-input-device-platforms*)
  ;; this has to come last because the previous forms may need to refer
  ;; to the old value to see if any bindings need to be forwarded
  (setq *current-input-device-platform* platform))

;; initialize these based on the value of the current platform
(defun initialize-input-lookup-arrays ()
  (setq *key-names*
        (make-array (list *key-name-lookup-array-size*
                          (1+ (length (input-device-shift-list
                                       *initial-platform*))))
                    :initial-element nil)
        *alternate-key-names*
        (make-array (list *key-name-lookup-array-size*
                          (1+ (length (input-device-shift-list
                                       *initial-platform*))))
                    :initial-element nil))
  ;; for the moment, no need to translate unshifted keys
  (dotimes (i *key-name-lookup-array-size*)
    (setf (aref *alternate-key-names* i 0) :no-alternates))
  ;; regular mouse array is already done in make-input-devices....
  (setq *alternate-mouse-click-name-translation-table*
        (make-mouse-click-name-translation-table *initial-platform*))
  ;; and for the border areas...
  (dolist (name (defined-mouse-border-areas))
    (setf (get name 'alternate-click-name-table)
          (make-mouse-click-name-translation-table *initial-platform*))))

;
;;; initial setup
(eval-when (eval load)
           (initialize-input-lookup-arrays)
           (make-input-devices *initial-platform* nil)
           )



;;;;  Main dispatch function called by system dependent input loops or
;;;;  event handlers

;;; Hacked up to handle MCL's crippled character system
(defun handle-boxer-input (input &optional bits)
  ;;;	(increment-key-tick)		;for use with multiple-kill hack
  (status-line-undisplay 'boxer-editor-error)
  ;; increment the event counter
  (next-boxer-event)
  (boxer-eval::report-eval-errors-in-editor
   ;; net prims in particular may signal eval errors as a response to
   ;; an editor command, catch it at this level so the entire command
   ;; gets aborted rather than just the net loading piece.
   (COND ((key-event? input)
          ;; Some sort of  key code. Try to lookup a name for it. If it
          ;; has a name call boxer-eval:handle-boxer-key with the name.
          (let ((key-name (lookup-key-name (if (numberp input)
                                             input
                                             (char-code input))
                                           (or bits (input-bits input)))))
            (record-key-input input (or bits (input-bits input)))
            (if (or (null key-name)
                    (not (handle-boxer-key key-name
                                           (if (numberp input) input
                                             (char-code input))
                                           (or bits (input-bits input)))))
              (unhandled-boxer-input key-name))))
         ((mouse-event? input)
          (let ((handler (get (mouse-event-type input) ':boxer-input)))
            (record-mouse-input input)
            (if (or (null handler)
                    (not (funcall handler input)))
              (unhandled-boxer-input (get-mouse-click-name input)))))
         (t (unhandled-boxer-input input)))))

;;; The following comment is preserved for posterity.
;; For now just be obnoxious
(defun unhandled-boxer-input (arg)
  (boxer-editor-error
   (if (null arg) "Unusable key" "~A undefined")
   arg))





(defun mouse-click-boxer-input-handler (blip)
  (let ((window (mouse-event-window blip))
        (click  (mouse-event-click  blip))
        (x-pos  (mouse-event-x-pos  blip))
        (y-pos  (mouse-event-y-pos  blip))
        (bits   (mouse-event-bits   blip))
        (click? (eq (mouse-event-type   blip)
                    ':mouse-click)))
    ;; now call the mouse tracker to see if we are on a border area
    (multiple-value-bind (mouse-bp local-x local-y area)
                         (mouse-position-values x-pos y-pos)
                         (declare (ignore local-x local-y))
                         (let ((click-name (lookup-click-name click bits area)))
                           (handle-boxer-mouse-click
                            click-name window x-pos y-pos mouse-bp click? click bits area)))))


(defun get-mouse-click-name (blip)
  (multiple-value-bind (mouse-bp local-x local-y area)
                       (mouse-position-values (mouse-event-x-pos blip) (mouse-event-y-pos blip))
                       (values (lookup-click-name (mouse-event-click blip)
                                                  (mouse-event-bits blip)
                                                  area)
                               mouse-bp local-x local-y)))


(setf (get :mouse-click :boxer-input) 'mouse-click-boxer-input-handler)
(setf (get :mouse-hold  :boxer-input) 'mouse-click-boxer-input-handler)


;;; Documentation Support

(defmacro record-command-key (key-name command-name)
  `(eval-when (compile load eval)
              (when (not (null (assoc ,key-name *boxer-command-key-alist*)))
                (setq *boxer-command-key-alist*
                      (delete (assoc ,key-name *boxer-command-key-alist*)
                              *boxer-command-key-alist*)))
              (push (cons ,key-name ,command-name) *boxer-command-key-alist*)))

(defun record-command-key-internal (key-name command-name)
  (when (not (null (assoc key-name *boxer-command-key-alist*)))
    (setq *boxer-command-key-alist*
          (delete (assoc key-name *boxer-command-key-alist*)
                  *boxer-command-key-alist*)))
  (push (cons key-name command-name) *boxer-command-key-alist*))

;; note that while there might be several keys for one command,
;; there can only be one command for each key (at top level)

(defun get-command-for-key (key-name)
  (cdr (assoc key-name *boxer-command-key-alist*)))

(defun get-keys-for-command (command)
  (with-collection
    (dolist (pair *boxer-command-key-alist*)
      (when (eq command (cdr pair))
        (collect (car pair))))))

