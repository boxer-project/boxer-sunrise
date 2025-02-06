;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER; Base:8.-*-
;;;;
;;;;        Boxer
;;;;        Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
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

(defclass gesture-spec ()
  ((data :initform nil :initarg :data :accessor gesture-spec-data)
   (modifiers :initform nil :initarg :modifiers :accessor gesture-spec-modifiers)))

(defun make-gesture-spec (data modifiers)
  (make-instance 'gesture-spec :data data :modifiers modifiers))

(defgeneric gesture-spec-p (x) (:method (x) nil) (:method ((x gesture-spec)) t))

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

(defvar *initial-platform* #+(or os-macosx macosx)  :lwm
                           #+win32   :ibm-pc
                           #+linux   :linux)

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
           (define-input-devices :lwm  ; mac's under lispworks opengl port
             ("SHIFT" "CONTROL" "CONTROL-SHIFT" "OPTION"                                      ;;  1  2  3  4
             "SHIFT-OPTION" "CONTROL-OPTION" "CONTROL-SHIFT-OPTION" "COMMAND"                 ;;  5  6  7  8
             "SHIFT-COMMAND" "CONTROL-COMMAND" "CONTROL-SHIFT-COMMAND" "OPTION-COMMAND"       ;;  9 10 11 12
             "SHIFT-OPTION-COMMAND" "CONTROL-OPTION-COMMAND" "CONTROL-SHIFT-OPTION-COMMAND")  ;;    13 14 15

             ("CLICK" "MIDDLE-CLICK" "RIGHT-CLICK"                       ;; 0  1  2
              "DOUBLE-CLICK" "DOUBLE-MIDDLE-CLICK" "DOUBLE-RIGHT-CLICK"  ;; 3  4  5
              "DOWN" "MIDDLE-DOWN" "RIGHT-DOWN"                          ;; 6  7  8
              "UP" "MIDDLE-UP" "RIGHT-UP")                               ;; 9 10 11
             *lwm-keyboard-key-name-alist*)

           ;; pick names to maximize compatibility with mac code
           ;; assign to left button to be plain "CLICK"
           (define-input-devices :ibm-pc
            ("SHIFT"
            "CTRL" "CTRL-SHIFT"
            "ALT" "SHIFT-ALT" "CTRL-ALT" "CTRL-SHIFT-ALT"
            "WIN" "SHIFT-WIN" "CTRL-WIN" "CTRL-SHIFT-WIN"
            "ALT-WIN" "SHIFT-ALT-WIN" "CTRL-ALT-WIN"
            "CTRL-SHIFT-ALT-WIN")

             ("CLICK" "MIDDLE-CLICK" "RIGHT-CLICK"                       ;; 0  1  2
              "DOUBLE-CLICK" "DOUBLE-MIDDLE-CLICK" "DOUBLE-RIGHT-CLICK"  ;; 3  4  5
              "DOWN" "MIDDLE-DOWN" "RIGHT-DOWN"                          ;; 6  7  8
              "UP" "MIDDLE-UP" "RIGHT-UP")                               ;; 9 10 11
             *lwm-keyboard-key-name-alist*)

           (define-input-devices :linux
            ("SHIFT"
            "CTRL" "CTRL-SHIFT"
            "ALT" "SHIFT-ALT" "CTRL-ALT" "CTRL-SHIFT-ALT"
            "META" "SHIFT-META" "CTRL-META" "CTRL-SHIFT-META"
            "ALT-META" "SHIFT-ALT-META" "CTRL-ALT-META"
            "CTRL-SHIFT-ALT-META")

             ("CLICK" "MIDDLE-CLICK" "RIGHT-CLICK"                       ;; 0  1  2
              "DOUBLE-CLICK" "DOUBLE-MIDDLE-CLICK" "DOUBLE-RIGHT-CLICK"  ;; 3  4  5
              "DOWN" "MIDDLE-DOWN" "RIGHT-DOWN"                          ;; 6  7  8
              "UP" "MIDDLE-UP" "RIGHT-UP")                               ;; 9 10 11
             *lwm-keyboard-key-name-alist*)
)


;;;; Now, setup the keyboard...
;; This is split into 2 parts, a common part which defines alphanumeric
;; keys which should be common to all keyboard and a different part which
;; defines keys (like function keys) which are specific to particular
;; kinds of keyboards

(defun define-basic-keys (platform)
  "Give names to all the standard character keys.  Alphabetic
  keys are by given the lowercase meaning, with CAPITAL
  defined as a shift key.

  This handles most alphanumeric keys and special symbols.
  "

  ;; #o101 (65) is Capitol A in ascii/unicode and we loop until #o133 (91) which
  ;; is #\[ the character right after Z. We define this character, and the character
  ;; #o40 (32) which translates us to the lower case letters in ascii starting at
  ;; 97 for #\a.
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
    ((= key-code #o177))                                       ; Stops at the DEL key
    (unless (or (and (>= key-code #o101) (<= key-code #o132))  ; Capitol A-Z in ascii
                (and (>= key-code #o141) (<= key-code #o172))) ; Lower case a-z
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
      (char-code (cadr key-that-format-~c-loses-on)) platform)))

;; this exists as a separate function because it may have to called
;; at startup time (e.g. for the X-Windows implementation, the particulars
;; of the keyboard are not generally known until then)

(defun configure-for-keyboard (platform)
  ;; finally, handle any machine/window system specific stuff
  ;; that doesn't fit very well into common lisp chars
  (dolist (special-key (input-device-special-keys platform))
    (define-key-and-all-its-shifted-key-names
      (car special-key) (cadr special-key) platform)))

;;;; Mice

;;;; Click ==> name translation
(defvar *default-mouse-click-name-translation-table*)

(defvar *alternate-mouse-click-name-translation-table*)

(defconstant *maximum-number-of-desired-mouse-clicks* 2)

(defsubst maximum-mouse-button-encoding ()
  (-& (array-dimension *default-mouse-click-name-translation-table* 0) 1))

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
      (symbol-format nil "MOUSE-~A-ON-~A" button place))
     (t
      (symbol-format nil "~A-MOUSE-~A-ON-~A" shift button place)))))

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

(defun set-mouse-translation-table (platform)
  ;; setup the default click table...
  (setup-mouse-translation-table platform nil)
  ;; now setup any tables for specific parts of the box borders
  (dolist (name (defined-mouse-border-areas))
    (setup-mouse-translation-table platform name)))

;;; putting it all together
(defun make-input-devices (platform)
  (set-mouse-translation-table platform)
  (define-basic-keys platform)
  (configure-for-keyboard platform)
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
           (make-input-devices *initial-platform*))



;;;;  Main dispatch function called by system dependent input loops or
;;;;  event handlers

(defun key-to-keep-shifted? (data)
  "Some keys, such as the arrow keys, we want to preserve the shiftability so that different
  commands can be run for them (such as keyboard text selecting.) Look inside boxer-key-handler
  for further details."
  (member data '(:left :right :down :up)))

(defun remove-shift-bit (bits)
  "Temporary function to remove the shift bit (see boxer-bugs-107) until we figure
   out all the semantics of keyboard layouts and when to include/remove shift for
   magic names."
   (logandc2 bits 1))

;;; Hacked up to handle MCL's crippled character system
(defun handle-boxer-input (input &optional (raw-bits 0) (keep-shift nil))
  (let ((bits (if keep-shift raw-bits (remove-shift-bit raw-bits))))
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
              (if (or (null key-name)
                      (not (handle-boxer-key key-name
                                            (if (numberp input) input
                                              (char-code input))
                                            (or bits (input-bits input)))))
                (unhandled-boxer-input key-name))))
          ((mouse-event? input)
            (mouse-click-boxer-input-handler input))
          (t
            (unhandled-boxer-input input))))))

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

(defun lookup-input-name (data)
  "Takes a character/number (key-event?), mouse-event, or gesture-spec and returns
  the boxer-user symbol for the input such as `A-KEY` or `MOUSE-CLICK`"
  (cond ((characterp data)
         (lookup-key-name (char-code data) 0))
        ((numberp data)
         (lookup-key-name data 0))
        ((mouse-event? data)
         (let ((click  (mouse-event-click  data))
                       (x-pos  (mouse-event-x-pos  data))
                       (y-pos  (mouse-event-y-pos  data))
                       (bits   (mouse-event-bits   data)))
                       ;; now call the mouse tracker to see if we
                       ;; are on a border area
                       (multiple-value-bind (mouse-bp local-x local-y
                                                      area)
                    (mouse-position-values x-pos y-pos)
                         (declare (ignore mouse-bp local-x local-y))
                         (lookup-click-name click bits area))))
        ((gesture-spec-p data)
         (lookup-key-name (gesture-spec-data data) (gesture-spec-modifiers data)))
        (t nil)))

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

