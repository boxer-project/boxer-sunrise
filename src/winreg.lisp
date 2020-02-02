; -*- Mode:LISP; Syntax: Common-Lisp; Package:Boxer; -*-
#|





  Copyright 2004 - 2012 Pyxisystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


    registration utilities for Windows


Modification History (most recent at top)

10/11/12 fixed bug in format directive in print-license-key
         :integer ==> :signed-long in foreign-function "ExtractIcon"
10/06/09 changed :errorp for registry functions back to nil, check at higher levels
         and generate warnings instead.  Error @ low level prevent boxer startup
 8/22/09 changed :errorp for registry functions to T
12/15/05 redisplay init for *boxer-file-icon-image* uses lw:lisp-image-name
         to get app's directory instead of get-working-directory which can
         vary depending upon hopw the app is started
10/10/05 temporary file icon hack using external image
 9/25/05 change standard-icon to use fli:make-pointer instead of
         boxer::make-pointer (how did this EVER work before ??)
 9/23/05 added "Demo" button to boxer-license-dialog
12/22/04 ported 30 day expiration utilities from macreg
 5/12/04 changed *windows-draw-icon-options* to 4 which seems to work...
 5/30/03 licensing utilites done (
 5/16/03 draw-icon options dispatched on *windows-draw-icon-options*
 4/02/03 started file


|#



#|


 Functions for dealing with the Windows Registry...

The following registry-related symbols are exported from the WIN32
package. WITH-REGISTRY-KEY is a macro, the other ones are functions.

   close-registry-key
   collect-registry-subkeys
   create-registry-key
   delete-registry-key
   enum-registry-value
   open-registry-key
   query-registry-key-info
   query-registry-value
   registry-value
   set-registry-value
   with-registry-key

All of this seems to be a thin wrapper around FLI calls which is
exemplified by the fact that the first value of, e.g., calling
OPEN-REGISTRY-KEY is a fixnum.
To simply read a key's value from the registry you can use
QUERY-REGISTRY-VALUE:

   CL-USER 211 > (win32:query-registry-value
"Software\\Xanalys\\LispWorks\\4\\2\\Environment\\LISPWORKS-TOOLS\\GLOBAL-DEFAUL
TS\\"
"Init Filename")
   "\"C:\\\\cygwin\\\\home\\\\edi\\\\.lispworks\""
   T


The first argument is a path to the key starting from the root key
(see below), the second one is the name of the key.

The function accepts a keyword argument :ROOT which describes where
the path will start from. This can be an already open key or it can be
a symbol from the keyword package denoting one of the root keys. By
trial and error I found out that the following keyword symbols are
acceptable:

   :ROOT for HKEY_CLASSES_ROOT
   :USER for HKEY_CURRENT_USER
   :LOCAL-MACHINE for HKEY_LOCAL_MACHINE
   :USERS for HKEY_USERS

No idea if there are others. :USER seems to be the default value.

Here's an example on how to use an already open key which also
showcases the WITH-REGISTRY-KEY macro:

   CL-USER 212 > (win32:with-registry-key (system-key
"HARDWARE\\DESCRIPTION\\System\\" :root :local-machine)
                   (win32:query-registry-value "CentralProcessor\\0\\"
"ProcessorNameString" :root system-key))
   "Intel(R) Pentium(R) III Mobile CPU      1200MHz"
   T

Use NIL as the path if the root key is already where you wanted to go:

   CL-USER 218 > (win32:with-registry-key (processor-key
"HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0\\" :root :local-machine)
                   (win32:query-registry-value nil "ProcessorNameString"
:root processor-key))
   "Intel(R) Pentium(R) III Mobile CPU      1200MHz"
   T

Use NIL as the key's name if you want to get at the default value:

   CL-USER 219 > (win32:query-registry-value ".html" nil :root :root)
   "MozillaHTML"
   T

As you might have expected this is all case-insensitive:

Another keyword argument is ERRORP the use of which should be obvious:

   CL-USER 223 > (win32:query-registry-value ".htmlx" nil :root :root)

   Error: Failed to open registry subkey .htmlx (type ROOT) - Win32
Error #x2.
   CL-USER 225 > (win32:query-registry-value ".htmlx" nil :root :root
:errorp nil)
   NIL

COLLECT-REGISTRY-SUBKEYS will allow you to descend into the registry
tree:

   CL-USER 226 > (win32:collect-registry-subkeys ".html" :root :root)
   ("OpenWithList" "PersistentHandler")

OPEN-REGISTRY-KEY basically accepts the same arguments as
QUERY-REGISTRY-VALUE except for the key's name. This function has its
counterpart in CLOSE-REGISTRY-KEY.

   CL-USER 231 > (win32:open-registry-key ".html" :root :root)
   310
   T

   CL-USER 232 > (win32:close-registry-key 310)
   0

It's probably wiser to use the WITH-REGISTRY-KEY macro. If you
macroexpand this macro you'll also get a hint at what the second
return value of OPEN-REGISTRY-KEY is supposed to mean:

   CL-USER 240 > (pprint (macroexpand-1 '(win32:with-registry-key
(my-key ".html" :root :root) my-body)))

   (MULTIPLE-VALUE-BIND (MY-KEY #:CLOSEP612)
       (WIN32:OPEN-REGISTRY-KEY ".html" :ROOT :ROOT :ERRORP T)
     (WHEN MY-KEY (UNWIND-PROTECT (PROGN MY-BODY) (WHEN #:CLOSEP612
(WIN32::REG-CLOSE-KEY MY-KEY)))))

Now, if you want to go wild and create, change and delete registry
entries, here are some more examples:

   CL-USER 241 > (win32:create-registry-key "Foo" :root :user)
   312
   :CREATED-NEW-KEY

   CL-USER 242 > (win32:create-registry-key "Foo" :root :user)

   308
   :OPEND-EXISTING-KEY ;;; sic!

   CL-USER 243 > (setf (win32:registry-value "Foo" "Bar" :root :user
:expected-type :integer) 42)
   0

   CL-USER 244 > (setf (win32:registry-value "Foo" "Baz" :root :user
:expected-type :string) "Frob")
   0

   CL-USER 245 > (win32:registry-value "Foo" "Bar" :root :user
:expected-type :integer)
   42
   T

   CL-USER 246 > (win32:enum-registry-value "Foo" 0 :root :user)
   "Bar"
   :INTEGER
   42

   CL-USER 247 > (win32:registry-value "Foo" "Bar" :root :user)
   42

   T

   CL-USER 248 > (win32:delete-registry-key "Foo" :root :user)
   T

For the EXPECTED-TYPE keyword above the following list might come in
handy.

   CL-USER 252 > (pprint win32::*registry-type-values*)

   ((:NONE . 0)
    (:STRING . 1)
    (:ENVIRONMENT-STRING . 2)
    (:BINARY . 3)
    (:INTEGER . 4)
    (:LITTLE-ENDIAN-INTEGER . 4)
    (:BIG-ENDIAN-INTEGER . 5)
    (:SYMBOLIC-LINK . 6)
    (:STRING-ARRAY . 7)
    (:RESOURCE-LIST . 8)
    (:HARDWARE-RESOURCE-LIST . 9))


****************

I use win32::registry-value for registry access like this:

    (win32::registry-value
     *register*
     *key-name*
     :expected-type :lisp-object
     :errorp nil)

If *register* is "SOFTWARE\\Foo\\Bar" (according to other keys I think
Foo should be vendor name and Bar program name, or somesuch) and
*key-name* is "Gazonk" this ends up in the current user part of
registry in the key Foo\Bar\Gazonk.

This function is setf-able, which is how I store things.

****************

win32:registry-value also takes a :key-type keyword argument, takes
values such as :local-machine. (Sorry, don't have complete list to
hand.)



|#


(in-package :boxer)

;;; HDC utilities

(defmacro with-hdc-pen ((newpen hdc) &body body)
  (let ((oldpenvar (gensym)))
    `(fli:with-dynamic-foreign-objects ()
       (let ((,oldpenvar (win32::select-object ,hdc ,newpen)))
         (unwind-protect
             (progn . ,body)
           (win32::select-object ,hdc ,oldpenvar))))))

(defmacro with-hdc-brush ((newbrush hdc) &body body)
  (let ((oldbrushvar (gensym)))
    `(fli:with-dynamic-foreign-objects ()
       (let ((,oldbrushvar (win32::select-object ,hdc ,newbrush)))
         (unwind-protect
             (progn . ,body)
           (win32::select-object ,hdc ,oldbrushvar))))))

(defmacro with-hdc-pen-and-brush ((newpen newbrush hdc) &body body)
  (let ((oldpenvar (gensym))
        (oldbrushvar (gensym)))
    `(fli:with-dynamic-foreign-objects ()
       (let ((,oldpenvar (win32::select-object ,hdc ,newpen))
             (,oldbrushvar (win32::select-object ,hdc ,newbrush)))
         (unwind-protect
             (progn . ,body)
           (win32::select-object ,hdc ,oldpenvar)
           (win32::select-object ,hdc ,oldbrushvar))))))

(defun get-icon-path (type)
  (let ((ftype (win32::registry-value type nil :key-type :root
                                      :errorp nil :expected-type :string)))
    (unless (null ftype)
      (win32::registry-value (concatenate 'string ftype "\\DefaultIcon") nil
                             :key-type :root :expected-type :string :errorp nil))))

;; returns (values pathname index)
(defun parse-icon-path (pathname)
  (let* ((commapos (position #\, pathname))
         (index (read-from-string (subseq pathname (1+ commapos)) nil nil)))
    (when (integerp index)
      (values (subseq pathname 0 commapos) index))))




;;; fli

;(fli:define-foreign-function (%extract-icon "ExtractIcon" :dbcs)
;    ((hinstance :pointer) (iconfilename :pointer) (icon-index :int))
;  :result-type :integer :module :shell32)

(fli:define-foreign-function (%extract-icon "ExtractIcon" :dbcs)
    ((hinstance :pointer) (iconfilename :pointer) (icon-index :signed-long))
  :result-type :signed-long :module :shell32)

(defun extract-icon (string index)
  (fli:with-foreign-string (new-ptr element-count byte-count) string
    (declare (ignore element-count byte-count))
     (%extract-icon (fli:make-pointer :address (win32::get-current-process))
                    new-ptr (abs index))))

;-- Standard Icon IDs
(defconstant IDI_APPLICATION 32512)
(defconstant IDI_HAND 32513)
(defconstant IDI_QUESTION 32514)
(defconstant IDI_EXCLAMATION 32515)
(defconstant IDI_ASTERISK 32516)
(defconstant IDI_WINLOGO 32517)

(defun standard-icon (icon-id)
  (win32::load-icon (fli::make-pointer :address 0) icon-id))

(defun icon-value-internal (type)
  (let ((ip (get-icon-path type)))
    (multiple-value-bind (path idx)
        (when (stringp ip) (parse-icon-path ip))
      (cond ((integerp idx) (extract-icon path idx))
            ;; default
            (t (standard-icon idi_winlogo))))))

;; a caching scheme, we'll use an alist instead of a hashtable since we
;; don't expect too many entries
(defvar *icon-values-cache* nil)
(defun icon-value-entry (type) (cdr (assoc type *icon-values-cache*
                                           :test #'string-equal)))
(defun set-icon-value-entry (type new-value)
  (let ((new-entry (cons type new-value)))
    (push new-entry *icon-values-cache*)
    new-value))

(defun icon-value (type)
  (or (icon-value-entry type)
      (set-icon-value-entry type (icon-value-internal type))))

(defvar *windows-draw-icon-options* 4)
;; 4, 8 and 12 seems to work

;; try out debugging tests for various values of *windows-draw-icon-options*
;; attributes: offsets, bg/fg color, paint function, other HDC
(defconstant *byte-0* (byte 1 0))
(defconstant *byte-1* (byte 1 1))
(defconstant *byte-2* (byte 1 2))
(defconstant *byte-3* (byte 1 3))


(defsubst di-offset?    ()
  (not (zerop& (ldb *byte-0* *windows-draw-icon-options*))))

(defsubst di-set-rop?   ()
  (not (zerop& (ldb *byte-1* *windows-draw-icon-options*))))

(defsubst di-set-pen?   ()
  (not (zerop& (ldb *byte-2* *windows-draw-icon-options*))))

(defsubst di-set-brush? ()
  (not (zerop& (ldb *byte-3* *windows-draw-icon-options*))))

(defun di-options (n)
  (let ((*windows-draw-icon-options* n))
    (format t "~&offset is: ~A~&Set ROP is:~A~&Set Pen is:~A~&Set Brush is:~A"
            (di-offset?) (di-set-rop?) (di-set-pen?) (di-set-brush?))))

(defun draw-icon (icon-value x y)
  (with-pen-color (*white*)
    (draw-rectangle alu-seta 32 32 x y))
  (let ((xcoord (if (di-offset?) x (+& x %origin-x-offset)))
        (ycoord (if (di-offset?) y (+& y %origin-y-offset)))
        (hdc (slot-value (capi-internals::representation *boxer-pane*)
                         'capi-win32-lib::hdc)))
    (when (di-set-rop?) (win32::set-rop2  hdc alu-seta))
    (cond ((and (di-set-pen?) (di-set-brush?))
           (with-hdc-pen-and-brush (win32::*white-pen* win32::*white-brush* hdc)
             (win32::draw-icon hdc xcoord ycoord icon-value)))
          ((di-set-pen?)
           (with-hdc-pen (win32::*white-pen* hdc)
             (win32::draw-icon hdc xcoord ycoord icon-value)))
          ((di-set-brush?)
           (with-hdc-brush (win32::*white-brush* hdc)
             (win32::draw-icon hdc xcoord ycoord icon-value)))
          (t (win32::draw-icon hdc xcoord ycoord icon-value)))))




;;; Boxer Licensing stuff
;;; To Do: 1) fold in machine ID for better security ?
;;;        2) network poll for multiples ?  which port # to use ?
;;;        3) unlocking demo ?

(defun get-boxer-registry-value (key)
  (win32::registry-value "Software\\Pyxisystems\\Boxer"
                         key :key-type :local-machine :errorp nil))

(defun get-boxer-license-key ()
  (get-boxer-registry-value "Serial"))

(defun set-boxer-registry-value (key keystring)
  ;(win32:create-registry-key "Software\\Pyxisystems\\Boxer" key)
  (setf (win32::registry-value "Software\\Pyxisystems\\Boxer"
                               key :key-type :local-machine
                               :expected-type :string :errorp nil)
        keystring))

(defun set-boxer-license-key (keystring)
  (set-boxer-registry-value "Serial" keystring))

(defun boxer-license-dialog (&optional offer-demo?)
  (let* ((group1 (make-instance 'capi:text-input-pane :x 10 :y 40
                                :width 20 :height 15
                                :max-characters 4 :enabled t))
         (group2 (make-instance 'capi:text-input-pane :x 80 :y 40
                               :max-width 30
                               :max-characters 4 :enabled t))
         (group3 (make-instance 'capi:text-input-pane :x 150 :y 40
                               :max-width 30
                               :max-characters 4 :enabled t))
         (group4 (make-instance 'capi:text-input-pane :x 220 :y 40
                               :max-width 30
                               :max-characters 4 :enabled t))
         (license-text (make-instance 'capi:title-pane :x 20 :y 20
                                      :text "License Key Entry"))
         (items (list
                ;; the license number in 4 digit fields...
                license-text
                group1
                (make-instance 'capi:title-pane :x 70 :y 40 :text "-")
                group2
                (make-instance 'capi:title-pane :x 140 :y 40 :text "-")
                group3
                (make-instance 'capi:title-pane :x 210 :y 40 :text "-")
                group4
                ;; other buttons
                (make-instance 'capi:push-button :x 60 :y 70 :width 50 :height 20
                               :text "OK"
                               :selection-callback
                               #'(lambda (&rest ignore)
                                   (declare (ignore ignore))
                                   (capi:exit-dialog
                                    (concatenate
                                     'string
                                     (capi:text-input-pane-text group1)
                                     (capi:text-input-pane-text group2)
                                     (capi:text-input-pane-text group3)
                                     (capi:text-input-pane-text group4)))))
                (make-instance 'capi:push-button :x 190 :y 70 :width 50 :height 20
                               :text "Cancel"
                               :selection-callback 'capi:abort-dialog)))
         (ld (capi:make-container
              (make-instance
               'capi:pinboard-layout :min-width 300 :min-height 120
               :description
               (if offer-demo?
                   (append items
                           (list (make-instance 'capi:push-button
                                                :x 120 :y 70 :width 50 :height 20
                                                :text "Demo"
                                                :selection-callback
                                                #'(lambda (&rest ignore)
                                                    (declare (ignore ignore))
                                                    (capi:exit-dialog "0")))))
                 items)
               :title "License Key Entry"))))
    (capi:display-dialog ld :modal t)))


(defun valid-boxer-license? ()
  (let ((existing (get-boxer-license-key)))
    (cond ((null existing) nil)
          (t (valid-license-number (read-from-string existing))))))

;;; Valid boxer license keys
;;; 4 fields of 4 base 10 numbers
(defconstant *license-min* 1000000000000000)
(defconstant *license-prime-A* 8377)
(defconstant *license-prime-B* 9277)

(defvar *license-key-stream* T)

(defun valid-license-number (n)
  (and (> n *license-min*)
       (zerop (mod n *license-prime-A*))
       (zerop (mod n *license-prime-B*))))

;;;This should NOT appear in the app !!!!
#|
(defun generate-license-keys (&key (min-key *license-min*) (n-keys 10))
  (do ((n (* (ceiling (/ min-key (max *license-prime-A*
                                      *license-prime-B*)))
             *license-prime-B*)
          (+ n *license-prime-B*))
       (count 0))
      ((>= count n-keys) count)
    (when (zerop (mod n *license-prime-A*))
      ;; output a key
      (print-license-key n)
      (incf count))))
|#



(defun print-license-key (n)
  (format *license-key-stream* "~19,'0,'-,4:D~%" n))


;;; New expiration scheme for demo, no more hard coded date
;;;
;;; Weaknesses
;;;    1) expiration scheme just a simple (unencrypted flag)
;;;    2) if (unencrypted) date file is constantly reset, the magic numbers
;;;       won't increment - fixed, dates are now encrypted
;;;    3) flush pyxi support files, redownload
;;;    4) encryptation keys are hardcoded in the boxer app making them
;;;       easy to discover
;;;    Conclusion: the current protection scheme is probably proof against
;;;    casual copying but wont stop a halfway competent hacker

;;; eventually move this to a bundled file after digitool makes the switch
;;; the resource records the date the app expired so we could conceivably
;;; pass that info upwards to generate a more informative error message...
;;; for 30 day expiration, write a magic number out for each day
;;; anything but a number in this array indicates no startup
;;; these essentially function as one time pads except they sit in the app
;;; Note: all these numbers were achieved via (random most-positive-fixnum)
(defvar *magic-expiration-array*
  #(34734175  271118789 276516700 38802561  25471640  135929489 405980097
    509323053 430523074 231983845 149157935 269856512 406603131 102862860
    296363948 105592821 197139497 513778864 209276327 133492344 494102449
    535204507 213166693 115796836 467770347 423059588 400308497 51688408
    158928445 436662981))

(defconstant *expiration-code* 240353669)

(defvar *magic-date-array*
  #(453620908 34734175  271118789 276516700 38802561  25471640  135929489
    405980097 509323053 430523074 231983845 149157935 269856512 406603131
    102862860 296363948 105592821 197139497 513778864 209276327 133492344
    494102449 535204507 213166693 115796836 467770347 423059588 400308497
    51688408  158928445))

(defun expired-boxer? ()
  (or ;(not (null (ccl::get-resource :BXEP 128)))
   ;; need to find a Windows way of modifying the app
      (let ((magic (get-demo-magic)))
        (when (numberp magic) (= *expiration-code* magic)))))

(defun %now-date ()
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (+ (* year 10000) (* month 100) date)))

(defun expire-boxer ()
;   (let ((exp-info (ccl::make-record :boxer-expiration-info :date (%now-date))))
;     (ccl::add-resource exp-info :BXEP 128)
;     (ccl::write-resource exp-info)
;     (ccl::release-resource exp-info)
  ;; need to find a Windows way of modifying the app
  (set-boxer-registry-value "DMagic" (format nil "~D" *expiration-code*)))

(defun demo-boxer-license? ()
  (let ((existing (get-boxer-license-key)))
    (cond ((null existing) nil)
          (t (zerop (read-from-string existing))))))

;; if things aren't what they are supposed be, we'll try and fix them for now
;; when we are more sure of things, we can be more punitive - up to,
;; and including (ccl:quit)
(defvar *demo-nice?* nil)

(defun unexpected-magic-info ()
  (cond ((not (null *demo-nice?*))
         (initialize-demo-date))
        (t
         (capi:display-message "Warning:Corrupt Demo License Info")
         (lw::quit))))

(defun initialize-demo-date ()
  (write-demo-info (svref *magic-expiration-array* 0) (%now-date) 0))

(defun write-demo-info (magic date
                              &optional
                              (place (position magic
                                               *magic-expiration-array*
                                               :test #'=)))
  (cond ((null place)
         (error "Trying to write an invalid demo code:~D" magic))
        (t
         (set-boxer-registry-value "DMagic" (format nil "~D" magic))
         (set-boxer-registry-value "DDate" (format nil "~D" (+ date
                                                               (svref *magic-date-array*
                                                                      place)))))))


(defun get-demo-magic ()
  (let ((ml (get-boxer-registry-value "DMagic")))
    (when (stringp ml) (read-from-string ml nil nil))))

(defun get-demo-date (magic)
  (let ((place (position magic *magic-expiration-array* :test #'=)))
    (cond ((null place)
           (error "Invalid demo code: ~D" magic))
          (t
           (let ((dl (get-boxer-registry-value "DDate")))
             (when (stringp dl)
               (- (read-from-string dl nil nil)
                  (svref *magic-date-array* place))))))))

(defun demo-banner (place)
  (setq *boxer-version-info*
        (cond ((numberp place)
               (format nil "Boxer Free Demo: Day ~D" (1+ place)))
              ((eq place :last) "Boxer Free Demo: Last Day")
              ((eq place :reset) "Boxer Free Demo")
              (t "Boxer Free Demo"))))


;; write the next magic number if it's been more than a day since the last date,
;; maybe expired the boxer
;; also setup message

(defun demo-next-day ()
  ;; all this had better be true
  (let* ((magic-number (get-demo-magic))
         (last-date (get-demo-date magic-number))
         (place (when (numberp magic-number)
                  (position magic-number *magic-expiration-array*
                            :test #'=)))
         (last-place (1- (length *magic-expiration-array*)))
         (now (%now-date)))
    (cond ((not (numberp place))
           (unexpected-magic-info))
          ((not (numberp last-date))
           (demo-banner :reset)
           (initialize-demo-date))
          ((< place last-place)
           ;; the steady state case
           (cond ((> now last-date)
                  (write-demo-info (svref *magic-expiration-array*
                                          (1+ place))
                                   now)
                  (demo-banner (1+ place)))
                 ((= now last-date)
                  ;; same day, don't increment
                  (demo-banner place))
                 (t ;; tampering ?
                    (write-demo-info (svref *magic-expiration-array* place)
                                     now)
                    (demo-banner place))))
          ((>= place last-place)
           (expire-boxer)
           (demo-banner :last)))))

;;; temporary boxer file icon drawing hack
(defvar *boxer-file-icon-image* nil)

(def-redisplay-initialization
 (setq *boxer-file-icon-image*
       (ignore-errors
         (gp:convert-external-image
          *boxer-pane*
          (gp:read-external-image
           (merge-pathnames "boxer-file-icon.BMP"
                            (lw::lisp-image-name)))))))

(defun draw-file-icon-image (x y)
  (gp:draw-image *boxer-pane* *boxer-file-icon-image* x y))

#|

(defun draw-round-rectangle (port x y width height width-ellipse
height-ellipse)
  (fli:with-dynamic-foreign-objects ()
    (let* ((hwnd (slot-value (capi-internals::representation port) 'win32:hwnd))
           (hdc (slot-value (capi-internals::representation port)
                            'capi-win32-lib::hdc))
           (hpen (win32::create-pen win32::ps_solid 1 win32::black_pen))
           (oldpen (win32::select-object hdc hpen))
           (logbrush (fli:allocate-foreign-object :type
'win32::tag-logbrush))
           (hbrush win32::*black-brush*
;            (win32::create-brush-indirect
;                    (progn
;                      (setf (fli:foreign-slot-value logbrush
;'win32::lb-style) win32::bs_solid) ; hollow, ownerdraw, solid
;                      logbrush))
)
           (oldbrush (win32::select-object hdc hbrush)))
      (win32::set-rop2 hdc alu-seta)
      (win32:round-rect
       hdc
       x y (+ x width) (+ y height)
       width-ellipse height-ellipse)
      (win32::select-object hdc oldpen)
      (win32::delete-object hpen)
      (win32::select-object hdc oldbrush)
      ;(win32::delete-object hbrush)
      )))

(defun my-draw-icon (port hicon x y)
  (fli:with-dynamic-foreign-objects ()
    (let* ((hwnd (slot-value (capi-internals::representation port) 'win32:hwnd))
           (hdc (slot-value (capi-internals::representation port)
                            'capi-win32-lib::hdc))
           (hpen (win32::create-pen win32::ps_solid 0 win32::black_pen))
           (oldpen (win32::select-object hdc hpen))
           (logbrush (fli:allocate-foreign-object :type
'win32::tag-logbrush))
           (hbrush (win32::create-brush-indirect
                    (progn
                      (setf (fli:foreign-slot-value logbrush
                                                    'win32::lb-style) win32::bs_ownerdraw) ; hollow, ownerdraw, solid
                      logbrush))
            )
           (oldbrush (win32::select-object hdc hbrush)))
      (win32::set-rop2  hdc alu-seta)
      (win32:draw-icon hdc  x y hicon)
      (win32::select-object hdc oldpen)
      (win32::delete-object hpen)
      (win32::select-object hdc oldbrush)
      (win32::delete-object hbrush)
      )))
|#

#|
;;random stuff

(ww::shell-open nil "foo.wav"    )

;;;;;;;;;;;;;;;;;;;;; MCI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From: Arthur Lemmens <alemmens@xs4all.nl>
;; Rudimentary interface to mciSendString (Win32),
;; so we can play audio files.

(fli:define-c-typedef fli-hwnd (:unsigned :long))

(fli:define-c-typedef mcierror :long)

(fli:define-foreign-function (mci-send-string "mciSendString" :dbcs)
  ((lpszCommand :pointer)
   (lpszReturnString :pointer)
   (cchReturn (:unsigned :int))
   (hwndCallBack fli-hwnd))
  :result-type mcierror)

(defun mci-eval (command)
  ;; Simplest possible version, ignoring return strings and callbacks.
  (fli:with-foreign-string
    (c-command element-count byte-count :external-format :ascii)
    command
    (declare (ignore element-count byte-count))
    (mci-send-string c-command nil 0 0)))

(defun play-audio (filename)
  ;; Just a crude hack.
  (mci-eval "close sound wait") ;; Close previously played file.
  (mci-eval (format nil "open ~A alias sound wait" filename))
  (mci-eval "play sound from 0"))


;; Test it.

(play-audio "c:\\windows\\media\\tada.wav")



|#
