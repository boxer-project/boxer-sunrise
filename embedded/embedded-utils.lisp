(in-package :boxer)

(defun link_initial_box_to_node (gdnode gdrow)
  (putprop *initial-box* gdnode :gdnode)
  (putprop (first-inferior-row *initial-box*) gdrow :gdnode))

(defmethod fetch-godot-obj ((self box))
  (let* ((godot-box (getprop self :gdnode)))
    (unless godot-box
      (setf godot-box (gdboxer-make-box-internal self))
      (putprop self godot-box :gdnode))
    godot-box))

(defmethod fetch-godot-obj ((self row))
  (let* ((godot-row (getprop self :gdnode)))
    (unless godot-row
      (setf godot-row (gdboxer-make-row self))
      (putprop self godot-row :gdnode)
      (fill-in-godot-row godot-row self))
    godot-row))

;;; sgithens Prototyping wrapping box construction and "stuff"
(defmethod initialize-instance :after ((self box)  &rest init-plist)
  (format t "Just initialized a box! ~A doit: ~A data: ~A~%" self (doit-box? self) (data-box? self)))

(defmethod initialize-instance :after ((self row)  &rest init-plist)
  (fetch-godot-obj self))

(defmethod initialize-instance :after ((self name-row)  &rest init-plist)
  nil ;; (fetch-godot-obj self)
  )

;;; Cursor and point
(defmethod (setf bp-row) :after (value bp)
  (format t "setf bp-row: ~A ~A~%" value bp))

(defmethod (setf bp-cha-no) :after (value bp)
  (format t "setf bp-cha-no: ~A ~A~%" value bp))

;;;
;;; CHAS
;;;
(defmethod fast-chas-array-set-cha :after (chas-arr cha-no cha)
  (let ((row (chas-array-parent-row chas-arr)))
    (when row
      (let ((godot-row (fetch-godot-obj row)))
        (godot-insert-cha-signal godot-row cha cha-no)))))

(DEFMETHOD DELETE-CHA-AT-CHA-NO :after ((SELF ROW) CHA-NO)
  (let ((row (fetch-godot-obj self)))
    (when row
      (gdboxer-delete-cha-signal row cha-no))))

(defmethod delete-chas-between-cha-nos :after ((self row) strt-cha-no stop-cha-no)
  (let ((row (fetch-godot-obj self)))
    (when row
      (gdboxer-delete-chas-between-cha-nos-signal row strt-cha-no stop-cha-no))))

;;;
;;; ROWS
;;;

(defun godot-insert-cha-signal (godot-row cha cha-no)
  (when godot-row
    (format t "lisp insert cha: ~A ~A ~A~%" godot-row cha cha-no)
    (if (cha? cha)
      (gdboxer-insert-cha-signal godot-row (char-code cha) cha-no)
      (gdboxer-insert-cha-signal godot-row (fetch-godot-obj cha) cha-no))))

(defun fill-in-godot-row (godot-row row)
  "Fill in the godot-row with the contents of row, assuming nothing has been added to it yet."
  (let ((cha-no 0))
      (do-row-chas ((cha row))
        (godot-insert-cha-signal godot-row cha cha-no)
        (incf cha-no))))

(defmethod make-row :around (list)
  (let* ((new-row (call-next-method))
         (insert-pos 0)
         (godot-row (fetch-godot-obj new-row)))
    new-row))

(defmethod set-name :after ((self box) new-name-row)
  ;; TODO Occasionally new-name-row is a String, but I think that's literally just for the
  ;; WORLD name-row, but still revisit this.
  (let* ((godot-box (fetch-godot-obj self))
         (godot-name-row nil))
    (setf godot-name-row (gdboxer-get-name-row godot-box))
    (putprop new-name-row godot-name-row :gdnode)
    ;; Populate the new row, assuming the name has already been set in this name-row
    ;; TODO replace with fill-in-godot-row
    (let ((cha-no 0))
      (do-row-chas ((cha new-name-row))
        (godot-insert-cha-signal godot-name-row cha cha-no)
        (incf cha-no))))
        )

(defun gdboxer-make-box-internal (box)
  (let ((togo (gdboxer-make-box box)))
    (cond ((doit-box? box)
           (gdboxer-toggle-to-doit togo))
          ((data-box? box)
           (gdboxer-toggle-to-data togo))
          (t
           nil))
    togo))

(defmethod (setf superior-box) :after (sup-box row)
  ;; (format t "superior-box1.3: ~A ~A name-row: ~A~%" sup-box row (name-row? row))
  (when sup-box
    (let* ((godot-box (fetch-godot-obj sup-box))
           (godot-row nil))
      (setf godot-row (if (name-row? row)
                        (gdboxer-get-name-row godot-box)
                        (fetch-godot-obj row)))
      (unless godot-row
        (format t "setf superior-box with nil row.")
        (setf godot-row (gdboxer-make-row row))
        (putprop row godot-row :gdnode)
        (fill-in-godot-row godot-row row))
      ;; TODO TODO TODO, this null pointer check is because I'm still figuring out how to get the name row...
      ;; because we make this box and try to get the name-row before it's added to the scene treee...
      ;; so I can add the turtle boxes now, but the names of the rows aren't showing up
      (when (and (name-row? row) (not (ffi:null-pointer-p godot-row)))
        (putprop row godot-row :gdnode)
        (fill-in-godot-row godot-row row)
        (gdboxer-set-superior-box godot-row godot-box)))))

(defmethod (setf previous-row) :after (value row)
  (format t "previous-row: ~A ~A~%" value row))

(defmethod (setf next-row) :after (value row)
  ;; We're going to implement this for cases where the value is not nil, meaning an
  ;; actual row is getting put in next.
  (format t "next-row: ~A ~A~%" value row)
  (when value
    (let ((row-no (row-row-no (superior-box row) row))
          (godot-box (fetch-godot-obj (superior-box row)))
          (godot-row (fetch-godot-obj row))
          (godot-value (fetch-godot-obj value)))
      (format t "   rows no: ~A~%" row-no)
      (when row-no
        (godot-insert-row-at-row-no (superior-box row) godot-box value godot-value (1+ row-no))))))

(defmethod delete-row-at-row-no :after ((box box) pos &optional (check-closet t))
  ;; TODO sgithens Does this get a negative value or something for the closet row?
  (format t "TODO TODO TODO delete-row-at-row-no3.0: ~A pos: ~A check-closet: ~A~%" box pos check-closet)
  (let ((godot-box (fetch-godot-obj box)))
    (gdboxer-delete-row-at-row-no godot-box pos)));)

(defmethod delete-row ((self box) row &optional (check-closet t))
  (break "TODO TODO TODO delete-row: ~A pos: ~A check-closet: ~A~%" box pos check-closet))

(defmethod kill-box-contents :before ((self box) &optional (check-closet t))
  (let ((godot-box (fetch-godot-obj self)))
    (dotimes (i (length-in-rows self))
      (gdboxer-delete-row-at-row-no godot-box 0))))

(defun godot-insert-row-at-row-no (box godot-box row godot-row pos)
  (gdboxer-insert-row-at-row-no godot-box godot-row pos)
  godot-row)

;;;
;;; DISPLAY STYLES
;;;
(defun godot-update-display-style (disp-style)
  (let ((style (display-style-style disp-style))
        (box (display-style-parent disp-style))
        (godot-box nil))
    (when box
      (setf godot-box (fetch-godot-obj box))
      (gdboxer-set-graphics-mode-p godot-box (if (display-style-graphics-mode? disp-style) 1 0))
      (cond
        ((fixed-size? box)
        (gdboxer-set-property godot-box "display_style" 3))
        ((eq style :normal)
        (gdboxer-set-property godot-box "display_style" 2))
        ((eq style :shrunk)
        (gdboxer-set-property godot-box "display_style" 1))
        ((eq style :supershrunk)
        (gdboxer-set-property godot-box "display_style" 0))
        (t
        (error "Unknown godot-update-display-style-style: ~A" style))))))

(defmethod (setf display-style-style) :after (new-style dis-style)
  (godot-update-display-style dis-style))

(defmethod (setf display-style-graphics-mode?) :after (new-graphics-mode dis-style)
  (godot-update-display-style dis-style))

;;;
;;; BOXES
;;;

(defmethod display-style-list :before ((self box))
  (setf (display-style-parent (slot-value self 'display-style-list)) self))

(defmethod (setf display-style-list) :after (new-style box)
  (godot-update-display-style new-style))

(defmethod set-type :after ((self box) new-type)
  (let ((godot-box (fetch-godot-obj self)))
    (when godot-box
      (cond ((equalp (symbol-name new-type) "DOIT-BOX")
           (gdboxer-toggle-to-doit godot-box))
          ((equalp (symbol-name new-type) "DATA-BOX")
           (gdboxer-toggle-to-data godot-box))
          (t
           nil)))))

;; For some reasion it's very important that this one be :before
(defmethod (setf first-inferior-row) :before (row box)
  (when (and row box)
    (let ((godot-row (fetch-godot-obj row))
          (godot-box (fetch-godot-obj box)))
      (godot-insert-row-at-row-no box godot-box row godot-row 0))))

(defmethod (setf superior-row) :after (value box)
  (format t "superior-row: ~A ~A~%" value box))

(defmethod (setf name) :after (value box)
  (format t "name: ~A ~A~%" value box))

(defmethod (setf display-style-list) :after (ds box)
  (format t "setf display-style-list: ~A ~A~%" ds box)
  (setf (display-style-parent ds) box)
  (godot-update-display-style ds))

(defmethod insert-row-at-row-no :after ((box box) row row-no
                                       &optional (check-closet t))
  "This could either be a brand new row (which doens't have a gdnode) or an existing
   row that just needs to be moved to a different place."
  (format t "insert-row-at-row-no box: ~A row: ~A row-no: ~A~%" box row row-no )
  (let ((godot-row (fetch-godot-obj row))
        (godot-box (fetch-godot-obj box)))
      (progn
        (when (and godot-box (not (eq row-no (row-row-no box row))))
          ;; I'm not sure this putprop is really necessary??
          (putprop row (godot-insert-row-at-row-no box godot-box row godot-row row-no) :gdnode)))))

(defmethod insert-row-before-row :before ((box box) row before-row &optional (check-closet t))
  (break "Wraping insert-row-before-row: row: ~A" row))

(defmethod insert-row-after-row :after ((box box) row after-row &optional (check-closet t))
  (let ((after-row-no (row-row-no box after-row))
        (godot-row (fetch-godot-obj row))
        (godot-box (fetch-godot-obj box)))
    (godot-insert-row-at-row-no box godot-box row godot-row (1+ after-row-no))))

;;;
;;; GRAPHICS-SHEETS
;;;
(defmethod clear-box :after ((self box) &key (bitmap-p t) (graphics-list-p t))
  (gdboxer-clear-box (fetch-godot-obj self) bitmap-p graphics-list-p))

(defun godot-update-graphics-sheet (box sheet)
  (let* (;(box (graphics-sheet-superior-box sheet))
         (godot-box (fetch-godot-obj box))
         (pixmap (graphics-sheet-bit-array sheet)))
    ;; draw-wid draw-hei
    (gdboxer-set-graphics-sheet-draw-dims godot-box (graphics-sheet-draw-wid sheet) (graphics-sheet-draw-hei sheet))

    ;; bit-array
    (when pixmap
      (gdboxer-set-graphics-sheet-bit-array godot-box (ogl-pixmap-width pixmap) (ogl-pixmap-height pixmap) (ogl-pixmap-texture pixmap)))

    ;; draw-mode

    ;; graphics-command-list
    (let ((gl (graphics-sheet-graphics-list sheet)))
      (when gl
        (do-vector-contents (command gl)
          (when (eq 35 (aref command 0))
            (gdboxer-push-graphics-command godot-box (aref command 0) (aref command 1) (aref command 2) (aref command 3) (aref command 4) nil)))))

    ;; background
    (let ((value (graphics-sheet-background sheet)))
      (when value
        (gdboxer-set-graphics-sheet-background (fetch-godot-obj box) (aref value 1) (aref value 2) (aref value 3) (aref value 4))))
  ))

(defmethod (setf graphics-sheet-background) :after (value sheet)
  (let ((box (graphics-sheet-superior-box sheet)))
    (when box
      (gdboxer-set-graphics-sheet-background (fetch-godot-obj box) (aref value 1) (aref value 2) (aref value 3) (aref value 4)))))

;; (defmethod (setf graphics-sheet-bit-array) :after (pixmap sheet)
;;   (format t "~%Set graphics sheet BIT ARRAY~%")
;;   (let* ((box (graphics-sheet-superior-box sheet))
;;          (godot-box (getprop box :gdnode)))
;;     (format t "~%SETTING GRAPHICS_SHEET_HACKGROUND 2.3: ~A ~%" value)
;;     (gdboxer-set-graphics-sheet-bit-array godot-box (ogl-pixmap-width pixmap) (ogl-pixmap-height pixmap)
;;       (ogl-pixmap-data pixmap)))
;; )

(defmethod (setf graphics-info) :after ((sheet graphics-sheet) box)
  (let* ((godot-box (fetch-godot-obj box)))
    (gdboxer-set-property godot-box "flipped_box_type" 1)
    (godot-update-graphics-sheet box sheet)))

;;;
;;; GRAPHICS-OBJECTS
;;;

(defmethod (setf graphics-info) :after ((sheet graphics-object) box)
  (format t "setf graphics-info with Turtle type object~%")
  (let* ((godot-box (fetch-godot-obj box)))
    (gdboxer-set-property godot-box "flipped_box_type" 2)))

;;
;; Hacking
;;

;; Sometimes nil screen-boxes end up in the mix...
(DEFMETHOD SUPERIOR? (self ANOTHER-BOX)
  "is the arg a superior of the box ?"
  nil)

; Hacking in vector support, TODO, put back in main and recompile it
(defun make-name-row (list &optional (cached-name nil))
  (let* ((new-row (make-instance 'name-row :cached-name cached-name))
         (ca (chas-array new-row))
         (idx 0)
         (length (length list)))
    (dolist (item list)
      (cond ((numberp item)
             (fast-string-into-chas-array (format nil "~a" item) ca))
        ((stringp item)
         (fast-string-into-chas-array item ca))
        ((symbolp item)
         (fast-string-into-chas-array (symbol-name item) ca))
        ((box? item) (error "You must be losing to put ~A here" item))
        ((vectorp item)
         (fast-string-into-chas-array (map 'string #'(lambda (x) x) item) ca))
        (t (error "Don't know how to make a row out of ~S" item)))
      (incf& idx)
      (unless (=& idx length)
        (fast-chas-array-append-cha ca #\space)))
    new-row))

(defun setup-standard-colors ()
;; (def-redisplay-initialization
  ;; set up some standard colors for sprites
  (dolist (color *standard-colors*)
      (let ((colorbox (boxer::make-color-internal
                       (cadr color) (caddr color) (cadddr color)))
            (variable-name (boxer::intern-in-bu-package
                            (string-upcase (car color)))))
        (boxer-eval::boxer-toplevel-set variable-name colorbox))))

(defvar *next-event* #(0 0 0 0 0 0 0 0 0 0 0 0))

(defun ecl-boxer-command-loop-internal ()
  ;; initialization
  ;; This is having some sort of error due to parent rows not being able to be added and such things
  ;; (setup-standard-colors)

  ;; (flush-input)
  (loop
    ;; (when *clicked-startup-file*
    ;;   (queue-event *clicked-startup-file*)
    ;;   (setf *clicked-startup-file* nil))
    (catch 'boxer::boxer-editor-top-level
      (let ((input (fetch-event-from-queue *next-event*)))
        (cond ((null input)
               nil)
              ((equal 0 (aref input 0))
               nil)
              ((equal 1 (aref input 0))
                (format t "Lisp: Handling keyboard input: ~A,  ~A~%" (aref input 1) (aref input 2))
                (godot-handle-boxer-input (aref input 1) (aref input 2)))
              ((equal 2 (aref input 0))
                (format t "Lisp: Handling Mouse input: ~A~%" input)
                (godot-handle-mouse-input (aref input 1) (aref input 2) (aref input 3) (aref input 4)
                                          (aref input 5) (aref input 6)))
              ((equal 3 (aref input 0))
               (format t "Lisp: Handlign function call: ~A~%" input)
               (funcall (find-symbol (aref input 2) "BOXER") (aref input 3)))
              ((equal 4 (aref input 0))
               (format t "Lisp: Exiting~%")
               (return))
              ((gesture-spec-p input)
               ;; We are adding this gesture condition in addition to the key-event? because at some point
               ;; during a lispworks major version change, the ability to encode the modifier keys as part of
               ;; the reader char seems to have gone away.  By adding an option to push an entire gesture-spec
               ;; to the *boxer-eval-queue* we can just manually pick out the char-code and input bits.
               (let* ((data (gesture-spec-data input))
                      (charbits (gesture-spec-modifiers input)))
                 (handle-boxer-input data charbits (key-to-keep-shifted? data))))
              ((key-event? input)
               (handle-boxer-input (input-code input) (input-bits input)))
              ((mouse-event? input)
               (handle-boxer-input input))
              ((eq input :stop-and-quit)
               #+lispworks (capi:apply-in-pane-process *boxer-pane* #'(lambda ()
                (setf (boxer::active *boxer-pane*) nil)
                (setf boxer::*quit-boxer* t))))
              ((and (symbolp input) (not (null (symbol-function input))))
               (funcall input))
              ((and (consp input)
                    (or (functionp (car input))
                        (and (symbolp (car input))
                             (not (null (symbol-function (car input)))))))
               (apply (car input) (cdr input)))
              ((not (null boxer::*boxer-system-hacker*))
               (error "Unknown object, ~A, in event queue" input)))
        )
      )))

(defmethod append-graphics-command :after (gclist com-list)
  (format t "APPEND GRAPHICS COMMAND AFTER: ~A~%" com-list)
  (let* ((agent (graphics-command-list-agent %graphics-list))
         (graphics-box (assoc-graphics-box agent))
         (godot-box (fetch-godot-obj graphics-box)))
    ;; This is not very elegent, however the longest graphics command is never more than 6 params
    (gdboxer-push-graphics-command godot-box (nth 0 com-list) (nth 1 com-list) (nth 2 com-list) (nth 3 com-list) (nth 4 com-list) (nth 5 com-list))))

(defun repaint-in-eval (&optional force?)
  nil)

(defun STRING-HEI (font-no)
  ;; TODO TODO TODO
  16)

(defun string-wid (font-no string)
  ;; TODO TODO TODO
  10)

(defun godot-open-file (path)
  (format t "~%godot-open-file: ~A~%" path)
  (com-open-box-file path))

(defun bw::SHIFT-KEY-PRESSED? () nil)

(defun godot-update-point-location ()
  "Update the location of point in Godot, using the current Boxer *point*."
  (let ((godot-row (fetch-godot-obj (bp-row *point*))))
    (when godot-row
      (gdboxer-point-location (fetch-godot-obj (bp-row *point*)) (bp-cha-no *point*)))))

(defun godot-handle-boxer-input (data bits)
  (format t "~%godot-handle-boxer-input3: ~A bits: ~A~%" data bits)
  (cond
    ((eq data -1)
     (handle-boxer-input :up bits))
    ((eq data -2)
     (handle-boxer-input :down bits))
    ((eq data -3)
     (handle-boxer-input :left bits))
    ((eq data -4)
     (handle-boxer-input :right bits))
    (t
     (handle-boxer-input data bits)))

  (format t "~%>>POINT2.0: ~A : ~A : ~A : ~A~%" *point* (bp-row *point*)
    (fetch-godot-obj (bp-row *point*)) (bp-cha-no *point*))

  (godot-update-point-location)

  (format t "~%Current Doc: ~%~A~%" (boxer::textify-thing boxer::*initial-box*)))

(defun godot-handle-mouse-input (action-code row pos click bits area-code)
  ;; Action encoding
  ;; Action is: 0 - press/MOUSE-DOWN 1 - click/MOUSE-CLICK 2 - release/MOUSE-UP 3 - double click/ MOUSE-DOUBLE-CLICK

  ;; Area encoding
  ;; 0 :INSIDE
  ;; :OUTSIDE :NAME :SCROLL-BAR :TYPE :BOTTOM-RIGHT :BOTTOM-LEFT
  ;; :TOP-RIGHT :TOP-LEFT
  (let ((click-bp (MAKE-INITIALIZED-BP :fixed row pos))
        (action (case action-code
                  (0 'BOXER-USER::MOUSE-DOWN)
                  (1 'BOXER-USER::MOUSE-CLICK)))
        (area (case area-code
                (0 :inside)
                (otherwise nil))))
    ;; (handle-boxer-mouse-click 'BOXER-USER::MOUSE-DOWN *boxer-pane* 0 0 click-bp t 6 0 nil)
    (handle-boxer-mouse-click action *boxer-pane* 0 0 click-bp t 6 0 area))
  (godot-update-point-location))

(defmethod allocate-screen-obj-for-use-in ((self actual-obj-subclass) use-in-screen-box)
  nil); oof

(DEFUN BOX-SCREEN-POINT-IS-IN ()	  ;returns the box that the screen part of
      ;;  (SCREEN-OBJ-ACTUAL-OBJ (POINT-SCREEN-BOX))
  (point-box)
       )	;*point* refers to

(DEFMETHOD SUPERIOR-SCREEN-BOX (obj)
          ;;  (SCREEN-BOX SCREEN-OBJ)
  nil)

(DEFMETHOD DISPLAYED-SCREEN-OBJS (obj
                                  &OPTIONAL (WINDOW *BOXER-PANE*))
  '())

(DEFUN OUTERMOST-BOX (&OPTIONAL (WINDOW *BOXER-PANE*))
  *initial-box*
      ;;  (SCREEN-OBJ-ACTUAL-OBJ (boxer-window::outermost-screen-box WINDOW))
       )

;;;
;;; Draw high
;;;

(defun %set-pen-size (v)
  ;; (setf (boxgl-device-pen-size bw::*boxgl-device*) v)
  nil)

(defun set-pen-color (color)
  ;; (%set-pen-color color)
  nil)

;;;
;;; Hacking pixmaps
;;;

(defclass ogl-pixmap ()
  ((width             :initarg :width             :initform 0   :accessor ogl-pixmap-width)
   (height            :initarg :height            :initform 0   :accessor ogl-pixmap-height)
   ;; In Godot we're going to make this texture be a PackedByteArray
   (texture           :initarg :texture           :accessor ogl-pixmap-texture)
   (update-texture-p  :initarg :update-texture-p  :initform nil :accessor ogl-pixmap-update-texture-p
    :documentation "If this is true, then changes have been made to the pixel data and should be
                    redrawn to the GL texture.")
   (data              :initarg :data              :initform nil :accessor ogl-pixmap-data)
   (depth             :initarg :depth             :initform 32  :accessor ogl-pixmap-depth)))

(defgeneric ogl-pixmap-p (x) (:method (x) nil) (:method ((x ogl-pixmap)) t))

(defun make-ogl-pixmap (width height &key (texture 0))
  (cond ((and (integerp width)  (not (minusp width))
              (integerp height) (not (minusp height)))
        ;;  (format t "Make OGL Pixmap size-of2: ~A  count: ~A~%" (ffi:size-of-foreign-type :unsigned-int) (* width height))
         (make-instance 'ogl-pixmap :width width :height height
                        :data (ffi:allocate-foreign-object :unsigned-int (* width height))
                        :texture (gdboxer-make-packed-byte-array (* width height)))
        )
        (t (error "Pixmap dimensions, (~S, ~S) must be non-negative integers"
                  width height))))

(defun ogl-free-pixmap (pixmap)
  (when (ogl-pixmap-p pixmap)
    ;; TODO TODO TODO
    ;; (cffi:foreign-free (ogl-pixmap-data pixmap))
    ))

(defun pixmap-pixel (pixmap x y)
  (let* ((data (ogl-pixmap-data pixmap))
         (pwid (ogl-pixmap-width pixmap))
         (phei (ogl-pixmap-height pixmap))
         (ogl-y (- phei y 1)))
    (ffi::%foreign-data-ref data (+ x (* ogl-y pwid)) :unsigned-int)
    ))

(defun set-pixmap-pixel (pixmap x y newpixel)
  (let* ((data (ogl-pixmap-data pixmap))
         (pwid (ogl-pixmap-width pixmap))
         (phei (ogl-pixmap-height pixmap))
         (ogl-y (- phei y 1)))
    (ffi::%foreign-data-set data (+ x (* ogl-y pwid)) :unsigned-int newpixel)
    ;; (format t " pix: ~A ~A " (+ x (* ogl-y pwid)) newpixel)
    (gdboxer-packed-byte-array-set (ogl-pixmap-texture pixmap) (+ x (* ogl-y pwid)) newpixel))
  (setf (ogl-pixmap-update-texture-p pixmap) t))

;; (defsetf pixmap-pixel set-pixmap-pixel)

(defmethod (setf pixmap-pixel) (value pixmap x y)
  (set-pixmap-pixel pixmap x y value))

(defvar *gl-rgba-rev-alpha-byte* (byte 8 24))
(defvar *gl-rgba-rev-blue-byte* (byte 8 16))
(defvar *gl-rgba-rev-green-byte* (byte 8 8))
(defvar *gl-rgba-rev-red-byte* (byte 8 0))

;; NOTE: this must match the format in *pixmap-data-type* and *pixmap-data-format*
(defun make-offscreen-pixel (red green blue &optional (alpha 255))
  (dpb alpha *gl-rgba-rev-alpha-byte*
       (dpb blue *gl-rgba-rev-blue-byte*
            (dpb green *gl-rgba-rev-green-byte* red))))

;;;
;;; XREF
;;;
(defstruct xref
  (pathname nil)
  (icon-cache nil)
  (mime-type nil)
  (active-info nil))

(defun set-xref-boxtop-info (box &optional creator ftype)
  ;; TODO TODO TODO
)

;;;
;;; TODO Why aren't these somewhere in core??
;;;

;; (defvar BOXER-WINDOW::*CLICKED-STARTUP-FILE* nil)

(defun repaint (&optional just-windows?)
  nil)

(defmethod fixed-size? ((self box))
  (let ((ds (display-style-list self)))
    (and (not (member *outermost-screen-box* (screen-objs self)))
         (numberp (display-style-fixed-wid ds))
         (numberp (display-style-fixed-hei ds)))))

(defmethod display-style ((box box))
  (display-style-style (display-style-list box)))


(defmethod set-display-style-list ((box box) new-style)
  (setf (display-style-list box) new-style))

(defmethod set-border-style ((self box) new-style)
  (let ((display-style (display-style-list self)))
    (setf (display-style-border-style display-style) new-style)))

(defmethod border-style ((self port-box))
  (let ((target (slot-value self 'ports)))
    (unless (null target)
      (display-style-border-style (display-style-list target)))))

(defmethod shrunken? (oof) nil)

(defmethod shrunken? ((self box))
  (let ((style (display-style-style (display-style-list self))))
    (or (eq style :shrunk) (eq style :supershrunk))))

(DEFMETHOD SHRINK ((SELF BOX))
           (SET-DISPLAY-STYLE SELF ':SHRUNK)
           (MODIFIED SELF))

(DEFMETHOD UNSHRINK ((SELF BOX))
           (SET-DISPLAY-STYLE SELF ':NORMAL)
           (MODIFIED SELF))

(defmethod supershrink ((self box))
  (set-display-style self ':supershrunk)
  (modified self))

(defmethod set-display-style ((self box) new-value)
  ;; sgithens hack, make sure this gets set on new boxes...
  (setf (display-style-parent (display-style-list self)) self)

  (setf (display-style-style (display-style-list self)) new-value))

(defmethod clear-graphics-canvas (obj)
  "")

;; Traces
;; (trace boxer-eval::boxer-eval)
;; (trace boxer-eval::make-error-result)
;; ;; (trace boxer::bin-load-next-command)
;; (trace boxer::handle-boxer-mouse-click)
;; (trace boxer::com-open-box-file)

;; (trace boxer::insert-row-at-row-no)
;; (trace (setf boxer::next-row))
;; (trace (setf boxer::first-inferior-row))
;; (trace boxer::godot-insert-row-at-row-no)
;; (trace boxer::gdboxer-insert-row-at-row-no)
;; (trace boxer::kill-box-contents)
;; (trace boxer::insert-row-after-row)
