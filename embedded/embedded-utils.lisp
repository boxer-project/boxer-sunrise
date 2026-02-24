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

(defmethod fetch-godot-obj ((self turtle))
  (let* ((godot-turtle (getprop self :gdnode)))
    (unless godot-turtle
      (setf godot-turtle (gdboxer-make-turtle self))
      (putprop self godot-turtle :gdnode))
    godot-turtle))

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
;;; Filling in screen-objs
;;;

(defun print-box-tree (&optional (obj *initial-box*) (depth 0))
  (cond
    ((box? obj)
     (format t "~%~V,,,' A+ Box: ~A Style: ~A Screen-objs: #~A ~A"
       (* 2 depth) "" (name obj) (display-style-style (display-style-list obj))
       (length (screen-objs obj)) (screen-objs obj))
     (do-box-rows ((row obj))
      (print-box-tree row (1+ depth))
     ))
    ((row? obj)
     (format t "~%~V,,,' A- Row: ~A Screen-objs: #~A ~A"
       (* 2 depth) "" obj (length (screen-objs obj)) (screen-objs obj))
     (do-row-chas ((cha obj))
      (when (box? cha)
        (print-box-tree cha (1+ depth)))))))

;; (defun print-screen-box-tree )

(defun fill-in-screen-objs (&optional (obj *initial-box*))
  (cond
   ((box? obj)
    (do-box-rows ((row obj))
      (allocate-screen-obj-for-use-in row (car (screen-objs obj)))
      (fill-in-screen-objs row)))
   ((row? obj)
    (do-row-chas ((cha obj))
      (when (box? cha)
        (fill-in-screen-objs cha))))))

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
      (godot-call-main "_on_gd_boxer_boxer_delete_cha" row cha-no))))

(defmethod delete-chas-between-cha-nos :after ((self row) strt-cha-no stop-cha-no)
  (let ((row (fetch-godot-obj self)))
    (when row
      (godot-call-main "_on_gd_boxer_boxer_delete_chas_between_cha_nos" row strt-cha-no stop-cha-no))))

;;;
;;; ROWS
;;;

(defun godot-insert-cha-signal (godot-row cha cha-no)
  (when godot-row
    ;; (format t "lisp insert cha: ~A ~A ~A~%" godot-row cha cha-no)
    (if (cha? cha)
      (godot-call-main "_on_gd_boxer_boxer_insert_cha" godot-row (char-code cha) cha-no)
      (godot-call-main "_on_gd_boxer_boxer_insert_cha" godot-row (fetch-godot-obj cha) cha-no))))

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
  (format t "~%set-name: self: ~A new-name-row: ~A" self new-name-row)
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
        (incf cha-no)))))

(defun gdboxer-make-box-internal (box)
  (let ((togo (gdboxer-make-box box)))
    (cond ((doit-box? box)
           (godot-call togo "toggle_to_doit"))
          ((data-box? box)
           (godot-call togo "toggle_to_data"))
          (t
           nil))
    togo))

(defmethod (setf superior-box) :after (sup-box row)
  (format t "superior-box1.3: ~A ~A name-row?: ~A~%" sup-box row (name-row? row))
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
      (cond
        ((and (name-row? row) (not (ffi:null-pointer-p godot-row)))
         (putprop row godot-row :gdnode)
         (fill-in-godot-row godot-row row)
         (godot-call godot-row "set_superior_box" godot-box))
        ((name-row? row) ;; must be null still, we'll set the special godot property to fill it in later
         (format t "~% I hope the name is just a string: ~A" (name sup-box))
         (gdboxer-set-property godot-box "queued_name" (coerce (name sup-box) 'string))
         (gdboxer-set-property godot-box "queued_name_row_boxerref" godot-row))))))

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
    (godot-call godot-box "delete_row_at_row_no" pos)))

(defmethod delete-row ((self box) row &optional (check-closet t))
  (break "TODO TODO TODO delete-row: ~A pos: ~A check-closet: ~A~%" box pos check-closet))

(defmethod kill-box-contents :before ((self box) &optional (check-closet t))
  (let ((godot-box (fetch-godot-obj self)))
    (dotimes (i (length-in-rows self))
      (godot-call godot-box "delete_row_at_row_no" 0))))

(defun godot-insert-row-at-row-no (box godot-box row godot-row pos)
  (godot-call godot-box "insert_row_at_row_no" godot-row pos)
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
           (godot-call godot-box "toggle_to_doit"))
          ((equalp (symbol-name new-type) "DATA-BOX")
           (godot-call godot-box "toggle_to_data"))
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

(defmethod insert-row-after-row :after ((box box) row after-row &optional (check-closet t))
  (let ((after-row-no (row-row-no box after-row))
        (godot-row (fetch-godot-obj row))
        (godot-box (fetch-godot-obj box)))
    (godot-insert-row-at-row-no box godot-box row godot-row (1+ after-row-no))))

;;;
;;; GRAPHICS-SHEETS
;;;
(defun gboolean (value)
  (if value 1 0))

(defun godot-call (godot-obj func_name &rest args)
  (gdboxer-call-godot (coerce (append (list godot-obj func_name) args) 'vector)))

(defun godot-call-main (func_name &rest args)
  (gdboxer-call-godot-main (coerce (append (list func_name) args) 'vector)))


(defmethod clear-box :after ((self box) &key (bitmap-p t) (graphics-list-p t))
  (godot-call (fetch-godot-obj self) "clear_box" (gboolean bitmap-p) (gboolean graphics-list-p)))

(defun godot-init-graphics-sheet (box)
  ;; For use in on-ready. If this is a graphics box, then update it's sheet
  (when (graphics-box? box)
    (godot-update-graphics-sheet box (graphics-info box)))
  (when (sprite-box? box)
    (godot-update-graphics-object box)))

(defun godot-update-graphics-object (box)
  (let* ((godot-box (fetch-godot-obj box)))
    (gdboxer-set-property godot-box "flipped_box_type" 2)))

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
        (do-vector-contents (com gl)
          (let ((command (coerce com 'list)))
            (godot-call godot-box "push_graphics_command" (nth 0 command) (nth 1 command) (nth 2 command) (nth 3 command) (nth 4 command) (nth 5 command))))))

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

(defun apply-graphics-list (godot-obj gl)
  ;; (let ((gl (graphics-sheet-graphics-list sheet)))
  (when gl
    (do-vector-contents (com gl)
      (let ((command (coerce com 'list)))
        (godot-call godot-obj "push_graphics_command"
          (nth 0 command) (nth 1 command) (nth 2 command) (nth 3 command) (nth 4 command) (nth 5 command))))))
            ;; )

;; Adding removing sprites from a box
(defmethod add-graphics-object :after ((self box) turtle)
  (format t "add-graphics-object box: ~A graphics-obj: ~A~%" self turtle)
  (format t "   the shape2: ~A~%" (box-interface-value (slot-value turtle 'shape)))
  (let ((godot-box    (fetch-godot-obj self))
        (godot-turtle (fetch-godot-obj turtle)))
    (godot-call godot-box "add_turtle" godot-turtle)

    ;; shape
    (apply-graphics-list godot-turtle (box-interface-value (slot-value turtle 'shape)))
    ;; position
    (gdboxer-set-property godot-turtle "position_x" (box-interface-value (slot-value turtle 'x-position)))
    (gdboxer-set-property godot-turtle "position_y" (box-interface-value (slot-value turtle 'y-position)))
    ;; heading
    (gdboxer-set-property godot-turtle "rotation_degrees" (box-interface-value (slot-value turtle 'heading)))
    ;; size
    (gdboxer-set-property godot-turtle "locked_scale" (box-interface-value (slot-value turtle 'sprite-size)))
    ;; shown?
    (gdboxer-set-property godot-turtle "visible" (gboolean (box-interface-value (slot-value turtle 'shown?))))

    ))

(defmethod remove-graphics-object :after ((self box) old-object)
  (format t "remove-graphics-object box: ~A graphics-obj: ~A" self old-object))

(defmethod (setf graphics-info) :after ((sheet graphics-object) box)
  (format t "setf graphics-info with Turtle type object~%")
  (let* ((godot-box (fetch-godot-obj box)))
    (gdboxer-set-property godot-box "flipped_box_type" 2)))

(defmethod move-to :after ((self graphics-object) x-dest y-dest
                    &optional dont-update-box)
  (let* ((godot-turtle (fetch-godot-obj self)))
    (gdboxer-set-property godot-turtle "position_x" x-dest)
    (gdboxer-set-property godot-turtle "position_y" y-dest)))

(defmethod turn-to :after ((self turtle) new-heading &optional dont-update-box)
  (format t "turn-to: new-heading: ~A~%" new-heading)
  (let* ((godot-turtle (fetch-godot-obj self)))
    (gdboxer-set-property godot-turtle "rotation_degrees" new-heading)))

(defmethod set-sprite-size ((self turtle) new-size &optional dont-update-box)
  (let* ((godot-turtle (fetch-godot-obj self)))
    (gdboxer-set-property godot-turtle "locked_scale" new-size)))

(defmethod set-shown? ((self graphics-cursor) new-value
                                              &optional dont-update-box (explicit t))
  (let* ((godot-turtle (fetch-godot-obj self)))
    (gdboxer-set-property godot-turtle "visible" (gboolean new-value))))

;;;
;;; Clipboard Cut/Paste
;;;

;; TODO partially duped from clipboard.lisp/paste-text
(defun godot-paste-text (text)
  (unless (null text)
    (dotimes (i (length text))
      (let ((char (aref text i)))
        (if (member char '(#\Newline #\Return #\Linefeed))
            (boxer::insert-row boxer::*point*
                                (boxer::make-initialized-row) :moving)
            (boxer::insert-cha boxer::*point* char :moving))))
    (godot-update-point-location)))


;;
;; Hacking
;;

(defmethod row-row-no (self row)
 ;; if row is null...
 0)

;; Both of these need to be overridden to avoid calling non embedded code. This
(defmethod ensure-row-is-displayed ((self row) screen-box
                                              &optional (direction -1) scroll-anyway)
  nil)

(defmethod ensure-row-is-displayed ((row name-row) screen-box
                                                   &optional (direction -1) scroll-anyway)
  nil)

(defmethod SCROLL-TO-ACTUAL-ROW (obj) nil)

;; Sometimes nil screen-boxes end up in the mix...
(DEFMETHOD SUPERIOR? (self ANOTHER-BOX)
  "is the arg a superior of the box ?"
  nil)

(defmethod CIRCULAR-PORT? (self &optional ignore) nil)

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
               (apply (find-symbol (aref input 2) "BOXER")
                 (loop as i from 3 to (+ 2 (aref input 1)) collect (aref input i))))
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
    (godot-call godot-box "push_graphics_command" (nth 0 com-list) (nth 1 com-list) (nth 2 com-list) (nth 3 com-list) (nth 4 com-list) (nth 5 com-list))))

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
    (format t "~%Update point location: bp-row: ~A godot-obj: ~A" (bp-row *point*) godot-row)
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
  ;; (format t "~%Current Doc: ~%~A~%" (boxer::textify-thing boxer::*initial-box*))
  (format t "~%Filling in screen objs3")
  (fill-in-screen-objs)
  (print-box-tree)
  (godot-update-point-location))

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
                  (1 'BOXER-USER::MOUSE-CLICK)
                  (3 'BOXER-USER::MOUSE-DOUBLE-CLICK)))
        (area (case area-code
                (0 :inside)
                (otherwise nil))))
    ;; (handle-boxer-mouse-click 'BOXER-USER::MOUSE-DOWN *boxer-pane* 0 0 click-bp t 6 0 nil)
    (handle-boxer-mouse-click action *boxer-pane* 0 0 click-bp t 6 0 area))
  (godot-update-point-location))

(DEFUN BOX-SCREEN-POINT-IS-IN ()	  ;returns the box that the screen part of
      ;;  (SCREEN-OBJ-ACTUAL-OBJ (POINT-SCREEN-BOX))
  (point-box)
       )	;*point* refers to

(DEFMETHOD DISPLAYED-SCREEN-OBJS (obj
                                  &OPTIONAL (WINDOW *BOXER-PANE*))
  '())

(DEFUN OUTERMOST-BOX (&OPTIONAL (WINDOW *BOXER-PANE*))
  *initial-box*
      ;;  (SCREEN-OBJ-ACTUAL-OBJ (boxer-window::outermost-screen-box WINDOW))
       )

;;;
;;; Mouse Commands
;;;

(defun shrink-box (box)
  ;; TODO this should eventually take screen-box once that's hooked up correctly
  (let ((bp (make-bp :fixed)))
    (setf (bp-row bp) (first-inferior-row box)
          (bp-cha-no bp) 0
          (bp-screen-box bp) (car (screen-objs box)))
    (mouse-tl-corner-collapse-box bp)))

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
;;; TODO Hack overrides
;;;

(defun repaint (&optional just-windows?)
  nil)

(defmethod shrunken? (oof) nil)

(defmethod set-display-style ((self box) new-value)
  ;; sgithens hack, make sure this gets set on new boxes...
  (setf (display-style-parent (display-style-list self)) self)

  (setf (display-style-style (display-style-list self)) new-value))

(defmethod clear-graphics-canvas (obj)
  ;; TODO this needs to be overridden for each implementation
  nil)

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
