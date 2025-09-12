(in-package :boxer)

(defun wowwow ()
  (print "So much wowowow"))

;;; sgithens Prototyping wrapping box construction and "stuff"
(defmethod initialize-instance :after ((self box)  &rest init-plist)
  (format t "~%Just initialized a box! ~A" self)
)

(defmethod initialize-instance :around ((self data-box)  &rest init-plist)
  (format t "~% Just initialized a DATA box!" )
  (if (next-method-p)
    (call-next-method))
)

(defmethod initialize-instance :after ((self doit-box)  &rest init-plist)
  (format t "~%Just initialized a DOIT box! ~A" self)
)

(defmethod initialize-instance :after ((self row)  &rest init-plist)
  (let ((godot-node (getprop self :gnode))
        ;; (sup-box (superior-box self))
        )

    ;; (format t "~%Just initialized a row! ~A sup-box: ~A godot-node: ~A" self sup-box godot-node)
    (putprop self (gdboxer-make-row) :gdnode)
    (format t "~%. Finished godot making row")
  )
)

;;; Cursor and point
(defmethod (setf bp-row) :after (value bp)
  (format t "~%setf bp-row: ~A ~A" value bp)
)

(defmethod (setf bp-cha-no) :after (value bp)
  (format t "~%setf bp-cha-no: ~A ~A" value bp)
)

;;; CHAS

(defmethod change-cha-at-cha-no :after ((self row) n new-cha)
  (format t "~%change-cha-at-cha-no ~A ~A ~A" self n new-cha))

(DEFMETHOD INSERT-CHA-AT-CHA-NO :after ((SELF ROW) CHA CHA-NO)
  (format t "~%INSERT-CHA-AT-CHA-NO 12.3 ~A ~A ~A" self cha cha-no)
  ;; check and see if this row has a :gdnode prop
  (let ((row (getprop self :gdnode)))
    (when row
      (if (cha? cha)
        (gdboxer-insert-cha-signal row (char-code cha) cha-no)
        (gdboxer-insert-cha-signal row (getprop cha :gdnode) cha-no)
      )
      )))

(defmethod insert-row-chas-at-cha-no :around ((self row) row cha-no
                                      &optional
                                        (from-start-cha-no 0)
                                        (from-stop-cha-no (length-in-chas row))
                                        )
  (format t "~%around: insert-row-chas-at-cha-no ~A copy-row: ~A cha-no: ~A start: ~A end: ~A"
      self row cha-no from-start-cha-no from-stop-cha-no)

  (let ((insert-pos cha-no)
        ;; (cur-no from-start-cha-no)
        (godot-row (getprop self :gdnode)))
    (do-row-chas ((c row :start from-start-cha-no :stop from-stop-cha-no))
    ;; TODO in boxer we'll need to adjust each box node and move it around...
        (cond ((cha? c)
               (gdboxer-insert-cha-signal godot-row (char-code c) insert-pos))
         (t
          ;; (set-superior-row c self)
          ;; (insert-self-action c)
          (format t "~%Does this box char have a godotbox4.4?: ~A" (getprop c :gdnode))
          (gdboxer-insert-cha-signal godot-row (getprop c :gdnode) insert-pos)
         )
          )
      ;; (incf cur-no)
      (incf insert-pos)))
  (if (next-method-p)
    (call-next-method)))

(DEFMETHOD DELETE-CHA-AT-CHA-NO :after ((SELF ROW) CHA-NO)
  (format t "~%DELETE-CHA-AT-CHA-NO: ~A cha-no: ~A" self cha-no)
  (let ((row (getprop self :gdnode)))
    (when row
      (gdboxer-delete-cha-signal row cha-no)
      ;; (format t "~%This row has a Godot Row Node 2.a")
      )))

(defmethod delete-chas-between-cha-nos :after ((self row) strt-cha-no stop-cha-no)
  (format t "~%delete-chas-between-cha-nos: ~A strt-cha-no: ~A stop-cha-no: ~A" self strt-cha-no stop-cha-no)
  (let ((row (getprop self :gdnode)))
    (when row
      (gdboxer-delete-chas-between-cha-nos-signal row strt-cha-no stop-cha-no)
      ))
)

;;; ROWS

(defmethod make-row :around (list)
  (let* ((new-row (call-next-method))
         (insert-pos 0)
         (godot-row (getprop new-row :gdnode)))
    (format t "~%make-row :around - list: ~A row: ~A" list new-row)
    (do-row-chas ((c new-row))
        (cond ((cha? c)
               (gdboxer-insert-cha-signal godot-row (char-code c) insert-pos))
         (t
          (gdboxer-insert-cha-signal godot-row (getprop c :gdnode) insert-pos)
         )
          )
      (incf insert-pos))
    new-row))

(defmethod (setf superior-box) :after (sup-box row)
  (format t "~%superior-box: ~A ~A" sup-box row)
  (when sup-box
    (let ((godot-box (getprop sup-box :gdnode))
        (godot-row (getprop row :gdnode)))
    (unless godot-box
      (format t "~%This superior-box does not have a godot-box: row: ~A~%" godot-row)
      (setf godot-box (gdboxer-make-box)))
    ;; (gdboxer-set-superior-box godot-row godot-box)
  )))

(defmethod (setf previous-row) :after (value row)
  (format t "~%previous-row: ~A ~A" value row))

(defmethod (setf next-row) :after (value row)
  (format t "~%next-row: ~A ~A" value row))

(defmethod (setf chas-array) :after (value row)
  (format t "~%chas-array: ~A ~A" value row))

(defmethod delete-row-at-row-no :after ((box box) pos &optional (check-closet t))
  ;; TODO sgithens Does this get a negative value or something for the closet row?
  (format t "~%TODO TODO TODO delete-row-at-row-no3.0: ~A pos: ~A check-closet: ~A" box pos check-closet)
  ;; (unless (null (next-row (first-inferior-row box)))
    (let ((godot-box (getprop box :gdnode)))
      (gdboxer-delete-row-at-row-no godot-box pos)));)

(defmethod delete-row ((self box) row &optional (check-closet t))
  (format t "~%TODO TODO TODO delete-row: ~A pos: ~A check-closet: ~A" box pos check-closet)
)

;;; BOXES

(defmethod (setf first-inferior-row) :after (row box)
  (let ((godot-row (getprop row :gdnode))
        (godot-box (getprop box :gdnode)))
    (format t "~%first-inferior-row: ~A ~A" row box)
    (unless godot-box
      (setf godot-box (gdboxer-make-box))
      (putprop box godot-box :gdnode))
    (gdboxer-insert-row-at-row-no godot-box godot-row 0)))

(defmethod (setf superior-row) :after (value box)
  (format t "~%superior-row: ~A ~A" value box))

(defmethod (setf name) :after (value box)
  (format t "~%name: ~A ~A" value box))

(defmethod insert-row-at-row-no :after ((box box) row row-no
                                       &optional (check-closet t))
  "This could either be a brand new row (which doens't have a gdnode) or an existing
   row that just needs to be moved to a different place."
  (format t "~%insert-row-at-row-no box: ~A row: ~A row-no: ~A" box row row-no )
  (let ((godot-row (getprop row :gdnode))
        (godot-box (getprop box :gdnode)))
    ;; (if godot-row
      ;; (error "~%Need to handle a previous row being inserted")
      (progn
        (when godot-box
          (format t "~%Inserting new row")
          ;; godot-row might be null if it's a new row
          (putprop row (gdboxer-insert-row-at-row-no godot-box godot-row row-no) :gdnode)
          ;; (error "~%Why doesn't this box have a godot-node?")
          ;; change the above to unless and put in some sort of startup check?
          ;; maybe just don't evaluate this file until we start up the evaluator/editor
          )
        )
      ;; )
      )
  )

(defun link_initial_box_to_node (gdnode gdrow)
  (putprop *initial-box* gdnode :gdnode)
  (putprop (first-inferior-row *initial-box*) gdrow :gdnode)
  (format t "~%Linked to initial box: ~A row: ~A" gdnode gdrow))


;; (defmethod chas-array-slide-chas-neg :after (chas-array strt-cha-no
;;                                   distance old-active-length)
;;   (format t "~%chas-array-slide-chas-neg ~A ~A ~A ~A" chas-array strt-cha-no
;;                                   distance old-active-length)
;; )

;; (defmethod chas-array-slide-chas-pos :after (chas-array strt-cha-no
;;                                   distance old-active-length)
;;   (format t "~%chas-array-slide-chas-pos ~A ~A ~A ~A" chas-array strt-cha-no
;;                                   distance old-active-length)
;; )

;;
;; Hacking
;;
(defun godot-handle-boxer-input (data bits)
  (format t "~%godot-handle-boxer-input3: ~A bits: ~A" data bits)
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
    (getprop (bp-row *point*) :gdnode) (bp-cha-no *point*))
  (let ((godot-row (getprop (bp-row *point*) :gdnode)))
    (when godot-row
      (gdboxer-point-location (getprop (bp-row *point*) :gdnode) (bp-cha-no *point*))
    )
  )

  (format t "~%Current Doc: ~%~A~%" (boxer::textify-thing boxer::*initial-box*))
  )

(defmethod allocate-screen-obj-for-use-in ((self actual-obj-subclass)
                                           use-in-screen-box)
  ; oof
)


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
;;; TODO Why aren't these somewhere in core??
;;;
(defmethod shrunken? ((self box))
  (let ((style (display-style-style (slot-value self 'display-style-list))))
    (or (eq style :shrunk) (eq style :supershrunk))))

