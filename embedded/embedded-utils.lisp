(in-package :boxer)

(defun wowwow ()
  (print "So much wowowow"))

;;; sgithens Prototyping wrapping box construction and "stuff"
(defmethod initialize-instance :after ((self box)  &rest init-plist)
  (format t "~%Just initialized a box! ~A" self)
)

(defmethod initialize-instance :after ((self row)  &rest init-plist)
  (format t "~%Just initialized a row! ~A" self)
)

;;; CHAS

(defmethod change-cha-at-cha-no :after ((self row) n new-cha)
  (format t "~%change-cha-at-cha-no ~A ~A ~A" self n new-cha))

(DEFMETHOD INSERT-CHA-AT-CHA-NO :after ((SELF ROW) CHA CHA-NO)
  (format t "~%INSERT-CHA-AT-CHA-NO 12.3 ~A ~A ~A" self cha cha-no)
  ;; (gdboxer-insert-cha-signal (+ 2 (char-code cha)))
  )


;;; ROWS

(defmethod (setf superior-box) :after (value row)
  (format t "~%superior-box: ~A ~A" value row))

(defmethod (setf previous-row) :after (value row)
  (format t "~%previous-row: ~A ~A" value row))

(defmethod (setf next-row) :after (value row)
  (format t "~%next-row: ~A ~A" value row))

(defmethod (setf chas-array) :after (value row)
  (format t "~%chas-array: ~A ~A" value row))

;;; BOXES

(defmethod (setf first-inferior-row) :after (value box)
  (format t "~%first-inferior-row: ~A ~A" value box))

(defmethod (setf superior-row) :after (value box)
  (format t "~%superior-row: ~A ~A" value box))

(defmethod (setf name) :after (value box)
  (format t "~%name: ~A ~A" value box))





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
