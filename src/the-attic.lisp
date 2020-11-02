;;;;
;;;; The attic
;;;;

;;;;
;;;; FILE: infsup.lisp
;;;;

;;; These are no longer used.  They work (6/6/88), but do much useless work,
;;; and KILL-ROW was ever called.  See KILL-BOX-CONTENTS, the more efficient
;;; replacement.
#|
(defmethod insert-box-rows-at-row-no ((self box) box row-no)
  (let ((box-first-row (kill-row box (first-inferior-row box))))
    (unless (null box-first-row)
      (let ((row-at-row-no (row-at-row-no self row-no))
      (row-bf-row-no (row-at-row-no self (- row-no 1)))
      (box-last-row (do* ((next-box-row (next-row box-first-row)
                (next-row box-row))
        (box-row box-first-row next-box-row))
             (())
          (set-superior-box box-row self)
          (do-row-chas ((c box-row)) (unless (cha? c) (insert-self-action c)))
          (if (null next-box-row) (return box-row)))))
  (set-previous-row box-first-row row-bf-row-no)
  (set-next-row box-last-row row-at-row-no)
  (set-next-row row-bf-row-no box-first-row)
  (set-previous-row row-at-row-no box-last-row)))))

(defmethod delete-rows-between-row-nos ((self box) strt-row-no stop-row-no
          &optional (check-closet t))
  (let* ((strt-row (row-at-row-no self strt-row-no))
   (stop-row (row-at-row-no self stop-row-no))
   (strt-row-prev-row (unless (null strt-row) (previous-row strt-row)))
         (stop-row-next-row (unless (null stop-row) (next-row stop-row)))
   (return-box (make-initialized-box))
   (closet-row (slot-value self 'closets)))
    (do ((row strt-row (next-row row)))
  ((null row))
      (when (or (null check-closet) (not (eq row closet-row)))
  (do-row-chas ((c row)) (unless (cha? c) (delete-self-action c)))
  (set-superior-box row nil)))
    (set-previous-row strt-row nil)
    (set-next-row strt-row nil)
    (if (null strt-row-prev-row)
  (set-first-inferior-row self stop-row-next-row)
  (set-next-row strt-row-prev-row stop-row-next-row))
    (unless (null stop-row-next-row)
      (set-previous-row stop-row-next-row strt-row-prev-row))
    (append-row return-box strt-row)
    return-box))

(defmethod delete-between-rows ((self box) strt-row stop-row
        &optional (check-closet t))
  (let ((strt-row-prev-row (when (not-null strt-row)(previous-row strt-row)))
  (stop-row-next-row (when (not-null stop-row)(next-row stop-row)))
  (return-box (make-initialized-box))
  (closet-row (slot-value self 'closets)))
    (do ((row strt-row (next-row row)))
  ((eq row stop-row-next-row))
      (when (or (null check-closet) (not (eq row closet-row)))
  (do-row-chas ((c row)) (unless (cha? c) (delete-self-action c)))
  (set-superior-box row nil)))
    (set-previous-row strt-row nil)
    (set-next-row stop-row nil)
    (if (null strt-row-prev-row)
  (set-first-inferior-row self stop-row-next-row)
  (set-next-row strt-row-prev-row stop-row-next-row))
    (when (not-null stop-row-next-row)
      (set-previous-row stop-row-next-row strt-row-prev-row))
    (set-first-inferior-row return-box strt-row)
    return-box))

(defmethod kill-rows-at-row-no ((self box) strt-row-no)
  (let ((stop-row-no (length-in-rows self)))
    (delete-rows-between-row-nos self strt-row-no stop-row-no)))

(defmethod kill-row ((self box) row)
  (kill-rows-at-row-no self (row-row-no self row)))
|#

#|  obsolete, give it 6 months and then remove from source (7/19/91)
(defun delete-rows-to-end-of-box (bp &optional (force-bp-type nil))
  (action-at-bp-internal
    (let ((box (bp-box bp))
    (row (bp-row bp)))
      (unless (null box)
  (kill-rows-at-row-no box (+ (row-row-no box row) 1))))))
|#

#|
(defun find-path-from-superior-to-inferior (superior-box inferior-box)
  (nreverse
    (with-collection
      (do ((box inferior-box (superior-box box)))
    ((eq box superior-box))
  (collect box)))))
|#


;;;;
;;;; FILE: lw-menu.lisp
;;;;

#|

(defun menu-boxtop-standard (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          (t (box::putprop spb :standard :boxtop)))))

(defun menu-boxtop-folder (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          (t (box::putprop spb :folder :boxtop)))))

(defun menu-boxtop-name-only (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          (t (box::putprop spb :name-only :boxtop)))))
|#

;;; MIME menus

;; stub these for now...

(defun boxer::edit-mime-mapping () nil)

(defun boxer::mime-type-dialog (mime-type app-sig file-type)
  (declare (ignore mime-type app-sig file-type))
  nil)

#|
(defun edit-mime-mapping ()
  (let ((entries-dialog-item
         (make-instance 'ccl::sequence-dialog-item
           :view-position #@(30 30) :view-size #@(350 100)
           :table-sequence *mime-mac-file-types*
           :visible-dimensions #@(1 8)
           :table-vscrollp t
           :table-print-function #'(lambda (entry stream)
                                     (format stream "~30A ~6A ~A"
                                             (car entry) (cadr entry)
                                             (caddr entry))))))
    (flet ((selected-entry ()
             (let ((idx (ccl::selected-cells entries-dialog-item)))
               (unless (null idx)
                  (elt (ccl::table-sequence entries-dialog-item)
                       (ccl::point-v (car idx)))))))
      (let* ((labels (ccl::make-dialog-item
                      'ccl:static-text-dialog-item #@(30 10) #@(350 15)
                      "Mail Type                     Application      File Type"))
             (new-button (ccl:make-dialog-item 'ccl:button-dialog-item
                                               #@(10 200) #@(50 20) "New"
                                               #'(lambda (di) (declare (ignore di))
                                                  (add-new-mime-type)
                                                  (ccl:set-table-sequence
                                                   entries-dialog-item
                                                   *mime-mac-file-types*))))
             (copy-button (ccl:make-dialog-item
                           'ccl:button-dialog-item #@(80 200) #@(50 20) "Copy"
                           #'(lambda (di) (declare (ignore di))
                              (let ((entry (selected-entry)))
                                (cond ((null entry) (add-new-mime-type))
                                      (t (add-new-mime-type
                                          :mime-type (car entry)
                                          :app-sig (cadr entry)
                                          :file-type (caddr entry))
                                         (ccl:set-table-sequence
                                          entries-dialog-item
                                          *mime-mac-file-types*)))))))
             (edit-button (ccl:make-dialog-item
                           'ccl:button-dialog-item #@(150 200) #@(50 20) "Edit"
                           #'(lambda (di) (declare (ignore di))
                              (let ((entry (selected-entry)))
                                (cond ((null entry) (boxer::beep))
                                      (t (edit-mime-entry entry)
                                         (ccl:set-table-sequence
                                          entries-dialog-item
                                          *mime-mac-file-types*)))))))
             (delete-button (ccl:make-dialog-item
                             'ccl:button-dialog-item #@(220 200) #@(50 20) "Delete"
                             #'(lambda (di) (declare (ignore di))
                                (let ((entry (selected-entry)))
                                  (cond ((null entry) (boxer::beep))
                                        (t (setq *mime-mac-file-types*
                                                 (delete entry
                                                         *mime-mac-file-types*))
                                           (ccl:set-table-sequence
                                            entries-dialog-item
                                            *mime-mac-file-types*)))))))
             (save-button (ccl:make-dialog-item
                           'ccl:button-dialog-item #@(290 200) #@(50 20) "Save"
                             #'(lambda (di) (declare (ignore di))
                                (save-mime-mapping))))
             (ok-button (ccl:make-dialog-item
                         'ccl:button-dialog-item #@(360 200) #@(50 20) "Done"
                                          #'(lambda (di) (declare (ignore di))
                                             (ccl::return-from-modal-dialog t))))
             (dialog (make-instance 'ccl:dialog :window-title "Mail Attachments"
                       :view-position '(:top 60) :view-size #@(420 250)
                       :close-box-p nil
                       :view-subviews (list labels entries-dialog-item
                                            new-button copy-button edit-button
                                            delete-button save-button ok-button))))
        (ccl:modal-dialog dialog)))))


(defun mime-type-dialog (mime-type app-sig file-type)
  (let* ((mime-label (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                           #@(10 10) #@(150 20) "Mail Type"))
         (mime-di (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                        #@(10 30) #@(150 20) (or mime-type "")))
         (app-label (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                          #@(170 10) #@(100 20)
                                          "Application"))
         (app-di  (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                        #@(170 30) #@(60 20) (or app-sig "????")))
         (file-label (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                          #@(290 10) #@(100 20)
                                          "File Type"))
         (file-di (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                        #@(290 30) #@(60 20) (or file-type "????")))
         (doc-box (ccl::make-dialog-item 'ccl:static-text-dialog-item
                                        #@(10 60) #@(300 20) "")))
    (let* ((like-button (ccl:make-dialog-item
                         'ccl:button-dialog-item #@(10 90) #@(100 20) "Example..."
                         #'(lambda (di)
                             (declare (ignore di))
                             (let ((file (ccl:choose-file-dialog)))
                               (ccl:set-dialog-item-text
                                app-di (String (ccl:mac-file-creator file)))
                               (ccl:set-dialog-item-text
                                file-di (string (ccl:mac-file-type file)))))))
           (ok-button (ccl:make-dialog-item
                       'ccl:button-dialog-item #@(170 90) #@(50 20) "OK"
                       #'(lambda (di)
                           (declare (ignore di))
                           (cond ((not (= (length (ccl:dialog-item-text app-di)) 4))
                                  (boxer::beep)
                                  (ccl:set-dialog-item-text
                                   doc-box "App must be 4 chars long"))
                                 ((not (= (length (ccl:dialog-item-text file-di)) 4))
                                  (boxer::beep)
                                  (ccl:set-dialog-item-text
                                   doc-box "File Types must be 4 chars long"))
                                 (t
                                  (ccl:return-from-modal-dialog
                                   (values (list (ccl:dialog-item-text mime-di)
                                                 (ccl:dialog-item-text app-di)
                                                 (ccl:dialog-item-text file-di))
                                           t)))))))
             (cancel-button (ccl:make-dialog-item
                             'ccl:button-dialog-item #@(240 90) #@(50 20) "Cancel"
                             #'(lambda (di) (declare (ignore di))
                                (ccl:return-from-modal-dialog (values nil nil)))))
             (dialog (make-instance 'ccl:dialog
                       :window-title "Mail Type Entry"
                       :view-position '(:top 390) :view-size #@(400 120)
                       :close-box-p nil
                       :view-subviews (list mime-label mime-di app-label app-di
                                            file-label file-di doc-box
                                            like-button ok-button cancel-button))))
      (multiple-value-bind (result ok?)
          (ccl:modal-dialog dialog)
        (unless (null ok?)
          (list (car result)
                (intern (cadr result) (find-package "KEYWORD"))
                (intern (caddr result) (find-package "KEYWORD"))))))))
|#
