;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;  CAPI implementation of a Preferences Dialog
;;;;
(in-package :boxer-window)


;;;; Preferences...

;; this should find a reasonable directory
;; for now use "~"
(defun boxer::default-lw-pref-file-name ()
  ;; TODO sgithens, refactor this out with a better cross platform version...
  (merge-pathnames "Boxer.prf"
    #+win32 (sys:get-folder-path :appdata)
    #+macosx (sys:get-folder-path :my-preferences)
    #+linux (uiop:xdg-config-home)))

(defvar boxer::*preference-dialog-change-list* nil)

(defvar *prefs-subgroups* nil)

;; this is smarter than the mac version. it does a prepass so we no longer have
; to rely on the prefs being well ordered in the source
(defun make-preferences-dialog ()
  (let ((groups nil)
        (tab-layout-groups nil))
    ;; prepass to organize into groups
    (dolist (pref boxer::*boxer-preferences-list*)
      (let* ((di-info (get pref 'boxer::system-parameter-pref-dialog-info))
             (subgroup (cadr di-info))
             (existing-group (assoc subgroup groups)))
        (cond ((not (null existing-group))
               (nconc existing-group (list pref)))
              (t (setq groups (append groups (list (list subgroup pref))))))))
    ;; now we have a lists of lists, the CAR of each sublist will be the
    ;; group name and the CDR will be a list of prefs
   (dolist (group groups)
     ;; loop through making items for a tab layout, remember the CAR is the
     ;; subgroup name and the CDR is a list of prefs
     ;; when we are done, then tab-layout-groups will be a list of
     ;; title strings and layout instances suitable as :items for a capi:tab-layout
       (setq tab-layout-groups
             (nconc tab-layout-groups
                    (list
                     (list
                      (string-capitalize (car group))
                      (make-instance
                       'capi:column-layout
                       :description
                       (boxer::with-collection
                         (dolist (pref (cdr group))
                           (let* ((di-info
                                   (get pref
                                        'boxer::system-parameter-pref-dialog-info))
                                  (value (car di-info)) (value-type (caddr di-info))
                                  (ac-fun (cadddr di-info)) (doc-fun (nth 4 di-info)))
                             (collect (case value-type
                                        (:boolean
                                         (make-boolean-pref
                                                   pref (symbol-value value)
                                                   ac-fun doc-fun))
                                        (:number
                                         (make-number-pref
                                                  pref (symbol-value value)
                                                  ac-fun doc-fun))
                                        ((:string :keyword)
                                         (make-string-pref
                                          pref (symbol-value value)
                                          ac-fun doc-fun)))))
                           )))))))
       )
   ;; and now the main dialog
   (capi:make-container
    (make-instance 'capi:column-layout
     :description
     (list
      ;; here are the pieces of the prefs dialog
      (make-instance 'capi:tab-layout
                     :print-function 'car :visible-child-function 'cadr
                    ;  :combine-child-constraints t
                     :items tab-layout-groups)
      ;; buttons (cancel, Set, Set&Save)
      (make-instance 'capi:push-button-panel
                         :items (list
                           (make-instance 'capi:push-button
                             :text "Cancel" :selection-callback 'capi:abort-dialog)
                           (make-instance 'capi:push-button
                             :text "Set" :selection-callback #'(lambda (&rest ignore)
                                                                 (declare (ignore ignore))
                                                                 (capi:exit-dialog t)))
                           (make-instance 'capi:push-button
                             :text "Set & Save"
                             :selection-callback #'(lambda (&rest ignore)
                                                     (declare (ignore ignore))
                                                      (capi:exit-dialog :save)))
                         ))
  )))))

(defun fixup-pref-name (name)
  (substitute #\space #\- (string-capitalize name)))

;;; remember that the defboxer-preference macro in syspims.lisp automagically
;;; defines the action and doc functions and that these function expect to
;;; receive a single dialog-item as their argument
(defun make-boolean-pref (name value action-function doc-function)
  (make-instance 'capi:column-layout
                 :description
                 (list
                  (make-instance 'capi:check-button :text (fixup-pref-name name)
                  :selected value
                                 :callback-type :item
                                 :selection-callback #'(lambda (item)
                                                         (funcall action-function
                                                                  item))
                                 :retract-callback #'(lambda (item)
                                                       (funcall action-function
                                                                item)))
                  (make-instance 'capi:display-pane :text (funcall doc-function) :background :transparent
                                 :visible-min-width '(:character 100) :visible-min-height '(:character 2)))))

(defun make-number-pref (name value action-function doc-function)
  (make-instance 'capi:column-layout
                 :description
                 (list
                  (make-instance 'capi:text-input-pane :title (fixup-pref-name name)
                                 :text (format nil "~A" value)
                                 :max-width 75
                                 ; :best-width 150
                                 :change-callback-type :item
                                 :change-callback #'(lambda (item)
                                                      (funcall action-function item)))
                  (make-instance 'capi:display-pane :text (funcall doc-function) :background :transparent
                                 :visible-min-width '(:character 100) :visible-min-height '(:character 2)))))

(defun make-string-pref (name value action-function doc-function)
  (make-instance 'capi:column-layout
                 :description
                 (list
                  (make-instance 'capi:text-input-pane :title (fixup-pref-name name)
                                 :text (format nil "~A" value)
                                 :max-width 75
                                 ; :best-width 150
                                 :change-callback-type :item
                                 :change-callback #'(lambda (item)
                                                      (funcall action-function item)))
                  (make-instance 'capi:display-pane :text (funcall doc-function) :background :transparent
                                 :visible-min-width '(:character 100) :visible-min-height '(:character 2)))))

; eventually...
;(defun make-choice-pref (name value action-function doc-function)
;  )

(boxer::defboxer-command boxer::com-boxer-preferences ()
  "Open the Preferences Dialog"
  (let ((dialog (make-preferences-dialog)))
    (unwind-protect
      (let ((ok? (capi:display-dialog dialog :modal t :owner *boxer-frame*)))
        (when ok?
          (dolist (change boxer::*preference-dialog-change-list*)
            (funcall (car change) (cdr change))))
        (when (eq ok? :save) (boxer::write-preferences)))
      (setq boxer::*preference-dialog-change-list* nil)
    )
    boxer-eval::*novalue*))
