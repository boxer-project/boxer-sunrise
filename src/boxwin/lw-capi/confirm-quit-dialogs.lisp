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
;;;;         This file the dialogs for the workflow of quitting Boxer when there are modified file boxes,
;;;;         giving the option to save each one.

(in-package :boxer-window)

;;; Save-Modified-Box-Dialog

(defun boxer::save-modified-box-dialog (box &key (prompt-start "The box,")
                                            no-zoom-offered?)
  (let* ((box-file (boxer::getprop box :associated-file))
         (dialog (capi:make-container
                  (make-instance 'capi:row-layout :description (list
                  (make-instance 'capi:column-layout
                                 :visible-max-width 10
                                 :visible-min-width 10
                                 :description '())
                  (make-instance 'capi:column-layout
                                 :description (list (make-instance 'capi:message-pane
                                                                   :visible-min-height '(:character 4)
                                                                   :text
                                                                   (format nil "Do you want to save the
changes made to the box:
\"~A.~A\"?"
                                                                           (pathname-name box-file) (pathname-type box-file)))
                                                    (make-instance 'capi:push-button
                                                                   :text "Save"
                                                                   :visible-max-width 230
                                                                   :visible-min-width 230
                                                                   :callback #'(lambda (data int)
                                                                                 (declare (ignore data int))
                                                                                 (capi:exit-dialog :save)))
                                                    (make-instance 'capi:push-button
                                                                   :text "Don't Save"
                                                                   :visible-max-width 230
                                                                   :visible-min-width 230
                                                                   :callback #'(lambda (data int)
                                                                                 (declare (ignore data int))
                                                                                 (capi:exit-dialog
                                                                                  :dont-save)))
                                                    (make-instance 'capi:push-button
                                                                   :text "Cancel"
                                                                   :visible-max-width 230
                                                                   :visible-min-width 230
                                                                   :callback 'capi:abort-dialog)))
                  (make-instance 'capi:column-layout
                                 :visible-max-width 10
                                 :visible-min-width 10
                                 :description '())))

                  :title "Warning"
                  :visible-border t))
         (action (capi:display-dialog dialog :owner *boxer-frame*)))
    (case action
      ;; do we want to wrap the save-generic with
      ;; a (catch 'cancel-boxer-file-dialog)
      ;; this will allow cancels out of the individual file op, if not, a cancel
      ;; out of the file op will cancel the top level quit operation
      (:save (boxer::save-generic
              box (or box-file
                      (boxer::boxer-new-file-dialog
                       :prompt (format nil "Save ~A box into file:"
                                       (boxer::box-save-name box))
                       :box box))))
      (:dont-save nil)
      (otherwise (throw 'boxer::cancel-boxer-file-dialog nil)))))

(defun lw-menu-quit (interface)
  "Checks the return from lw-quit before calling destroy. Using for hooks where it doesn't appear the
  'capi:confirm-destroy-function is being used."
  (when (lw-quit interface)
    (capi:destroy interface)))

(defun lw-quit (interface)
  "Checks if there are modified files and starts the dialogs for it if there are. Returns t if we should continue to
  close the application."
  (let ((togo (if (equal (length (boxer::modified-boxes-for-close)) 0)
                t
                (start-review-modified-files-dialog interface))))
    (when togo
      ;; Stop rendering otherwise we get openGL errors during the close process
      (setf (boxer::active *boxer-pane*) nil))))

(defun start-review-modified-files-dialog (interface)
  "Launches the workflow of dialog boxes for reviewing/discarding unsaved files. Returns t if we should continue
  to close the application, nil if closing should be cancelled."
  (catch 'boxer::cancel-boxer-file-dialog
    (let* ((dialog (capi:make-container
                  (make-instance 'capi:row-layout
                                 :description (list
                                 (make-instance 'capi:column-layout
                                                :visible-max-width 10
                                                :visible-min-width 10
                                                :description '())
                                 (make-instance 'capi:column-layout
                                 :visible-max-width 230
                                 :visible-min-width 230
                                 :description (list
                                               (make-instance 'capi:message-pane
                                                              :text (format nil "You have ~A file boxes with
unsaved changes. Do you want to
review these changes before
quitting?" (length (boxer::modified-boxes-for-close)))
                                                              :visible-min-height '(:character 5))
                                               (make-instance 'capi:message-pane
                                                              :text "If you don't review your boxes, all
your changes will be lost."
                                                              :visible-min-height '(:character 3))
                                               (make-instance 'capi:push-button
                                                              :text "Review Changes"
                                                              :visible-max-width 230
                                                              :visible-min-width 230
                                                              :callback #'(lambda (data int)
                                                                            (declare (ignore data int))
                                                                            (capi:exit-dialog :review)))
                                               (make-instance 'capi:push-button
                                                              :text "Discard Changes"
                                                              :visible-max-width 230
                                                              :visible-min-width 230
                                                              :callback #'(lambda (data int)
                                                                            (declare (ignore data int))
                                                                            (capi:exit-dialog :discard)))
                                               (make-instance 'capi:push-button
                                                              :text "Cancel"
                                                              :visible-max-width 230
                                                              :visible-min-width 230
                                                              ; :callback 'capi:abort-dialog)))
                                                              :callback #'(lambda (data int)
                                                                            (declare (ignore data int))
                                                                            (capi:exit-dialog :cancel)))))
                                 (make-instance 'capi:column-layout
                                                :visible-max-width 10
                                                :visible-min-width 10
                                                :description '())))
                  ))
         (action (capi:display-dialog dialog :owner *boxer-frame*)))
    (case action
      (:review
       (boxer::close-box-prescan boxer::*initial-box*)
       ;; If we get through the entire box prescan and none of the dialogs have thrown
       ;; cancel-boxer-file-dialog with nil we can exit with t
       t)
      (:discard t)
      (:cancel nil)
      (otherwise nil)))))

;; (boxer-eval::defboxer-key (bu::q-key 1) com-lw-quit) sgithens TODO March 7, 2020

(defun unsaved-boxes? ()
  (catch 'unsaved
    (boxer::do-dirty-file-boxes (fb)
      (when (and (not (eq fb boxer::*initial-box*))   ; not the top
                 (boxer::superior? fb boxer::*initial-box*)  ; is still connected
                 (or (not (boxer::read-only-box? fb)) ;
                     (and (null (boxer::getprop fb :associated-file))
                          (null (boxer::getprop fb :url)))))
        (throw 'unsaved t)))
    nil))
