#|

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+

  Menu functions for Harlequin Lispworks on the PC
  see boxwin-lw.lisp for where these are installed


Modification History (most recent at top)

 8/ 3/13 menu-font-bigger/smaller
 7/30/13 set-font-size-menu-selection, font-size-menu-action
 3/21/12 menu-file-toggle use the same abstractions as :link/:nolink actions in box properties
 1/ 6/12 added :edit option to xref-existing-file-dialog, changed layout to be more readable
12/29/11 boxer::open-xref-dialog, xref-missing-file-ref-dialog, xref-existing-file-ref-dialog
12/09/11 added open-xref, menu-open-xref, handled various compiler warnings
 6/17/10 menu-find checks to see if we may already be in a mode
 1/05/10 boxer::default-lw-pref-file-name now uses sys:get-folder-path
 9/08/09 added *.box~ and *.box# to *boxer-file-filters* as possible "Boxer Files"
 8/04/08 make-preferences-dialog now calculates dialog size instead of using fixed constants
 8/07/08 make-box-props-dialog rewritten to be more dynamicly stretchy
10/28/07 box-properties rewritten
10/26/07 make-box-props-dialog: #+cocoa sizes and offsets because mac menus are
         fatter
12/20/05 added menu-clipboard-paste, menu-paste-graphics
12/17/05 added com-lw-quit and bound it to the ctrl-q key
 1/23/05 removed :best-width initarg from make-{number,string}-pref
 1/12/05 added data,interface args to menu-enter-license-key to
         keep the menu system mollified
 1/09/05 added com-hardcopy-lw-window for lispworks ctrl-P
12/21/04 menu-enter-license-key added
 3/27/03 lw-quit now offers to quit without reviewing unsaved boxes
11/19/02 boxer-{open, open-lost, new}-file-dialog uses *boxer-pathname-default* to
         generate initial value (mostly for ".box")
10/22/02 code review: no need to make changes in Places menu code because the
         mechanism for articulating the menu predates the introduction of the
         place menu enhancements.
 4/03/02 link-text-callback checks *link-radio-panel*
 3/20/02 make-display-props-cluster and boxer::com-edit-box-properties now
         handles shrink-on-exit? too
 3/15/02 make-display-props-cluster and boxer::com-edit-box-properties now
         handles always-zoom? and shrink-proof?
 2/20/02 set-font-menu-selection now updates the items in the menu
 2/19/02 com-edit-box-properties now uses (un)mark-box-as-file instead of trying
         to do all the pieces separately (fixes bug with nofile leaving file boxtop
 1/09/02 numerous small changes to box properties menus lw-menu.lisp
 1/10/02 added redisplay to menu-box-properties because there are now some menu
         items in the properties dialog which can alter the screen
10/14/01 added redisplays to close-file-box and save-document
 5/11/01 changed prompt for menu-name-spelling-help to
         "List Boxes or Primitives whose name contains:"
 5/05/01 tuned make-preferences-dialog, make-{boolean,string,number}-pref
 5/03/01 fixed catch tag which was in the incorrect package in lw-quit
         fixed package warnings in unstore-box
 4/09/01 outlink-port-dialog ported
 3/06/01 added menu-font-bugger/smaller
 2/19/01 unstore-box outlink-port-dialog edit-mime-mapping mime-type-dialog
 1/15/01 added :owner args to capi:display-dialog calls
10/14/00 added enabled and print functions for menus
10/10/00 fixed blowout in menu-box-properties
10/08/00 added :operation args to all capi:prompt-for-file calls to get correct
         button text
10/05/00 fixed bad throw tag in boxer-open-file-dialog
         added :owner args to capi:prompt calls so dialog appears in correct place
 7/14/00 menu-stop now works
 7/10/00 changed default-lw-pref-file-name to use "~" directory
 5/21/00 box properties menus
 5/14/00 save-modified-box-dialog & add-path-to-ro-box-dialog
 1/17/00 started file


|#

(in-package :boxer-window)

(defun boxer::open-xref-file-dialog (&key (prompt "Link to file ") directory)
  (multiple-value-bind (path success?)
      (capi:prompt-for-file prompt :filters '("All Files" "*.*")
                            :pathname (merge-pathnames
                                       (or directory "")
                                       boxer::*boxer-pathname-default*)
                            :operation :open :owner *boxer-frame*)
    (if (null success?)
        (throw 'boxer::cancel-boxer-file-dialog nil)
      path)))

;;; dialogs used for handling inferior file boxes (see boxer::close-box-prescan)

(defun boxer::outlink-port-dialog (box &key nports)
  (declare (ignore nports)) ; use this later to report # of ports
  (let* ((dialog (capi:make-container
                  (list (make-instance
                         'capi:title-pane :x 20 :y 20 :width 330 :height 60
                         :text
                         "Warning: The target of a port being saved is not inside the document being saved"
                         )
                        (make-instance 'capi:push-button :x 20 :y 90
                                       :width 100 :height 20 :text "Save Anyway"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog :save)))
                        (make-instance 'capi:push-button :x 145 :y 90
                                       :width 100 :height 20 :text "Zoom to Port"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog :zoom)))
                        (make-instance 'capi:push-button :x 270 :y 90
                                       :width 75 :height 20 :text "Cancel"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog nil))))
                  :title "Warning"
                  :visible-border t))
         (action (capi:display-dialog dialog :owner *boxer-frame*)))
    (case action
      ;; do we want to wrap the save-generic with (catch 'cancel-boxer-file-dialog)
      ;; this will allow cancels out of the individual file op, if not, a cancel
      ;; out of the file op will cancel the top level quit operation
      (:save nil)
      (:zoom (boxer::move-to-bp-values boxer::*point*
                                       (boxer::box-first-bp-values box))
       (throw 'boxer::cancel-boxer-file-dialog nil))
      (otherwise  (throw 'boxer::cancel-boxer-file-dialog nil)))))

;; single click on an xref box calls this to decide what to do...
;; can return :open, :change or :move
(defun boxer::open-xref-dialog (current-file)
  (if (Null current-file)
      (xref-missing-file-ref-dialog)
    (xref-existing-file-ref-dialog current-file)))

(defun xref-missing-file-ref-dialog ()
  (let* ((start-text "You Need to Choose a file to link to before you can")
         (app-text "Click \"Choose\" to pick the file for the link")
         (open-button (make-instance 'capi:push-button :x 80 :y 60
                                       :width 40 :height 20 :text "Open"
                                       :enabled nil
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog :open))))
         (dialog (capi:make-container
                  (list (make-instance 'capi:title-pane
                                       :x 20 :y 20 :width 160 :height 30 :text start-text)
                        open-button
                        (make-instance 'capi:title-pane
                                       :x 20 :y 95 :width 160 :height 50 :text app-text)
                        (make-instance 'capi:push-button
                                       :x 70 :y 155 :width 75 :height 20 :text "Choose"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog :change)))


                        (make-instance 'capi:push-button :x 70 :y 250
                                       :width 75 :height 20 :text "Cancel"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog nil))))
                  :title "Link to File"
                  :visible-border t)))
    (capi:display-dialog dialog :owner *boxer-frame*)))

(defun xref-existing-file-ref-dialog (current-file)
  (let* ((start-text (format nil "This Box is a Link to the file ~A.  You can..." current-file))
          (app-text (format nil "the file, using its own application"))
          (dialog (capi:make-container
                   (list
                    (make-instance 'capi:column-layout
                                   :description
                                   (list
                                    (make-instance 'capi:title-pane :text start-text)
                                    (make-instance 'capi:row-layout
                                                   :description
                                                   (list (make-instance 'capi:push-button
                                                                        :text "Edit the Box"
                                                                        :callback
                                                                        #'(lambda (data int)
                                                                            (declare (ignore data int))
                                                                            (capi:exit-dialog :edit)))))
                                    (make-instance 'capi:row-layout
                                                   :description
                                                   (list (make-instance 'capi:push-button
                                                                        :text "Open"
                                                                        :callback
                                                                        #'(lambda (data int)
                                                                            (declare (ignore data int))
                                                                            (capi:exit-dialog :open)))
                                                         (make-instance 'capi:title-pane :text app-text)))
                                    (make-instance 'capi:row-layout
                                                   :description
                                                   (list (make-instance 'capi:push-button
                                                                        :text "Change"
                                                                        :callback
                                                                        #'(lambda (data int)
                                                                            (declare (ignore data int))
                                                                            (capi:exit-dialog :change)))
                                                         (make-instance 'capi:title-pane
                                                                        :text
                                                                        "the link to a different file or")))
                                    (make-instance 'capi:row-layout
                                                   :description
                                                   (list (make-instance 'capi:push-button
                                                                         :text "Move"
                                                                         :callback
                                                                         #'(lambda (data int)
                                                                             (declare (ignore data int))
                                                                             (capi:exit-dialog :move)))
                                                         (make-instance 'capi:title-pane
                                                                        :text
                                                                        "the file to a different folder")))
                                    (make-instance 'capi:push-button
                                                   :text "Cancel"
                                                   :callback #'(lambda (data int)
                                                                 (declare (ignore data int))
                                                                 (capi:exit-dialog nil)))))))))
     (capi:display-dialog dialog :owner *boxer-pane*)))

(defun boxer::add-path-to-ro-box-dialog (box)
  (let* ((dialog (capi:make-container
                  (list (make-instance
                         'capi:title-pane :x 20 :y 20 :width 330 :height 60
                         :text
                         (format
                          nil
                          "The Read Only Box: ~A does not have an associated file"
                          (boxer::current-file-status box)))
                        (make-instance 'capi:push-button :x 20 :y 70
                                       :width 75 :height 20 :text "Add File"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog :save)))
                        (make-instance 'capi:push-button :x 105 :y 70
                                       :width 75 :height 20 :text "Don't Save"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog
                                                      :dont-save)))
                        (make-instance 'capi:push-button :x 190 :y 70
                                       :width 75 :height 20 :text "Zoom To"
                                       :callback #'(lambda (data int)
                                                     (declare (ignore data int))
                                                     (capi:exit-dialog :zoom)))
                        (make-instance 'capi:push-button :x 275 :y 70
                                       :width 75 :height 20 :text "Cancel"
                                       :callback 'capi:abort-dialog))
                  :title "Warning"
                  :visible-border t))
         (action (capi:display-dialog dialog :owner *boxer-frame*)))
       (case action
        (:addfile
         ;; do we need to make sure the superior gets saved like Unstore does ?
         (boxer::putprop box (boxer::boxer-open-file-dialog) :associated-file))
        (:dont-save (boxer::mark-file-box-clean box))
        (:zoom (boxer::move-to-bp-values boxer::*point*
                                         (boxer::box-first-bp-values box))
         (throw 'boxer::cancel-boxer-file-dialog nil))
        (otherwise (throw 'boxer::cancel-boxer-file-dialog nil)))))


;;; NOTE: Menu callback functions take 2 args (data interface)

;; Menu functions

;; **** File Menu ****

(defun new-file-box (data interface)
  (declare (ignore data interface))
  (boxer::com-new-file-box))

(defun open-file (data interface)
  (declare (ignore data interface))
  (queue-event 'boxer::com-open-box-file))

(defun close-file-box (data interface)
  (declare (ignore data interface))
  (boxer::drawing-on-window (*boxer-pane*)
    (boxer::com-close-box)))

;; 2021-06-30 boxer-bugs-46
;; This ideally will replace both menu-file-toggle and save-box-as
;; by simplifying the single menu to 2 states, either:
;;  - Save Box as File   or
;;  - Unmark This Box as File
;;
;; Once we've tested this out and like it, I'll remove the other options
;; from this clutter. - sgithens
(defun menu-file-box-as-file (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          ;; Box already saved as file
          ((box::storage-chunk? spb)
           (boxer::mark-file-box-clean spb)
           (boxer::unmark-box-as-file spb)
           (boxer::modified spb))
          ;; Box not yet marked/saved as file
          (t
           (save-box-as data interface)))))

(defun menu-file-box-as-file-print (mi)
  (declare (ignore mi))
    (let ((spb (safe-point-box)))
      (cond ((null spb))
            ((box::storage-chunk? spb) "Unmark Box as File")
            (t "Save Box as File"))))

;; These can be removed once boxer-bugs-46 is finalized
;; (defun menu-file-toggle (data interface)
;;   (declare (ignore data interface))
;;   (let ((spb (safe-point-box)))
;;     (cond ((null spb))
;;           ((box::storage-chunk? spb)
;;            (boxer::mark-file-box-clean spb)
;;            (boxer::unmark-box-as-file spb)
;;            (boxer::modified spb))
;;           (t
;;            (boxer::mark-box-as-file spb)
;;            (boxer::mark-file-box-dirty spb)
;;            (boxer::modified spb))))
;;   (boxer::repaint))

;; (defun menu-file-toggle-print (mi)
;;   (declare (ignore mi))
;;   (let ((spb (safe-point-box)))
;;     (cond ((null spb))
;;           ((box::storage-chunk? spb) "Unmark Box as File")
;;           (t "Mark Box as File"))))

(defun save-document (data interface)
  "There are 4 possibilities here:
     1. We aren't in any type of saved box.
        Label: Saved...  Action: Save As
     2. This should be filtered out by the dis/enabled function, but if we're in a file
        box that is read-only, we won't do anything.
     3. We are inside some heirarchy of a file box that has no changes
        Label: Save      Action: Save that file box
     4. We are inside some heirarchy of a file box that has changes
        Label: Save      Action: Save that file box

     The actions for 3 and 4 may diverge at some point..."
  (declare (ignore data interface))
  (let* ((filebox (boxer::current-file-box))
        (read-only (boxer::read-only-box? filebox))
        (modified (boxer::file-modified? filebox))
        (has-file (not (null (boxer::getprop filebox :associated-file))))) ; TODO should this include :url?
    (cond ((not has-file) 1. Not in any type of saved box.
           (save-document-as data interface))
          ((and has-file read-only) 2. Read-only File
           nil)
          ((and has-file (not modified))
           (boxer::com-save-document))
          ((and has-file modified)
           (boxer::com-save-document)))))

;; boxer-bugs-46. We are changing "Save" to act as "Save..." (Save As) rather
;; than be greyed out and disabled
(defun save-document-title-function (mi)
  (let* ((filebox (boxer::current-file-box))
        (read-only (boxer::read-only-box? filebox))
        (modified (boxer::file-modified? filebox))
        (has-file (not (null (boxer::getprop filebox :associated-file))))) ; TODO should this include :url?
    (cond ((not has-file)
           "Save...")
          ((and has-file read-only)
           "Save (RO)")
          (t
           "Save"))))

(defun save-document-enabled-function (mi)
  "We only want the Save menu to be disabled if we're in a Read-Only file box."
  (declare (ignore mi))
  (let* ((filebox (boxer::current-file-box))
        (read-only (boxer::read-only-box? filebox))
        (modified (boxer::file-modified? filebox))
        (has-file (not (null (boxer::getprop filebox :associated-file))))) ; TODO should this include :url?
    (not read-only)))

;; Keeping here just for note-taking until we've finalized the menus.
(defun save-menu-item-enabled-function (save-item)
  (declare (ignore save-item))
  ;; grey out File menu items if they are redundant or not applicable
  ;; "Save" is not applicable if there is not an existing filename
  ;; or if the file box has not been modified...
  (let ((filebox (boxer::current-file-box)))
    (not (or (boxer::read-only-box? filebox)
             (not (boxer::file-modified? filebox))
             (and (null (boxer::getprop filebox :associated-file))
                  (null (boxer::getprop filebox :url)))))))

(defun save-document-as (data interface)
  (declare (ignore data interface))
  (boxer::com-save-document T))

(defun save-box-as (data interface)
  (declare (ignore data interface))
  (boxer::com-box-save-as T))

(defun open-xref (data interface)
  (declare (ignore data interface))
  (queue-event 'menu-open-xref))

(defun menu-open-xref ()
  (boxer::com-open-link-file))


;;; for ctrl-P

;; sgithens March 7, 2020
;; (boxer::defboxer-command boxer::com-hardcopy-lw-window ()
;;    "Print the Boxer screen on the currently selected printer"
;;    (window-hardcopy *boxer-frame*)
;;    boxer-eval::*novalue*)

(boxer::defboxer-command com-LW-quit ()
  "Quit Out of Boxer"
  (lw-quit nil *boxer-pane*)
  boxer-eval::*novalue*)

(defun toggle-dev-overlay (data interface)
  (if boxer::*show-dev-overlay*
    (setf boxer::*show-dev-overlay* nil)
    (setf boxer::*show-dev-overlay* t)))

(defun rotate-canvas-demo (data interface)
  (bw::queue-event 'boxer-opengl::rotation-demo))

;;; **** Edit Menu ****

(defun menu-cut-region (data interface)
  (declare (ignore data interface))
  (boxer::com-cut-region))

(defun menu-copy-region (data interface)
  (declare (ignore data interface))
  (boxer::com-copy-region))

(defun menu-retrieve (data interface)
  (declare (ignore data interface))
  (boxer::com-retrieve))

(defun menu-clipboard-paste (d i)
  (declare (ignore d i))
  (boxer-lw-capi::paste *boxer-frame*)
  boxer-eval::*novalue*)

(defun menu-select-box-contents (data interface)
  (declare (ignore data interface))
  (boxer::com-select-box-contents))

(defun menu-prefs (data interface)
  (declare (ignore data interface))
  (boxer::com-boxer-preferences))

(defun menu-searchbar-find (data interface)
  (declare (ignore data interface))
  (setf *boxer-window-show-searchbar-p* t)
  (update-visible-editor-panes)
  (capi:set-pane-focus (slot-value (slot-value *boxer-frame* 'search-pane) 'search-input)))

(defun searchbar-close (data interface)
  (declare (ignore data interface))
  (setf *boxer-window-show-searchbar-p* nil)
  (update-visible-editor-panes)
  (boxer::cancel-search-text))

(defun menu-find (data interface)
  (declare (ignore data interface))
  (let* ((insearch (boxer::mode-key 'bu::CTRL-F-KEY))
         (fun (when (boxer-eval::boxer-function? insearch)
                (boxer-eval::compiled-boxer-function-object insearch))))
    (if (null fun)
        (boxer::com-search-forward)
      (funcall fun))))

;; **** View Menu ****

(defun show-toolbar-popup-callback (menu)
  (setf (capi:item-selected menu) *boxer-window-show-toolbar-p*))

(defun show-statusbar-popup-callback (menu)
  (setf (capi:item-selected menu) *boxer-window-show-statusbar-p*))

(defun menu-zoom-in (data interface)
  (declare (ignore data interface))
  (unless (>= (zoom-level *boxer-pane*) 4)
    (setf (zoom-level *boxer-pane*) (+ 0.25 (zoom-level *boxer-pane*))))
  (reset-global-scrolling))

(defun menu-zoom-out (data interface)
  (declare (ignore data interface))
  (unless (<=  (zoom-level *boxer-pane*) 0.50)
    (setf (zoom-level *boxer-pane*) (- (zoom-level *boxer-pane*) 0.25)))
  (reset-global-scrolling))

;; **** Do Menu ****


(defun menu-do-line (data interface)
  (declare (ignore data interface))
  (queue-event 'boxer::com-doit-now))

(defun menu-step (data interface)
  (declare (ignore data interface))
  (queue-event 'boxer::com-step))

;; copied from mac. need to inplement boxer-interrupt...
(defun menu-stop (data interface)
  (declare (ignore data interface))
  (if boxer::*evaluation-in-progress?*
      (boxer-interrupt)
      (queue-event 'boxer::com-abort)))


;;; **** Box Menu ****

(defun menu-data-box (data interface)
  (declare (ignore data interface))
  (boxer::com-make-and-enter-data-box))

(defun menu-doit-box (data interface)
  (declare (ignore data interface))
  (boxer::com-make-and-enter-box))

(defun menu-turtle-box (data interface)
  (declare (ignore data interface))
  (boxer::com-make-turtle-box))

(defun menu-graphics-box (data interface)
  (declare (ignore data interface))
  (boxer::com-make-graphics-box))

(defun menu-sprite-box (data interface)
  (declare (ignore data interface))
  (boxer::com-make-sprite-box))

(defun menu-port (data interface)
  (declare (ignore data interface))
  (boxer::com-place-port))

(defun menu-unbox (data interface)
  (declare (ignore data interface))
  (boxer::com-unboxify))


;;; **** Font Menu ****

(defun menu-font-bigger (data interface)
  (declare (ignore data interface))
  (unless (>= boxer::*font-size-baseline* 4)
    (setq boxer::*font-size-baseline* (+ 0.25 boxer::*font-size-baseline*)))
  (update-toolbar-font-buttons))

(defun menu-font-smaller (data interface)
  (declare (ignore data interface))
  (unless (<=  boxer::*font-size-baseline* 0.50)
    (setq boxer::*font-size-baseline* (- boxer::*font-size-baseline* 0.25)))
  (update-toolbar-font-buttons))

(defun get-current-font ()
  (let ((region (or boxer::*region-being-defined* (boxer::get-current-region))))
    (cond ((null region) boxer::*current-font-descriptor*)
          (t (boxer::bp-closest-bfd (boxer::interval-start-bp region))))))

;; adds a check mark to the string if it matches the current font
;; note that this may have to recalculate the items because of newly added fonts
(defun set-font-menu-selection (menu)
  (let* ((current-font (boxer::bfd-font-no (get-current-font)))
         (font-name (boxer::font-name current-font))
         (select-idx (when (stringp font-name)
                       (setf (capi::collection-items menu) boxer::*font-families*)
                       (position font-name (capi::collection-items menu)
                                 :test #'(lambda (fn it)
                                           (string-equal fn
                                                         (capi::menu-item-data it)))))))
    (when (numberp select-idx)
      (setf (capi::choice-selection menu) select-idx))))


(defun font-menu-action (data int)
  (declare (ignore int))
  (let ((region (or boxer::*region-being-defined*
                    (boxer::get-current-region))))
    (cond ((not (null region))
           (boxer::change-region-font region data))
          (t
           (setf (boxer::bfd-font-no boxer::*current-font-descriptor*)
                 (boxer::%set-font (boxer::bfd-font-no
                                    boxer::*current-font-descriptor*)
                                   data)))))
  (update-toolbar-font-buttons))

;; support for displaying current (because of Zoom) absolute font sizes
;; should be in this function
(defun set-font-size-menu-selection (menu)
  (let* ((current-font (boxer::bfd-font-no (get-current-font)))
         (font-size (boxer::font-size current-font)))
    (setf (capi::choice-selection menu) (position font-size boxer::*menu-font-sizes*))))

(defun font-size-menu-action (data int)
  (declare (ignore int))
  (let ((region (or boxer::*region-being-defined*
                    (boxer::get-current-region))))
    (cond ((not (null region))
           (boxer::change-region-font-size region data))
          (t
           (setf (boxer::bfd-font-no boxer::*current-font-descriptor*)
                 (boxer::%set-font-size (boxer::bfd-font-no
                                         boxer::*current-font-descriptor*)
                                        data)))))
  (update-toolbar-font-buttons))

(defun set-font-style-menu-selection (menu)
  (let ((current-font (boxer::bfd-font-no (get-current-font))))
    (cond ((boxer::normal-font? current-font)
           (setf (capi::choice-selected-items menu) '(:plain)))
          ((boxer::bold-font? current-font)
           (cond ((boxer::italic-font? current-font)
                  (setf (capi::choice-selected-items menu) '(:bold :italic)))
                 (t (setf (capi::choice-selected-items menu) '(:bold)))))
          ((boxer::italic-font? current-font)
           (setf (capi::choice-selected-items menu) '(:italic)))
          (t ;; something is wrong if we ever get here
             (setf (capi::choice-selected-items menu) nil)))))

(defun font-style-menu-action (data int)
  (declare (ignore int))
  (let* ((region (or boxer::*region-being-defined*
                     (boxer::get-current-region)))
         (current-font (boxer::bfd-font-no (get-current-font)))
         (attribute-on? (case data
                          (:plain t)
                          (:bold   (boxer::bold-font?   current-font))
                          (:italic (boxer::italic-font? current-font)))))
    (cond ((not (null region))
           (boxer::change-region-style region data (not attribute-on?)))
          (t
           (setf (boxer::bfd-font-no boxer::*current-font-descriptor*)
                 (boxer::set-font-style (boxer::bfd-font-no
                                         boxer::*current-font-descriptor*)
                                        data (not attribute-on?))))))
  (update-toolbar-font-buttons))

(defun set-font-color-menu-selection (menu)
  (let* ((current-color (boxer::bfd-color (get-current-font)))
         (select-idx (position current-color (capi::collection-items menu)
                               :test #'(lambda (fs it)
                                         (color= fs (capi::menu-item-data it))))))
    (when (numberp select-idx)
      (setf (capi::choice-selection menu) select-idx))))

(defun font-color-menu-action (data int)
  (declare (ignore int))
  (let ((region (or boxer::*region-being-defined*
                    (boxer::get-current-region))))
    (cond ((not (null region))
           (boxer::change-region-color region data))
          (t (setf (boxer::bfd-color boxer::*current-font-descriptor*) data))))
  (update-toolbar-font-buttons))

(defun init-font-color-menu ()
  (setf (capi::menu-items *font-sub-color-menu*)
        (list (make-instance 'capi::menu-component
                             :interaction :single-selection
                             :popup-callback 'set-font-color-menu-selection
                             :callback 'font-color-menu-action
                             :items (list (make-instance 'capi::menu-item
                                                         :title "Black" :data boxer::*black*)
                                          (make-instance 'capi::menu-item
                                                         :title "White" :data boxer::*white*)
                                          (make-instance 'capi::menu-item
                                                         :title "Red" :data boxer::*red*)
                                          (make-instance 'capi::menu-item
                                                         :title "Green" :data boxer::*green*)
                                          (make-instance 'capi::menu-item
                                                         :title "Blue" :data boxer::*blue*)
                                          (make-instance 'capi::menu-item
                                                         :title "Cyan" :data boxer::*cyan*)
                                          (make-instance 'capi::menu-item
                                                         :title "Magenta" :data boxer::*magenta*)
                                          (make-instance 'capi::menu-item
                                                         :title "Yellow" :data boxer::*yellow*)
                                          (make-instance 'capi::menu-item
                                                         :title "Orange" :data boxer::*orange*)
                                          (make-instance 'capi::menu-item
                                                         :title "Purple" :data boxer::*purple*)
                                          (make-instance 'capi::menu-item
                                                         :title "Gray" :data boxer::*gray*))))))

(boxer::def-redisplay-initialization (init-font-color-menu))

;;; **** Other Menu ****

;; Popup callback functions

;; these need to be better armored, so be paranoid because if there is an error
;; it will lose for every event poll
(defun safe-point-box ()
  (when (boxer::bp? boxer::*point*)
    (let ((row (boxer::bp-row boxer::*point*)))
      (when (boxer::row? row) (boxer::superior-box row)))))

;; Need separate enabled and update functions because menu system keeps
;; trying to reset the enabled slot back to T when an update function is called

(defun menu-name (data interface)
  (declare (ignore data interface))
  (boxer::com-name-box))

(defun box-check-menu-item-enabled? (mi)
  (declare (ignore mi))
  (if (boxer::box? (safe-point-box)) T nil))

(defun closet-menu-item-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((not (boxer::box? spb)) "Toggle Closet")
          ((boxer::closet-opened? spb) "Close Closet")
          (t "Open Closet"))))

(defun menu-closet-flip (data interface)
  (declare (ignore data interface))
  (boxer::com-toggle-closets))

(defun graphics-menu-item-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((boxer::display-style-graphics-mode?
            (boxer::display-style-list spb))
           "Flip to Text")
          (t  "Flip to Graphics"))))

(defun graphics-flip-menu-item-enabled? (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (if (and (boxer::box? spb) (not (null (boxer::graphics-sheet spb))))
        t
        nil)))

(defun menu-graphics-flip (data interface)
  (declare (ignore data interface))
  (boxer::com-toggle-box-view))

(defun type-menu-item-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((boxer::data-box? spb) "Flip to Doit")
          ((boxer::doit-box? spb) "Flip to Data")
          (t  "Toggle Type"))))

(defun type-flip-menu-item-enabled? (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (if (or (boxer::data-box? spb) (boxer::doit-box? spb))
        t
        nil)))

(defun menu-data-doit-flip (data interface)
  (declare (ignore data interface))
  (boxer::com-toggle-box-type))

(defun menu-transparency-flip (data interface)
  (declare (ignore data interface))
  (boxer::com-toggle-box-transparency))

(defun trans-menu-item-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((not (boxer::box? spb)) "Toggle Exports")
          ((null (boxer::exports spb)) "Export Names")
          (t "Contain Names"))))

;;
(defun menu-box-properties (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (when (boxer::box? spb)
      (boxer::com-edit-box-properties spb))))

(defun vanilla-menu-item-print (mi)
  (declare (ignore mi))
  (if (boxer::fast-memq boxer::*global-top-level-mode* boxer::*active-modes*)
      "Local Key/Mouse Mode"
    "Top Level Key/Mouse Mode"))

(defun menu-key-mouse-mode (data interface)
  (declare (ignore data interface))
  (boxer::com-toggle-vanilla-mode)
  (update-toolbar-font-buttons))

;;; **** Places Menu ****

(defun menu-set-mark (data interface)
  (declare (ignore data interface))
  (boxer::com-set-mark))

(defun menu-last-mark (data interface)
  (declare (ignore data interface))
  (boxer::com-goto-previous-place))

(defun menu-top-level (data interface)
  (declare (ignore data interface))
  (boxer::com-goto-top-level))

(defun menu-zoom-to-target (data interface)
  (declare (ignore data interface))
  (boxer::com-move-to-port-target))

(defun menu-remember-place () (boxer::com-point-to-register))

(defun menu-remember-here (data interface)
  (declare (ignore data interface))
  ;; get out of the menu process so we can poll for input
  (queue-event 'menu-remember-place))

;; The 1st 3 items are standard and should always appear, additional
;; items are calculated from boxer::*recorded-place-alist*
(defun calculate-places (menu)
  (when (eq menu *boxer-frame*) (setq menu (slot-value *boxer-frame* 'place-menu)))
  (let* ((current-items (capi::menu-items menu))
         (current-place-items (nthcdr 3 current-items)))
    ;; reconcile the place registers and the actual items in the menu
    ;; first, make sure all the items already there are valid
    (dolist (item current-place-items)
      (unless (member item boxer::*recorded-place-alist*
                      :test #'(lambda (item place-pair)
                                (string-equal (capi::menu-item-data item)
                                              (car place-pair))))
        (setq current-place-items (delete item current-place-items))))
    ;; then see if we need to add any new entries
    (dolist (pair boxer::*recorded-place-alist*)
      (unless (member pair current-place-items
                      :test #'(lambda (pair item)
                                (string-equal (car pair)
                                            (capi::menu-item-data item))))
        (setq current-place-items
              (append current-place-items
                      (list (make-instance 'capi::menu-item
                                           :data (car pair)
                                           :title (place-register-string
                                                   (car pair) (cdr pair))
                                           :callback 'place-menu-item-action))))))
    ;; now current-place-items is synched with boxer::*recorded-place-alist*
    (setf (cdddr current-items) current-place-items)
    current-items))


(defun place-register-string (string register)
  (let* ((box (boxer::process-doit-cursor-position-box register)))
    (declare (ignore box))
    string))
;;    (format nil "~C: ~A" (char-upcase char) (box-location-string box "" 12))

(defun place-menu-item-action (data int)
  (declare (ignore int))
  (let* ((register (assoc data boxer::*recorded-place-alist*
                          :test #'string-equal))
         (status (if (null register) :error (boxer::move-to-place (cdr register)))))
    (cond ((eq status ':error)
           (setq boxer::*recorded-place-alist*
                 (delete register boxer::*recorded-place-alist*))
           (boxer::boxer-editor-warning "Place ~A is no longer in the editor" data))
          (t nil))))

;;; **** MacOS Window Menu ****

#+macosx
(defun menu-window-minimize (data interface)
  "Currently we only have one window, so we can just operate on *boxer-frame*.
   But when we begin supporting multiple windows we'll need to list them all and
   operate on the currently focused window."
   (objc:invoke (slot-value (slot-value bw::*boxer-frame* 'capi-internals:representation) 'capi-cocoa-library::window)
                "miniaturize:" nil))

;;; **** Help Menu ****

(defun menu-inputs-help (data interface)
  (declare (ignore data interface))
  (boxer::com-prompt))

(defun near-string ()
  (let ((c (boxer::prompter-chunk-before-point)))
    (unless (null c)
      (let ((v (boxer::chunk-chunk (boxer::get-pointer-value c nil))))
        (when (symbolp v) (symbol-name v))))))

(defun menu-name-spelling-help (data interface)
  (declare (ignore data interface))
  (boxer::insert-cha boxer::*point*
                     (boxer::convert-doit-result-for-printing
                      (boxer::name-help-internal
                       (string-upcase
                        (capi::prompt-for-string
                         "List Boxes or Primitives whose name contains:"
                         :initial-value (near-string)))))))

(defun menu-key-mouse-help (data interface)
  (declare (ignore data interface))
   ;; get out of the menu process so we can poll for input
  (queue-event 'boxer::com-document-key))

;; Sub Menus
;; Display Properties Sub Menu

(defun menu-zoom-toggle (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          ((box::always-zoom? spb) (box::set-always-zoom? spb nil))
          (T (box::set-always-zoom? spb T)))))

(defun menu-zoom-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((null spb) "")
          ((box::always-zoom? spb) "Don't Zoom")
          (t "Always Zoom"))))

(defun menu-exit-shrink-toggle (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          ((box::shrink-on-exit? spb) (box::set-shrink-on-exit? spb nil))
          (t (box::set-shrink-on-exit? spb T)))))

(defun menu-exit-shrink-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((null spb) "")
          ((box::shrink-on-exit? spb) "Don't Shrink on Exit")
          (t "Shrink on Exit"))))

(defun menu-auto-fill-toggle (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          ((box::auto-fill? spb) (box::set-auto-fill? spb nil))
          (t (box::set-auto-fill? spb T)))))

(defun menu-auto-fill-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((null spb) "")
          ((box::auto-fill? spb) "Auto Fill Off")
          (t "Auto Fill On"))))

(defun menu-shrink-proof-toggle (data interface)
  (declare (ignore data interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          ((box::shrink-proof? spb) (box::set-shrink-proof? spb nil))
          (t (box::set-shrink-proof? spb T)))))

(defun menu-shrink-proof-print (mi)
  (declare (ignore mi))
  (let ((spb (safe-point-box)))
    (cond ((null spb) "")
          ((box::shrink-proof? spb) "Shrinkable")
          (t "Shrink Proof"))))

(defun menu-boxtop-callback (data interface)
  (declare (ignore interface))
  (let ((spb (safe-point-box)))
    (cond ((null spb))
          ((eq data :graphics-named-boxtop)
           (box::putprop spb 'bu::boxtop :boxtop))
          ;; doen't work yet...
          ((eq data :named-graphics)
           (box::putprop spb
                         (multiple-value-bind (string cancelled?)
                             (box::get-string-from-status-line "Graphics Named: ")
                           (if cancelled?
                               'bu::boxtop
                             (boxer::intern-in-bu-package string)))
                         :boxtop))
          (t (box::putprop spb data :boxtop)))))



;; called inside the save modified box dialog. We can't just turn off the
;; flags, we also have to be careful that the superior is marked modified
;; so that it will get saved.  We need to do this even if it has already been
;; saved since it needs to be saved again to get the infs of the box we are
;; unstoring.  This requires checking the current value of *dirty-file-boxes* and
;; side-effecting it even though we are probably already iterating through it
;; see close-box-prescan for details....
(defun unstore-box (box)
  (setf (boxer::storage-chunk? box) nil)
  (boxer::mark-file-box-clean box)
  (setf (boxer::display-style-border-style (boxer::display-style-list box))
        (when (boxer::exports box) :dashed))
  ;; now make sure the superior is marked and that the it is already (or gets
  ;; placed) in *dirty-file-boxes* AFTER the box
  (let* ((remaining-fbs (boxer::fast-memq box boxer::*dirty-file-boxes*))
         (superior-box (boxer::superior-box box))
         (superior-file-box (when (boxer::box? superior-box)
                              (boxer::current-file-box superior-box))))
    (unless (null superior-file-box)
      (cond ((boxer::fast-memq superior-file-box remaining-fbs)
             ;; the superior is already dirty and will get processed eventually
             ;; so we do nothing
             )
            (t
             ;; we have to mark the superior and place it on the end of the queue
             ;; we dont use mark-file-box-dirty because that will frob the queue
             ;; in the wrong way
             (setf (boxer::file-modified? superior-file-box) t)
             (nconc remaining-fbs (list superior-file-box)))))))

;;; Box Properties...
;; the properties dialog has a popup in the upper left corner to select the
;; general category of the properties.  The props come in several categories
;; and selecting each category will update the dialog window to include dialog
;; items which correspond to the selected category (The seems to be an emergent
;; style in many mac applications which have to manage a plethora of prefs/props)
;;
;; For the PC port, we will use a tab layout instead

(defvar *box-properties-dialog*)
(defvar *current-cluster*)
(defvar *box-properties-main-items*)

(defvar *box-properties-change-list* nil
  "Changes made by the user in the box props dialog are pushed onto this list")

;; parameterize all the layout (Mac menus are plumper)
(defvar *box-props-dialog-width* #+cocoa 260 #-cocoa 200)
(defvar *box-props-dialog-height* #+cocoa 300 #-cocoa 220)

(defun queue-box-prop-change (change)
  (let ((existing (assoc (car change) *box-properties-change-list*)))
    (if (null existing)
        (push change *box-properties-change-list*)
        (setf (cdr existing) (cdr change)))))

;; vars (should be unneccessary now...)

(defvar *link-props-cluster* nil)
(defvar *link-radio-panel* nil)
(defvar *link-file-props* nil)
(defvar *link-prop-text-input* nil)

(defvar *display-props-cluster* nil)

(defvar *boxtop-props-cluster* nil)
(defvar *boxtop-named-graphic-text-input* nil)

(defun clear-props-cluster-vars ()
  (setq *link-props-cluster* nil *link-file-props* nil *link-prop-text-input* nil
        *display-props-cluster* nil
        *boxtop-props-cluster* nil *boxtop-named-graphic-text-input* nil))

;; getting items from the dialog
(defun box-props-dialog-tabs (dialog)
  (slot-value (car (capi:layout-description (car (slot-value (slot-value dialog 'capi:layout)
                                                             'capi::description)))) ; gets the TAB Layout
              'capi::items-representation))

(defun box-props-dialog-links-tab (dialog)
  (cadr (find "Links" (box-props-dialog-tabs dialog)
              :test #'(lambda (i si) (string-equal i (car si))))))

(defun box-props-dialog-sizing-tab (dialog)
  (cadr (find "Sizing" (box-props-dialog-tabs dialog)
              :test #'(lambda (i si) (string-equal i (car si))))))

(defun box-props-dialog-boxtops-tab (dialog)
  (cadr (find "Boxtops" (box-props-dialog-tabs dialog)
              :test #'(lambda (i si) (string-equal i (car si))))))

(defun link-prop-text-input-item (dialog)
  (find-if #'(lambda (x) (typep x 'capi:text-input-pane))
           (slot-value (bw::box-props-dialog-links-tab dialog)
                       'capi::description)))

(defun link-prop-file-props (dialog)
  (find-if #'(lambda (x) (typep x 'capi::check-button-panel))
           (slot-value (bw::box-props-dialog-links-tab dialog)
                       'capi::description)))

(defun link-prop-radio-buttons (dialog)
  (find-if #'(lambda (x) (typep x 'capi::radio-button-panel))
           (slot-value (bw::box-props-dialog-links-tab dialog)
                       'capi::description)))

;; there are 3 sub layouts,
;; 1. links:
(defun file-prop-select (data)
  (queue-box-prop-change (list (cadr data) t)))

(defun file-prop-retract (data)
  (queue-box-prop-change (list (cadr data) nil)))

(defun set-link-file-props (text-input-item file-props
                                            t-or-nil &optional (new-text ""))
  (dolist (fp (capi::button-group-buttons file-props))
    (setf (capi:button-enabled fp) t-or-nil))
  (cond ((null t-or-nil)
         (setf (capi:text-input-pane-enabled text-input-item) nil))
        (t
         (setf (capi:text-input-pane-enabled text-input-item) t)
         (setf (capi:text-input-pane-text text-input-item) new-text))))

(defun link-radio-button-callback (data interface)
  (let ((text-input-item (link-prop-text-input-item interface))
        (file-props (link-prop-file-props interface)))
    (cond ((eq data :nolink)
           (set-link-file-props text-input-item file-props nil)
           (queue-box-prop-change (list :link data)))
          ((eq data :file)
           (set-link-file-props
            text-input-item file-props
            t (namestring (merge-pathnames "NewFile.box"
                                           boxer::*boxer-pathname-default*)))
           (queue-box-prop-change
            (list :link data (capi:text-input-pane-text text-input-item))))
          ((eq data :url)
           (set-link-file-props text-input-item file-props t (format nil "http://"))
           (queue-box-prop-change
            (list :link data (capi:text-input-pane-text text-input-item)))))))

(defun link-text-callback (text interface)
  (let ((radio-mode (capi:item-data
                     (capi:choice-selected-item
                      (link-prop-radio-buttons interface)))))
    (when (or (eq radio-mode :file) (eq radio-mode :url))
      (queue-box-prop-change (list :link radio-mode text)))))

(defun make-link-props-cluster (box)
  (let* ((link-radio-panel
          (make-instance 'capi:radio-button-panel
                         :layout-class 'capi:column-layout
                         :callback-type :data-interface
                         :selection
                         (cond ((not (boxer::storage-chunk? box)) 0)
                               ((boxer::getprop box :url) 2)
                               (t 1)) ;default to file for storage-chunks
                         :items
                         (list
                          (make-instance 'capi:radio-button
                                         :data :nolink
                                         :text "No Link"
                                         :selection-callback
                                         'link-radio-button-callback)
                          (make-instance 'capi:radio-button
                                         :data :file
                                         :text "Link to File"
                                         :selection-callback
                                         'link-radio-button-callback)
                          (make-instance 'capi:radio-button
                                         :data :url
                                         :text "Link to URL"
                                         :selection-callback
                                         'link-radio-button-callback))))
         (link-file-props
          (make-instance 'capi:check-button-panel
                         :layout-class 'capi:column-layout
                         :callback-type :data
                         :selection-callback 'file-prop-select
                         :retract-callback   'file-prop-retract
                         :print-function 'car
                         :selection
                         (let ((selections nil))
                           (when (boxer::relative-filename? box) (push 2 selections))
                           (when (boxer::autoload-file? box) (push 1 selections))
                           (when (boxer::read-only-box? box) (push 0 selections))
                           selections)
                         :items
                         '(("Read Only Box" :read-only)
                           ("Auto Load Box" :auto-load)
                           ("Relative Filename" :relative-filename))))
         (link-prop-text-input
          (make-instance 'capi:text-input-pane
                         :text (let ((url (box::getprop box :url)))
                                 (cond (url (boxnet::urlstring url))
                                       (t (let ((f (box::getprop box
                                                                 :associated-file)))
                                            (if f (namestring f) "")))))
                         :change-callback-type :data-interface
                         :change-callback 'link-text-callback))
         (link-props-cluster
          (make-instance 'capi:column-layout
                         :description
                         (list
                          (make-instance 'capi:title-pane
                                         :text
                                         "Long term storage controls for this box")
                          link-radio-panel
                          link-file-props
                          (make-instance 'capi:title-pane :text "Link/File:")
                          link-prop-text-input))))
    (when (not (boxer::storage-chunk? box))
      (set-link-file-props link-prop-text-input link-file-props nil))
    link-props-cluster))

;; 2. display properties

;; the data consists of a string/keyword list pair

(defun display-prop-select (data)
  (queue-box-prop-change (list (cadr data) t)))

(defun display-prop-retract (data)
  (queue-box-prop-change (list (cadr data) nil)))

(defun make-display-props-cluster (box)
  (setq *display-props-cluster*
        (make-instance 'capi:column-layout
                       :description
                       (list
                        (make-instance 'capi:title-pane
                                       ;; extra spaces = crock fix to menu formatting
                                       :text "Display Properties of the Box        ")
                        (make-instance 'capi:title-pane
                                       :text "")
                        (make-instance 'capi:check-button-panel
                                       :layout-class 'capi:column-layout
                                       :callback-type :data
                                       :selection-callback 'display-prop-select
                                       :retract-callback 'display-prop-retract
                                       :print-function 'car
                                       :selection
                                       (let ((selections nil))
                                         (when (boxer::always-zoom? box)
                                           (push 0 selections))
                                         (when (boxer::shrink-on-exit? box)
                                           (push 1 selections))
                                         (when (box::bottom-right-hotspot-active?
                                                box)
                                           (push 2 selections))
                                         (when (boxer::auto-fill? box)
                                           (push 3 selections))
                                         (when (boxer::shrink-proof? box)
                                           (push 4 selections))
                                         selections)
                                       :items
                                       '(("Always Zoom to Fullscreen" :always-zoom)
                                         ("Shrink on Exit" :shrink-on-exit)
                                         ("Manual Sizing" :manual-size)
                                         ("Auto Fill Text" :autofill)
                                         ("Shrink Proof" :shrink-proof)))))))

;; 3. boxtop properties

(defun boxtop-props-text-input-item (dialog)
  (find-if #'(lambda (x) (typep x 'capi:text-input-pane))
           (slot-value (bw::box-props-dialog-boxtops-tab dialog)
                       'capi::description)))

(defvar *boxtop-choice* nil)

(defun boxtop-basic-prop-select (data interface)
  (let ((text-input (boxtop-props-text-input-item interface)))
    (unless (null text-input) (setf (capi:text-input-pane-enabled text-input) nil))
    (queue-box-prop-change (list :boxtop data))))

(defun boxtop-named-graphic-prop-select (data interface)
  (declare (ignore data))
  (let ((text-input (boxtop-props-text-input-item interface)))
    (unless (null text-input) (setf (capi:text-input-pane-enabled text-input) T))
    (queue-box-prop-change (list :boxtop :named-graphic
                                 (boxer::intern-in-bu-package
                                  (string-upcase
                                   (capi:text-input-pane-text text-input)))))))

(defun boxtop-named-graphic-name-select (text)
  (queue-box-prop-change (list :boxtop :named-graphic
                               (boxer::intern-in-bu-package (string-upcase text)))))

;; note: should diable text-input on init when named graphics is not selection
(defun make-boxtop-props-cluster (box)
  (make-instance 'capi:pinboard-layout
                 :min-width (- *box-props-dialog-width* 20)
                 :min-height (- *box-props-dialog-height* 70)
                 :description
                 (list
                  (make-instance
                   'capi:title-pane :x 0 :y 0 ;:min-width 150 :min-height 30
                   :text "Icon for shrunken box")
                  (make-instance 'capi::radio-button-panel :x 0 :y 40
                                 :layout-class 'capi::column-layout
                                 :callback-type :data-interface
                                 :selection-callback 'boxtop-basic-prop-select
                                 :selection
                                 (let ((current-boxtop (boxer::getprop box :boxtop)))
                                   (cond ((eq current-boxtop ':framed) 1)
                                         ((eq (symbol-package current-boxtop)
                                              boxer::pkg-bu-package) 2)
                                         ((eq current-boxtop ':folder) 3)
                                         ((eq current-boxtop ':name-only) 4)
                                         (t 0)))
                                 :items
                                 (list
                                  (make-instance 'capi:radio-button
                                                 :data :standard
                                                 :text "Standard Boxtop")
                                  (make-instance 'capi:radio-button
                                                 :data :framed
                                                 :text "Framed Boxtop")
                                  (make-instance 'capi:radio-button
                                                 :data :named-graphics
                                                 :text "Graphic Named"
                                                 :selection-callback
                                                 'boxtop-named-graphic-prop-select)
                                  (make-instance 'capi:radio-button
                                                 :data :folder
                                                 :text "Folder Picture")
                                  (make-instance 'capi:radio-button
                                                 :data :name-only
                                                 :text "Name Only")))
                  (make-instance 'capi:text-input-pane
                                 :x #+cocoa 110 #-cocoa 100
                                 :y #+cocoa 90 #-cocoa 70 :text "Boxtop"
                                 :change-callback-type :data
                                 :change-callback
                                 'boxtop-named-graphic-name-select))))


;; putting it all together

(defun make-box-props-dialog (box)
  (let ((link-props-cluster (make-link-props-cluster box))
        (display-props-cluster (make-display-props-cluster box))
        (boxtop-props-cluster (make-boxtop-props-cluster box)))
    (capi:make-container
     (make-instance
      'capi:column-layout
      :adjust :center
      :description
      (list
       (make-instance 'capi:tab-layout
                      :print-function 'car :visible-child-function 'cadr
                      :items (list
                              (list "Links"   link-props-cluster)
                              (list "Sizing"  display-props-cluster) ; "Display"
                              (list "Boxtops" boxtop-props-cluster)))
       (make-instance 'capi:row-layout
                      :adjust :center
                      :description
                      (list
                       (make-instance 'capi:push-button :x 10 :y 10
                                      :width 75 :height 20
                                      :text "Cancel"
                                      :selection-callback 'capi:abort-dialog)
                       (make-instance 'capi:push-button :x 120 :y 10
                                      :width 75 :height 20
                                      :text "OK"
                                      :selection-callback
                                      #'(lambda (&rest ignore)
                                          (declare (ignore ignore))
                                          (capi:exit-dialog
                                           *box-properties-change-list*)))))
       nil)))))

(boxer::defboxer-command boxer::com-edit-box-properties (&optional
                                                  (box boxer::*hotspot-mouse-box*))
  "Edit properties of a box"
  (unwind-protect
      (let ((ok? (capi:display-dialog (make-box-props-dialog box)
                                      :owner *boxer-pane*)))
        ;; *box-properties-change-list* is now returned by display dialog
        ;; because (on the mac) it is bound in the pane process
        (when ok?
          (dolist (change *box-properties-change-list*)
            (case (car change)
              (:link
               (ecase (cadr change)
                 ((:file :url)
                  (unless (boxer::storage-chunk? box)
                    (boxer::mark-box-as-file box)
                    (boxer::mark-file-box-dirty box)
                    (boxer::modified box))
                  ;; now specific stuff
                  (cond ((eq (cadr change) :url)
                         (boxer::removeprop box :associated-file)
                         ;; need reality checking here...
                         (boxer::putprop
                          box (boxnet::make-url-from-string (caddr change)) :url))
                        (t
                         (boxer::removeprop box :url)
                         (boxer::putprop
                          box (pathname (caddr change)) :associated-file))))
                 (:nolink
                  (unless (null (boxer::storage-chunk? box))
                    ;; need some cleanup action.  Note: it is possible to get here
                    ;; with an originally non storage box if the user clicks on
                    ;; "file" or "url" then clicks back to "no link"
                    (boxer::mark-file-box-clean box)
                    (boxer::unmark-box-as-file box)
                    (boxer::modified box)))))
              (:auto-load (boxer::set-autoload-file? box (cadr change))
                          (boxer::mark-file-box-dirty box))
              ;; should we mark the file box dirty flag for such minor changes ?
              (:always-zoom  (boxer::set-always-zoom?  box (cadr change)))
              (:shrink-on-exit (boxer::set-shrink-on-exit? box (cadr change)))
              (:manual-size (cond ((null (cadr change))
                                   (boxer::com-mouse-toggle-br-hotspot box)
                                   (boxer::com-unfix-box-size box))
                                  (t (boxer::com-mouse-toggle-br-hotspot box))))
              (:autofill (boxer::set-auto-fill? box (cadr change))
                         (when (and (boxer::data-box? box)
                                    ;; is it ever the right thing to fill doit boxe?
                                    (boxer::display-style-fixed-wid
                                     (boxer::display-style-list box)))
                           (boxer::com-fill-box box)))
              (:shrink-proof (boxer::set-shrink-proof? box (cadr change)))
              (:read-only (boxer::set-read-only-box? box (cadr change))
                          (boxer::mark-file-box-dirty box))
              (:relative-filename (boxer::set-relative-filename? box (cadr change))
                                  (boxer::mark-file-box-dirty box))
              (:boxtop (cond ((eq (cadr change) :named-graphic)
                              (boxer::putprop box (let ((string (caddr change)))
                                             (if (null string) 'bu::boxtop
                                                 (boxer::intern-in-bu-package
                                                  (string-upcase string))))
                                             :boxtop))
                             (t (boxer::putprop box (cadr change) :boxtop))))))))
    (setq *box-properties-change-list* nil))
  (clear-props-cluster-vars)
  boxer-eval::*novalue*)
