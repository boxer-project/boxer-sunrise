;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    File Operation Editor Commands



Modification History (most recent at top)

11/07/13 com-export-box-as added :if-does-not-exist :ok
 2/17/12 *name-link-boxes* controls whether com-open-link-file creates boxes with same name as file
 2/ 1/12 com-export-box-as moved here from impexp.lisp so system can recompile cleanly
12/29/11 change com-open-link-file to use open-xref-file-dialog
12/12/11 com-open-link-file
 5/31/08 no more drawing-on-port in com-open-box-file (leads to deadlock in opengl)
 9/14/05 com-save-document & com-box-save-as: read-only flag was being
         improperly reset
 8/20/05 changed file-overwrite-ok to use my-y-or-n-dialog which is smarter about
         spacing around the button text
 8/08/05 added set-file-box-properties to com-box-save-as
 7/17/05 set-file-box-properties added as a hook for initializing new file
         boxes, for now, just sets always-zoom? to T
 6/27/04 moved the with-hilited-box to be around calls to save because
         OSX file dialog messed it up when there was only the single use
         wrapped around multiple save calls as well as the file dialog
10/28/03 com-box-save-as, com-save-document changed to use with-hilited-box
 4/21/03 merged current LW and MCL files
 9/02/02 added com-toggle-modified-flag
 2/13/01 merged current LW and MCL files
 1/19/00 Lispworks changes-mostly removing defaults for new functions
 4/20/99 com-box-save-as, com-save-document, close-box-prescan, com-close-box
         check for outlink ports
 2/23/99 added no filename warning to quiet option of com-save-document
 2/11/99 com-unfile-document
 2/10/99 com-save-document supports nowarnings? option
 9/29/98 com-save-document will suppress read-only flag when asking for an
         explicit filename
 9/13/98 com-open-box-file will bash the NAME slot to nil if the box looks
         like it was a saved top level world
 9/11/98 com-save-document and com-save-box-as: When saving a Net box as a file,
         make sure :URL prop is removed BEFORE call to save-generic
 9/08/98 added check for existing directory in com-box-save-as and com-save-document
         before using existing filename as a default in prompting for a new pathname

9/08/98 Started Logging changes: source = boxer version 2.3beta


|#

(in-package :boxer)


;;; minimal implementation, see mac-menu for more complete ones...

;; use this if it looks like you might be overwriting a
;; file and want to  know if it is ok
(defun file-overwrite-ok (reason)
  #+mcl
  (my-y-or-n-dialog reason :yes-text "Save" :no-text "Don't Save":cancel-text nil)
  #+lispworks
  (capi::prompt-for-confirmation reason)
  #-(or lispworks mcl)
  (status-line-y-or-n-p reason))

(defun box-save-name (box)
  (let ((raw-name (slot-value box 'name)))
    (cond ((stringp raw-name) raw-name)
          ((name-row? raw-name) (text-string raw-name))
          ((null raw-name) "unnamed")
          (t "???"))))

(defun check-for-outlink-ports (box)
  (let* ((bls (branch-links box))
         (pbls (with-collection
                 (dolist (bl bls)
                   (unless (eq (link-type bl) 'target-branch-link)
                     (collect bl))))))
    (unless (or (null *warn-about-outlink-ports*) (null pbls))
      (outlink-port-dialog box :nports (length pbls)))))



(defboxer-command com-box-save-as (&optional always-ask-for-filename? box)
  "Saves the current box into a file"
  (catch 'cancel-boxer-file-dialog
    (boxer-eval::report-eval-errors-in-editor
      (let ((what (or box (box-point-is-in))))
        (check-for-outlink-ports what)
        (with-hilited-box (what)
          (let ((existing-filename (getprop what :associated-file)))
            (multiple-value-bind (filename format read-only?)
                (if (or always-ask-for-filename? (null existing-filename))
                  (boxer-new-file-dialog
                   :prompt (format nil "Save ~A box into file:"
                                   (box-save-name what))
                   ;; need to check for dir to avoid blowouts
                   :directory (let ((existing-dir (unless (null existing-filename)
                                                    (make-pathname
                                                     :directory
                                                     (pathname-directory
                                                      existing-filename)))))
                                (cond ((or (null existing-filename)
                                           (not (probe-file existing-dir)))
                                       nil)
                                      ((read-only-box? what)
                                       (directory-namestring existing-filename))
                                      (t existing-filename)))
                   :box what)
                  (values existing-filename
                          (or (getprop what :preferred-file-format) :boxer)
                          (read-only-box? box)))
              (when (not (null (getprop what :url)))
                ;; if there is a URL, we must remove it BEFORE saving as
                ;; a file or else dump-box will black box it
                (removeprop what :url))
              (save-generic what filename
                            :format format :read-only? read-only?)
              ;; update the preferred file format and read only properties
              (let ((existing-ff (getprop what :preferred-file-format)))
                (when (and (neq existing-ff format)
                           (not (and (eq format :boxer) (null existing-ff))))
                  (putprop what format :preferred-file-format))
                (when (neq read-only? (read-only-box? what))
                  (setf (read-only-box? what) read-only?)))
              (unless (storage-chunk? what)
                ;; set file props only if we are transitioning
                (set-file-box-properties what))
              (mark-box-as-file what filename)
              ;; Add to Open Recents...
              (add-recent-file filename)
              (mark-file-box-clean what)
              (modified what)))))))
  boxer-eval::*novalue*)


;; a "document" is whatever the (current-file-box) is
;; 6/27/04 moved the with-hilited-box to be around calls to save because
;; OSX file dialog messed it up when there was only the single use wrapped around
;; multiple save calls as well as the file dialog
(defboxer-command com-save-document (&optional always-ask-for-filename? nowarnings?)
  "Saves the current file box"
  (catch 'cancel-boxer-file-dialog
    (boxer-eval::report-eval-errors-in-editor
      (let* ((box (current-file-box))
             (raw-filename (getprop box :associated-file))
             (dialog-prompt (format nil "Save ~A box into file:"
                                    (box-save-name box)))
             ;; careful, the filename might be relative
             (existing-filename (cond ((null raw-filename) nil)
                                      ((eq (car (pathname-directory raw-filename))
                                           :absolute)
                                       raw-filename)
                                      (t ; must be relative
                                       (let* ((sup-file-box (current-file-box
                                                             (superior-box box)))
                                              (sup-filename (getprop
                                                             sup-file-box
                                                             :associated-file)))
                                         (cond ((not (null sup-filename))
                                                (merge-pathnames raw-filename
                                                                 sup-filename))
                                               (t ; can't resolve relative path
                                                ;; so set things up for prompting
                                                (Setq always-ask-for-filename? t
                                                      dialog-prompt
                                                      "Can't merge, Save into file:")
                                                raw-filename))))))
             (existing-url (getprop box :url)))
        (check-for-outlink-ports box)
        (cond ((and existing-url (not always-ask-for-filename?))
               (with-hilited-box (box)
                 (boxnet::save-box-using-url existing-url box)))
              ((and nowarnings? (null existing-filename))
               ;; At least give a warning, error seems inappropriate
               ;; from inside a quietly-save
               (boxer-editor-warning
                "Box has no filename. Box not saved"))
              (nowarnings?
               (when (not (null existing-url))
                 ;; if there is a URL, we must remove it BEFORE
                 ;; saving as a file or else dump-box will black box it
                 (removeprop box :url))
               (with-hilited-box (box)
                 (save-generic box existing-filename
                               :format (or (getprop
                                            box :preferred-file-format)
                                           :boxer)
                               :always-save? T)))
              (t
               (multiple-value-bind (filename format read-only?)
                   (if (or always-ask-for-filename? (null existing-filename))
                     (boxer-new-file-dialog
                      :prompt dialog-prompt
                      ;; need to check for the dir to avoid
                      ;; blowouts on the Mac (and maybe others)
                      :directory
                      (let ((existing-dir (unless (null
                                                   existing-filename)
                                            (make-pathname
                                             :directory
                                             (pathname-directory
                                              existing-filename)))))
                        (when (and existing-dir
                                   (probe-file existing-dir))
                          existing-filename))
                      ;; suppress read only if we are explicitly
                      ;; asking for a new filename
                      :box box
                      :no-read-only? always-ask-for-filename?)
                     (values existing-filename
                             (or (getprop box :preferred-file-format) :boxer)
                             (read-only-box? box)))
                 (when (not (null existing-url))
                   ;; if there is a URL, we must remove it BEFORE
                   ;; saving as a file or else
                   ;; dump-box will black box it
                   (removeprop box :url))
                 (with-hilited-box (box)
                   (save-generic box filename
                                 :format format :read-only? read-only?))
                 ;; update the preferred file format and
                 ;; read only properties
                 (let ((existing-ff (getprop box
                                             :preferred-file-format)))
                   (when (and (neq existing-ff format)
                              (not (and (eq format :boxer)
                                        (null existing-ff))))
                     (putprop box format :preferred-file-format))
                   (when (neq read-only? (read-only-box? box))
                     (setf (read-only-box? box) read-only?)))
                 (unless (eq existing-filename filename)
                   (putprop box filename :associated-file))
                 (add-recent-file filename))))
        (mark-file-box-clean box))))
  boxer-eval::*novalue*)

(defboxer-command com-open-box-file (&optional explicit-redisplay file-to-open)
  "Inserts the contents of a Boxer file"
  (if (name-row? (point-row))
    (boxer-editor-error "Can't insert a (file) box while in a name")
    (progn
      (catch 'cancel-boxer-file-dialog
        (boxer-eval::report-eval-errors-in-editor
          ;; (with-drawing-port *boxer-pane*
          (let* ((filename (or file-to-open (boxer-open-file-dialog)))
                 (box (read-internal filename)))
            (when (box? box)
              ;; if this was previously, a saved world box, we need to
              ;; fix up the name slot
              (multiple-value-bind (ro? world-box?)
                  #+mcl (boxer-file-info filename) ;; looks in resource fork
                #-mcl (values nil nil)
                (declare (ignore ro?))
                (when (or world-box?
                          (and (stringp (slot-value box 'name))
                               (string= (slot-value box 'name) "WORLD")))
                  (setf (slot-value box 'name) nil)))
              (insert-cha *point* box)
              ;; Add the newly opened file to the recents list
              (add-recent-file filename)
              (when explicit-redisplay (repaint))))))
      ;; this marks the box superior to the box being loaded
      (mark-file-box-dirty (point-row))))
  boxer-eval::*novalue*)

;; the default should set always-zoom,
;; hook for something more elaborate, later
;; should we keep this separate or fold it into mark-box-as-file ?
;; for now, leave separate since we'll want to make it interact with
;; initialize-new-box-properties
(defun set-file-box-properties (box)
  (set-always-zoom? box t))

(defboxer-command com-new-file-box (&optional explicit-redisplay)
  "makes a File (data) box at the cursor location."
  (let ((region-to-box (or *region-being-defined* (get-current-region))))
    (reset-editor-numeric-arg)
    (cond ((name-row? (point-row))
	   (reset-region)
	   (boxer-editor-error "You cannot make boxes inside a name"))
	  ((not (null region-to-box))
           (let ((box (make-initialized-box :type 'data-box)))
             (set-file-box-properties box)
             (kill-region region-to-box)
	     (insert-cha *point* box ':fixed)
             ;; mark the box before entering
             (mark-file-box-dirty (point-row))
	     (com-enter-box)
	     (yank-region *point* region-to-box)
	     (flush-region region-to-box)
             (mark-box-as-file box)
	     (exiting-region-mode)))
          (t
           (let ((box (make-initialized-box :type 'data-box)))
             (set-file-box-properties box)
             (mark-box-as-file box)
             (set-type box 'data-box)
             (insert-cha *point* box ':fixed)
             ;; mark the containing box before entering
             (mark-file-box-dirty (point-row))
             (com-enter-box)))))
  (when explicit-redisplay (repaint))
  boxer-eval::*novalue*)

;; rewrite this to CONS less later....
(defmethod editor-location-string ((box box))
  (let* ((sb (superior-box box))
         (nr (name-row box))
         (local-string (if (null nr) "[]" (text-string nr))))
    (cond ((null sb) local-string)
          (t (concatenate 'string (editor-location-string sb) "." local-string)))))

(defun modified-boxes-for-close (&optional (box *initial-box*))
  "Returns a subset of the boxes in *dirty-file-boxes*. Essentially, we only boxes which
  are still connected hierarchicly below the *initial-box*. Occasionally the dirty file list
  will contain some boxes which are no longer in the structure."
  (remove-if-not #'(lambda (fb)
                     (and (not (eq fb *initial-box*)) (superior? fb *initial-box*)))
                 *dirty-file-boxes*))

;; look for modified inferior file boxes and offer to save them
;; should also look for RO boxes with no pathname
(defun close-box-prescan (box)
  (do-dirty-file-boxes (fb)
    (when (and (not (eq fb box)) (superior? fb box))
      (cond ((not (read-only-box? fb))
             (check-for-outlink-ports fb)
             (save-modified-box-dialog fb :prompt-start "The sub box,"))
            ((null (getprop fb :associated-file))
             ;; a read only box which doesn't have a filename associated
             (add-path-to-ro-box-dialog fb))))))


(defboxer-command com-close-box ()
  "Exits and Closes the current file box, making it black again"
  (let* ((box (current-file-box))
         (destination (superior-box box)))
    (catch 'cancel-boxer-file-dialog
      ;; first check all the inferiors....
      (close-box-prescan box)
      ;; now check for modification of the box and offer to save if so
      (when (file-modified? box)
        (check-for-outlink-ports box)
        (save-modified-box-dialog box :prompt-start "The main box,"
                                  :no-zoom-offered? t))
      (cond ((eq box *initial-box*)
             ;; forbid this for now, later we might want to kil the window
             ;; first need to hack OPEN to possibly create a window
             (boxer-editor-warning "Sorry, Can't Close the Top Level Box"))
            (t
             ;; now exit the file box
             ;; this will bring us up to the same level as the file box
             (loop (if (or (superior? box (point-box))
                           (null (superior-box (point-box))))
                       (return nil)
                       (com-exit-box)))
             ;; now one more exit
             (com-exit-box)
             ;;we should now be at the next higher level, next to the file box
             ;; now close the box (deallocating infs)
             (when (eq (point-box) destination)
               ;; make sure we are really where we think we are before removing stuff
               (shrink box)
               (handle-boxtop-for-close box)
               (let ((rows (rows box)))
                 (kill-box-contents box)
                 (queue-editor-objs-for-deallocation rows))))))
    boxer-eval::*novalue*))

;; this should be installed in the file menu as "Export..."

(defboxer-command com-export-box-as (&optional (ffc *default-ffc*))
  "Write the contents of a box out in a different format"
  (catch 'cancel-boxer-file-dialog
    (let ((filename (capi:prompt-for-file "Export box to..."
                                          :filter (ffc-file-filter ffc)
                                          :filters *foreign-file-filters*
                                          :operation :save
                                          :if-exists :ok
                                          :if-does-not-exist :ok
                                          :owner *boxer-frame*)))
      ;; choose export type...
      (unless (null filename) ;; If the user hit cancel on the file dialog, filename is nil
        (with-hilited-box ((point-box))
          (writing-foreign-file (stream ffc filename)
            (export-box-internal (point-box) ffc stream)))))))

;; if we are closing a box with a boxtop, need to put the boxtop onto the
;; :cached-boxtop prop for the box since the inferiors will get deallocated
(defmethod handle-boxtop-for-close ((box box))
  (let ((boxtop (boxtop box)))
    (when (graphics-sheet? boxtop)
      (let ((new-bt (copy-graphics-sheet boxtop box)))
        ;; use a new graphic sheet because the old one is going to get deallocated
        (putprop box new-bt :cached-boxtop)))))

(defboxer-command com-toggle-modified-flag ()
  "Toggles the file modified status of the current document"
  (cond ((file-box-dirty? (point-box))
         (mark-file-box-clean (point-box)))
        (t (mark-file-box-dirty (point-box))))
  (modified (point-box))
  boxer-eval::*novalue*)

(defboxer-command com-unmodify-document ()
  "Clears the file modified status of the current document"
  (mark-file-box-clean (point-box))
  ;; not neccessary now that only the title bar indicated mod status but will
  ;; be needed if we ever have visual indicators on the box border
  (modified (point-box))
  boxer-eval::*novalue*)

(defboxer-command com-unfile-document ()
  "Removes the file status of the current document"
  (let ((where (if (box? boxer-eval::*lexical-variables-root*)
                   boxer-eval::*lexical-variables-root*
                   (point-box))))
    (mark-file-box-clean where)
    (unmark-box-as-file (current-file-box where))
    (modified where)
    boxer-eval::*novalue*))



;;; file commands from the type tag popup

(defboxer-command com-tt-toggle-type (&optional (box *hotspot-mouse-box*))
  "Toggle the box type from the type tag pop up menu"
  (when (box? box) (toggle-type box))
  (mark-file-box-dirty box)
  (modified box)
  boxer-eval::*novalue*)

(defboxer-command com-tt-toggle-storage-chunk (&optional (box *hotspot-mouse-box*))
  "Toggle the Storage chunk flag.  Indicated by box border thickness"
  (when (box? box)
    (let ((old-s (storage-chunk? box))
          (exports? (not (null (slot-value box 'exports)))))
      (cond (old-s ; toggling to non-file
             ;; inform other bookkeeping about it, has to happen before the
             ;; storage-chunk? flag is cleared...
             (mark-file-box-clean box)
             (setf (storage-chunk? box) (not old-s))
             ;; now hack the border
             (set-border-style box (if exports? :dashed nil)))
            (t ; toggling to file
             ;; need to set the storage-chunk? flag before marking
             (setf (storage-chunk? box) (not old-s))
             (mark-file-box-dirty box)
             (set-border-style box (if exports? :thick-dashed :thick))))
      ;; do we need to frob props ? (:url, :assocated-filename and format)
      (modified box)))
  boxer-eval::*novalue*)

;; relatively simple because there aren't concrete indicators for these states
(defboxer-command com-tt-toggle-read-only (&optional (box *hotspot-mouse-box*))
  "Toggle the Read Only Flag"
  (when (box? box)
    (setf (read-only-box? box) (not (read-only-box? box)))
    (mark-file-box-dirty box))
  boxer-eval::*novalue*)

(defboxer-command com-tt-toggle-autoload-file (&optional (box *hotspot-mouse-box*))
  "Toggle the Auto Load Flag"
  (when (box? box)
    (setf (autoload-file? box) (not (autoload-file? box)))
    (mark-file-box-dirty box))
  boxer-eval::*novalue*)



(defvar *name-link-boxes* t
  "When T, give link boxes the same name as the filename, NIL for unnamed link boxes")

(defboxer-command com-open-link-file (&optional explicit-redisplay)
  "Inserts the contents of a Boxer file"
  (if (name-row? (point-row))
    (boxer-editor-error "Can't insert a (file) box while in a name")
    (progn
      (catch 'cancel-boxer-file-dialog
        (boxer-eval::report-eval-errors-in-editor
          ;; (with-drawing-port *boxer-pane*
          (let* ((filename (open-xref-file-dialog))
                 (xbox (make-xfile-box filename)))
            (when *name-link-boxes*
              (set-name xbox (make-name-row (list (pathname-name filename)))))
            (insert-cha *point* xbox)
            (when explicit-redisplay (repaint)))))
      ;; this marks the box superior to the box being loaded
      (mark-file-box-dirty (point-row))))
  boxer-eval::*novalue*)
