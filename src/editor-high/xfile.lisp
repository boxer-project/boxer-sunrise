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



    External File reference for the Mac

    in the OpenGL MacOS version of Boxer, Applescript is used to communicate with these
    external files.  See applescript.lisp


Modification History (most recent at top)

 1/ 7/12 added xref scoping (get-xref)
12/20/11 finished porting edit-mac-file-ref to edit-xref-file-ref
12/09/11 added active-info slot to mac-file-ref's
 6/06/05 #+lispworks fill-icon-cache had wrong args
 7/07/04 fixed typo in fill-icon-cache
 4/05/04 make-xfile-box, fill-icon-cache updated for OSX
 5/21/01 set-xref-boxtop-info fill-icon-cache
 1/29/01 initial lispworks version
11/17/99 find-xref-retry-path: null pathname type now defaults to :unspecific
10/14/99 set-xref-boxtop-info uses find-xref-retry-path to (possibly) fixup
         the xref pathname
 5/19/99 :change option in edit-mac-file-ref now hacks change to box file
 4/28/99 support :move option in edit-mac-file-ref
 4/11/99 edit-mac-file-ref
 1/29/99 set-xref-boxtop-info probes for xref path in case it doesn't exist
 1/23/99 moved file sys hooks to dumper.lisp because of system load dependencies
 1/12/99 added hooks for VC (copying & printing) and file operations
12/07/98 started logging changes


|#

(in-package :boxer)



;; pathname refers to the mac file's pathname
;; icon-cache caches the Finder icon image as a pixmap
;; mime-type is used if the pathname is not mac typed (e.g. a temporary file
;; from a base64 mail stream) as a hint on which mac app to launch
;; NOTE: used to be called MAC-FILE-REF
(defstruct xref
  (pathname nil)
  (icon-cache nil)
  (mime-type nil)
  (active-info nil))  ; in LWM, will be an applescript container reference string
                      ; the result of calling "open" on the pathname


(defvar *terse-xfile-doc* nil)
(defvar *terse-xfile-makebox-doc* '(("This Box is connected to")
                                    ("a non boxer file")))

(defvar *verbose-xfile-makebox-doc* '(("This Box is connected to")
                                      ("a non boxer file")
                                      ()
                                      ("A Double click in or on this box")
                                      ("will launch an Application")
                                      ("to handle the referenced document")
                                      ()
                                      ("Single Click in or on this box")
                                      ("to change the document reference")))

;; makes a box, preshrunken
;; with a mac file icon and
;; a mouse-click boxes to launch or edit the file
;; NOTE: pathname can point to a boxer box, which needs to be handled specially
(defun make-xfile-box (pathname &optional mime-info)
  (let* ((box (make-box (if *terse-xfile-doc*
                          *terse-xfile-makebox-doc*
                          *verbose-xfile-makebox-doc*)
                        'data-box))

         (xref (make-xref :pathname pathname)))
    (cond ((null mime-info)
           ;; look for an icon
           #+(and mcl (not carbon-compat))
           (let* ((creator (ccl::mac-file-creator pathname))
                  (ftype   (ccl::mac-file-type    pathname))
                  (icon    (ccl::file-icon creator ftype)))
             (unless (null icon)
               (setf (xref-icon-cache xref) icon)))
           #+(or carbon-compat opengl)
           (let ((icr (get-iconref-from-file pathname)))
             (when (iconref? icr) (setf (xref-icon-cache xref) icr)))
           )
          (t (setf (xref-mime-type xref) mime-info)))
    (add-xref-closet-boxes box)
    (putprop box xref :xref)
    (set-xref-boxtop-info box)
    (shrink box)
    box))

;; stubs, real version should eventually go into cocoa.lisp
(defun get-iconref-from-file (pathname)
  (declare (ignore pathname))
  nil)

(defun iconref? (thing) (declare (ignore thing)) nil)

(defun add-xref-closet-boxes (box)
  (let ((boxer-eval::*warn-about-primitive-shadowing* nil) ; suppress warnings....
        (closet-boxes (list (make-box '(("Edit-Internal-Xref"))
                                      'doit-box
                                      "Mouse-Click")
                            (make-box '(("Launch-Internal-Xref"))
                                      'doit-box
                                      "Mouse-Double-Click")))
        (existing-closet (slot-value box 'closets)))
    (if (null existing-closet)
        (add-closet-row box (make-row closet-boxes))
        (dolist (cb closet-boxes) (append-cha existing-closet cb)))))


;; looks up the mac creator and file type for PATHNAME and
;; set the boxtop info for BOX to reflect this info
(defun set-xref-boxtop-info (box &optional creator ftype)
  (let ((xref (getprop box :xref)))
    (cond ((null xref)
           (error "The box, ~A, has no external file reference" box))
          (t
           (fill-icon-cache box xref creator ftype)
           ;; set the boxtop to use the xref
           (putprop box :xref :boxtop)))))

;;; **** Stub
#+lispworks
(defun fill-icon-cache (box xref creator ftype)
  (declare (ignore box xref creator ftype))
  nil)


#+mcl
(defun fill-icon-cache (box xref creator ftype)
  (let ((path (mac-file-ref-pathname xref)))
    ;; see if we can fill the icon cache
    (cond ((null path))
          ((probe-file path)
           ;; it is possible to have a bad path, especially when reading
           ;; in boxes from other machines which may have other machine
           ;; dependent pathnames in them
           (setf (mac-file-ref-icon-cache xref)
                 #-carbon-compat
                 (ccl::file-icon (or creator
                                     (when path
                                       (ccl::mac-file-creator path)))
                                 (or ftype
                                     (when path
                                       (ccl::mac-file-type  path))))
                 #+carbon-compat
                 (let ((icr (if (and creator ftype)
                              (get-iconref creator ftype)
                              (get-iconref-from-file path)
                              )))
                   (when (iconref? icr) icr))
                 ))
          (t
           ;; should probably try and do somethign reasable here
           ;; If the containing box and the xref has been moved via the
           ;; Finder, then the pathname in the boxer file will be
           ;; incorrect, so try looking in the directory of the
           ;; containing box
           (let* ((cfb (current-file-box box))
                  (cfb-path (or (and cfb (getprop cfb :associated-file))
                                *autoloading-namestring*))
                  (retry-path (and cfb-path
                                   (find-xref-retry-path path cfb-path))))
             (when (and retry-path (probe-file retry-path))
               (setf (mac-file-ref-pathname xref) retry-path)
               (setf (mac-file-ref-icon-cache xref)
                     #-carbon-compat
                     (ccl::file-icon (or creator
                                         (ccl::mac-file-creator retry-path))
                                     (or ftype
                                         (ccl::mac-file-type  retry-path)))
                     #+carbon-compat
                     (let ((icr (if (and creator ftype)
                                  (get-iconref creator ftype)
                                  (get-iconref-from-file retry-path))))
                       (when (iconref? icr) icr))
                     )))))))


;; similiar to the pathname search function in fill-box-from-local-file
;; except we don't need to hack the relative pathname case
;; NOTE: reverse order of search between sup-path and *autoloading-namestring* ?
(defun find-xref-retry-path (xref-path sup-path)
  (let ((retry-path (merge-pathnames (file-namestring xref-path) sup-path)))
    (when (probe-file retry-path) (return-from find-xref-retry-path retry-path)))
  ;; possible subdirectory wins
  (do* ((subdirs (reverse (cdr (pathname-directory xref-path)))
                 (cdr subdirs))
        (subdir (car subdirs) (car subdirs))
        (newsubdirs (list :relative subdir)
                    (list* :relative subdir (cdr newsubdirs))))
       ((null subdir) nil)
    (let ((retry-path (merge-pathnames
                       (make-pathname :directory newsubdirs
                                       :name (pathname-name xref-path)
                                      :type (or (pathname-type xref-path) :unspecific))
                       sup-path)))
      (when (probe-file retry-path) (return retry-path)))))

(defmethod os-open-xref ((self xref))
  "Open/launch the xref file in it's corresponding application."
  ;; TODO sgithens 2024-10-16 add linux/win32/webgl support
  (external-program:run "open" (list (xref-pathname self))))

;; ==> Single click
#+lispworks
(defun edit-xref (box &optional (xref (getprop box :xref)))
  (when (null xref)
    (let ((new-xref (make-xref)))
      (putprop box new-xref :xref)  (setq xref new-xref)))
  (case (open-xref-dialog (xref-pathname xref))
    (:edit (com-enter-box box (car (displayed-screen-objs box))))
    (:open (cond ((null (xref-pathname xref))
                  (boxer-eval::primitive-signal-error :mac-interface
                                                "No file to launch"))
                 ((not (probe-file (xref-pathname xref)))
                  (boxer-eval::primitive-signal-error :mac-interface
                                                "File not Found"
                                                (xref-pathname xref)))
                 (t
                  (os-open-xref xref))))
    (:change
     (let ((newpath (open-xref-file-dialog)))  ;; should we use :directory (xref-pathname xref)
       ;; newpath can point to a boxer file which should be handled
       ;; specially i.e. change to a file box
       (cond ((boxer-file-contents? newpath)
              (remove-xfile-props box)
              (mark-box-as-file box newpath))
             (t
              (applescript-new-path-for-xref xref newpath)
              (set-xref-boxtop-info box)))
       (modified box)))
    (:move
     (let ((newpath (boxer-new-file-dialog :prompt "Move the Link file to:"
                                           :directory (xref-pathname xref))))
       (rename-file (xref-pathname xref) newpath)
       (applescript-moved-path-for-xref xref newpath)))
    (t ;; can come here if CANCELed from the dialog
       )))

(defun remove-xfile-props (box)
  (removeprop box :xref)
  (setf (slot-value box 'first-inferior-row) nil)
  (let ((mcb  (boxer-eval::lookup-static-variable-in-box-only box 'bu::mouse-click))
        (mdcb (boxer-eval::lookup-static-variable-in-box-only box
                                                        'bu::mouse-double-click)))
    (delete-cha (superior-row mcb)  mcb)
    (delete-cha (superior-row mdcb) mdcb)))

;;;; Hooks

(defun xref-dump-plist-length (box)
  (let ((xref (getf (plist box) :xref)))
    (if (null xref) 0 2)))

(defun xref-dump-plist-internal (box stream)
  (let* ((xref (getf (plist box) :xref))
         (mime-vals (when xref (xref-mime-type xref))))
    (cond ((null xref))
          ((null mime-vals)
           (dump-boxer-thing :xref stream)
           (dump-boxer-thing (namestring (xref-pathname xref)) stream))
          (t
           (dump-boxer-thing :xref stream)
           ; fake a dump-list
           (dump-list-preamble 2 stream)
           (dump-boxer-thing (namestring (xref-pathname xref)) stream)
           (dump-boxer-thing mime-vals stream)))))

(defun copy-xref-special-property (from-box to-box)
  (let ((xref (getf (plist from-box) :xref)))
    (unless (null xref)
      (putprop to-box (copy-xref xref) :xref)
      (putprop to-box :xref :boxtop))))

(defun edvc-xref-hook (eb vc)
  (let ((xref (getf (plist eb) :xref)))
    (unless (null xref)
      (setf (getf (vc-graphics vc) 'xref) (copy-xref xref)))))

(defun vcvc-xref-hook (ovc vc)
  (let ((xref (getf (vc-graphics ovc) 'xref)))
    (unless (null xref)
      (setf (getf (vc-graphics vc) 'xref) (copy-xref xref)))))

(defun print-vc-xref-hook (vc eb)
  (let ((xref (getf (vc-graphics vc) 'xref)))
    (unless (null xref)
      (putprop eb xref :xref)
      (putprop eb :xref :boxtop))))

(eval-when (load)
  ;; no need for load-module because xref's are handled explicitly in
  ;; the partial-initialize method

  ;; Copying hooks
  (unless (member 'copy-xref-special-property *copy-special-box-properties-hook*)
    (push 'copy-xref-special-property *copy-special-box-properties-hook*))
  (unless (member 'edvc-xref-hook *edvc-special-structures-hook*)
    (push 'edvc-xref-hook *edvc-special-structures-hook*))
  (unless (member 'vcvc-xref-hook *vcvc-special-structures-hook*)
    (push 'vcvc-xref-hook *vcvc-special-structures-hook*))
  (unless (member 'print-vc-xref-hook *print-vc-special-structures-hook*)
    (push 'print-vc-xref-hook *print-vc-special-structures-hook*))

  ;; 2022-10-16 TODO sgithens Eventually move these alongside the actual boxer-styles
  ;; code. Currently, it's loaded before these hook lists exist, so we are
  ;; keeping them here until I properly reorganize things.
  ;; Same goes for the hooks registered in dumper.lisp.
  (unless (member 'copy-css-styles-special-property *copy-special-box-properties-hook*)
    (push 'copy-css-styles-special-property *copy-special-box-properties-hook*))
  (unless (member 'edvc-css-styles-hook *edvc-special-structures-hook*)
    (push 'edvc-css-styles-hook *edvc-special-structures-hook*))
  (unless (member 'vcvc-css-styles-hook *vcvc-special-structures-hook*)
    (push 'vcvc-css-styles-hook *vcvc-special-structures-hook*))
  (unless (member 'print-vc-css-styles-hook *print-vc-special-structures-hook*)
    (push 'print-vc-css-styles-hook *print-vc-special-structures-hook*))
  )


;;; xref scoping for use with TELL

(defun get-xref ()
  (do ((box (static-root) (when (box? box) (superior-box box))))
      ((null box) nil)
    (let ((xref (getprop box :xref)))
      (when (not (null xref))
        (return xref)))))
