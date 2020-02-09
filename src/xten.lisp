;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$


 Copyright 1996 - 1998 Regents of the University of California

 Enhancements and Modifications Copyright 1998 - 2005 Pyxisystems LLC

                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    Support for extensions to Boxer

Primitives:

 o Extension-Info       lists current, and available but not loaded extensions
 o Add-Extension <x>    adds extension to the extension map (what loads at startup)
 o Remove-Extension <x> removes extension from the extension map
 o Load-Extension <x>   loads extension immediately but don't (?) add
                        it to the startup list
 o Describe-Extension <x>

 + load-boxer-extensions is called from window-system-specific-start-boxer (boxwin-xx)


Modification History (most recent at top)

 6/20/05 catch 'abort-extension wrapped around load-extension so that individual
         extensions or patches can check criteria the abort the load if neccessary
         predicates can check (sm::system-properties (find-system-named 'boxer))
         and then use abort-extension (which does a (throw 'abort-extension))
 4/21/03 merged current LW and MCL files
11/04/02 load-boxer-extensions changed to use boxer-editor-message instead of
         boxer-editor-warning to avoid annoying beeps
 9/20/02 started porting xten.lisp to LW
 2/17/01 merged current LW and MCL files
 9/07/99 load-extension hacks aliased folders in extension pathnames instead of
         blowing out, also errors during startup are ignored
 7/15/99 load-extension now actually checks for a successful load
 5/25/98 added support for multi source file extensions in Utilities
 5/25/98 Started Logging Changes: surce = boxer version 2.3 alphaR1

|#

#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)

(defvar *boxer-extensions* nil)

(defstruct boxer-extension
  (pretty-name nil)
  (version nil)
  (comments nil))

;; this should appear in the module file being loaded
;; for mac extensions, this will be redundant with the version info returned from
;; xten-info, for other file systems without resource forks, this will be the
;; primary method for storing extension info
(defun declare-boxer-extension (name &key (version 0.1) comments)
  (unless (member name *boxer-extensions*
                  :test #'(lambda (a b)
                            (string-equal a (boxer-extension-pretty-name b))))
    (push (make-boxer-extension
           :pretty-name name :version version :comments comments)
          *boxer-extensions*)))

;; LISPWORKS:*LISPWORKS-DIRECTORY*, (user-homedir-pathname), (system:lispworks-dir "Extensions")

;; this is init'd in window-system-specific-start-boxer in boxwin-lw.lisp
;; just before extensions are loaded
#+lispworks
(defvar *starting-directory-pathname*)

(defun starting-directory ()
  #+ccl
  (ccl::mac-default-directory)
  #+lispworks
  *starting-directory-pathname*
  #-(or ccl lispworks)
  *default-pathname-defaults*)

(defun xdir ()
  (append (pathname-directory (starting-directory)) '("Extensions")))

(defun dxdir ()
  (append (pathname-directory (starting-directory)) '("Extensions(off)")))

;; xtension info that's available to the OS, on the mac, this
;; stuff can be found in the resource fork
;; returns major, minor, patch-level, version-string and version-info
(defun xten-info (pathname)
  #+ccl
  (when (probe-file pathname)
    (ccl::with-open-resource-file (x pathname)
      (let ((vv (ccl::get-resource "vers" 1)))
        (unless (null vv)
          (let* ((vp (ccl::%get-ptr vv))
                 (major (ccl::%get-byte vp)) (minor (ccl::%get-byte vp 1)))
            (values (+ (* 10 (ldb #.(byte 4 4) major)) (ldb #.(byte 4 0) major))
                    (ldb (byte 4 4) minor)
                    (ldb (byte 4 0) minor)
                    (ccl::%get-string vp 6)
                    (ccl::%get-string vp (+ 7 (ccl::%get-byte vp 6)))))))))
  )

;; this is called from start-boxer (boxwin) before prefs are loaded, in case
;; the module defines new prefs....
(defun load-boxer-extensions ()
  (dolist (x (get-xtns)) (load-extension x)))

(defun get-xtns (&optional (dir (xdir)))
  (remove-duplicates
   (mapcar #'pathname-name
           (directory (make-pathname :directory dir :name "*"
                                     :type #+(and mcl powerpc) "PPC"
                                           #+(and mcl M68K)    "68K"
                                           #+win32             "bxe"
                                           #+(and lispworks mac) "BXPPC"
                                           )
                      #+ccl
                      :test
                      #+ccl
                      #'(lambda (f) (eq (ccl::mac-file-type f) :BOXE))))))

(defun xpath (name &optional (dir (xdir)))
  (make-pathname :directory dir :name name
                 :type #+(and mcl powerpc) "PPC"
                       #+(and mcl (not powerpc)) "68K"
                       #+win32 "bxe"))

(defun load-extension (x)
  (let ((xpath (xpath x)))
    (when (and (probe-file xpath) #+ccl (eq (ccl::mac-file-type xpath) :BOXE))
      (multiple-value-bind (major minor patch version-string version-info)
          (xten-info xpath)
        (declare (ignore version-string))
        (catch 'abort-extension
          (multiple-value-bind (good? err)
              #+ccl
              (ccl::%fasload (namestring (truename xpath)))
              #+lispworks
              (let ((system::*binary-file-type* "bxe"))
                (load (namestring (truename xpath))))
              #-(or ccl lispworks)
              (load (namestring (truename xpath)))
              (cond ((null good?)
                     #+ccl
                     (when (null bw::*boxer-bootstrap-status*) (ccl::%err-disp err))
                     )
                    (t
                     (boxer-editor-message "Loading ~A Extension" x)
                     (unless (member x *boxer-extensions*
                                     :test #'(lambda (a b)
                                               (string-equal
                                                a (boxer-extension-pretty-name b))))
                       (push (if (null major)
                               (make-boxer-extension :pretty-name x)
                               (make-boxer-extension :pretty-name x ; version-string ??
                                                     :version (format nil "~D.~D.~D"
                                                                      major minor patch)
                                                     :comments version-info))
                             *boxer-extensions*))))))))))

;; individual patches/extensions can test
;; (sm::system-properties (find-system-named 'boxer))
;; :major/minor-version-number
(defun abort-extension-load () (throw 'abort-extension nil))

;; reads the map file and returns a list of extensions to be loaded (unused now)
(defun read-extension-map (mapfile)
  (let ((xtns nil))
    (with-open-file (s mapfile)
      (loop
        (let ((line (read-line s nil nil)))
          (if (null line) (return)
              (setq xtns
                    (nconc xtns
                           (list (string-trim '(#\space #\tab #\newline)
                                              line))))))))
    xtns))

(defun insure-extension-dirs ()
  (let ((xdir  (make-pathname :directory (xdir)))
        (dxdir (make-pathname :directory (dxdir))))
    (when (null (probe-file xdir))
      #+ccl       (ccl::create-directory  xdir)
      #+lispworks (system::make-directory xdir)
      #-(or ccl lispworks) (error "Don't know how to make directories in ~A"
                                  (lisp-implementation-type)))
    (when (null (probe-file dxdir))
      #+ccl       (ccl::create-directory  dxdir)
      #+lispworks (system::make-directory dxdir)
      #-(or ccl lispworks) (error "Don't know how to make directories in ~A"
                                  (lisp-implementation-type)))))



;;; Primitives
;; returns 3 boxes, currently loaded extensions, the startup list and the list
;; of available extensions

(defboxer-primitive bu::extension-info ()
  (insure-extension-dirs)
  (let* ((current (mapcar #'boxer-extension-pretty-name *boxer-extensions*))
         (available (append (get-xtns (xdir)) (get-xtns (dxdir))))
         (startup (get-xtns)))
    (crock-make-vc
     (list (make-row (list "Current:" (make-box (mapcar #'row-entry-from-xtstring
                                                        current))))
           (make-row (list "Startup:" (make-box (mapcar #'row-entry-from-xtstring
                                                        startup))))
           (make-row (list "Available:" (make-box (mapcar #'row-entry-from-xtstring
                                                          available))))
           (make-row (list (make-xten-doc-box)))))))

;; remember to get rid of possible �'s
(defun row-entry-from-xtstring (xs) (list (remove #\� xs)))

(defun make-xten-doc-box ()
  (let ((box (make-box `(("Load-Extension" ,(make-box '(("<extension>")) 'data-box)
                          ":" "loads the <extension>")
                         ("Add-Extension" ,(make-box '(("<extension>")) 'data-box)
                          ":" "adds the <extension> to the startup list to be")
                         ("              loaded when the Boxer application is opened")
                         ("Remove-Extension" ,(make-box '(("<extension>")) 'data-box)
                          ":" "removes the <extension> from the list of Extensions")
                         ("                 to be loaded when the Boxer application is opened"))
                       'data-box
                       "Extension Primitives")))
    (shrink box)
    box))

(defun xname-from-box (box)
  (string-trim '(#\space #\tab #\newline) (box-text-string box)))

(defboxer-primitive bu::add-extension (x)
  (insure-extension-dirs)
  (let ((xstring (xname-from-box x))
        (current (get-xtns))
        (disabled (get-xtns (dxdir))))
    (cond ((member xstring current :test #'string-equal)
           ;; do nothing, it's already there
           )
          ((member xstring disabled :test #'string-equal)
           (rename-file (xpath xstring (dxdir))
                        (xpath xstring (xdir))))
          (t (eval::primitive-signal-error :extension
                                           "Can't find the Extension, "
                                           xstring
                                           ", You can add it manually by dropping the file"
                                           "into the \"Extensions\" folder"))))
  eval::*novalue*)

(defboxer-primitive bu::remove-extension (x)
  (insure-extension-dirs)
  (let ((xstring (xname-from-box x))
        (current (get-xtns)))
    (when (member xstring current :test #'string-equal)
      ;; should we error if we can't find it ?
      (rename-file (xpath xstring (xdir))
                   (xpath xstring (dxdir))))
    eval::*novalue*))


#| old style xmapfile based prims....
(defboxer-primitive bu::add-extension (x)
  (let* ((xstring (xname-from-box x))
         (xmapfile (make-pathname :directory (xdir) :name "extension" :type "map"))
         (xmapback (make-pathname :directory (xdir) :name "extension" :type "bak"))
         (xtns (when (probe-file xmapfile) (read-extension-map xmapfile)))
         (tmpmap (make-pathname :directory (xdir)
                                :name (format nil "~A" (gensym)) :type "tmp")))
    (cond ((member xstring xtns :test #'string-equal)
           ;; do nothing, it's already there....
           )
          (t
           (setq xtns (nconc xtns (list xstring)))
           ;; write out new map file
           (with-open-file (out tmpmap :direction :output
                                :if-exists :supersede :if-does-not-exist :create)
             (dolist (x xtns) (write-line x out)))
           (when (probe-file xmapfile)
             (rename-file xmapfile xmapback #+ccl :if-exists #+ccl :supersede))
           (rename-file tmpmap xmapfile))))
  eval::*novalue*)

(defboxer-primitive bu::remove-extension (x)
  (let* ((xstring (xname-from-box x))
         (xmapfile (make-pathname :directory (xdir) :name "extension" :type "map"))
         (xmapback (make-pathname :directory (xdir) :name "extension" :type "bak"))
         (xtns (when (probe-file xmapfile) (read-extension-map xmapfile)))
         (tmpmap (make-pathname :directory (xdir)
                                :name (format nil "~A" (gensym)) :type "tmp")))
    (cond ((member xstring xtns :test #'string-equal)
           (setq xtns (delete xstring xtns :test #'string-equal))
           ;; write out new map file
           (with-open-file (out tmpmap :direction :output
                                :if-exists :supersede :if-does-not-exist :create)
             (dolist (x xtns) (write-line x out)))
           (when (probe-file xmapfile)
             (rename-file xmapfile xmapback #+ccl :if-exists #+ccl :supersede))
           (rename-file tmpmap xmapfile))
          (t ;; not in map so do nothing...
           )))
  eval::*novalue*)

|#

(defboxer-primitive bu::load-extension (x)
  (insure-extension-dirs)
  ;(ccl::eval-enqueue `(load-extension ,(xname-from-box x)))
  (load-extension (xname-from-box x))
  eval::*novalue*)

#+ccl
(defboxer-primitive bu::describe-extension (x)
  (extension-info-box (xname-from-box x)))

#+ccl
(defun extension-info-box (x)
  (let ((path (xpath x)))
    (if (probe-file path)
        (multiple-value-bind (major minor patch vs vi)
            (xten-info path)
          (crock-make-vc (list (list (format nil "version ~D.~D.~D"
                                             major minor patch))
                               (list vs)
                               (list vi)))))))



;; for multi source file extensions

(defun make-xten (xten-name xtype &rest files)
  #+ccl
  (let ((xtenpath (make-pathname :type xtype :defaults xten-name))
        (eof-value (cons 'a 'b)))
    ;;announce...
    (format t "~&Making mac extension, ~A, for ~A" xten-name xtype)
    (cond ((null (cdr files)) ; one file...
           (with-open-file (out xtenpath
                                :direction :output
                                :element-type '(unsigned-byte 8))
             (with-open-file (in (car files) :element-type '(unsigned-byte 8))
               (loop (let ((byte (read-byte in nil eof-value)))
                       (if (eq byte eof-value) (return) (write-byte byte out)))))))
          (t
           (ccl::require :fasl-concatenate)
           (cond ((string= xtype "68K")
                  (ccl::fasl-concatenate xtenpath files))
                 ((string= xtype "PPC")
                  (ccl::pfsl-concatenate xtenpath files)))))
    (ccl-file->xten xtenpath))
  #+win32
  (let ((xtenpath (make-pathname :type xtype :name xten-name :directory (xdir)
                                 :defaults (car files)))
        (tmpfilename (make-pathname :name (string (gensym)) :type "lisp")))
  ;; announce & reminder
  (format t "~&Making PC extension, ~A " xten-name)
  (format t "~&remember to use source files and include a declare-boxer-extension")
  ;; we could pipe declare form into tmp file....
  (cond ((null (cdr files)) ; one file
         (rename-file
          (compile-file (make-pathname :defaults (car files) :type "lisp"))
          xtenpath))
        (t
         (unwind-protect
             (progn
               ;; 1st concatenate the source
               (with-open-file (out tmpfilename :direction :output)
                 (dolist (f files)
                   (with-open-file (in f)
                     (loop (let ((line (read-line in nil nil)))
                             (cond ((null line) (return))
                                   (t (write-line line out))))))))
               (compile-file tmpfilename :output-file xtenpath))
           (delete-file tmpfilename))))))


;; Utilities
(defun line-prompt (prompt-string) (format t "~&~A: " prompt-string) (read-line))

#+ccl
(defun ccl-file->xten (pathname &optional
                                (major (read-from-string
                                        (line-prompt "Major Release Number")))
                                (minor (read-from-string
                                        (line-prompt "Minor Release Number")))
                                (patch-level
                                 (read-from-string (line-prompt "Patch Number")))
                                (version-string (line-prompt "Version String"))
                                (version-info
                                 (line-prompt "Version (in Get) Info")))
  (ccl::set-mac-file-type pathname :BOXE)
  (ccl::set-mac-file-creator pathname :BOXR)
  (ccl-set-xten-info-internal pathname major minor patch-level
                              version-string version-info))

;; prompt for major, minor, patch numbers
;; then version-string and version-info strings

#+ccl
(defun ccl-set-xten-info-internal (pathname major minor patch-level
                                        version-string version-info)
  (let ((globalvers (ccl::get-resource "vers" 1)))
    (ccl::with-open-resource-file (x pathname :if-does-not-exist :create)
      (let* ((existing (ccl::get-resource "vers" 1))
             (file-res? (and (not (null existing))
                             (not (= (ccl::%ptr-to-int existing)
                                     (ccl::%ptr-to-int globalvers)))))
             (vershandle (#_NewHandle (+ 6 ; total bytes fro numerical info
                                           (1+ (length version-string))
                                           (1+ (length version-info)))))
             ;; maybe use ccl::%stack-block instead ?
             (vp (ccl::%get-ptr vershandle)))
        (when (> major 9)
          (multiple-value-bind (tens ones) (floor major 10)
            (setq major (dpb tens #.(byte 4 4) ones))))
        (ccl::%put-byte vp major)
        ;; minor and patch levels
        (ccl::%put-byte vp (dpb minor #.(byte 4 4) patch-level) 1)
        ;; set the dev stage, pre-release rev code and
        ;; region codes to reasonable defaults (maybe prompt for them later)
        (ccl::%put-byte vp #x80 2) ; dev #x20=pre-alpha, #x40=alpha, #x60=beta, #x80=release
        (ccl::%put-byte vp 0 3) ; pre-release revision level
        ;(ccl::%put-word vp usregion 4) ; region code (what's US ? see inside mac:text)
        (ccl::%put-string vp version-string 6)
        (ccl::%put-string vp version-info (+ 7 (length version-string)))
        ;; now put it away
        (when file-res? (ccl::remove-resource existing))
        (ccl::add-resource vershandle "vers" 1)
        (ccl::write-resource vershandle)
        (ccl::release-resource vershandle)))))
