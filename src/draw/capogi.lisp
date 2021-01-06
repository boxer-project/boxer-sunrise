;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER-WINDOW; -*-
#|


     Additional Portions Copyright 2014 PyxiSystems LLC


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+


 Caching font glyph bitmaps

There are 4 layers of representation of characters
1) capi - draw-character onto a capi:output-pane and utilities to read the pixels from it
2) internal interchange layer - ogl-char structures and arrays of them to make a font
                               no explicit OpenGL data structures although the glyph bitmap
                               data should be in a form acceptable to GLBitmap (either as a
                               foreign byte array or ready for the FLI to convert it)
                               (list of bytes allows for inclusion in dump, ffi array may
                               be needed if persitence bitmap data is required)
                               CAPOGI (CAPI - OpenGL interchange) layer
                               (9/23/13:prefer ffi array to speed up GPU caching, allow both,
                               disksave with lists, switch to FFI when converting to OpenGL)
3) OpenGL layer - interchange layer is converted to an OpenGL display list a la
                  Chapter 8 of the Red Book
4) files - ability to read/write from interchange layer to/from files
           use pieces of boxer fasl dumper to allow for future imbedding of fonts in boxer files


 Capogi fonts interface with boxer as the "Native Font" in the Opengl-font structs

Modification History (most recent at the top)

 4/ 7/14  {save}-capogi-fonts-info, fill-capogi-font-cache can now save info
 4/ 2/14  *white-enuff* set to 2.0s0 after some experimentation
 3/15/14  white-point?, *white-enuff*
 2/15/14  shortened print-capogi-font result
12/10/13  boxer-font-spec->capogi-font, capogi-size-idx
11/09/13  boxer-font-spec->capogi-font now defaults to "Courier New" if no family match
 8/11/13  started file

|#



(in-package :boxer-window)

(defvar *capogi-font-cache* nil) ;an array of arrays of arrays like *font-cache*

;; these are for reverse lookup (fontspec -> capogi font)
(defvar *capogi-font-families* nil)
(defvar *capogi-font-sizes* nil)

(defstruct capogi-char
  (char #\null)
  (wid 0)
  (hei 0)  ;; should we use ascent, descent instead ?
  (data nil))  ;; list of bytes, left to right, bottom to top padded out to full bytes

(defvar *capogi-char-count* 255)

(defstruct (capogi-font (:constructor %make-capogi-font)
                        (:print-function print-capogi-font)
                        (:predicate capogi-font?))
  (capi-font nil) ; can be an instance of a capi font or a list of (family size styles)
  (count *capogi-char-count*)
  (height 0)  ;; any other font metrics ?
  (ascent 0)
  (fixed-width nil) ;; nil for var width fonts, otherwise a number
  (chars nil))

(defun print-capogi-font (font stream level)
  (declare (ignore level))
  (format stream "#<CAPOGI-FONT ")
  (print-capogi-font-internal font stream)
  (format stream ">"))

(defun print-capogi-font-internal (font stream)
  (let ((f (capogi-font-capi-font font)))
    (cond ((listp f)
           (format stream "~A ~D ~{ ~A~}" (car f) (cadr f) (cddr f)))
          (t
           (format stream "~A" (font-pretty-name f))))))

(defun font-pretty-name (gpfont &optional stream)
  (let* ((fdesc (gp:font-description gpfont))
         (attr (unless (null fdesc) (gp:font-description-attributes fdesc))))
    (unless (null attr)
      (format stream "~A ~A ~A ~D"
              (getf attr :family) (getf attr :weight) (getf attr :slant) (getf attr :size)))))

;; because the glyphs are aliased, we look check if the pixel is white or not, instead of
;; looking for black

(defvar *white-enuff* 2.0s0)
;; get-point returns a simple vector with #(:RGB Red Blue Green)
(defun white-point? (point)
;  ;; just checking the blue component which seems like it might be enough
;  (= (svref point 3) 1.0s0)
  (let ((total (+ (svref point 1) (svref point 2) (svref point 3))))
    ;; possibly change this to use a weighted luminance formula such as
    ;; Y = 0.2126 R + 0.7152 G + 0.0722 B
    ;; max value for total will be 3.0s0
    (> total *white-enuff*)))

;;; converting to OpenGL
(defun make-opengl-font-from-capogi-font (cfont)
  (let ((oglfont (%make-opengl-font :native-font cfont))
        (cfw (capogi-font-fixed-width cfont)))
    (setf (opengl-font-height oglfont) (capogi-font-height cfont)
          (opengl-font-ascent oglfont) (capogi-font-ascent cfont)
          (opengl-font-width  oglfont) cfw)
    (when (null cfw)
      (let* ((ccount (capogi-font-count cfont))
             (wa (make-array ccount))
             (chars (capogi-font-chars cfont)))
      (setf (opengl-font-widths-array oglfont) wa)
        ))
    oglfont))

;;; file operations

;; setq this when saving
(defvar *capogi-font-directory* nil)
;; this is more useful with delivered boxer images
(defun capogi-font-directory ()
  (or *capogi-font-directory*
      (setq *capogi-font-directory* (font-directory-search))))


;; remember that the Mac app is inside a directory "bundle"
;; <bundle directory>/Contents/MacOS/<executable>
;; we can put the fonts into the bundle in the location <bundle directory>/Contents/Resources/Fonts
(defun font-directory-search ()
  (let ((testfile "Arial10.cfnt")
        (searchlist (list *capogi-font-directory*
                          #+macosx
                          (make-pathname :directory
                                         (append (butlast (pathname-directory (lw:lisp-image-name)))
                                                 '("Resources" "Fonts")))
                          (make-pathname :directory
                                         (append (pathname-directory (lw:lisp-image-name))
                                                 '("Fonts"))))))
    (dolist (folder-key '(:appdata :local-appdata :common-appdata))
      (push (make-pathname :directory (append (pathname-directory
                                               (sys::get-folder-path folder-key))
                                              '("Boxer" "Fonts")))
            searchlist))
    (dolist (sd searchlist)
      (when (probe-file (merge-pathnames testfile sd))
        (return sd)))))

(defvar *capogi-font-file-type* "cfnt")

(defvar *capogi-font-file-version* 1)

(defmacro with-capogi-font-stream ((stream-var file direction) &body body)
  `(with-open-file (,stream-var ,file :direction ,direction :element-type '(unsigned-byte 8))
     . ,body))

(defun cfont-filename (cfont)
  (let ((fv (capi-font-values (capogi-font-capi-font cfont))))
    (make-cfont-filename (car fv) (cadr fv) (cddr fv))))

(defun make-cfont-filename (family size styles)
  (format nil "~A~D~A.~A"
          family (round size)  ; MacOS allows floating font sizes
          (cond ((null styles) "")
                ((and (member :bold styles) (member :italic styles)) "bi")
                ((member :bold styles) "b")
                ((member :italic styles) "i")
                (t ""))
          *capogi-font-file-type*))

;; core interface,
;; fill-bootstrapped-font-caches calls make-boxer-font to make an OpenGL font with
;; the result of this function as the "native font"
;; look in the cache, then load if needed
;; we assume that all available fonts are either already in the cache as capogi fonts or
;; loadable from files

(defun capogi-fam-idx (fam-name)
  (position fam-name *capogi-font-families* :test #'string-equal))

;; smarter, closest match instead of only exact....
(defun capogi-size-idx (size)
  (let ((sizelength (length *capogi-font-sizes*)))
    (dotimes (i sizelength (values (1-& sizelength) t))
      (let ((csize (svref *capogi-font-sizes* i)))
        (cond ((= csize size) (return i))
              ((> csize size) (return (values i t))))))))

;  (position size *capogi-font-sizes* :test #'=)

;; should return the default (Courier New, idx = 0) if no match is found
;; should warn when exact match is not found ?
(defun boxer-font-spec->capogi-font (fontspec)
  (let* ((fam-idx (capogi-fam-idx (car fontspec)))
         (fam (if (null fam-idx)
                  (svref *capogi-font-cache* 0)
                (svref *capogi-font-cache* fam-idx))))
    (cond ((null fam) (error "Unable to get capogi font for ~A" fontspec))
          (t (let* ((size-idx (capogi-size-idx (cadr fontspec)))
                    (size (unless (null size-idx) (svref fam size-idx))))
               (cond ((null size) nil)
                     (t (svref size (font-styles-byte (cddr fontspec))))))))))

(defun capi-font-values (f)
  (cond ((gp::font-p f)
         (let* ((fd (gp:font-description f))
                (weight (gp:font-description-attribute-value fd :weight))
                (slant  (gp:font-description-attribute-value fd :slant)))
           (list* (gp:font-description-attribute-value fd :family)
                  (gp:font-description-attribute-value fd :size)
                  (cond ((and (eq weight :normal) (eq slant :roman)) nil)
                        ((and (eq weight :bold)   (eq slant :italic)) '(:bold :italic))
                        ((eq weight :bold) '(:bold))
                        ((eq slant :italic) '(:italic))))))
        ((listp f) f)
        (t (error "Unknown CAPI font value, ~A" f))))

(defun font-styles-byte (styles)
  (cond ((null styles) 0)
        ((and (member :bold styles) (member :italic styles)) 3)
        ((member :bold styles) 1)
        ((member :italic styles) 2)
        (t 0)))

;; might want to pack the char data
;; doesn't hack data which has already been converted to FFI
;; each char will dump 4 bytes of charcode (to allow future possibility for unicode)
;; 1 byte for char width(any chars larger than 255 ?), and then the data, 2 bytes length + bytes

(defun load-capogi-font (stream)
  ;; 1st check for magic numbers and get file version number
  (unless (and (= (read-byte stream) #xF0) (= (read-byte stream) #x3D))
    (error "~A is out of synch or NOT a Capogi Font stream" stream))
  (let* ((version (read-byte stream))
         (family-name (read-simple-string stream))
         (size (read-byte stream))
         (style-byte (read-byte stream))
         (cfont (%make-capogi-font
                 :capi-font (list* family-name size (styles-from-byte style-byte))
                 :height (read-byte stream)
                 :ascent (read-byte stream)
                 :fixed-width (let ((fwbyte (read-byte stream)))
                                (if (zerop fwbyte) nil fwbyte))))
         (ccount (read-byte stream))
         (chars (make-array ccount)))
    (declare (ignore version)) ; will use it eventually and must get the byte out anyway
    (setf (capogi-font-chars cfont) chars)
    cfont))

(defun read-simple-string (stream)
  (let* ((count (read-byte stream))
         (string (make-string count)))
    (dotimes (i count) (setf (char string i) (code-char (read-byte stream))))
    string))

(defun styles-from-byte (byte)
  (cond ((zerop byte) nil)
        ((= byte 1) '(:bold))
        ((= byte 2) '(:italic))
        ((= byte 3) '(:bold :italic))
        (t '(:gak))))

(defvar *convert-char-data-on-load?* t) ; any reason to NOT do this ?

;;;; not for regular Boxer operations

(defun init-capogi-font-cache ()
  (flet ((make-size-cache ()
           (let* ((sizes (length boxer::*font-sizes*))
                  (size-cache (make-array sizes)))
             (dotimes (i sizes) (setf (aref size-cache i) (make-array 4)))
             size-cache)))
    (let* ((fam-size (length boxer::*font-families*))
           (return-cache (make-array fam-size)))
      (dotimes (j fam-size) (setf (aref return-cache j) (make-size-cache)))
      return-cache)))

;; called during boxer startup, needs to make sure all the capogifonts are loaded
;; Note that they may have already been disksaved
;; should populate the reverse mapping to allow lookup by fontspec
(defun load-capogi-font-cache (&optional verbose?)
  (when (null *capogi-font-cache*)
    (setq *capogi-font-cache* (init-capogi-font-cache)))
  (when (or (null *capogi-font-families*)
            (not (= (length *capogi-font-families*) (length boxer::*font-families*))))
    (setq *capogi-font-families* (make-array (length boxer::*font-families*)
                                             :initial-contents boxer::*font-families*)))
  (when (null *capogi-font-sizes*)
    (setq *capogi-font-sizes* (let* ((l (length boxer::*font-sizes*))(ra (make-array l)))
                                (dotimes (i l) (setf (svref ra i) (svref boxer::*font-sizes* i)))
                                ra)))
  (do* ((i 0 (1+ i))
        (fams boxer::*font-families* (cdr fams))
        (fam (car fams) (car fams)))
       ((null fam))
    (dotimes (j (length boxer::*font-sizes*))
      (let ((size (svref boxer::*font-sizes* j))
            (k 0))
        (dolist (style '(nil (:bold) (:italic) (:bold :italic)))
          (when verbose?
            (format t "~%Loading ~A ~D ~A => (~D,~D,~D)"
                    fam size (if (null style) "" style) i j k))
          (with-capogi-font-stream (stream (merge-pathnames (make-cfont-filename fam
                                                                                 size
                                                                                 style)
                                                            (capogi-font-directory))
                                           ':input)
            (setf (svref (svref (svref *capogi-font-cache* i) j) k)
                  (load-capogi-font stream)))
          (setq k  (1+ k)))))))

;; returns a list of strings
(defun capogi-fonts-info ()
  "This is used by the show-font-info primitive."
  (let* ((cfd (capogi-font-directory))
         (infofilename (merge-pathnames "info.txt" cfd))
         (return-strings (list (format nil"Active Font directory is ~A" (bw::capogi-font-directory)))))
    (when (probe-file infofilename)
      (with-open-file (s infofilename :direction :input :element-type 'character)
        (loop (let ((line (read-line s nil nil)))
                (cond ((null line) (return return-strings))
                      (t (push line return-strings)))))))))

(eval-when (load)
  (unless (member :capogi *features*) (push :capogi *features*))
  )
