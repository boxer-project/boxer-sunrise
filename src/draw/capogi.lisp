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

(defun allocate-capogi-char-data (capogi-char length)
  (setf (capogi-char-data capogi-char) capogi-char)
  (fli:allocate-foreign-object :type :byte :nelems length))

(defun free-capogi-char-data (capogi-char)
  (fli:free-foreign-object (capogi-char-data capogi-char)))


;; these dimensions need to be large enough for the largest glyph
(defvar *glyph-slate-wid* 200)
(defvar *glyph-slate-hei* 150)

(capi:define-interface glyph-slate ()
    ()
  (:panes
   (glyph-pane output-pane
               :drawing-mode :compatible
              :input-model '(((:button-1 :press) show-current-font)
                             (:character show-current-font))
              ))
  (:menus
   (file-menu "File" ((:component
                       (("Open Font" :accelerator #\o :callback 'menu-open-capi-font)))
                      (:component
                       (("Save" :callback 'save-ogl-bitmap-font)))
                      (:component
                       (("Quit" :callback 'capi:destroy))))))
  (:menu-bar  file-menu)
  (:default-initargs :title "Glyph Drawing Surface"
   :width *glyph-slate-wid* :height *glyph-slate-hei*))

(defvar *glyph-window*)
(defvar *glyph-pane*)

(defun make-glyph-window ()
  (setq *glyph-window* (make-instance 'glyph-slate)
        *glyph-pane* (slot-value *glyph-window* 'glyph-pane))
  (capi:display *glyph-window*))


;; array of
(defun make-glyph-array (length) (vector length))

(defvar *snap-to-font-name* nil)

(defun open-capi-font (family size styles)
  (set-glyph-pane-font
   (gp:find-best-font *glyph-pane*
                      (gp:make-font-description :family family
                                                :size size
                                                :weight (if (member :bold styles)
                                                            :bold
                                                          :normal)
                                                :slant (if (member :italic styles)
                                                           :italic
                                                         :roman)))))

(defun menu-open-capi-font (&rest ignore)
  (declare (ignore ignore))
  (let ((font (capi:prompt-for-font "Font to Convert...")))
    (capi:apply-in-pane-process *glyph-pane* #'set-glyph-pane-font font)))

(defun set-glyph-pane-font (font)
  (unless (null font)
    (setf (gp::graphics-state-font (gp::get-graphics-state *glyph-pane*)) font)
    (gp:with-graphics-state (*glyph-pane*)
      (let* ((pretty-name (font-pretty-name font))
             (height (gp:get-font-height *glyph-pane*)))
        (gp:draw-rectangle *glyph-pane* 0 0 (gp:port-string-width *glyph-pane* pretty-name) (+ height 5)
                           :foreground (gp::graphics-port-background *glyph-pane*)
                           :operation alu-seta :filled t)
        (gp:draw-string *glyph-pane* pretty-name 20 height)))))

(defun show-current-font (&rest ignore)
  (declare (ignore ignore))
  (gp:with-graphics-state (*glyph-pane*)
      (let* ((pretty-name (font-pretty-name
                           (gp::graphics-state-font (gp::get-graphics-state *glyph-pane*))))
             (height (gp:get-font-height *glyph-pane*)))
        (gp:draw-string *glyph-pane* pretty-name 0 height))))

(defun font-pretty-name (gpfont &optional stream)
  (let* ((fdesc (gp:font-description gpfont))
         (attr (unless (null fdesc) (gp:font-description-attributes fdesc))))
    (unless (null attr)
      (format stream "~A ~A ~A ~D"
              (getf attr :family) (getf attr :weight) (getf attr :slant) (getf attr :size)))))



(defun make-capogi-font (&optional (pane *glyph-pane*)
                                   (font (gp:graphics-state-font (gp::get-graphics-state pane))))
  (let* ((data-array (make-array *capogi-char-count*))
         (capogi-font (%make-capogi-font :capi-font font
                                         :height (gp:get-font-height pane font)
                                         :ascent (gp:get-font-ascent pane font)
                                         :fixed-width (when (gp:font-fixed-width-p pane font)
                                                        (gp::get-font-average-width pane font))
                                         :chars data-array)))
    (gp:with-graphics-state (pane :operation alu-seta)
      ;; set font
      (dotimes (i *capogi-char-count*)
        (setf (aref data-array i)
              (new-capogi-char (code-char i))))
      (dolist (tpair *unicode-window-1252*)
        (let ((charcode (car tpair)) (glindex (cadr tpair)))
          (unless (>= glindex *capogi-char-count*)
            (setf (aref data-array glindex)
                  (new-capogi-char (code-char charcode)))))))
    capogi-font))


;; window should be made and font already setup...
;; character is drawn with the upper left corner at (cx,cy)
;; this is the inner loop function - optimize here (particularly the stew of pixels & colors)

(defun new-capogi-char (char &optional (cx 0) (cy 0) (pane *glyph-pane*))
  (gp:with-graphics-state (pane :operation alu-seta)
    (multiple-value-bind (left top right bottom)
        (gp:get-character-extent pane char)
      (let* ((wid (abs (- left right))) (hei (abs (- top bottom)))
             (new-char (make-capogi-char :char char :wid wid :hei hei))
             (hor-bytes (ceiling wid 8))
             (maxx (floor wid))
             (bytes nil))
        ;; erase
        (gp::draw-rectangle pane cx cy (+ wid 1) hei
                            :foreground (gp::graphics-port-background pane)
                            :operation alu-seta :filled t)
        ;; now draw the char
        (gp:draw-character pane char (- cx left) (- cy top))
        ;; now we translate the screen data to a list of bytes in left->right,
        ;; bottom->top order.
        ;; actually the loops are backward cause we push each byte as we calculate them
        (let* ((image (gp:make-image-from-port pane cx cy (+ wid 1) hei))
               (imax  (gp:make-image-access pane image)))
          (unwind-protect
              (progn
                (gp::image-access-transfer-from-image imax)
                (do ((y 0 (1+ y)))
                    ((>= y hei) (setf (capogi-char-data new-char) bytes))
                  (do ((x (* (1- hor-bytes) 8) (- x 8)))
                      ((< x 0))
                    (push (glyph-byte-value x y maxx imax) bytes))))
            (gp:free-image-access imax)
            (gp::free-image pane image))
        new-char)))))

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


(defun glyph-byte-value (x y maxx image-access)
  (let ((return-byte 0))
    (dotimes (i 8)
      (let* ((gx (+ x i))
             (point (color:unconvert-color  *glyph-pane* (gp:image-access-pixel image-access gx y)))
             (white? (white-point? point))
             )
        (cond ((>= gx maxx) (return nil)) ;; no more valid points to check
              ((not white?) (setq return-byte (+ return-byte (ash 1 (- 7 i))))) )))
    return-byte))

(defun draw-char-for-caching (charcode &optional (x 0) (y 0) (pane *glyph-pane*))
  (gp:with-graphics-state (pane :operation alu-seta)
    (multiple-value-bind (left top right bottom)
        (gp::get-character-extent pane charcode)
      (gp::draw-rectangle pane x y (abs (- left right)) (abs (- top bottom))
                          :foreground (gp::graphics-port-background pane)
                          :operation alu-seta :filled t)
      (gp::draw-character pane charcode (- x left) (- y top))
      (values (abs (- left right)) (abs (- top bottom))))))

;; useful for debugging
(defun show-capogi-char-data (char)
  (let* ((w (capogi-char-wid char))
         (h (capogi-char-hei char))
         (data (capogi-char-data char))
         (hbytes (ceiling w 8)))
    (format t "~%~C" (capogi-char-char char))
    (do ((i (1- h) (1- i)))
        ((minusp i))
      (terpri)
      (dotimes (j hbytes) (format t "~8,'0B  " (nth (+ (* i hbytes) j)  data))))
;    (dotimes (i h)
;      (terpri)
;      (dotimes (j hbytes) (format t "~8,'0B  " (nth (+ (* i hbytes) j)  data))))
    ))




;;; converting to OpenGL
(defun make-opengl-font-from-capogi-font (cfont)
  (let ((oglfont (register-opengl-font-from-native-font cfont))
        (cfw (capogi-font-fixed-width cfont)))
    (setf (opengl-font-height oglfont) (capogi-font-height cfont)
          (opengl-font-ascent oglfont) (capogi-font-ascent cfont)
          (opengl-font-width  oglfont) cfw)
    (when (null cfw)
      (let* ((ccount (capogi-font-count cfont))
             (wa (make-array ccount))
             (chars (capogi-font-chars cfont)))
      (setf (opengl-font-widths-array oglfont) wa)
      (dotimes (i ccount)
        (setf (svref wa i) (capogi-char-wid (svref chars i))))))
    oglfont))

;; should fill the widths-array in the same loop...
;; needs to be called inside of an   (opengl:rendering-on (window)

(defun cache-capogi-font (cfont &optional make-widths-array?)
  (let* ((ccount (capogi-font-count  cfont))
         (fhei (capogi-font-height cfont))
         (fascent (capogi-font-ascent cfont))
         (fbaseline (float (- fhei fascent)))
         (wa (when (and make-widths-array? (null (capogi-font-fixed-width cfont)))
               (make-array ccount)))
         (chars (capogi-font-chars cfont))
         (ba (opengl:gl-gen-lists ccount)))
    (opengl:gl-pixel-storei opengl:*gl-unpack-alignment* 1)
    (dotimes (i ccount)
      (let* ((char (svref chars i))
             (cwid (capogi-char-wid char))
             (raw-data (capogi-char-data char))
             (ffi-data (cond ((typep raw-data 'fli::pointer) raw-data)
                             ((listp raw-data)
                              (fli:allocate-foreign-object :type '(:unsigned :byte)
                                                           :initial-contents raw-data))
                             (t (error "Bad char data ~A" raw-data)))))
        (when wa (setf (svref wa i) cwid))
        (opengl:gl-new-list (+ ba i) opengl:*gl-compile*)
        ;; the bitmap should have its origin at its baseline
        (opengl:gl-bitmap cwid fhei 0.0 fbaseline (float cwid) 0.0 ffi-data)
        (opengl:gl-end-list)
        (unless (eq raw-data ffi-data) (setf (capogi-char-data char) ffi-data))))
    (values ba wa)))



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

(defun dump-capogi-font (font)
  (with-capogi-font-stream (s (merge-pathnames (cfont-filename font) (capogi-font-directory))
                              :output)
    (dump-capogi-font-internal font s)))

;; clean byte stream version...
(defun dump-capogi-font-internal (font stream)
  (let ((font-values (capi-font-values (capogi-font-capi-font font)))
        (chars (capogi-font-chars font)))
    ;; capogi-font "magic" number: 2 bytes #xF0, #x3D corresponds to boxer dumper bin-op
    (write-byte #xF0 stream) (write-byte #x3D stream)
    ;; 1 byte for version number
    (write-byte *capogi-font-file-version* stream)
    ;; name string,
    (dump-simple-string (car font-values) stream)
    ;; 1 byte for size, 1 byte for styles
    ;; on mac, size is float but we are only interested in integer sizes, so this is safe
    (write-byte (round (cadr font-values)) stream)
    (write-byte (font-styles-byte (cddr font-values)) stream)
    ;; 1 byte for height, qbyte for ascent, 1 byte for fixed-width (0 means no fixed width)
    (write-byte (capogi-font-height font) stream)
    (write-byte (capogi-font-ascent font) stream)
    (let ((fw (capogi-font-fixed-width font)))
      (write-byte (if (null fw) 0 (ceiling fw)) stream))
    ;; now the chars...
    (let ((ccount (capogi-font-count font)))
      (when (> ccount 255)
        (error "~D chars is larger than the current file format supports" ccount))
      (write-byte ccount stream)
      (dotimes (i ccount) (dump-capogi-char (svref chars i) stream)))))

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

;; limitation that string < 255 chars
(defun dump-simple-string (string stream)
  (let ((count (length string)))
    (if (> count 255)
        (error "~D exceeds capability for DUMP-SIMPLE-STRING" count)
      (write-byte count stream))
    (dotimes (i count) (write-byte (char-code (char string i)) stream))))

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

(defun dump-capogi-char (glyph stream)
  (let* ((data (capogi-char-data glyph))
         (dlength (if (listp data) (length data) (car (fli:foreign-array-dimensions data))))
         (charcode (char-code (capogi-char-char glyph))))
    (flet ((char-bytes ()
             (values (ldb (byte 8 24) charcode) (ldb (byte 8 16) charcode)
                     (ldb (byte 8  8) charcode) (ldb (byte 8  0) charcode))))
      ;; 1st dump 4 bytes of char code
      (multiple-value-bind (b1 b2 b3 b4)
          (char-bytes)
        (write-byte b1 stream) (write-byte b2 stream)
        (write-byte b3 stream) (write-byte b4 stream))
      ;; now 1 byte of width
      (write-byte (capogi-char-wid glyph) stream)
      ;; 2 bytes of data length
      (write-byte (ldb (byte 8 8) dlength) stream) (write-byte (ldb (byte 8 0) dlength) stream)
      ;; now the data
      (if (listp data)
          (dolist (b data) (write-byte b stream))
        (dotimes (i dlength)
          (write-byte (fli:foreign-aref data i) stream))))))

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
    (dotimes (i ccount) (setf (svref chars i) (load-capogi-glyph stream)))
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

(defun load-capogi-glyph (stream)
  (let ((char (make-capogi-char)))
    ;; first, read 4 bytes and assemble the character
    (setf (capogi-char-char char)
          (code-char (dpb (read-byte stream) (byte 8 24)
                          (dpb (read-byte stream) (byte 8 16)
                               (dpb (read-byte stream) (byte 8 8)
                                    (read-byte stream))))))
    ;; now 1 byte of width
    (setf (capogi-char-wid char) (read-byte stream))
    ;; now the data
    (let* ((count (dpb (read-byte stream) (byte 8 8 ) (read-byte stream)))
           (f-array (fli:allocate-foreign-object :nelems count :type '(:unsigned :byte))))
      (dotimes (i count)
        (setf (fli:dereference f-array :index i) (read-byte stream)))
      (setf (capogi-char-data char) f-array))
    char))



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

(defun fill-capogi-font-cache (&optional verbose? save?)
  (when (null *capogi-font-cache*)
    (setq *capogi-font-cache* (init-capogi-font-cache)))
  (make-glyph-window) ; setq's *glyph-window* and *glyph-pane*
  (let ((last-time (get-internal-real-time)))
    (dolist (font-family boxer::*font-families*)
      (dotimes (i (length boxer::*font-sizes*))
        (let ((size (svref boxer::*font-sizes* i)))
          (dolist (style '(nil (:bold) (:italic) (:bold :italic)))
            (open-capi-font font-family size style)
            (let ((fam-i (position font-family boxer::*font-families* :test #'string=))
                  (size-i (boxer::%font-size-to-idx size))
                  (style-i (boxer::%font-face-to-idx style)))
              (when verbose?
                (format t "~&~A  ~D ~{~A ~}" font-family size style))
              ;; put the capogi font into *capogi-font-cache*
              (let ((styles (aref (aref *capogi-font-cache* fam-i) size-i))
                    (new-font (make-capogi-font)))
                (setf (aref styles style-i) new-font)
                (when verbose?
                  (format t ".....Making (~A sec) " (let ((new (get-internal-real-time)))
                                                      (prog1
                                                          (/ (- new last-time) 1000.0)
                                                        (setq last-time new)))))
                (when save?
                  (dump-capogi-font new-font)
                  (when verbose?
                    (format t ".....Saving (~A sec) " (let ((new (get-internal-real-time)))
                                                        (prog1
                                                            (/ (- new last-time) 1000.0)
                                                          (setq last-time new)))))))))))))
  (when save? (save-capogi-fonts-info))
  (capi::destroy *glyph-window*))

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

;; it may be neccessary to do this before a delivery - especially as load-capogi-font currently
;; generates ffi data
(defun clear-capogi-font-map ()
  (unless (null *capogi-font-cache*)
    (dotimes (i (length *capogi-font-cache*))
      (let ((fam (svref *capogi-font-cache* i)))
        (dotimes (j (length fam))
          (let ((size (svref fam j)))
            (dotimes (k (length size))
              (setf (svref size k) nil))))))))

(defun save-capogi-fonts-info (&optional (other-info (format nil "Converted with white defined as unweighted RGB > ~F" *white-enuff*)))
  (let* ((cfd (capogi-font-directory))
         (infofilename (merge-pathnames "info.txt" cfd)))
    (with-open-file (s infofilename :direction :output :element-type 'character :if-exists :supersede)
      (multiple-value-bind (sec min hou date month year)
          (decode-universal-time (get-universal-time))
        (format s "OpenGL CAPI fonts converted on ~D:~D:~D  ~D/~D/~D~%" hou min sec month date year))
      (format s "Font Families: ")
      (do* ((fams boxer::*font-families* (cdr fams))
            (fam (car fams) (car fams)))
           ((null fam) (terpri s))
        (write-string fam s)
        (unless (null (cdr fams)) (write-char #\, s) (write-char #\space s)))
      (format s "Sizes: ")
      (dotimes (i (length boxer::*font-sizes*))
        (let ((size (svref boxer::*font-sizes* i)))
          (format s "~D " size)))
      (unless (null other-info) (format s "~%~A" other-info)))))

;; returns a list of strings
(defun capogi-fonts-info ()
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


#|

;;; testing...

(Setq *gw* (make-glyph-window)) ; then Open Font

(defun test-draw-capogi-char (char x y)
  (drawing-on-window (*boxer-pane*)
                     (opengl::gl-pixel-storei opengl::*gl-unpack-alignment* 1)
                     (opengl::gl-raster-pos2-i x y)
                     (fli:with-dynamic-foreign-objects ((bm (:unsigned :byte)
                                                            :nelems (length (opengl::capogi-char-data char))
                                                            :initial-contents (opengl::capogi-char-data char)))
                       (opengl::gl-bitmap (opengl::capogi-char-wid char) (opengl::capogi-char-hei char) 0.0 0.0 0.0 0.0 bm)))
  (force-graphics-output))

(defun test-draw-capogi-string (string font x y)
  (boxer::drawing-on-window (boxer::*boxer-pane*)
    (opengl::gl-pixel-storei opengl::*gl-unpack-alignment* 1)
    (opengl::gl-raster-pos2-i x y)
    (do* ((chars (capogi-font-chars font))
          (i 0 (1+ i)))
         ((= i (length string)))
      (let ((char (svref chars (char-code (char string i)))))
        (fli:with-dynamic-foreign-objects ((bm (:unsigned :byte)
                                               :nelems (length (opengl::capogi-char-data char))
                                               :initial-contents (opengl::capogi-char-data char)))
           (opengl::gl-bitmap (opengl::capogi-char-wid char) (opengl::capogi-char-hei char) 0.0 0.0
                          (float (capogi-char-wid char)) 0.0 bm)))))
  (boxer::force-graphics-output))

|#

