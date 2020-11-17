;;;;
;;;; The attic
;;;;

;;;;
;;;; FILE: capogi.lisp
;;;;

(defun allocate-capogi-char-data (capogi-char length)
;;   (setf (capogi-char-data capogi-char) capogi-char)
;;   (fli:allocate-foreign-object :type :byte :nelems length))
;;
;; (defun free-capogi-char-data (capogi-char)
;;   (fli:free-foreign-object (capogi-char-data capogi-char)))

;; array of
(defun make-glyph-array (length) (vector length))

(defvar *snap-to-font-name* nil)

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

;; ;; useful for debugging
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

(defun cfont-filename (cfont)
  (let ((fv (capi-font-values (capogi-font-capi-font cfont))))
    (make-cfont-filename (car fv) (cadr fv) (cddr fv))))

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

;;;;
;;;; FILE: comsf.lisp
;;;;

#+mcl
(progn
  (defun normal-font? (boxer-font)
    (zerop& (font-style boxer-font)))

  (defun bold-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 1))))

  (defun italic-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 2))))

  (defun underline-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 4))))

  (defun outline-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 8))))

  (defun shadow-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 16))))

  (defun condense-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 32))))

  (defun extend-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 64))))

  ;; like typep for font styles...
  (defun font-stylep (boxer-font style)
    (cond ((or (null style) (eq style :plain))
           (zerop& (font-style boxer-font)))
          (t
           (not (zerop& (logand& (font-style boxer-font)
                                 (case style
                                   (:bold 1) (:italic 2) (:underline 4)
                                   (:outline 8) (:shadow 16)
                                   (:condense 32) (:extend 64))))))))

  (defun font-styles (boxer-font)
    (let ((style-byte (font-style boxer-font))
          (return-styles nil))
      (do* ((pos 1 (ash pos 1))
            (styles '(:bold :italic :underline :outline :shadow :condense :extend)
                    (cdr styles))
            (style (car styles) (car styles)))
          ((null style))
        (unless (zerop& (logand& style-byte pos)) (push style return-styles)))
      (nreverse return-styles)))


  (defun set-font-style (boxer-font style to-on?)
    (cond ((or (null style) (eq style :plain))
           (%set-font-style boxer-font 0))
          (t (let* ((current-style (font-style boxer-font))
                    (new-style (case style
                                   (:bold (dpb& (if to-on? 1 0)
                                                '#.(byte 1 0) current-style))
                                   (:italic (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 1) current-style))
                                   (:underline (dpb& (if to-on? 1 0)
                                                     '#.(byte 1 2) current-style))
                                   (:outline (dpb& (if to-on? 1 0)
                                                   '#.(byte 1 3) current-style))
                                   (:shadow (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 4) current-style))
                                   (:condense (dpb& (if to-on? 1 0)
                                                    '#.(byte 1 5) current-style))
                                   (:extend (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 6) current-style)))))
               (%set-font-style boxer-font new-style)))))
  )

#|
(defmethod change-font-between-cha-nos ((row row) new-font-no
          &optional
          (start-cha-no 0)
          (stop-cha-no (length-in-chas row)))
  (let* ((last-font-no 0)
   (chas-array (slot-value row 'chas-array)))
    (cond ((null (chas-array-fds chas-array))
     ;; there are no Font Descriptors in the row so we can
     ;; just insert new ones without checking
     (if  (=& stop-cha-no (chas-array-active-length chas-array))
    ;; we don't need to insert another FD if we
    ;; are at the end of the row
    (push (make-bfd start-cha-no new-font-no)
          (chas-array-fds chas-array))
    ;; looks like we have to know when to stop
    (setf (chas-array-fds chas-array)
          (list (make-bfd start-cha-no new-font-no)
          (make-bfd stop-cha-no last-font-no)))))
    (t
     ;; looks like we may have to check and (possibly) alter
     ;; the values of intervening Font Descriptors
     (do* ((fds (chas-array-fds chas-array) (cdr fds))
     (next-fds (cdr fds) (cdr fds))
     (current-fd (car fds) (car fds))
     (next-fd (car next-fds) (car next-fds))
     (inside-new-font? nil)
     (bfd-cha-no (if (null inside-new-font?)
         start-cha-no
         stop-cha-no)))
    ((null next-fds) (setf (cdr fds) (list bfd)))
       (cond ((=& bfd-cha-no (bfd-cha-no current-fd))
        ;; if there already is a FD at the right place, bash
        ;; its values rather than splicing a new FD into the list
        (cond ((null inside-new-font?)
         (setq inside-new-font? t)
         (setq last-font-no (bfd-font-no current-fd))
         (setf (bfd-font-no current-fd)
         (logior& (bfd-font-no current-fd)
            (bfd-font-no bfd))))
        (t
         ;; if there is already a FD where we want
         ;; to stop, then we just leave it alone
         (return))))
       ((>& bfd-cha-no (bfd-cha-no current-fd))
        (cond ((null next-fd)

         (setf (cdr fds) (list bfd))
         (return))
        ((<& bfd-cha-no (bfd-cha-no next-fd))
         ;; we are in between 2 FD's so we should splice in here
         (setf (cdr fds) (cons bfd next-fds))
         (return))
        ((=& bfd-cha-no (bfd-cha-no next-fd))
         (setf (bfd-font-no next-fd)
         (logior& (bfd-font-no next-fd) (bfd-font-no bfd)))
         (return))))
       (t
        ;; record the last-font-no
        (setq last-font-no (bfd-font-no current-fd))))))







  (let ((new-bfd (make-bfd start-cha-no new-font-no)))
    ;; first, insert the Font Descriptor
    (insert-bfd row new-bfd)
    ;; now OR the new-font-no into any intervening Font Descriptors
    (dolist (bfd (remaining-bfds row start-cha-no))
      (cond ((>& (bfd-cha-no bfd) stop-cha-no)
       ;; there are still subsequent FD's down the row so we need
       ;; to add a FD which


|#

#|
(DEFBOXER-COMMAND COM-BOLDFACE-FONT-WORD ()
  "Changes the next word to be in boldface. "
  (WITH-MULTIPLE-EXECUTION
    (MOVE-POINT (BP-CHANGE-STYLE-FORWARD-WORD-VALUES *POINT* ':BOLD)))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-BOLDFACE-FONT-CHA ()
  "Change the next character to be in boldface. "
  (WITH-MULTIPLE-EXECUTION
    (LET ((OLD-CHAR (CHA-AT-CHA-NO (POINT-ROW) (POINT-CHA-NO))))
      (CHANGE-CHA-AT-CHA-NO (POINT-ROW)
      (POINT-CHA-NO)
      (MAKE-CHAR OLD-CHAR
           (CHAR-BITS OLD-CHAR) ':BOLD (CHAR-FONT-FAMILY OLD-CHAR)))
      (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-ITALICS-FONT-WORD ()
  "Changes the next word to be in italics. "
  (WITH-MULTIPLE-EXECUTION
    (MOVE-POINT (BP-CHANGE-STYLE-FORWARD-WORD-VALUES *POINT* :ITALIC)))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-ITALICS-FONT-CHA ()
  "Change the next character to be in italics. "
  (WITH-MULTIPLE-EXECUTION
    (LET ((OLD-CHAR (CHA-AT-CHA-NO (POINT-ROW) (POINT-CHA-NO))))
      (CHANGE-CHA-AT-CHA-NO (POINT-ROW)
      (POINT-CHA-NO)
      (MAKE-CHAR OLD-CHAR
           (CHAR-BITS OLD-CHAR) ':ITALIC (CHAR-FONT-FAMILY OLD-CHAR)))
      (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))))
  boxer-eval::*novalue*)


|#

#+mcl
(defvar *macl-standard-font-sizes*)

#+mcl
(defvar *current-macl-font-size* 10)

#+mcl
(defvar *macl-typeface* "courier")

#+mcl
(eval-when (eval load)
  (setq *macl-standard-font-sizes* '(9 10 12 14 18 24))
  (setf (cdr (last *macl-standard-font-sizes*)) *macl-standard-font-sizes*)
  )

#+mcl
(defun next-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (cadr (member *current-macl-font-size*
                                            *macl-standard-font-sizes*)))
        :plain))

#+mcl
(defun larger-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (max *current-macl-font-size*
                                   (cadr (member *current-macl-font-size*
                                                 *macl-standard-font-sizes*))))
        :plain))

#+mcl
(defun smaller-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (min *current-macl-font-size*
                                   (let ((prev (car *macl-standard-font-sizes*))
                                         (here *macl-standard-font-sizes*))
                                     (do* ((where (cdr here) (cdr where))
                                           (size (car where) (car where)))
                                          ((eq where here) prev)
                                       (when (= size *current-macl-font-size*)
                                         (return prev))
                                       (setq prev size)))))
        :plain))

#+mcl
(defun max-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (let ((max 0)
                                    (here *macl-standard-font-sizes*))
                                (do* ((where (cdr here) (cdr where))
                                      (size (car where) (car where)))
                                     ((eq where here) max)
                                  (when (> size max) (setq max size)))))
        :plain))

(defvar *font-size-state-var* 0)

#+mcl
(defboxer-command com-toggle-font-size ()
  "Cycles through the available font sizes for the Boxer Window"
  (bw::reinitialize-font-map (bw::sheet-font-map *boxer-pane*)
                             (next-font-spec))
  boxer-eval::*novalue*)

#-mcl
(defboxer-command com-toggle-font-size ()
  "Cycles through the available font sizes for the Boxer Window"
  (cond ((eql *font-size-state-var* 0)
   (progn
     (setq *font-size-state-var* 1)
     (com-fat)))
  ((eql *font-size-state-var* 1)
   (progn
     (setq *font-size-state-var* 2)
     (com-bloat)))
  (t
   (progn (setq *font-size-state-var* 0)
    (com-nutri-system))))
  boxer-eval::*novalue*)

;;;;; Global Fonts

(defvar *supported-font-sizes*
  #+mcl '(9 10 12 14 18 24) #+lwwin '(6 8 10 12 14 16 18 24))

(defboxer-command COM-NUTRI-SYSTEM ()
  "shrink the fonts"
  ;; mac version just resets *default-font-descriptor*
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size
                                 (reverse *supported-font-sizes*)))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the smallest supported font size"
                                 (car *supported-font-sizes*)))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

(defboxer-command COM-FAT ()
  "make-fonts bigger"
  ;; mac version just resets *default-font-descriptor*
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size *supported-font-sizes*))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the largest supported font size"
                                 (car (last *supported-font-sizes*))))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

(defboxer-command COM-BLOAT ()
  "bloat the fonts"
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size *supported-font-sizes*))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the largest supported font size"
                                 (car (last *supported-font-sizes*))))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

;;;;
;;;; FILE: draw-low-opengl.lisp
;;;;

;;; Opengl configuration variables
;;; in theory, the GPU will be queried as a redisplay init and these
;;; parameters will be initialized.
(defvar *OpenGL-lots-o-memory-yay* t
  "Mostly having to do with how much stuff we will try and cache on GPU")

(defun configure-gpu-parameters ()
  ;; font vars
;  *opengl-font-start*  ; 0 or 32
;  *opengl-font-count*  ; 128 or 256
  )
(def-redisplay-initialization (configure-gpu-parameters))

;;; why doesn't this work ?  It's a documented function
; (gp:port-depth window))

(defun make-boxer-font-ogl (rawfontspec calculate-parameters?)
  (let* ((alias (font-family-alias (car rawfontspec)))
         (fontspec (if alias (list* alias (cdr rawfontspec)) rawfontspec))
         (sysfont (boxer-font-spec->lw-font fontspec))
         (font-no (fontspec->font-no fontspec)))
    (cond ((null font-no)
           ;; no cached font, we have to be careful here because a possible
           ;; scenario is that we are translating a mac font which could come out
           ;; as an existing PC font
           ;; wait until we have a solid native font before converting to an
           ;; opengl font
           (let* ((oglfont  (if (null calculate-parameters?)
                              (bw::register-opengl-font-from-native-font sysfont)
                              (bw::make-opengl-font-from-native-font sysfont)))
                  (localname (unless (null oglfont)
                               (gp:font-description-attribute-value
                                (gp:font-description sysfont) :family)))
                  (newfontspec (list* localname (cdr fontspec)))
                  (new-font-no (fontspec->font-no newfontspec T)))
             (unless (null localname)
               (set-font-family-alias (car rawfontspec) localname)
               (unless (member localname *font-families* :test #'string-equal)
                 (nconc *font-families* (list localname))))
             (or (find-cached-font new-font-no nil)
                 (cache-font oglfont new-font-no nil))
             new-font-no))
      (t
       (or (find-cached-font font-no nil)
           (let ((font (if (null calculate-parameters?)
                         (bw::register-opengl-font-from-native-font
                          (boxer-font-spec->lw-font fontspec))
                         (bw::make-opengl-font-from-native-font sysfont))))
             (cache-font font font-no nil)))
       font-no))))

;; the LW font internals looks like it supports :underline, but leave out for
;; now because it isn't documented
(defun boxer-font-spec->lw-font (spec)
  (let ((family (car spec))
        (size (or (cadr spec) 10))
        (styles (cddr spec)))
    (gp:find-best-font *boxer-pane*
                       (gp:make-font-description :family family
                                                 :size size
                                                 :weight (if (member :bold styles)
                                                           :bold
                                                           :normal)
                                                 :slant (if (member :italic styles)
                                                          :italic
                                                          :roman)
                                                 :underline
                                                 (not (null (member :underline
                                                                    styles)))))))

; not used ?
;(defun fast-cha-ascent () (gp:get-font-ascent %drawing-array))
;(defun fast-cha-hei ()
;  (+& (gp:get-font-ascent  %drawing-array)
;      (gp:get-font-descent %drawing-array)))

#| ;; no longer used?
(defmacro with-drawing-into-new-bitmap ((bitmap-name
           drawable bit-width bit-height
           . window-system-specific-args)
          &body body)
  (declare (ignore window-system-specific-args))
  `(let ((,bitmap-name (make-offscreen-bitmap ,drawable ,bit-width ,bit-height)))
     (drawing-on-bitmap (,bitmap-name)
       (progn
         (%erase-rectangle ,bit-width ,bit-height 0 0 ,bitmap-name)
         ,@body))
     ,bitmap-name))
|#

; gp::erase-rectangle ignores the state of the transform
; so it loses for sub boxes during redisplay
  ;(gp:clear-rectangle window x y w h)

;;;;; obsolete.... commented out 7/18/13
;(defvar *font-map* (make-array '(8)))
;(defvar *font-codes* (make-array '(8)))

;;; ???
;(defun sheet-font-map (w)
;  (declare (ignore w))
;  *font-map*)

#|
;; make a true type font, and associate it with a font number
;; font may already exists be associated
;; cases: 1) no font-no => get new ttf-font (a) fill cache or (b) not (c) cache is filled with another font
;;        2) font-no => (a) cache is filled or (b) fill cache

(defun make-boxer-font (rawfontspec &optional (calculate-parameters? T))
  (let* ((alias (font-family-alias (car rawfontspec)))
         ;; this allows boxer level font translation (eq "Geneva" => "Arial")
         (fontspec (if alias (list* alias (cdr rawfontspec)) rawfontspec))
         (font-no (fontspec->font-no fontspec)))
    (cond ((null font-no)
           ;; no assigned font
           (let* ((ttf-font (bw::register-ttf-font fontspec))
                  (ttf-fontspec (bw::ttf-fontspec ttf-font))
                  ;; register-ttf-font may map the requested font into an existing ttf-font
                  (ttf-font-no (fontspec->font-no ttf-fontspec T)))
             (when (not (string= (car fontspec) (car ttf-fontspec)))
               (set-font-family-alias (car rawfontspec) (car ttf-fontspec)))
             (unless (member (car ttf-fontspec) *font-families* :test #'string-equal)
               (nconc *font-families* (list (car ttf-fontspec))))
             (unless (find-cached-font ttf-font-no)
               (cache-font ttf-font ttf-font-no))
             (when calculate-parameters?
               (bw::cache-ttf-font *boxer-pane* :font ttf-font))
             ttf-font-no))
          (t
           (unless (find-cached-font font-no)
             (let ((ttf-font (bw::register-ttf-font fontspec)))
               (cache-font ttf-font font-no)
               (when calculate-parameters?
                 (bw::cache-ttf-font *boxer-pane* :font ttf-font))))
           font-no))))
|#

;(defun %set-font-size (boxer-font new-size)
;  (let ((new-idx (%font-size-to-idx new-size)))
;    (dpb new-idx (byte %%font-size-bit-length   %%font-size-bit-pos) boxer-font)))

#| ;; leave this here in case we need to remember how to do software clipping
(defun %bitblt-to-screen (alu wid hei from-array fx fy tx ty)
  (let* ((scaled-to-x (+& %origin-x-offset tx)) (scaled-to-y (+& %origin-y-offset ty))
         (clipped-to-x (clip-x scaled-to-x))    (clipped-to-y (clip-y scaled-to-y))
   (+wid (abs& wid))
   (+hei (abs& hei))
   (lef-overrun (max& 0 (-& scaled-to-x clipped-to-x)))
   (top-overrun (max& 0 (-& scaled-to-y clipped-to-y)))
   (rig-overrun (max& 0 (-& (+& clipped-to-x +wid)
          (clip-x (+& clipped-to-x +wid)))))
   (bot-overrun (max& 0 (-& (+& clipped-to-y +hei)
          (clip-y (+& clipped-to-y +hei)))))
   (clipped-wid (*& (sign-of-no wid)
        (max& 0 (-& +wid lef-overrun rig-overrun))))
   (clipped-hei (*& (sign-of-no hei)
        (max& 0 (-& +hei top-overrun bot-overrun)))))
    (or (zerop& clipped-wid)
        (zerop& clipped-hei)
        (gp:pixblt %drawing-array alu from-array clipped-to-x clipped-to-y
                   clipped-wid clipped-hei fx fy))))
|#

#|
(defun %bitblt-from-screen (alu wid hei to-array fx fy tx ty)
  (let* ((scaled-from-x (+& %origin-x-offset fx))
         (scaled-from-y (+& %origin-y-offset fy))
         (clipped-from-x (clip-x scaled-from-x))
         (clipped-from-y (clip-y scaled-from-y))
         (+wid (abs& wid))
   (+hei (abs& hei))
   (lef-overrun (max& 0 (-& scaled-from-x clipped-from-x)))
   (top-overrun (max& 0 (-& scaled-from-y clipped-from-y)))
   (rig-overrun (max& 0 (-& (+& clipped-from-x +wid)
                                  (clip-x (+& clipped-from-x +wid)))))
   (bot-overrun (max& 0 (-& (+& clipped-from-y +hei)
                                  (clip-y (+& clipped-from-y +hei)))))
   (clipped-wid (*& (sign-of-no wid) (max& 0 (-& +wid lef-overrun rig-overrun))))
   (clipped-hei (*& (sign-of-no hei) (max& 0 (-& +hei top-overrun bot-overrun)))))
    (or (zerop& clipped-wid)
        (zerop& clipped-hei)
        (gp:pixblt to-array alu %drawing-array tx ty
                   clipped-wid clipped-hei clipped-from-x clipped-from-y))))
|#

#|
(defun %bitblt-in-screen (alu wid hei array fx fy tx ty)
  (let* ((scaled-from-x (+& %origin-x-offset fx))
         (scaled-from-y (+& %origin-y-offset fy))
         (scaled-to-x   (+& %origin-x-offset tx))
         (scaled-to-y   (+& %origin-y-offset ty))
         (clipped-from-x (clip-x scaled-from-x))
         (clipped-from-y (clip-y scaled-from-y))
   (clipped-to-x (clip-x scaled-to-x))
   (clipped-to-y (clip-y scaled-to-y))
   (+wid (abs& wid))
   (+hei (abs& hei))
   (lef-overrun (max& 0
                            (-& scaled-from-x clipped-from-x)
                            (-& scaled-to-x clipped-to-x)))
   (top-overrun (max& 0
          (-& scaled-from-y clipped-from-y)
          (-& scaled-to-y clipped-to-y)))
   (rig-overrun (max& 0
          (-& (+& clipped-from-x +wid)
        (clip-x (+& clipped-from-x +wid)))
          (-& (+& clipped-to-x +wid)
        (clip-x (+& clipped-to-x +wid)))))
   (bot-overrun (max& 0
          (-& (+& clipped-from-y +hei)
                                (clip-y (+& clipped-from-y +hei)))
          (-& (+& clipped-to-y +hei)
        (clip-y (+& clipped-to-y +hei)))))
   (clipped-wid (*& (sign-of-no wid) (max& 0 (-& +wid lef-overrun rig-overrun))))
   (clipped-hei (*& (sign-of-no hei) (max& 0 (-& +hei top-overrun bot-overrun)))))
    (or (zerop& clipped-wid)
        (zerop& clipped-hei)
        (gp:pixblt array alu array clipped-to-x clipped-to-y
                   clipped-wid clipped-hei clipped-from-x clipped-from-y))))
|#

;; this could draws into an auxiliary buffer and then
;; transfers the bits from that buffer into an ogl-pixmap
#|
(defmacro with-system-dependent-bitmap-drawing ((bitmap &optional
                                                        bitmap-width bitmap-height)
                  &body body)
  (declare (ignore bitmap-width bitmap-height))
  `(opengl::rendering-on (*boxer-pane*)
     (unwind-protect
         (progn
           (bw::gl-draw-buffer bw::*gl-aux1*)
           (progn . ,body)
           (bw::gl-flush)
           (opengl::%pixblt-from-screen ,bitmap 0 0
                                        (opengl::ogl-pixmap-width  ,bitmap)
                                        (opengl::ogl-pixmap-height ,bitmap)
                                        0 0 bw::*gl-aux1*))
       (bw::gl-draw-buffer bw::*gl-back*))))
|#

;;;;
;;;; FILE: editor.lisp
;;;;

#|
(defun get-boxer-status-string (outermost-box-name other-string)
  (flet ((get-boxer-version-string ()
           (or *boxer-version-info*
               (system-version 'boxer))))

    (cond ((null other-string)
     (when (null outermost-box-name)
       (setq outermost-box-name (name-string (outermost-box))))
     (if (null *editor-numeric-argument*)
         (format nil "~A |         Outermost Box: ~A"
           (get-boxer-version-string) outermost-box-name)
         (format nil "~A |         Outermost Box: ~A | Arg: ~D"
           (get-boxer-version-string) outermost-box-name
           *editor-numeric-argument*)))
    (t
     (if (null *editor-numeric-argument*)
         (format nil "~A |         ~A" (get-boxer-version-string) other-string)
         (format nil "~A |         ~A | Arg: ~D" (get-boxer-version-string) other-string *editor-numeric-argument*))))))


(defun redraw-status-line (&optional new-name other-string)
  (window-system-dependent-redraw-status-line
   (get-boxer-status-string new-name other-string)
   (not (null new-name))))
|#

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
;;;; FILE: keys-new.lisp
;;;;
(boxer-eval::defboxer-key (bu::>-key 2) com-fat)
(boxer-eval::defboxer-key (bu::<-key 2) com-nutri-system)

#+sun (boxer-eval::defboxer-key bu::R2-key com-print-screen)

#+sun (boxer-eval::defboxer-key (bu::R2-key 2) com-print-screen-to-file)

#+apple (boxer-eval::defboxer-key (bu::return-key 1) com-doit-now) ; should be com-step

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

;;;;
;;;; FILE: opengl-utils.lisp
;;;;

;; conversion from a native font to an OpenGL font
;; 1) get a LW font from a boxer font spec
;; 2) make an opengl font struct
;; 3) calculate height & width info for the font using the native font
;;    we could have opengl do it but then we have to convert formats & work
;;    in floating point
;;
;; 4) use win32::wgl-use-font to cache local font in GPU
;; *) font caching mechanism uses the DL-base-addr slot in opengl font struct
;; *) Note that the :start arg to wgl-use-font should be the same offset used
;;    when drawing chars, that's what *opengl-starting-char-index* is for

(defun register-opengl-font-from-native-font (native-font &optional (pane *boxer-pane*))
  (declare (ignore pane))
  (%make-opengl-font :native-font native-font))

(defun make-opengl-font-from-native-font (native-font &optional (pane *boxer-pane*))
  (let ((oglfont (register-opengl-font-from-native-font native-font)))
    (fill-oglfont-parameters oglfont pane)
    oglfont))

(defvar *use-capogi-fonts* t) ; want to allow option for scaleable vector OpenGL fonts in Windows
