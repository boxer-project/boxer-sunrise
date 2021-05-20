;;;;
;;;; The attic
;;;;

;;;;
;;;; FILE: boxdef.lisp
;;;;

(defvar *uc-copyright-free* t)

;; sgithens 2021-05-07 Oddly it doesn't look like these three are used anymore...

(DEFVAR *CURRENT-SCREEN-BOX* NIL
  "The Lowest Level Screen Box Which Contains the *Point*")

(DEFVAR *MARKED-SCREEN-BOX* NIL
  "The Lowest Level Scren Box Which Contains the *mark*")

(DEFVAR *OUTERMOST-BOX* NIL
  "Inside of REDISPLAYING-WINDOW, this variable is bound to the window
   being redisplayed's outermost-box. This is the box which currently
   fills that window.")

(defvar *egc-enabled?* nil)

(DEFVAR *CONTROL-CHARACTER-DISPLAY-PREFIX* #\
  "For display of control characters (all of them until we decide on different prefixes")


;;;;
;;;; FILE: boxwin-opengl.lisp
;;;;


(defvar *blinker-alpha-value* .3)

(defun update-blinker-color ()
  #+win32
  (let ((bc (color:get-color-spec 'win32::color_highlight)))
    (setq *blinker-color* (make-ogl-color (color:color-red bc)
                                          (color:color-green bc)
                                          (color:color-blue bc)
                                          *blinker-alpha-value*))))

;; for ALT key handling
#+lispworks4
(defmethod capi::interface-keys-style ((self boxer-frame)) :emacs)

;(defun boxer-expose-window-handler (pane x y wid hei)
;  (declare (ignore pane x y wid hei))
;  (funcall *expose-window-handler-function*))

;(defun boxer-expose-window-handler (pane x y wid hei)
;  (declare (ignore pane x y))
;    (funcall *expose-window-handler-function* wid hei))

;(defun bootstrap-expose-window-function (wid hei)
;  (rendering-on (*boxer-pane*) (ogl-reshape wid hei)))

#|
(defsetf sheet-blinker-list (window) (new-list)
  `(let ((entry (box::fast-assq ,window *boxer-window-blinker-alist*)))
     (if (null entry)
   (push ,new-list *boxer-window-blinker-alist*)
   (setf (cdr entry) ,new-list))))
|#

#|
(defmacro with-open-blinker ((blinker) &body body)
  `(macrolet ((draw-generic-blinker (blinker)
                `(etypecase ,blinker
                   (region-row-blinker (draw-region-row-blinker ,blinker))
                   (blinker (draw-blinker ,blinker)))))
     (boxer::without-interrupts
       (unwind-protect
         (progn (when (blinker-visibility ,blinker)
                  (box::drawing-on-window-without-prepare-sheet (*boxer-pane*)
                    (box::with-origin-at (0 0) ; invoke the quickdraw scaling
           (draw-generic-blinker ,blinker))))
                ;; Erase the blinker if it is visible
                ;; then do whatever you were going to do
                . ,body)
         (when (blinker-visibility ,blinker)
           ;; If the blinker is supposed to be visible, then redraw it
           (box::drawing-on-window-without-prepare-sheet (*boxer-pane*)
             (box::with-origin-at (0 0) ; invoke the quickdraw scaling
               (draw-generic-blinker ,blinker))))))))

(defmacro with-open-blinkers (blinker-list &body body)
  `(macrolet ((draw-generic-blinker (blinker)
     `(etypecase ,blinker
       (region-row-blinker (draw-region-row-blinker ,blinker))
       (blinker (draw-blinker ,blinker)))))
;     (box::drawing-on-window-without-prepare-sheet (*boxer-pane*) ; +++ this is probably not the right thing, fix
;; **** Almost, actually this needs to be wrapped around the blinker drawing
;; **** but NOT the body since the body will be inside a drawing-on-window
      (unwind-protect
   (progn (boxer::without-interrupts
                  (boxer::drawing-on-window-without-prepare-sheet (*boxer-pane*)
                    ;; **** this will set the value of the offset to be the
                    ;; **** origin of the *boxer-pane*
                    (boxer::with-origin-at (0 0)
                      ;; **** this will set the offset in the window system
          (dolist (b ,blinker-list)
            (when (blinker-visibility b)
                          (draw-generic-blinker b))))))
    . ,body)
       (boxer::without-interrupts
         (boxer::drawing-on-window-without-prepare-sheet (*boxer-pane*)
           ;; **** this will set the value of the offset to be the
           ;; **** origin of the *boxer-pane*
           (boxer::with-origin-at (0 0)
             ;; **** this will set the offset in the window system
       (dolist (b ,blinker-list)
         (when (blinker-visibility b)
                 (draw-generic-blinker b)))))))))

|#

#|
(defun image-to-bitmap (image)
  (unless (null image)
    (let* ((wid (gp:image-width image)) (hei (gp:image-height image))
           (bim (make-offscreen-bitmap *boxer-pane* wid hei)))
      (with-system-dependent-bitmap-drawing (bim wid hei)
        (%erase-rectangle wid hei 0 0 bim)
        (with-pen-color (boxer::*red*)
          (boxer::draw-rectangle alu-seta 10 10 10 10))
        (gp:draw-image bim image 0 0))
      (values bim wid hei))))
|#

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
;;;; FILE: comdef.lisp
;;;;

;; #-opengl
;; (defun track-mouse-area (hilight-fun &key x y width height)
;;   (let ((backing-store (allocate-backing-store width height)))
;;     (drawing-on-window (*boxer-pane*)
;;       (let ((min-x (-& x *border-tab-hysteresis*))
;;             (min-y (-& y *border-tab-hysteresis*))
;;             (max-x (+& x width *border-tab-hysteresis*))
;;             (max-y (+& y height *border-tab-hysteresis*))
;;             (icon-on? t))
;;         (flet ((icon-on ()
;;                  (erase-rectangle width height x y)
;;                  (funcall hilight-fun x y width height)
;;                  (force-graphics-output)
;;                  (setq icon-on? T))
;;                (icon-off ()
;;                  (repaint) ; gak !!
;;                  ;(bitblt-to-screen alu-seta width height backing-store 0 0 x y)
;;                  ;(force-graphics-output)
;;                  (setq icon-on? nil)))
;;           ;; 1st grab what's on the screen...
;;           (bitblt-from-screen alu-seta width height backing-store x y 0 0)
;;           ;; initially turn the icon on since that is how we got here in the 1st place
;;           (icon-on)
;;           (multiple-value-bind (final-x final-y)
;;               (with-mouse-tracking ((mouse-x x) (mouse-y y))
;;                 (progn
;;                   (cond ((and (null icon-on?)
;;                               ;; if the icon is off, and we move back in
;;                               (<& min-x mouse-x max-x) (<& min-y mouse-y max-y))
;;                          ;; then turn the icon back on
;;                          (icon-on))
;;                         ((and icon-on?
;;                               ;; if the icon is on and we move out
;;                               (or (not (<& min-x mouse-x max-x))
;;                                   (not (<& min-y mouse-y max-y))))
;;                          ;; then turn off the visual indicator
;;                          (icon-off)))))
;;             ;; first turn the icon off if it is on...
;;             (unless (null icon-on?) (icon-off))
;;             (deallocate-backing-store backing-store)
;;             ;; now return whether we are still on...
;;             (and (<& min-x final-x max-x) (<& min-y final-y max-y))))))))

;;;; Wrong! You should be using modes now !!!

#|
;;; leave it here until we manage to flush old code
;;; utilities for temporarily rebinding keys (like for
;;; copying/moving regions)

(defvar *saved-key-functions* nil)

;;; Note, this is saving and rebinding TOP LEVEL bindings
;;; shadowed bindings will remain unaffected
(defun save-and-rebind-key (key-name new-function)
  (let ((existing (if (boundp key-name) (caddr (symbol-value key-name)) ':unbound))
        (entry (fast-assq key-name *saved-key-functions*)))
    ;; record the old version
    (cond ((null entry) (push (cons key-name existing) *saved-key-functions*))
      (t (setf (cdr entry) existing)))
    ;; now set it to the new version
    (boxer-eval::boxer-toplevel-set-nocache
     key-name
     (boxer-eval::make-compiled-boxer-function
      :arglist nil :precedence 0 :infix-p nil :object new-function))))

;; key should look like 'bu::crap
(defun restore-saved-function (key-name)
  (let ((entry (cdr (fast-assq key-name *saved-key-functions*)))
        (vanilla (lookup-mode-key *global-top-level-mode* key-name)))
    (cond ((null entry)
           (cond ((null vanilla)
                  ;(warn "No saved function for ~A, Unbinding the key" key-name)
                  (boxer-eval::boxer-toplevel-nocache-unset key-name))
             (t
              (warn "No saved function for ~A, setting to top level value" key-name)
              (boxer-eval::boxer-toplevel-set-nocache key-name vanilla))))
      ((or (eq entry ':unbound) (eq entry boxer-eval::*novalue*))
       (boxer-eval::boxer-toplevel-nocache-unset key-name))
      ((boxer-eval::compiled-boxer-function? entry)
       (boxer-eval::boxer-toplevel-set-nocache key-name entry))
      (t
       ;; probably means the previous binding for the key was
       ;; to a box, right thing for now is to unbind the key
       (if (null vanilla)
         (boxer-eval::boxer-toplevel-nocache-unset key-name)
         (progn
          (warn "No saved function for ~A, setting to top level value" key-name)
          (boxer-eval::boxer-toplevel-set-nocache key-name vanilla)))))))
|#

#|
(defun make-generic-port (&rest foo)
  "make a generic port so the person may redirect it"
  (declare (ignore foo))
  (if (null *dummy-box*) (setq *dummy-box* (make-dummy-box)))
  (if (null *generic-port*)
      (progn (setq *generic-port* (port-to-internal *dummy-box*))
       (INSERT-CHA *POINT* *generic-port*)
       (set-mouse-cursor :retarget)
       (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0)
          #'com-redirect-generic-port)
       (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1
                                                            0 :graphics)
          #'com-redirect-generic-port)
       (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1
                                                            0 :sprite)
          #'com-redirect-generic-port)
       (add-redisplay-clue (point-row) ':insert)
       boxer-eval::*novalue*)
      (progn
  (boxer-editor-error "Use the generic port you have made.")
  boxer-eval::*novalue*)))
|#

#| ;; converted to using modes
(defun clean-mouse-port-state ()
  (reset-mouse-cursor)
  ;;; rebind the mouse-middle
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1
                                                    0 :graphics))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1
                                                    0 :sprite))
  (setq *generic-port* nil))
|#

#|
    (save-and-rebind-key main-cut-name #'com-suck-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 2 :graphics)
                         #-mcl (current-mouse-click-name 0 0 :graphics)
                         #'com-suck-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 2 :sprite)
                         #-mcl (current-mouse-click-name 0 0 :sprite)
                         #'com-suck-region)
    (save-and-rebind-key main-copy-name #'com-suck-copy-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 1 :graphics)
                         #-mcl (current-mouse-click-name 2 0 :graphics)
                         #'com-suck-copy-region)
    (save-and-rebind-key #+mcl (current-mouse-click-name 0 1 :sprite)
                         #-mcl (current-mouse-click-name 2 0 :sprite)
                         #'com-suck-copy-region)
|#

#|
  (restore-saved-function #+mcl (current-mouse-click-name 0 2)
                          #-mcl (current-mouse-click-name 0 0))
  (restore-saved-function #+mcl (current-mouse-click-name 0 2 :graphics)
                          #-mcl (current-mouse-click-name 0 0 :graphics))
  (restore-saved-function #+mcl (current-mouse-click-name 0 2 :sprite)
                          #-mcl (current-mouse-click-name 0 0 :sprite))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1)
                          #-mcl (current-mouse-click-name 2 0))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1 :graphics)
                          #-mcl (current-mouse-click-name 2 0 :graphics))
  (restore-saved-function #+mcl (current-mouse-click-name 0 1 :sprite)
                          #-mcl (current-mouse-click-name 2 0 :sprite))
|#

;;;;
;;;; FILE: coms-oglmouse.lisp
;;;;

#|  ; unused ?
(defun reconcile-region-blinker-list (region blinker-list)
  (let ((existing-blinkers (interval-blinker-list region)))
    (cond ((not (null existing-blinkers))
           (when *boxer-system-hacker*
             (error "Region, ~A, already has blinkers" region))
           (dolist (bl blinker-list) (remove-region-row-blinker bl)))
          (t
           ;; we can just set it because the region redisplay will
           ;; handle the rest-see update-row-blinker-list called by
           ;; interval-update-redisplay-all-rows (region.lisp)
           (setf (interval-blinker-list region) blinker-list)
           ;; we do have to hack the visibility flag because we know
           ;; that the blinkers are already visible
           (setf (interval-visibility region) t)))))
|#

#|
;;; Multi-purpose box size changer.
;;; Lets you get by with just one button for changing size.

;;; Normal boxes:
;;;If clicked on the left half of the box, make it full screen.
;;;If on the right half of a box, make the box smaller.
;;; Shrunken boxes:
;;;Make full screen.
;;; Full Screen boxes:
;;;If clicked on the left half, make the box shrunken.
;;;If on the right half, make it normal size.
(defboxer-command com-mouse-change-box-size (&optional (window *boxer-pane*)
                                             (x (bw::boxer-pane-mouse-x))
                                             (y (bw::boxer-pane-mouse-y))
                                             (mouse-bp
                                              (mouse-position-values x y))
                                             (click-only? t))
  "change the size of the box according to where you mouse"
  window x y click-only? ;  (declare (ignore window x y click-only?))
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (let ((new-box (bp-box mouse-bp))
  (new-row (bp-row mouse-bp))
  (mouse-screen-box (bp-screen-box mouse-bp))
  (new-cha-no (bp-cha-no mouse-bp)))
    (when (and (not (null new-row)) (box? new-box))
      (let ((actual-obj (screen-obj-actual-obj mouse-screen-box)))
  (cond ((eq mouse-screen-box (outermost-screen-box))
         (multiple-value-bind (row cha-no screen-box rel-x rel-y)
       (mouse-position-values x y)
     (declare (ignore row cha-no screen-box rel-y))
     (send-exit-messages new-box mouse-screen-box)
     (move-point-1 new-row new-cha-no mouse-screen-box)
     (if (< (* rel-x 2)
      (screen-object-width mouse-screen-box))
         (com-collapse-box)
         (com-shrink-box))))
        ((shrunken? actual-obj)
         (send-exit-messages
    new-box mouse-screen-box
    (eq (superior-box (point-box)) new-box))
         (move-point-1 new-row new-cha-no mouse-screen-box)
         ;; no send-exit-messages yet.
         (com-mouse-set-outermost-box window x y mouse-bp click-only?))
        (t
         (multiple-value-bind (row cha-no screen-box rel-x rel-y)
     (screen-obj-at-position x y)
     row cha-no screen-box rel-y
     (send-exit-messages new-box mouse-screen-box)
     (move-point-1 new-row new-cha-no mouse-screen-box)
     (cond ((< (* rel-x 2)
         (screen-object-width mouse-screen-box))
      (enter new-box)
      (com-expand-box))
           (t
      (com-collapse-box)))))))))
  boxer-eval::*novalue*)

|#

;;; mouse dispatch function

;;; mouse dispatch function
;(defun com-mouse-left-do-it-all (window x y mouse-bp click-only?)
;  (if #+clx (bw::mouse-on-region-being-defined-p) #-clx nil
;(com-suck-region window x y mouse-bp click-only?)
;      (com-mouse-collapse-box  window x y mouse-bp click-only?)))
;(defun com-mouse-right-do-it-all (window x y mouse-bp click-only?)
;  (if #+clx (bw::mouse-on-region-being-defined-p) #-clx nil
;      (com-suck-copy-region window x y mouse-bp click-only?)
;      (com-mouse-expand-box window x y mouse-bp click-only?)))

;;;
#| ;; old, use modes now
(defun entering-suitcase-bindings ()
  (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0)
                       #'com-bring-back-region)
  (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0 :graphics)
                       #'com-bring-back-region)
  (save-and-rebind-key (current-mouse-click-name #+mcl 0 #-mcl 1 0 :sprite)
                       #'com-bring-back-region))

(defun exiting-suitcase-bindings ()
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0 :graphics))
  (restore-saved-function (current-mouse-click-name #+mcl 0 #-mcl 1 0 :sprite)))
|#

#|
(defun mouse-in-scroll-bar-internal (screen-box x y click-only?
            box-window-x box-window-y
            wid hei left top right bottom)
  (declare (ignore left))
  (let* ((inside-hei (-& hei top bottom))
   (available-shaft (-& inside-hei (*& *scroll-button-height* 2)))
   (ed-box (screen-obj-actual-obj screen-box))
   (norows (length-in-rows ed-box))
         (maxrow nil) ; a cache for the lowest desired row number
   (scroll-row (scroll-to-actual-row screen-box))
   (current-row-no (if (null scroll-row) 0
           (row-row-no ed-box scroll-row)))
   ;; mini scroll if there is more room than we want to use
   (mini-scroll? (>& available-shaft
         (* *max-scroll-grid-increment* norows)))
   (shaft-height (if mini-scroll?
         (* *max-scroll-grid-increment* norows)
         available-shaft))
   (shaft-y (if mini-scroll?
          (+& top *scroll-button-height*
        (floor (-& available-shaft
             (* *max-scroll-grid-increment* norows))
         2)
                          box-window-y)
          (+& top *scroll-button-height* box-window-y)))
   (elevator-y (+& (floor (* (/ current-row-no norows) shaft-height))
       shaft-y))
   (row-heights (make-array norows :initial-element nil))
         (x-offset (+& box-window-x (-& wid right))))
    (declare (simple-vector row-heights))
    (labels ((new-row-no (y)
               ;; make the lower limit include a full box of text
               ;; scrolling down to the last row makes it hard to use
               ;; the scrolling buttons on the resulting 1 row high box
               (let ((raw-row (round (* (/ (min& (max& 0 (-& y shaft-y)) shaft-height)
                         shaft-height)
                      (1-& norows)))))
                 (cond ((null maxrow)
                        (let ((end (ending-row-no raw-row)))
                          (cond ((>=& end norows)
                                 ;; cache should be filled by call to ending-row-no
                                 (or maxrow raw-row))
                                (t raw-row))))
                       (t (min raw-row maxrow)))))
       (get-row-height (row-no)
         (let ((entry (svref& row-heights row-no)))
     (cond ((null entry)
      (let* ((edrow (row-at-row-no ed-box row-no))
             (scrow (car (screen-objs edrow)))
             (row-height
        (if (and scrow
           (not (screen-obj-y-got-clipped?
                 scrow)))
            (screen-obj-hei scrow)
            (estimate-row-height edrow))))
        (setf (svref& row-heights row-no) row-height)
        row-height))
           (t entry))))
       (draw-scroll-grid ()
               ;; erase the box border
               (erase-rectangle *scroll-grid-width* shaft-height
                                x-offset shaft-y)
               (let* ((single-incr (/ shaft-height norows))
                      (draw-singles? (< *min-scroll-grid-increment*
                                        single-incr)))
                 (when (< *min-scroll-grid-increment* (* 10 single-incr))
                   ;; don't draw grid lines at all if they would be too dense
                   (dotimes (i norows)
                     (let ((y-offset (+& (floor (* i single-incr)) shaft-y)))
                       (cond ((zerop& (mod i 10))
                              (draw-rectangle alu-seta
                                              (-& *scroll-grid-width* 2) 2
                                              x-offset y-offset))
                             ((zerop& (mod i 5))
                              (draw-rectangle alu-seta
                                              (-& *scroll-grid-width* 4) 2
                                              (+& x-offset 2) y-offset))
                             ((not (null draw-singles?))
                              (draw-rectangle alu-seta
                                              (-& *scroll-grid-width* 6) 1
                                              (+& x-offset 3)
                                              y-offset))))))))
       (ending-row-no (starting-row)
         (do* ((last-row (length row-heights)) (height 0)
         (row starting-row (1+& row)))
        ((>=& row last-row)
                     (setq maxrow starting-row) ;; fill the maxrow cache
                     row)
     ;; need to do this AFTER last-row check
     (setq height (+& height (get-row-height row)))
     (when (>=& height inside-hei)
                   (return row))))
       (draw-line-indicator ()
               (erase-rectangle *scroll-grid-width* 10 x-offset (-& shaft-y 10))
               ;; make the line counter 1-based
               (draw-string alu-seta *box-border-label-font-no*
                            (elevator-row-string (1+ current-row-no))
                            x-offset (-& shaft-y 10))
               (erase-rectangle *scroll-grid-width* 10
                                x-offset (+& shaft-y shaft-height))
               (draw-string alu-seta *box-border-label-font-no*
                            (elevator-row-string
                             (1+ (ending-row-no current-row-no)))
                            x-offset (+& shaft-y shaft-height)))
       (draw-temp-elevator ()
         (draw-rectangle alu-xor
                               (-& *scroll-grid-width* 2)
                               (+& *scroll-button-height* 2)
             x-offset (1-& elevator-y)))
       (erase-temp-elevator ()
         (draw-rectangle alu-xor
                               (-& *scroll-grid-width* 2)
                               (+& *scroll-button-height* 2)
             x-offset (1-& elevator-y))))
      (if click-only?
    ;; don't have to do tracking, just figure out the row
    (set-scroll-to-actual-row screen-box
            (row-at-row-no ed-box (new-row-no y)))
    ;; draw the grid and track
    (drawing-on-window (*boxer-pane*)
            (with-clipping-inside (x-offset (- shaft-y 10)
                                            *scroll-grid-width* (+ 20 shaft-height))
              (draw-scroll-grid)
              ;; draw the original
              (draw-temp-elevator)
              (draw-line-indicator)
              (with-mouse-tracking ((mouse-x x) (mouse-y y))
                (let ((new (new-row-no mouse-y)))
                  (unless (=& new current-row-no)
                    (erase-temp-elevator)
                    (setq current-row-no new
                          elevator-y (+& (floor (* (/ current-row-no norows)
                                                   shaft-height))
                                         shaft-y))
                    (draw-line-indicator)
                    (draw-temp-elevator))))
              (erase-temp-elevator)
              (erase-rectangle *scroll-grid-width* shaft-height x-offset shaft-y))
            (force-graphics-output)
      ;; actually make the change
      (set-scroll-to-actual-row screen-box
                                      (new-elevator-scrolled-row ed-box
                                                                 current-row-no))
      (set-force-redisplay-infs? screen-box))))))
|#

#|
(defvar *christmas-half-time* 0.5)
(defvar *number-of-christmas-blinks* 3)

(defboxer-command com-christmas-tree ()
  "Lights up the mouse sensitive parts of a box's border"
  ;; first, if there already is an existing region, flush it
  (reset-region)
  (drawing-on-window (*boxer-pane*)
  (let* ((screen-box (point-screen-box))
   (box-type (box-type screen-box))
   (resize-backing-store (allocate-backing-store
        *mouse-resize-corner-bitmap*))
   (toggle-view-backing-store (allocate-backing-store
             *mouse-toggle-view-bitmap*))
   (name-stub-backing-store (allocate-backing-store
           *mouse-name-tab-bitmap*))
   (toggle-type-backing-store (allocate-backing-store
             (if (eq box-type 'data-box)
                 *mouse-doit-toggle-bitmap*
                 *mouse-data-toggle-bitmap*))))
    (flet ((grab-back (store x y)
          (bitblt-from-screen alu-seta
            (offscreen-bitmap-width store)
            (offscreen-bitmap-height store)
            store x y 0 0))
     (stamp-icon (icon x y)
           (bitblt-to-screen alu-seta
           (offscreen-bitmap-width icon)
           (offscreen-bitmap-height icon)
           icon 0 0 x y)))
      (multiple-value-bind (box-window-x box-window-y)
        (xy-position screen-box)
      (multiple-value-bind (left top right bottom)
    (box-borders-widths box-type screen-box)
  (declare (ignore top))
  (multiple-value-bind (delta-x delta-y)
      (box-borders-offsets box-type screen-box)
    (let ((resize-x (-& (+& box-window-x (screen-obj-wid screen-box))
            right))
    (resize-y (-& (+& box-window-y (screen-obj-hei screen-box))
            bottom))
    (toggle-view-x (+& box-window-x delta-x))
    (toggle-view-y (+& box-window-y delta-y
           (box-borders-cached-name-tab-height
            (box-type screen-box) screen-box)))
    (toggle-type-x (+& box-window-x left))
    (toggle-type-y (-& (+& box-window-y (screen-obj-hei screen-box))
           bottom)))
      ;; first grab underlying areas
      (grab-back resize-backing-store resize-x resize-y)
      (grab-back toggle-view-backing-store toggle-view-x toggle-view-y)
      (when (fast-memq box-type '(data-box 'doit-box))
        (grab-back toggle-type-backing-store toggle-type-x toggle-type-y))
      (with-multiple-execution
    (dotimes (i *number-of-christmas-blinks*)
      (stamp-icon (let ((z (mod i 3)))
        (cond ((=& z 0) *mouse-resize-corner-bitmap*)
              ((=& z 1) *mouse-expand-corner-bitmap*)
              (t *mouse-shrink-corner-bitmap*)))
            resize-x resize-y)
      (stamp-icon *mouse-toggle-view-bitmap* toggle-view-x toggle-view-y)
      (when (fast-memq box-type '(data-box 'doit-box))
        (stamp-icon (if (eq box-type 'data-box)
            *mouse-doit-toggle-bitmap*
            *mouse-data-toggle-bitmap*)
        toggle-type-x toggle-type-y))
                  (force-graphics-output)
      (sleep *christmas-half-time*)
      (stamp-icon resize-backing-store resize-x resize-y)
      (stamp-icon toggle-view-backing-store toggle-view-x toggle-view-y)
      (when (fast-memq box-type '(data-box 'doit-box))
        (stamp-icon toggle-type-backing-store
        toggle-type-x toggle-type-y))
                  (force-graphics-output)
      (sleep *christmas-half-time*))))))))
    ;; deallocate the backing stores
    (deallocate-backing-store *mouse-resize-corner-bitmap*
            resize-backing-store)
    (deallocate-backing-store *mouse-toggle-view-bitmap*
            toggle-view-backing-store)
    (deallocate-backing-store *mouse-name-tab-bitmap*
            name-stub-backing-store)
    (deallocate-backing-store (if (eq box-type 'data-box)
          *mouse-doit-toggle-bitmap*
          *mouse-data-toggle-bitmap*)
            toggle-type-backing-store)))
  boxer-eval::*novalue*)
|#

;;;;
;;;; FILE: comsb.lisp
;;;;

;(defboxer-COMMAND COM-UNBOXIFY ()
;  "unboxes the point-box"
;  (reset-editor-numeric-arg)
; (with-multiple-execution
;  (unless (eq (point-box) *initial-box*)
;    (let* ((bp-1 (make-bp :moving))
;	   (bp-2 (make-bp :moving))
;	   (doomed-region (make-editor-region bp-1  bp-2)))
;
;    ;;; bracket the box
;      (move-bp bp-1
;	       (BOX-FIRST-BP-VALUES (superior-box (bp-row *point*))))
;      (move-bp bp-2
;	       (BOX-LAST-BP-VALUES (superior-box (bp-row *point*))))
;      (setf (interval-box doomed-region) (point-box))
;      ;;; kill region ,exit,rubout box ,yank stuff
;      (kill-region doomed-region)
;
;      (LET ((BOX (BOX-SCREEN-POINT-IS-IN)))
;	(UNLESS (EQ BOX *INITIAL-BOX*)
;	  (EXIT BOX (SUPERIOR-SCREEN-BOX (SCREEN-BOX-POINT-IS-IN))
;		(SUPERIOR-BOX BOX) t)))
;      (add-redisplay-clue (bp-row *point*) ':insert)
;      (delete-cha-at-cha-no (point-row) (1- (point-cha-no)))
;      (yank-region *point*  doomed-region)
;      (add-redisplay-clue (bp-row *point*) ':insert)
;
;      ;;; cleanup bps, editor-arg
;      (delete-bp (bp-row bp-2) bp-2)
;      (delete-bp (bp-row bp-1) bp-1)
;      (reset-editor-numeric-arg)
;
;    )))
;  boxer-eval::*novalue*)
;
;

;;;;;;;;;;;;;;;; THIS HAS BEEN SUPERCEDED BY UNBOX-BOX
;;;;;;;;;;;;;;;; To make unbox-region and unbox-point-box things do the same thing
;;;; A cheap hack. Kill the box's contents, insert it into the above
;;;; hierarchy, and kill the box.
;(defun com-unboxify-point-box ()
;  "unboxify the point-box"
;  (with-multiple-execution
;      (unless (eq (point-box) *initial-box*)
;	(let* ((bp-1 (make-bp :moving))
;	       (bp-2 (make-bp :moving))
;	       (doomed-region (make-editor-region bp-1  bp-2)))
;	  (move-bp bp-1
;		   (BOX-FIRST-BP-VALUES (superior-box (bp-row *point*))))
;	  (move-bp bp-2
;		   (BOX-LAST-BP-VALUES (superior-box (bp-row *point*))))
;	  (setf (interval-box doomed-region) (point-box))
;	  (kill-region doomed-region)
;	  (LET ((BOX (BOX-SCREEN-POINT-IS-IN)))
;	    (UNLESS (EQ BOX *INITIAL-BOX*)
;	      (EXIT BOX (SUPERIOR-SCREEN-BOX (SCREEN-BOX-POINT-IS-IN))
;		    (SUPERIOR-BOX BOX) t)))
;	  (add-redisplay-clue (bp-row *point*) ':insert)
;	  (delete-cha-at-cha-no (point-row) (1- (point-cha-no)))
;	  (yank-region *point*  doomed-region)
;	  (add-redisplay-clue (bp-row *point*) ':insert)
;	  (delete-bp (bp-row bp-2) bp-2)
;	  (delete-bp (bp-row bp-1) bp-1)
;	  ))))

#| ;; old version
(defboxer-command com-fill-rows (&optional (region nil))
  "fill the region"
  (let* ((region-to-fill (if region region
			     (or *region-being-defined*
				 (get-current-region)))))
    (unless (null region-to-fill)
      (let ((sbp (interval-start-bp region-to-fill))
	    (ebp (interval-stop-bp region-to-fill)))
	(cond
	  ((eql (superior-box (bp-row sbp))
		(superior-box (bp-row ebp)))
	   (progn
	     (set-a-fill-margin (superior-box (bp-row ebp)))
	     (if (bp-< sbp ebp)
		 (fill-stuff sbp ebp)
		 (fill-stuff ebp sbp))))
	  (t (boxer-editor-error "Endpoints for fill must be in the same box")))
	)
      ))
  (mark-file-box-dirty (point-row))
  (reset-region)
  boxer-eval::*novalue*)

#| ;; old old version
(defboxer-command COM-FILL-BOX ()
  "fill a given boxes rows"
  ;; if there is a region, get rid of it
  (reset-region)
  (reset-editor-numeric-arg)
  (let* ((bp-1 (make-bp :moving))
	 (bp-2 (make-bp :moving))
	 (region (make-editor-region bp-1  bp-2)))
    (move-bp bp-1
	     (box-first-bp-values (superior-box (bp-row *point*))))
    (move-bp bp-2
	     (box-last-bp-values (superior-box (bp-row *point*))))
    (setf (interval-box region) (point-box))
    (com-fill-rows region)
    (delete-bp (bp-row bp-2) bp-2)
    (delete-bp (bp-row bp-1) bp-1)
    boxer-eval::*novalue*))
|#

(defboxer-command com-fill-box (&optional (box (point-box)))
  "Reformats the box's contents"
  (reset-region)
  (when (display-style-fixed-wid (display-style-list box))
    (multiple-value-bind (start-row start-cha-no)
        (box-first-bp-values box)
      (multiple-value-bind (stop-row stop-cha-no)
          (box-last-bp-values box)
        (let ((start-bp (make-bp ':fixed)) (stop-bp  (make-bp ':fixed))
              (*auto-fill-margin* (display-style-fixed-wid
                                   (display-style-list box))))
          (setf (bp-row start-bp) start-row (bp-cha-no start-bp) start-cha-no
                (bp-row stop-bp) stop-row (bp-cha-no stop-bp) stop-cha-no)
          (fill-stuff start-bp stop-bp)
          (mark-file-box-dirty (first-inferior-row box))))))
  boxer-eval::*novalue*)



;;;; various functions needed by the fill-rows routine
(defvar *auto-fill-margin* 300)
(defvar *end-row* nil)

(defun auto-margin () *auto-fill-margin*)



(defun set-a-fill-margin (box)
  (let ((sbox (car(screen-objs box))))
    (if sbox
	(setq *auto-fill-margin*
              (- (screen-obj-wid sbox) (vertical-border-width box))))))


;;;; WIDTHS OF CHARACTER/BOXES in pixels
;;;; the width of a thing (box, char)
(defun thing-wid (x)
  (cond ((null x) 0)
	((box? x) (box-wid-in-pixels x))
	(t (cha-wid x))))

;;;; loop through the box, adding up the sum of the cha widths and the borders.
;;;; need to ask ed about the width values and what is what.

(defun box-wid-in-pixels (box)
  (let ((max-wid *minimum-box-wid*))
    (do* ((r (first-inferior-row box) (next-row r))
	  ;(rwid (pix-sum r)(pix-sum r))
	  ;(dummy (if (> rwid max-wid) (setq max-wid rwid))
	  ;       (if (> rwid max-wid) (setq max-wid rwid)))
	  )
	  ((null (next-row r))))
    (if (car (screen-objs box))
	(multiple-value-bind (l-bord-wid top r-bord-wid bottom)
	    (box-borders-widths
	     (box-type (car (screen-objs box)))
	     (car (screen-objs box)))
	  (declare (ignore top bottom))
	  (+ max-wid l-bord-wid r-bord-wid))
	(+ max-wid 9 9))
    ))


;;;; what is the sum of the pix vals for the given row
(defun pix-sum (row)
    (if (eql nil (cha-at-cha-no row 0))
	0
	(do* ((index 0 (1+ index))
	      (c (cha-at-cha-no row index)
		 (cha-at-cha-no row index))
	      (pc (thing-wid c) (if c (+ pc (thing-wid c)) pc))
	      )
	     ((eql nil (cha-at-cha-no row index))
	      pc))))

(defun diagnose (row)
  (if (null row)
      nil
      (let ((ps (pix-sum row))
	    (am (auto-margin)))
	(cond
	  ((and
	    (< ps am)
	    (eql *end-row* row))
	   :too-small-leave-alone)
	  ((eql ps 0)
	   :empty-row)
	  ((< ps am)
	   :too-small)
	  ((and
	    (> ps am)
	    (eql *end-row* row))
	   :too-big-use-return)
	  ((> ps am)
	   :too-big)
	  ((eql ps am)
	   :just-right)))))


;;;; fill the specified row, return t if changes any rows, else nil
(defun fill-row (row)
      (let ((d (diagnose row)))
	(cond
	  ((null d)  (progn
			(setq *end-row* nil)
			nil))
	  ((eql d :empty-row)
	       	   (progn (erase-row row) t))
	   ((or (eql d :just-right)
		(eql d :too-small-leave-alone))
	    nil)

	  ((eql d :too-small)
	   (progn
	     (if (and (not (eql (next-row row) *end-row*))
		      (blank-row(next-row row)))
		 (erase-row (next-row row)))
	     (add-space-if-necessary row)
	     (let ((numup (where-to-break
			   (next-row row)
			   (bounded-length
			    (next-row row)
			    (- (auto-margin) (pix-sum row))))))
	       (if (numberp numup)
		   (progn
		     (pull-up-chas row numup)
		     (fill-row row)
		     t)
		   nil
		   ))))
	  ((eql d :too-big)
	   (let ((numstaying (where-to-break
			      row
			      (bounded-length row (auto-margin)))))
	     (add-space-if-necessary row)
	     (if (numberp numstaying)
		 (push-down-chas
		  row
		  (- (length-in-chas row)
		     numstaying)
		  ))
	     t))
	  ((eql d :too-big-use-return)
	   (let ((numstaying (where-to-break
			      row
			      (bounded-length row (auto-margin)))))
	     (add-space-if-necessary row)
	     (if (numberp numstaying)
		 (progn
		   (split-rows
		    row
		    numstaying)
		   (setq *end-row* (next-row row))
		   t)
		 nil)
	     )
	   )))
      )





;;;; manipulators
;;;; push the number of chas from row down to the next row
(defun push-down-chas (row num)
  (if (< (length-in-chas row) num) (break "push-down-chas-called with num too big"))
  (if (> num  0)
      (insert-row-chas-at-cha-no (next-row row)
				 (delete-chas-between-cha-nos
				  row
				  (- (length-in-chas row) num)
				  (length-in-chas row))
				 0)
      ))



;;;; pull up chas from the row's next row.
;;;;
(defun pull-up-chas (row num)
  (if (>= (length-in-chas(next-row row)) num)
      (if (> num 0)
	  (insert-row-chas-at-cha-no row
				     (delete-chas-between-cha-nos (next-row row)
								  0
								  num)
				     (length-in-chas row))
	  )
      (break "num too big"))
  )


(defun erase-row (row)
  (delete-row-at-row-no
   (superior-box row)
   (row-row-no (superior-box row) row)
   ))




;;;; add a space on the end of a row
(defun add-space-if-necessary (row)
  (if (or (eql (length-in-chas row) 0)
	  (and
	   (not (eql (cha-at-cha-no row (1- (length-in-chas row)))
		     #\Space))
	   (not (eql (cha-at-cha-no row (1- (length-in-chas row)))
		     #\-))))
      (progn
	(add-redisplay-clue row ':insert)
	(chas-array-insert-cha
	 (chas-array row)
	 (length-in-chas row)
	 #\space))))










;;;; breaking rows apart. where to do it

;;;; used to get a "correct" break at a space or hyphen
;;;; cha-num will be the upperbound of the break
;;;; return the length of the cleanly broken row, not
;;;; longer than cha-num
;;;; returns empty-row if there is no break below cha-num

(defun where-to-break (row cha-num)
  (cond ((= cha-num 0) :empty-row)
	((< (length-in-chas row) (1+ cha-num))
	 (length-in-chas row))
	(t (do* ((index (1- cha-num) (1- index))
		 (cha (cha-at-cha-no row index)(cha-at-cha-no row index))
		 )
		((or (box? cha)
		     (eql cha #\ )
		     (eql cha #\-)
		     (eql cha nil)
		     (eql index 0))
		 (cond
		   ((null cha) (progn  (print "special") cha-num))
		   ((box? cha) (1+ index))
		   ((> index 0) (1+ index))
		   ( t :empty-row)))))))

;;;; what is the greatest upper bound of the length of the row
;;;; that has a pixel count less than pcount
;;;; i.e. (bounded-length (row infinity)) is the length of the row.
;;;; (bounded-length (row 7)) should return 1 if the row's first cha is a
;;;; character


(defun bounded-length (row pcount)
  (if (null row)
      0
      (do* ((index 0 (1+ index))
	    (c (cha-at-cha-no row index)
	       (cha-at-cha-no row index))
	    (pc (thing-wid c)(+ pc (thing-wid c))))
	   ((or (eql nil (cha-at-cha-no row index))
		(> pc pcount)) index)
	)
      ))





;;;; split the row into two, with n-left-on chars in the top row
(defun split-rows (row n-left-on)
  (let ((drow (kill-chas-at-cha-no row n-left-on)))
    (insert-row-at-row-no
     (superior-box row)
     drow
     (1+ (row-row-no
	  (superior-box row)
	  row))
     )))


(defun blank-row (row)
  (cond ((null row) nil)
	(t (eql (cha-at-cha-no row 0) nil))))


;;;; do the filling, with the given rows as the borders

(defun fill-rows (start end)
  (setq *end-row* end)
  (fill-all-but-last-rows start)
  (fill-last-row))

(defun fill-all-but-last-rows (row)
  (if (or (null row) (eql row *end-row*))
      nil
      (progn
	(fill-row row)
	(fill-all-but-last-rows (next-row row)))))


(defun fill-last-row ()
  (if (blank-row *end-row*)
      (erase-row *end-row*)
      (if (fill-row *end-row*) (fill-last-row))))




;;; stuff to strip out spaces in a region

;;; kill spaces between two bp's args: start stop
(defun space-killer-iterator (bp1 bp2)
  (do ((row (bp-row bp1) (next-row row)))
      ((eql row (bp-row bp2))  (space-killer row))
    (space-killer row))
  )

(defun SPACE-KILLER (row)
  "kill xtra spaces on the given row"
  (let ((space-mode t))
    (do* ((index 0 (1+ index))
          (cha (cha-at-cha-no row index) (cha-at-cha-no row index)))
         ((eql index (length-in-chas row)))
      (cond ((eql cha #\Space)
             (if space-mode
               (progn
                 (delete-cha-at-cha-no row index)
                 (setq index (1- index)))
               (setq space-mode t)))
            (t (setq space-mode nil))))))

(defun fill-stuff (bp1 bp2)
  (let ((srow (bp-row bp1)) (erow (bp-row bp2)))
    (space-killer-iterator bp1 bp2)
    (fill-rows srow erow)))

; (defboxer-command com-fill-rows (&optional
;				  (region
;				   (or
;				    *region-being-defined*
;				    (get-current-region))
;				   ))
;   "fill the region"
;   (reset-editor-numeric-arg)
;   (unless (null region)
;     (let ((sbp (interval-start-bp region))
;	   (ebp (interval-stop-bp region)))
;       (cond
;	 ((eql (superior-box (bp-row sbp))
;	       (superior-box (bp-row ebp)))
;	  (progn
;	    (set-a-fill-margin (superior-box (bp-row ebp)))
;	    (if (bp-< sbp ebp)
;		(fill-stuff sbp ebp)
;		(fill-stuff ebp sbp))))
;	 (t
;	  (boxer-editor-error "Endpoints for fill must be in the same box")))
;       )
;     )
;   (reset-region)
;   boxer-eval::*novalue*)
;

|#


;;;;
;;;; FILE: comsf.lisp
;;;;

;; from set-font-style
  (cond ((or (null style) (eq style :plain))
         (%set-font-style boxer-font 0))
    (t (let* ((current-style (font-style boxer-font))
              (new-style (case style
                           (:bold (dpb& (if to-on? 1 0)
                                        '#.(byte 1 0) current-style))
                           (:italic (dpb& (if to-on? 1 0)
                                          '#.(byte 1 1) current-style))
                           (:underline (dpb& (if to-on? 1 0)
                                             '#.(byte 1 2) current-style)))))
         (%set-font-style boxer-font new-style))))


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

;; like typep for font styles...
(defun font-stylep (boxer-font style)
  (cond ((or (null style) (eq style :plain))
         (zerop& (font-style boxer-font)))
    (t
     (not (zerop& (logand& (font-style boxer-font)
                           (case style
                             (:bold 1) (:italic 2) (:underline 4))))))))

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
;;;; FILE: dataprims.lisp
;;;;

(boxer-eval::defboxer-primitive bu::redirect ((boxer-eval::dont-copy port) (bu::port-to target))
  "retarget the port to the given box"
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::redirect " is no longer available"))
        (t
         (retarget-internal port target)
         boxer-eval::*novalue*)))

;;;;
;;;; FILE: disdcl.lisp
;;;;

(DEFVAR %DRAWING-FONT-MAP NIL
        "Inside of a drawing-on-window, this variable is bound to %drawing-window's
   font-map.")

;;;;
;;;; FILE: disply.lisp
;;;;

#|

(defun screen-object-new-width (screen-object)
  (when screen-object
    (if (screen-cha? screen-object)
      (cha-wid screen-object)
      (screen-obj-new-wid screen-object))))

(defun screen-object-new-height (screen-object)
  (when screen-object
    (if (screen-cha? screen-object)
      (cha-hei)
      (screen-obj-new-hei screen-object))))
|#

;; These don't seem to be used in the new OpenGL redisplay...
#|
(defun move-screen-rows (sv from-row-no delta-x delta-y &optional to-row-no no-draw?)
  (unless (>=& from-row-no (storage-vector-active-length  sv))
    (multiple-value-bind (x-offset y-offset)
                         (screen-obj-offsets (sv-nth from-row-no sv))
                         (let ((wid 0) (hei 0))
                           (do-vector-contents (screen-row sv :start from-row-no :stop to-row-no)
                             (setq wid (max wid (screen-obj-wid screen-row))
                                   hei (+  hei (screen-obj-hei screen-row)))
                             (incf (screen-obj-x-offset screen-row) delta-x)
                             (incf (screen-obj-y-offset screen-row) delta-y))
                           ;; finally do the actual moving
                           (when (null no-draw?)
                             (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))))))

(defun move-screen-chas (sv from-cha-no delta-x delta-y &optional to-cha-no)
  (multiple-value-bind (x-offset y-offset)
                       (screen-obj-offsets (sv-nth from-cha-no sv))
                       (let ((wid 0) (hei 0))
                         (do-screen-chas-with-font-info (screen-cha sv
                                                                    :start from-cha-no
                                                                    :stop to-cha-no)
                           (setq wid (+ wid (screen-object-width screen-cha))
                                 hei (max  hei (screen-object-height screen-cha)))
                           (unless (screen-cha? screen-cha)
                             (incf (screen-obj-x-offset screen-cha) delta-x)
                             (incf (screen-obj-y-offset screen-cha) delta-y)))
                         ;; finally do the actual moving
                         (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))))

;;; this is used in pass-2 to blit inferiors over to make room for character
;;; insertions, we can't use move-screen-chas because the chas we want to move
;;; no longer correspond to the cha-nos because the screen structure has been
;;; patched up in pass-1.  We iterate ONLY to look for screen-boxes so that
;;; their offsets can be updated
(defun slide-screen-chas (sv from-cha-no delta-cha-no
                             wid hei x-offset y-offset delta-x delta-y
                             &optional no-drawing fds)
  (unless (>=& (+& from-cha-no delta-cha-no)
               (storage-vector-active-length sv))
    (cond ((and (null wid) (not no-drawing))
           ;; we need to calculate the width from the chas we are about
           ;; to blit as well as adjust the offsets of any screen-boxes
           ;; that happen to be in the row
           (setq wid 0)
           (do-screen-chas-with-font-info (screen-cha
                                           sv
                                           :start (+& from-cha-no
                                                      delta-cha-no)
                                           :font-descriptors fds)
             (incf& wid (screen-object-width screen-cha))
             (unless (screen-cha? screen-cha)
               (incf& (screen-obj-x-offset screen-cha) delta-x))))
      (t
       ;; just update any offsets of boxes
       (do-vector-contents (screen-cha sv :start (+& from-cha-no
                                                     delta-cha-no))
         (unless (screen-cha? screen-cha)
           (incf& (screen-obj-x-offset screen-cha) delta-x)))))
    (unless no-drawing
      (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))))

(defun move-screen-obj (screen-obj delta-x delta-y &optional no-drawing)
  (when (not-null screen-obj)
    ;; sgithens TODO (check-screen-obj-arg screen-obj)
    (multiple-value-bind (wid hei)
                         (screen-obj-size screen-obj)
                         (multiple-value-bind (x-offset y-offset)
                                              (screen-obj-offsets screen-obj)
                                              (unless no-drawing
                                                (bitblt-move-region wid hei x-offset y-offset delta-x delta-y))
                                              (incf (screen-obj-x-offset screen-obj) delta-x)
                                              (incf (screen-obj-y-offset screen-obj) delta-y)))))

(DEFUN MOVE-GRAPHICS-SHEET (GRAPHICS-SCREEN-SHEET DELTA-X DELTA-Y)
       (WHEN (NOT-NULL GRAPHICS-SCREEN-SHEET)
             (CHECK-GRAPHICS-SCREEN-SHEET-ARG GRAPHICS-SCREEN-SHEET)
             (LET* ((GRAPHICS-SHEET (GRAPHICS-SCREEN-SHEET-ACTUAL-OBJ
                                     GRAPHICS-SCREEN-SHEET))
                    (WID (GRAPHICS-SHEET-DRAW-WID GRAPHICS-SHEET))
                    (HEI (GRAPHICS-SHEET-DRAW-HEI GRAPHICS-SHEET))
                    (X-OFFSET (GRAPHICS-SCREEN-SHEET-X-OFFSET GRAPHICS-SCREEN-SHEET))
                    (Y-OFFSET (GRAPHICS-SCREEN-SHEET-Y-OFFSET GRAPHICS-SCREEN-SHEET)))
                   (BITBLT-MOVE-REGION WID HEI X-OFFSET Y-OFFSET DELTA-X DELTA-Y)
                   (INCF (GRAPHICS-SCREEN-SHEET-X-OFFSET GRAPHICS-SCREEN-SHEET) DELTA-X)
                   (INCF (GRAPHICS-SCREEN-SHEET-Y-OFFSET GRAPHICS-SCREEN-SHEET) DELTA-Y))))

(defun move-inferior-screen-objs (inferiors delta-x delta-y)
  (cond ((null inferiors))
    ((graphics-screen-sheet? inferiors)
     (move-graphics-sheet inferiors delta-x delta-y))
    ((screen-row? (sv-nth 0 inferiors))
     (move-screen-rows inferiors 0 delta-x delta-y))
    ((or (screen-cha? (sv-nth 0 inferiors))
         (screen-box? (sv-nth 0 inferiors)))
     (move-screen-chas inferiors 0 delta-x delta-y))
    ((screen-obj? inferiors)
     (move-screen-obj inferiors delta-x delta-y))
    (t
     (barf "Don't know how to move inferior screen object(s), ~S"
           inferiors))))

(DEFUN GRAY-SIZE-AND-OFFSETS (SCREEN-BOX)
       (let ((box-type (slot-value screen-box 'box-type)))
         (MULTIPLE-VALUE-BIND (OUTER-WID OUTER-HEI)
                              (box-borders-minimum-size box-type screen-box)
                              (MULTIPLE-VALUE-BIND (IL IT IR IB)
                                                   (box-borders-widths box-type screen-box)
                                                   (VALUES (- OUTER-WID IL IR) (- OUTER-HEI IT IB) IL IT)))))

(DEFUN MOVE-GRAY-REGION (SCREEN-BOX DELTA-X DELTA-Y)
       (MULTIPLE-VALUE-BIND (GRAY-WID GRAY-HEI GRAY-X GRAY-Y)
                            (GRAY-SIZE-AND-OFFSETS SCREEN-BOX)
                            (BITBLT-MOVE-REGION GRAY-WID GRAY-HEI GRAY-X GRAY-Y DELTA-X DELTA-Y)))

|#

;; #-lispworks6
;; (DEFUN SET-OUTERMOST-SCREEN-BOX (NEW-OUTERMOST-SCREEN-BOX
;; 				 &OPTIONAL (WINDOW *BOXER-PANE*))
;;   (WITHOUT-INTERRUPTS		      ;keep the mouse process from looking at
;;     (REDISPLAYING-WINDOW (WINDOW)     ;the screen when it is in a munged state
;;       (UNLESS (EQ NEW-OUTERMOST-SCREEN-BOX *OUTERMOST-SCREEN-BOX*)
;; 	(DECONFIGURE-SCREEN-BOX-TO-BE-OUTERMOST-BOX *OUTERMOST-SCREEN-BOX*
;; 						    WINDOW)
;; 	(CONFIGURE-SCREEN-BOX-TO-BE-OUTERMOST-BOX NEW-OUTERMOST-SCREEN-BOX
;; 						  WINDOW)
;; 	(ERASE-SCREEN-OBJ *OUTERMOST-SCREEN-BOX*)
;; 	(SETQ *OUTERMOST-SCREEN-BOX* NEW-OUTERMOST-SCREEN-BOX)))
;;     (SETQ *OUTERMOST-SCREEN-BOX* (OUTERMOST-SCREEN-BOX)) ; why ??
;;     (LET ((*COMPLETE-REDISPLAY-IN-PROGRESS?* T)
;; 	  (OLD-SCREEN-ROW (UNLESS (NULL NEW-OUTERMOST-SCREEN-BOX)
;; 			    (SCREEN-ROW NEW-OUTERMOST-SCREEN-BOX))))
;;       (WHEN (SCREEN-ROW? OLD-SCREEN-ROW)
;; 	;; we need to break up the screen-structure
;; 	(KILL-SCREEN-CHAS-FROM OLD-SCREEN-ROW 0)
;; 	(if (fast-memq (superior old-screen-row) *outermost-screen-box-stack*)
;; 	    (deallocate-inferiors (superior old-screen-row))
;; 	    (deallocate-self (superior old-screen-row))))
;;       (repaint-window window))))


#|

(defmethod xy-position ((self screen-obj))
  (multiple-value-bind (superior-x-off superior-y-off)
                       (cond ((outermost-screen-box? self)
                              (values 0 0))
                         (t
                          (xy-position (superior self))))
                       (values (+ superior-x-off (screen-obj-x-offset self))
                               (+ superior-y-off (screen-obj-y-offset self)))))
|#

;;; The OLD cursor tracker

#|
(defun bp-positions (bp)
  (check-bp-arg bp)
  (let ((box (bp-box bp))
        (row (bp-row bp))
        (screen-box (bp-screen-box bp)))
    (COND ((NULL BOX) NIL)
          ((name-row? row)
           (screen-box-name-row-bp-position screen-box row))
          ((eq ':shrunk (display-style screen-box))
           (screen-box-first-bp-position screen-box))
          ((NULL (CURRENT-SCREEN-ROW ROW))
           (SCREEN-BOX-LAST-BP-POSITION SCREEN-BOX))
          (t
           (row-point-position (current-screen-row row) screen-box)))))

(defun screen-box-first-bp-position (screen-box)
  (multiple-value-bind (x y)
                       (xy-position screen-box)
                       (multiple-value-bind (il it)
                                            (box-borders-widths (slot-value screen-box 'box-type) screen-box)
                                            (cons (+ x il) (+ y it)))))

(DEFUN SCREEN-BOX-LAST-BP-POSITION (SCREEN-BOX)
       (MULTIPLE-VALUE-BIND (X Y)
                            (XY-POSITION SCREEN-BOX)
                            (CONS (+ X (SCREEN-OBJ-WID SCREEN-BOX))
                                  (- (+ Y (SCREEN-OBJ-HEI SCREEN-BOX)) *MINIMUM-CURSOR-HEIGHT*))))

(defun screen-box-name-row-bp-position  (screen-box name-row)
  (declare (ignore name-row))
  (let ((cha-no (bp-cha-no *point*)))
    (multiple-value-bind (x y)
                         (xy-position screen-box)
                         (multiple-value-bind (tab-x tab-y)
                                              (box-borders-tab-position (slot-value screen-box 'box-type)
                                                                        screen-box x y cha-no)
                                              (cons tab-x tab-y)))))

(DEFUN ROW-POINT-POSITION (SCREEN-ROW &optional screen-box)
       (LET* ((ROW (SCREEN-OBJ-ACTUAL-OBJ SCREEN-ROW))
              (LENGTH-IN-CHAS (LENGTH-IN-CHAS ROW))
              (CHA-NO (BP-CHA-NO *POINT*)))
             (COND ((NULL (BP-SCREEN-BOX *POINT*))
                    (BARF NIL "Lost the current Screen Box"))
                   ((>= CHA-NO LENGTH-IN-CHAS)
                    (END-OF-ROW-POINT-LOCATION SCREEN-ROW))
                   (T (INSIDE-OF-ROW-POINT-LOCATION SCREEN-ROW CHA-NO)))))

(DEFUN END-OF-ROW-POINT-LOCATION (SCREEN-ROW)
       (MULTIPLE-VALUE-BIND (SCREEN-ROW-X SCREEN-ROW-Y)
                            (XY-POSITION SCREEN-ROW)
                            (CONS (+ SCREEN-ROW-X (SCREEN-OBJ-WID SCREEN-ROW)) SCREEN-ROW-Y)))

(DEFUN INSIDE-OF-ROW-POINT-LOCATION (SCREEN-ROW CHA-NO)
       (MULTIPLE-VALUE-BIND (SCREEN-ROW-X SCREEN-ROW-Y)
                            (XY-POSITION SCREEN-ROW)
                            (CONS (+ SCREEN-ROW-X
                                     (X-COORDINATE-OF-CHA-NO SCREEN-ROW CHA-NO)) SCREEN-ROW-Y)))

(DEFUN X-COORDINATE-OF-CHA-NO (ROW CHA-NO &AUX(X-COORD 0))
       (DO* ((INDEX 0 (+ INDEX 1))
             (CHA (SCREEN-CHA-AT-CHA-NO ROW INDEX)
                  (SCREEN-CHA-AT-CHA-NO ROW INDEX)))
            ((OR (NULL CHA)(= INDEX CHA-NO)) X-COORD)
            (SETQ X-COORD (+ X-COORD (SCREEN-OBJECT-WIDTH CHA)))))
|#

;;;;
;;;; FILE: draw-high-common.lisp
;;;;

;;; WITH-FONT-MAP-BOUND is meant to be used by all those functions
;;; (like BOX-BORDER-FN's that have to be called in an environment where the
;;; font map is supposed to be bound but nothing else (like all those
;;; wonderful drawing type things and stuff) needs to be bound

(defmacro with-font-map-bound ((window) &body body)
  `(let ((%drawing-font-map (sheet-font-map ,window)))
     %drawing-font-map				;bound but never used etc.
     . ,body))

;;;;
;;;; FILE: draw-high-hardware-clip.lisp
;;;;

(defmacro clip-x (scaled-x) scaled-x)
(defmacro clip-y (scaled-y) scaled-y)


(defun bitblt-within-screen (alu full-wid full-hei from-x from-y to-x to-y)
  (let (;; hardware clipping is only performed on the destination
        ;; rect, so we have to make sure we don't pull in any
        ;; pixels from outside the clipping region from the source rect
        (wid (min& full-wid (-& %local-clip-rig from-x)))
        (hei (min& full-hei (-& %local-clip-bot from-y))))
    (%bitblt-in-screen alu wid hei
           %drawing-array from-x from-y to-x   to-y)))


(defun bitblt-move-region (full-wid full-hei from-x from-y delta-x delta-y)
  (let (;; hardware clipping is only performed on the destination
        ;; rect, so we have to make sure we don't pull in any
        ;; pixels from outside the clipping region from the source rect
        (wid (min& full-wid (-& %clip-rig from-x)))
        (hei (min& full-hei (-& %clip-bot from-y))))
    (unless (or (zerop full-wid) (zerop full-hei))
    (%bitblt-in-screen alu-seta wid hei
           %drawing-array from-x from-y
           (+& from-x delta-x) (+& from-y delta-y))
    ;; Now we erase the part of the screen which is no longer covered.
    (unless (zerop delta-x)
      (erase-rectangle (abs delta-x) hei
           (cond ((plusp delta-x) from-x)
           ((>& (abs delta-x) wid) from-x)
                             #+lwwin
                             ;;If the region we're moving is partly
           ;;not displayed due to clipping we have to
           ;;clear out stuff specially.  This has a
           ;;few bugs but it works better than with
           ;;out it.
                             ;; NOTE: this is because LW does software clipping for
                             ;; %bitblt ops
           ((>& (+& wid from-x  %origin-x-offset) %clip-rig)
            (+& %clip-rig delta-x (-& %origin-x-offset)))
           (t (+& from-x wid delta-x)))
           from-y))
    (unless (zerop delta-y)
      (erase-rectangle wid (abs delta-y)
           from-x
           (cond ((plusp delta-y) from-y)
           ((>& (abs delta-y) hei) from-y)
                             #+lwwin
                             ;; same software clipping stuff, doo dah doo dah...
                             ((>& (+& hei from-y %origin-y-offset) %clip-bot)
          (+& %clip-bot delta-y (-& %origin-y-offset)))
           (t (+& from-y hei delta-y))))))))

;;;;
;;;; FILE: draw-low-opengl.lisp
;;;;

;; stub
(defun sheet-font-map (w) (declare (ignore w)) nil)

(defun max-font-size-baseline-value ()
  (- (length *font-sizes*) 7 1))

(defvar *current-font-family-index* 0)

(defun max-font-size-baseline-value ()
  (- (length *font-sizes*) 7 1))

(defvar *current-font-family-index* 0)

;;
;;  o fonts remain a fixnum but we use a caching mechanism
;;    fields of a fontspec are used as indices into simple arrays
;;
;;  o We use this indirect mechanism rather than using LW fonts
;;    directly for several reasons
;;    1) There are lots of font dependencies in the bootstrapping
;;       process and in LWWin, fonts can't be defined until AFTER
;;       the boxer window has been created. The indirection lets
;;       us refer to a font before it is actually defined.
;;    2) We need font= to be a fast operation, using fixnums makes
;;       this so.  Using LW fonts would be a field by field comparison
;;       at best.
;;    3) At some point, perhaps for speed, we might want to move to
;;       using native Windoze fonts directly.  This makes it easy to
;;       change.
;;
;;  o LW most-positive-fixnum = (1- (expt 2 29))  [previously (1- (expt 2 23))]
;;    therefore, LW boxer fonts will have to be 28 bits wide
;;
;;  o A fontspec is a list of family name, size &rest style-attributes
;;
;; The Boxer font-no is defined as:
;;
;;   o the upper 8 bits specify a font family.  On the mac, the translation between
;;     number and font name was handled by the MacOS, for LWWin, we'll just
;;     use an array of font family names, with the number being the index into
;;     the array.
;;
;;   o the style is in the next 4.  Together, the font and the face
;;     correspond to the MCL concept of the font/face code.
;;
;;   o the low 12 bits are used as a size index.  NOT the size.
;;
;;   NOTE: we can juggle the various field sizes as needed.  In particular,
;;         the face field can go down to 3 bits to support BOLD, ITALICS and
;;         UNDERLINE.  The size field needs only be able to cover the allowed
;;         (by the interface) sizes.
;;
;;  The Structure of the Font Cache
;;
;;   o an array of size %%font-family-size(8)
;;     each element of the array will point to a size array
;;
;;   o each size array will be = to the number of allowed (by the interface)
;;     sizes for each font. For now, we'll specify the defined sizes as:
;;     6,8,10,12,14,16,18,24 implying a size array with length 8
;;
;;   o each entry in the size array points to an array of specific face styles
;;     for each font, for now, it will be length 4 to support all the possible
;;     combinations of BOLD, ITALICS
;;
;;   o this leaves an extra 4 more bits, either for more sizes or families
;;
;;   Boxer Font Number
;;   +--------------+---------------+-------------+
;;   |Family index 8|  size index 12|style index 4|
;;   +------+-------+------+--------+-----+-------+
;;          |              |             |
;;          +---+          +--+          +--+
;;   Font Cache |    +-+      |             |
;;              |   0| +      |             |
;;              |    | |      |             |
;;              +-> i|++------+-> +-+       | styles
;;                   : : size |   | |       | cache
;;                   : : cache+-> |++-------+->+-+
;;                                : :       |  | |
;;                                : :       +> |++---> LW font
;;                                             : :
;;

;; Low Level Font Utilities
(defconstant %%font-family-bit-pos 16)
(defconstant %%font-family-bit-length 8)
(defconstant %%font-size-bit-pos 4)
(defconstant %%font-size-bit-length 12)
(defconstant %%font-face-bit-pos 0)
(defconstant %%font-face-bit-length 4)

(defconstant %%font-family-size (expt 2 %%font-family-bit-length))

;; doesn't fill the reserved 8-bit sized field, but keep them
;; only as big as wee need
;(defconstant %%font-size-cache-length 12)
(defconstant %%font-face-cache-length 4)

(defun %make-font-size-cache ()
  (make-array (length *font-sizes*) :initial-element nil))
; (make-array ;%%font-size-cache-length :initial-element nil))

(defun %make-font-face-cache ()
  (make-array %%font-face-cache-length :initial-element nil))

;; field extractors
(defun %font-family-idx (font-no)
  (ldb& (byte %%font-family-bit-length %%font-family-bit-pos) font-no))

(defun %font-face-idx (font-no)
  (ldb& (byte %%font-face-bit-length   %%font-face-bit-pos)   font-no))

(defun font-values (font-no)
  (declare (values family-idx size-idx face-idx))
  (values (%font-family-idx font-no)
          (%font-size-idx   font-no)
          (%font-face-idx   font-no)))

;; search the font cache for an existing match
;; returns fixnum index or NIL
(defun font-family-name-pos (string)
  (dotimes (i *current-font-family-index*)
    (let ((fam-name (svref& *font-family-names* i)))
      (when (string-equal string fam-name) ; case insensitive
        (return i)))))

(defun new-font-family (family-name)
  (setf (svref& *font-family-names* *current-font-family-index*) family-name
        (svref& *font-cache* *current-font-family-index*) (%make-font-size-cache))
  (prog1 *current-font-family-index* (incf& *current-font-family-index*)))

(defun %font-name-to-idx (string &optional create?)
  (let ((existing (font-family-name-pos string)))
    (if create? (or existing (new-font-family string)) existing)))

;; BOLD, ITALIC are documented, looks like UNDERLINE is
;; supported but not documented
(defun %font-face-to-idx (style-list)
  (let ((idx 0))
    (dolist (style style-list)
      (case style
        (:bold      (setq idx (logior #b1   idx)))
        (:italic    (setq idx (logior #b10  idx)))
        ;(:underline (setq idx (logior #b100 idx)))
        ))
    idx))

(defun cache-font (font font-no &optional (translate-size t))
  (multiple-value-bind (fam size face)
                       (font-values font-no)
                       (let ((size-cache (svref& *font-cache* fam)))
                         (when (null size-cache)
                           (setq size-cache (%make-font-size-cache))
                           (setf (svref& *font-cache* fam) size-cache))
                         (let ((face-cache (svref& size-cache (if translate-size
                                                                (font-size-to-idx size)
                                                                size))))
                           (when (null face-cache)
                             (setq face-cache (%make-font-face-cache))
                             (setf (svref& size-cache size) face-cache))
                           (setf (svref& face-cache face) font)))))

(defun fontspec->font-no (fontspec &optional create?)
  (let ((fambits (%font-name-to-idx (or (car fontspec) *default-font-family*)
                                    create?)))
    (unless (null fambits)
      (dpb& fambits
            (byte %%font-family-bit-length %%font-family-bit-pos)
            (dpb& (%font-size-to-idx (or (cadr fontspec) *default-font-size*))
                  (byte %%font-size-bit-length   %%font-size-bit-pos)
                  (%font-face-to-idx (or (cddr fontspec) *default-font-face*)))))))


(defun bfd-font-size-name (size-idx) (svref *bfd-font-size-names* (1- size-idx)))

(defun get-size-menu-names ()
  (svref *font-size-menu-names* *font-size-baseline*))

(defun set-font-family-alias (family-name local-name)
  (let ((existing (font-family-alias family-name)))
    (cond ((null existing)
           (push (cons family-name local-name) *font-family-aliases*))
      ((string= local-name (cadr existing)))
      (t (if *boxer-system-hacker*
           (error "Trying to CHANGE an alias for ~S from ~S to ~S"
                  family-name (cadr existing) local-name)
           (warn "Trying to CHANGE an alias for ~S from ~S to ~S"
                 family-name (cadr existing) local-name))))))

(defun fontspec->font-values (fontspec)
  (let ((fambits (%font-name-to-idx (or (car fontspec) *default-font-family*))))
    (unless (null fambits)
      (values fambits
              (%font-size-to-idx (cadr fontspec))
              (%font-face-to-idx (or (cddr fontspec) *default-font-face*))))))

(defvar *font-family-names* (make-array %%font-family-size :initial-element nil))

;; these next 3 take a font code and should return a new font code
(defun %set-font (boxer-font new-fname)
  (let ((new-idx (%font-name-to-idx new-fname)))
    (dpb new-idx (byte %%font-family-bit-length %%font-family-bit-pos) boxer-font))
    )

(defun %set-font-size (boxer-font new-size)
  (dpb new-size (byte %%font-size-bit-length   %%font-size-bit-pos) boxer-font))

(defun %set-font-style (boxer-font new-style-byte)
  (dpb new-style-byte (byte %%font-face-bit-length   %%font-face-bit-pos)
       boxer-font))

;; used for bootstrapping, normally we go through make-boxer-font
(defun %make-font-number-internal (fam-idx size &rest styles)
  (dpb fam-idx (byte %%font-family-bit-length %%font-family-bit-pos)
       (%set-font-size (%font-face-to-idx styles) size)))

(defun %%make-font-number-internal (fam-idx size-idx &rest styles)
  (dpb fam-idx (byte %%font-family-bit-length %%font-family-bit-pos)
       (dpb size-idx (byte %%font-size-bit-length   %%font-size-bit-pos)
            (%font-face-to-idx styles))))

(defun make-boxer-font (rawfontspec)
  (let* ((alias (font-family-alias (car rawfontspec)))
         (fontspec (if alias (list* alias (cdr rawfontspec)) rawfontspec))
         (font-no (fontspec->font-no fontspec)))
    (cond ((null font-no)
           (let ((oglfont (bw::%make-opengl-font :font-triple fontspec))
                 (new-font-no (fontspec->font-no fontspec T)))
             (or (find-cached-font new-font-no nil)
                 (cache-font oglfont new-font-no nil))
             new-font-no))
      (t
        (or (find-cached-font font-no nil)
           (cache-font (bw::%make-opengl-font :font-triple fontspec) font-no nil)
            )
       font-no))))

;; there are 2 notions of size.
;;
;; the absolute font size is used internally in the font-cache structures,
;;
;; the font size, as it appears inside of Boxer-Font-Descriptors is a relative number
;; from 1 - 7, with 3 being interpreted as "Normal" (leaving 2 smaller and 4 larger sizes)
;;
(defvar *bfd-font-size-names*
  (vector "Smallest" "Smaller" "Normal" "Larger" "Larger2" "Larger3" "Largest"))

;; these need to be in sync (used to initialize the font size menu)
;; see also font-size-menu-item-name
(defvar *bfd-font-size-values* '(1 2 3 4 5 6 7))

(defun make-size-menu-names ()
  (let* ((sl (length *font-sizes*))
         (n-sizes (- sl 7)) ;; number of possible size settings.  Changed to 7 for boxer-bugs-39
         (names-array (make-array n-sizes))
         (menu-length (length *bfd-font-size-names*)))
    (dotimes (i n-sizes)
      (let ((names (make-list menu-length)))
        (setf (svref names-array i) names)
        (dotimes (j menu-length)
          (let ((size-name (svref *bfd-font-size-names* j)))
            (setf (nth j names)
                  (format nil "~A (~D)" size-name (svref *font-sizes* (+ i j))))))))
    names-array))

(defvar *font-size-menu-names* (make-size-menu-names))

(defun font-size-menu-item-name (data)
  (nth (1- data) (svref *font-size-menu-names* *font-size-baseline*)))

(defun find-cached-font (font-no &optional (translate-size t))
  (multiple-value-bind (fam size face)
                       (font-values font-no)
                       (let ((size-cache (svref& *font-cache* fam)))
                         (unless (null size-cache)
                           (let ((face-cache (svref& size-cache (if translate-size
                                                                  (font-size-to-idx size)
                                                                  size))))
                             (unless (null face-cache) (svref& face-cache face))))))
                             )

;; sgithens Only used in make-boxer-font
;; (defun cache-font (font font-no &optional (translate-size t))
;;   ; TODO - reimplement
;;   )

;; bypass the caching mechanism because this will get called
;; at a stage where we can't open any fonts, instead, we hand
;; make the font numbers but defer actually finding fonts to
;; fill the cache until after the boxer window has been created
(defun init-bootstrapping-font-vars ()
  (setq  *normal-font-no*           (%%make-font-number-internal 0 3)
         *box-border-label-font-no* (%%make-font-number-internal 0 1)
         *box-border-name-font-no*  (%%make-font-number-internal 0 3 :bold)
         *sprite-type-font-no*      (%%make-font-number-internal 0 3 :bold)
         *boxtop-text-font*         (%%make-font-number-internal 0 3 :bold)
         ))

;; this is here so it can be kept in synch with  init-bootstrapping-font-vars
;; note that make-boxer-font is called for side effect, it fills the relevant
;; font caches with the internal representation system font
;; Note: 4/16/2010 do not calculate all the font info for every font up font
(defun fill-bootstrapped-font-caches ()
  (dolist (font-family *font-families*)
    (dotimes (i (length *font-sizes*))
      (let ((size (svref *font-sizes* i)))
        (dolist (style '(nil (:bold) (:italic) (:bold :italic))) ; leave out :underline for now
          (make-boxer-font (list* font-family size style)))))))


;; THIS is safe to do
(eval-when (load)  (init-bootstrapping-font-vars))

(defvar *default-font-size*   3)
(defvar *default-font-face* nil)

;; END MAJOR FONT REFACTORING

;; this is a stub
;; it is used by some window systems(the mac) to insure all graphics
;; commands are sent to the VIEW arg
;;
;; we seem to access the graphics state a lot so it might be advantageous to
;; bind it within this macro

(defvar %graphics-state nil)

(defmacro current-graphics-state ()
  '(or %graphics-state (gp:get-graphics-state (or %drawing-array *boxer-pane*))))

;;; See Inside Mac V-53
#| ;; unused ?
(defun pixmap? (bm-or-pm) (typep bm-or-pm 'gp::pixmap-port))
|#

;; yuck !!!
(defun window-depth (window)
  (declare (ignore window)) (capi:screen-depth (car (capi::screens))))


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
;;;; FILE: evalmacs.lisp
;;;;

;;; POSSIBLE-EVAL-OBJECT? tells whether it is legal to look in slot 0.
(defmacro possible-eval-object? (thing)
  #+(or lucid lispworks)
  `(simple-vector-p ,thing)
  #+(or excl lispm)
  T
  #+mcl
  `(vectorp ,thing)  ; +++ I guess
  #-(or lucid lispm excl mcl lispworks)
  (warn "Check if your CLOS or PCL implementation uses vectors to make its objects~%~
         If (vectorp (make-instance <whatever>) is NIL, then~
         change the definition of ~S to simply return T" 'possible-eval-object)
  #-(or lucid lispm excl mcl lispworks)
  `(vectorp ,thing))

;;;;
;;;; FILE : gdispl.lisp
;;;;

    #-opengl
    (let ((diameter  (fixr (+ radius radius)))
          (fix-radius (fixr radius)))
      (%draw-filled-arc %drawing-array *graphics-state-current-alu*
                        (ensure-legal-window-coordinate
                        (scale-x (-& x fix-radius)))
                        (ensure-legal-window-coordinate
                        (scale-y (-& y fix-radius)))
                        diameter diameter
                        start-angle sweep-angle))

#-opengl
    (let ((diameter  (fixr (* 2 radius)))
          (fix-radius (fixr radius)))
      (%draw-arc %drawing-array *graphics-state-current-alu*
                (ensure-legal-window-coordinate (scale-x (-& x fix-radius)))
                (ensure-legal-window-coordinate (scale-y (-& y fix-radius)))
                diameter diameter
                start-angle sweep-angle))

    #-opengl
    (let ((diameter  (fixr (+ radius radius)))
          (fix-radius (fixr radius)))
      (%draw-filled-arc %drawing-array *graphics-state-current-alu*
                        (ensure-legal-window-coordinate
                        (scale-x (-& x fix-radius)))
                        (ensure-legal-window-coordinate
                        (scale-y (-& y fix-radius)))
                        diameter diameter
                        0 360))

    #-opengl
    (let ((diameter  (fixr (* 2 radius)))
          (fix-radius (fixr radius)))
      (%draw-arc %drawing-array *graphics-state-current-alu*
                (ensure-legal-window-coordinate (scale-x (-& x fix-radius)))
                (ensure-legal-window-coordinate (scale-y (-& y fix-radius)))
                diameter diameter 0 360))

          #-opengl
          (drawing-on-bitmap (new-bitmap)
                             (with-pen-color ((or (graphics-sheet-background sheet) *background-color*))
                               (draw-rectangle alu-seta new-wid new-hei 0 0)))

        #-opengl
        (drawing-on-bitmap (new-bitmap)
                           (case *boxer-graphics-box-bit-gravity*
                             (:top-right (bitblt-to-screen alu-seta
                                                           (min& old-wid new-wid)
                                                           (min& old-hei new-hei)
                                                           old-bitmap 0 0 0 0))
                             (:center (bitblt-to-screen alu-seta
                                                        (min& old-wid new-wid)
                                                        (min& old-hei new-hei)
                                                        old-bitmap
                                                        (max& 0 (round (-& old-wid new-wid) 2))
                                                        (max& 0 (round (-& old-hei new-hei) 2))
                                                        (max& 0 (round (-& new-wid old-wid) 2))
                                                        (max& 0 (round (-& new-hei old-hei) 2))))))

;;;;
;;;; FILE: file-prims.lisp
;;;;

(boxer-eval::defboxer-primitive bu::mark-for-saving ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::mark-for-saving
                                       " is no longer available, use "
                                       'bu::toggle-modified-flag " instead"))
        (t
         (when (box? boxer-eval::*lexical-variables-root*)
           (mark-file-box-dirty boxer-eval::*lexical-variables-root*))
         boxer-eval::*novalue*)))

;;;;
;;;; FILE: gcmeth.lisp
;;;;

;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version stamps the rectangle in turtle coordinates
;;; NOT array/window coordinates
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod turtle-rect ((self graphics-cursor) wid hei
                                               &optional (orientation :centered))
  (declare (ignore orientation))	; a hook for later
  (let ((alu (get-alu-from-pen
              (box-interface-value (slot-value self 'pen)))))
    (cond ((null alu))
      ((not (null %learning-shape?))
       (record-boxer-graphics-command-centered-rectangle
        (x-position self) (y-position self)
        (coerce wid 'boxer-float) (coerce hei 'boxer-float)))
      ((not (no-graphics?))
       (let ((abs-x (absolute-x-position self))
             (abs-y (absolute-y-position self)))
         (record-boxer-graphics-command-centered-rectangle
          abs-x abs-y wid hei)
         (with-graphics-screen-parameters
           (centered-rectangle abs-x abs-y wid hei)))))))

;;;;
;;;; FILE: grfdfs.lisp
;;;;

    #+gl
    (setf (graphics-sheet-draw-mode new-gs) ':window)

    #+gl
    (setf (graphics-sheet-draw-mode new-gs) ':window)


;;;;
;;;; FILE: grmeth.lisp
;;;;

;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version does NO transformations.  Instead, the coordinates
;;; are left in turtle space and the hardware performs the transformations
;;; whenever it renders the graphics box
;;;
;;; ****************   NOTE   ****************
;;;

#+gl
(defmethod move-to ((self graphics-object) x-dest y-dest
		    &optional dont-update-box)
  (let ((x-position (slot-value self 'x-position))
	(y-position (slot-value self 'y-position))
	(pen-alu (get-alu-from-pen (pen self))))
    (cond  ((not (and (numberp x-dest) (numberp y-dest)))
	    (error "one of the args, ~s or ~s, was not a number"
		   x-dest y-dest))
	   (t
	    (unless (typep x-dest 'boxer-float)
	      (setq x-dest (coerce x-dest 'boxer-float)))
	    (unless (typep y-dest 'boxer-float)
	      (setq y-dest (coerce y-dest 'boxer-float)))
	    (cond ((not (null %learning-shape?))
		   ;; don't draw while learning shape.
		   (unless (null pen-alu)
		     (record-boxer-graphics-command-line-segment
		      (box-interface-value x-position)
		      (box-interface-value y-position)
		      x-dest y-dest))
		   ;; While in learning-shape, don't update any boxes
		   (setf (box-interface-value x-position) x-dest)
		   (setf (box-interface-value y-position) y-dest))
		  ;; Have to make fence mode work some other time
		  ((and (eq %draw-mode ':fence)
			(not (point-in-array? array-x-dest array-y-dest)))
		   (error "you hit the fence"))
		  (t
		   (cond ((no-graphics?)
			  ;; this means we can't even do any wrapping
			  ;; calculations, so just set values
			  (set-xy self x-dest y-dest dont-update-box))
			 (t
			  (multiple-value-bind (abs-x-dest abs-y-dest)
			      (make-absolute self x-dest y-dest)
			    (let ((abs-x (absolute-x-position self))
				  (abs-y (absolute-y-position self)))
			      (without-interrupts
			       (when (and (null (slot-value self
							    'superior-turtle))
					  (eq %draw-mode ':wrap))
				 (setq x-dest (wrap-x-coordinate x-dest)
				       y-dest (wrap-y-coordinate y-dest)))
			       ;; this may have to change...
			       (cond ( %mouse-usurped
				      ;; don't update boxes during follow-mouse
				      (setf (box-interface-value x-position)
					    x-dest)
				      (setf (box-interface-value y-position)
					    y-dest))
				     (
				      (set-xy self x-dest y-dest
					      dont-update-box)))
			       (when (and (not (null pen-alu))
					  (not (zerop (pen-width self))))
				 (record-boxer-graphics-command-line-segment
				  abs-x abs-y abs-x-dest abs-y-dest)
				 (with-graphics-screen-parameters
				     (line-segment
				      abs-x abs-y abs-x-dest abs-y-dest))))
			      ;; invalidate the shape and extent caches
					;(invalidate-window-shape-and-extent-caches self)
			      ))))))))))

;;;
;;; ****************   NOTE   ****************
;;;
;;; In the SGI version, only 'overlay will be supported
;;; because both XOR-REDRAW and SAVE-UNDER will be comparatively
;;; SLOW.  This should eventually be changed but there are too many
;;; places right now that depend on (not (eq 'xor-redraw)) to mean
;;; use a save-under which isn't supported in the SGI version.
;;;
;;; At some point, this needs to change to 'OVERLAY and all those
;;; other places fixed...
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod update-save-under ((button button))
  (setf (slot-value button 'save-under) 'xor-redraw))

;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version doesn't use window shape caches
;;; so this is NOOPed out
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod invalidate-window-shape-and-extent-caches ((self button))
  nil)


;;;
;;; ****************   NOTE   ****************
;;;
;;; The SGI version doesn't cache the transformed shape
;;; instead, the shape list is left in turtle centered
;;; (and turtle oriented) space and we instruct the hardware
;;; to render the turtle with the appropriate transformations
;;;
;;; ****************   NOTE   ****************
;;;

#+gl
(defmethod draw ((self button))
  (unless (null (shown? self))
    (unwind-protect
	 (progn
	   ;; save away old transformation state
	   (bw::drawmode gl::overdraw)
	   (bw::pushmatrix)
	   ;; now transform
	   (bw::translate (x-position self) (y-position self) 0.0)
	   ;;   should evetually be (z-position self) -----^
	   (bw::rotate (- (* 10 (round (heading self)))) #\z)
	   (playback-graphics-list-internal (shape self))
	   (unless (eq (shown? self) ':no-subsprites)
	     (dolist (subs (slot-value self 'subsprites))
	       (draw subs))))
      (bw::popmatrix)
      (bw::drawmode gl::normaldraw))))

;;;
;;; ****************   NOTE   ****************
;;;
;;; This should actually be called slow-erase
;;; Anyways, this just blanks the overlay plane
;;; at some point, a finer grained version of this
;;; ought to be written.  ALternatively, rewrite the
;;; various macros (like with-sprite-primitive-environment
;;; in grfdfs.lisp to just blank the overlay plane
;;; once and NOT loop through the list of sprites
;;;
;;; ****************   NOTE   ****************
;;;
#+gl
(defmethod fast-erase ((turtle button))
  (bw::clear-overlay-planes))


;;;
;;; ****************   NOTE   ****************
;;;
;;; Punting on enclosing-rectangle for the SGI for now.  Need
;;; to think about what hardware resources are available and
;;; how fast they wil be.  The alternative is to use a cache
;;; like the generic version and cycle through the transformed
;;; shape except that the cache can be
;;; a lot smaller since it doesn't have to hold the window
;;; coordinate version of the sprite's shape.
;;;
;;; ****************   NOTE   ****************
;;;

#+gl
(defmethod enclosing-rectangle ((self button))
  (warn "Enclosing-Rectangle is not implemented")
  (values 0 0 0 0))

    ;; on the SGI version we don't use it because we let the hardware
    ;; draw the turtle directly from the turtle's shape
    #-gl

;;;;
;;;; FILE: grobjs.lisp
;;;;

;;; The actual def-redisplay-initialization moved to gdispl.lisp
;;; for ordering reasons
#|
;;; This has to be a redisplay init because make-turtle-shape
;;; depends upon the runtime value of *foreground-color* which
;;; is not defined until AFTER the windows are created for some
;;; window systems (like CLX)
(def-redisplay-initialization ; :turtle-shape
    (setq *default-graphics-object-shape*
    (let ((%graphics-list (make-turtle-shape 8))
    (*graphics-command-recording-mode* ':boxer))
            (record-boxer-graphics-command-change-alu alu-seta)
      (record-boxer-graphics-command-change-pen-width 1)
      (record-boxer-graphics-command-centered-rectangle
       0.0 0.0
       *default-graphics-object-size* *default-graphics-object-size*)
      %graphics-list)
    *default-turtle-shape*
    (let ((%graphics-list (make-turtle-shape 8))
    (*graphics-command-recording-mode* ':boxer))
            (record-boxer-graphics-command-change-alu alu-seta)
      (record-boxer-graphics-command-change-pen-width 1)
      ;; the base line
      (record-boxer-graphics-command-line-segment
       (- *turtle-half-base*) (- (/ *turtle-height* 3.0))
       *turtle-half-base* (- (/ *turtle-height* 3.0)))
      ;; the right side
      (record-boxer-graphics-command-line-segment
       *turtle-half-base* (- (/ *turtle-height* 3.0))
       0.0 (* 2 (/ *turtle-height* 3)))
      ;; the left side
      (record-boxer-graphics-command-line-segment
       0.0 (* 2 (/ *turtle-height* 3))
       (- *turtle-half-base*) (- (/ *turtle-height* 3.0)))
      %graphics-list)))

|#

;;; We go through these contortions in order to reduce
;;; the floating point CONSing.  Using this version seems to
;;; reduce the FP consing from about 8 DP-FP numbers per call to 2
#+lcl3.0
(defun wrap-x-coordinate (user-x)
  (let ((float-temp 0.0) (float-width (float (the fixnum %drawing-width))))
    (declare (float float-temp float-width))
    (setq float-temp (float-plus %drawing-half-width user-x))
    (float-minus (if (and (plusp float-temp) (< float-temp float-width))
         float-temp
         (let ((scratch 0.0))
           (declare (float scratch))
           (setq scratch float-temp)
           (setq float-temp
           (values (ffloor float-temp float-width)))
           (setq float-temp (float-times float-temp float-width))
           (setq float-temp (float-minus scratch float-temp))
           (if (minusp float-temp)
         (float-plus float-temp float-width)
         float-temp)))
     %drawing-half-width)))


;;; We go through these contortions in order to reduce
;;; the floating point CONSing.  Using this version seems to
;;; reduce the FP consing from about 8 DP-FP numbers per call to 2
#+lcl3.0
(defun wrap-y-coordinate (user-y)
  (let ((float-temp 0.0) (float-height 0.0))
    (declare (float float-temp float-height))
    (setq float-height (float (the fixnum %drawing-height)))
    (setq float-temp (float-minus %drawing-half-height user-y))
    (float-minus %drawing-half-height
     (if (and (plusp float-temp) (< float-temp float-height))
         float-temp
         (let ((scratch 0.0))
           (declare (float scratch))
           (setq scratch float-temp)
           (setq float-temp
           (values (ffloor float-temp float-height)))
           (setq float-temp (float-times float-temp float-height))
           (setq float-temp (float-minus scratch float-temp))
           (if (minusp float-temp)
         (float-plus float-temp float-height)
         float-temp))))))

;;;;
;;;; FILE: grprim1.lisp
;;;;

(boxer-eval::defboxer-primitive bu::clear-graphics ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::clear-graphics
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

(boxer-eval::defboxer-primitive bu::cleargraphics ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::cleargraphics
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

(boxer-eval::defboxer-primitive bu::cg ()
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::cg
                                       " is no longer available"))
        (t
         (clearscreen-internal))))

;;;;
;;;; FILE: grprim2.lisp
;;;;

;(defsprite-function bu::stamp-partial-bitmap ((bu::port-to graphics-box)
;					      (boxer-eval::numberize src-x)
;					      (boxer-eval::numberize src-y)
;					      (boxer-eval::numberize width)
;					      (boxer-eval::numberize height))
;  (sprite turtle)
;  (with-sprites-hidden t
;    (stamp-partial-bitmap-for-turtle ...))
;  boxer-eval::*novalue*)

;;;; mousing around...
;; #-opengl
;; (defsprite-function bu::follow-mouse () (sprite turtle)
;;   (let ((screen-box (or (car (fast-memq (bp-screen-box *mouse-bp*)
;;                                         ;; is *mouse-bp* valid ?
;;                                         (get-visible-screen-objs
;;                                          (slot-value turtle 'assoc-graphics-box))))
;;                         (car (displayed-screen-objs
;; 			      ;; this is wrong but ignore ports for the moment
;; 			      (slot-value turtle 'assoc-graphics-box))))))
;;     (multiple-value-bind (window-x-offset window-y-offset)
;; 	(xy-position screen-box)
;;       (multiple-value-bind (left top right bottom)
;; 	  (box-borders-widths (box-type screen-box) screen-box)
;; 	(let* ((min-x (+& window-x-offset left))
;; 	       (min-y (+& window-y-offset top))
;;                (superior-turtle (superior-turtle turtle))
;;                (sup-x (if (null superior-turtle) 0
;;                           (absolute-x-position superior-turtle)))
;;                (sup-y (if (null superior-turtle) 0
;;                           (absolute-y-position superior-turtle))))
;; 	  (flet ((translate-x (window-x)
;; 			      (- (user-coordinate-x (-& window-x min-x))
;;                                  sup-x))
;; 		 (translate-y (window-y)
;; 			      (- (user-coordinate-y (-& window-y min-y))
;;                                  sup-y)))
;; 	    (warp-pointer *boxer-pane*
;; 			  (+ window-x-offset left (fix-array-coordinate-x
;; 						   (absolute-x-position turtle)))
;; 			  (+ window-y-offset top  (fix-array-coordinate-y
;; 						   (absolute-y-position turtle))))
;; 	    (multiple-value-bind (final-x final-y moved?)
;; 		(let ((%mouse-usurped t))
;; 		  (with-mouse-tracking-inside ((mouse-x min-x) (mouse-y min-y)
;; 					       min-x min-y
;; 					       (-& (+& window-x-offset
;; 						       (screen-obj-wid
;; 							screen-box))
;; 						   right 1)
;; 					       (-& (+& window-y-offset
;; 						       (screen-obj-hei
;; 							screen-box))
;; 						   bottom 1)
;; 					       #+MCL :view #+MCL *boxer-pane*)
;; 		  (with-sprites-hidden t
;; 		    (move-to turtle
;; 			     (translate-x mouse-x) (translate-y mouse-y)))))
;; 	      (when moved?
;; 		(with-sprites-hidden t
;; 		  (move-to turtle
;; 			   (translate-x final-x) (translate-y final-y))))))))))
;;     boxer-eval::*novalue*)

(defsprite-function bu::stamp-wedge ((boxer-eval::numberize radius)
                                     (boxer-eval::numberize sweep-angle))
                    (sprite turtle)
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'stamp-wedge " is no longer available, use "
                                       'draw-wedge " instead"))
        (t
         (if (< radius 0)
             (boxer-eval::primitive-signal-error :sprite-error
				           "The Radius, "
				           radius
				           "Should be 0 or greater")
           (with-sprites-hidden t
	     (stamp-wedge turtle radius sweep-angle)))))
      boxer-eval::*novalue*)

(defsprite-function bu::stamp-arc ((boxer-eval::numberize radius)
                                   (boxer-eval::numberize sweep-angle))
                    (sprite turtle)
  (cond ((not (null *uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'stamp-arc " is no longer available, use "
                                       'draw-arc " instead"))
        (t
         (if (< radius 0)
             (boxer-eval::primitive-signal-error :sprite-error
				           "The Radius, "
				           radius
				           "Should be 0 or greater")
           (with-sprites-hidden t
	     (stamp-arc turtle radius sweep-angle)))))
  boxer-eval::*novalue*)

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
;;;; FILE: macros.lisp
;;;;

;;; Trig in Degrees
;; There are 3 flavors of trig here, use the one which works best in
;; your implementation.  The three flavors are:
;;  . double precision floating point.  Seems to work best when there
;;    is good hardware support for IEEE floating point and also
;;    in implementations which do not carry out single precision
;;    arithmetic (like lucid's)
;;
;;  . single precision floating point.  Can be faster is the implementation
;;    actually suports it.  As opposed to pretending to support it but
;;    really doing double precision and then coercing
;;
;;  . Table Lookup + linear interpolation.  Useful for implementations
;;    which lack fast trig functions
;;

(defconstant +degs->rads+ (/ pi 180.0))

#-(or lispm tltrig)
(progn					; double precision
  (defun sind (x) (sin (float-times (float x) +degs->rads+)))

  (defun cosd (x) (cos (float-times (float x) +degs->rads+)))
  )

#+tltrig
(progn					; table lookup + linear interpolation
;;; we could phase shift instead of having 2 tables
;;; but lets not get TOO complicated unless we have to
  (defun make-sin-table ()
    (let ((table (make-array 360 :element-type 'double-float)))
      (dotimes (i 360)
        (setf (aref table i) (sin (* i +degs->rads+))))
      table))

  (defun make-cos-table ()
    (let ((table (make-array 360 :element-type 'double-float)))
      (dotimes (i 360)
        (setf (aref table i) (cos (* i +degs->rads+))))
      table))

  (defvar *sin-lookup-table* (make-sin-table))
  (defvar *cos-lookup-table* (make-cos-table))

  (defmacro sin-table-lookup (idx)
    `(the float (aref (the (simple-array float (360)) *sin-lookup-table*)
                      (the fixnum ,idx))))

  (defmacro cos-table-lookup (idx)
    `(the float (aref (the (simple-array float (360)) *cos-lookup-table*)
                      (the fixnum ,idx))))

  (defun tlsin-float-internal (degrees)
    (declare (float degrees))
    (multiple-value-bind (idx frac)
        (the (values fixnum float) (floor degrees))
      (declare (fixnum idx) (float frac))
      (setq idx (mod idx 360))
      (let ((t1 0.0) (t2 0.0))
        (declare (float t1 t2))
        (setq t1 (sin-table-lookup idx)
              t2 (sin-table-lookup (1+& idx)))
        ;; make t2 hold the difference
        (setq t2 (float-minus t2 t1))
        ;; now scale the difference
        (setq t2 (float-times t2 frac))
        ;; the answer is...
        (float-plus t1 t2))))

  (defun sind (degrees)
    (if (typep degrees 'fixnum)
        (sin-table-lookup (mod (the fixnum degrees) 360))
        (tlsin-float-internal degrees)))

  (defun tlcos-float-internal (degrees)
    (declare (float degrees))
    (multiple-value-bind (idx frac)
        (the (values fixnum float) (floor degrees))
      (declare (fixnum idx) (float frac))
      (setq idx (mod idx 360))
      (let ((t1 0.0) (t2 0.0))
        (declare (float t1 t2))
        (setq t1 (cos-table-lookup idx)
              t2 (cos-table-lookup (1+& idx)))
        ;; make t2 hold the difference
        (setq t2 (float-minus t2 t1))
        ;; now scale the difference
        (setq t2 (float-times t2 frac))
        ;; the answer is...
        (float-plus t1 t2))))

  (defun cosd (degrees)
    (if (typep degrees 'fixnum)
        (cos-table-lookup (mod (the fixnum degrees) 360))
        (tlcos-float-internal degrees)))
  )


;;;;
;;;; FILE: makcpy.lisp
;;;;

#|
;; Yuck, this is an example of how NOT to do this, it iterates over
;; the characters at least 4 (!) times
(defmethod text-string ((row row))
  (let* ((chas (chas row))
	(NO-OF-BOXES (COUNT-IF #'BOX? CHAS)))
    (IF (ZEROP NO-OF-BOXES)
	(let ((string (make-array (length chas)
				   :ELEMENT-TYPE
				   #-(or mcl symbolics) 'STRING-CHAR
				   #+(or mcl symbolics) 'character)))
	  (do ((i 0 (1+ i))
	       (chas chas (cdr chas)))
	      ((null chas))
	      (setf (aref string i)
		    #+(or symbolics mcl) (car chas)
		    #-(or symbolics mcl) (make-char (car chas))))
	  string)
	(let ((return-string (make-array (+& (length chas) no-of-boxes)
					 :element-type
					 #-(or mcl symbolics) 'string-char
					 #+(or mcl symbolics) 'character
					 :fill-pointer 0)))
	  (dolist (cha chas (values return-string t))
	    (cond ((cha? cha) (vector-push cha return-string))
		  (t
		   (vector-push #\[ return-string)
		   (vector-push #\] return-string))))))))

;; too much CONSing...
(defmethod text-string ((box box))
  (let ((return-string ""))
    (do-box-rows ((row box))
       (if (eq row (first-inferior-row box))
	   (setq return-string (text-string row))
	   (setq return-string (concatenate 'string
					    return-string
					    (make-string 1 :initial-element #\return)
					    (text-string row)))))
    return-string))

|#

#|  (originally from emanip.lisp ) this still needs to be converted....

(DEFUN MAKE-BOX-FROM-STRING (STRING)
  "make a box from a string.  carriage returns start new rows.  this is the inverse function
to the :TEXT-STRING method of boxes. "
  (MAKE-BOX
    (LOOP WITH START = 0
	  FOR INDEX FROM 0 TO (1- (LENGTH STRING))
	  FOR CHA = (AREF STRING INDEX)
	  WHEN (OR (CHAR= CHA #\CR) (CHAR= CHA #\LINE))
	    COLLECT (LIST (NSUBSTRING STRING START INDEX)) INTO ROWS
	  WHEN (OR (CHAR= CHA #\CR) (CHAR= CHA #\LINE))
	    DO (SETQ START (1+ INDEX))
	  FINALLY
	    (RETURN (APPEND ROWS (LIST (LIST (NSUBSTRING STRING START INDEX))))))))




;;;;BOX-EQUAL
(DEFUN BOX-EQUAL (BOX1 BOX2)
  (EQUAL BOX1 BOX2))

(DEFUN ROW-EQUAL (ROW1 ROW2)
  (EQUAL ROW1 ROW2))

;(DEFMETHOD (BOX :EQUL) (BOX)
;  (LET ((MY-LENGTH-IN-ROWS (LENGTH-IN-ROWS SELF))
;	(HE-LENGTH-IN-ROWS (LENGTH-IN-ROWS BOX)))
;    (COND ((NOT (= MY-LENGTH-IN-ROWS HE-LENGTH-IN-ROWS)) NIL)
;	  (T
;	   (DO* ((ROW-NO 0 (+ ROW-NO 1))
;		 (MY-ROW (ROW-AT-ROW-NO SELF ROW-NO) (ROW-AT-ROW-NO SELF ROW-NO))
;		 (HE-ROW (ROW-AT-ROW-NO BOX ROW-NO) (ROW-AT-ROW-NO BOX ROW-NO)))
;		((>= ROW-NO MY-LENGTH-IN-ROWS) T)
;	     (OR (EQUAL MY-ROW HE-ROW)
;		 (RETURN NIL)))))))
;
;(DEFMETHOD (ROW :EQUL) (ROW)
;  (LET ((MY-LENGTH-IN-CHAS (LENGTH-IN-CHAS SELF))
;	(HE-LENGTH-IN-CHAS (LENGTH-IN-CHAS ROW)))
;    (COND ((NOT (= MY-LENGTH-IN-CHAS HE-LENGTH-IN-CHAS)) NIL)
;	  (T
;	   (DO* ((CHA-NO 0 (+ CHA-NO 1))
;		 (MY-CHA (CHA-AT-CHA-NO SELF CHA-NO) (CHA-AT-CHA-NO SELF CHA-NO))
;		 (HE-CHA (CHA-AT-CHA-NO ROW CHA-NO) (CHA-AT-CHA-NO ROW CHA-NO)))
;		((>= CHA-NO MY-LENGTH-IN-CHAS) T)
;	     (COND ((AND (BOX? MY-CHA) (BOX? HE-CHA))
;		    (IF (NOT (EQUAL MY-CHA HE-CHA))
;			(RETURN NIL)))
;		   ((= (CHAR-CODE MY-CHA) (CHAR-CODE HE-CHA))
;		    T)
;		   (T (RETURN NIL))))))))

|#

;;;;
;;;; FILE: misc-prims.lisp
;;;;

(boxer-eval::defboxer-primitive bu::set-text-size ((bu::port-to box)
                                                   (boxer-eval::numberize width)
                                                   (boxer-eval::numberize height))
  (cond ((not (null *uc-copyright-free*))
          (boxer-eval::primitive-signal-error :copyright
                                              'bu::set-text-size
                                              " is no longer available, use "
                                              'bu::set-text-dimensions " instead"))
    (t
      (let ((realbox (box-or-port-target box)))
        (when (box? realbox)
          (let ((*current-font-descriptor* (closest-bfd
                                            (first-inferior-row realbox) 0)))
            (multiple-value-bind (font-cha-wid font-cha-hei)
                                (current-font-values)
                                (set-fixed-size realbox
                                                (* width font-cha-wid) (* height font-cha-hei))
                                ;; allow further mousing
                                (unless (bottom-right-hotspot-active? realbox)
                                  (set-bottom-right-hotspot-active? realbox t))
                                (modified realbox)))))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::set-port-text-size ((boxer-eval::dont-copy port)
                                                        (boxer-eval::numberize width)
                                                        (boxer-eval::numberize height))
  (cond ((not (null *uc-copyright-free*))
          (boxer-eval::primitive-signal-error :copyright
                                              'bu::set-port-text-size
                                              " is no longer available, use "
                                              'bu::set-port-text-dimensions
                                              " instead"))
    (t
      (let ((realbox (box-or-port-target port)))
        (when (virtual-port? port)
          (setq port (vp-editor-port-backpointer port)))
        (when (and (box? realbox) (port-box? port))
          (let ((*current-font-descriptor* (closest-bfd
                                            (first-inferior-row realbox) 0)))
            (multiple-value-bind (font-cha-wid font-cha-hei)
                                (current-font-values)
                                (set-fixed-size port
                                                (* width font-cha-wid) (* height font-cha-hei))
                                ;; allow further mousing
                                (unless (bottom-right-hotspot-active? port)
                                  (set-bottom-right-hotspot-active? port t))
                                (modified port)))))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::scroll-to-row ((bu::port-to box) (boxer-eval::numberize row))
  (cond ((not (null *uc-copyright-free*))
          (boxer-eval::primitive-signal-error :copyright
                                              'bu::scroll-to-row
                                              " is no longer available, use "
                                              'bu::set-scroll-row " instead"))
    (t
      (let ((target (box-or-port-target box)))
        (cond ((and (box? target)
                    (typep row 'fixnum) (plusp& row))
              (let ((edrow (row-at-row-no target (1- row)))
                    (screen-objs (screen-objs target)))
                (unless (null edrow)
                  (cond ((null screen-objs)
                          (record-scroll-info target nil edrow))
                    (t
                      (dolist (screen-obj screen-objs)
                        (set-scroll-to-actual-row screen-obj edrow))))))
              boxer-eval::*novalue*)
          (t
          (boxer-eval::primitive-signal-error
            :scrolling-error "You can only scroll editor boxes")))))))

;;;;
;;;; FILE: mouse.lisp
;;;;

;;; old
;(defsubst screen-boxes-in-row (screen-row)
;  (subset #'screen-box? (inferiors screen-row)))

#|
;;; **** this is now hacked in boxwin-mcl by modifying the coords
;;; **** of the mouse blip
;; make sure the returned value will be a row
;;; +++ I made these account for getting their coordinates in the window frame, parallel to
;;; mouse-position-values, above.  This is necessary to make tracking work right on the Mac
;;; version.  I have no idea what effect it will have on the Sun version. -- mt
#+mcl
(defun mouse-position-screen-row-values (global-x global-y)
  (multiple-value-bind (so local-offset position)
      (screen-obj-at (outermost-screen-box)
                     (- global-x (sheet-inside-left *boxer-pane*))
                     (- global-y (sheet-inside-top *boxer-pane*)))
    (cond ((screen-row? so)
       (values so local-offset))
      (t
       (let ((sr (screen-row so)))
         (if (not (screen-row? sr))
         (ecase position
           (:top (values (first-screen-row so) local-offset))
           (:bottom (values (last-screen-row so) local-offset)))
         (values sr local-offset)))))))

|#

;;; this is obsolete
(defmacro with-mouse-bp-bound ((x y window) &body body)
  (declare (ignore x y window body))
  (error "This macro is obsolete"))

;;; This shouldn't be consing up a BP every time ....
;(defmacro with-mouse-bp-bound ((x y window) &body body)
;  "This macro sets up an environment where MOUSE-BP is bound to a BP which
;   indicates where in the actual structure the mouse is pointing to.
;   MOUSE-SCREEN-BOX is also bound to the screen box which the mouse is
;   pointing to. "
;  `(let ((mouse-bp (make-bp ':fixed)))
;     (multiple-value-bind (mouse-row mouse-cha-no mouse-screen-box)
;	 (screen-obj-at-position ,x ,y ,window)
;     (unwind-protect
;       (progn
;	 (set-bp-row mouse-bp mouse-row)
;	 (set-bp-cha-no mouse-bp mouse-cha-no)
;	 (set-bp-screen-box mouse-bp mouse-screen-box)
;	 . ,body)
;       (when (not-null (bp-row mouse-bp))
;	 (delete-bp (bp-row mouse-bp) mouse-bp))))))



#|
;;;; RESIZE Support

(defconstant *resize-blinker-width* 1)

(defun draw-resize-blinker (x y wid hei)
  (draw-rectangle alu-xor *resize-blinker-width* hei x y)
  (draw-rectangle alu-xor
          (-& wid *resize-blinker-width*) *resize-blinker-width*
          (+& x *resize-blinker-width*) y)
  (draw-rectangle alu-xor
          *resize-blinker-width* (-& hei *resize-blinker-width*)
          (-& (+& x wid ) *resize-blinker-width*)
          (+& y *resize-blinker-width*))
  (draw-rectangle alu-xor
          (-& wid (*& 2 *resize-blinker-width*)) *resize-blinker-width*
          (+& x *resize-blinker-width*)
          (-& (+& y hei) *resize-blinker-width*)))

(defun resize-tracker (editor-box x y screen-box)
  (multiple-value-bind (box-window-x box-window-y)
      (xy-position screen-box)
    (multiple-value-bind (left top right bottom)
        (box-borders-widths (slot-value screen-box 'box-type) screen-box)
     (drawing-on-window (*boxer-pane*)
    ;; draw the resize icon on the box...
        (bitblt-to-screen alu-seta 7 7 *mouse-resize-bitmap* 0 0
              (-& (+& box-window-x (screen-obj-wid screen-box))
                  right)
              (-& (+& box-window-y (screen-obj-hei screen-box))
                  bottom))
    (let ((old-wid x) (old-hei y))
      ;; draw-the initial resize blinker
      (draw-resize-blinker box-window-x box-window-y old-wid old-hei)
    ;; now track the mouse
    (multiple-value-bind (final-x final-y)
        (with-mouse-tracking ((mouse-x box-window-x)
                  (mouse-y box-window-y))
          (let ((new-wid (-& mouse-x box-window-x))
            (new-hei (-& mouse-y box-window-y)))
        (unless (or (minusp new-wid) (minusp new-hei))
          ;; erase the previous resize blinker
          (draw-resize-blinker box-window-x box-window-y
                       old-wid old-hei)
          ;; and update the values and draw the new one
          (setq old-wid new-wid old-hei new-hei)
          (draw-resize-blinker box-window-x box-window-y
                       new-wid new-hei))))
      ;; erase the last hollow blinker
      (draw-resize-blinker box-window-x box-window-y old-wid old-hei)
      (unless (or (<& final-x box-window-x)
              (<& final-y box-window-y))
        (set-fixed-size editor-box
                (- final-x box-window-x left right)
                (- final-y box-window-y top bottom))))))))
  (modified editor-box)
  (box-first-bp-values editor-box))


(setf (get :resize-tab 'mouse-bp-values-handler) 'resize-tracker)

|#

;;;;
;;;; FILE: mousedoc.lisp
;;;;

  #+mcl "Click to Supershrink (Hold for more choices)"
  #+mcl "Click to Shrink (Hold for more choices)"
  #+mcl "Click to Expand (Hold for more choices)"
  #+mcl "Click to Expand to Fullscreen (Hold for more choices)"
  #+mcl "Flip to Graphics (Hold for more choices)"
  #+mcl "Flip to Text (Hold for more choices)"
  #+mcl "Hold for more choices"
  #+mcl "Hold for more choices"

  #-opengl
(defun mouse-doc-status-backing () (svref& *mouse-doc-status* 2))

#-opengl
(defun set-mouse-doc-status-backing (newback)
  (setf (svref& *mouse-doc-status* 2) newback))

           #-opengl
           (boxer::drawing-on-window (*boxer-pane*)
             (document-mouse-dispatch place screen-box T))

#-opengl
           (boxer::drawing-on-window (*boxer-pane*)
             (document-mouse-dispatch place screen-box))

  #+mcl
(defun popup-doc-delay ()
  (let ((original-event-id (event-id)))
    (or
     (process-wait-with-timeout "Mouse Documentation"
                                (round (* 60 *mouse-doc-wait-time*))
                                #'(lambda ()
                                    (not (= (event-id) original-event-id))))
     (boxer::drawing-on-window (*boxer-pane*)
       ;; why is this neccessarry ? shouldn't we already be in a drawing-on-window ?
       ;; perhaps the process switch messes the graphics state up ?
       (neq (mouse-place) (mouse-doc-status-place))))))

           #-opengl ; only need to "undraw" for non OpenGL
    (boxer::drawing-on-window (*boxer-pane*) (undocument-mouse-dispatch))

    #-opengl
        (when *change-mouse-on-hotspots*
          (set-mouse-cursor-internal *current-mouse-cursor*))

#-opengl
(defun undocument-mouse-dispatch ()
  (let* ((place (mouse-doc-status-place))
         (screen-box (mouse-doc-status-screen-box))
         (edbox (cond ((boxer::screen-box? screen-box)
                       (boxer::screen-obj-actual-obj screen-box))
                      ((boxer::sprite-box? screen-box)
                       screen-box)
                      (t nil)))
         (target (when edbox (boxer::box-or-port-target edbox))))
    (case place
      (:top-left     (boxer::popup-undoc-shrink
                      screen-box
                      (and (eq (boxer::display-style edbox) :shrunk)
                           (not (eq screen-box (outermost-screen-box))))))
      (:top-right    (boxer::popup-undoc-expand
                      screen-box (neq (boxer::display-style edbox) :shrunk)))
      (:bottom-left  (unless (null (slot-value target 'boxer::graphics-sheet))
                       (boxer::popup-undoc-view-flip
                        screen-box (not (boxer::graphics-screen-box? screen-box)))))
      (:bottom-right (if (boxer::bottom-right-hotspot-active? edbox)
                       (boxer::popup-undoc-resize screen-box)
                       (boxer::popup-undoc-resize screen-box t)))
      ((:type :port-target-type) (boxer::popup-undoc-toggle-type screen-box))
      ;; future expansion...
      (:name-handle)
      (:graphics (boxer::popup-undoc-graphics screen-box))
      (:sprite))))

;;;;
;;;; FILE: new-borders.lisp
;;;;

#|
(defun scroll-up-tracking-info (screen-box)
  )

(defun scroll-down-tracking-info (screen-box)
  )

;; ???
(defun scroll-elevator-tracking-info (screen-box)
  )
|#

#|
;; old style
(defun draw-mouse-shrink-corner (x y)
  (with-pen-color (*mouse-doc-highlight-color*)
    (draw-poly alu-seta (list (cons (+ x -1) (+ y 3)) (cons (+ x 3) (+ y 3))
                              (cons (+ x 3)  (+ y -1))(cons (+ x -1) (+ y 3))))
    (draw-poly alu-seta (list (cons (+ x 4) (+ y -1)) (cons (+ x 4) (+ y 3))
                              (cons (+ x 7)  (+ y 3)) (cons (+ x 4) (+ y -1))))
    (draw-poly alu-seta (list (cons (+ x 8) (+ y 4)) (cons (+ x 4) (+ y 4))
                              (cons (+ x 4)  (+ y 8))(cons (+ x 8) (+ y 4))))
    (draw-poly alu-seta (list (cons (+ x -1) (+ y 4)) (cons (+ x 3) (+ y 4))
                              (cons (+ x 3)  (+ y 7))(cons (+ x -1) (+ y 4))))))
|#

#|
;; old style
(defun draw-mouse-expand-corner (x y)
  (let* ((last-pt (+& *border-inside-space* *basic-border-width*))
         (arrow-side (floor last-pt 2)))
    (with-pen-color (*mouse-doc-highlight-color*)
      ;; top left
      (draw-poly alu-seta (list (cons x y)   (cons x (+& y arrow-side 1))
                                (cons (+& x arrow-side 1) y) (cons x y)))
      ;; top right
      (draw-poly alu-seta (list (cons (-& (+& x last-pt) arrow-side 1) y)
                                (cons (+& x last-pt) y)
                                (cons (+& x last-pt) (+& y arrow-side))
                                (cons (-& (+& x last-pt) arrow-side 1) y)))
      ;; bottom right
      (draw-poly alu-seta (list (cons (+& x last-pt) (+& y arrow-side))
                                (cons (+& x last-pt) (+& y last-pt))
                                (cons (-& (+& x last-pt) arrow-side 1) (+& y last-pt))
                                (cons (+& x last-pt) (+& y arrow-side))))
      ;; bottom left
      (draw-poly alu-seta (list (cons (+& x arrow-side) (+& y last-pt))
                                (cons x (+& y last-pt))
                                (cons x (-& (+& y last-pt) arrow-side 1))
                                (cons (+& x arrow-side) (+& y last-pt)))))))
|#

;;;;
;;;; FILE: opengl-utils.lisp
;;;;

;; stub
(defun sheet-font-map (w) (declare (ignore w)) nil)

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

;;;;
;;;; FILE: oglscroll.lisp
;;;;
(defun draw-vertical-scroll-buttons (x y)
  (let ((left-x  (+ x *scroll-info-offset*))
        (mid-x   (+ x *scroll-info-offset* (/ *scroll-button-width* 2)))
        (right-x (+ x *scroll-info-offset* *scroll-button-width*))
        (top-button-bottom-y (+ y 1 *scroll-button-length*))
        (bottom-button-top-y (+ y 1 *scroll-button-length* 1)))
    (with-pen-color (*scroll-buttons-color*)
      ;; upper button
      (draw-poly alu-seta (list (cons mid-x (+ y 1))
                                (cons left-x top-button-bottom-y)
                                (cons right-x top-button-bottom-y)))
      ;; lower-button
      (draw-poly alu-seta (list (cons left-x bottom-button-top-y)
                                (cons right-x bottom-button-top-y)
                                (cons mid-x (+ y 1 *scroll-button-length* 1 *scroll-button-length*)))))))

(defun draw-horizontal-scroll-buttons (x y)
  (let ((top-y  (+ y *scroll-info-offset*))
        (mid-y   (+ y *scroll-info-offset* (round *scroll-button-width* 2)))
        (bottom-y (+ y *scroll-info-offset* *scroll-button-width*))
        (left-button-right-x (+ x 1 *scroll-button-length*))
        (right-button-left-x (+ x 1 *scroll-button-length* 1)))
    (with-pen-color (*scroll-buttons-color*)
      ;; upper button
      (draw-poly alu-seta (list (cons (+ x 1) mid-y)
                                (cons left-button-right-x top-y)
                                (cons left-button-right-x bottom-y)))
      ;; lower-button
      (draw-poly alu-seta (list (cons right-button-left-x top-y)
                                (cons right-button-left-x bottom-y)
                                (cons (+ x 1 *scroll-button-length* 1 *scroll-button-length*) mid-y))))))


;;;;
;;;; FILE: popup.lisp
;;;;

;; sgithens remove #-opengl
(defmethod menu-select ((menu popup-menu) x y)
  (multiple-value-bind (mwid mhei) (menu-size menu)
    (let* ((window-width (sheet-inside-width *boxer-pane*)); what about %clip-rig?
           (window-height (sheet-inside-height *boxer-pane*)) ; %clip-bot?
           (fit-x (-& (+& x mwid) window-width))
           (fit-y (-& (+& y mhei) window-height))
           (backing (make-offscreen-bitmap *boxer-pane* mwid mhei))
           ;; if either fit-? vars are positive, we will be off the screen
           (real-x (if (plusp& fit-x) (-& window-width mwid) x))
           (real-y (if (plusp& fit-y) (-& window-height mhei) y))
           ;; current-item is bound out here because we'll need it after the
           ;; tracking loop exits...
           (current-item nil))
      (unless (zerop& (mouse-button-state))
        ;; make sure the mouse is still down
        ;; if the original x and y aren't good, warp the mouse to the new location
        ;; a more fine tuned way is to use fit-x and fit-y to shift the current
        ;; mouse pos by the amount the menu is shifted (later...)
        (drawing-on-window (*boxer-pane*)
          (when (or *select-1st-item-on-popup* (plusp& fit-x) (plusp& fit-y))
            (warp-pointer *boxer-pane* (+& real-x 5) (+& real-y 5)))
          ;; grab the area into the backing store
          (bitblt-from-screen alu-seta mwid mhei backing real-x real-y 0 0)
          ;; now draw the menu and loop
          (unwind-protect
            (progn
              ;; draw menu
              (draw-menu menu real-x real-y)
              ;; loop
              (let ((current-y 0) (current-height 0))
                (with-mouse-tracking ((mouse-x real-x) (mouse-y real-y))
                  (let ((local-x (-& mouse-x real-x)) (local-y (-& mouse-y real-y)))
                    (if (and (<& 0 local-x mwid) (<& 0 local-y mhei))
                      ;; this means we are IN the popup
                      (multiple-value-bind (ti iy ih)
                                           (track-item menu local-y)
                        (cond ((and (null current-item)
                                    (not (slot-value ti 'enabled?)))
                               ;; no current, selected item is disabled...
                               )
                              ((null current-item)
                               ;; 1st time into the loop, set vars and then
                               (setq current-item ti current-y iy current-height ih)
                               ;; highlight
                               (draw-rectangle alu-xor (-& mwid 3) ih
                                               (1+& real-x) (+& real-y iy)))
                              ((eq ti current-item)) ; no change, do nothing
                              ((not (slot-value ti 'enabled?))
                               ;; new item is disabled but we have to erase...
                               (draw-rectangle alu-xor (-& mwid 3) current-height
                                               (1+& real-x) (+& real-y current-y))
                               (setq current-item nil))
                              (t ; must be a new item selected,
                               (draw-rectangle alu-xor (-& mwid 3) ih
                                               (1+& real-x) (+& real-y iy))
                               ;; erase old,
                               (draw-rectangle alu-xor (-& mwid 3) current-height
                                               (1+& real-x) (+& real-y current-y))
                               ;; set vars
                               (setq current-item ti current-y iy current-height ih))))
                      ;; we are OUT of the popup
                      (cond ((null current-item)) ; nothing already selected
                            (t ; erase old item
                             (draw-rectangle alu-xor (-& mwid 3) current-height
                                             (1+& real-x) (+& real-y current-y))
                             (setq current-item nil))))))
                ;; loop is done, either we are in and item or not
                ;; why do we have to do this ?
                #+carbon-compat
                (window-system-dependent-set-origin %origin-x-offset %origin-y-offset)
                (unless (null current-item)
                  ;; if we are in an item, flash and erase the highlighting
                  (dotimes (i 5)
                    (draw-rectangle alu-xor (-& mwid 3) current-height
                                    (1+& real-x) (+& real-y current-y))
                    (force-graphics-output)
                    (snooze .05)))))
            (bitblt-to-screen alu-seta mwid mhei backing 0 0 real-x real-y)
            (free-offscreen-bitmap backing)))
        ;; funcall the action (note we are OUTSIDE of the drawing-on-window
        (unless (null current-item)
          (let ((action (slot-value current-item 'action)))
            (unless (null action) (funcall action))))))))

;; use on non double-buffered window systems
#-opengl
(progn
;;; !!!! SHould use allocate-backing-store.....
;; called at the beginning and whenever the popup doc font is changed
(defun allocate-popup-backing ()
  (let ((wid 0)
        (padding (*& (+& *popup-doc-border-width* *popup-doc-padding*) 2)))
    (dolist (doc *popup-docs*)
      (setq wid (max wid (string-wid *popup-doc-font* (popup-doc-string doc)))))
    (let ((new-wid (+ padding wid))
          (new-hei (+ (string-hei *popup-doc-font*) padding)))
    (when (or (null *popup-doc-backing-store*)
              (not (= new-wid (offscreen-bitmap-width *popup-doc-backing-store*)))
              (not (= new-hei (offscreen-bitmap-height *popup-doc-backing-store*))))
      (unless (null *popup-doc-backing-store*)
        (free-offscreen-bitmap *popup-doc-backing-store*))
      (setq *popup-doc-backing-store*
            (make-offscreen-bitmap *boxer-pane* new-wid new-hei))))))

(def-redisplay-initialization (allocate-popup-backing))

)

#-opengl
(defmethod erase-doc ((self popup-doc) x y)
  (let ((total-padding (*& (+& *popup-doc-border-width* *popup-doc-padding*) 2)))
    (bitblt-to-screen alu-seta
                      (+& (string-wid *popup-doc-font* (slot-value self 'string))
                          total-padding)
                      (+& (string-hei *popup-doc-font*) total-padding)
                      *popup-doc-backing-store* 0 0 x y)
    (force-graphics-output)
    (setq *popup-doc-on?* nil)))

#-opengl (hotspot-back (allocate-backing-store *mouse-shrink-corner-bitmap*))
          #-opengl (hotwid (offscreen-bitmap-width  *mouse-shrink-corner-bitmap*))
          #-opengl (hothei (offscreen-bitmap-height *mouse-shrink-corner-bitmap*))

          handle the highlighting immediatement
          #-opengl
          (unless popup-only?
            (bw::set-mouse-doc-status-backing hotspot-back)
            (bitblt-from-screen alu-seta hotwid hothei hotspot-back
                                corner-x corner-y 0 0)
            ;; draw the hotspot
            (bitblt-to-screen alu-seta hotwid hothei *mouse-shrink-corner-bitmap*
                              0 0 corner-x corner-y))
          #+opengl

#-opengl
(defun popup-undoc-shrink (screen-box &optional supershrink?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (cond ((eq (screen-obj-actual-obj screen-box) *initial-box*)
                      :top-left-initial-box)
                     ((not (null  supershrink?))
                      :shrunken-top-left)
                     (t :top-left))))
        (backing (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-shrink-corner-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-shrink-corner-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (let ((corner-x (+& x delta-x))
              (corner-y (+& y delta-y (box-borders-cached-name-tab-height
                                       box-type screen-box))))
          (unless (or (null *popup-mouse-documentation?*)
                      (null *popup-doc-on?*))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (erase-doc doc doc-x doc-y)))
          ;; restore the hospot area
          (unless (null backing)
            (bitblt-to-screen alu-seta hotwid hothei backing 0 0 corner-x corner-y)
            (deallocate-backing-store *mouse-shrink-corner-bitmap* backing)))))))

#-opengl
(defun popup-undoc-expand (screen-box &optional fullscreen?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (cond ((eq screen-box (outermost-screen-box))
                     :top-right-outermost-box)
                    ((not (null fullscreen?))
                     :fullscreen)
                    (t :top-right))))
        (hotspot-back (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-expand-corner-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-expand-corner-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (declare (ignore delta-x))
        (multiple-value-bind (lef top rig bot)
            (box-borders-widths box-type screen-box)
          (declare (ignore lef top bot))
          (let ((corner-x (+& x (screen-obj-wid screen-box) (-& rig)))
                (corner-y (+& y delta-y (box-borders-cached-name-tab-height
                                         box-type screen-box))))
            (unless (or (null *popup-mouse-documentation?*)
                        (null *popup-doc-on?*))
              (multiple-value-bind (doc-x doc-y)
                  (popup-doc-offset-coords doc corner-x corner-y)
                (erase-doc doc doc-x doc-y)))
            (unless (null hotspot-back)
              (bitblt-to-screen alu-seta hotwid hothei hotspot-back
                                0 0 corner-x corner-y)
              (deallocate-backing-store *mouse-expand-corner-bitmap*
                                        hotspot-back))))))))

#-opengl
(defun popup-undoc-view-flip (screen-box &optional to-graphics?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area
              (if to-graphics? :bottom-left-g :bottom-left-t)))
        (hotspot-back (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-toggle-view-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-toggle-view-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (delta-x delta-y)
          (box-borders-offsets box-type screen-box)
        (declare (ignore delta-y))
        (multiple-value-bind (lef top rig bot)
            (box-borders-widths box-type screen-box)
          (declare (ignore lef top rig))
          (let ((corner-x (+& x delta-x))
                (corner-y (+& y (screen-obj-hei screen-box) (-& bot))))
            (unless (or (null *popup-mouse-documentation?*)
                        (null *popup-doc-on?*))
              (multiple-value-bind (doc-x doc-y)
                  (popup-doc-offset-coords doc corner-x corner-y)
                (erase-doc doc doc-x doc-y)))
            (unless (null hotspot-back)
              (bitblt-to-screen alu-seta hotwid hothei hotspot-back
                                0 0 corner-x corner-y)
              (deallocate-backing-store *mouse-toggle-view-bitmap*
                                        hotspot-back))))))))

#-opengl
(defun popup-undoc-resize (screen-box &optional is-off?)
  (let ((box-type (box-type screen-box))
        (doc (get-popup-doc-for-area (if is-off? :bottom-right-off :bottom-right)))
        (hotspot-back (bw::mouse-doc-status-backing))
        (hotwid (offscreen-bitmap-width  *mouse-resize-corner-bitmap*))
        (hothei (offscreen-bitmap-height *mouse-resize-corner-bitmap*)))
    (multiple-value-bind (x y)
        (xy-position screen-box)
      (multiple-value-bind (lef top rig bot)
          (box-borders-widths box-type screen-box)
        (declare (ignore lef top))
        (let ((corner-x (+& x (screen-obj-wid screen-box) (-& rig)))
              (corner-y (+& y (screen-obj-hei screen-box) (-& bot))))
          (unless (or (null *popup-mouse-documentation?*)
                      (null *popup-doc-on?*))
            (multiple-value-bind (doc-x doc-y)
                (popup-doc-offset-coords doc corner-x corner-y)
              (erase-doc doc doc-x doc-y)))
          (unless (null hotspot-back)
            (bitblt-to-screen alu-seta hotwid hothei hotspot-back
                              0 0 corner-x corner-y)
            (deallocate-backing-store *mouse-resize-corner-bitmap*
                                      hotspot-back)))))))

                                  ;; these are now all in the props dialog (mac)
                                  #-(or mcl lispworks)
                                  (make-instance 'menu-item
                                    :title "File"
                                    :action 'com-tt-toggle-storage-chunk)
                                  #-(or mcl lispworks)
                                  (make-instance 'menu-item
                                    :title "Read Only"
                                    :action 'com-tt-toggle-read-only)
                                  #-(or mcl lispworks)
                                  (make-instance 'menu-item
                                    :title "Autoload"
                                    :action 'com-tt-toggle-autoload-file)

;; frobs the items in the pop up to be relevant
;; more elegant to do this by specializing the menu-item-update method
;; on a tt-pop-up-menu-item class.  Wait until we hack the fonts to do this...
#-(or mcl lispworks)
(defun update-tt-menu (box)
  (let ((type-item  (car (menu-items *tt-popup*)))
        (store-item (find-menu-item *tt-popup* "File"))
        (read-item  (find-menu-item *tt-popup* "Read Only"))
        (autl-item  (find-menu-item *tt-popup* "Autoload")))
    (cond ((data-box? box)
           (set-menu-item-title type-item "Flip to Doit")
           (menu-item-enable type-item))
          ((doit-box? box)
           (set-menu-item-title type-item "Flip to Data")
           (menu-item-enable type-item))
          (t
           (set-menu-item-title type-item "Flip Box Type")
           (menu-item-disable type-item)))
    (cond ((storage-chunk? box)
           (set-menu-item-check-mark store-item t)
           ;; enable the other menu items in case they have been previously disabled
           (menu-item-enable read-item)
           (menu-item-enable autl-item))
          (t (set-menu-item-check-mark store-item nil)
             ;; disable remaining items because they only apply to storage-chunks
             (menu-item-disable read-item)
             (menu-item-disable autl-item)))
    ;; synchronize the remaining items, even if they are disabled, they
    ;; should still reflect the current values of the box
    (set-menu-item-check-mark read-item (read-only-box? box))
    (set-menu-item-check-mark autl-item (autoload-file? box))))


    ;; crock,
    #+carbon-compat
    (window-system-dependent-set-origin %origin-x-offset %origin-y-offset)

                ;; why do we have to do this ?
                #+carbon-compat
                (window-system-dependent-set-origin %origin-x-offset %origin-y-offset)

;;;;
;;;; FILE: prims.lisp
;;;;

(defrecursive-eval-primitive bu::any-of ((dont-copy box)
                                         (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null boxer::*uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::any-of " is no longer available, use "
                                       'bu::some " instead"))
        (t
         (cond ((not (null rest-of-line-must-be-empty))
                (signal-error
                 :any-of-bug
                 "ANY-OF statements must appear on lines by themselves."))
               ((or (numberp box) (not (fast-eval-data-box? box)))
                (signal-error :any-of-bug "expects a data box"))
               (t (let* ((code (interpreted-boxer-function-text
	                        (if (boxer::virtual-copy? box)
	                            (cached-code-virtual-copy box)
	                          (cached-code-editor-box box))))
                         (1stline (do ((line (car code) (car code)))
			              ((null code) nil)
			            (if (null line)
                                        (pop code)
                                      (return (pop code))))))
                    (if (null 1stline) *false*
                      (progn
                        (set-and-save-state-variables code)
                        (recursive-eval-invoke
                         (list* 'boxer-eval::any-of 1stline)))))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::any-of nextline)))))))

;; give helper function same name in eval package is a crock to make
;; the error message come out right
(defboxer-primitive boxer-eval::any-of ((dont-copy clause) (list-rest ignore))
  ignore ; bound but not used blah blah...
  (cond ((true? clause)
         (setq *boolean-clauses* nil) *true*)
        ((false? clause) *false*)
        (t (signal-error :any-of clause "neither true nor false"))))

(defrecursive-eval-primitive bu::all-of ((dont-copy box)
                                         (list-rest rest-of-line-must-be-empty))
  :state-variables (*boolean-clauses*)
  :before
  (cond ((not (null boxer::*uc-copyright-free*))
         (boxer-eval::primitive-signal-error :copyright
                                       'bu::all-of " is no longer available, use "
                                       'bu::every " instead"))
        (t
         (cond ((not (null rest-of-line-must-be-empty))
                (signal-error
                 :all-of-bug
                 "ALL-OF statements must appear on lines by themselves."))
               ((or (numberp box) (not (fast-eval-data-box? box)))
                (signal-error :all-of-bug "expects a data box"))
               (t (let* ((code (interpreted-boxer-function-text
	                        (if (boxer::virtual-copy? box)
	                            (cached-code-virtual-copy box)
	                          (cached-code-editor-box box))))
                         (1stline (do ((line (car code) (car code)))
			              ((null code) nil)
			            (if (null line)
                                        (pop code)
                                      (return (pop code))))))
                    (if (null 1stline) *true*
                      (progn
                        (set-and-save-state-variables code)
                        (recursive-eval-invoke
                         (list* 'boxer-eval::all-of 1stline)))))))))
  :after (cond ((null *boolean-clauses*)
                (restore-state-variables) nil)
               (t (let ((nextline (do ((line (car *boolean-clauses*)
					     (car *boolean-clauses*)))
				      ((null *boolean-clauses*) nil)
				    (if (null line) (pop *boolean-clauses*)
					(return (pop *boolean-clauses*))))))
		    (cond ((null nextline)
			   (restore-state-variables) nil)
			  (t
			   (cons 'boxer-eval::all-of nextline)))))))

(defboxer-primitive boxer-eval::all-of ((dont-copy clause) (list-rest ignore))
  ignore
  (cond ((true? clause) *true*)
        ((false? clause) (setq *boolean-clauses* nil) *false*)
        (t (signal-error :any-of clause "neither true nor false"))))

;;;;
;;;; FILE: realprinter.lisp
;;;;

#|
;;; This is what we stick into rows for ports until we can retarget
;;; them.  The reason we need to do this is that inserting a port
;;; that doesn't have a legit target plays havoc with all the port caching
;;; code so we defer inserting ports untilthe very end when we have both
;;; a well defined hierarchy that we are inserting into and a legitimate
;;; port-target pair to insert.
;;;

;;; establish a default for these so we don't blow out
(defmethod insert-self-action ((self t) &optional superior)
  ;(declare (ignore self superior))
  nil)

(defmethod delete-self-action ((self t) &optional superior)
  ;(declare (ignore self superior))
  nil)
;; old stuff.  Using editor ports instead with the relevant info on the PLiST

;;; this ought to be just a vector but I'd like to be able to see
;;; what is what for now
(defstruct (port-retargetting-placeholder
	     (:conc-name prp-)
	     #+symbolics
	     (:print-function print-placeholder-internal))
  row
  target)

#+symbolics
(defun print-placeholder-internal (pl stream depth)
  (declare (ignore pl depth))
  (format stream ""))

|#

#|
;;; old stuff.  when vc's are targets, use real ports and record the VC
;; on the plist to be retargetted later

(defun make-port-from-vp (vp)
  ;; now reset the target
  (let ((target (vp-target vp)))
    (cond ((virtual-copy? target)
	   ;; this may have been an interior link so record it
	   ;; cause we may need to retarget it later
	   (let ((placeholder (make-port-retargetting-placeholder
				:target target)))
	     (record-port-printing placeholder)
	     placeholder))
	  ((box? target)
	   (let ((port (make-uninitialized-box 'port-box)))
	     ;; fixup some slots that the init method would have fixed
	     (setf (slot-value port 'display-style-list) (make-display-style))
	     (setf (slot-value port 'tick) (tick))
	     ;; ports to editor boxes can be made immediately
	     (set-port-to-box port target)
	     ;; record it so port caching will work later
	     (record-outlink-port port)
	     (unless (null (vp-name vp))
	       (set-name port (make-name-row (list (vp-name vp)))))
	     port))
	  (t
	   (error "Virtual Port target, ~S, is not a Box or Virtual Copy"
		  target)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;           Old stuff below                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(defun convert-vc-box-to-editor-box (vc)
  (let ((box (make-uninitialized-box (vc-type vc)))
	(last-row (convert-evrow-to-editor-row (car (vc-rows vc)) vc)))
    (initialize-from-defaults box)
    (insert-row-at-row-no box last-row 0)
    (dolist (evrow (cdr (vc-rows vc)))
      (let ((new-row (convert-evrow-to-editor-row evrow vc)))
	(insert-row-after-row box new-row last-row)
	(setq last-row new-row)))
    (when (vc-name vc)
      (set-name box (make-name-row `(,(vc-name vc)))))
    box))


(defun convert-evrow-to-editor-row (evrow superior-box)
  (make-row-no-spaces
    (mapcon #'(lambda (list)
		(convert-to-editor-object
		  (get-pointer-value (car list) superior-box)
		  (null (cdr list))))
	    (evrow-pointers evrow))))

(defun convert-to-editor-object (object lastp)
  (cond ((chunk-p object)
	 (let* ((left (chunk-left-format object))
		(pname (chunk-pname object))
		(right (chunk-right-format object))
		(chars (fi-chas left)))
	   (nconc (subseq chars
			  (fi-start left)
			  (fi-stop left))
		  (cond ((box? pname) (list (copy-box pname)))
			((virtual-copy? object)
                         (list (convert-vc-box-to-editor-box pname)))
			(t (subseq chars
				   (fi-start pname)
				   (fi-stop pname))))
		  (if lastp
		      (subseq chars
			      (fi-start right)
			      (fi-stop right))))))
	(t
	 (let ((result (cond ((or (numberp object)
                                  (symbolp object)
                                  (characterp object))
			      object)
			     ((virtual-copy? object)
			      (convert-vc-box-to-editor-box object))
			     ((box? object) (copy-box object))
			     (t (error "~S is not a chunk" object)))))
	   (if lastp (list result) (list result #\space))))))

;;; *** We need to rewrite convert-to-editor-object
(defun make-row-no-spaces (list)
  (let* ((new-row (make-initialized-row))
	 (ca (chas-array new-row))
	 (idx 0))
    (dolist (item list)
      (cond ((numberp item) (fast-string-into-chas-array
			     (convert-number-to-string item) ca))
	    ((stringp item) (fast-string-into-chas-array item ca))
	    ((symbolp item) (fast-string-into-chas-array (symbol-name item) ca))
	    ((characterp item) (fast-chas-array-append-cha ca item))
	    ((box? item)
	     (fast-chas-array-append-cha ca item)
	     (set-superior-row item new-row))
	    (t (error "Don't know how to make a row out of ~S" item)))
      (incf idx))
    new-row))

|#

             #+lcl3.0 (case (lcl::extreme-float-p number)
                        (:minus-infinity most-negative-long-float)
                        (:plus-infinity most-positive-long-float)
                        (t number))

;;;;
;;;; FILE: region.lisp
;;;;

;;; The fast track
#|

(defun update-tracking-blinkers (blinkers screen-box start-row stop-row
                      start-x stop-x context-x context-y)
  (let ((remaining-blinkers blinkers)
    (return-blinkers blinkers)
    (sr-x (+ context-x (screen-obj-x-offset screen-box)
                  (slot-value screen-box 'scroll-x-offset)))
    (sr-y (+ context-y (screen-obj-y-offset screen-box)
                  (slot-value screen-box 'scroll-y-offset))))
    (do-self-and-next-sv-contents (screen-row
                   (slot-value screen-box 'screen-rows)
                   start-row)
      (let ((blinker (or (car remaining-blinkers)
             ;; if we have run out, add them onto the
             ;; end of the return-blinkers list
             (let ((new (allocate-region-row-blinker screen-row)))
               (nconc return-blinkers (list new))
               new))))
    ;; next in line...
    ;; do this now because ww may RETURN from the next expression
    (setq remaining-blinkers (cdr remaining-blinkers))
    (let ((bl-y (+ sr-y (screen-obj-y-offset screen-row)))
          (bl-hei (screen-obj-hei screen-row)))
      ;; now handle specific case
      (cond ((and (eq screen-row start-row) (eq screen-row stop-row))
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row)
                        (min start-x stop-x))
                    bl-y
                    (abs (- stop-x start-x)) bl-hei)
         ;; pop out of the loop now that we have gotten to the last row
         (return))
        ((eq screen-row start-row)
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row)
                        start-x)
                    bl-y
                    (- (screen-obj-wid screen-row)
                        start-x)
                    bl-hei))
        ((eq screen-row stop-row)
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row))
                    bl-y stop-x bl-hei)
         (return))
        (t
         (update-blinker-values blinker screen-row
                    (+ sr-x
                        (screen-obj-x-offset screen-row))
                    bl-y
                    (screen-obj-wid screen-row) bl-hei))))))
    ;; if there are any extra blinkers in the list, turn them
    ;; off, we can stop when we have found one that is already turned off
    (dolist (bl remaining-blinkers)
      (cond ((null (region-row-blinker-visibility bl)) (return))
        (t (with-open-blinker (bl #+clx nil)
         (setf (region-row-blinker-visibility bl) nil
               (region-row-blinker-uid bl) nil)))))
    #+clx (bw::display-finish-output bw::*display*)
    return-blinkers))

(defun update-blinker-values (blinker uid x y wid hei)
  (cond ((and (eq uid (region-row-blinker-uid blinker))
          (not (null (region-row-blinker-visibility blinker)))
          (= y (region-row-blinker-y blinker))
          (= hei (region-row-blinker-hei blinker))
          (or (not (= x (region-row-blinker-x blinker)))
          (not (= wid (region-row-blinker-wid blinker)))))
     ;; check for and optimize the common case of moving
     ;; back and forth along the same row
     #+clx
     ;; this stuff really belongs in boxwin-xxx
     (progn
       ;; we know that either the wid or x or both have changed
       (unless (= (region-row-blinker-x blinker) x)
         (%draw-rectangle (abs (- (bw::blinker-x blinker) x))
                  (bw::blinker-height blinker)
                  (min x (bw::blinker-x blinker))
                  (bw::blinker-y blinker)
                  alu-xor (bw::blinker-window blinker)))
       (let ((new-right (+ x wid))
         (old-right (+ (bw::blinker-x blinker)
                (bw::blinker-width blinker))))
         (unless (= new-right old-right)
           (%draw-rectangle (abs (- new-right old-right))
                (bw::blinker-height blinker)
                (min& new-right old-right)
                (bw::blinker-y blinker)
                alu-xor (bw::blinker-window blinker))))
       (setf (region-row-blinker-x blinker) x
         (region-row-blinker-wid blinker) wid)
       )
     #-clx
     (with-open-blinker (blinker #+clx nil)
       (setf (region-row-blinker-x blinker) x
         (region-row-blinker-wid blinker) wid))
     )
     ((or (not (eq uid (region-row-blinker-uid blinker)))
          (null (region-row-blinker-visibility blinker))
          (not (=& x (region-row-blinker-x blinker)))
          (not (=& y (region-row-blinker-y blinker)))
          (not (=& wid (region-row-blinker-wid blinker)))
          (not (=& hei (region-row-blinker-hei blinker))))
      (with-open-blinker (blinker #+clx nil)
        (setf (region-row-blinker-visibility blinker) t
          (region-row-blinker-uid blinker) uid
          (region-row-blinker-x blinker) x
          (region-row-blinker-y blinker) y
          (region-row-blinker-wid blinker) wid
          (region-row-blinker-hei blinker) hei)))))
|#


;;;;
;;;; FILE: sysprims.lisp
;;;;
#+mcl
               (value ,(ecase value-type
                         (:boolean '(ccl::check-box-checked-p di))
                         (:number '(let ((ns (ccl::dialog-item-text di)))
                                     (when (numberstring? ns)
                                       (ignoring-number-read-errors
                                         (read-from-string ns nil nil)))))
                         (:string '(ccl::dialog-item-text di))
                         (:keyword '(ccl::dialog-item-text di))))

; from write-preferences
#+mcl
  (when (probe-file file)
    (ccl::set-mac-file-creator file :BOXR)
    (ccl::set-mac-file-type file :BOXP))

;; no longer needed, perhaps replace with interrupt poll count value
;#+carbon-compat
;(defboxer-preference bu::immediate-sprite-drawing (true-or-false)
;  ((*sprite-drawing-flush?* :boolean
;                            (boxer-eval::boxer-boolean *sprite-drawing-flush?*))
;   #+lwwin graphics #-lwwin graphics-settings
;   ("Should sprite graphics draw immediately with every command")
;   ("Setting this to false speeds up complicated sprite graphics"))
;  (cond ((not (null true-or-false))
;	 (setq *sprite-drawing-flush?* t))
;	(t
;	 (setq *sprite-drawing-flush?* nil)))
;  boxer-eval::*novalue*)

;; obsolete
;(defboxer-preference bu::enable-box-type-toggling-with-mouse (true-or-false)
;  ((*enable-mouse-toggle-box-type?* :boolean
;    (boxer-eval::boxer-boolean *enable-mouse-toggle-box-type?*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Should the mouse be able to toggle the box")
;   ("type by clicking on the type label ?"))
;  (setq *enable-mouse-toggle-box-type?* true-or-false)
;  boxer-eval::*novalue*)

;; removed 5/12/99 this is never encountered in practice now that we have menus
;; on the mouse corners
;(defboxer-preference bu::warn-about-disabled-commands (true-or-false)
;  ((*warn-about-disabled-commands* :boolean
;    (boxer-eval::boxer-boolean *warn-about-disabled-commands*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Should the user be informed when trying a disabled command.  ")
;   ("or should the action do nothing."))
;  (setq *warn-about-disabled-commands* true-or-false)
;  boxer-eval::*novalue*)

;; removed 5/24/01: no one is ever going to know what to do with this,
;; we should eventually replace it with an "animation speed" slider which also
;; controls things like speed of popup mouse doc unscrolling and move-to-bp
;(defboxer-preference bu::zoom-pause ((boxer-eval::numberize seconds))
;  ((*move-bp-zoom-pause-time* :number *move-bp-zoom-pause-time*)
;   #+lwwin editor #-lwwin editor-settings
;   ("How many seconds pause should there be between steps while")
;   ("zooming to a place (e.g., the target of a port)")
;   ("Setting to 0 will disable animation."))
;  (setq *move-bp-zoom-pause-time* seconds)
;  boxer-eval::*novalue*)

; removed 2/22/97
;(defboxer-preference bu::lock-all-closets (true-or-false)
;  ((*lock-all-closets* :boolean (boxer-eval::boxer-boolean *lock-all-closets*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Should all closets start out being locked"))
;  (setq *lock-all-closets* true-or-false)
;  (force-redisplay)
;  boxer-eval::*novalue*)

;; removed 9/14/98
;; used to be only-shrink-wrap-text-boxes
;(defboxer-preference bu::disable-box-resizing (true-or-false)
;  ((*only-shrink-wrap-text-boxes* :boolean (boxer-eval::boxer-boolean
;                                            *only-shrink-wrap-text-boxes*))
;   #+lwwin editor #-lwwin editor-settings
;   ("Do not allow the size of text boxes to be fixed. If TRUE,")
;   ("boxes will automatically stretch to fit the contents"))
;  (setq *only-shrink-wrap-text-boxes* true-or-false)
;  boxer-eval::*novalue*)

;;turned off for now
;(defboxer-preference bu::slow-graphics-toggling (true-or-false)
;  ((*slow-graphics-toggle* :boolean (boxer-eval::boxer-boolean *slow-graphics-toggle*))
;   editor-settings
;   ("Require the user to confirm graphics toggling by holding the mouse")
;   ("button down for a small interval before the action will take place"))
;  (setq *slow-graphics-toggle* true-or-false)
;  boxer-eval::*novalue*)

;; removed 5/12/99 at andy's suggestion
;(defboxer-preference bu::only-scroll-current-box (true-or-false)
;  ((*only-scroll-current-box?* :boolean (boxer-eval::boxer-boolean *only-scroll-current-box?*))
;   editor-settings
;   ("Limit the ability to scroll the contents to the current box"))
;  (setq *only-scroll-current-box?* true-or-false)
;  (force-redisplay)
;  boxer-eval::*novalue*)

#| ;; removed 9/08/02
(defboxer-preference bu::fullscreen-window (true-or-false)
  ((bw::*fullscreen-window-p* :boolean
                              (boxer-eval::boxer-boolean bw::*fullscreen-window-p*))
   #+lwwin editor #-lwwin editor-settings
   ("Should the boxer window occupy the entire screen ?"))
  (setq bw::*fullscreen-window-p* true-or-false)
  boxer-eval::*novalue*)
|#

;; removed 5/12/99 at andy's suggestion
;(defboxer-preference bu::enable-egc (true-or-false)
;   ((*egc-enabled?* :boolean (boxer-eval::boxer-boolean *egc-enabled?*))
;    #+lwwin evaluator #-lwwin evaluator-settings
;    ("Should the Ephemeral Garbage Collector be turned on ?"))
;  (let ((new-value true-or-false))
;    (setq *egc-enabled?* new-value)
;    #+ccl (ccl::egc new-value)
;    #+lucid (if new-value (lcl::egc-on) (lcl::egc-off))
;    )
;  boxer-eval::*novalue*)

;; what about ALL bitmap ops...

;; removed 5/12/99 at andy's suggestion
;#+mcl
;(defboxer-preference bu::use-fast-bitmap-loaders (true-or-false)
;  ((*use-mac-fast-bitmap-loaders* :boolean
;                                  (boxer-eval::boxer-boolean
;                                   *use-mac-fast-bitmap-loaders*))
;   File-System-Settings
;   ("Use the experimental (may crash your machine) fast bitmap operations ?"))
;  (setq *use-mac-fast-bitmap-loaders* true-or-false)
;  boxer-eval::*novalue*)

;; not currently use
#|
(defboxer-preference bu::max-viewable-message-size ((boxer-eval::numberize length))
  ((boxnet::*max-viewable-message-length*
    :number boxnet::*max-viewable-message-length*)
   #+lwwin network #-lwwin network-settings
   ("What is the maximum size mail message that will appear in Boxer ?"))
  (setq boxnet::*max-viewable-message-length* length)
  boxer-eval::*novalue*)
|#

;;; temporary
#|
#+lwwin
(defboxer-preference bu::draw-icon-options ((boxer-eval::numberize new-option))
                     ((*windows-draw-icon-options* :number
                                                   *windows-draw-icon-options*)
                      #+lwwin temporary
                      ("Try different draw-icon display routines")
                      ("Should be an interger between 0 and 15"))
  (when (and (integerp new-option) (<=& 0 new-option 15))
    (setq  *windows-draw-icon-options* new-option))
  boxer-eval::*novalue*)
|#

;;;;
;;;; FILE: vars.lisp
;;;;

#|
;;; FOR-EACH-ITEM
(define-eval-var *for-each-item-variable-object* :global nil)
(define-eval-var *for-each-item-counter* :global nil)
(define-eval-var *for-each-item-length* :global nil)
(define-eval-var *for-each-item-box* :global nil)
(define-eval-var *for-each-item-list* :global nil)

;;; FOR-EACH-BOX
(define-eval-var *for-each-box-variable-object* :global nil)
(define-eval-var *for-each-box-counter* :global nil)
(define-eval-var *for-each-box-length* :global nil)
(define-eval-var *for-each-box-box* :global nil)
(define-eval-var *for-each-box-list* :global nil)

;;; FOR-EACH-ROW
(define-eval-var *for-each-row-variable-object* :global nil)
(define-eval-var *for-each-row-counter* :global nil)
(define-eval-var *for-each-row-length* :global nil)
(define-eval-var *for-each-row-box* :global nil)
(define-eval-var *for-each-row-list* :global nil)
|#

;;;;
;;;; FILE: vrtdef.lisp
;;;;

;; sgithens This doesn't seem to be used anywhere... what do we currently look at to determine if eval is in progress???
(DEFVAR *EVAL-IN-PROGRESS* NIL
  "Bound by top level eval functions and used by the :MODIFIED message to
   decide when to flush old eval structure. ")
