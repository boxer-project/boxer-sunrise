;;;;
;;;; The attic
;;;;

;;;;
;;;; FILE: boxwin-opengl.lisp
;;;;

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
;;;; FILE: mouse.lisp
;;;;

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
