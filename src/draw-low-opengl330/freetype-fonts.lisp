;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                        +-Data--+
;;;;               This file is part of the | BOXER | system
;;;;                                        +-------+
;;;;
;;;;
;;;;   Utilities for Rendering Fonts using Freetype2.

(in-package :boxer)

(defparameter *freetype-faces* nil
  "Hash table of freetype face instances.")

(defparameter *freetype-pixmap-generate-count* 0
  "Number of times we've generaged a pixmap for freetype rendering.  For performance tracking.")

(defparameter *freetype-pixmap-cache* nil
  "Hash table of cached pixmaps for drawing. The keys are font descriptions, and the values are box-glyph
  structs.

  Example entry for letter D at 100% scale in size 12 Arial font:
      Key:   ((\"Arial\" 12) #\\D 1.0)
      Value: #S(BOXER::BOX-GLYPH :CH #\\D :WIDTH 9 :ROWS 9 :BEARING-X 0 :BEARING-Y 9 :ADVANCE 9.0 :TEXTURE-ID 6)
  ")

(defun make-freetype-face (font-file)
  (freetype2::new-face (merge-pathnames *capogi-font-directory* font-file)))

(defun load-freetype-faces ()
  "Load initial set of fonts"
  (setf *freetype-faces* (make-hash-table :test #'equal))
  (setf *freetype-pixmap-cache* (make-hash-table :test #'equal))
  (setf (gethash "LiberationSans-Regular" *freetype-faces*) (make-freetype-face "LiberationSans-Regular.ttf"))
  (setf (gethash "LiberationSans-Bold" *freetype-faces*) (make-freetype-face "LiberationSans-Bold.ttf"))
  (setf (gethash "LiberationSans-Italic" *freetype-faces*) (make-freetype-face "LiberationSans-Italic.ttf"))
  (setf (gethash "LiberationSans-BoldItalic" *freetype-faces*) (make-freetype-face "LiberationSans-BoldItalic.ttf"))

  (setf (gethash "LiberationMono-Regular" *freetype-faces*) (make-freetype-face "LiberationMono-Regular.ttf"))
  (setf (gethash "LiberationMono-Bold" *freetype-faces*) (make-freetype-face "LiberationMono-Bold.ttf"))
  (setf (gethash "LiberationMono-Italic" *freetype-faces*) (make-freetype-face "LiberationMono-Italic.ttf"))
  (setf (gethash "LiberationMono-BoldItalic" *freetype-faces*) (make-freetype-face "LiberationMono-BoldItalic.ttf"))

  (setf (gethash "LiberationSerif-Regular" *freetype-faces*) (make-freetype-face "LiberationSerif-Regular.ttf"))
  (setf (gethash "LiberationSerif-Bold" *freetype-faces*) (make-freetype-face "LiberationSerif-Bold.ttf"))
  (setf (gethash "LiberationSerif-Italic" *freetype-faces*) (make-freetype-face "LiberationSerif-Italic.ttf"))
  (setf (gethash "LiberationSerif-BoldItalic" *freetype-faces*) (make-freetype-face "LiberationSerif-BoldItalic.ttf")))

(defun check-fontspec (triple)
  "While we are continuing to refactor fonts in the system, sometimes the font size comes in as zero,
  for now we will reset it to 12."
  (if (equal (cadr triple) 0)
    (list (car triple) 12 (cddr triple))
    triple)) ;; TODO We should make this also capitalize the font and make sure the zoom is 1.0... hmm this is only the font triple...

(defun font-face-from-fontspec (capi-fontspec &optional (font-zoom 1.0))
  "Returns the freetype2 font face and sets the current char size from a fontspec of the
  type '(\"Arial\" 12 :BOLD)"
  (let* ((name (car capi-fontspec))
         (size (cadr capi-fontspec))
         (style (cddr capi-fontspec))
         (face-family-name nil) ; ie. LiberationSerif
         (face-style-name nil) ; ie. Regular, Bold, Italic, BoldItalic
         (face-full-name) ; ie. LiberationSans-Bold
         (face nil))
    (cond ((equal name "Times New Roman")
           (setf face-family-name "LiberationSerif"))
          ((equal name "Courier New")
           (setf face-family-name "LiberationMono"))
          (t
           (setf face-family-name "LiberationSans")))
    (cond ((and (member :BOLD style) (member :ITALIC style))
           (setf face-style-name "BoldItalic"))
          ((member :BOLD style)
           (setf face-style-name "Bold"))
          ((member :ITALIC style)
           (setf face-style-name "Italic"))
          (t
           (setf face-style-name "Regular")))
    (setf face (gethash (concatenate 'string face-family-name "-" face-style-name) *freetype-faces*))
    ;; floor'ing the result as even though freetype specifies decimal font sizes right now, our current
    ;; opengl methods require whole numbers
    (freetype2:set-char-size face (floor (* font-zoom (* size 64))) 0 72 72)
    face))

(defun current-freetype-font (current-font &optional (font-zoom 1.0))
  "Returns the current face and size based on the values in *current-font-descriptor*
  The current fonts are:
      'Arial' 'Courier New' 'Times New Roman' 'Verdana'
  The style numbers currently correspond to:
      0 - Plain    1 - Bold     2 - Italic      3 - BoldItalic
  The current font sizes are:
      9, 10, 12, 14, 16, 20, 24
  "
  (font-face-from-fontspec (check-fontspec (opengl-font-fontspec current-font)) font-zoom))

(defun find-box-glyph (ch current-font &optional (font-zoom 1.0))
  "Returns a box-glyph which includes the openGL texture id for rendering. Needs to cache glyphs based on the following:
   CAPI Font List, Char/String
  "
  (let* ((capi-fontspec (check-fontspec (opengl-font-fontspec current-font)))
         (font-face (current-freetype-font current-font (coerce font-zoom 'single-float)))
         (cache-key `(,capi-fontspec ,ch ,(coerce font-zoom 'single-float)))
         (cached-glyph (gethash cache-key *freetype-pixmap-cache*)))
    (unless cached-glyph
      (setf cached-glyph (create-box-glyph font-face ch))
      (setf (gethash cache-key *freetype-pixmap-cache*) cached-glyph)
      (setf *freetype-pixmap-generate-count* (1+ *freetype-pixmap-generate-count*)))
    cached-glyph))

(defun create-box-glyph (font-face ch &optional (create-texture? nil))
  "Takes a freetype2 font face and the character code to generate.
  Returns a box-glyph struct."
  (freetype2:load-char font-face ch)
  (let* ((togo      (make-box-glyph))
         ;; This call to get-advance must happen either before we call render-glyph, as it mucks
         ;; with the glyph buffers. (It could also be called as the very last thing.)
         (advance   (freetype2::get-advance font-face ch))
         (glyphslot (freetype2:render-glyph font-face))
         (bitmap    (freetype2::ft-glyphslot-bitmap glyphslot))
         (width     (freetype2::ft-bitmap-width bitmap))
         (rows      (freetype2::ft-bitmap-rows bitmap))
         (buffer    (freetype2::ft-bitmap-buffer bitmap))
         (bearing-x (freetype2::ft-glyphslot-bitmap-left glyphslot))
         (bearing-y (freetype2::ft-glyphslot-bitmap-top glyphslot))
         (texture-id nil))
    (setf (box-glyph-ch togo) ch)
    (setf (box-glyph-width togo) width)
    (setf (box-glyph-rows togo) rows)
    (setf (box-glyph-bearing-x togo) bearing-x)
    (setf (box-glyph-bearing-y togo) bearing-y)
    (setf (box-glyph-advance togo) advance)

    (if create-texture?
      (setf (box-glyph-texture-id togo) (create-glyph-texture font-face ch glyph)))

    togo))

(defvar *gl-glyph-texture-count* 0)

(defun create-glyph-texture (font-face ch glyph)
  "Returns the integer id for a new texture from freetype fontface for `ch`.
   Needs to be run inside an active openGL context."
  ;; TODO Refactor this, so we're not rendering the glyph twice. if the buffer is saved on the
  ;; glyph it gets all jittery. We need to be able to create a glyph without the GL texture since
  ;; some widths need to be calculated when an openGL context is not available.
  (freetype2:load-char font-face ch)
  (let* ((glyph-texture   (gl:gen-texture))
         (glyphslot (freetype2:render-glyph font-face))
         (bitmap    (freetype2::ft-glyphslot-bitmap glyphslot))
         (buffer    (freetype2::ft-bitmap-buffer bitmap)))
    (gl:pixel-store :unpack-alignment 1)
    (gl:bind-texture :texture-2d glyph-texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d
      :texture-2d 0 :red
      (box-glyph-width glyph)
      (box-glyph-rows glyph)
      0 :red :unsigned-byte
      buffer)
    (gl:generate-mipmap :texture-2d)

    (gl:bind-texture :texture-2d 0)
    (incf *gl-glyph-texture-count*)
  #+lispworks (log:debug "~%glyph-texture: ~A  mp: ~A" glyph-texture (mp:get-current-process))
  glyph-texture))

;;;
;;; Glyph Texture Atlas
;;;

(defparameter *freetype-glyph-atlas* nil)

(defclass glyph-atlas ()
  ((texture-id :initform 0 :initarg :texture-id :accessor glyph-atlas-texture-id
    :documentation "The integer for the openGL texture this atlas lives on.")
   (width :initform 0 :initarg :width :accessor glyph-atlas-width
    :documentation "Width of the atlas (pixels)")
   (height :initform 0 :initarg :height :accessor glyph-atlas-height
    :documentation "Height of the atlas (pixels)")
   (glyphs :initform (make-hash-table :test #'equal) :accessor glyph-atlas-glyphs
    :documentation "Table of glyphs in the atlas, keyed by font descriptor, char, and scale: '((\"Arial\" 12) #\\D 1.0)
                   Values are of type struct box-glyph which should have the atlas locations filled in.
                   ")
   (sizes :initform '(8 9 10 11 12 14 16 18 20 22 24 26 28 36 48 72 288) :initarg :sizes :accessor glyph-atlas-sizes
    :documentation "A list of integers containing the font/glyphs sizes we pre-rendered on the atlas.")))

(defmethod get-glyph ((self glyph-atlas) spec)
  "
  Fetches a box-glyph capable of rendering a glyph from the texture atlas. If the atlas does not have the specified
  character in the font, then nil is returned. The texture on the atlas may be scaled down if the exact size is not
  on the atlas. (ie. We'll pick the next largest size to scale on to the actual font size)

  The spec is a list of font-spec, character, and zoom level.
  Spec example is: '((\"Arial\" 12 :BOLD) #\A 1.0)
  "
  (let* ((final-glyph nil)
         (cha (second spec))
         (font-spec (car spec))
         (capitalized-font-name (string-upcase (car font-spec)))
         (capitalized-font-spec (cons capitalized-font-name (cdar spec)))
         (capitalized-spec (cons capitalized-font-spec (cdr spec))))
    ;; We're going to find a glyph to return, assuming the char in this font appears in at least some size on the
    ;; altas. (1) if it's already cached in the atlas glyphs we'll return it. (2) We'll multiply the font by the
    ;; zoom to get it's size, and find the closest size on the atlas. If that also doesn't exist, the glyph/font
    ;; must not be there at all. (in the future we hope to add new chars to the atlas at runtime)
    ;; If that atlas-glyph does exist, we also get the exact sized freetype-glyph, and then add the texture
    ;; coordinates and dimensions from the atlas-glyph to it. This is what we then add to the atlas glyph hash
    ;; and return.  These are all of type box-glyph.

    ;; 1. Does the texture atlas have this glyph already?
    (when (gethash  capitalized-spec (glyph-atlas-glyphs self))
      (log:debug "~%Got capitalized-spec glyph from atlas: ~A" capitalized-spec)
      (setf final-glyph (gethash  capitalized-spec (glyph-atlas-glyphs self))))

    (unless final-glyph
      ;; 2 Get the real font size taking in to account the zoom level
      (let* ((zoom (third spec))
             (zoomed-font-size (* zoom (second font-spec)))
             (closest-size (closest-font-size self zoomed-font-size))
             (atlas-key (list (cons capitalized-font-name (cons closest-size (cddr font-spec))) cha 1.0))
             (atlas-glyph (gethash  atlas-key (glyph-atlas-glyphs self))))
        (when atlas-glyph ;; If this there isn't a glyph for the char any the closest zoom level, we still have to return nil
          (let* ((font-face (font-face-from-fontspec capitalized-font-spec zoom))
                 (freetype-glyph (create-box-glyph font-face cha nil)))
            ;; Set the texture bits of freetype-glyph from the atlas-glyph, store in the atlas with the
            ;; original zoom level and set as the final-glyph.
            (setf (box-glyph-tx freetype-glyph) (box-glyph-tx atlas-glyph)
                  (box-glyph-ty freetype-glyph) (box-glyph-ty atlas-glyph)
                  (box-glyph-t-width freetype-glyph) (box-glyph-t-width atlas-glyph)
                  (box-glyph-t-rows freetype-glyph) (box-glyph-t-rows atlas-glyph))
            (setf (gethash capitalized-spec (glyph-atlas-glyphs self)) freetype-glyph)
            (setf final-glyph freetype-glyph)))))
    final-glyph))

(defun closest (n items)
  (cond ((null items)
          nil)
        ((null (cadr items))
          (car items))
        ((<= n (car items))
          (car items))
        (t
          (closest n (cdr items)))))

(defmethod closest-font-size ((self glyph-atlas) size)
  "Returns the closest largest size font size. So we return either the same size if it's on the atlas, or
   the next largest size so it can be sized down. If a size is larger than the largest size, we return the
   largest size."
  (closest size (glyph-atlas-sizes self)))

(defun pre-render-font-to-atlas (atlas font-spec font-zoom start-x start-y)
  "For the given font-face render the first 32-128 characters starting at the x and y location.
  Returns the ending x/y positions as a list '(x y). This is so if you want to continue adding more
  glyphs at the end of the font for higher unicode points they can be appended along with the
  existing font glyphs.

  Assumes the atlas textures are bound and set up for glTexImage2d usage.

  Example usage:
  (pre-render-font-to-atlas atlas '(\"Arial\" 12 :BOLD) 1.0 0 12)
  (pre-render-font-to-atlas atlas '(\"Arial\" 16 :BOLD) 1.0 0 40)
  "
  (let ((max-width 0)
        (font-face (font-face-from-fontspec font-spec font-zoom)))
    (loop with x = start-x
          with y = start-y
          for i from 32 to 128 do (progn
      (freetype2:load-char font-face (code-char i))
      (let* ((cache-key `(,font-spec ,(code-char i) ,(coerce font-zoom 'single-float)))
             (advance   (freetype2::get-advance font-face (code-char i)))
             (glyphslot (freetype2:render-glyph font-face))
             (bitmap    (freetype2::ft-glyphslot-bitmap glyphslot))
             (width     (freetype2::ft-bitmap-width bitmap))
             (rows      (freetype2::ft-bitmap-rows bitmap))
             (buffer    (freetype2::ft-bitmap-buffer bitmap))
             (bearing-x (freetype2::ft-glyphslot-bitmap-left glyphslot))
             (bearing-y (freetype2::ft-glyphslot-bitmap-top glyphslot))
             (tx        (/ x (glyph-atlas-width atlas)))
             (ty        (/ y (glyph-atlas-height atlas)))
             (glyph (make-box-glyph :ch (code-char i) :width width :rows rows :bearing-x bearing-x :bearing-y bearing-y
                                    :tx tx :ty ty :advance advance :t-width width :t-rows rows)))
        (gl:tex-sub-image-2d :texture-2d 0 x y width rows :red :unsigned-byte buffer)
        (setf (gethash cache-key (glyph-atlas-glyphs atlas)) glyph)
        (setf x (+ x width)))
        (setf max-width x)))
    (log:debug "Final texture width for ~A ~A is ~A" font-spec font-zoom max-width)))

(defmethod make-glyph-atlas (&key (atlas-width 16000) (atlas-height 16000))
  "Create a new glyph atlas, allocating an openGL texture, and prefilling our default font sizes with the ASCII
  character set. This must be run inside an active openGL context.

  References: https://en.wikibooks.org/wiki/OpenGL_Programming/Modern_OpenGL_Tutorial_Text_Rendering_02"

  (let* ((texture     (gl:gen-texture))
         (atlas       (make-instance 'glyph-atlas :texture-id texture :width atlas-width :height atlas-height)))
    ;; create a texture for the entire atlas
    #-win32 (progn
    (gl:pixel-store :unpack-alignment 1)
    (gl:active-texture :texture0)
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :red atlas-width atlas-height 0 :red :unsigned-byte (cffi:null-pointer))
    (gl:generate-mipmap :texture-2d)

    ;; add some glyphs
    (let ((count 0))
      ;; Upper casing font names, since they can vary from boxer storage
      (dolist (font-family '("ARIAL" "COURIER NEW" "TIMES NEW ROMAN"))
        (dolist (font-size (glyph-atlas-sizes atlas))
          (pre-render-font-to-atlas atlas `(,font-family ,font-size) 1.0 0 count)
          (setf count (+ count font-size))
          (pre-render-font-to-atlas atlas `(,font-family ,font-size :BOLD) 1.0 0 count)
          (setf count (+ count font-size))
          (pre-render-font-to-atlas atlas `(,font-family ,font-size :ITALIC) 1.0 0 count)
          (setf count (+ count font-size))
          (pre-render-font-to-atlas atlas `(,font-family ,font-size :BOLD :ITALIC) 1.0 0 count)
          (setf count (+ count font-size)))))

    ;; return the new atlas
    (gl:bind-texture :texture-2d 0))
    atlas))

(defmethod draw ((self glyph-atlas))
  "Debugging/inspection utility to just draw the entire glyph atlas texture on the screen."
  (enable-gl-shader-program bw::*boxgl-device* (ft-glyph-shader bw::*boxgl-device*))
  (gl:pixel-store :unpack-alignment 1) ;; is this really necessary when just drawing?
  (gl:bind-texture :texture-2d (glyph-atlas-texture-id self))
  (gl:active-texture :texture0)
  (let* ((tx 0)
         (ty 200)
         (wid (glyph-atlas-width self))
         (hei (glyph-atlas-height self))
         (vertices (float-vector tx         ty         0.0 0.0 ;; 0.0 1.0
                                 tx         (+ ty hei) 0.0 1.0 ;; 0.0 0.0
                                 (+ tx wid) ty         1.0 0.0 ;; 1.0 1.0
                                 (+ tx wid) (+ ty hei) 0.0 1.0 ;; 1.0 0.0
                                 tx         (+ ty hei) 1.0 1.0 ;; 0.0 0.0
                                 (+ tx wid) ty         1.0 0.0 ;; 1.0 1.0
                                 ))
          (arr (gl:alloc-gl-array :float (length vertices))))
    (dotimes (i (length vertices))
      (setf (gl:glaref arr i) (aref vertices i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)
    (gl:draw-arrays :triangles 0 6))
  (unenable-shader-programs device))

(defmethod glyph-count ((self glyph-atlas))
  "Returns the number of glyphs currently stored in the atlas."
  (hash-table-count (glyphs self)))
