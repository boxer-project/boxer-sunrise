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
  "Hash table of cached pixmaps for drawing.")

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
    triple))

(defun current-freetype-font (current-font &optional (font-zoom 1.0))
  "Returns the current face and size based on the values in *current-font-descriptor*
  The current fonts are:
      'Arial' 'Courier New' 'Times New Roman' 'Verdana'
  The style numbers currently correspond to:
      0 - Plain    1 - Bold     2 - Italic      3 - BoldItalic
  The current font sizes are:
      9, 10, 12, 14, 16, 20, 24
  "
  (let* ((capi-fontspec (check-fontspec (opengl-font-fontspec current-font)))
         (name (car capi-fontspec))
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

(defstruct box-glyph
  ch
  width
  rows
  bearing-x
  bearing-y
  advance
  texture-id
  buffer)


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
  (log:debug "~%glyph-texture2: ~A  mp: ~A" glyph-texture (mp:get-current-process))
  glyph-texture))
