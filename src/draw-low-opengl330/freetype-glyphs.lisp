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
;;;;   Utilities and datastructures for individual font glyphs.

(in-package :boxer)

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
      (setf (box-glyph-texture-id togo) (create-glyph-texture width rows buffer)))

    togo))

(defun create-glyph-texture (width rows buffer)
  "Returns the integer id for a new texture from freetype fontface for `ch`.
   Needs to be run inside an active openGL context."
  (let ((glyph-texture   (gl:gen-texture)))
    (gl:pixel-store :unpack-alignment 1)
    (gl:bind-texture :texture-2d glyph-texture)
    (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

    (gl:tex-image-2d
      :texture-2d 0 :red
      width
      rows
      0 :red :unsigned-byte
      buffer)
    (gl:generate-mipmap :texture-2d)

    (gl:bind-texture :texture-2d 0)
  (log:debug "~%glyph-texture: ~A  mp: ~A" glyph-texture mp:*current-process*)
  glyph-texture))
