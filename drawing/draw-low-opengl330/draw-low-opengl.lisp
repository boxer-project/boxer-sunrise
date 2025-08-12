;;;;
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
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;   This file contains the low level window system dependent interface to
;;;;   the OpenGL library calls.
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   4/23/14 added string-ascent
;;;;   3/20/14 changed *font-families*: moved "arial" to the default and added "verdana"
;;;;  11/09/13 added ("Helvetica" . "Arial") to *font-family-aliases*
;;;;  10/16/13 make-boxer-font-{ogl,capogi}
;;;;   9/25/13 fontspec->font-values,
;;;;   8/02/13 removed warnings
;;;;   7/26/13 Relative Font size utilities: *font-size-baseline*, *bfd-font-size-names*,
;;;;           bfd-font-size-name, %font-size-to-idx, *font-sizes*,  font-size-to-idx,
;;;;           %font-size-idx-to-size, find-cached-font, cache-font, font-size,
;;;;           init-bootstrapping-font-vars, fill-bootstrapped-font-caches
;;;;   1/23/13 functions which use pixblt must convert to fixnums before calling
;;;;  11/21/12 find-filled-font = find-cached-font + bw::ensure-oglfont-parameters
;;;;           used by string-{wid,hei}
;;;;   9/15/12 my-clip-rect now has to coerce its args to fixnums
;;;;   8/27/12 functions with explicit calls to opengl prims now refer to another layer of prims
;;;;           in opengl-utils.lisp which can (optionally) type check and/or coerce
;;;;           %draw-line, %draw-rect, %set-pen-size,
;;;;   3/ 6/11 added *boxtop-text-font* init to init-bootstrapping-font-vars
;;;;   2/28/11 changed with-system-dependent-bitmap-drawing to use the back buffer instead of an aux buffer
;;;;   2/15/11 fill-bootstrapped-font-caches fills for ALL glut fonts
;;;;   2/13/11 string-{wid,hei} now use bw::with-ogl-font to insure fonts are filled
;;;;   2/ 6/11 auxiliary-buffer-{count,exists?}, with-system-dependent-bitmap-drawing
;;;;   4/27/10 added ogl-reshape to with-drawing-port
;;;;   4/20/10 %draw-circle %draw-arc using new opengl-draw-xxx functions
;;;;   4/17/10 font caching mechanism changed: make-boxer-font, fill-bootstrapped-font-caches,
;;;;           cache-on-startup?
;;;;  12/13/09 copy-image-to-bitmap & uncolor->pixel utilities for converting images (from capi:clipboard)
;;;;           to OpenGL bitmaps
;;;;  11/27/09 Added *closet-color* and it's init's
;;;;  11/13/09 Added screen-pixel-color & offscreen-pixel-color to sharpen distinction between pixels & colors
;;;;           which have become muddled in higher level code
;;;;   9/05/09 maintaining-drawing-font
;;;;   3/13/09 boxer-points->window-system-points for OpenGL
;;;;   2/22/09 copy-offscreen-bitmap
;;;;   3/02/08 pixel-dump-value handles possible non 1.0 alpha values
;;;;   1/13/08 my-clip-rect, x & y off by 1
;;;;   5/22/07 border colors added:*default-border-color*, *border-gui-color*
;;;;   GPU configuration: parameters and initialization
;;;;   5/13/06 prelim version done
;;;;   1/23/05 added flush-port-buffer stub
;;;;   1/04/05 %erase-rectangle
;;;;   3/01/02 fixed bug in %draw-xor-string
;;;;   2/22/02 font aliasing
;;;;   2/19/02 %font-name-to-idx, fontspec->font-no, make-boxer-font
;;;;   2/17/02 added pixel-dump-value-internal used by dump-true-color-pixmap for
;;;;           platform independence of pixels in offscreen pixmaps
;;;;   2/16/02 %draw-xor-string makes sure we have plusp wid & hei before blitting
;;;;   1/12/02 %draw-xor-string now hacks clipping
;;;;  10/10/01 %draw-string window arg changed to %drawing-array to handle bitmap binding
;;;;  10/06/01 pixel-dump-value now swaps Red and Blue bytes
;;;;   9/06/01 %set-image-pixel no longer needs to coerce to a colorref
;;;;   5/08/01 %draw-rectangle checks clipping state for unusual cases which don't seem
;;;;           to be handled by the window system
;;;;   4/30/01 %bitblt functions now do clipping in software because pixblt doe'sn't
;;;;           pay attention to the mask
;;;;   2/26/01 %draw-point, %draw-poly XOR case :foreground changed to #xffffff
;;;;   2/20/01 fixed %draw-arc & %draw-filled-arc
;;;;   2/13/01 merged current LW and MCL files (draw-low-mcl.lisp)
;;;;   2/13/01 maintaining-drawing-font folded into rebind-font-info
;;;;   2/04/01 fixed typos in %draw-arc, %draw-filled-arc & draw-point
;;;;  11/01/00 with-system-dependent-bitmap-drawing now binds %graphics-state
;;;;           so (current-graphics-state) can win
;;;;  10/04/00 changed image-pixel to return a fixnum instead of a win32::colorref
;;;;           because that is what the dumping functions expect
;;;;           %set-image-pixel also changed
;;;;   8/22/00 added %make-color-from-bytes/100s
;;;;   7/09/00 defined %get-pixel using gp:get-point and ported pixel handling functions
;;;;           :foreground #xffffff for XOR ops (was #xffff) 5/31/00 relaxed %font-size-to-idx to handle values (like 1) actually found in files
;;;;   5/08/00 fixed deferred my-clip-rect
;;;;   5/03/00 fixed X/Y reversal in boxer-points->window-system-points
;;;;   3/24/00 make-boxer-font changed to return encoded numeric value instead of
;;;;           internal system font
;;;;   2/02/00 added full font caching to fill-bootstrapped-font-caches
;;;;  12/15/99 added 7 to %font-size-to-idx to support loadingof old mac files
;;;;  11/29/99 fixed string-wid & string-hei: documentation for gp:get-string-extent had
;;;;           the order of returned values wrong (pg 136)
;;;;  11/16/99 %erase-rectangle changed to use gp::draw-rectangle because gp::clear-rectangle
;;;;           ignores the offset state of the graphics state's transform which causes it to
;;;;           lose for inferior boxes during redisplay
;;;;   3/29/99 new fonts finished ??
;;;;  11/02/98 Copied from draw-low-mcl.lisp and reduced to function & args stubs
;;;;           for conversion to CAPI, GP lispworks
;;;;
;;;;  notes:: check points arg on draw-poly
;;;;          search for &&& which marks stubs
;;;;

(in-package :boxer-opengl)

(defun opengl-enables ()
  "Run our standard set of openGL enables and disables."
  ;; (gl:enable :scissor-test)
  (gl:enable :line-smooth)
  (gl:enable :polygon-smooth)
  (gl:enable :blend)
  (gl:enable :multisample)
  ;; We need to change our painting algorithm, lest enabling depth test just displays
  ;; a totally white canvas.
  (gl:disable :depth-test)
  (gl:enable :stencil-test)
  (gl:stencil-func :notequal 1 #xFF)
  (gl:stencil-op :keep :keep :replace)
  (ignore-stencil)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:hint :line-smooth-hint :nicest)
  (gl:hint :polygon-smooth-hint :nicest))

(defun %clear-window (color)
  (gl::clear-color (aref color 1) (aref color 2) (aref color 3) 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))

(defun %flush-port-buffer (&optional (pane *boxer-pane*))
  (update-framerate)
  #+lispworks (opengl:rendering-on (pane)
    (opengl::swap-buffers pane)))


(defun %string-wid (font-no string)
  (let ((font (find-cached-font font-no)))
    (if (null font)
      (error "No cached font for ~X" font-no)
      (with-ogl-font (font)
                         (if (symbolp string)
                           (ogl-string-width (string string) font)
                           (ogl-string-width string font))))))

(defun %string-hei (font-no)
  (let ((font (find-cached-font font-no)))
    (if (null font)
      (error "No cached font for ~X" font-no)
      (with-ogl-font  (font)
                          ;; most of the drawing code requires this to be a fixnum, thus the floor
                          (floor (ogl-font-height font))))))

(defun %string-ascent (font-no)
  (let ((font (find-cached-font font-no)))
    (if (null font)
      (error "No cached font for ~X" font-no)
      (with-ogl-font  (font)
                          ;; most of the drawing code requires this to be a fixnum, thus the floor
                          (floor (ogl-font-ascent font))))))

(defun ogl-char-width (cha &optional (font *current-opengl-font*))
  (let* ((glyph (find-box-glyph cha font boxer::*font-size-baseline*))
         (advance (box-glyph-advance glyph)))
    ;; A number of our fixnum operations will fail if this isn't an integer.
    (floor advance)))

;; proportional fonts
(defun %cha-wid (char)
  (ogl-char-width char))

(defun ogl-string-width (string &optional (font *current-opengl-font*))
  (let ((total 0))
    (for:for ((i over string))
      (setf total (+ total (ogl-char-width i))))
    total))

(defun ogl-reshape (width height)
  (setf (boxgl-device-projection-matrix bw::*boxgl-device*)
        (create-ortho-matrix width height))
  (gl:viewport 0 0 width height))

;;;; COLOR (incomplete)

;;; neccessary but not sufficient...
;; (Defun color? (thing) #+lispworks (typep thing 'opengl::gl-vector))

(defun %set-pen-color (color)
  "This expects either an already allocated GL 4 vector that represents a color, an RGB percentage vector
  of the form #(:RGB 0.0 0.0 1.0) (for blue), or a number that corresponds to an unsigned 32-bit integer
  with the RGBA values."
  (cond ((integerp color)
         (setf (boxgl-device-pen-color bw::*boxgl-device*)
               `#(:rgb
                       ,(/ (ldb (byte 8 16) color) 255)
                       ,(/ (ldb (byte 8 8) color) 255)
                       ,(/ (ldb (byte 8 0) color) 255)
                       1.0) ;; sgithens TODO I'm not sure we support alpha pen colors yet, may need to increment the box file version
         ))
        ((and (vectorp color) (eq :RGB (aref color 0)))
          (setf (boxgl-device-pen-color bw::*boxgl-device*) color))
        ;; ((color? color)
        ;;   (setf (boxgl-device-pen-color bw::*boxgl-device*) (bw::ogl-color->rgb color)))

        (t
         (error "Bad color passed to %set-pen-color: ~A " color))))

;;;
;;; Drawing functions
;;;

(defun %draw-c-arc (x y radius start-angle sweep-angle &optional filled?)
  "circular arc, rather than the more generic elliptical arc
opengl arc drawing routine starts at 3 oclock and sweeps clockwise in radians
also, opengl-draw-arc expects positive angle args"
  (if (minusp sweep-angle)
    ;; change the start so that we can use ABS sweep
    ; (gl-add-arc bw::*boxgl-device* x y radius
    ;                      (* +degs->rads+ (mod (- (+ start-angle sweep-angle) 90) 360))
    ;                      (* +degs->rads+ (abs sweep-angle))
    ;                      filled?)
    ; (gl-add-arc bw::*boxgl-device* x y radius
    ;                      (* +degs->rads+ (mod (- start-angle 90) 360))
    ;                      (* +degs->rads+ sweep-angle)
    ;                      filled?)
    (gl-add-shader-arc bw::*boxgl-device* x y radius
                       (* +degs->rads+ (mod (- (+ start-angle sweep-angle) 90) 360))
                       (* +degs->rads+ (abs sweep-angle))
                       filled?)
    (gl-add-shader-arc bw::*boxgl-device* x y radius
                       (* +degs->rads+ (mod (- start-angle 90) 360))
                       (* +degs->rads+ sweep-angle)
                       filled?)))

(defun %draw-cha (char x y &key (gl-model nil))
  "Font is managed by set-font-info.  Note that anything else that might change
the window font (ie, draw-string) has to change it back for this to work.
5/11/98 draw to baseline instead of top left"
  (if gl-model
      (add-char gl-model bw::*boxgl-device* x y char)
      (gl-add-char bw::*boxgl-device* x y char)))

(defun %draw-circle (x y radius &optional filled?)
  ; (gl-add-circle bw::*boxgl-device* x y radius filled?)
  (gl-add-shader-circle  bw::*boxgl-device* x y radius filled?))

(defun %draw-ellipse (x y width height &optional filled?)
  (gl-add-shader-ellipse bw::*boxgl-device* x y width height filled?))

(defun %draw-line (x0 y0 x1 y1)
  "Low level draw-line.  If we are using the graphics list performance, this will
  copy the points to the c-buffer we are putting vertices in, otherwise it will
  call the old single openGL draw line that uses and glBegin and glEnd.
  "
  (if *cur-gl-model-screen-obj*
    (when (needs-update *cur-gl-model-screen-obj*)
      (add-line *cur-gl-model-screen-obj* bw::*boxgl-device* x0 y0 x1 y1))
    (gl-add-line bw::*boxgl-device* x0 y0 x1 y1)))

(defun %draw-point (x y)
  (gl-add-point bw::*boxgl-device* x y))

(defun %draw-poly (points)
  (gl-add-poly bw::*boxgl-device* points))

(defun %draw-rectangle (width height x y)
  (gl-add-rect bw::*boxgl-device* x y width height))

(defun %draw-string (font string x y)
  ;; The check for *cur-gl-model-screen-obj* happens inside of gl-add-string
  (gl-add-string bw::*boxgl-device* (find-cached-font font) string x y))

(defun %bitblt-to-screen (wid hei from-array fx fy tx ty)
  ;; remember that  we are drawing from the lower left....
  (gl-add-pixmap bw::*boxgl-device* from-array (round tx) (round ty) (round wid) (round hei) fx fy))

(defun %bitblt-from-screen (wid hei pixmap fx fy tx ty)
  ;; bw::gl-read-pixels
  ;; (bw::gl-read-buffer bw::*gl-front*) ; read from the (visible) front buffer
  (%pixblt-from-screen pixmap (round fx)
                                ;; not quite right especially with a translated fy
                                (round (- (viewport-height *boxer-pane*) (+ fy hei)))
                                (round wid) (round hei)))

;;;;
;;;; Boxer bitmaps
;;;;

(defun clear-offscreen-bitmap (bm &optional (clear-color *background-color*))
  (clear-ogl-pixmap bm (make-offscreen-pixel
                                (round (* (color-red clear-color) 255))
                                (round (* (color-green clear-color) 255))
                                (round (* (color-blue clear-color) 255)))))

;; NOTE: these functions actually return COLORS rather than the raw (system dependent) pixel value
;; used to grab a pixel value from the screen
(defvar *screen-pixel-buffer*)
(def-redisplay-initialization (setq *screen-pixel-buffer* (make-ogl-pixmap 1 1)))

(defun %get-pixel (port x y)
  (declare (ignore port))
  (%bitblt-from-screen 1 1 *screen-pixel-buffer* x y 0 0)
  (pixmap-pixel *screen-pixel-buffer*  0 0))

(defun offscreen-pixel-color (x y pixmap)
  (pixel->color (pixmap-pixel pixmap x y)))

(defun %pixblt-from-screen (pixmap fx fy wid hei)
  (%gl:read-pixels fx fy wid hei *pixmap-data-type* *pixmap-data-format* (ogl-pixmap-data pixmap)))
