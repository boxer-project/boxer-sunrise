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

(in-package :boxer)

;;;
;;; Constants and Variables
;;;

(defvar *boxer-frame* nil
  "This frame contains *turtle-pane* *boxer-pane* etc.")

(defvar *name-pane* nil)

(defvar *boxer-pane* nil
  "The pane which contains the actual boxer screen editor.")

;;colors, these get initialized AFTER the boxer window has been created
;(defvar *foreground-color*) ; defined in boxdef
(defvar *background-color*)

;; here for bootstrapping purposes, these might get changed later
;; by initialize-colors which actually looks at the window
(setq *foreground-color* (bw::make-ogl-color 0.0 0.0 0.0)
      *background-color* (bw::make-ogl-color 1.0 1.0 1.0))

(defvar *black*)
(defvar *white*)
(defvar *red*)
(defvar *green*)
(defvar *blue*)
(defvar *yellow*)
(defvar *magenta*)
(defvar *cyan*)
(defvar *orange*)
(defvar *purple*)
(defvar *gray*)
(defvar *closet-color*)

;; color variables that boxer uses
(defvar *default-border-color*) ;
(defvar *border-gui-color*)  ; color of temporary interface elements when mouse is held


(defun initialize-colors ()
  "This must be called AFTER *boxer-pane* has been created, as it depends on several
some opengl features to be available first."
  (setq *black*   (bw::ogl-convert-color #(:RGB 0.0 0.0 0.0))
        *white*   (bw::ogl-convert-color #(:RGB 1.0 1.0 1.0))
        *red*     (bw::ogl-convert-color #(:RGB 1.0 0.0 0.0))
        *green*   (bw::ogl-convert-color #(:RGB 0.0 1.0 0.0))
        *blue*    (bw::ogl-convert-color #(:RGB 0.0 0.0 1.0))
        *yellow*  (bw::ogl-convert-color #(:RGB 1.0 1.0 0.0))
        *magenta* (bw::ogl-convert-color #(:RGB 1.0 0.0 1.0))
        *cyan*    (bw::ogl-convert-color #(:RGB 0.0 1.0 1.0))
        *orange*  (bw::ogl-convert-color #(:RGB 1.0 0.6470585 0.0))
        *purple*  (bw::ogl-convert-color #(:RGB 0.627451 0.1254902 0.941175))
        *gray*    (bw::ogl-convert-color #(:RGB 0.752941 0.752941 0.752941)))
  (setq *foreground-color* *black*
        *background-color* *white*
        *default-border-color* *black*
        *closet-color* (bw::make-ogl-color .94 .94 .97)
        *border-gui-color* *default-border-color*))

;;; **** returns pixel value(window system dependent) at windw coords (x,y)
;;; see BU::COLOR-AT in grprim3.lisp
(defun window-pixel (x y &optional (view *boxer-pane*)) (%get-pixel view x y))
(defun window-pixel-color (x y &optional (view *boxer-pane*)) (opengl::pixel->color (%get-pixel view x y)))

;;;
;;; Drawing and Geometry layout type Macros
;;

(defmacro with-blending-on (&body body)
  ;; sgithens 2023-01-17 TODO Blending is essentially always on these days. Remove this once the old
  ;; openGL immediate mode version is removed completely.
  (let ((current-blend-var (gensym)))
    `(let ((,current-blend-var nil ))
       (unwind-protect
        (progn
         . ,body)))))

(defun %set-pen-size (v)
  (setf (boxgl-device-pen-size bw::*boxgl-device*) v))

;; needs to be happening inside a drawing-on-window (actually rendering-on)
(defmacro with-pen-size ((newsize) &body body)
  (let ((oldpsvar (gensym)) (nochangevar (gensym)) (newsizevar (gensym)))
    `(let ((,oldpsvar (boxgl-device-pen-size bw::*boxgl-device*))
           (,newsizevar (float ,newsize))
           (,nochangevar nil))
       (unwind-protect
        (progn
         (cond ((= ,newsizevar ,oldpsvar) (setq ,nochangevar t))
           (t (%set-pen-size ,newsize)))
         . ,body)
        (unless ,nochangevar (%set-pen-size ,oldpsvar))))))

(defmacro with-line-stippling ((pattern factor) &body body)
  (let ((stipplevar (gensym)))
    `(let ((,stipplevar (line-stipple bw::*boxgl-device*)))
       (unwind-protect
        (progn
          (setf (line-stipple bw::*boxgl-device*) t)
         . ,body)
        (unless ,stipplevar
          (setf (line-stipple bw::*boxgl-device*) nil))))))

(defmacro maintaining-drawing-font (&body body)
  (let ((font-var (gensym)))
    `(let ((,font-var *current-opengl-font*))
       (unwind-protect
        (progn . ,body)
        ;; NOTE: fonts aren't necessarily EQ
        (unless (eql *current-opengl-font* ,font-var)
          (setq *current-opengl-font* ,font-var))))))

(defmacro rebind-font-info ((font-no) &body body)
  `(let ((%drawing-font-cha-hei %drawing-font-cha-hei)
         (%drawing-font-cha-ascent %drawing-font-cha-ascent))
     (unless (null ,font-no)
       (maintaining-drawing-font
        (set-font-info ,font-no)
        ,@body))))

(defmacro with-drawing-port (view &body body)
  `(let ((%drawing-array ,view))
     (opengl::rendering-on (,view)
                           ;; always start by drawing eveywhere
                           (bw::ogl-reshape (sheet-inside-width ,view) (sheet-inside-height ,view))
                           (opengl::gl-scissor 0 0 (sheet-inside-width ,view) (sheet-inside-height ,view))
                           . ,body)))

(defmacro boxer-points->window-system-points (boxer-point-list (x-arg x-form)
                                                               (y-arg y-form))
  "this takes a set of boxer points and converts them into a form that
the window system desires.  The x/y-function args will be funcalled
on the coordinate as it is converted.

OpenGL expects a list of X Y pairs"
  `(macrolet ((x-handler (,x-arg) ,x-form)
              (y-handler (,y-arg) ,y-form))
             (let ((trans nil))
               (dolist (pt ,boxer-point-list (nreverse trans))
                 (push (list (x-handler (car pt)) (y-handler (cdr pt))) trans)))))

(defmacro prepare-sheet ((window) &body body)
  `(with-drawing-port ,window
     ;; make sure things are the way they should be
     ,@body))

(defun my-clip-rect (lef top rig bot)
  ;; gl-scissor uses OpenGL coords (0,0) = bottom,left
  ;; 1/13/2008 - fine tuned X  (- lef 1) => lef  &
  ;; Y   (- (sheet-inside-height box::%drawing-array) bot) =>
  (let* ((x (floor lef))
         ; Using our shaders, this 1+ adds a tiny big of uncovered red at the bottom of the screen.
         ; (y (floor (1+ (- (sheet-inside-height box::%drawing-array) bot))))
         (y (floor  (- (sheet-inside-height box::%drawing-array) bot)))
         (wid (ceiling (- rig (- lef 1))))
         (hei (ceiling (- bot top))))
    ;; For some reason, parts of our repaint code are generating a negative wid/hei, so just adding
    ;; this check for the time being.
    (when (and (>= wid 0) (>= hei 0))
      (gl:scissor x y wid hei))))

(defun window-system-dependent-set-origin (h v)
  "Translates the current transform matrix by an additional h and v distance.
  Does not replace it from scratch. In the repaint code using this macro we typically
  see the origin adjusted by some amount, and then un-adjusted by it with the negative
  amounts to put it back."
  (let* ((current-transform (3d-matrices:mat4 (boxer::boxgl-device-transform-matrix bw::*boxgl-device*)))
         (adjust-matrix (3d-matrices:mat4 (boxer::create-transform-matrix h v)))
         (new-transform (3d-matrices:m* current-transform adjust-matrix)))
    (setf (boxer::boxgl-device-transform-matrix bw::*boxgl-device*)
          (3d-matrices:marr4 new-transform))
    (update-matrices-ubo bw::*boxgl-device*) ;; todo
    ; (update-transform-ubo bw::*boxgl-device*)
    ))

(defvar %local-clip-lef 0)
(defvar %local-clip-top 0)
(defvar %local-clip-rig (expt 2 15))
(defvar %local-clip-bot (expt 2 15))

(defvar %clip-total-height nil)

(defmacro with-window-system-dependent-clipping ((x y wid hei) &body body)
  `(unwind-protect
    (let ((%clip-lef (max %clip-lef (+ %origin-x-offset ,x)))
          (%clip-top (max %clip-top (+ %origin-y-offset ,y)))
          (%clip-rig (min %clip-rig (+ %origin-x-offset ,x ,wid)))
          (%clip-bot (min %clip-bot (+ %origin-y-offset ,y ,hei))))
      ;; make sure that the clipping parameters are always at least
      ;; as restrictive as the previous parameters
      (my-clip-rect %clip-lef %clip-top %clip-rig %clip-bot)
      . ,body)
    ;; reset the old clip region
    (my-clip-rect %clip-lef %clip-top
                  %clip-rig %clip-bot)))

(defun clear-window (w)
  (gl::clear-color (bw::ogl-color-red *background-color*)
                   (bw::ogl-color-green *background-color*)
                   (bw::ogl-color-blue *background-color*)
                   0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit))

;;; used by repaint-in-eval
(defvar *last-eval-repaint* 0)

(defvar *eval-repaint-quantum* 50
  "Number of internal-time-units before the next buffer flush")

(defvar *eval-repaint-ratio* 2)

(defvar *last-repaint-duration* 0)

;;; Things for measuring repaint times
(defvar *last-framerate-time* (get-universal-time))
(defvar *number-of-frames* 0)
(defvar *current-framerate* 0)

(defun update-framerate ()
  "Register a repaint to update the statistics for the repaint rate."
  ;; https://www.opengl-tutorial.org/miscellaneous/an-fps-counter
  (let* ((current-time (get-universal-time))
        (diff (- current-time *last-framerate-time*)))
    (setq *number-of-frames* (1+ *number-of-frames*))
    (when (>= diff 1)
      (setq *current-framerate* (/ 1000.0 *number-of-frames*))
      (setq *number-of-frames* 0)
      (setq *last-framerate-time* (get-universal-time)))
  ))

(defun %flush-port-buffer (&optional (pane *boxer-pane*))
  (update-framerate)
  ; (opengl::rendering-on (pane) (opengl::gl-flush))
  (opengl::swap-buffers pane))

(defun string-wid (font-no string)
  (let ((font (find-cached-font font-no)))
    (if (null font)
      (error "No cached font for ~X" font-no)
      (bw::with-ogl-font (font)
                         (if (symbolp string)
                           (bw::ogl-string-width (string string) font)
                           (bw::ogl-string-width string font))))))

(defun string-hei (font-no)
  (let ((font (find-cached-font font-no)))
    (if (null font)
      (error "No cached font for ~X" font-no)
      (bw::with-ogl-font  (font)
                          (bw::ogl-font-height font)))))

(defun string-ascent (font-no)
  (let ((font (find-cached-font font-no)))
    (if (null font)
      (error "No cached font for ~X" font-no)
      (bw::with-ogl-font  (font)
                          (bw::ogl-font-ascent font)))))


;; proportional fonts
(defun cha-wid (char)
  (bw::ogl-char-width char))

(defun cha-ascent () %drawing-font-cha-ascent)

(defun cha-hei () %drawing-font-cha-hei)


;;;; COLOR (incomplete)

;; Boxer represents colors as RGB triples where each component is
;; between 0 and 100 inclusively.

;;
;; use this to convert a boxer RGB component to a window system dependent
;; color component.  Domain checking should be assumed to occur at a
;; higher (in boxer proper) level but paranoia is ok too
;; in CLX,  color component is between 0.0 and 1.0
;; Mac stores them as 3 concatenated 8-bit fields of an integer
;; LWWin RGB color spec component is between 0.0 and 1.0 but we pass pixels
;; instead.
;; Get pixels from color specs via ogl-convert-color which
;; Opengl color = gl-vector of 4 floats between 0.0 and 1.0

(defmacro color-red (pixel) `(bw::ogl-color-red ,pixel))

(defmacro color-green (pixel) `(bw::ogl-color-green ,pixel))

(defmacro color-blue (pixel) `(bw::ogl-color-blue ,pixel))

(defmacro color-alpha (pixel) `(bw::ogl-color-alpha ,pixel))

;; this should return a suitable argument to set-pen-color
;; should assume that R,G,and B are in boxer values (0->100)
;; **** If the current screen is B&W, this needs to return a valid value ****
;; **** perhaps after doing a luminance calculation ****
;; **** NTSC  luminance = .299Red + .587Green + .114Blue
;; **** SMPTE luminance = .2122Red + .7013Green + .0865Blue
(defun %make-color (red green blue &optional alpha)
  (%make-color-from-100s red green blue (or alpha 100.0)))

(defun %make-color-from-bytes (red green blue &optional (alpha 255))
  (bw::make-ogl-color (/ red   255.0)
                      (/ green 255.0)
                      (/ blue  255.0)
                      (/ alpha 255.0)))

(defun %make-color-from-100s (red green blue alpha)
  (bw::make-ogl-color (/ red   100.0)
                      (/ green 100.0)
                      (/ blue  100.0)
                      (/ alpha 100.0)))

;;; neccessary but not sufficient...
(Defun color? (thing) (typep thing 'opengl::gl-vector))

;; we are comparing WIN32::COLORREF's not COLOR:COLOR-SPEC's
;; so use WIN32:COLOR= instead of COLOR:COLORS=
(defun color= (c1 c2)
  (if (and c1 c2) ; these  can't be nil
    (bw::ogl-color= c1 c2)))

(defun %set-pen-color (color)
  "This expects either an already allocated GL 4 vector that represents a color, or an RGB percentage vector
  of the form #(:RGB 0.0 0.0 1.0) (for blue)."
  (cond ((and (vectorp color) (eq :RGB (aref color 0)))
        ;  (unless (equalp color (boxgl-device-pen-color bw::*boxgl-device*))
          ; (format *standard-output* "~%Actually different 1 color: ~A ugh: ~A" color (boxgl-device-pen-color bw::*boxgl-device*))
          (setf (boxgl-device-pen-color bw::*boxgl-device*) color)
          ; (update-matrices-ubo bw::*boxgl-device*))
          )
        ((color? color)
        ;  (unless (equalp (bw::ogl-color->rgb color) (boxgl-device-pen-color bw::*boxgl-device*))
          ; (format *standard-output* "~%Actually different 2 color: ~A ugh: ~A" (boxgl-device-pen-color bw::*boxgl-device*) (bw::ogl-color->rgb color))
          (setf (boxgl-device-pen-color bw::*boxgl-device*) (bw::ogl-color->rgb color))
          ; (update-matrices-ubo bw::*boxgl-device*)
          )

        (t
         (error "Bad color passed to %set-pen-color: ~A " color)))
  )

(defmacro with-pen-color ((color) &body body)
  `(bw::maintaining-ogl-color
    (bw::%set-pen-color ,color)
    . ,body))

(defmacro maintaining-pen-color (&body body)
  `(bw::maintaining-ogl-color . ,body))

(defun pixel-rgb-values (pixel)
  "Returns a list of RGB values for the dumper.
Should return the values in the boxer 0->100 range (floats are OK)"
  (list (* (color-red pixel)   100)
        (* (color-green pixel) 100)
        (* (color-blue pixel)  100)
        (* (color-alpha pixel) 100)))

;; new dumper interface...
;; leave pixel-rgb-values alone because other (non file) things now depend on it
;; NOTE: mac pixel dump value has Red as high byte and blue as low byte
;; so for compatibility between platforms, we must swap bytes because the
;; colorref-value arranges the color bytes as blue-green-red
;;
;; Opengl: this is supposed to return an RGB 24 bit value
;; Opengl color are stored as 0.0 to 1.0 floats, so we normalize to 255 and deposit the bytes
;; (what about the alpha (potential) value ??)
(defvar *red-byte-position* (byte 8 16))
(defvar *green-byte-position* (byte 8 8))
(defvar *blue-byte-position* (byte 8 0))

(defun pixel-dump-value (pixel)
  "TODO need to deal with possible alpha values..."
  (let ((alpha (color-alpha pixel)))
    (cond ((< alpha 1.0)
           ;; a transparent color, so return a list with the alpha value since the
           ;; dumper would be unhappy with a non fixnum pixel
           (pixel-rgb-values pixel))
      (t ; pack a fixnum (in the right order)
         (let* ((redbyte (round (* (color-red pixel) 255)))
                (greenbyte (round (* (color-green pixel) 255)))
                (bluebyte (round (* (color-blue pixel) 255))))
           (dpb& redbyte *red-byte-position*
                 (dpb& greenbyte *green-byte-position*
                       bluebyte)))))))

(defun pixel-dump-value-internal (winpixel)
  "we need to shave off the alpha value because higher level code
(dump-true-color-pixmap: dumper.lisp) assumes fixnum pixels"
  (let* ((returnpixel (logand winpixel #xff00))
         (redbyte (ldb (byte 8 0) winpixel))
         (bluebyte (ldb (byte 8 16) winpixel)))
    (dpb redbyte (byte 8 16)       ; move red to high position
         (dpb bluebyte (byte 8 0)   ; move blue to low byte
              returnpixel))))

;;;
;;; Drawing functions
;;;

(defun %draw-arc (bit-array alu x y width height th1 th2)
  "See the-attic for the previous lispworks GP library version of this function.
It's not clear yet whether we'll need to re-implement this for the future."
  (declare (ignore bit-array alu x y width height th1 th2)))

(defun %draw-c-arc (x y radius start-angle sweep-angle &optional filled?)
  "circular arc, rather than the more generic elliptical arc
opengl arc drawing routine starts at 3 oclock and sweeps clockwise in radians
also, opengl-draw-arc expects positive angle args"
  (if (minusp sweep-angle)
    ;; change the start so that we can use ABS sweep
    (gl-add-arc bw::*boxgl-device* x y radius
                         (* +degs->rads+ (mod (- (+ start-angle sweep-angle) 90) 360))
                         (* +degs->rads+ (abs sweep-angle))
                         filled?)
    (gl-add-arc bw::*boxgl-device* x y radius
                         (* +degs->rads+ (mod (- start-angle 90) 360))
                         (* +degs->rads+ sweep-angle)
                         filled?)

                         )

                         )

(defun %draw-cha (x y char &key (gl-model nil))
  "Font is managed by set-font-info.  Note that anything else that might change
the window font (ie, draw-string) has to change it back for this to work.
5/11/98 draw to baseline instead of top left"
  (if gl-model
      (add-char gl-model bw::*boxgl-device* x y char)
      (gl-add-char bw::*boxgl-device* x y char)))

(defun %draw-circle (x y radius &optional filled?)
  (gl-add-circle bw::*boxgl-device* x y radius filled?))

(defun %draw-filled-arc (bit-array alu x y width height th1 th2)
  "See the-attic for the previous lispworks GP library version of this function.
It's not clear yet whether we'll need to re-implement this for the future."
  (declare (ignore bit-array alu x y width height th1 th2)))

(defun %draw-line (x0 y0 x1 y1)
  "Low level draw-line.  If we are using the graphics list performance, this will
  copy the points to the c-buffer we are putting vertices in, otherwise it will
  call the old single openGL draw line that uses and glBegin and glEnd.
  "
  (gl-add-line bw::*boxgl-device* x0 y0 x1 y1))

(defun %draw-point (x y)
  (gl-add-point bw::*boxgl-device* x0 y0))

(defun %draw-poly (points)
  (gl-add-poly bw::*boxgl-device* points))

(defun %draw-rectangle (width height x y)
  (unless (or (>= %clip-lef %clip-rig) (>= %clip-top %clip-bot))
    (gl-add-rect bw::*boxgl-device* x y width height)))

(defun %erase-rectangle (w h x y window)
  (unless (null window)
    (with-pen-color (*background-color*)
      (%draw-rectangle w h x y))))

(defun %draw-string (font string x y)
  (gl-add-string bw::*boxgl-device* (find-cached-font font) string x y))

(defun %bitblt-to-screen (wid hei from-array fx fy tx ty)
  ;; remember that  we are drawing from the lower left....
  (gl-add-pixmap bw::*boxgl-device* from-array (round tx) (round ty) (round wid) (round hei) fx fy))

(defun %bitblt-from-screen (wid hei to-array fx fy tx ty)
  ;; bw::gl-read-pixels
  ;; (bw::gl-read-buffer bw::*gl-front*) ; read from the (visible) front buffer
  (%pixblt-from-screen to-array (round fx)
                                ;; not quite right especially with a translated fy
                                (round (- (sheet-inside-height *boxer-pane*) (+ fy hei)))
                                (round wid) (round hei) tx ty))

;;;;
;;;; Boxer bitmaps
;;;;

;; 2/28/2011: Use the back buffer instead because its hard to reliably get an aux
;; buffer on different platforms...
(defmacro with-system-dependent-bitmap-drawing ((bitmap &optional
                                                        bitmap-width bitmap-height)
                                                &body body)
  (declare (ignore bitmap-width bitmap-height))
  `(opengl::rendering-on (*boxer-pane*)
                     (gl:draw-buffer :back) ;gl:aux1)
                     (progn . ,body)
                     (gl:flush)
                     (%pixblt-from-screen ,bitmap 0 (- (sheet-inside-height *boxer-pane*)
                                                       (ogl-pixmap-height ,bitmap))
                                                    (ogl-pixmap-width  ,bitmap)
                                                    (ogl-pixmap-height ,bitmap)
                                                    0 0 :back)))

(defun clear-offscreen-bitmap (bm &optional (clear-color *background-color*))
  (clear-ogl-pixmap bm (make-offscreen-pixel
                                (round (* (color-red clear-color) 255))
                                (round (* (color-green clear-color) 255))
                                (round (* (color-blue clear-color) 255)))))

(defun deallocate-system-dependent-structures (box)
  (let ((gi (graphics-info box)))
    (when (graphics-sheet? gi)
      (let ((bm (graphics-sheet-bit-array gi)))
        (unless (null bm) (ogl-free-pixmap bm)))))
  ;; this is for quicktime
  ;(let ((avi (av-info box))) (unless (null avi) (deallocate-av-info avi)))
  )

;; NOTE: these functions actually return COLORS rather than the raw (system dependent) pixel value
;; used to grab a pixel value from the screen
(defvar *screen-pixel-buffer*)
(def-redisplay-initialization (setq *screen-pixel-buffer* (make-ogl-pixmap 1 1)))

(defun %get-pixel (port x y)
  (declare (ignore port))
  (%bitblt-from-screen 1 1 *screen-pixel-buffer* x y 0 0)
  (pixmap-pixel *screen-pixel-buffer*  0 0))

(defun offscreen-pixel-color (x y pixmap)
  (opengl::pixel->color (opengl::pixmap-pixel pixmap x y)))
