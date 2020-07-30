; -*- MODE:LISP; Syntax: Common-Lisp; Package:Boxer; -*-

#|



             COPYRIGHT 1998 - 2014 PyxiSystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



 This file contains the low level window system dependent interface to
 the mac.


Modification History (most recent at top)

 4/23/14 added string-ascent
 3/20/14 changed *font-families*: moved "arial" to the default and added "verdana"
11/09/13 added ("Helvetica" . "Arial") to *font-family-aliases*
10/16/13 make-boxer-font-{ogl,capogi}
 9/25/13 fontspec->font-values,
 8/02/13 removed warnings
 7/26/13 Relative Font size utilities: *font-size-baseline*, *bfd-font-size-names*,
         bfd-font-size-name, %font-size-to-idx, *font-sizes*,  font-size-to-idx,
         %font-size-idx-to-size, find-cached-font, cache-font, font-size,
         init-bootstrapping-font-vars, fill-bootstrapped-font-caches
 1/23/13 functions which use pixblt must convert to fixnums before calling
11/21/12 find-filled-font = find-cached-font + bw::ensure-oglfont-parameters
         used by string-{wid,hei}
 9/15/12 my-clip-rect now has to coerce its args to fixnums
 8/27/12 functions with explicit calls to opengl prims now refer to another layer of prims
         in opengl-utils.lisp which can (optionally) type check and/or coerce
         %draw-line, %draw-rect, %set-pen-size,
 3/ 6/11 added *boxtop-text-font* init to init-bootstrapping-font-vars
 2/28/11 changed with-system-dependent-bitmap-drawing to use the back buffer instead of an aux buffer
 2/15/11 fill-bootstrapped-font-caches fills for ALL glut fonts
 2/13/11 string-{wid,hei} now use bw::with-ogl-font to insure fonts are filled
 2/ 6/11 auxiliary-buffer-{count,exists?}, with-system-dependent-bitmap-drawing
 4/27/10 added ogl-reshape to with-drawing-port
 4/20/10 %draw-circle %draw-arc using new opengl-draw-xxx functions
 4/17/10 font caching mechanism changed: make-boxer-font, fill-bootstrapped-font-caches,
         cache-on-startup?
12/13/09 copy-image-to-bitmap & uncolor->pixel utilities for converting images (from capi:clipboard)
         to OpenGL bitmaps
11/27/09 Added *closet-color* and it's init's
11/13/09 Added screen-pixel-color & offscreen-pixel-color to sharpen distinction between pixels & colors
         which have become muddled in higher level code
 9/05/09 maintaining-drawing-font
 3/13/09 boxer-points->window-system-points for OpenGL
 2/22/09 copy-offscreen-bitmap
 3/02/08 pixel-dump-value handles possible non 1.0 alpha values
 1/13/08 my-clip-rect, x & y off by 1
 5/22/07 border colors added:*default-border-color*, *border-gui-color*
 GPU configuration: parameters and initialization
 5/13/06 prelim version done
 1/23/05 added flush-port-buffer stub
 1/04/05 %erase-rectangle
 3/01/02 fixed bug in %draw-xor-string
 2/22/02 font aliasing
 2/19/02 %font-name-to-idx, fontspec->font-no, make-boxer-font
 2/17/02 added pixel-dump-value-internal used by dump-true-color-pixmap for
         platform independence of pixels in offscreen pixmaps
 2/16/02 %draw-xor-string makes sure we have plusp wid & hei before blitting
 1/12/02 %draw-xor-string now hacks clipping
10/10/01 %draw-string window arg changed to %drawing-array to handle bitmap binding
10/06/01 pixel-dump-value now swaps Red and Blue bytes
 9/06/01 %set-image-pixel no longer needs to coerce to a colorref
 5/08/01 %draw-rectangle checks clipping state for unusual cases which don't seem
         to be handled by the window system
 4/30/01 %bitblt functions now do clipping in software because pixblt doe'sn't
         pay attention to the mask
 2/26/01 %draw-point, %draw-poly XOR case :foreground changed to #xffffff
 2/20/01 fixed %draw-arc & %draw-filled-arc
 2/13/01 merged current LW and MCL files (draw-low-mcl.lisp)
 2/13/01 maintaining-drawing-font folded into rebind-font-info
 2/04/01 fixed typos in %draw-arc, %draw-filled-arc & draw-point
11/01/00 with-system-dependent-bitmap-drawing now binds %graphics-state
         so (current-graphics-state) can win
10/04/00 changed image-pixel to return a fixnum instead of a win32::colorref
         because that is what the dumping functions expect
         %set-image-pixel also changed
 8/22/00 added %make-color-from-bytes/100s
 7/09/00 defined %get-pixel using gp:get-point and ported pixel handling functions
         :foreground #xffffff for XOR ops (was #xffff) 5/31/00 relaxed %font-size-to-idx to handle values (like 1) actually found in files
 5/08/00 fixed deferred my-clip-rect
 5/03/00 fixed X/Y reversal in boxer-points->window-system-points
 3/24/00 make-boxer-font changed to return encoded numeric value instead of
         internal system font
 2/02/00 added full font caching to fill-bootstrapped-font-caches
12/15/99 added 7 to %font-size-to-idx to support loadingof old mac files
11/29/99 fixed string-wid & string-hei: documentation for gp:get-string-extent had
         the order of returned values wrong (pg 136)
11/16/99 %erase-rectangle changed to use gp::draw-rectangle because gp::clear-rectangle
         ignores the offset state of the graphics state's transform which causes it to
         lose for inferior boxes during redisplay
 3/29/99 new fonts finished ??
11/02/98 Copied from draw-low-mcl.lisp and reduced to function & args stubs
         for conversion to CAPI, GP lispworks

notes:: check points arg on draw-poly
        search for &&& which marks stubs

|#


(in-package :boxer)

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

;;; skinable support (move to box-borders ?)
;;; corners, sides, trans sides, labels, names




;;;; Constants and Variables

;; These specify how to combine bits already there with the drawing bits
;; The common lisp boole-xxx constants are the right thing
;; (see LispWorks User Guide 15.3, pg 126)

(defconstant alu-andca boole-andc1   "Erase")
(defconstant alu-seta  boole-1       "Drawing bits have priority")
(defconstant alu-xor   boole-xor     "XORs the bits")
(defconstant alu-and   boole-and     "ANDs the bits")
(defconstant alu-ior   boole-ior     "ORs the bits")
(defconstant alu-setz  boole-clr     "Set to Zero")


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


;; must be called AFTER *boxer-pane* has been created
(defun initialize-colors ()
  (setq *black*   (bw::ogl-convert-color (color::get-color-spec :black))
        *white*   (bw::ogl-convert-color (color::get-color-spec :white))
        *red*     (bw::ogl-convert-color (color::get-color-spec :red))
        *green*   (bw::ogl-convert-color (color::get-color-spec :green))
        *blue*    (bw::ogl-convert-color (color::get-color-spec :blue))
        *yellow*  (bw::ogl-convert-color (color::get-color-spec :yellow))
        *magenta* (bw::ogl-convert-color (color::get-color-spec :magenta))
        *cyan*    (bw::ogl-convert-color (color::get-color-spec :cyan))
        *orange*  (bw::ogl-convert-color (color::get-color-spec :orange))
        *purple*  (bw::ogl-convert-color (color::get-color-spec :purple))
        *gray*    (bw::ogl-convert-color (color::get-color-spec :gray)))
  (setq *foreground-color* *black*
        *background-color* *white*
        *default-border-color* *black*
        *closet-color* (bw::make-ogl-color .94 .94 .97)
        *border-gui-color* *default-border-color*))

;;; In the new regime, coordinates are relative to the grafport (window) rather than the pane.

(defun sheet-inside-top (window) (declare (ignore window)) 0)
(defun sheet-inside-left (window) (declare (ignore window)) 0)

;; what about capi:simple-pane-visible-size ?
(defun sheet-inside-height (window) (gp:port-height window))
(defun sheet-inside-width  (window) (gp:port-width  window))

;; yuck !!!
(defun window-depth (window)
  (declare (ignore window)) (capi:screen-depth (car (capi::screens))))

;;; why doesn't this work ?  It's a documented function
; (gp:port-depth window))


;;; &&&& stub
;;; **** returns pixel value(window system dependent) at windw coords (x,y)
;;; see BU::COLOR-AT in grprim3.lisp
(defun window-pixel (x y &optional (view *boxer-pane*)) (%get-pixel view x y))
(defun window-pixel-color (x y &optional (view *boxer-pane*)) (opengl::pixel->color (%get-pixel view x y)))

(defmacro rebind-font-info ((font-no) &body body)
  `(let ((%drawing-font %drawing-font)
	 (%drawing-font-cha-wid %drawing-font-cha-wid)
	 (%drawing-font-cha-hei %drawing-font-cha-hei)
	 (%drawing-font-cha-ascent %drawing-font-cha-ascent))
     (unless (null ,font-no)
       (maintaining-drawing-font
         (set-font-info ,font-no)
         ,@body))))

;; this is a stub
;; it is used by some window systems(the mac) to insure all graphics
;; commands are sent to the VIEW arg
;;
;; we seem to access the graphics state a lot so it might be advantageous to
;; bind it within this macro

(defvar %graphics-state nil)

(defmacro with-drawing-port (view &body body)
  `(let ((%drawing-array ,view)
         (%graphics-state (gp:get-graphics-state ,view)))
     (opengl::rendering-on (,view)
       ;; always start by drawing eveywhere
       (bw::ogl-reshape (sheet-inside-width ,view) (sheet-inside-height ,view))
       (bw::gl-scissor 0 0 (sheet-inside-width ,view) (sheet-inside-height ,view))
     . ,body)))

(defmacro current-graphics-state ()
  '(or %graphics-state (gp:get-graphics-state (or %drawing-array *boxer-pane*))))

;; **** added WITH-PORT
(defmacro prepare-sheet ((window) &body body)
  `(with-drawing-port ,window
      ;; make sure things are the way they should be
     ;(bw::with-open-blinkers (bw::sheet-blinker-list ,window)
     ,@body))

;;; This is called to make sure the quickdraw clipping is set up to match the boxer clipping.
(defun update-window-system-state () )

;; gl-scissor uses OpenGL coords (0,0) = bottom,left
;; 1/13/2008 - fine tuned X  (- lef 1) => lef  &
;; Y   (- (sheet-inside-height box::%drawing-array) bot) =>
(defun my-clip-rect (lef top rig bot)
;  (format t "~&SCissor ~D, ~D, ~D ~D" lef (- (sheet-inside-height box::%drawing-array)
;                         box::%origin-y-offset)
;          (-& rig lef) (-& bot top))
  (bw::gl-scissor (floor lef)
                  (floor (1+ (- (sheet-inside-height box::%drawing-array) bot)))
                  (ceiling (- rig (- lef 1)))
                  (ceiling (- bot top))))

;;; **** NEW, used in draw-high-highware-clip
(defun window-system-dependent-set-origin (h v)
  (bw::gl-translatef h v 0.0))

(defvar %local-clip-lef 0)
(defvar %local-clip-top 0)
(defvar %local-clip-rig (expt 2 15))
(defvar %local-clip-bot (expt 2 15))

(defvar %clip-total-height nil)

;;; **** NEW, used in draw-high-highware-clip
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

(defun sheet-screen-array (window) window)

;;; Note: naive implementation, see draw-low-clx.lisp for caching version
(defun window-inside-size (w)
  (values (sheet-inside-width  w) (sheet-inside-height w)))

(defun clear-window (w)
  (bw::rendering-on (w)
    ;; sets the clearing color
    ;; shouldn't need to do this EVERY time
    (bw::gl-clear-color (bw::ogl-color-red *background-color*)
                        (bw::ogl-color-green *background-color*)
                        (bw::ogl-color-blue *background-color*)
                        0.0)
    ;(gl-clear-depth d)
    ;(gl-clear-accum r g b alpha)
    ;(gl-clear-stencil s)
    ;; clears the screen to the clearing color
    ;; 2nd arg is logior of possible:
    ;; *gl-color-buffer-bit*
    ;; *GL-depth-BUFFER-BIT*
    ;; *GL-accum-BUFFER-BIT*
    ;; *GL-stencil-BUFFER-BIT*
    (bw::gl-clear bw::*gl-color-buffer-bit*)))

;;; used by repaint-in-eval
(defvar *last-eval-repaint* 0)

(defvar *eval-repaint-quantum* 50
  "Number of internal-time-units before the next buffer flush")

(defvar *eval-repaint-ratio* 2)

(defvar *last-repaint-duration* 0)

;;; stub for double buffering window systems
(defun flush-port-buffer (&optional (pane *boxer-pane*))
  (bw::rendering-on (pane) (bw::gl-flush))
  (bw::swap-buffers pane))

(defun string-wid (font-no string)
  (let ((font (find-filled-font font-no)))
    (if (null font)
        (error "No cached font for ~X" font-no)
        (bw::with-ogl-font (font)
          (bw::ogl-string-width string font)))))

(defun string-hei (font-no)
  (let ((font (find-filled-font font-no)))
    (if (null font)
        (error "No cached font for ~X" font-no)
      (bw::with-ogl-font  (font)
          (bw::ogl-font-height font)))))

(defun string-ascent (font-no)
  (let ((font (find-filled-font font-no)))
    (if (null font)
        (error "No cached font for ~X" font-no)
      (bw::with-ogl-font  (font)
          (bw::ogl-font-ascent font)))))



;;; Fonts

;;;;; obsolete.... commented out 7/18/13
;(defvar *font-map* (make-array '(8)))
;(defvar *font-codes* (make-array '(8)))

;;; ???
;(defun sheet-font-map (w)
;  (declare (ignore w))
;  *font-map*)

;; stub
(defun sheet-font-map (w) (declare (ignore w)) nil)

(defvar %drawing-font-cha-ascent nil)

(defun set-font-info (x)
  (let ((system-font (find-cached-font x)))
    (bw::ogl-set-font system-font)
    (multiple-value-bind (ascent height widmax leading)
        (bw::ogl-font-info system-font)
      (setq %drawing-font x
            %drawing-font-cha-wid widmax ;; originally (- widmax 3)
            %drawing-font-cha-ascent ascent
            %drawing-font-cha-hei (+ height leading)))
    x))



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



(defvar *font-size-baseline* 1 )

(defun max-font-size-baseline-value ()
  (- (length *font-sizes*) 7 1))

;; Low Level Font Utilities
(defconstant %%font-family-bit-pos 16)
(defconstant %%font-family-bit-length 8)
(defconstant %%font-size-bit-pos 4)
(defconstant %%font-size-bit-length 12)
(defconstant %%font-face-bit-pos 0)
(defconstant %%font-face-bit-length 4)

(defconstant %%font-family-size (expt 2 %%font-family-bit-length))
(defvar *current-font-family-index* 0)

;; doesn't fill the reserved 8-bit sized field, but keep them
;; only as big as wee need
;(defconstant %%font-size-cache-length 12)
(defconstant %%font-face-cache-length 4)

(defvar *font-cache* (make-array %%font-family-size :initial-element nil))
(defvar *font-family-names* (make-array %%font-family-size :initial-element nil))

;; Must be True Type Fonts
(defvar *font-families*
  #+glut
  '("Fixed" "Times" "Helvetica")
  #+capogi
  '("Arial" "Courier New" "Times New Roman" "Verdana")
  #-(or glut capogi)
  '("Courier" "Arial" "Times"))  ; Verdana ?

(defvar *default-font-family* (car *font-families*))
(defvar *default-font-size*   3)
(defvar *default-font-face* nil)

(defun %make-font-size-cache ()
  (make-array (length *font-sizes*) :initial-element nil))
; (make-array ;%%font-size-cache-length :initial-element nil))

(defun %make-font-face-cache ()
  (make-array %%font-face-cache-length :initial-element nil))

;; field extractors
(defun %font-family-idx (font-no)
  (ldb& (byte %%font-family-bit-length %%font-family-bit-pos) font-no))
(defun %font-size-idx (font-no)
  (ldb& (byte %%font-size-bit-length   %%font-size-bit-pos)   font-no))
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

(defun bfd-font-size-name (size-idx) (svref *bfd-font-size-names* (1- size-idx)))

;; available sizes are: 6,8,10,12,14,16,18,24
;; NOTE: we an get size 7 when loading old mac boxes...
(defun %font-size-to-idx (size)
  (cond ((<=& size  8) size)  ;; sgithens: support for relative size saving, see comments on *dump-relative-font-sizes?* in dumper.lisp
        ((<=& size  9) 1)
        ((<=& size 10) 2)
        ((<=& size 12) 3)
        ((<=& size 14) 4)
        ((<=& size 16) 5)
        ((<=& size 20) 6)
        ((<=& size 24) 7)
        ((<=& size 28) 8)
        ((<=& size 32) 9)
        ((<=& size 40) 10)
        ((<=& size 48) 11)
        ((<=& size 56) 12)
        (t 13)))

(defvar *font-sizes* (vector 8 9 10 12 14 16 20 24 28 32 40 48 56 64)) ;(vector 6 8 10 12 14 16 18 24)

;; "size" is the size value returned from font-values of a relative font (used in BFD's)
;; abstract through this function to allow for more complicated translations, perhaps skipping
;; entries in the master cache to get the correct size ratios
(defun font-size-to-idx (size) (+& size *font-size-baseline* -1))

(defun %font-size-idx-to-size (sidx) (svref *font-sizes* sidx))

(defun make-size-menu-names ()
  (let* ((sl (length *font-sizes*))
         (n-sizes (- sl 6)) ;; number of possible size settings
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

(defun get-size-menu-names ()
  (svref *font-size-menu-names* *font-size-baseline*))

(defun font-size-menu-item-name (data)
  (nth (1- data) (svref *font-size-menu-names* *font-size-baseline*)))

;; size resolution happens here. The relative size is resolved along with *font-size-baseline*
(defun find-cached-font (font-no &optional (translate-size t))
  (multiple-value-bind (fam size face)
      (font-values font-no)
    (let ((size-cache (svref& *font-cache* fam)))
      (unless (null size-cache)
        (let ((face-cache (svref& size-cache (if translate-size
                                                 (font-size-to-idx size)
                                               size))))
          (unless (null face-cache) (svref& face-cache face)))))))

;; use when we are bypassing the bw::ogl-set-font mechanism
(defun find-filled-font (font-no)
  (let ((f (find-cached-font font-no)))
    (unless (null f) (bw::ensure-oglfont-parameters f) f)))

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

(defun fontspec->font-values (fontspec)
  (let ((fambits (%font-name-to-idx (or (car fontspec) *default-font-family*))))
    (unless (null fambits)
      (values fambits
              (%font-size-to-idx (cadr fontspec))
              (%font-face-to-idx (or (cddr fontspec) *default-font-face*))))))

;; this is the main hook for hacking cross platform fonts
;; need to think more deeply about rejecting "obviously" bogus font names
;; currently, we rely on gp:find-best-font which just returns a "default" for
;; unhandled names (looks like it's "MS Serif")

;; use an alist for now, we may have to get more complicated if it looks like
;; the number of "foreign" fonts will be large
(defvar *font-family-aliases*
  '(("Geneva" . "Arial") ("Courier" . "Courier New") ("Times" . "Times New Roman") ("Helvetica" . "Arial"))
  "Initialize this variable if you want explicit font translations")

(defun font-family-alias (family-name)
  (cdr (assoc family-name *font-family-aliases* :test #'string-equal)))

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

;; main interface function, how/when cache ????

(defun make-boxer-font (rawfontspec &optional (calculate-parameters? T))
  (if bw::*use-capogi-fonts*
      (make-boxer-font-capogi rawfontspec)
    (make-boxer-font-ogl rawfontspec calculate-parameters?)))

;; always "calculate parameters" because they are already available in the capogi font structure
(defun make-boxer-font-capogi (rawfontspec)
  (let* ((alias (font-family-alias (car rawfontspec)))
         (fontspec (if alias (list* alias (cdr rawfontspec)) rawfontspec))
         (cfont (bw::boxer-font-spec->capogi-font fontspec))
         (font-no (fontspec->font-no fontspec)))
    (cond ((null font-no)
           (let ((oglfont (bw::make-opengl-font-from-capogi-font cfont))
                 (new-font-no (fontspec->font-no fontspec T)))
             (or (find-cached-font new-font-no nil)
                 (cache-font oglfont new-font-no nil))
             new-font-no))
          (t
           (or (find-cached-font font-no nil)
               (cache-font (bw::make-opengl-font-from-capogi-font cfont) font-no nil))
           font-no))))

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



;; the external interface, see comsf.lisp for usage
(defun font-style (font-no) (%font-face-idx font-no))

;; returns absolute font size
(defun font-size (font-no)
  (%font-size-idx-to-size (font-size-to-idx (%font-size-idx font-no))))

(defun font-name (font-no)
  (let* ((idx (%font-family-idx font-no)))
    (when (< idx (length *font-family-names*))
      (svref *font-family-names* idx))))

;; these next 3 take a font code and should return a new font code
(defun %set-font (boxer-font new-fname)
  (let ((new-idx (%font-name-to-idx new-fname)))
    (dpb new-idx (byte %%font-family-bit-length %%font-family-bit-pos) boxer-font)))

;(defun %set-font-size (boxer-font new-size)
;  (let ((new-idx (%font-size-to-idx new-size)))
;    (dpb new-idx (byte %%font-size-bit-length   %%font-size-bit-pos) boxer-font)))

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
  #+glut (opengl::initialize-glut-font-pointers)
  #+glut (opengl::initialize-glut-font-metrics)
  #+capogi (bw::load-capogi-font-cache)
  (dolist (font-family *font-families*)
    (dotimes (i (length *font-sizes*))
      (let ((size (svref *font-sizes* i)))
        (dolist (style '(nil (:bold) (:italic) (:bold :italic))) ; leave out :underline for now
          (make-boxer-font (list* font-family size style)
                           #+glut T
                           #-glut (cache-on-startup? font-family size style)))))))

(defun cache-on-startup? (family size style)
  (declare (ignore style))
  (and (string-equal family "Courier")
       (<=  size 18)))

;; old version, these are the ones which MUSTbe defined
;; note that "Courier New" must be the 1st family cached
;  (make-boxer-font '("Courier New" 10)) ; this should fill family=0, size=10
;  ;; now fill the rest of the family=0, size=10 slots
;  (make-boxer-font '("Courier New" 10 :bold))
;  (make-boxer-font '("Courier New" 10 :italic))
;  (make-boxer-font '("Courier New" 10 :bold :italic))
;  (make-boxer-font '("Courier New" 6)


;; THIS is safe to do
(eval-when (load)  (init-bootstrapping-font-vars))

(defun set-font (boxer-font)
  (let ((system-font (find-cached-font boxer-font)))
    (if (null system-font)
        (error "No cached font for ~X" boxer-font)
      (bw::ogl-set-font system-font))))

(defmacro maintaining-drawing-font (&body body)
  (let ((font-var (gensym)) (fba-var (gensym)))
    `(let ((,font-var bw::*current-opengl-font*)
           (,fba-var bw::*current-opengl-font-base-addr*))
       (unwind-protect
           (progn . ,body)
         ;; NOTE: fonts aren't necessarily EQ
         (unless (eql bw::*current-opengl-font* ,font-var)
           (setq bw::*current-opengl-font* ,font-var
                 bw::*current-opengl-font-base-addr* ,fba-var))))))

;; proportional fonts
(defun cha-wid (char)
  (bw::ogl-char-width char))

(defun cha-ascent () %drawing-font-cha-ascent)

; not used ?
;(defun fast-cha-ascent () (gp:get-font-ascent %drawing-array))
;(defun fast-cha-hei ()
;  (+& (gp:get-font-ascent  %drawing-array)
;      (gp:get-font-descent %drawing-array)))

(defun cha-hei () %drawing-font-cha-hei)



(defun %draw-string (alu font string x y &optional (window %drawing-array))
  (declare (ignore alu window))
  (let ((system-font (find-cached-font font)))
    (if (null system-font)
        (error "Can't find cached font for ~X" font)
      (bw::with-ogl-font (system-font)
        (bw::ogl-draw-string string x y)))))


;;; this takes a set of boxer points and converts them into a form that
;;; the window system desires.  The x/y-function args will be funcalled
;;; on the coordinate as it is converted.
;;;
;;; OpenGL expects a list of X Y pairs
(defmacro boxer-points->window-system-points (boxer-point-list (x-arg x-form)
							       (y-arg y-form))
     `(macrolet ((x-handler (,x-arg) ,x-form)
		(y-handler (,y-arg) ,y-form))
        (let ((trans nil))
          (dolist (pt ,boxer-point-list (nreverse trans))
            (push (list (x-handler (car pt)) (y-handler (cdr pt))) trans)))))

;;; Real Primitives

;;; **** used in sprite graphics for parameter checking
;;; **** are these the right numbers for MCL ?
(defun max-window-coord () #.(expt 2 15))
(defun min-window-coord () #.(- (expt 2 15)))

;;; Font is managed by set-font-info.  Note that anything else that might change
;;; the window font (ie, draw-string) has to change it back for this to work.
;;; 5/11/98 draw to baseline instead of top left
(defun %draw-cha (alu x y char)
  (declare (ignore alu))
  (bw::ogl-draw-char char x y))

(defun %draw-line (x0 y0 x1 y1 alu end-point? window)
  (declare (ignore alu end-point? window))
  (bw::ogl-draw-line x0 y0 x1 y1))

(defun %set-pen-size (v)
  (bw::ogl-set-pen-size v))

;; needs to be happening inside a drawing-on-window (actually rendering-on)
(defmacro with-pen-size ((newsize) &body body)
  (let ((oldpsvar (gensym)) (nochangevar (gensym)) (newsizevar (gensym)))
    `(let ((,oldpsvar (bw::get-opengl-state bw::*gl-line-width* :float))
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
    `(let ((,stipplevar (bw::get-opengl-state bw::*gl-line-stipple* :boolean)))
       (unwind-protect
           (progn
             (opengl::gl-line-stipple ,factor ,pattern)
             (opengl::gl-enable bw::*gl-line-stipple*)
             . ,body)
         (unless ,stipplevar (opengl::gl-disable bw::*gl-line-stipple*))))))




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
(Defun color? (thing) (typep thing 'bw::gl-vector))

;; we are comparing WIN32::COLORREF's not COLOR:COLOR-SPEC's
;; so use WIN32:COLOR= instead of COLOR:COLORS=
(defun color= (c1 c2)  (bw::ogl-color= c1 c2))

(defun %set-pen-color (color)
  (bw::ogl-set-color color))

(defun %pen-color= (color)
  (color= color
          (bw::ogl-current-color)))

;;; How's this?
(defmacro with-pen-color ((color) &body body)
  `(bw::maintaining-ogl-color
     (bw::ogl-set-color ,color)
     . ,body))

;; figure out if we are using this for convert-color specs or not
(defun normalize-color-component (value) (/ value 100.0))

(defmacro maintaining-pen-color (&body body)
  `(bw::maintaining-ogl-color . ,body))

;;; returns a list of RGB values for the dumper
;;; should return the values in the boxer 0->100 range (floats are OK)

(defun pixel-rgb-values (pixel)
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
(defconstant *red-byte-position* (byte 8 16))
(defconstant *green-byte-position* (byte 8 8))
(defconstant *blue-byte-position* (byte 8 0))

;; need to deal with possible alpha values...
(defun pixel-dump-value (pixel)
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

;; we need to shave off the alpha value because higher level code
;; (dump-true-color-pixmap: dumper.lisp) assumes fixnum pixels
(defun pixel-dump-value-internal (winpixel)
  (let* ((returnpixel (logand winpixel #xff00))
         (redbyte (ldb (byte 8 0) winpixel))
         (bluebyte (ldb (byte 8 16) winpixel)))
    (dpb redbyte (byte 8 16)       ; move red to high position
          (dpb bluebyte (byte 8 0)   ; move blue to low byte
                returnpixel))))

(defun %draw-poly (points alu array)
  (declare (ignore alu array))
  (bw::ogl-draw-poly points))

;; CLX like version
(defun %draw-rectangle (width height x y alu bit-array) ; &key (xor-color #xffffff)
  (declare (ignore alu bit-array))
  (unless (or (>= %clip-lef %clip-rig) (>= %clip-top %clip-bot))
    (bw::ogl-draw-rect x y (+ x width) (+ y height))))

;;; **** NEW: draw a single pixel at window's X,Y
;;; just drawing a little rect for now,there's probably a better way to do this
(defun %draw-point (x y alu window)
  (declare (ignore alu window))
  (bw::ogl-draw-point x y))

(defun %erase-rectangle (w h x y window)
  (unless (null window)
    (with-pen-color (*background-color*)
      (%draw-rectangle w h x y alu-seta window))))

; gp::erase-rectangle ignores the state of the transform
; so it loses for sub boxes during redisplay
  ;(gp:clear-rectangle window x y w h)

;;; These take topleft coordinates
;; the angle args are like CLX, start at 3 oclock and positive sweep is
;; counterclockwise.  Need to convert to boxer sense where we start at
;; 12oclock and positive sweep is clockwise and in degrees

(defun %draw-circle (x y radius &optional filled?)
  (bw::opengl-draw-circle x y radius filled?))

;; circular arc, rather than the more generic elliptical arc
;; opengl arc drawing routine starts at 3 oclock and sweeps clockwise in radians
;; also, opengl-draw-arc expects positive angle args
(defun %draw-c-arc (x y radius start-angle sweep-angle &optional filled?)
  (if (minusp sweep-angle)
      ;; change the start so that we can use ABS sweep
      (bw::opengl-draw-arc x y radius
                       (* *degs->rads* (mod (- (+ start-angle sweep-angle) 90) 360))
                       (* *degs->rads* (abs sweep-angle))
                       filled?)
    (bw::opengl-draw-arc x y radius
                     (* *degs->rads* (mod (- start-angle 90) 360))
                     (* *degs->rads* sweep-angle)
                     filled?)))


;; not currently used, leave here to document calling convention
(defun %draw-arc (bit-array alu x y width height th1 th2)
  (declare (ignore bit-array alu x y width height th1 th2))
#|
  (if (=& alu alu-xor)
      (gp:draw-arc bit-array x y width height (* (- 90 th1) *degs->rads*)
                   (* th2 *degs->rads* -1)
               :operation alu :filled nil :foreground #xffffff)
    (gp:draw-arc bit-array x y width height (* (- 90 th1) *degs->rads*)
                 (* th2 *degs->rads* -1)
               :operation alu :filled nil))
|#
)

;; not currently used, leave here to document calling convention
(defun %draw-filled-arc (bit-array alu x y width height th1 th2)
  (declare (ignore bit-array alu x y width height th1 th2))
#|
  (if (=& alu alu-xor)
      (gp:draw-arc bit-array x y width height (* (- 90 th1) *degs->rads*)
                   (* th2 *degs->rads* -1)
               :operation alu :filled t :foreground #xffffff)
    (gp:draw-arc bit-array x y width height (* (- 90 th1) *degs->rads*)
                 (* th2 *degs->rads* -1)
                 :operation alu :filled t))
|#
)

;; BITBLT Operations
;; NOTE: pixblt does not honor the graphics context mask so we have to
;; do the clipping in software

(defmacro clip-x (scaled-x) `(max& %clip-lef (min& ,scaled-x %clip-rig)))
(defmacro clip-y (scaled-y) `(max& %clip-top (min& ,scaled-y %clip-bot)))
(defmacro sign-of-no (x) `(if (plusp& ,x) 1 -1))

;; bw::gl-draw-pixels (w h
;; remember that  we are drawing from the lower left....
(defun %bitblt-to-screen (alu wid hei from-array fx fy tx ty)
  (declare (ignore alu))
  (opengl::%pixblt-to-screen from-array (round tx) (round ty) (round wid) (round hei) fx fy))

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

;; bw::gl-read-pixels
;; (bw::gl-read-buffer bw::*gl-front*) ; read from the (visible) front buffer
(defun %bitblt-from-screen (alu wid hei to-array fx fy tx ty)
  (declare (ignore alu))
  (opengl::%pixblt-from-screen to-array (round fx)
                               ;; not quite right especially with a translated fy
                               (round (- (sheet-inside-height *boxer-pane*) (+ fy hei)))
                               (round wid) (round hei) tx ty))

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

(defun %bitblt-in-screen (alu wid hei array fx fy tx ty)
  (declare (ignore alu array))
  (opengl::%pixblt-in-screen (round wid) (round hei)
                             (round fx) (round fy) (round tx) (round ty)))

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

;; &&&& stub
(defun %bitblt-tile-to-screen (alu w h tile bitmap to-x to-y)
  (declare (ignore alu bitmap))
  (opengl::%pixblt-to-screen tile (round to-x) (round to-y) (round w) (round h) 0 0))

;; &&&& stub
(defun %bitblt-icon-to-screen (icon tx ty) (declare (ignore icon tx ty)) )






;;;; Boxer bitmaps

(defun make-offscreen-bitmap (window w h)
  (declare (ignore window))
  (opengl::make-ogl-pixmap w h))

;; check for the existence of auxiliary buffer so we can signal
;; an error at the right level
(defun auxiliary-buffer-count ()
  (bw::get-opengl-state opengl::*gl-aux-buffers* :signed-32))

(defun auxiliary-buffer-exists? ()
  (not (zerop (auxiliary-buffer-count))))

;; this could draws into an auxiliary buffer and then
;; transfers the bits from that buffer into an ogl-pixmap
#|
(defmacro with-system-dependent-bitmap-drawing ((bitmap &optional
                                                        bitmap-width bitmap-height)
					        &body body)
  (declare (ignore bitmap-width bitmap-height))
  `(bw::rendering-on (*boxer-pane*)
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

;; 2/28/2011: Use the back buffer instead because its hard to reliably get an aux
;; buffer on different platforms...
(defmacro with-system-dependent-bitmap-drawing ((bitmap &optional
                                                        bitmap-width bitmap-height)
					        &body body)
  (declare (ignore bitmap-width bitmap-height))
  `(bw::rendering-on (*boxer-pane*)
     (bw::gl-draw-buffer bw::*gl-aux1*)
     (progn . ,body)
     (bw::gl-flush)
     (opengl::%pixblt-from-screen ,bitmap 0 (- (sheet-inside-height *boxer-pane*)
                                               (opengl::ogl-pixmap-height ,bitmap))
                                  (opengl::ogl-pixmap-width  ,bitmap)
                                  (opengl::ogl-pixmap-height ,bitmap)
                                  0 0 bw::*gl-back*)))

(defun clear-offscreen-bitmap (bm &optional (clear-color *background-color*))
  (opengl::clear-ogl-pixmap bm (opengl::make-offscreen-pixel
                                (round (* (color-red clear-color) 255))
                                (round (* (color-green clear-color) 255))
                                (round (* (color-blue clear-color) 255)))))

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

;;; These assume bitmap bounds are zero based
(defun offscreen-bitmap-height (bm) (opengl::ogl-pixmap-height bm))

(defun offscreen-bitmap-width (bm) (opengl::ogl-pixmap-width bm))

(defun free-offscreen-bitmap (bitmap) (opengl::ogl-free-pixmap bitmap))

;; also yuck, but it might actually be correct
;; see window-depth for real yuck
(defun offscreen-bitmap-depth (bitmap)
  (opengl::ogl-pixmap-depth bitmap))

;;; See Inside Mac V-53
#| ;; unused ?
(defun pixmap? (bm-or-pm) (typep bm-or-pm 'gp::pixmap-port))
|#

;; copy-graphics-sheet in makcpy,  stamp-bitmap in  gcmeth ?
(defun copy-offscreen-bitmap (alu wid hei src src-x src-y dst dst-x dst-y)
  (declare (ignore alu))
  (opengl::%copy-pixmap-data wid hei src src-x src-y dst dst-x dst-y))

(defun deallocate-system-dependent-structures (box)
  (let ((gi (graphics-info box)))
    (when (graphics-sheet? gi)
      (let ((bm (graphics-sheet-bit-array gi)))
        (unless (null bm) (deallocate-bitmap bm)))))
  ;; this is for quicktime
  ;(let ((avi (av-info box))) (unless (null avi) (deallocate-av-info avi)))
  )

(defun deallocate-bitmap (bm) (opengl::ogl-free-pixmap bm))

;; NOTE: these functions actually return COLORS rather than the raw (system dependent) pixel value
;; used to grab a pixel value from the screen
(defvar *screen-pixel-buffer*)
(def-redisplay-initialization (setq *screen-pixel-buffer* (make-offscreen-bitmap *boxer-pane* 1 1)))

(defun %get-pixel (port x y)
  (declare (ignore port))
  (%bitblt-from-screen alu-seta 1 1 *screen-pixel-buffer* x y 0 0)
  (offscreen-pixel 0 0 *screen-pixel-buffer*))

;; **** Look here !!! ****
;; this is supposed to return an object that you can use offscreen-pixel
;; with.  In the clx implementation, it actually brings the data over to
;; the client side in an array.  In other implementations,
;; offscreen-bitmap-image might just get the actual image data out from
;; behind any containing structures
;;
;; In the mac implementation, we just refer to the GWorld.  We may want to change
;; this to refer to the Pixmap of the GWorld in the future since we have already
;; made the semantic split in the higher level code.  The caveat would be in properly
;; interpreting the meaning of the pixel arg in the SETF version of IMAGE-PIXEL.
;;
;; In the OpenGL implementation, we have to deal with the fact that the image is
;; built from the bottom up instead of the top down.  This is done and the load/save level
;; it also means we have o pass the entire pixmap struct so we can use the width, height
;; slots to calculate the correct offset for the pixel's location

(defun offscreen-bitmap-image (bm) bm)

;; **** A hook for those implementations in which OFFSCREEN-BITMAP-IMAGE
;; returns a separate structure like X.  Probably a NOOP for any native
;; window system
(defun set-offscreen-bitmap-image (bm image)
  (declare (ignore bm image))
  nil)

;; **** may have to change this depending on what offscreen-bitmap-image does
;; THIS should be SETFable
;; Image-Pixel and the SETF is supposed to work in conjuction with offscreen-bitmap-image's

(defmacro image-pixel (x y pixmap)
  `(offscreen-pixel ,x ,y ,pixmap))

;;;
(defun %set-image-pixel (x y pixmap new-pixel)
  (opengl::set-pixmap-pixel new-pixel pixmap x y))

(defsetf image-pixel %set-image-pixel)


(defun offscreen-pixel (x y pixmap)
   (opengl::pixmap-pixel pixmap x y))

(defun offscreen-pixel-color (x y pixmap)
  (opengl::pixel->color (opengl::pixmap-pixel pixmap x y)))

;;; capi:clipboard returns IMAGES
(defun copy-image-to-bitmap (image bm w h)
  (let ((ia (gp:make-image-access *boxer-pane* image))
        (bdata (opengl::ogl-pixmap-data bm)))
    (unwind-protect
        (progn
          (gp::image-access-transfer-from-image ia)
          (dotimes (y h)
            (dotimes (x w)
              (setf (fli:dereference bdata :index (+ x (* (- h y 1) w)))
                    (uncolor->pixel
                     (color:unconvert-color *boxer-pane* (gp:image-access-pixel ia x y)))))))
      (gp:free-image-access ia))))

;; uncolor = result of color:unconvert-color which is a simple-vector with components of
;; :RGB, red, green, blue & alpha in 0->1.0 floa format
(defun uncolor->pixel (uncolor)
  (flet ((convert-color-component (cc)
           (floor (* cc 255))))
    (opengl::make-offscreen-pixel (convert-color-component (svref uncolor 1))
                                  (convert-color-component (svref uncolor 2))
                                  (convert-color-component (svref uncolor 3))
                                  (convert-color-component (svref uncolor 4)))))

;;; should we pass blend functions in ? ((src-blend-func dst-blend-func) &body body)
(defmacro with-blending-on (&body body)
  (let ((current-blend-var (gensym)))
    `(let ((,current-blend-var (bw::gl-enabled? bw::*gl-blend*)))
       (unwind-protect
           (progn
             (bw::gl-blend-func bw::*gl-src-alpha* bw::*gl-one-minus-src-alpha*)
             (bw::gl-enable bw::*gl-blend*)
             . ,body)
         (unless ,current-blend-var (bw::gl-disable bw::*gl-blend*))))))
