;; -*- Mode:LISP; Syntax:Common-Lisp; Package:BOXER; -*-
#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-Data--+
             This file is part of the | BOXER | system
                                      +-------+



   This file contains boxer functions which use the
   graphics subsystem.

   Mostly leftovers, graphics commands which don't use sprites...

Modification History (most recent at top)

 2/28/11 fully functional #+opengl FREEZE, rewritten using the back-buffer
 1/02/10 temporarily restored partial working FREEZE for OpenGL
11/18/09 sprite stacking order prims
 4/21/08 bu::hide/show-private-graphics
 3/01/07 add alpha channel support for colors
         lookup-color-values-in-box, make-color-box, make-color-internal,
         bu::make-transparent-color, update-color-box-internal
 4/19/03 merged current LW and MCl files
 2/12/02 added bitmap dirty? flag handling to freeze, snip and set-color-at
10/14/01 SET-GRAPHICS-SIZE, SNIP insures wid/hei args to be > 0
 1/17/01 add-color-def, lookup-color-values-in-box #'= changed to #'color=
 1/17/01 Started logging changes: source = Boxer Version 2.3 alpha PC

|#

(in-package :boxer)




;;; for graphics boxes

(defun modified-graphics (box)
  ;; note that the usual MODIFIED method is NOT enough to get the
  ;; box redisplayed because of the way graphics-screen-boxes
  ;; were hacked (to speed up sprites)
  ;; we need to explicity set the FORCE-REDISPLAY-INFS? flag in
  ;; any existing graphics-screen boxes
  (queue-object-to-be-modified box))
;;   (dolist (port (ports box))
;;     (dolist (sb (screen-objs port))
;;       (when (graphics-screen-box? sb) (set-force-redisplay-infs? sb t))))
;;   (dolist (sb (screen-objs box))
;;     (when (graphics-screen-box? sb) (set-force-redisplay-infs? sb t)))
        ;; )

;;; should new-graphics being empty = clear graphics ?
;;; or should there be a separate clear-graphics primitive ?
;;; Error for now....

(boxer-eval::defboxer-primitive bu::change-graphics ((bu::port-to graphics-box)
                                         (boxer-eval::dont-copy new-graphics))
  (let ((new-gs (get-graphics-sheet (box-or-port-target new-graphics))))
    (cond ((null new-gs)
           (boxer-eval::primitive-signal-error :graphics-error
                                         "No graphics information in"
                                         new-graphics))
          (t (let* ((box (box-or-port-target graphics-box))
                    (old-gs (get-graphics-sheet box)))
               (cond ((and (sprite-box? (superior-box box))
                           (equal (string-upcase (name box)) "SHAPE"))
                      (set-shape (graphics-info (superior-box box)) (box-or-port-target new-graphics)))
                     ((and (box? box) (null old-gs))
                      (setf (graphics-info box)
                            (copy-graphics-sheet new-gs box))
                      (modified-graphics box))
                     ((box? box)
                      (let ((gs (copy-graphics-sheet new-gs box))
                            (old-objs (graphics-sheet-object-list old-gs)))
                        (setf (graphics-sheet-object-list gs) old-objs)
                        (setf (graphics-info box) gs))
                      (clearscreen box :none)
                      (modified-graphics box))
                     (t
                      ;; must be a VC, we can just bash the value but 1st,
                      ;; record the sheet for later deallocation in
                      ;; case it contains a bitmap.  Note that if the VC
                      ;; ALREADY has a sheet with a bitmap, it will have been
                      ;; recorded
                      (let ((sheet (copy-graphics-sheet new-gs nil)))
                        (queue-non-lisp-structure-for-deallocation sheet)
                        (setf (getf (vc-graphics box) 'graphics-sheet)
                              sheet)))))))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::graphics-mode ()
  (let ((gb (get-relevant-graphics-box)))
    (cond ((or (null gb) (eq gb :no-graphics))
           (boxer-eval::primitive-signal-error :graphics-error
                                         "No Graphics Box found"))
          (t
           (make-vc (list (intern-in-bu-package
                           (graphics-sheet-draw-mode
                            (get-graphics-sheet gb)))))))))

(boxer-eval::defboxer-primitive bu::set-graphics-mode ((boxer-eval::dont-copy mode))
  (let ((gb (get-relevant-graphics-box)))
    (cond ((or (null gb) (eq gb :no-graphics))
           (boxer-eval::primitive-signal-error :graphics-error
                                         "No Graphics Box found"))
          (t
           (let ((new-mode (caar (raw-unboxed-items mode)))
                 (gs (get-graphics-sheet gb)))
             (case new-mode
               (bu::clip (setf (graphics-sheet-draw-mode gs) :clip))
               (bu::wrap (setf (graphics-sheet-draw-mode gs) :wrap))
               (otherwise (boxer-eval::primitive-signal-error
                           :graphics-error new-mode
                           "is not a valid Graphics Mode"))))))
    boxer-eval::*novalue*))

(boxer-eval::defboxer-primitive bu::graphics-size ()
  (let ((gb (get-relevant-graphics-box)))
    (cond ((or (null gb) (eq gb :no-graphics))
           (boxer-eval::primitive-signal-error :graphics-error
                                         "No Graphics Box found"))
          (t
           (let ((gs (get-graphics-sheet gb)))
             (make-vc (list (list (graphics-sheet-draw-wid gs)
                                  (graphics-sheet-draw-hei gs)))))))))

(boxer-eval::defboxer-primitive bu::set-graphics-size ((boxer-eval::numberize width)
                                           (boxer-eval::numberize height))
  (let ((gb (get-relevant-graphics-box)))
    (cond ((or (null gb) (eq gb :no-graphics))
           (boxer-eval::primitive-signal-error :graphics-error
                                         "No Graphics Box found"))
          ((or (< width  1) (< height 1))
           (boxer-eval::primitive-signal-error
            :graphics-error "Both graphics box dimensions should be at least " 1))
          (t
           (let ((gs (get-graphics-sheet gb)))
             (unless (null gs)
               (resize-graphics-sheet
                gs (values (round width)) (values (round height)))))
           (when (box? gb)
             (modified gb))
           boxer-eval::*novalue*))))




;;; color primitives

(defun lookup-color-values-in-box (box pixel)
  (when (box? box)
    (let ((colordefs (getprop box :color-definition)))
      (unless (null colordefs)
        (let ((color (assoc pixel colordefs :test #'color=)))
          (unless (null color)
            (values (cadr color) (caddr color) (cadddr color) (nth 4 color))))))))

(defun add-color-def (alist pixel rgblist)
  (let ((existing (assoc pixel alist :test #'color=)))
    (if (null existing)
        (nconc existing (list (cons pixel rgblist)))
        (progn
          (setf (cdr existing) rgblist) alist))))

(defun make-color-box (r g b &optional alpha)
  (let ((box (make-box (list (if (null alpha) (list r g b) (list r g b alpha))))))
    (add-closet-row box (make-row (list (make-box '(("Update-Color-Box"))
                                                  'doit-box
                                                  "Modified-Trigger"))))
    box))

(defun make-color-internal (red green blue &optional alpha)
  (let* ((pixel (%make-color red green blue alpha))
         (box (make-color-box red green blue alpha))
         (graphics-sheet (%make-simple-graphics-sheet
                          *pen-color-graphics-width*
                          *pen-color-graphics-size* box)))
    (putprop box (list (cons pixel (if (null alpha)
                                       (list red green blue)
                                     (list red green blue alpha))))
             :color-definition)
    (setf (graphics-sheet-background graphics-sheet) pixel)
    (setf (graphics-info box) graphics-sheet)
    (setf (display-style-graphics-mode? (display-style-list box)) t)
    box))

;; leave this for old code
(boxer-eval::defboxer-primitive bu::make-color ((boxer-eval::numberize red)
                                    (boxer-eval::numberize green)
                                    (boxer-eval::numberize blue))
  (cond ((or (not (<= 0 red 100)) (not (<= 0 green 100)) (not (<= 0 blue 100)))
         (boxer-eval::primitive-signal-error
          :graphics "A color component value was not between 0 and 100 (~A, ~A,~A)"
          red green blue))
        (t
         (make-color-internal red green blue))))

(boxer-eval::defboxer-primitive bu::make-transparent-color ((boxer-eval::numberize red)
                                                (boxer-eval::numberize green)
                                                (boxer-eval::numberize blue)
                                                (boxer-eval::numberize opacity))
  (cond ((or (not (<= 0 red 100)) (not (<= 0 green 100)) (not (<= 0 blue 100))
             (not (<= 0 opacity 100)))
         (boxer-eval::primitive-signal-error
          :graphics "A color component value was not between 0 and 100 (~A,~A,~A,~A)"
          red green blue opacity))
        (t
         (make-color-internal red green blue opacity))))

;;; hung on the modified trigger of COLOR boxes
(boxer-eval::defboxer-primitive bu::update-color-box ()
  (let ((box (get-graphics-box)))
    (update-color-box-internal box)
    boxer-eval::*novalue*))


;;; probably ought to modify the values in the editor
(defun update-color-box-internal (box)
  (if (color-box? box)
      (multiple-value-bind (red green blue alpha) ; alpha may be nil for old boxes
          (parse-color-box box)
        (unless (<= 0 red 100)
          (let ((newred (if (< red 0) 0 100)))
            (sprite-update-warning
             "The Red Component, ~A, was not between 0 and 100, new color will use red = ~D"
             red newred)
            (setq red newred)))
        (unless (<= 0 green 100)
          (let ((newgreen (if (< green 0) 0 100)))
            (sprite-update-warning
             "The Green Component, ~A, was not between 0 and 100, new color will use green = ~D"
             green newgreen)
            (setq green newgreen)))
        (unless (<= 0 blue 100)
          (let ((newblue (if (< blue 0) 0 100)))
            (sprite-update-warning
             "The Blue Component, ~A, was not between 0 and 100, new color will use blue = ~D"
             blue newblue)
            (setq blue newblue)))
        (cond ((null alpha) (setq alpha 100))
              ((not (<= 0 alpha 100))
               (sprite-update-warning
                "Opacity, ~A, was not between 0 and 100, new color will use blue = ~D"
                alpha 100)
               (setq alpha 100)))
        (let* ((pixel (%make-color red green blue alpha))
               (graphics-sheet (or (get-graphics-sheet box)
                                   (%make-simple-graphics-sheet
                                    *pen-color-graphics-width*
                                    *pen-color-graphics-size* box))))
          (putprop box (add-color-def (getprop box :color-definition)
                                      pixel (list red green blue alpha))
                   :color-definition)
          (setf (graphics-sheet-background graphics-sheet) pixel)
          (setf (graphics-info box) graphics-sheet)
          (setf (display-style-graphics-mode? (display-style-list box)) t)
          (modified-graphics box)))
      ;; looks like the CHANGEd box does not conform to color box standards...
      ;; set the values in the box back and signal an error
      (let* ((graphics-sheet (get-graphics-sheet box))
             (current-pixel (and (graphics-sheet? graphics-sheet)
                                 (graphics-sheet-background graphics-sheet))))
        (when current-pixel
          (bash-box-to-list-value box (pixel-rgb-values current-pixel)))
        (boxer-eval::primitive-signal-error
         :graphics "Can't make a color from the new values in the color box: "
         box))))


;; boxer (100-based) normalized RGB components
;; Numbers from X11 rgb.txt, feel free to change per implementation
(defvar *standard-colors* '(("Red" 100 0 0) ("Green" 0 100 0) ("Blue" 0 0 100)
                            ("Yellow" 100 100 0) ("Cyan" 0 100 100)
                            ("Magenta" 100 0 100) ("Black" 0 0 0)
                            ("White" 100 100 100) ("Orange" 100 65 0)
                            ("Purple" 63 13 94) ("Gray" 50 50 50)))

;; have to do this so they get defined dynamically after the display
;; is up and running (probably matters most for X)
(def-redisplay-initialization
  ;; set up some standard colors for sprites
  (dolist (color *standard-colors*)
      (let ((colorbox (boxer::make-color-internal
                       (cadr color) (caddr color) (cadddr color)))
            (variable-name (boxer::intern-in-bu-package
                            (string-upcase (car color)))))
        (boxer-eval::boxer-toplevel-set variable-name colorbox))))




(boxer-eval::defboxer-primitive bu::snap ((bu::Port-to box))
  (let* ((gb (box-or-port-target box))
         (graphics-sheet (if (box? gb)
                             (graphics-info gb)
                             (graphics-info-graphics-sheet (vc-graphics gb))))
         (newvc (make-empty-vc)))
    (cond ((null graphics-sheet)
           (boxer-eval::primitive-signal-error :graphics box
                                         "does not have any graphics"))
          (t
           (let ((sheet (copy-graphics-sheet graphics-sheet nil)))
             (queue-non-lisp-structure-for-deallocation sheet)
             (setf (vc-graphics newvc) (list 'graphics-sheet sheet))
             newvc)))))

;;; this should play the GB's graphics list into an auxiliary OpenGL buffer and then
;;; grab the pixels from it instead of using the main screen...
#+opengl-for-complete-correctness
(boxer-eval::defboxer-primitive bu::freeze ()
  (boxer-eval::primitive-signal-error :graphics "Freeze does not work yet."))

(defun make-color-editor-box-from-pixel (pix)
  (let* ((rgb-values (pixel-rgb-values pix))
         (box (make-color-box (car rgb-values) (cadr rgb-values)
                              (caddr rgb-values)))
         (graphics-sheet (%make-simple-graphics-sheet
                          *pen-color-graphics-width*
                          *pen-color-graphics-size* box)))
    (putprop box (list (cons pix rgb-values)) :color-definition)
    (setf (graphics-sheet-background graphics-sheet) pix)
    (setf (graphics-info box) graphics-sheet)
    (setf (display-style-graphics-mode? (display-style-list box)) t)
    box))

(defun make-color-box-from-pixel (pix)
  (let* ((rgb-values (pixel-rgb-values pix))
         (box (make-vc (list (make-evrow-from-entries rgb-values))))
         (graphics-sheet (%make-simple-graphics-sheet
                          *pen-color-graphics-width*
                          *pen-color-graphics-size* box)))
    (setf (graphics-sheet-background graphics-sheet) pix)
    (setf (vc-graphics box) (list 'graphics-sheet graphics-sheet))
    box))

(boxer-eval::defboxer-primitive bu::color-at ((boxer-eval::numberize x) (boxer-eval::numberize y))
  (let ((gb (get-relevant-graphics-box)))
    (if (or (null gb) (eq gb :no-graphics))
        (boxer-eval::primitive-signal-error :graphics "Can't find a graphics box")
        (let ((sb (car (displayed-screen-objs gb))))
          (if (null sb)
              (boxer-eval::primitive-signal-error
               :graphics "Pixel at " x "," y "is not visible")
              (multiple-value-bind (box-x box-y)
                  (xy-position sb)
                (multiple-value-bind (lef top rig bot)
                    (box-borders-widths (box-type sb) sb)
                  (declare (ignore rig bot))
                  (with-graphics-vars-bound (gb)
                    (let ((gb-x (fix-array-coordinate-x (float x)))
                          (gb-y (fix-array-coordinate-y (float y))))
                      (if (and (< 0 gb-x (screen-obj-wid sb))
                               (< 0 gb-y (screen-obj-hei sb)))
                          (make-color-box-from-pixel
                           (window-pixel-color (floor (+ box-x lef gb-x))
                                               (floor (+ box-y top gb-y))))
                          (boxer-eval::primitive-signal-error
                           :graphics
                           "Pixel at " x "," y "is not visible")))))))))))

(boxer-eval::defboxer-primitive bu::bg-color-at ((boxer-eval::numberize x) (boxer-eval::numberize y))
  (let ((gb (get-relevant-graphics-box)))
    (if (or (null gb) (eq gb :no-graphics))
        (boxer-eval::primitive-signal-error :graphics "Can't find a graphics box")
        (let* ((gs (graphics-info gb))
               (bit-array (graphics-sheet-bit-array gs))
               (background (graphics-sheet-background gs)))
          (with-graphics-vars-bound-internal gs
            (let ((gb-x (fix-array-coordinate-x (float x)))
                  (gb-y (fix-array-coordinate-y (float y))))
              (if (and (<=& 0 gb-x (graphics-sheet-draw-wid gs))
                       (<=& 0 gb-y (graphics-sheet-draw-hei gs)))
                  (cond ((not (null bit-array))
                         (make-color-box-from-pixel
                           (pixmap-pixel-color bit-array gb-x gb-y)))
                        ((not (null background))
                         (make-color-box-from-pixel background))
                        (t (make-color-box-from-pixel *background-color*)))
                  (boxer-eval::primitive-signal-error
                   :graphics x "," y "is not in the graphics box"))))))))

;; a faster version (because it doesn't have to CONS color boxes) of
;; COLOR= [BG-COLOR-AT x y] [COLOR-AT x y]
(boxer-eval::defboxer-primitive bu::bg-color-at? ((boxer-eval::numberize x) (boxer-eval::numberize y))
  (let ((gb (get-relevant-graphics-box)))
    (if (null gb)
        (boxer-eval::primitive-signal-error :graphics "Can't find a graphics box")
        (let* ((gs (graphics-info gb))
               (sb (car (displayed-screen-objs gb)))
               (bit-array (graphics-sheet-bit-array gs))
               (background (graphics-sheet-background gs)))
          (if (null sb)
              (boxer-eval::primitive-signal-error
               :graphics "Pixel at " x "," y "is not visible")
          (with-graphics-vars-bound-internal gs
            (multiple-value-bind (box-x box-y)
                  (xy-position sb)
                (multiple-value-bind (lef top rig bot)
                    (box-borders-widths (box-type sb) sb)
                  (declare (ignore rig bot))
                  (let ((gb-x (fix-array-coordinate-x (float x)))
                        (gb-y (fix-array-coordinate-y (float y))))
                    (if (and (< 0 gb-x (screen-obj-wid sb))
                             (< 0 gb-y (screen-obj-hei sb)))
                        (boxer-eval::boxer-boolean
                         (color= (window-pixel-color (+ box-x lef gb-x)
                                                     (+ box-y top gb-y))
                                 (cond ((not (null bit-array))
                                        (pixmap-pixel-color bit-array gb-x gb-y))
                                       (t
                                        (or background *background-color*)))))
                        (boxer-eval::primitive-signal-error
                         :graphics
                         "Pixel at " x "," y "is not visible")))))))))))

(defun color-from-box (box)
  (let ((gs (if (box? box)
                (graphics-info box)
                (graphics-info-graphics-sheet (vc-graphics box)))))
    (unless (null gs) (graphics-sheet-background gs))))

;;; Like COLOR= <color> COLOR-AT <x> <y>
;;; except no color box consing
(boxer-eval::defboxer-primitive bu::color-at= ((boxer-eval::numberize x) (boxer-eval::numberize y)
                                   (boxer-eval::dont-copy color))
  (let ((gb (get-relevant-graphics-box)) (c (color-from-box color)))
    (cond ((null gb)
           (boxer-eval::primitive-signal-error :graphics
                                         "Can't find a graphics box"))
          ((null c)
           (boxer-eval::primitive-signal-error :graphics
                                         color "is not a color box"))
          (t
           (let ((sb (car (displayed-screen-objs gb))))
             (if (null sb)
                 (boxer-eval::primitive-signal-error
                  :graphics "Pixel at " x "," y "is not visible")
                 (multiple-value-bind (box-x box-y)
                     (xy-position sb)
                   (multiple-value-bind (lef top rig bot)
                       (box-borders-widths (box-type sb) sb)
                     (declare (ignore rig bot))
                     (with-graphics-vars-bound (gb)
                       (let ((gb-x (fix-array-coordinate-x (float x)))
                             (gb-y (fix-array-coordinate-y (float y))))
                         (if (and (< 0 gb-x (screen-obj-wid sb))
                                  (< 0 gb-y (screen-obj-hei sb)))
                             (boxer-eval::boxer-boolean
                              (color=
                               (window-pixel-color (+ box-x lef gb-x)
                                                   (+ box-y top gb-y))
                               c))
                             (boxer-eval::primitive-signal-error
                              :graphics
                              "Pixel at " x "," y "is not visible"))))))))))))

;;; Like COLOR= <color> BG-COLOR-AT <x> <y>
;;; except no color box consing
(boxer-eval::defboxer-primitive bu::bg-color-at= ((boxer-eval::numberize x) (boxer-eval::numberize y)
                                      (boxer-eval::dont-copy color))
  (let ((gb (get-relevant-graphics-box)) (c (color-from-box color)))
    (cond ((null gb)
           (boxer-eval::primitive-signal-error :graphics
                                         "Can't find a graphics box"))
          ((null c)
           (boxer-eval::primitive-signal-error :graphics
                                         color "is not a color box"))
          (t
           (let* ((gs (graphics-info gb))
                  (bit-array (graphics-sheet-bit-array gs))
                  (background (graphics-sheet-background gs)))
             (with-graphics-vars-bound-internal gs
               (let ((gb-x (fix-array-coordinate-x (float x)))
                     (gb-y (fix-array-coordinate-y (float y))))
                 (if (and (<=& 0 gb-x (graphics-sheet-draw-wid gs))
                          (<=& 0 gb-y (graphics-sheet-draw-hei gs)))
                     (boxer-eval::boxer-boolean
                      (color=
                       (cond ((not (null bit-array))
                              (pixmap-pixel-color bit-array gb-x gb-y))
                             (t (or background *background-color*)))
                       c))
                     (boxer-eval::primitive-signal-error
                      :graphics x "," y "is not in the graphics box")))))))))

(boxer-eval::defboxer-primitive bu::color= ((boxer-eval::dont-copy color1)
                                (boxer-eval::dont-copy color2))
  (let ((c1 (color-from-box color1)) (c2 (color-from-box color2)))
    (cond ((null c1) (boxer-eval::primitive-signal-error
                      :graphics color1 "is not a color box"))
          ((null c2) (boxer-eval::primitive-signal-error
                      :graphics color2 "is not a color box"))
          (t (boxer-eval::boxer-boolean (color= c1 c2))))))

(boxer-eval::defboxer-primitive bu::set-color-at ((boxer-eval::numberize x) (boxer-eval::numberize y)
                                                  (boxer-eval::dont-copy color))
  (let ((gb (get-relevant-graphics-box)))
    (if (null gb)
        (boxer-eval::primitive-signal-error :graphics "Can't find a graphics box")
        (let* ((pixcolor (color-from-box color)))
          (when (null pixcolor)
            (boxer-eval::primitive-signal-error :graphics color "should be a color box"))
          (with-graphics-vars-bound (gb)
            (let ((cur-color *graphics-state-current-pen-color*))
              ;; TODO: We could check the current color and perhaps not change it, if they are equal...
              ;;       ... but  there might be scenerios where we want this output to really stand along
              (record-boxer-graphics-command-change-graphics-color pixcolor)
              (record-boxer-graphics-command-dot x y)
              (record-boxer-graphics-command-change-graphics-color cur-color))))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::show-sprite-properties ()
  (let* ((sprite-box (get-sprites))
         (sprite (unless (null sprite-box) (graphics-info sprite-box))))
    (cond ((or (null sprite) (not (turtle? sprite)))
           (boxer-eval::primitive-signal-error :sprite-error
                                         "Can't find a sprite"))
          (t
           (let ((closet-row (closet-row sprite-box)))
             (dolist (slot-name (all-interface-slots sprite))
               (let ((slot (slot-value sprite slot-name)))
                 (when (null (box-interface-box slot))
                   (append-cha closet-row
                               ;; just give them the right name and let the
                               ;; link box into interface mechanism do its job
                               (make-box '(()) 'data-box
                                         (box-interface-slot-name slot))
                         ;      (make-sprite-instance-var slot)
                               )))))
           boxer-eval::*novalue*))))

(boxer-eval::defboxer-primitive bu::hide-private-graphics ()
  (let* ((sprite-box (get-sprites))
         (sprite (unless (null sprite-box) (graphics-info sprite-box))))
    (cond ((or (null sprite) (not (turtle? sprite)))
           (boxer-eval::primitive-signal-error :sprite-error
                                         "Can't find a sprite"))
          (t (let ((pgl (slot-value sprite 'private-gl)))
               (unless (null pgl) (setf (graphics-command-list-hidden pgl) t))
               boxer-eval::*novalue*)))))

(boxer-eval::defboxer-primitive bu::show-private-graphics ()
  (let* ((sprite-box (get-sprites))
         (sprite (unless (null sprite-box) (graphics-info sprite-box))))
    (cond ((or (null sprite) (not (turtle? sprite)))
           (boxer-eval::primitive-signal-error :sprite-error
                                         "Can't find a sprite"))
          (t (let ((pgl (slot-value sprite 'private-gl)))
               (unless (null pgl) (setf (graphics-command-list-hidden pgl) nil))
               boxer-eval::*novalue*)))))

;;; Stacking Order Prims
;;; NOTE: "top" means drawing last & "bottom" means drawing first
(boxer-eval::defboxer-primitive bu::place-on-top ()
  (let* ((sprite-box (get-sprites))
         (sprite (unless (null sprite-box) (graphics-info sprite-box)))
         (gb (unless (null sprite) (assoc-graphics-box sprite))))
    (unless (null gb)
      (let* ((gs (graphics-sheet gb))
             (oblist (when (graphics-sheet? gs) (graphics-sheet-object-list gs))))
        (unless (null (cdr oblist)) ; don't have to do anything for list of 1 sprite either
          (setf (graphics-sheet-object-list gs)
                (append (remove sprite oblist) (list sprite)))))))
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::place-on-bottom ()
  (let* ((sprite-box (get-sprites))
         (sprite (unless (null sprite-box) (graphics-info sprite-box)))
         (gb (unless (null sprite) (assoc-graphics-box sprite))))
    (unless (null gb)
      (let* ((gs (graphics-sheet gb))
             (oblist (when (graphics-sheet? gs) (graphics-sheet-object-list gs))))
        (unless (null (cdr oblist)) ; don't have to do anything for list of 1 sprite either
          (setf (graphics-sheet-object-list gs)
                (append (list sprite) (remove sprite oblist)))))))
  boxer-eval::*novalue*)

