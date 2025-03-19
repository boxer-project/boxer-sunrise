;;;;
;;;;      Boxer
;;;;      Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;      Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those ;;;;  portions may be
;;;;      used for any purpose, including commercial ones, providing that notice of MIT copyright is ;;;;  retained.
;;;;
;;;;      Licensed under the 3-Clause BSD license. You may not use this file except in compliance with ;;;;  this license.
;;;;
;;;;      https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                               +-Data--+
;;;;                      This file is part of the | BOXER | system
;;;;                                               +-------+
;;;;
;;;;        Drawing routines for Boxtops.
;;;;
(in-package :boxer)

(defun boxtop-size (boxtop editor-box)
  (let ((bp (getprop editor-box :boxtop)))
    (cond ((eq bp :name-only) (text-boxtop-size boxtop t))
      ((eq bp :folder)    (folder-boxtop-size boxtop))
      ((eq bp :framed)    (graphics-boxtop-size boxtop t))
      ((graphics-sheet? boxtop) (graphics-boxtop-size boxtop))
      ((eq bp :xref)      (xref-boxtop-size boxtop))
      ;; Note: we can find :FILE inside mac files.  It means to
      ;; draw the boxer file icon
      ((eq bp :file)      (file-boxtop-size boxtop))
      (t (Error "Don't know how to handle boxtop ~A using ~S" boxtop bp)))))


(defun graphics-boxtop-size (gs &optional framed?)
  (if framed?
    (values (+& (graphics-sheet-draw-wid gs) 2)
            (+& (graphics-sheet-draw-hei gs) 2))
    (values (graphics-sheet-draw-wid gs) (graphics-sheet-draw-hei gs))))

(defun text-boxtop-size (text &optional framed?)
  (let ((textwid (if (listp text)
                   (reduce #'(lambda (a b)
                                     (max (string-wid *boxtop-text-font* a)
                                          (string-wid *boxtop-text-font* b)))
                           text)
                   (string-wid *boxtop-text-font* text)))
        (texthei (if (listp text)
                   ;(reduce #'(lambda (a b) (+& (string-hei *boxtop-text-font*)
                   ;                            (string-hei *boxtop-text-font*)))
                   ;        text)
                   (*& (string-hei *boxtop-text-font*) (length text))
                   (string-hei *boxtop-text-font*))))
    (if framed?
      (values (+ (ceiling textwid) 4) (+ texthei 4))
      (values (ceiling textwid) texthei))))

(defvar *folder-graphic-width* 30)
(defvar *folder-graphic-height* 20)
(defvar *folder-graphic-tab-height* 5)
(defvar *folder-graphic-tab-width* 10)
(defvar *folder-graphic-tab-delta* 5)

(defun folder-graphics-wid () *folder-graphic-width*)
(defun folder-graphics-hei ()
  (+& *folder-graphic-tab-height* *folder-graphic-height*))

(defun folder-boxtop-size (name)
  (multiple-value-bind (name-wid name-hei) (text-boxtop-size name)
                       (values (max (folder-graphics-wid) name-wid)
                               (+ (folder-graphics-hei) name-hei))))

;; 32x32 are the dimensions of a standard mac icon
(defun xref-boxtop-size (xref)
  (let* ((pathname (xref-pathname xref))
         (name (unless (null pathname) (pathname-name pathname))))
    (values (max 32 (ceiling (string-wid *boxtop-text-font* name)))
            (+ 32   (string-hei *boxtop-text-font*)))))

(defvar *file-boxtop-margin* 3)

;; means to make it look like an system icon with the name underneath
(defun file-boxtop-size (boxtop)
  (values (max& 32 (+ *file-boxtop-margin*
                      (ceiling (string-wid *boxtop-text-font* boxtop))))
          (+ 32 (string-hei *boxtop-text-font*))))

(defun draw-folder-graphic (x y)
  ;; the tab
  (draw-line x (+ y *folder-graphic-tab-height*)
             (+ x *folder-graphic-tab-delta*) y)
  (draw-line (+ x *folder-graphic-tab-delta*) y
             (+ x *folder-graphic-tab-delta* *folder-graphic-tab-width*) y)
  (draw-line (+ x *folder-graphic-tab-delta* *folder-graphic-tab-width*) y
             (+ x (*& 2 *folder-graphic-tab-delta*) *folder-graphic-tab-width*)
             (+ y *folder-graphic-tab-height*))
  ;; the main rectangle (l,t,r,b)
  (draw-rectangle 1 *folder-graphic-height*
                  x (+ y *folder-graphic-tab-height*))
  (draw-rectangle *folder-graphic-width* 1
                  x (+ y *folder-graphic-tab-height*))
  (draw-rectangle 1 *folder-graphic-height*
                  (+ x *folder-graphic-width* -1)
                  (+ y *folder-graphic-tab-height*))
  (draw-rectangle *folder-graphic-width* 1
                  x (+ y *folder-graphic-tab-height* *folder-graphic-height*)))

;; Note that the clipping, and origin has already been setup inside the redisplay
(defmethod draw-boxtop ((self screen-box) boxtop editor-box x y wid hei)
  (let ((bp (getprop editor-box :boxtop)))
    (cond
      ((eq bp :name-only)
       (start-drawing-screen-obj-model self)
       ;; This should be on a mesh that only needs updating if the text changes
       (draw-text-boxtop editor-box boxtop x y wid hei)
       (stop-drawing-screen-obj-model))
      ((eq bp :folder) (draw-folder-boxtop editor-box boxtop x y))
      ((eq bp :framed) (draw-graphics-boxtop self boxtop x y wid hei t))
      ((graphics-sheet? boxtop) (draw-graphics-boxtop self boxtop x y wid hei))
      ((eq bp :xref)   (draw-xref-boxtop boxtop x y wid))
      ;; Note: we can find :FILE inside mac files.  It means to
      ;; draw the boxer file icon
      ((eq bp :file)      (draw-file-boxtop boxtop x y wid))
      (t (Error "Don't know how to handle boxtop ~A using ~S" boxtop bp)))))

;; text is always framed (for now)
(defun draw-text-boxtop (actual-obj text x y wid hei)
  (with-border-drawing-styles (actual-obj)
    (draw-rectangle 1 hei x y) ; left
    (draw-rectangle wid 1 x y) ; top
    (draw-rectangle 1 hei (+ x wid -1) y) ; right
    (draw-rectangle wid 1 x (+ y hei -2)) ; bottom ( -2 ?)
    (draw-string *boxtop-text-font* text (+ x 2) (+ y 2))))

(defun draw-folder-boxtop (actual-obj text x y)
  (with-border-drawing-styles (actual-obj)
    (let* ((stringw (ceiling (String-wid *boxtop-text-font* text)))
          (wdiff (- stringw (folder-graphics-wid))))
      (draw-folder-graphic (if (plusp wdiff) (+ x (/ wdiff 2)) x) y)
      (draw-string *boxtop-text-font* text
                  x (+ y (folder-graphics-hei))))))

;;

(defun draw-xref-boxtop (boxtop x y wid)
  (let* ((ic (xref-icon-cache boxtop))
         (mt (xref-mime-type  boxtop))
         (pathname (xref-pathname boxtop))
         (name (unless (null pathname) (pathname-name pathname)))
         (horiz-fudge (/ (- wid 32) 2)))
    (cond ((not (null ic))
           ;; if there is a mac icon, prefer it...
           #-carbon-compat
           (bitblt-to-screen 32 32 ic 0 0 (+ x horiz-fudge) y)
           #+carbon-compat
           (draw-iconref ic (+ x horiz-fudge) y)
           )
      ((not (null mt))
       ;; no icon, so try and do somthing reasonable from the MIME info
       (draw-rectangle 32 32 (+ x horiz-fudge) y))
      (t ; no icon, no mime info
         (draw-rectangle 32 32 (+ x horiz-fudge) y)))
    ;; and now, the name
    (draw-string *boxtop-text-font* name x (+ y 32))))

(defun draw-graphics-boxtop-internal (scr-box boxtop x y wid hei)
  "Takes the screen-box, boxtop graphics-sheet, x, y, wid, and hei and draws the graphics boxtop."
  (unless (null (graphics-sheet-background boxtop))
    (with-pen-color ((graphics-sheet-background boxtop))
      (draw-rectangle wid hei x y)))
  (unless (null (graphics-sheet-bit-array boxtop))
    (bitblt-to-screen wid hei (graphics-sheet-bit-array boxtop) 0 0 x y))
  ;; then handle any sprite graphics...
  (unless (null (graphics-sheet-graphics-list boxtop))
    (with-graphics-vars-bound-internal boxtop
      (if *use-opengl-framebuffers*
        (let ((canvas (get-graphics-canvas-for-screen-obj scr-box wid hei))
              (mesh (get-canvas-mesh scr-box)))
          ;; TODO sgithens 2024-12-20
          ;; Note: This is perhaps a bit of a workaround... The issue is that the graphics list here is
          ;; on the boxtop box of the next lower box in scope, but since the containing box is shrunk it doesn't
          ;; actually have a screen-object attached to it. So if you use something like the 'change' primitive to
          ;; update it, it doesn't actually clear this screen canvas since this is a different box. So... for now
          ;; we are adding copy of the graphics list on THIS screen box and seeing if it changed to determine whether
          ;; or not to clear the graphics canvas.
          (let ((cur-graphics-list (getprop scr-box :boxtop-graphics-list)))
            (when (not (equalp cur-graphics-list (graphics-sheet-graphics-list boxtop)))
              (clear canvas)
              (putprop scr-box (graphics-sheet-graphics-list boxtop) :boxtop-graphics-list)))
          (playback-graphics-list-incrementally (graphics-sheet-graphics-list boxtop) canvas wid hei :mesh mesh)
          (boxer-opengl::draw-canvas-mesh mesh (graphics-canvas-pixmap canvas)))
        (boxer-playback-graphics-list (graphics-sheet-graphics-list boxtop) :translate? t :use-cur-model-matrix t)))))

(defun draw-graphics-boxtop (scr-box boxtop x y wid hei &optional framed?)
  (cond ((null framed?)
         ;; just do the graphics
         (with-model-matrix ((world-internal-matrix scr-box))
           (draw-graphics-boxtop-internal scr-box boxtop x y wid hei)))
    (t (with-model-matrix ((world-internal-matrix scr-box))
         (draw-graphics-boxtop-internal scr-box boxtop x y (- wid 2) (- hei 2)))
       ;; draw the border frame
       (draw-line 0 0 0 hei)
       (draw-line 0 0 wid 0)
       (draw-line 0 hei wid hei)
       (draw-line wid 0 wid hei))))

(defun draw-file-boxtop (boxtop x y wid)
  (let ((horiz-offset (floor (- wid 32) 2)))
    (draw-rectangle 32 32 (+ x horiz-offset) y)
    ;; now, the name
    (draw-string *boxtop-text-font* boxtop x (+ y 32))))
