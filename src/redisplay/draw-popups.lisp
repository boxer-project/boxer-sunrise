;;;;  ;-*- Mode:Lisp; Package:boxer; Syntax: Common-Lisp; -*-
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
;;;;
;;;;                                           +-Data--+
;;;;                  This file is part of the | Boxer | System
;;;;                                           +-------+
;;;;
;;;;
;;;;        Code for drawing popup context menus
;;;;
(in-package :boxer)

(defmethod item-size ((item menu-item))
  (values
   (+ (* *menu-item-margin* 2)
       (string-wid (get-menu-item-font item) (slot-value item 'title)))
   ;; leave a pixel above and below
   (+ (string-hei (get-menu-item-font item)) 2)))

(defmethod item-height ((item menu-item))
  (+ (string-hei (get-menu-item-font item)) 2))

(defmethod draw-item ((item menu-item) x y)
  (if (null (slot-value item 'enabled?))
      ;; grey it, if we can
      (with-pen-color (*gray*)
        ;; yuck why isn't gray bound somewhere
        (let ((check (slot-value item 'checked?)))
          (rebind-font-info ((get-menu-item-font item))
            (cond ((null check))
                  ((characterp check)
                   (draw-cha check (+ x 2) (+ y (cha-ascent))))
                  (t (draw-cha *default-check-char*
                               (+ x 2)  (+ y (cha-ascent)))))))
        (draw-string (get-menu-item-font item) (slot-value item 'title)
                     (+ x *menu-item-margin*) (1+ y)))
      (progn
        (with-pen-color (*black*)
          (draw-string (get-menu-item-font item) (slot-value item 'title)
                       (+ x *menu-item-margin*) (1+ y))
          (let ((check (slot-value item 'checked?)))
            (rebind-font-info ((get-menu-item-font item))
              (cond ((null check))
                    ((characterp check)
                     (draw-cha check (+ x 2)  (+ y (cha-ascent))))
                    (t (draw-cha *default-check-char*
                                 (+ x 2)  (+ y (cha-ascent))))))))))
  ;; return height used up
  (+ (string-hei (get-menu-item-font item)) 2))

(defmethod draw-menu ((item active-popup-menu))
  (with-slots (popup-menu x y highlight-rect) item
    (multiple-value-bind (mwid mhei) (menu-size popup-menu)
      (erase-rectangle mwid mhei x y)
      (let ((acc-y (1+ y)))
        (dolist (item (slot-value popup-menu 'items))
          (setq acc-y (+ acc-y (draw-item item x acc-y)))))
      ;; draw the top & left borders
      (draw-rectangle (- mwid 2) 1 x y)
      (draw-rectangle 1 (- mhei 2) x y)
      ;; and the bottom & right stubs
      (draw-rectangle 4 1 x (+ y (- mhei 2)))
      (draw-rectangle 1 4 (+ x (- mwid 2)) y)
      ;; draw the shadow rects
      (draw-rectangle (- mwid 4) 2 (+ x 4) (+ y (- mhei 2)))
      (draw-rectangle 2 (- mhei 4) (+ x (- mwid 2)) (+ y 4))
      ;; If an item is highlighted, draw it
      (when highlight-rect
        (with-pen-color (*blinker-color*)
          (apply #'draw-rectangle highlight-rect))))))

(defmethod draw-doc ((self popup-doc) x y)
  (let* ((swid (ceiling (string-wid *popup-doc-font* (slot-value self 'string))))
         (shei (string-hei *popup-doc-font*))
         (pad (+ *popup-doc-border-width* *popup-doc-padding*))
         (full-pad (* pad 2))
         (full-wid (+ swid full-pad))
         (full-hei (+ shei full-pad)))
    ;; frame (left, top, right and bottom)
    (draw-rectangle *popup-doc-border-width* full-hei x y)
    (draw-rectangle full-wid *popup-doc-border-width* x y)
    (draw-rectangle *popup-doc-border-width* full-hei
                    (- (+ x full-wid) *popup-doc-border-width*) y)
    (draw-rectangle full-wid *popup-doc-border-width*
                    x (- (+ y full-hei) *popup-doc-border-width*))
    ;; background
    (with-pen-color (*popup-doc-color*)
      (draw-rectangle
                      (+ swid (* *popup-doc-padding* 2))
                      (+ shei (* *popup-doc-padding* 2))
                      (+ x *popup-doc-border-width*)
                      (+ y *popup-doc-border-width*)))
    ;; doc string
    (draw-string *popup-doc-font* (slot-value self 'string)
                 (+ x pad) (+ y pad)))
  ;; set the flag
  (setq *popup-doc-on?* t))

  (defmethod erase-doc ((self popup-doc) x y)
  (declare (ignore x y))
  (repaint-window)
  (setq *popup-doc-on?* nil))
