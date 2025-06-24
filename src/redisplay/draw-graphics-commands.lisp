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
;;;;         Draw operations for graphics-commands
(in-package :boxer)

(defun ck-mode-draw-line (from-x from-y to-x to-y alu)
 (if (eq %draw-mode ':wrap)
     (boxer-wrap:draw-wrap-line from-x (- from-y) to-x (- to-y))
     (draw-clip-line from-x from-y to-x to-y)))

(defun draw-clip-line (from-x from-y to-x to-y)
  ;(draw-line from-x from-y to-x to-y alu t)
  ;; draw-line will clip the coordinates making the slope wrong...
  (draw-line from-x from-y to-x to-y))

(defdraw-graphics-command (boxer-line-segment x0 y0 x1 y1)
  "Takes a boxer command starting with 35:
    ex #(35 -0.5 -0.5 0.0 0.5)"
  ;; The y-axis needs to be flipped
  (ck-mode-draw-line x0 (- y0) x1 (- y1)
                     *graphics-state-current-alu*))

(defdraw-graphics-command (boxer-centered-string x y text)
  (let ((y (- y)))
    (do* ((height (1+ (string-hei *graphics-state-current-font-no*)))
          (s text (subseq s (let ((p (position #\newline s)))
                              (if (null p) 0 (1+ p)))))
          (trimmed-string (subseq s 0 (position #\newline s))
                          (subseq s 0 (position #\newline s)))
          (width (string-wid *graphics-state-current-font-no* trimmed-string)
                 (string-wid *graphics-state-current-font-no* trimmed-string))
          (wx (- x (/ width 2)) (- x (/ width 2)))
          (wy y (+ wy height)))
      ((not (position #\newline s))
       (draw-string *graphics-state-current-font-no* trimmed-string wx wy))
      (draw-string *graphics-state-current-font-no* trimmed-string wx wy))))

(defdraw-graphics-command (boxer-left-string x y text)
  (let ((y (- y)))
    (loop
      (draw-string *graphics-state-current-font-no*
                    (subseq text 0 (position #\newline text)) x y)
      ;; If we have drawn the last line (the current line has no CR's)
      (if (not (position #\newline text))
        (return)
        (setq text (subseq text (let ((p (position #\newline text)))
                            (if (null p) 0 (1+ p))))
              y (+ y 1 (string-hei *graphics-state-current-font-no*)))))))

(defdraw-graphics-command (boxer-right-string x y text)
  (let ((y (- y))
        (width 0)
        (trimmed-string ""))
    (loop
      (setq trimmed-string (subseq text 0 (position #\newline text))
            width (string-wid *graphics-state-current-font-no* trimmed-string))
      (draw-string *graphics-state-current-font-no* trimmed-string (- x width) y)
      ;; If we have drawn the last line (the current line has no CR's)
      (if (not (position #\newline text))
        (return)
        (setq text (subseq text (let ((p (position #\newline text)))
                                  (if (null p) 0 (1+ p))))
              y (+ y (1+ (string-hei *graphics-state-current-font-no*))))))))

(defdraw-graphics-command (boxer-centered-rectangle x y w h)
    (draw-rectangle w h
                   (- x (/ w 2))
                   ;; The y axis is flipped
                   (- (+ y (/ h 2)))))

;; maybe this should be a circle ? Need to check relative speeds
;; also, should probably use the pen-width ?
(defdraw-graphics-command (boxer-dot x y)
    (draw-rectangle *graphics-state-current-pen-width* *graphics-state-current-pen-width*
                 (- x (/ *graphics-state-current-pen-width* 2))
                 (- (+ y (/ *graphics-state-current-pen-width* 2)))))

(defdraw-graphics-command (boxer-hollow-rectangle x y w h)
  ;; sgithens TODO should we be using *graphics-state-current-pen-width*, rather then boxgl-device-pen-size?
  ;;               Currently this doens't seem to be running in the graphics context that has that bound.
  (let* ((pen-size (boxgl-device-pen-size bw::*boxgl-device*))
         (right    (- (+ x (/ w 2)) (/ pen-size 2)))
         (left     (+ (- x (/ w 2)) (/ pen-size 2)))
         ;; Full right and left are flush with edge for the top
         ;; and bottom line, otherwise there is rectangle missing
         ;; from the corner.
         (full-right (+ x (/ w 2)))
         (full-left  (- x (/ w 2)))
         ;; The y axis is flipped
         (top      (- (- (+ y (/ h 2)) (/ pen-size 2))))
         (bottom   (- (+ (- y (/ h 2)) (/ pen-size 2)))))
    (draw-line right top right bottom)
    (draw-line full-right bottom full-left bottom)
    (draw-line left bottom left top)
    (draw-line full-left top full-right top)))

(defdraw-graphics-command (boxer-centered-bitmap bitmap x y wid hei)
    (bitblt-to-screen wid hei bitmap 0 0
                      (- x (floor wid 2))
                      (- (+ y (floor hei 2)))))

(defdraw-graphics-command (boxer-wedge x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (draw-c-arc x y radius start-angle sweep-angle t))))

(defdraw-graphics-command (boxer-arc x y radius start-angle sweep-angle)
  (let ((y (- y)))
    (when (plusp radius)
      (draw-c-arc x y radius start-angle sweep-angle nil))))

(defdraw-graphics-command (boxer-filled-ellipse x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h t)))

(defdraw-graphics-command (boxer-ellipse x y w h)
  (let ((y (- y)))
    (draw-ellipse x y w h nil)))

(defdraw-graphics-command (boxer-filled-circle x y radius)
  (let ((y (- y)))
    (draw-circle x y radius t)))

(defdraw-graphics-command (boxer-circle x y radius)
  (let ((y (- y)))
    (draw-circle x y radius nil)))
