;;;;
;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                       +-Data--+
;;;;              This file is part of the | BOXER | system
;;;;                                       +-------+
;;;;
;;;;    This file contains methods for clear-box and clearscreen.  Initially this was refactored from grfdfs.lisp because
;;;;    of compile time dependencies between that file and gdispl.lisp. This functionality has been placed after both of
;;;;    them.
;;;;

(in-package :boxer)

(defmethod clear-box ((self box) &key (bitmap-p t) (graphics-list-p t))
  (let ((graphics-sheet (slot-value self 'graphics-info)))
    (unless (null graphics-sheet)
      (let ((graphics-list (graphics-sheet-graphics-list graphics-sheet))
            (bit-array (graphics-sheet-bit-array graphics-sheet))
            (gswid (graphics-sheet-draw-wid graphics-sheet))
            (gshei (graphics-sheet-draw-hei graphics-sheet))
            (bg (graphics-sheet-background graphics-sheet)))
        ;; first, clear the storage for each of the drawing surfaces
        (when (and graphics-list-p (not (null graphics-list)))
          (clear-graphics-list graphics-list))
        (when bitmap-p
          (cond ((not (null bit-array))
                 (clear-offscreen-bitmap bit-array (or bg *background-color*)))
            ((color? bg)
             (setf (graphics-sheet-background graphics-sheet) nil))
            ;; tiling pattern code here
            ))
        ;; Clear any framebuffers if present
        (clear-graphics-canvas self)))))

(defmethod clearscreen ((self box)
                        &optional surface)
  (cond ((eq surface :background)
         (clear-box self :bitmap-p t :graphics-list-p nil))
    ((eq surface :foreground)
     (clear-box self :bitmap-p nil :graphics-list-p t))
    ((eq surface :none)
     (clear-box self :bitmap-p nil :graphics-list-p nil))
    (t (clear-box self))))
