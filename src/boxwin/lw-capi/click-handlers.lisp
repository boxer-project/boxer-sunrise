;;;;      Boxer
;;;;      Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
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
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;  This file contains the somewhat repetive click handlers for the main
;;;;  Boxer Window capi:define-interface instantiaion.

(in-package :boxer-window)

;; these are interface stubs, the main work is done after
;; Single Clicks...
;; the only work done here is to setup *mouse-down-p* for use by MOUSE-BUTTONS

(defconstant *click-1-byte-selector* (byte 1 0))
(defconstant *click-2-byte-selector* (byte 1 1))
(defconstant *click-3-byte-selector* (byte 1 2))

(defvar *mouse-down-p* nil)

(defvar *modern-press-no-1* 0
  "This will determine which press we're on, so we can emit click or double click
   on the release. Possible values are:
   - 0    Nothing is going on with the mouse
   - 1    The mouse has been pressed once
   - 2    The mouse has been pressed twice

   This supports the new Mouse 2021 Click Events.")
(defvar *modern-press-coords-1* '(0 . 0))
(defvar *modern-press-bits-1* 0
  "This records whether Control, Shift, or some combination of them
  are being held down during the mouse actions.")

(defvar *modern-press-no-2* 0)
(defvar *modern-press-coords-2* '(0 . 0))
(defvar *modern-press-bits-2* 0)

(defvar *modern-press-no-3* 0)
(defvar *modern-press-coords-3* '(0 . 0))
(defvar *modern-press-bits-3* 0)

(defun check-click-wander (coord1 coord2)
  "Takes 2 dotted lists of '(x . y) coordinates, and checks to see if they are within
  the *double-click-wander* tolerance. Returns t if it is within the bounds, nil
  otherwise."
  (and (boxer::<& (boxer::abs& (boxer::-& (car coord1) (car coord2)))
                  *double-click-wander*)
       (boxer::<& (boxer::abs& (boxer::-& (cdr coord1) (cdr coord2)))
                  *double-click-wander*)))

(defun boxer-click-1-handler (w x y &key (bits 0))
  (declare (ignore w))
  (setq *modern-press-no-1* 1)
  (setq *modern-press-bits-1* bits)
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 1))
        (t (setq *mouse-down-p* (dpb 1 *click-1-byte-selector* *mouse-down-p*))))
  (setf *modern-press-coords-1* (cons x y))
  (if *use-mouse2021*
    (boxer-click-handler x y 6 nil bits)  ;; left down
    (boxer-click-handler x y 0 nil bits)))

(defun boxer-click-2-handler (w x y &key (bits 0))
  (declare (ignore w))
  (setq *modern-press-no-2* 1)
  (setq *modern-press-bits-2* bits)
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 2))
        (t (setq *mouse-down-p* (dpb 1 *click-2-byte-selector* *mouse-down-p*))))
  (setf *modern-press-coords-2* (cons x y))
  (if *use-mouse2021*
    (boxer-click-handler x y 7 nil bits)  ;; middle down
    (boxer-click-handler x y 1 nil bits)))

(defun boxer-click-3-handler (w x y &key (bits 0))
  (declare (ignore w))
  (setq *modern-press-no-3* 1)
  (setq *modern-press-bits-3* bits)
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 4))
        (t (setq *mouse-down-p* (dpb 1 *click-3-byte-selector* *mouse-down-p*))))
  (setf *modern-press-coords-3* (cons x y))
  (if *use-mouse2021*
    (boxer-click-handler x y 8 nil bits)  ;; right down
    (boxer-click-handler x y 2 nil bits)))

;; shifted singled clicks...

(defun boxer-c-click-1-handler (w x y)
  (boxer-click-1-handler w x y :bits 1))

(defun boxer-c-click-2-handler (w x y)
  (boxer-click-2-handler w x y :bits 1))

(defun boxer-c-click-3-handler (w x y)
  (boxer-click-3-handler w x y :bits 1))

(defun boxer-a-click-1-handler (w x y)
  (boxer-click-1-handler w x y :bits 2))

(defun boxer-a-click-2-handler (w x y)
  (boxer-click-2-handler w x y :bits 2))

(defun boxer-a-click-3-handler (w x y)
  (boxer-click-3-handler w x y :bits 2))

(defun boxer-c-a-click-1-handler (w x y)
  (boxer-click-1-handler w x y :bits 3))

(defun boxer-c-a-click-2-handler (w x y)
  (boxer-click-2-handler w x y :bits 3))

(defun boxer-c-a-click-3-handler (w x y)
  (boxer-click-3-handler w x y :bits 3))

;; The `os` click handler will be the Command key on MacOS, and
;; Window key on Win10.
(defun boxer-os-click-1-handler (w x y)
  (boxer-click-1-handler w x y :bits 4))

(defun boxer-os-click-2-handler (w x y)
  (boxer-click-2-handler w x y :bits 4))

(defun boxer-os-click-3-handler (w x y)
  (boxer-click-3-handler w x y :bits 4))


(defun control-log (w x y) (format t "~%Control Click"))
(defun meta-log (w x y) (format t "~%Meta Click"))
(defun hyper-log (w x y) (format t "~%Hyper Click"))


;; double clicks
(defun boxer-dclick-1-handler (w x y &key (bits 0))
  (declare (ignore w))
  (setq *mouse-down-p* 1)
  (setq *modern-press-no-1* 2)
  (setq *modern-press-bits-1* bits)
  (if (not *use-mouse2021*)
    (boxer-click-handler x y 0 t bits)))

(defun boxer-dclick-2-handler (w x y &key (bits 0))
  (declare (ignore w))
  (setq *mouse-down-p* 2)
  (setq *modern-press-no-2* 2)
  (setq *modern-press-bits-2* bits)
  (if (not *use-mouse2021*)
    (boxer-click-handler x y 1 t bits)))

(defun boxer-dclick-3-handler (w x y &key (bits 0))
  (declare (ignore w))
  (setq *mouse-down-p* 4)
  (setq *modern-press-no-3* 2)
  (setq *modern-press-bits-3* bits)
  (if (not *use-mouse2021*)
    (boxer-click-handler x y 2 t bits)))

;; shifted double clicks
(defun boxer-c-dclick-1-handler (w x y)
  (boxer-dclick-1-handler w x y :bits 1))

(defun boxer-c-dclick-2-handler (w x y)
  (boxer-dclick-2-handler w x y :bits 1))

(defun boxer-c-dclick-3-handler (w x y)
  (boxer-dclick-3-handler w x y :bits 1))

(defun boxer-a-dclick-1-handler (w x y)
  (boxer-dclick-1-handler w x y :bits 2))

(defun boxer-a-dclick-2-handler (w x y)
  (boxer-dclick-2-handler w x y :bits 2))

(defun boxer-a-dclick-3-handler (w x y)
  (boxer-dclick-3-handler w x y :bits 2))

(defun boxer-c-a-dclick-1-handler (w x y)
  (boxer-dclick-1-handler w x y :bits 3))

(defun boxer-c-a-dclick-2-handler (w x y)
  (boxer-dclick-2-handler w x y :bits 3))

(defun boxer-c-a-dclick-3-handler (w x y)
  (boxer-dclick-3-handler w x y :bits 3))

(defun boxer-os-dclick-1-handler (w x y)
  (boxer-dclick-1-handler w x y :bits 4))

(defun boxer-os-dclick-2-handler (w x y)
  (boxer-dclick-2-handler w x y :bits 4))

(defun boxer-os-dclick-3-handler (w x y)
  (boxer-dclick-3-handler w x y :bits 4))

;; this is called by the (:button :release) input type
(defun boxer-mouse-release-1-handler (w x y)
  (declare (ignore w x y))
  (when *use-mouse2021*
    (cond ((= *modern-press-no-1* 1)
           (boxer-click-handler x y 9 nil *modern-press-bits-1*)     ;; left up
           (if (check-click-wander *modern-press-coords-1* (cons x y))
             (boxer-click-handler x y 0 nil *modern-press-bits-1*)))    ;; left click
          ((= *modern-press-no-1* 2)
           (setq *modern-press-no-1* 0)
           (if (check-click-wander *modern-press-coords-1* (cons x y))
             (boxer-click-handler x y 0 t *modern-press-bits-1*)))  ;; left double click
          (t nil)))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 1) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-1-byte-selector* *mouse-down-p*)))))

(defun boxer-mouse-release-2-handler (w x y)
  (declare (ignore w x y))
  (when *use-mouse2021*
    (cond ((= *modern-press-no-2* 1)
           (boxer-click-handler x y 10 nil *modern-press-bits-2*)    ;; middle up
           (if (check-click-wander *modern-press-coords-2* (cons x y))
             (boxer-click-handler x y 1 nil *modern-press-bits-2*)))    ;; middle click
          ((= *modern-press-no-2* 2)
           (setq *modern-press-no-2* 0)
           (if (check-click-wander *modern-press-coords-2* (cons x y))
             (boxer-click-handler x y 1 t *modern-press-bits-2*)))  ;; middle double click
          (t nil)))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 2) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-2-byte-selector* *mouse-down-p*)))))

(defun boxer-mouse-release-3-handler (w x y)
  (declare (ignore w x y))
  (when *use-mouse2021*
    (cond ((= *modern-press-no-3* 1)
           (boxer-click-handler x y 11 nil *modern-press-bits-3*)    ;; right up
           (if (check-click-wander *modern-press-coords-3* (cons x y))
             (boxer-click-handler x y 2 nil *modern-press-bits-3*)))    ;; right click
          ((= *modern-press-no-3* 2)
           (setq *modern-press-no-3* 0)
           (if (check-click-wander *modern-press-coords-3* (cons x y))
             (boxer-click-handler x y 2 t *modern-press-bits-3*)))  ;; right double click
          (t nil)))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 4) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-3-byte-selector* *mouse-down-p*)))))
