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

(defvar *modern-press-no-2* 0)
(defvar *modern-press-no-3* 0)

(defun boxer-click-1-handler (w x y)
  (declare (ignore w))
  (setq *modern-press-no-1* 1)
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 1))
        (t (setq *mouse-down-p* (dpb 1 *click-1-byte-selector* *mouse-down-p*))))
  (if *use-mouse2021*
    (boxer-click-handler x y 6)  ;; left down
    (boxer-click-handler x y 0)))

(defun boxer-click-2-handler (w x y)
  (declare (ignore w))
  (setq *modern-press-no-2* 1)
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 2))
        (t (setq *mouse-down-p* (dpb 1 *click-2-byte-selector* *mouse-down-p*))))
  (if *use-mouse2021*
    (boxer-click-handler x y 7)  ;; middle down
    (boxer-click-handler x y 1)))

(defun boxer-click-3-handler (w x y)
  (declare (ignore w))
  (setq *modern-press-no-3* 1)
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 4))
        (t (setq *mouse-down-p* (dpb 1 *click-3-byte-selector* *mouse-down-p*))))
  (if *use-mouse2021*
    (boxer-click-handler x y 8)  ;; right down
    (boxer-click-handler x y 2)))

;; shifted singled clicks...

(defun boxer-c-click-1-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 1) (boxer-click-handler x y 0 nil 1))

(defun boxer-c-click-2-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 2) (boxer-click-handler x y 1 nil 1))

(defun boxer-c-click-3-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 4) (boxer-click-handler x y 2 nil 1))

(defun boxer-a-click-1-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 1) (boxer-click-handler x y 0 nil 2))

(defun boxer-a-click-2-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 2) (boxer-click-handler x y 1 nil 2))

(defun boxer-a-click-3-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 4) (boxer-click-handler x y 2 nil 2))

(defun boxer-c-a-click-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 nil 3))
(defun boxer-c-a-click-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 nil 3))
(defun boxer-c-a-click-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 nil 3))


;; double clicks
(defun boxer-dclick-1-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 1)
  (setq *modern-press-no-1* 2)
  (if (not *use-mouse2021*)
    (boxer-click-handler x y 0 t)))

(defun boxer-dclick-2-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 2)
  (setq *modern-press-no-2* 2)
  (if (not *use-mouse2021*)
    (boxer-click-handler x y 1 t)))

(defun boxer-dclick-3-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 4)
  (setq *modern-press-no-3* 2)
  (if (not *use-mouse2021*)
    (boxer-click-handler x y 2 t)))

;; shifted double clicks
(defun boxer-c-dclick-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 t 1))
(defun boxer-c-dclick-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 t 1))
(defun boxer-c-dclick-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 t 1))

(defun boxer-a-dclick-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 t 2))
(defun boxer-a-dclick-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 t 2))
(defun boxer-a-dclick-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 t 2))

(defun boxer-c-a-dclick-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 t 3))
(defun boxer-c-a-dclick-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 t 3))
(defun boxer-c-a-dclick-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 t 3))

;; this is called by the (:button :release) input type
(defun boxer-mouse-release-1-handler (w x y)
  (declare (ignore w x y))
  (when *use-mouse2021*
    (cond ((= *modern-press-no-1* 1)
           (boxer-click-handler x y 9)   ;; left up
           (boxer-click-handler x y 0))
          ((= *modern-press-no-1* 2)
           (setq *modern-press-no-1* 0)
           (boxer-click-handler x y 0 t))
          (t nil)))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 1) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-1-byte-selector* *mouse-down-p*)))))

(defun boxer-mouse-release-2-handler (w x y)
  (declare (ignore w x y))
  (when *use-mouse2021*
    (cond ((= *modern-press-no-2* 1)
           (boxer-click-handler x y 10)   ;; middle up
           (boxer-click-handler x y 1))   ;; middle click
          ((= *modern-press-no-2* 2)
           (setq *modern-press-no-2* 0)
           (boxer-click-handler x y 1 t))
          (t nil)))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 2) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-2-byte-selector* *mouse-down-p*)))))

(defun boxer-mouse-release-3-handler (w x y)
  (declare (ignore w x y))
  (when *use-mouse2021*
    (cond ((= *modern-press-no-3* 1)
           (boxer-click-handler x y 11)   ;; right up
           (boxer-click-handler x y 2))   ;; right click
          ((= *modern-press-no-3* 2)
           (setq *modern-press-no-3* 0)
           (boxer-click-handler x y 2 t))
          (t nil)))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 4) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-3-byte-selector* *mouse-down-p*)))))
