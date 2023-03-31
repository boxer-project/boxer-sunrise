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
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;    Lispworks CAPI versions of Clipboard functionality
;;;;

(in-package :boxer-lw-capi)

;; uncolor = result of color:unconvert-color which is a simple-vector with components of
;; :RGB, red, green, blue & alpha in 0->1.0 floa format
(defun uncolor->pixel (uncolor)
  (flet ((convert-color-component (cc)
                                  (floor (* cc 255))))
        (boxer::make-offscreen-pixel (convert-color-component (svref uncolor 1))
                                      (convert-color-component (svref uncolor 2))
                                      (convert-color-component (svref uncolor 3))
                                      (convert-color-component (svref uncolor 4)))))

;;; capi:clipboard returns IMAGES
(defun copy-image-to-bitmap (image bm w h)
  (let ((ia (gp:make-image-access bw::*boxer-pane* image))
        (bdata (boxer::ogl-pixmap-data bm)))
    (unwind-protect
     (progn
      (gp::image-access-transfer-from-image ia)
      (dotimes (y h)
        (dotimes (x w)
          (setf
            (cffi:mem-aref bdata boxer::*pixmap-ffi-type* (+ x (* (- h y 1) w)))
                (uncolor->pixel
                  (color:unconvert-color bw::*boxer-pane* (gp:image-access-pixel ia x y)))))))
     (gp:free-image-access ia))))

(defun image-to-bitmap (image)
  (unless (null image)
    (let* ((wid (gp:image-width image)) (hei (gp:image-height image))
           (bim (boxer::make-ogl-pixmap wid hei)))
      (copy-image-to-bitmap image bim wid hei)
      (values bim wid hei))))

;;; System clipboard

(defun paste-text ()
  (let ((string (capi::clipboard bw::*boxer-pane* :string)))
    (unless (null string)
      (dotimes (i (length string))
        (let ((char (aref string i)))
          (if (member char '(#\Newline #\Return #\Linefeed))
              (boxer::insert-row boxer::*point*
                                 (boxer::make-initialized-row) :moving)
              (boxer::insert-cha boxer::*point* char :moving)))))))

(defun paste-pict (&optional (img (capi::clipboard bw::*boxer-pane* :image)
                                  img-supplied-p))
  (multiple-value-bind (bm wid hei)
      (image-to-bitmap img)
    (unless (null bm)
      ;; memory leak ?
      (unless img-supplied-p (gp:free-image bw::*boxer-pane* img))
      (let* ((gb (boxer::make-box '(())))
             (gs (boxer::make-graphics-sheet wid hei gb)))
        (setf (boxer::graphics-sheet-bit-array gs) bm)
        (setf (boxer::graphics-sheet-bit-array-dirty? gs) T)
        (setf (boxer::graphics-info gb) gs)
        (setf (boxer::display-style-graphics-mode?
               (boxer::display-style-list gb)) T)
        (boxer::insert-cha boxer::*point* gb :moving)))))

(defmethod paste ((self bw::boxer-frame))
  (cond ((equal '(nil :lisp) (multiple-value-list (capi:clipboard-empty self :value)))
         ;; We are looking an undocumented multiple return value for type :value where
         ;; the second return value will be symbol :lisp if it came from lispworks. If
         ;; this is the case we know we were the last one to set the clipboard and yank
         ;; in our most recent item.
         ;; http://www.lispworks.com/documentation/lw71/CAPI-W/html/capi-w-206.htm#82688
         (boxer::com-yank))
        ((not (capi:clipboard-empty self :string))
         (paste-text))
        ((not (capi:clipboard-empty self :image))
         (paste-pict))
        (t (boxer::com-yank)))
  (boxer::repaint))
