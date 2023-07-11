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
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;
(in-package :boxer)

(defvar *black*   #(:RGB 0.0 0.0 0.0 1.0))
(defvar *white*   #(:RGB 1.0 1.0 1.0 1.0))
(defvar *red*     #(:RGB 1.0 0.0 0.0 1.0))
(defvar *green*   #(:RGB 0.0 1.0 0.0 1.0))
(defvar *blue*    #(:RGB 0.0 0.0 1.0 1.0))
(defvar *yellow*  #(:RGB 1.0 1.0 0.0 1.0))
(defvar *magenta* #(:RGB 1.0 0.0 1.0 1.0))
(defvar *cyan*    #(:RGB 0.0 1.0 1.0 1.0))
(defvar *orange*  #(:RGB 1.0 0.6470585 0.0 1.0))
(defvar *purple*  #(:RGB 0.627451 0.1254902 0.941175 1.0))
(defvar *gray*    #(:RGB 0.752941 0.752941 0.752941 1.0))

;; color variables that boxer uses
(defvar *background-color* *white*)
;; *foreground-color* is in boxdef.lisp
(defvar *default-border-color* *black*) ;
(defvar *border-gui-color* *default-border-color*)
(defvar *closet-color* #(:rgb .94 .94 .97 1.0))

(defun rgb-hex->rgb (color)
  (let ((hexstring (aref color 1)))
    `#(:rgb ,(/ (parse-integer (subseq hexstring 1 3) :radix 16) 255)
            ,(/ (parse-integer (subseq hexstring 3 5) :radix 16) 255)
            ,(/ (parse-integer (subseq hexstring 5 7) :radix 16) 255)
            1.0)))

(defun rgb->rgb-hex (rgb-color)
  (let* ((red (floor (* 255 (aref rgb-color 1))))
        (green (floor (* 255 (aref rgb-color 2))))
        (blue (floor (* 255 (aref rgb-color 3))))
        (hex-string (format nil "#~2,'0x~2,'0x~2,'0x" red green blue)))
  `#(:rgb-hex ,hex-string)))

;; sgithens TODO 2023-07-10 usage of this needs to be converted to just rgb-hex->rgb
(defun rgb-hex->ogl (color)
  (rgb-hex->rgb color)
  ; (bw::ogl-convert-color (rgb-hex->rgb color))
  )

(defun color-red (color) (aref color 1))

(defun color-green (color) (aref color 2))

(defun color-blue (color) (aref color 3))

(defun color-alpha (color) (aref color 4))

(defun %make-color (red green blue &optional alpha)
  (%make-color-from-100s red green blue (or alpha 100.0)))

(defun %make-color-from-bytes (red green blue &optional (alpha 255))
  `#(:rgb ,(/ red 255.0) ,(/ green 255.0) ,(/ blue 255.0) ,(/ alpha 255.0)))

(defun %make-color-from-100s (red green blue alpha)
  `#(:rgb ,(/ red   100.0) ,(/ green 100.0) ,(/ blue  100.0) ,(/ alpha 100.0)))

(defun pixel-rgb-values (pixel)
  "Returns a list of RGB values for the dumper.
Should return the values in the boxer 0->100 range (floats are OK)"
  (list (* (aref pixel 1)   100)
        (* (aref pixel 2) 100)
        (* (aref pixel 3)  100)
        (* (aref pixel 4) 100)))

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

;; we are comparing WIN32::COLORREF's not COLOR:COLOR-SPEC's
;; so use WIN32:COLOR= instead of COLOR:COLORS=
(defun color= (c1 c2)
  nil ;; sgithens TODO 2023-07-10 Removing ogl-colors
  ; (if (and c1 c2) ; these  can't be nil
  ;   (bw::ogl-color= c1 c2))
    )
