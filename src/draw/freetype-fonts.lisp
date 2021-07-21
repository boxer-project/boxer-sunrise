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
;;;;                                        +-Data--+
;;;;               This file is part of the | BOXER | system
;;;;                                        +-------+
;;;;
;;;;
;;;;   Utilities for Rendering Fonts using Freetype2.

(in-package :boxer)

(defparameter *freetype-faces* nil
  "Hash table of freetype face instances.")

(defparameter *freetype-pixmap-generate-count* 0
  "Number of times we've generaged a pixmap for freetype rendering.  For performance tracking.")

(defparameter *freetype-pixmap-cache* nil
  "Hash table of cached pixmaps for drawing.")

(defun make-freetype-face (font-file)
  (freetype2::new-face (merge-pathnames *capogi-font-directory* font-file)))

(defun load-freetype-faces ()
  "Load initial set of fonts"
  (setf *freetype-faces* (make-hash-table :test #'equal))
  (setf *freetype-pixmap-cache* (make-hash-table :test #'equal))
  (setf (gethash "LiberationSans-Regular" *freetype-faces*) (make-freetype-face "LiberationSans-Regular.ttf"))
  (setf (gethash "LiberationSans-Bold" *freetype-faces*) (make-freetype-face "LiberationSans-Bold.ttf"))
  (setf (gethash "LiberationSans-Italic" *freetype-faces*) (make-freetype-face "LiberationSans-Italic.ttf"))
  (setf (gethash "LiberationSans-BoldItalic" *freetype-faces*) (make-freetype-face "LiberationSans-BoldItalic.ttf"))

  (setf (gethash "LiberationMono-Regular" *freetype-faces*) (make-freetype-face "LiberationMono-Regular.ttf"))
  (setf (gethash "LiberationMono-Bold" *freetype-faces*) (make-freetype-face "LiberationMono-Bold.ttf"))
  (setf (gethash "LiberationMono-Italic" *freetype-faces*) (make-freetype-face "LiberationMono-Italic.ttf"))
  (setf (gethash "LiberationMono-BoldItalic" *freetype-faces*) (make-freetype-face "LiberationMono-BoldItalic.ttf"))

  (setf (gethash "LiberationSerif-Regular" *freetype-faces*) (make-freetype-face "LiberationSerif-Regular.ttf"))
  (setf (gethash "LiberationSerif-Bold" *freetype-faces*) (make-freetype-face "LiberationSerif-Bold.ttf"))
  (setf (gethash "LiberationSerif-Italic" *freetype-faces*) (make-freetype-face "LiberationSerif-Italic.ttf"))
  (setf (gethash "LiberationSerif-BoldItalic" *freetype-faces*) (make-freetype-face "LiberationSerif-BoldItalic.ttf")))

(defun check-fontspec (triple)
  "While we are continuing to refactor fonts in the system, sometimes the font size comes in as zero,
  for now we will reset it to 12."
  (if (equal (cadr triple) 0)
    (list (car triple) 12 (cddr triple))
    triple))

(defun current-freetype-font (current-font &optional (font-zoom 1.0))
  "Returns the current face and size based on the values in *current-font-descriptor*
  The current fonts are:
      'Arial' 'Courier New' 'Times New Roman' 'Verdana'
  The style numbers currently correspond to:
      0 - Plain    1 - Bold     2 - Italic      3 - BoldItalic
  The current font sizes are:
      9, 10, 12, 14, 16, 20, 24
  "
  (let* ((capi-fontspec (check-fontspec (opengl-font-fontspec current-font)))
         (name (car capi-fontspec))
         (size (cadr capi-fontspec))
         (style (cddr capi-fontspec))
         (face-family-name nil) ; ie. LiberationSerif
         (face-style-name nil) ; ie. Regular, Bold, Italic, BoldItalic
         (face-full-name) ; ie. LiberationSans-Bold
         (face nil))
    (cond ((equal name "Times New Roman")
           (setf face-family-name "LiberationSerif"))
          ((equal name "Courier New")
           (setf face-family-name "LiberationMono"))
          (t
           (setf face-family-name "LiberationSans")))
    (cond ((and (member :BOLD style) (member :ITALIC style))
           (setf face-style-name "BoldItalic"))
          ((member :BOLD style)
           (setf face-style-name "Bold"))
          ((member :ITALIC style)
           (setf face-style-name "Italic"))
          (t
           (setf face-style-name "Regular")))
    (setf face (gethash (concatenate 'string face-family-name "-" face-style-name) *freetype-faces*))
    ;; floor'ing the result as even though freetype specifies decimal font sizes right now, our current
    ;; opengl methods require whole numbers
    (freetype2:set-char-size face (floor (* font-zoom (* size 64))) 0 72 72)
    face))

(defun current-rgba-percents (ogl-color)
  `(,(bw::ogl-color-red ogl-color) ,(bw::ogl-color-green ogl-color)
    ,(bw::ogl-color-blue ogl-color) ,(bw::ogl-color-alpha ogl-color)))

(defun font-pixel-color-alpha (pixel-grayscale cur-color)
  "Calculate the fonts pixel color keeping the same color, but using the gray scale to set the alpha value.
   pixel-grayscale - 255 based value
   cur-color - 0 to 1 based RGBA pixels. List of 4 values."
  (let ((final-color 0))
    (setf final-color (dpb pixel-grayscale (byte 8 24) final-color)) ; alpha
    (setf final-color (dpb (floor (* (caddr cur-color) 255)) (byte 8 16) final-color)) ; blue
    (setf final-color (dpb (floor (* (cadr cur-color) 255)) (byte 8 8) final-color))  ; green
    (setf final-color (dpb (floor (* (car cur-color) 255)) (byte 8 0) final-color)) ; red
    final-color))

(defun make-freetype-pixmap (char-to-draw current-font cur-color &optional (font-zoom 1.0))
  (let* ((rendered-array (freetype2::toy-string-to-array (current-freetype-font current-font font-zoom) (string char-to-draw) :left-right))
         (advances (freetype2::get-string-advances (current-freetype-font current-font font-zoom) (string char-to-draw)))
         (width (freetype2::array-dimension rendered-array 1))
         (height (freetype2::array-dimension rendered-array 0))
         (fli-data
           (cffi:foreign-alloc opengl::*pixmap-ffi-type* :initial-element 0 :count (* width height)))
         (*count* 0)
         (*mypixmap* nil)
         (*origval* nil))
    (loop for y downfrom (- height 1) to 0 by 1 do                   ;dotimes (y height)
      (dotimes (x width)
        (setf *origval* (aref rendered-array y x))
        (setf
          (cffi:mem-aref fli-data opengl::*pixmap-ffi-type* (+ (* *count* width) x))
          (font-pixel-color-alpha *origval* cur-color)))
        (setf *count* (1+ *count*)))

    (setf *mypixmap* (opengl::%make-ogl-pixmap :width width :height height
                        :data fli-data ))
    *mypixmap*))

(defun find-freetype-pixmap (char-string current-font cur-ogl-color &optional (font-zoom 1.0))
  "Returns a pixmap ready for rendering. Needs to cache pixmaps based on the following:
   CAPI Font List, Color, Char/String
  "
  (let* ((capi-fontspec (check-fontspec (opengl-font-fontspec current-font)))
         (cur-color (current-rgba-percents cur-ogl-color))
         (cache-key `(,capi-fontspec ,cur-color ,char-string ,font-zoom))
         (cached-pixmap (gethash cache-key *freetype-pixmap-cache*)))
    (unless cached-pixmap
      (setf cached-pixmap (make-freetype-pixmap char-string current-font (current-rgba-percents cur-ogl-color) font-zoom))
      (setf (gethash cache-key *freetype-pixmap-cache*) cached-pixmap)
      (setf *freetype-pixmap-generate-count* (1+ *freetype-pixmap-generate-count*)))
    cached-pixmap))

(defun freetype-draw-char (char-to-draw xcoord ycoord current-font cur-ogl-color &optional (font-zoom 1.0) (adjust-baseline nil))
  (bw::ogl-current-color) ; This is necessary to update the global ogl variable from the gl state
  (opengl:rendering-on (bw::*boxer-pane*)
    (let* ((glyph-pixmap (find-freetype-pixmap char-to-draw current-font cur-ogl-color font-zoom))
           (height (opengl::ogl-pixmap-height glyph-pixmap))
           (widgth (opengl::ogl-pixmap-width glyph-pixmap)))
      (if adjust-baseline
        (opengl::%pixblt-to-screen glyph-pixmap xcoord (- ycoord height) widgth height 0 0)
        (opengl::%pixblt-to-screen glyph-pixmap xcoord ycoord widgth height 0 0)))))
