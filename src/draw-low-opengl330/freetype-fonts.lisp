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

(defun find-box-glyph (ch current-font &optional (font-zoom 1.0))
  "Returns a box-glyph which includes the openGL texture id for rendering. Needs to cache glyphs based on the following:
   CAPI Font List, Char/String
  "
  (let* ((capi-fontspec (check-fontspec (opengl-font-fontspec current-font)))
         (font-face (current-freetype-font current-font (coerce font-zoom 'single-float)))
         (cache-key `(,capi-fontspec ,ch ,(coerce font-zoom 'single-float)))
         (cached-glyph (gethash cache-key *freetype-pixmap-cache*)))
    (unless cached-glyph
      (setf cached-glyph (create-box-glyph font-face ch))
      (setf (gethash cache-key *freetype-pixmap-cache*) cached-glyph)
      (setf *freetype-pixmap-generate-count* (1+ *freetype-pixmap-generate-count*)))
    cached-glyph))
