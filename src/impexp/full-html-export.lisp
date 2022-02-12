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
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;         Full HTML Export - New HTML Export for 2021 that contains a full copy of all data
;;;;                            in the boxer structures, as well as flexible CSS classes and some
;;;;                            minimal javascript interaction.
;;;;
;;;;  TODO: Add in a general utility for espacping strings for special purpose HTML Characters

(in-package :boxer)

(defclass full-html-file-class
  (foreign-file-class)
  ())

(defmethod begin-foreign-stream ((ffc full-html-file-class)
                                 &optional (stream *standard-output*))
  (format stream "<html><head>
    <style>
        .box legend {
            border: thin solid gray;
            background-color: white;

        }

        .box {
            border: thin black solid;
            width: max-content;
            display: inline;
            vertical-align: top;
            padding-top: 0px;
            padding-bottom: 5px;
            padding-left: 8px;
            overflow: auto;
        }

        .box.boxtop-name-only {
            background-color: white;
            border: none;
        }

        .databox {
            border-radius: 10px;
        }

        .doitbox {

        }

        .graphicsbox {
            border-radius: 10px;
        }


        .portbox {
            border-style: double;
            border-width: 6;
        }

        .export-all-variables {
            border-style: dashed;
        }

        .shrunk {
            background-color: gray;

        }

        .box.supershrunk legend {
            display: none;
        }

        .box.supershrunk {
            border-radius: 0px;
            background-color: black;
            width: 10px;
            height: 10px;
            margin: 0px;
            padding: 0px;
        }

        .closet-row {
            background-color: #F2F2F8;
        }

        .closet-row legend {
            background-color: #F2F2F8;
        }
    </style>
  </head><body>"))

(defmethod write-foreign-file-header ((ffc full-html-file-class)
                                      box
                                      &optional (stream *standard-output*)))

(defun get-rgba-values-from-pixmap-pixel (pixel)
  "This is a pixel that comes from the C array allocated by fli and stored in the ogl-pixmap-data. These
  predefined byte specifiers from the opengl code allow retrieval of the bytes with ldb. Luckily, in our
  current situation they are already in the 0 to 255 range we require for each RGBA value."
  (list (ldb opengl::*gl-rgba-rev-red-byte* pixel)
        (ldb opengl::*gl-rgba-rev-green-byte* pixel)
        (ldb opengl::*gl-rgba-rev-blue-byte* pixel)
        (ldb opengl::*gl-rgba-rev-alpha-byte* pixel)))

(defun generate-png-from-ogl-pixmap (pixmap)
  "Using the zpng and qbase64 libraries we generate PNG image formatted data from the ogl-pixmap. This is streamed
  to a base64 input stream, and then converted to a string to go in to the data URL.

  TODO: I believe this is currently being allocated to an entire string, we should look at streaming the base64 output
  straight to the HTML stream so it's not allocating an entire separate string for the image."
  (let* ((width (opengl::ogl-pixmap-width pixmap))
         (height (opengl::ogl-pixmap-height pixmap))
         (data (opengl::ogl-pixmap-data pixmap))
         (cur-pixel nil)
         (png (make-instance 'zpng:pixel-streamed-png
                            :color-type :truecolor-alpha
                            :width width
                            :height height))
         (togo nil))
    (with-output-to-string (s)
      (with-open-stream (stream (make-instance 'qbase64:encode-stream :underlying-stream s))
        (zpng:start-png png stream)
        (dotimes (y height)
          (dotimes (x width)
            (setf cur-pixel (cffi:mem-aref data opengl::*pixmap-ffi-type* (+ x (* (- height y 1) width))))
            (zpng:write-pixel (get-rgba-values-from-pixmap-pixel cur-pixel) png)))
        (zpng:finish-png png))
      (setf togo (get-output-stream-string s)))
    togo))

(defmethod write-foreign-file-box ((ffc full-html-file-class) box stream)
  (let* ((*export-properties* (merge-export-properties
                               *export-properties*
                               (get-export-format-properties box)))
         (nr (name-row box))
         (graphics-mode? (display-style-graphics-mode? (display-style-list box)))
         (fixed-wid (display-style-fixed-wid (display-style-list box)))
         (fixed-hei (display-style-fixed-hei (display-style-list box)))
         (d-style (display-style-style (display-style-list box))) ; :normal :shrunk :supershrunk :boxtop
         (css-classes '("box")))

    ;; Box type css class
    (push (symbol-name (symbol-from-box-type box)) css-classes)

    ;; Box shurnk status style
    (push (symbol-name d-style) css-classes)

    ;; Name only boxtops just appear as the name with a border around them.
    (if (and (equal d-style :shrunk) (equal :NAME-ONLY (getf (plist box) :BOXTOP)))
      (push "boxtop-name-only" css-classes))

    ;; Optional export variables css class
    (if (slot-value box 'exports)
      ;; Are there any other exports than .EXPORT-ALL-VARIABLES. ?
      (push "export-all-variables" css-classes))

    ;; The fixed width and height occur when you manually drag a box to a desired size.
    (if (and fixed-wid fixed-hei)
      (format stream "<fieldset style=\"width: ~f; height: ~f;\" id=\"tick-~a\" class=\"~{~a ~}\">" fixed-wid fixed-hei (actual-obj-tick box) css-classes)
      (format stream "<fieldset id=\"tick-~a\" class=\"~{~a ~}\">" (actual-obj-tick box) css-classes))

    ;; Name row
    (format stream "<legend>~a</legend>" (get-export-namerow-string box))

    ;; This is not a graphics box, so iterate and paint the rows as usual. OR, it is a graphics box, but not
    ;; currently in graphics mode, so we'll render the internal sprite and other boxes since it's flipped to text.
    (when (and (not graphics-mode?) (equal d-style :normal))
      (do ((row (first-inferior-row box) (next-row row)))
        ((null row))
        (write-foreign-file-row ffc row stream)
        (if (next-row row)
          (format stream "<br/>"))))

    ;; if it's a graphics box, let's create the canvas
    (when (and (graphics-box? box) graphics-mode? (equal d-style :normal))
      (let* ((gs (graphics-info box))
             (width (graphics-sheet-draw-wid gs))
             (height (graphics-sheet-draw-hei gs))
             (bit-array-dirty? (graphics-sheet-bit-array-dirty? gs))
             (background-color (if (graphics-sheet-background gs)
                                   (ogl-color-to-css-hex (graphics-sheet-background gs))
                                   (ogl-color-to-css-hex *white*)))
             (glist (graphics-sheet-graphics-list gs))
             (glist-commands (aref glist 0))
             (gobj-list (graphics-sheet-object-list gs))
             (current-pen-color (ogl-color-to-css-hex *black*)))

        (format stream "<svg width=\"~a\" height=\"~a\" xmlns=\"http://www.w3.org/2000/svg\">" width height )
        ;; If it has a bit-array it's likely a raster image, but could have graphics commands as well? Either way,
        ;; we paint the raster image in the SVG. Otherwise we paint a rectangle over the entire graphics using the
        ;; background-color.
        (if (and bit-array-dirty? (graphics-sheet-bit-array gs))
          (let ((base64png (generate-png-from-ogl-pixmap (graphics-sheet-bit-array gs))))
            (format stream "<image href=\"data:image/png;base64,~a\"/>" base64png))
          (format stream "<rect width=\"100%\" height=\"100%\" fill=\"~a\"/>" background-color))
        (loop for command across (remove nil glist-commands)
          do
            (let ((opcode (aref command 0)))
              (cond ((equal opcode 4) ; change-graphics-color
                     (setf current-pen-color (ogl-color-to-css-hex (aref command 1))))
                    (t
                     (generate-svg-from-graphics-command command stream current-pen-color)))))
        (if gobj-list
         (loop for object in (remove nil gobj-list)
          do
            (loop for command across (remove nil (aref (slot-value object 'window-shape) 0))
              do (let ((opcode (aref command 0)))
              (cond ((equal opcode 4) ; change-graphics-color
                     (setf current-pen-color (ogl-color-to-css-hex (aref command 1))))
                    (t
                     (generate-svg-from-graphics-command command stream current-pen-color)))))))
        (format stream "</svg>")))

    (format stream "</fieldset>")))

(defun generate-svg-from-graphics-command (command stream current-pen-color)
  "Taking a graphics command (ex. '(3 1 1 10 10) ) we stream the appropriate svg tag for it.
  These are the commands detailed in gdispl.lisp. Currently this function just takes care of the
  actual drawing commands, not any state change commands such as changing pen-color."
  (let ((opcode (aref command 0)))
    (cond ((equal opcode 3) ; draw-line
           (format stream "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" stroke=\"~a\" />"
                   (aref command 1) (aref command 2) (aref command 3) (aref command 4) current-pen-color))
          ((equal opcode 7) ; centered-string
           (format stream "<text x=\"~f\" y=\"~f\" dominant-baseline=\"hanging\" text-anchor=\"middle\">~a</text>"
                   (aref command 1) (aref command 2) (aref command 3)))
          ((equal opcode 10) ; centered-rectangle
           (let* ((width (aref command 3))
                 (height (aref command 4))
                 (x (- (aref command 1) (/ width 2)))
                 (y (- (aref command 2) (/ height 2))))
             (format stream "<rect x=\"~f\" y=\"~f\" width=\"~f\" height=\"~f\" fill=\"~a\"/>"
                     x y width height current-pen-color)))
          ((equal opcode 15) ; 15 CENTERED-BITMAP (BITMAP X Y WIDTH HEIGHT)
           (let* ((width (aref command 4))
                 (height (aref command 5))
                 (x (- (aref command 2) (/ width 2)))
                 (y (- (aref command 3) (/ height 2))))
             (let ((base64png (generate-png-from-ogl-pixmap (aref command 1))))
               (format stream "<image x=\"~f\" y=\"~f\" width=\"~f\" height=\"~f\" href=\"data:image/png;base64,~a\"/>"
                       x y width height base64png))))
          ((equal opcode 30) ; filled circle
           (format stream "<circle cx=\"~f\" cy=\"~f\" r=\"~f\" fill=\"~a\" />"
                   (aref command 1) (aref command 2) (aref command 3) current-pen-color))
          (t
           (format t "~%HTML Need to Implement Graphics command: ~a" command)))))

(defun ogl-color-to-css-hex (color)
  "Converts one of our internal opengl colors to a hex representation like #ABCC00 thats
   ready to go into some html or css.

   These are percentages so we have to translate them to 0 -> 255 values."
  (let* ((red (floor (* 255 (bw::ogl-color-red color))))
         (green (floor (* 255 (bw::ogl-color-green color))))
         (blue (floor (* 255 (bw::ogl-color-blue color))))
         (alpha (bw::ogl-color-alpha color)))
    (format nil "#~2,'0x~2,'0x~2,'0x" red green blue)))

(defmethod write-foreign-file-row ((ffc full-html-file-class) row stream)
  (let* ((bfd-list (aref (chas-array row) 3))
         (cur-bfd (car bfd-list))
         (cur-color (if cur-bfd
                        (bfd-color cur-bfd)
                        *foreground-color*))
         (cur-css-color (ogl-color-to-css-hex cur-color)))

    (when (closet-row? row (superior-box row))
      (format stream "<div class=\"closet-row\">")
    )

    (do-row-chas ((cha row) (cha-no 0 (+ cha-no 1)))
      ;; TODO: Keeping track of fonts needs a bit more work. This is ludicrously inefficient since we're wrapping
      ;; each letter in a span. We'll need to do a bit of work on traversing the rows to put consective cha's together.
      (when (and cur-bfd (equal cha-no (bfd-cha-no cur-bfd)))
        (setf cur-color (bfd-color cur-bfd))
        (setf cur-css-color (ogl-color-to-css-hex cur-color))
        (when (cdr bfd-list)
            (setf cur-bfd (cadr bfd-list))
            (setf bfd-list (cdr bfd-list))))
      (cond ((box? cha)
             (write-foreign-file-box ffc cha stream))
            (t

             (format stream "<span style=\"color: ~a" cur-css-color)

             (when (not (null cur-bfd))
               (cond ((equal "Arial" (font-name (bfd-font-no cur-bfd)))
                      (format stream "; font-family: 'Arial'"))
                     ((equal "Courier New" (font-name (bfd-font-no cur-bfd)))
                      (format stream "; font-family: 'Courier New'"))
                     ((equal "Times New Roman" (font-name (bfd-font-no cur-bfd)))
                      (format stream "; font-family: 'Times New Roman'"))
                     ((equal "Verdana" (font-name (bfd-font-no cur-bfd)))
                      (format stream "; font-family: 'Verdana'")))
               (format stream "; font-size: ~apx " (font-size (bfd-font-no cur-bfd))))

             (format stream "\">")

             (when (not (null cur-bfd))
               (if (bold-font? (bfd-font-no cur-bfd))
                 (format stream "<strong>"))
               (if (italic-font? (bfd-font-no cur-bfd))
                 (format stream "<em>")))

             (if (equal #\space cha)
               (format stream "&nbsp;")
               (format stream "~a" (html-entities:encode-entities (string cha))))

             (when (not (null cur-bfd))
               (if (bold-font? (bfd-font-no cur-bfd))
                 (format stream "</strong>"))
               (if (italic-font? (bfd-font-no cur-bfd))
                 (format stream "</em>")))

             (format stream "</span>"))))

         (when (closet-row? row (superior-box row))
            (format stream "</div>"))))

(defmethod end-foreign-stream ((ffc full-html-file-class)
                               &optional (stream *standard-output*))
  (format stream "</body></html>"))

(def-export-type full-html-file-class "HTML5" "*.html" :respect-line-breaks t)
