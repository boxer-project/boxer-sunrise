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
;;;;    Structures and routines for implementation independent font information, such as Font Face, Size, and Styles.
;;;;
(in-package :boxer)

(defvar *capogi-font-directory* nil)

(defvar *current-opengl-font* nil
  "set-font sets, char drawing uses this value")

(defstruct (opengl-font (:constructor %make-opengl-font)
                        (:print-function %print-opengl-font))
  (fontspec nil))

(defun %print-opengl-font (font stream level)
  (declare (ignore level))
  (format stream "#<OGLFont ~A >"
             (opengl-font-fontspec font)))

(defvar *menu-font-sizes* '(8 9 10 11 12 14 16 18 20 22 24 26 28 36 48 72)
  "We support fonts of any size now, but these are the sizes that will be
  available from the drop down menu in the UI.")

(defvar *font-size-baseline* 1.0
  "This is the font zoom percentage. Default is 1, 100% In practice we are allowing this to be somewhere between .5
  and 4... 50% to 400% zooming. This should be a float.")

(defvar *font-cache* nil ;
  "This is a regular list of `opengl-font` structs. Order is important during boxer execution
  because the boxer-font-descriptors use the integer to track fonts. The order is not important
  between executions as the actual font sizes and other information are persisted in the `.box` format.")

;; Must be True Type Fonts
(defvar *font-families* '("Arial" "Courier New" "Times New Roman" "Verdana"))

(defvar *default-font-family* (car *font-families*))

;; available sizes are: 6,8,10,12,14,16,18,24
;; NOTE: we an get size 7 when loading old mac boxes...
(defun %font-size-to-idx (size)
  (cond ((<=& size  7) size) ;; sgithens: support for relative size saving, see comments on *dump-relative-font-sizes?* in dumper.lisp
        ((<=& size  8) 0)
        ((<=& size  9) 1)
        ((<=& size 10) 2)
        ((<=& size 12) 3)
        ((<=& size 14) 4)
        ((<=& size 16) 5)
        ((<=& size 20) 6)
        ((<=& size 24) 7)
        ((<=& size 28) 8)
        ((<=& size 32) 9)
        ((<=& size 40) 10)
        ((<=& size 48) 11)
        ((<=& size 56) 12)
        ((<=& size 64) 13)
        (t 14)))

(defvar *font-sizes* (vector 8 9 10 12 14 16 20 24 28 32 40 48 56 64)
  "Historic font sizes from before the complete transition from relative to absolute font sizes.") ;(vector 6 8 10 12 14 16 18 24)

(defun find-cached-font (font-no)
  (nth font-no *font-cache*))

(defun fontspec->font-no (fontspec)
  (position-if #'(lambda (x) (equal fontspec (opengl-font-fontspec x))) *font-cache*))

;; use an alist for now, we may have to get more complicated if it looks like
;; the number of "foreign" fonts will be large
(defvar *font-family-aliases*
  '(("Geneva" . "Arial") ("Courier" . "Courier New") ("Times" . "Times New Roman") ("Helvetica" . "Arial"))
  "Initialize this variable if you want explicit font translations")

(defun font-family-alias (family-name)
  (cdr (assoc family-name *font-family-aliases* :test #'string-equal)))

(defun make-boxer-font (rawfontspec &key (translate-relative-sizes t))
  "Makes a boxer font using the fontspec, or returns the existing font from the cache. The returned value is an integer
  with the key for the font during this runtime. These keys can change between executions of Boxer and are only meant
  for runtime. Fonts should always be persists as their fontspec.

  In historic versions of Boxer, the integer was an index in to a list of relative font sizes. The keyword parameter
  `translate-relative-sizes` will perform this translation for sizes 1-7. Any sizes about 7 will be their usual size."
  (let* ((alias (font-family-alias (car rawfontspec)))
         (fontname (or alias (car rawfontspec)))
         (fontsize (if (and translate-relative-sizes (< (cadr rawfontspec) 8))
                     (aref *font-sizes* (cadr rawfontspec))
                     (cadr rawfontspec)))
         (fontspec (list* fontname fontsize (cddr rawfontspec)))
         (font-no (fontspec->font-no fontspec))
         (oglfont nil))
    (if font-no
      font-no
      (progn
        (setf oglfont (%make-opengl-font :fontspec fontspec))
        (setf *font-cache* (append *font-cache* (list oglfont)))
        (if (null *current-opengl-font*)
          (setf *current-opengl-font* oglfont))
        (1- (length *font-cache*))))))

;; the external interface, see comsf.lisp for usage
(defun font-style (font-no)
  (cddr (opengl-font-fontspec (find-cached-font font-no))))

;; returns absolute font size
(defun font-size (font-no)
  (let* ((font (find-cached-font font-no))
         (fontspec (opengl-font-fontspec font)))
    (cadr fontspec)))

(defun font-name (font-no)
  (let* ((font (find-cached-font font-no))
         (fontspec (opengl-font-fontspec font)))
    (car fontspec)))

;; these next 3 take a font code and should return a new font code
(defun %set-font (font-no face-name)
  (let* ((font (find-cached-font font-no))
         (fontspec (opengl-font-fontspec font)))
    (make-boxer-font (cons face-name (cdr fontspec)))))

(defun %set-font-size (font-no size)
  (format t "~% %set-font-size font-no: ~a size: ~a" font-no size)
  (let* ((font (find-cached-font font-no))
         (fontspec (opengl-font-fontspec font)))
    (make-boxer-font (append (list (car fontspec) size) (cddr fontspec)))))

(defun %set-font-style (font-no style)
  (format t "~% %set-font-style font-no: ~a style: ~a" font-no style))

(defun normal-font? (font-no)
  (not (font-style font-no)))

(defun bold-font? (font-no)
  (member :bold (font-style font-no)))

(defun italic-font? (font-no)
  (member :italic (font-style font-no)))

(defun font-styles (font-no)
  (font-style font-no))

(defun set-font-style (font-no style to-on?)
  (let* ((font (find-cached-font font-no))
         (fontspec (opengl-font-fontspec font)))
    (cond ((eq style :plain)
           (make-boxer-font (subseq fontspec 0 2)))
          ((eq style :bold)
           (if to-on?
               (make-boxer-font (append (remove :BOLD fontspec) '(:BOLD)))
               (make-boxer-font (remove :BOLD fontspec))))
          ((eq style :italic)
           (if to-on?
               (make-boxer-font (append (remove :ITALIC fontspec) '(:ITALIC)))
               (make-boxer-font (remove :ITALIC fontspec)))))))

(defun initialize-fonts ()
  (let ((arial-12 (boxer::make-boxer-font '("Arial" 12)))
            (arial-16 (boxer::make-boxer-font '("Arial" 16)))
            (arial-16-bold (boxer::make-boxer-font '("Arial" 16 :bold))))
        (setq  boxer::*normal-font-no*           arial-16
                boxer::*default-font*             arial-16
                boxer::*box-border-label-font-no* arial-12
                boxer::*border-label-font*        arial-12
                boxer::*box-border-name-font-no*  arial-16-bold
                boxer::*border-name-font*         arial-16-bold
                boxer::*sprite-type-font-no*      arial-16-bold
                boxer::*initial-graphics-state-current-font-no* arial-16-bold
                boxer::*graphics-state-current-font-no* arial-16-bold
                boxer::*boxtop-text-font*         arial-16-bold)))
