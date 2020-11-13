;; -*- Mode:LISP;Syntax:Common-Lisp; Package:BOXER;-*-

#|


 $Header: comsf.lisp,v 1.0 90/01/24 22:09:10 boxer Exp $


 $Log:	comsf.lisp,v $
;;;Revision 1.0  90/01/24  22:09:10  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


    This file contains top level definitions for
    BOXER Editor Commands that deal with Fonts and
          character styles




Modification History (most recent at top)

 7/15/13 removed very old #+ font functions to allow removal of unused font defvars
10/06/07 #-opengl all references to add-redisplay-clue
 4/21/03 merged current LW and MCL files
1/22/02 change-region-font-internal rewritten to preserve the font info which is
        not the subject of the change.  Also changed
        change-region-font,style,size, color to use new -internal function
4/09/01 *supported-font-sizes, com-nutri-system, com-fat, com-bloat for lwwin
2/03/00 added normal-font?
9/09/99 change-region-font-internal grab last-bfd values at the beginning
        since those values can be side-effected if the last-bfd coincides
        with a row or region boundary
4/30/99 change-region-font-internal changed to NOT insert the trailing BFD if
        we are at the end of the row
3/29/99 finalized font LW font functions
1/04/99 added LW specific font functions
9/02/98 moved bfd= to editor.lisp, needed to be defined earlier
8/27/98 added current-font-values which return width,height of the current font
5/04/98 changed mac version of com-nutri-system, com-fat and com-bloat to
        use new font implementation
5/01/98 started logging changes: source = boxer version 2.3 (new fonts)


|#

(in-package :boxer)



;;;; Fonts & Styles

(DEFUN CHANGE-CHAS-OVER-VALUES (BP DIRECTION DELIMITER-CHAS FCN)
  (LET ((NOT-FIRST-CHA? NIL))
    (MAP-OVER-CHAS-IN-LINE (BP DIRECTION)
      (LET ((DELIMITER-CHA? (char-member cha delimiter-chas)))
  (cond ((AND (NULL CHA)(NULL NEXT-OR-PREVIOUS-ROW))
         ;; end/beginning of the box
         (RETURN (VALUES ROW CHA-NO)))
        ((AND NOT-FIRST-CHA? (NULL CHA))
         ;; end/beginning of the line
         (RETURN (VALUES ROW CHA-NO)))
        ((and not-first-cha? (box? cha))
         ;; go only one box at a time
        (return (values row cha-no)))
        ((AND NOT-FIRST-CHA? DELIMITER-CHA?)
         ;; end of the word
         (RETURN (VALUES ROW CHA-NO)))
        ((NOT DELIMITER-CHA?)
         ;; beginning of word
         (SETQ NOT-FIRST-CHA? T)
         (CHANGE-CHA-AT-CHA-NO ROW CHA-NO
         (funcall FCN (CHA-AT-CHA-NO ROW CHA-NO))))
        (T
         ;; delimiter chas before word
         (CHANGE-CHA-AT-CHA-NO ROW CHA-NO
         (funcall FCN (CHA-AT-CHA-NO ROW CHA-NO)))))))))




;;;; lower level utilities for handling fonts
;; perhaps these belong in a lower level file like draw-low-xxx ?
#+mcl
(progn
  (defun normal-font? (boxer-font)
    (zerop& (font-style boxer-font)))

  (defun bold-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 1))))

  (defun italic-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 2))))

  (defun underline-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 4))))

  (defun outline-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 8))))

  (defun shadow-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 16))))

  (defun condense-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 32))))

  (defun extend-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 64))))

  ;; like typep for font styles...
  (defun font-stylep (boxer-font style)
    (cond ((or (null style) (eq style :plain))
           (zerop& (font-style boxer-font)))
          (t
           (not (zerop& (logand& (font-style boxer-font)
                                 (case style
                                   (:bold 1) (:italic 2) (:underline 4)
                                   (:outline 8) (:shadow 16)
                                   (:condense 32) (:extend 64))))))))

  (defun font-styles (boxer-font)
    (let ((style-byte (font-style boxer-font))
          (return-styles nil))
      (do* ((pos 1 (ash pos 1))
            (styles '(:bold :italic :underline :outline :shadow :condense :extend)
                    (cdr styles))
            (style (car styles) (car styles)))
          ((null style))
        (unless (zerop& (logand& style-byte pos)) (push style return-styles)))
      (nreverse return-styles)))


  (defun set-font-style (boxer-font style to-on?)
    (cond ((or (null style) (eq style :plain))
           (%set-font-style boxer-font 0))
          (t (let* ((current-style (font-style boxer-font))
                    (new-style (case style
                                   (:bold (dpb& (if to-on? 1 0)
                                                '#.(byte 1 0) current-style))
                                   (:italic (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 1) current-style))
                                   (:underline (dpb& (if to-on? 1 0)
                                                     '#.(byte 1 2) current-style))
                                   (:outline (dpb& (if to-on? 1 0)
                                                   '#.(byte 1 3) current-style))
                                   (:shadow (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 4) current-style))
                                   (:condense (dpb& (if to-on? 1 0)
                                                    '#.(byte 1 5) current-style))
                                   (:extend (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 6) current-style)))))
               (%set-font-style boxer-font new-style)))))
  )

#+lispworks
(progn
  (defun normal-font? (boxer-font)
    (zerop& (font-style boxer-font)))

  (defun bold-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 1))))

  (defun italic-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 2))))

  (defun underline-font? (boxer-font)
    (not (zerop& (logand& (font-style boxer-font) 4))))

  (defun outline-font? (boxer-font) (declare (ignore boxer-font)) nil) ; mac only concept

  (defun shadow-font? (boxer-font) (declare (ignore boxer-font)) nil) ; mac only concept

  (defun condense-font? (boxer-font) (declare (ignore boxer-font)) nil) ; mac only concept

  (defun extend-font? (boxer-font) (declare (ignore boxer-font)) nil) ; mac only concept

  ;; like typep for font styles...
  (defun font-stylep (boxer-font style)
    (cond ((or (null style) (eq style :plain))
           (zerop& (font-style boxer-font)))
          (t
           (not (zerop& (logand& (font-style boxer-font)
                                 (case style
                                   (:bold 1) (:italic 2) (:underline 4))))))))

  (defun font-styles (boxer-font)
    (let ((style-byte (font-style boxer-font))
          (return-styles nil))
      (do* ((pos 1 (ash pos 1))
            (styles '(:bold :italic :underline)
                    (cdr styles))
            (style (car styles) (car styles)))
          ((null style))
        (unless (zerop& (logand& style-byte pos)) (push style return-styles)))
      (nreverse return-styles)))


  (defun set-font-style (boxer-font style to-on?)
    (cond ((or (null style) (eq style :plain))
           (%set-font-style boxer-font 0))
          (t (let* ((current-style (font-style boxer-font))
                    (new-style (case style
                                   (:bold (dpb& (if to-on? 1 0)
                                                '#.(byte 1 0) current-style))
                                   (:italic (dpb& (if to-on? 1 0)
                                                  '#.(byte 1 1) current-style))
                                   (:underline (dpb& (if to-on? 1 0)
                                                     '#.(byte 1 2) current-style)))))
               (%set-font-style boxer-font new-style)))))
  )

;; look for the bfd which specifies the font info for the position
(defun closest-bfd (row cha-no) (closest-bfd-internal (row-fds row) cha-no))

(defun closest-bfd-internal (fds cha-no)
  (let ((cfd *default-font-descriptor*))
    (dolist (fd fds)
      (cond ((>=& cha-no (bfd-cha-no fd)) (setq cfd fd))
            (t (return nil))))
    cfd))

(defun bp-closest-bfd (bp) (closest-bfd (bp-row bp) (bp-cha-no bp)))

; "hood" as in 'hood.  Returns
(defun bfd-hood (row cha-no)
  (let ((fds (row-fds row))
        (cfd *default-font-descriptor*))
    (dolist (fd fds cfd)
      (cond ((>=& cha-no (bfd-cha-no fd)) (setq cfd fd))
            (t (return (values cfd fd)))))))


;; font manipulation across regions...

;; keep the original sizes and styles unless overridden by newfont
(defun new-font-value (oldfont attribute newvalue)
  (ecase attribute
    (:family    (%set-font oldfont newvalue))
    (:size      (%set-font-size oldfont newvalue))
    (:style-on  (set-font-style oldfont newvalue t))
    (:style-off (set-font-style oldfont newvalue nil))))

;; loop through the region changing the particualr font attribute on all the
;; existing BFD's
;; splice out any duplicate BFD's as we go along (alternatively, we could have
;; handled duplicates in a 2nd pass using compact-fds)
;; attribute can be :family, :size, :color, :style-on, or :style-off

(defun change-region-font-internal (region attribute newvalue)
  (let* ((start-bp (interval-start-bp region))
         (closest-bfd (bp-closest-bfd start-bp))
         (oldfont (bfd-font-no closest-bfd))
         (newfont (cond ((eq attribute :color) oldfont)
                        (t (new-font-value oldfont attribute newvalue))))
         (color  (if (eq attribute :color) newvalue (bfd-color closest-bfd)))
         (start-row (bp-row start-bp))
         (stop-bp (interval-stop-bp region))
         (stop-row (bp-row stop-bp))
         (stop-cha-no (bp-cha-no stop-bp))
         (last-bfd (bp-closest-bfd stop-bp))
         ;; grab these values now, because they can be side-effected if the
         ;; last-bfd coincides with a boundary
         (last-font  (bfd-font-no last-bfd))
         (last-color (bfd-color   last-bfd))
         (last-bfd-exact? (= stop-cha-no (bfd-cha-no last-bfd)))) ; 1+ stop-cha ?
    ;; at this point, we know that the region is starting with the correct BFD
    ;; this avoids using DO-REGION-ROWS inside of WITH-REGION-TOP-LEVEL-BPS
    ;; because we can assume that the interval BP's are at the same level
    ;; and correctly ordered now that they are being made ONLY via
    ;; COM-MOUSE-DEFINE-REGION (4/16/98)
    (flet((process-row-fds (row &optional (start 0)
                                   (stop (1+ (length-in-chas row))))
            (let ((prevfont newfont) (prevcolor color) (redundant-fds nil)
                  (existing-start-bfd? nil))
                 (dolist (fd (row-fds row))
                   (cond ((=& start (bfd-cha-no fd))
                          ;; exact match
                          (cond ((eq attribute :color)
                                 (setf (bfd-color fd) color))
                                (t
                                 (setf (bfd-font-no fd)
                                       (setq prevfont
                                             (new-font-value (bfd-font-no fd)
                                                             attribute
                                                             newvalue)))))
                          (setq existing-start-bfd? t))
                         ((<& start (bfd-cha-no fd) stop)
                          (if (and (= prevfont (bfd-font-no fd))
                                   (color= prevcolor (bfd-color fd)))
                              (push fd redundant-fds)
                            ;; otherwise munge the existing FD
                            (cond ((eq attribute :color)
                                   (setf (bfd-color fd) color))
                                  (t
                                   (setf (bfd-font-no fd)
                                         (setq prevfont
                                               (new-font-value (bfd-font-no fd)
                                                               attribute
                                                               newvalue)))))))))
                 ;;
               (when (not existing-start-bfd?)
                 ;;  no FD at the beginning
                 (insert-bfd row (make-cfd start newfont color)))
               ;; now remove any redundant FD's
               (set-fds row (delete-if #'(lambda (rfd)
                                           (fast-memq rfd redundant-fds))
                                       (row-fds row))))
            ;; clue in the redisplay...
            #-opengl(add-redisplay-clue row :font-change)
            (modified row)))
      (cond ((eq start-row stop-row)
             (process-row-fds stop-row (bp-cha-no start-bp) stop-cha-no)
             (unless (or last-bfd-exact?
                         (= stop-cha-no (length-in-chas stop-row)))
               (insert-bfd stop-row (make-cfd stop-cha-no last-font last-color))))
            (t ;; first, handle BFD's on the 1st row
             (process-row-fds start-row (bp-cha-no start-bp))
             ;; now loop through the middle rows placing the correct BFD
             ;; at the beginning of the row and removing any other BFD's
             (do ((row (next-row start-row) (next-row row)))
                 ((or (null row) (eq row stop-row)))
               (process-row-fds row))
             ;; now handle the last row specially-any BFD's after the stop-cha-no
             ;; need to be LEFT ALONE
             ;; Also, insert the correct BFD at the beginning of the row and make
             ;; sure (possibly inserting) that there is a BFD at the end of the
             ;; region which switches back to the original font
             (process-row-fds stop-row 0 stop-cha-no)
             ;; now make sure we have a final BFD
             ;; unless we are at the end of a row, then leave it off so CR's
             ;; won't push the reverting BFD into the next row
             (unless (or last-bfd-exact?
                         (= stop-cha-no (length-in-chas stop-row)))
               (insert-bfd stop-row
                           (make-cfd stop-cha-no last-font last-color))))))))

(defun change-region-font (region newfontname)
  (change-region-font-internal region :family newfontname))

(defun change-region-font-size (region newsize)
  (change-region-font-internal region :size newsize))

(defun change-region-style (region style style-on?)
  (change-region-font-internal region (if style-on? :style-on :style-off) style))

(defun change-region-color (region newcolor)
  (change-region-font-internal region :color newcolor))

;; returns a list of any Font Descriptors beyond the cha-no of the row
(defmethod remaining-bfds ((row row) cha-no)
  (member cha-no (chas-array-fds (slot-value row 'chas-array))
    :test #'(lambda (cha-no bfd) (>& (bfd-cha-no bfd) cha-no))))


;; best guess for character width and height for the current font
;; what about (with-drawing-port *boxer-pane* (with-font-map-bound (*boxer-pane*)

(defun current-font-values (&optional (cha #\M))
  (rebind-font-info ((bfd-font-no *current-font-descriptor*))
    (values (cha-wid cha) (cha-hei))))

#|
(defmethod change-font-between-cha-nos ((row row) new-font-no
          &optional
          (start-cha-no 0)
          (stop-cha-no (length-in-chas row)))
  (let* ((last-font-no 0)
   (chas-array (slot-value row 'chas-array)))
    (cond ((null (chas-array-fds chas-array))
     ;; there are no Font Descriptors in the row so we can
     ;; just insert new ones without checking
     (if  (=& stop-cha-no (chas-array-active-length chas-array))
    ;; we don't need to insert another FD if we
    ;; are at the end of the row
    (push (make-bfd start-cha-no new-font-no)
          (chas-array-fds chas-array))
    ;; looks like we have to know when to stop
    (setf (chas-array-fds chas-array)
          (list (make-bfd start-cha-no new-font-no)
          (make-bfd stop-cha-no last-font-no)))))
    (t
     ;; looks like we may have to check and (possibly) alter
     ;; the values of intervening Font Descriptors
     (do* ((fds (chas-array-fds chas-array) (cdr fds))
     (next-fds (cdr fds) (cdr fds))
     (current-fd (car fds) (car fds))
     (next-fd (car next-fds) (car next-fds))
     (inside-new-font? nil)
     (bfd-cha-no (if (null inside-new-font?)
         start-cha-no
         stop-cha-no)))
    ((null next-fds) (setf (cdr fds) (list bfd)))
       (cond ((=& bfd-cha-no (bfd-cha-no current-fd))
        ;; if there already is a FD at the right place, bash
        ;; its values rather than splicing a new FD into the list
        (cond ((null inside-new-font?)
         (setq inside-new-font? t)
         (setq last-font-no (bfd-font-no current-fd))
         (setf (bfd-font-no current-fd)
         (logior& (bfd-font-no current-fd)
            (bfd-font-no bfd))))
        (t
         ;; if there is already a FD where we want
         ;; to stop, then we just leave it alone
         (return))))
       ((>& bfd-cha-no (bfd-cha-no current-fd))
        (cond ((null next-fd)

         (setf (cdr fds) (list bfd))
         (return))
        ((<& bfd-cha-no (bfd-cha-no next-fd))
         ;; we are in between 2 FD's so we should splice in here
         (setf (cdr fds) (cons bfd next-fds))
         (return))
        ((=& bfd-cha-no (bfd-cha-no next-fd))
         (setf (bfd-font-no next-fd)
         (logior& (bfd-font-no next-fd) (bfd-font-no bfd)))
         (return))))
       (t
        ;; record the last-font-no
        (setq last-font-no (bfd-font-no current-fd))))))







  (let ((new-bfd (make-bfd start-cha-no new-font-no)))
    ;; first, insert the Font Descriptor
    (insert-bfd row new-bfd)
    ;; now OR the new-font-no into any intervening Font Descriptors
    (dolist (bfd (remaining-bfds row start-cha-no))
      (cond ((>& (bfd-cha-no bfd) stop-cha-no)
       ;; there are still subsequent FD's down the row so we need
       ;; to add a FD which


|#




(defboxer-command COM-BOLDFACE-FONT-WORD ()
  "Changes the next word to be in boldface. "
  (boxer-editor-error "BOLDFACE-FONT-WORD does not work yet"))

(defboxer-command COM-BOLDFACE-FONT-CHA ()
  "Changes the next word to be in boldface. "
  (boxer-editor-error "BOLDFACE-FONT-CHA does not work yet"))

(defboxer-command COM-ITALICS-FONT-WORD ()
  "Changes the next word to be in italics. "
  (boxer-editor-error "ITALICS-FONT-WORD does not work yet"))

(defboxer-command COM-ITALICS-FONT-CHA ()
  "Changes the next word to be in italics. "
  (boxer-editor-error "ITALICS-FONT-CHA does not work yet"))

#|
(DEFBOXER-COMMAND COM-BOLDFACE-FONT-WORD ()
  "Changes the next word to be in boldface. "
  (WITH-MULTIPLE-EXECUTION
    (MOVE-POINT (BP-CHANGE-STYLE-FORWARD-WORD-VALUES *POINT* ':BOLD)))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-BOLDFACE-FONT-CHA ()
  "Change the next character to be in boldface. "
  (WITH-MULTIPLE-EXECUTION
    (LET ((OLD-CHAR (CHA-AT-CHA-NO (POINT-ROW) (POINT-CHA-NO))))
      (CHANGE-CHA-AT-CHA-NO (POINT-ROW)
      (POINT-CHA-NO)
      (MAKE-CHAR OLD-CHAR
           (CHAR-BITS OLD-CHAR) ':BOLD (CHAR-FONT-FAMILY OLD-CHAR)))
      (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-ITALICS-FONT-WORD ()
  "Changes the next word to be in italics. "
  (WITH-MULTIPLE-EXECUTION
    (MOVE-POINT (BP-CHANGE-STYLE-FORWARD-WORD-VALUES *POINT* :ITALIC)))
  boxer-eval::*novalue*)

(DEFBOXER-COMMAND COM-ITALICS-FONT-CHA ()
  "Change the next character to be in italics. "
  (WITH-MULTIPLE-EXECUTION
    (LET ((OLD-CHAR (CHA-AT-CHA-NO (POINT-ROW) (POINT-CHA-NO))))
      (CHANGE-CHA-AT-CHA-NO (POINT-ROW)
      (POINT-CHA-NO)
      (MAKE-CHAR OLD-CHAR
           (CHAR-BITS OLD-CHAR) ':ITALIC (CHAR-FONT-FAMILY OLD-CHAR)))
      (MOVE-POINT (BP-FORWARD-CHA-VALUES *POINT*))))
  boxer-eval::*novalue*)


|#



;;;; Capitalization

(defun boxer-char-upcase (cha)
  (if (characterp cha)
      (char-upcase cha)
      cha))

(defun boxer-char-downcase (cha)
  (if (characterp cha)
      (char-downcase cha)
      cha))

(defun uppercase-region (&optional (region (or *region-being-defined*
                                               (get-current-region))))
  (with-region-top-level-bps (region)
    (do-region-chas (cha region) (change-cha (boxer-char-upcase cha)))
    (do-region-rows (row region) (modified row)))
  (reset-region))

(defun lowercase-region (&optional (region (or *region-being-defined*
                                               (get-current-region))))
  (with-region-top-level-bps (region)
    (do-region-chas (cha region) (change-cha (boxer-char-downcase cha)))
    (do-region-rows (row region) (modified row)))
  (reset-region))

(defun capitalize-region (&optional (region (or *region-being-defined*
                                               (get-current-region))))
    (with-region-top-level-bps (region)
      (let ((next-cap? t))
        (do-region-chas (cha region)
          (cond ((or (box? cha) (char-member cha *word-delimiters*))
                 (setq next-cap? t))
                ((and (characterp cha) next-cap?)
                 (change-cha (boxer-char-upcase cha)) (setq next-cap? nil))
                (t (change-cha (boxer-char-downcase cha))))))
      (do-region-rows (row region) (modified row)))
  (reset-region))

(defun bp-uppercase-forward-word-values (bp)
  (change-chas-over-values bp 1 *word-delimiters* #'boxer-char-upcase))

(defun bp-lowercase-forward-word-values (bp)
  (change-chas-over-values bp 1 *word-delimiters* #'boxer-char-downcase))

(defun bp-capitalize-forward-word-values (bp)
  (when (member (bp-cha bp) *word-delimiters*)
    (do () ((not (member (bp-cha bp) *word-delimiters*)))
      (move-bp bp (bp-forward-cha-values bp))))
  (change-cha-at-cha-no (bp-row bp)
  (bp-cha-no bp)
  (boxer-char-upcase (cha-at-cha-no (bp-row bp) (bp-cha-no bp))))
  (move-bp bp (bp-forward-cha-values bp))
  (unless (member (bp-cha bp) *word-delimiters*)
    (move-bp bp (bp-lowercase-forward-word-values bp)))
  (values (bp-row bp) (bp-cha-no bp)))

(defboxer-command COM-UPPERCASE-WORD ()
  "Uppercases one or more words forward. "
  (if (null *region-being-defined*)
      (with-multiple-execution
        (move-point (bp-uppercase-forward-word-values *point*)))
      (uppercase-region))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defboxer-command COM-LOWERCASE-WORD ()
  "Changes one or more words forward to be in lowercase. "
  (if (null *region-being-defined*)
      (with-multiple-execution
        (move-point (bp-lowercase-forward-word-values *point*)))
      (lowercase-region))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)

(defboxer-command COM-CAPITALIZE-WORD ()
  "Changes one or more words forward to be capitalized on the initial letter.
If started in the middle of a word, capitalizes the current letter."
  (if (null *region-being-defined*)
      (with-multiple-execution
        (move-point (bp-capitalize-forward-word-values *point*)))
      (capitalize-region))
  (mark-file-box-dirty (point-row))
  boxer-eval::*novalue*)


;;; Network stuff

(defboxer-command com-receive-boxer-send ()
  "Inserts box received from a remote Boxer user"
  (mark-file-box-dirty (point-row))
  (receive-boxer-send))

;;;;; Global Fonts

#+mcl
(defvar *macl-standard-font-sizes*)

#+mcl
(defvar *current-macl-font-size* 10)

#+mcl
(defvar *macl-typeface* "courier")

#+mcl
(eval-when (eval load)
  (setq *macl-standard-font-sizes* '(9 10 12 14 18 24))
  (setf (cdr (last *macl-standard-font-sizes*)) *macl-standard-font-sizes*)
  )

#+mcl
(defun next-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (cadr (member *current-macl-font-size*
                                            *macl-standard-font-sizes*)))
        :plain))

#+mcl
(defun larger-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (max *current-macl-font-size*
                                   (cadr (member *current-macl-font-size*
                                                 *macl-standard-font-sizes*))))
        :plain))

#+mcl
(defun smaller-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (min *current-macl-font-size*
                                   (let ((prev (car *macl-standard-font-sizes*))
                                         (here *macl-standard-font-sizes*))
                                     (do* ((where (cdr here) (cdr where))
                                           (size (car where) (car where)))
                                          ((eq where here) prev)
                                       (when (= size *current-macl-font-size*)
                                         (return prev))
                                       (setq prev size)))))
        :plain))

#+mcl
(defun max-font-spec ()
  (list *macl-typeface* (setq *current-macl-font-size*
                              (let ((max 0)
                                    (here *macl-standard-font-sizes*))
                                (do* ((where (cdr here) (cdr where))
                                      (size (car where) (car where)))
                                     ((eq where here) max)
                                  (when (> size max) (setq max size)))))
        :plain))

(defvar *supported-font-sizes*
  #+mcl '(9 10 12 14 18 24) #+lwwin '(6 8 10 12 14 16 18 24))


(defboxer-command COM-NUTRI-SYSTEM ()
  "shrink the fonts"
  ;; mac version just resets *default-font-descriptor*
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size
                                 (reverse *supported-font-sizes*)))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the smallest supported font size"
                                 (car *supported-font-sizes*)))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

(defboxer-command COM-FAT ()
  "make-fonts bigger"
  ;; mac version just resets *default-font-descriptor*
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size *supported-font-sizes*))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the largest supported font size"
                                 (car (last *supported-font-sizes*))))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)

(defboxer-command COM-BLOAT ()
  "bloat the fonts"
  #+(or lwwin mcl)
  (let* ((main-font (bfd-font-no *default-font-descriptor*))
         (current-size (font-size main-font))
         ;; this CONS's, oh well, write the non consing loop out later
         (new-size (cadr (member current-size *supported-font-sizes*))))
    (cond ((null new-size)
           (boxer-editor-warning "~A is the largest supported font size"
                                 (car (last *supported-font-sizes*))))
          (t
           (setf (bfd-font-no *default-font-descriptor*)
                 (make-boxer-font (list #+mcl "Courier"
                                        #+lwwin "Courier New"
                                        new-size)))
           #-opengl(add-redisplay-clue (outermost-box) :clear-screen))))
  boxer-eval::*novalue*)


#+mcl
(defboxer-command com-toggle-font-size ()
  "Cycles through the available font sizes for the Boxer Window"
  (bw::reinitialize-font-map (bw::sheet-font-map *boxer-pane*)
                             (next-font-spec))
  boxer-eval::*novalue*)

(defvar *font-size-state-var* 0)

#-mcl
(defboxer-command com-toggle-font-size ()
  "Cycles through the available font sizes for the Boxer Window"
  (cond ((eql *font-size-state-var* 0)
   (progn
     (setq *font-size-state-var* 1)
     (com-fat)))
  ((eql *font-size-state-var* 1)
   (progn
     (setq *font-size-state-var* 2)
     (com-bloat)))
  (t
   (progn (setq *font-size-state-var* 0)
    (com-nutri-system))))
  boxer-eval::*novalue*)
