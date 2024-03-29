;;; depends on redisplay module, editor module
;;;; MAKE-PS-FILE is the routine for doing it all,
;;;; there are sub-functions for just a given box.

#|

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+


Modification History (most recent at top)

10/14/06 calls to name changed to calls to name-string
 4/21/03 merged current LW and MCL files, no diffs, updated copyright

|#

(in-package :boxer)

;;;; a flag used to (not) print the outermost box.

(defvar *print-outermost-box* t)
(defvar *turtle-size* 1)
(defvar *ps-file* "~boxer/test.ps")

;;;; macro to do an indent, and then a format command
;;;; using the given 'stream'. "t" is a valid stream
;;;; as it is only used with format
(defmacro ps-infor (stream str &rest args)
  `(progn (ps-indent ,stream)
	  (format ,stream ,str  ,@args)))

;;;; state variables for printing strings. see ps-dump-char
(defvar *printing-a-string* nil)

;;;; dump out the char, putting entering the right
;;;; state.
;;;;     Strings are printed in PS by putting them in parenthesis
;;;; If one is in the middle of printing a string, just append to
;;;; the file.
;;;;     This also takes care of certain special characters that
;;;; must be prefaced with a backslash
(defmacro ps-dump-char (cha s)
  `(progn
     (if (null *printing-a-string* )
	 (progn
	   (setq  *printing-a-string*  t)
	   (ps-infor ,s "(")
	   ))
     (setq  *printing-a-string*  t)
     (cond ((equal ,cha #\) )
	    (format ,s "\\)"))
	   ((equal ,cha #\( )
	    (format ,s "\\("))
	   ((equal ,cha #\\ )
	    (format ,s "\\\\"))
	   (t       (format ,s "~A" ,cha)))))



;;;; postscript constants

(defvar *ps-allow-optimizations* t)  ;;;; PS file size optimization
(defvar *ps-debug* nil)              ;;;; debugging comments in the PS code

;;;; Slots 3 and 4 of the PS table are for ports and graphics boxes.
;;;; they are used internally. The others show how Boxer boxes map
;;;; to the PS types of boxes.

(defvar *data-box-type-constant* 0)
(defvar *do-box-type-constant* 2)
(defvar *sprite-box-type-constant* 1)
(defvar *trans-box-type-constant* 5)

;;;; these should be defined in the site-dependent file
;;;; the HEADER FILE is tacked on ahead of everything
;;;; the HOST is where the file is sent, and the PRINTER
;;;; is the laser printer used
(defvar *ps-header-file*             "/giant/boxer/system/src/header.ps" )
(defvar *ps-postscript-printer*      "paperback")
(defvar *ps-postscript-printer-host* "dewey")




;;;; convert the Boxer box to the PostScript type number for that box
(defmethod ps-b-typ ((box box))
    (cond ((and (data-box? box)
		(eq (display-style-border-style (display-style-list box))
		    :dashed))
	   *trans-box-type-constant*)
	  ((data-box? box)   *data-box-type-constant*)
	  ((sprite-box? box) *sprite-box-type-constant*)
	  ((doit-box? box)   *do-box-type-constant*)))



;;;; this controls the mapping of Boxer distances to PostScript
;;;; distances
(defvar *point-per-pixel* 1.03)
(setq   *point-per-pixel* 1.03)

;;;; These are the borders around boxes. The box gets placed
;;;; outside-space-x pixels over and down outside-space-y pixels
(defvar *outside-space-x* 1)
(setq  *outside-space-x* 1)
(defvar *outside-space-y* 1)
(setq  *outside-space-y* 1)




;;;; this does the file manipulation,
;;;; prints the box to the gotten file,
;;;; and then uses Unix(TM) cat to send it to the printer-host
(defun make-ps-file (box fname &optional (need-to-move t))
  (boxer-editor-error "printing the screen")
  (let ((filename (if fname fname (get-temp-ps-file-name))))
    (if (probe-file fname)
	(delete-file fname))
#|
    #+lucid  (lcl::run-unix-program
	      "/bin/cat"
	      :input
	      *ps-header-file*
	      :output filename
	      :wait t)
    #+excl(excl::run-shell-command
	   "/bin/cat"
	   :input
	   *ps-header-file*
	   :output filename
	   :wait t)
    #-(or lucid excl) (error "shell commands not supported in ~A"
			     (lisp-implementation-type))
|#
    (with-open-file (ps-stream filename
				 :direction :output
				 :if-exists :append
				 :if-does-not-exist :create)
      ;; first, prepend the header
      (with-open-file (header-stream *ps-header-file* :direction :input)
	#+lucid
	(let ((in (lcl::underlying-stream header-stream))
	      (out (lcl::underlying-stream ps-stream))
	      (eof-value (list 'eof)))
	  (do ((char (lcl::fast-read-char in nil eof-value)
		     (lcl::fast-read-char in nil eof-value)))
	      ((eq char eof-value))
	    (lcl::fast-write-char char out)))
	#-lucid
	(let ((eof-value (list 'eof)))
	  (do ((char (read-char header-stream nil eof-value)
		     (read-char header-stream nil eof-value)))
	      ((eq char eof-value))
	    (write-char char ps-stream)))
	)
      ;; then send out the specifics of the box
      (ps-print-out-box box ps-stream need-to-move )
      (if (null fname)

	    (with-open-stream (csh
			       #+lucid  (lcl::run-unix-program
					 "csh"
					 :input :stream
					 :output :stream
					 :wait nil)
			       #+excl(excl::run-shell-command
				      "csh"
				      :input :stream
				      :output :stream
				      :wait nil)
			       #-(or lucid excl)
			       (error
				"shell commands not supported in ~A"
				(lisp-implementation-type)))
	      (format csh
		      (format
		       nil
		       "cat ~A | rsh ~A lpr -h -P~A ~% /bin/rm -f ~A~%"
		       filename
		       *ps-postscript-printer-host*
		       *ps-postscript-printer*
		       filename
		       ))
	      )
	    ))))





;;; indentation

;;;; indent level determines how many tabs
(defvar *indent-level* 0)

;;;; how many spaces per tab
(defvar *space-per-indent* 4)
(defun ps-indent (s)
  (if *ps-debug*
      (dotimes (dummy *indent-level*)
	(dotimes (d *space-per-indent*)
	  (format s " ")))))

;;;; these two manipulate the indent level.
(defun ps-inc-indent () (setq *indent-level*  (1+ *indent-level* )))
(defun ps-dec-indent ()
  (if (> *indent-level* 0)
      (setq *indent-level*  (1- *indent-level* ))
      (break "indent error")))



;;;; given that the current point's coordinates are on the PS stack,
;;;; generate code to move x pixels over and y units down
(defun implicit-move (x y s)
  (ps-infor s "copytop exch ~A add exch ~A add moveto~%"
	    (pi-to-po x) (pi-to-po y)))

;;;; should the page be landscaped?
;;;; compare the steepness of the rectangles of bwXbh and pwXph
(defun should-landscape  (bw bh pw ph)
  (<  (/ bh bw) (/ ph pw)))

;;;; If the box needs to be landscaped, landscape the coord system.
;;;; If things are landscaped, we must call get-scale-factor with
;;;; the new page wid and height.
;;;; Then scale things so that the outermost box fills the page.
;;;; The page has already been set up with margins around the edge.

(defun ps-scale (boxwid boxhei page-wid page-hei s)
  (if *ps-debug* (ps-infor s "% enter ps-scale~%"))
  (let ((scale-factor 0))
      (if  (should-landscape boxwid boxhei page-wid page-hei)
	   (progn (format s "landscape ~%")
		  ;;;; note shift of page-hei and page-wid compared to below
		  ;;;; when page is landscaped, those parms are switched
		  (setq scale-factor (get-scale-factor boxwid
						       boxhei
						       page-hei
						       page-wid))
		  (format s "~A dup scale~%"(float scale-factor)))
	   (progn
	     (setq scale-factor (get-scale-factor boxwid
						  boxhei
						  page-wid
						  page-hei))
	     (format s "~A dup scale~%" (float scale-factor))
	     ))
      (if *ps-debug* (ps-infor s "% exit ps-scale~%"))
      scale-factor))


;;;; brute force method. I don't see the way to figure out the maximum scaling
;;;; in a clean way.
;;;; get-scale factor takes the pixel values for bh and bw and has
;;;; to make sure everything will fit. Hence it does the funny stuff with
;;;; the point-per-pixel.
(defun get-scale-factor (bw bh pw ph)
  (let ((factor-a (*
		   1
		   (/ 1 *point-per-pixel*)
		   (/ ph bh)))
	(factor-b (*
		   1
		   (/ 1 *point-per-pixel*)
		   (/ pw bw))))
    (cond
      ((check-scale-factor (max factor-a factor-b) bw bh pw ph)
       (progn
	 (if *ps-debug* (print "by max"))
	 (max factor-a factor-b)))
      ((check-scale-factor (min factor-a factor-b) bw bh pw ph)
       (progn
	 (if *ps-debug* (print "by min"))
	 (min factor-a factor-b)))
      (t (progn
	   (format t "problems with scaling: box might not fit on page~%")
	   factor-a)))))

(defun check-scale-factor (factor bw bh pw ph)
  (and  (>= (+ 20 ph) (pi-to-po(* factor bh)))
	(>= (+ 20 pw) (pi-to-po(* factor bw)))))


(defun test-scale-code ()
  (let ((pw 612) (ph 792))
    (dotimes (w 100)
      (dotimes (h 100)
	(let ((bw (* 10 (1+ w)))
	      (bh (* 10 (1+ h))))
	  (if (should-landscape bw bh pw ph)
	      (let ((scale-factor (get-scale-factor bw bh ph pw)))
		(if (not (check-scale-factor  scale-factor bw bh ph pw))
		    (format t "land: ~A ~A~%" bw bh)))
	      (let ((scale-factor (get-scale-factor bw bh pw ph)))
		(if (not (check-scale-factor  scale-factor bw bh pw ph))
		    (format t ": ~A ~A~%" bw bh)))))))))





;;;; the Apple Laserwriter has a blank margin around each page.
;;;; cutting down the hypothetically available space on a page.
;;;; This variable is an attempt to incorporate that limitation.
(defvar *ps-margin* 40)

;;;; this function is used to print out the outermost box.
;;;; It dumps out some PS code to initialize some box margin constants
;;;; (called xsp & ysp), does some translations, initializes the fonts
;;;; and linewidth.

;;;; It puts on the stack the reference point for the box. See ps-dump-box
;;;; It then calls the function to dump out the position independent code
;;;; for the box.
;;;; It dumps out the showpage
(defun ps-print-out-box (screen-box s &optional (need-to-move t))
  (setq *indent-level* 0)
  (format s "%%BoundingBox: 0 0 ~A ~A~%" (* 72 8.5)(* 72 11.0))
  (format s "/xsp ~A  def ~%"                 ;;;; defs for some margins
	  (pi-to-po *outside-space-x*))
  (format s "/ysp  ~A  def ~%"
	  (pi-to-po *outside-space-y*))
  (format s "reverse-coord~%")    ;;;; set up +vertical going down page

    ;;;; given that the real page has margins about 20 pixels wide on
    ;;;; every side, we want to make the virtual page smaller and do the
    ;;;; scaling in a clean way.
  (let* ((page-wid (* 72 8.5))   ;;;; 72 points per inch X 8.5 inches
	 (page-hei (* 72 11.0))    ;;;; 72 points per inch X 11 inches
	 (new-page-wid (- page-wid (* 2 *ps-margin*)))
	 (new-page-hei (* page-hei (/ new-page-wid page-wid))))
    (if *ps-debug*
	(progn (format t "new-page-wid: ~A~%new-page-hei:~A~%"
		       new-page-wid
		       new-page-hei)))
    (format s "~A ~A translate~%"
	    *ps-margin*
	    (/ (- page-hei new-page-hei) 2))
    (format s "~A ~A scale~%"       ;;; scale the page to the new size
	    (/ new-page-wid page-wid)
	    (/ new-page-hei page-hei))

    (let ((scale-factor (ps-scale (slot-value screen-box 'wid)
				  (slot-value screen-box 'hei)
				  page-wid
				  page-hei
				  s)))
      ))

  (format s "0 text_font_size translate ~%")
      ;;; make sure there is room for
      ;;; the outermost box.
      ;;; remember, boxer is from u.l.
      ;;; while PS is l.l. corner
  (format s    "0 0 ~%")   ;;;; the ref for the outside box
  (format s    "newpath 0 0 moveto~%") ;;;; initialize path
  (format s    ".5 setlinewidth~%" )   ;;;; ps linewidth = 1/2 x linewidth
  ;;; set all the font sizes based on what the screen is set to
  ;;;; the font_size is determined from one parameter of the font
  (let ((font_size
	 (bw::font-descent (aref (bw::sheet-font-map *boxer-pane*) 0)))
	(tfont_size nil)
	(lfont_size nil))
    (cond ((eql font_size 3)
	   (setq tfont_size 12)
	   (setq lfont_size 8))
	  ((eql font_size 4)
	   (setq tfont_size 18)
	   (setq lfont_size 10))
	  ((eql font_size 5)
	   (setq tfont_size 24)
	   (setq lfont_size 10)))
    ;;; got the sizes, set the sizes
    (format
     s
     "/text_font_size ~A def /name_font_size ~A def /label_font_size ~A def~%"
     tfont_size
     tfont_size
     lfont_size
     ))
  (format s    "text_font ~%")         ;;;; switch to text font
  (format s "%%%% outermost box starts here~%~%~%~%")
  (setq *printing-a-string* nil)
  (ps-dump-box
   (slot-value screen-box 'actual-obj)
   screen-box need-to-move s )

  (format s "showpage ~%")
  (force-output s)
  (setq *printing-a-string* nil)
  (setq *indent-level* 0)
  )





;;;; %stack: ref-x ref-y

;;;; need-to-move means thing is not positioned already
;;;; It is expected that the reference point for the box is on the PS stack
;;;; usually this means that the coordinate of the next highest object is
;;;; on the stack. This is this way so that the ps-dump-box can dump out
;;;; position independent code. MAKE SURE THAT THE REF. COORD IS ON THE
;;;; STACK.

(defmethod ps-dump-box ((act-box box) screen-box need-to-move s
			&optional (need-to-correct t))
  (if (and (graphics-sheet act-box)
	   (display-style-graphics-mode?
		 (display-style-list  act-box)))
      (ps-dump-graphics-box act-box screen-box need-to-move s need-to-correct)
      (ps-dump-others-box act-box screen-box need-to-move s need-to-correct)))

;;;; Takes care of moving to the right spot if necessary, and then moving
;;;; up from the baseline. PUTS ON THE STACK THE REF FOR THE ROWS OR
;;;; GRAPH-COMS.
;;;; stack: x y (reference point)

(defun ps-box-header (need-to-move box s)
  (if need-to-move
      (ps-infor s "copytop exch ~A add exch ~A add moveto~%"
		(pi-to-po(screen-obj-x-offset box))
		(pi-to-po(screen-obj-y-offset box))))
  ;; give the box some margins
  (ps-infor s "xsp ysp rmoveto~%")
  ;; put the point on ps stack for the sub-rows move above
  ;; the baseline. The text draws from the bottom of the line,
  ;; boxes from the top of the line.
  (ps-infor s "currentpoint~%")
  (ps-infor s "0 text_font_size neg rmoveto ~%"))

;;;; the UL is still on the stack, so get the relative offsets for the
;;;; next cha and move there. This is equiv to pushing the carriage of
;;;; a  typewriter one space forward.
;;;; The "pop pop" takes off the coordinate (which was the ref for the
;;;; sub-boxes). Which was put on by ps-box-header.
(defun ps-box-prolog (need-to-correct wid s)
  (if need-to-correct
       (progn
	 (ps-infor s  "exch ~A add exch moveto " (pi-to-po wid))
	 (ps-infor s "xsp ysp neg rmoveto~%"))
       (ps-infor s "pop pop~%")))


;;;; This is used to dump non-graphics, non-port screen-boxes
;;;; First it checks if the wid and hei are "real"
;;;; It dumps out position independent code for the box.
;;;; It recursively dumps out the sub-objects.
;;;; It then cleans up with the prolog
(defun ps-dump-others-box (act-box screen-box need-to-move s need-to-correct)
  (if *ps-debug* (ps-infor s "% enter ps-dump-others-box~%"))

  (let ((wid (screen-obj-wid screen-box)) (hei (screen-obj-hei screen-box)))
    (if (and (> wid 0) (> hei 0))
	(progn
	  (ps-box-header nil screen-box s)         ;;;; deactivate positioning
	  (if (or *print-outermost-box*
		  (not (eql act-box (screen-obj-actual-obj
				     (outermost-screen-box)))))

	      (print-a-box
           ;;; hack the outermostbox
	       (if (eql *initial-box* act-box) "" (name-string act-box))
	       (ps-b-typ act-box)
	       wid
	       hei
	       (screen-box-shrunkp screen-box)
	       s))
	  (unless (screen-box-shrunkp screen-box)
	    (ps-inc-indent)
	    (ps-dump-rows screen-box s)
	    (ps-dec-indent))
	  (ps-box-prolog need-to-correct wid s)
	  (if *ps-debug*  (ps-infor s "% exit ps-dump-box~%"))))))

;;;; need-to-move means thing is not positioned already
;;;; This is used to dump out port-boxes.
;;;; First it checks if the wid and hei are "real"
;;;; It dumps out position independent code for the box.
;;;; It recursively dumps out the sub-objects.
;;;; It then cleans up with the prolog

(defmethod  ps-dump-box ((act-pbox port-box) screen-box need-to-move s
			 &optional (need-to-correct t))
  (if *ps-debug* (ps-infor s "% enter dump-port~%"))
  (let ((box (ports act-pbox))
	(wid      (slot-value screen-box 'wid))
	(hei      (slot-value screen-box 'hei))
	(nameP (name-string act-pbox )))
    (if (and (> wid 0) (> hei 0)) ;; filter out garbage
	(progn                    ;; print shell, contents, prolog
;	  (ps-box-header need-to-move screen-box s)
	  (ps-box-header nil screen-box s) ;deactivate positioning
	  (print-a-port
	   nameP (name-string box) (ps-b-typ box) wid hei
	   (screen-box-shrunkp screen-box) s)
	  (unless (screen-box-shrunkp screen-box)
	    (cond ((and (graphics-sheet act-pbox)
			(display-style-graphics-mode?
			 (display-style-list  act-pbox)))
		   ;(ps-indent s)
		   (d-g-vectors screen-box s))
		  (t (ps-inc-indent)
		     (ps-dump-rows screen-box s)
		     (ps-dec-indent))))
	  (ps-box-prolog need-to-correct wid s))))
  (if *ps-debug*
      (ps-infor s "% exit dump-port~%")))

;;;; need-to-move means thing is not positioned already.
;;;; First it checks if the wid and hei are "real"
;;;; It dumps out position independent code for the box.
;;;; It recursively dumps out the sub-objects.
;;;; It then cleans up with the prolog

(defun ps-dump-graphics-box (act-box screen-box need-to-move s
				     &optional (need-to-correct t))
  (let ((wid (screen-obj-wid screen-box))
	(hei (screen-obj-hei screen-box)))
    (if (and (> wid 0) (> hei 0))         ;; filter out garbage screen objs
	(progn
;	  (ps-box-header need-to-move screen-box s)
	  (ps-box-header nil screen-box s) ;deactivate positioning
	  (trace-a-graph-box (name-string act-box ) wid hei
	   (screen-box-shrunkp screen-box) screen-box s)
	  (ps-box-prolog need-to-correct wid s)))))


;;;; this is used to dump a cha (a box or char).
;;;; It makes sure that it keeps the right state (if a string
;;;; is in the middle of being printed, it terminates the string).
;;;; need-to-move means thing is not positioned already.

(defun ps-dump-cha (cha s &optional (need-to-correct t))
  (cond ((characterp cha)
	 (ps-dump-char cha s ))
	((screen-box? cha)
	 (progn
	   (terminate-string s)
	   (ps-dump-box (slot-value cha 'actual-obj) cha t s
			(and need-to-correct *ps-allow-optimizations*))))))

;;;; row iteration.
;;;; make the movement to the point for the row, dump out the ref for
;;;; the chas in the row, and then iterate through the chas, dumping
;;;; them out.
;;;; *****HACK****** I unrolled  the do macro -row-chas so that I could
;;;; do some
;;;; optimization on the final cha. It ammounts to not dumping out code
;;;; to advance the point horizonatlly when you have dumped out the last
;;;; cha (one is going to return, so it doesn't matter where you are).
;;;;
;;;; The terminate string makes sure that you close of the string if the
;;;; last cha happens to be a char.

(defun ps-dump-row (row s )
  (if *ps-debug* (ps-infor s "% enter ps-dump-row~%"))
  (implicit-move
   ;; hack to make things look right
   ( - (screen-obj-x-offset row) 4.5 )
   ;; hack to make things look right
   (- (screen-obj-y-offset row) 4.5)
   s)
  (ps-infor s "currentpoint ~%") ;ref for row-chas
  ;; I had to expand the macro to allow for optimizations
  (let ((last (STORAGE-VECTOR-ACTIVE-LENGTH (SLOT-VALUE row 'SCREEN-CHAS)))
	(contents   (%SV-CONTENTS (SLOT-VALUE row 'SCREEN-CHAS))))
    (unless (>=& 0 last)
      (do* ((index 0 (1+ index))
	    (cha (svref& contents index)
		 (svref& contents index)))
	   ((=& index last))
	(if (eql index (1- last))
	    (ps-dump-cha cha s nil)
	    (ps-dump-cha cha s )))))
    (terminate-string s)
    (ps-infor s "pop pop    ~32T%pop off row-cha-ref ~%~%")
    (if *ps-debug* (ps-infor s "% exit ps-dump-row~%")))

;;;; dump out the rows for the box. It doesn't have to do positioning
;;;; because the individual rows will handle their own positioning.
(defun ps-dump-rows (sbox s)
  (if *ps-debug* (ps-infor s "% enter ps-dump-rows~%"))
  (let ((count (aref (slot-value sbox 'screen-rows) 1)))
    (dotimes (index count)
      (ps-dump-row (aref (aref (slot-value sbox 'screen-rows) 0) index)
		   s)))
  (if *ps-debug* (ps-infor s "% exit ps-dump-rows~%" )))

;;;; This is used to get a temp file-name for a screen-dump.
;;;; They are of the form: "/tmp/ps****"
(defun get-temp-ps-file-name ()
  (do ((name
      (concatenate 'string "/tmp/ps"(symbol-name(gensym)))
      (concatenate 'string "/tmp/ps"(symbol-name(gensym)))))
    ((not(probe-file name)) name)))

;;;; Dump out the drawing commands for the sprites on the stream
;;;; Takes a graphics box and a stream
;;;; the code for the graphics box has just been dumped out, so these are
;;;; tacked on after the drawing commands. This all takes place within
;;;; a clip, so the sprites cannot draw on anything else
;;;; This function just does some setup and then dumps out the top
;;;; level-sprites, and their subsprites.
(defun do-the-sprite-thing (screen-box stream)
  (if *ps-debug* (format stream " %enter do the sprite thing~%"))
  (ps-infor stream "gsave~%")    ; do a gsave before set-up-sprite-offsets
  (set-up-sprite-offsets screen-box stream)
  (dolist (top-level-tur (graphics-sheet-object-list
			  (graphics-sheet
			   (box-or-port-target
			    (screen-obj-actual-obj screen-box)))))
    (let ((shown-state (shown? top-level-tur)))
      (if (or shown-state
	      (eql shown-state :subsprites))
	  (progn
	    (ps-infor  stream "gsave~%")
	    (dump-turtle top-level-tur stream)
	    (ps-infor  stream "grestore~%")))
      (if (or shown-state
	      (not (eql shown-state :no-subsprites))
	      (eql :subsprites shown-state))
	  (dump-sub-turtles  top-level-tur stream)
	  )))
  (ps-infor stream "grestore~%")
  (if *ps-debug* (format stream " %exit do the sprite thing~%")))

;;;; recursively output the ps code for the sub-sprites on the stream.
;;;; it has to set up the coord matrix for the subsprites based on the
;;;; position of the tur
(defun dump-sub-turtles (tur stream)
  (if *ps-debug* (format stream " %enter dump-sub-turtles~%"))
  (ps-infor stream "gsave~%")
  (dolist (sub-tur (slot-value tur 'subsprites))
    (let ((shown-state (shown? sub-tur)))
      (if (or shown-state
	      (eql shown-state :subsprites))
	  (progn
	    (ps-infor  stream "~%gsave~%")
	    (dump-turtle sub-tur stream)
	    (ps-infor  stream "grestore~%")))
      (if (or shown-state
	      (not (eql :no-subsprites shown-state))
	      (eql :subsprites shown-state))
	  (dump-sub-turtles  sub-tur stream))
      ))
  (ps-infor stream "grestore~%")
  (if *ps-debug* (format stream " %exit dump-sub-turtles~%"))
  )

;;;; center the coord system about the given sprite.
;;;;  ****HACK****
;;;; Note that we don't have to worry about the angle of the
;;;; sprite as it has already rotated the angle to draw itself!
(defun center-coord-about-sprite (turtle stream)
  (if *ps-debug* (format stream " %enter center-coord-about-sprite~%"))
  (ps-infor stream "~A ~A neg translate 0 0 moveto~%"
	      (absolute-x-position turtle)
	      (absolute-y-position turtle)
	      )
  (if *ps-debug* (format stream " %exit center-coord-about-sprite~%"))
  )

;;;; output the code to change the translation matrix so that the
;;;; coordinate origin is the center of the box. This is where sprite's
;;;; have their origin. Move to the origin
(defun set-up-sprite-offsets (screen-box stream)
  (if *ps-debug* (format stream " %enter set-up-sprite-offsets~%"))
  (let ((graphics-sheet (graphics-sheet (box-or-port-target
					 (screen-obj-actual-obj screen-box)))))
    (ps-infor stream "~A ~A translate 0 0 moveto~%"
	      (float (/ (graphics-sheet-draw-wid graphics-sheet) 2))
	      (float (/ (graphics-sheet-draw-hei graphics-sheet) 2))))
  (if *ps-debug* (format stream " %exit set-up-sprite-offsets~%")))

;;;; takes a turtle and a stream. dumps out the ps code to draw the turtle.
;;;; First it does the translation for the sprite draw commands relative
;;;; to the sprite coord system.
;;;; ****hack****
;;;; d-vecs has to take the sprite's heading as the sprite may have some
;;;; text. PostScript can print text at an agle, whereas X cannot. Hence,
;;;; just dumping stuff out will result in text being at the wrong angle.
;;;; The angle is passed along so that the text can be drawn at a corrected
;;;; angle.
;;;; This function assumes that the sprite's coord system  has been set up
;;;; with 0 0 as either the center of the graphics box or the super-sprite
(defun dump-turtle (turtle stream)
  (if *ps-debug* (format stream " %enter dump-turtle~%"))
  (let ((s (shape turtle)))
    (ps-infor stream "~A ~A neg translate ~A rotate~%"
	      (absolute-x-position turtle)
	      (absolute-y-position turtle)
	      (absolute-heading turtle))
    ;; new as of Mon Mar 11 20:22:37 PST 1991 Josh
    (let ((*turtle-size*  (aref (slot-value turtle 'sprite-size) 1)))
      (ps-infor stream "currentlinewidth ~A div setlinewidth ~A dup scale ~%"
		*turtle-size*
		*turtle-size*)
      (d-vecs s stream (absolute-heading turtle))))
  (if *ps-debug* (format stream " %exit dump-turtle~%")))

;;;; This command was created before d-vecs
;;;; Take the graphics box and dump out its drawing commands
;;;; after that dump out the sprites of the g-box
;;;; It dumps out a "stroke" every 200 commands
;;;; so that there is not lossage due to too big a drawing path.
(defun d-g-vectors (screen-box stream)
  (let ((box (box-or-port-target (screen-obj-actual-obj screen-box))))
    (dotimes (count (aref (graphics-graphics-list box) 1))
      (dvec (aref (aref (graphics-graphics-list box) 0) count) stream)
      (if (eql 0 (mod count 200)) (format stream " stroke ")))
    (do-the-sprite-thing screen-box stream)))

;;;; d-vecs dumps out the list of sprite drawing commands.
;;;; **** HACK ****
;;;; note that the sprite-angle has to get passed on
;;;; this is due to the fact that PostScript can print text at angles
;;;; and X cannot. see the definition of dvec for a description.

;;;; d-vecs dumps out a "stroke" every so often so that there is not
;;;; lossage due to too big a drawing path.
(defun d-vecs (array stream &optional (sprite-angle 0))
  (if *ps-debug* (format stream " %enter d-vecs~%"))
  (dotimes (index (aref array 1))
    (dvec (aref (aref array 0) index) stream sprite-angle)
    (if (eql 0 (mod index 200)) (format stream " stroke ")))
  (format stream "stroke~%")
  (if *ps-debug* (format stream " %exit d-vecs~%")))



;;;; Take the drawing commmand and dump out the ps code on the stream
;;;; the first conditions are for the screen structures, the ones that
;;;; can only have fixnum arguments.
;;;; The ones after 31 are the floating point opcodes. Those are for
;;;; sprites.

;;;; NOTE THAT FOR The sprite OPCODES ALL OF THE Y COORDINATES ARE NEGATED!!
;;;; This is because in sprite-space, a positive y goes up the screen,
;;;; whereas in X-space positive y goes down the screen.
;;;; the negation is done in the lisp

;;;; ****Hack****
;;;; X-windows does not allow one to draw text at an angle; PostScript does.
;;;; The problem this creates is that the sprite is rotated to its angle
;;;; before the drawing commands get dumped out. Hence text gets drawn at
;;;; and angle (under X, the text is drawn horizontaly, no matter the angle).
;;;; As to solve this problem, the optional parameter sprite-angle gets
;;;; passed into dvec

(defun dvec (vec s &optional (sprite-angle 0))
  (let ((opcode (aref vec 0)))
    (cond
      ;;;; CHANGE-ALU
      ((eql opcode 0)
       ;(format *error-output* "dvec: change-alu not supported~%")
       )

      ;;;; CHANGE-PEN-WIDTH
      ((eql opcode 1)
       (format s "stroke ~a 2 div setlinewidth~%"
	       (aref vec 1)))

      ;;;; CHANGE-GRAPHICS-FONT
      ((eql opcode 2)
       (let ((font-no (aref vec 1)))
	 (format s "~A g-set-font~%" font-no)))

      ;;;; LINE-SEGMENT
      ((eql opcode 3)
       (let ((x0 (aref vec 1))
	     (y0 (aref vec 2))
	     (x1 (aref vec 3))
	     (y1 (aref vec 4)))
	     ;;; (ps-indent s)
	 (format s "~a ~a moveto ~a ~a lineto~%"
		 x0 y0
		 x1 y1)))

      ;;;; CENTERED-STRING
      ((eql opcode 7)
       (let ((x (aref vec 1))
	     (y  (aref vec 2))
	     (string (aref vec 3))
	     )
	 (ps-infor s "~A ~A moveto "x y)
	 (name-format s string)
	 (format s "cent_string~%")
	 ))
      ;;;; LEFT-STRING
      ((eql opcode 8)
       (format *error-output* "dvec: left-string not supported~%"))
      ;;;; RIGHT-STRING
      ((eql opcode 9)
       (format *error-output* "dvec: right-string not supported~%"))

      ;;;; CENTERED-RECTANGLE
      ((eql opcode 10)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (width (aref vec 3))
	     (height (aref vec 4)))
	 (format s "~A ~A moveto ~A ~A fill_cent_rect ~%"
		 x y width height)))


      ;;;; DOT
      ((eql opcode 11)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (rad 1))
	 (format s "~A ~A moveto ~A  f_grcircle ~%"
		 x y rad)
	 ))
      ;;;; CENTERED-BITMAP
      ((eql opcode 15)
       (format *error-output* "dvec: centered-bitmap not supported~%"))

      ;;;; FILLED-ELLIPSE
      ((eql opcode 28)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (width (aref vec 3))
	     (height (aref vec 4)))
	 (format s "true ~A ~A moveto ~A ~A ellipse ~%"
		 x y width height)
	 ))

      ;;;; ELLIPSE
      ((eql opcode 29)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (width (aref vec 3))
	     (height (aref vec 4)))
	 (format s "false ~A ~A moveto ~A ~A ellipse ~%"
		 x y width height )
	 ))

      ;;;; FILLED-CIRCLE
      ((eql opcode 30)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (rad (aref vec 3)))
	 (format s "~A ~A moveto ~A f_grcircle ~%"
		 x y  rad)
	 ))

      ;;;; CIRCLE
      ((eql opcode 31)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (rad (aref vec 3)))
	 (format s "~A ~A moveto ~A grcircle ~%"
		 x y rad)
	 ))
      ;;;; BOXER-CHANGE-ALU
      ((eql opcode 32)
       ;(format *error-output* "dvec: boxer-change-alu not supported~%"))
       )
      ;;;; BOXER-CHANGE-PEN-WIDTH
      ((eql opcode 33)
       (format s "stroke ~a 2 div setlinewidth~%"
	       (float (/ (aref vec 1) *turtle-size*))))

      ;;;; BOXER-CHANGE-GRAPHICS-FONT
      ((eql opcode 34)
       (let ((font-no (aref vec 1)))
	 (format s "~A g-set-font~%" font-no)))

      ;;;; BOXER-LINE-SEGMENT
      ((eql opcode 35)
       (let ((x0 (aref vec 1))
	     (y0 (aref vec 2))
	     (x1 (aref vec 3))
	     (y1 (aref vec 4)))
	 (format s "~a ~a moveto ~a ~a lineto~%"
		 x0 (- y0)
		 x1 (- y1))))

      ;;;; BOXER-CHANGE-GRAPHICS-COLOR
      ((eql opcode 36)
       (format *error-output*
	       "dvec: BOXER-CHANGE-GRAPHICS-COLOR not supported~%"))
      ;;;; BOXER-CENTERED-STRING
      ((eql opcode 39)
       (let ((x (aref vec 1))
	     (y  (aref vec 2))
	     (string (aref vec 3))
	     )
	 (ps-infor s "~A ~A  moveto ~A  rotate " x (- y) (- sprite-angle))
	 (name-format s string)

	 (format s "cent_string ~A rotate~%" sprite-angle)
	 ))
      ;;;; BOXER-LEFT-STRING
      ((eql opcode 40)
       (format *error-output* "dvec: left-string not supported~%"))
      ;;;; BOXER-RIGHT-STRING
      ((eql opcode 41)
       (format *error-output* "dvec: right-string not supported~%"))
      ;;;; BOXER-CENTERED-RECTANGLE
      ((eql opcode 42)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (width (aref vec 3))
	     (height (aref vec 4)))
	 (format s "~A ~A  moveto ~A ~A fill_cent_rect ~%"
		 x (- y) width height)))

      ;;;; BOXER-DOT
      ((eql opcode 43)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (rad 1))
	 (format s "~A ~A  moveto ~A  f_grcircle ~%"
		 x (- y) rad)))
      ;;;; BOXER-CENTERED-BITMAP
      ((eql opcode 47)
       (format *error-output* "dvec: centered-bitmap not supported~%"))

      ;;;; BOXER-FILLED-ELLIPSE
      ((eql opcode 60)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (width (aref vec 3))
	     (height (aref vec 4)))
	 (format s "true ~A ~A moveto ~A ~A ellipse ~%"
		 x (- y) width height)))
      ;;;; BOXER-ELLIPSE
      ((eql opcode 61)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (width (aref vec 3))
	     (height (aref vec 4)))
	 (format s "false ~A ~A  moveto ~A ~A ellipse ~%"
		 x (- y) width height )))
      ;;;; BOXER-FILLED-CIRCLE
      ((eql opcode 62)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (rad (aref vec 3)))
	 (format s "~A ~A  moveto ~A f_grcircle ~%"
		 x (- y)  rad)))
      ;;;; BOXER-CIRCLE
      ((eql opcode 63)
       (let ((x (aref vec 1))
	     (y (aref vec 2))
	     (rad (aref vec 3)))
	 (format s "~A ~A moveto ~A grcircle ~%"
		 x (- y) rad))))))

;;; low-level PostScript generation

;;;; make a circle at the current point. For debugging the codegen.
(defun make-cir (s)
  (format s "~%circle~%"))


;;;; given the values, output postscript code to print the box shell
;;  PS stack:  fill? x y name type
;;;; x and y are scaled for point-space
;;;; types:
;;;;        name: string or nil
;;;;        type: string
;;;;        x, y: int
;;;;        fill: boolean

(defun print-a-box ( name type x y fill s)
  (ps-infor s "~A ~A ~A ~A "
	    (if fill "true" "false") (pi-to-po x) (pi-to-po y) type)
  (name-format s name)
  (format s "  dump_shaved_box~%"))

;;;; draw a g-box shell and its contents
;;;; x and y are scaled for point space
;;;; the postscript code for the box is ****HACKED*** because
;;;; one has to have the box clip stuff.
(defun trace-a-graph-box ( name x y fill screen-box s)
  (ps-infor s "~A ~A  ~A "
	    (if fill "true" "false")
	    (pi-to-po x)
	    (pi-to-po y))
  (name-format s name)
  (format s "trace_shaved_graph_box ~%")
  ;; dump out draw commands
  (unless (screen-box-shrunkp screen-box)
    (ps-indent s)(d-g-vectors screen-box s))
  ;; clean up
  (ps-infor s "~A dashed_rcb_tail graph_box_tail ~%"
	    (if	(eq (display-style-border-style
		     (display-style-list (screen-obj-actual-obj screen-box)))
		    :dashed)
		"true" ; "true" means draw dashed border (for transparent box)
		"false"))   ;"false" means draw normal border
  )

;;;; x and y are  scaled for point-space
;;;; types:
;;;;        nameP nameB: string or nil
;;;;        type: string
;;;;        x, y;: int
;;;;        fill: boolean

(defun print-a-port (nameP nameB type x y fill s)
  (ps-infor s "~A ~A ~A ~A "
	    type (if fill "true" "false") (pi-to-po x) (pi-to-po y))
  (name-format s nameB)
  (name-format s nameP)
  (format s " shaved_port~%"))


;;;; icky name escape stuff.......

;;;; This handles the characters that must be escaped with "\"
(defun ps-esc (s cha)
  (cond ((eql cha #\)) (format s "\\)"))
	((eql cha #\() (format s "\\("))
	((eql cha #\\) (format s "\\\\"))
	((or (eql cha #\tab) (eql cha #\newline) nil))
	(t (format s "~A" cha))))

;;;; "dump out the name on the stream, filtering ) to \) , ( to \("
;;;; this one encloses the name with "(" and ")"
(defun name-format (s name)
  (format s " (")
  (if  (equal name "Un-Named")
       nil
       (if (> (length name) 0)
	   (do*  ((len (length name) len)
		  (index 0 (1+ index))
		  (cha  (aref name index) (aref name index))
		  (dummy (ps-esc s cha)(ps-esc s cha)))
		 ((or (eql index (1- len))
		      (eql (aref name (1+ index)) #\newline))))))
  (format s ") ")
  )

;;;; ACCESSORS & TRANSLATORS
;;;; convert pixels to points
(defun pi-to-po (x)
  (float (* x *point-per-pixel*)))




;;;; screen-box-accessors

;;;; is the given screen box shrunk?
(defun screen-box-shrunkp (box)
  (if (eql (outermost-screen-box) box)
      nil
    (eql :shrunk
	 (display-style-style(display-style-list
			      (slot-value box 'actual-obj))))))


;;;; This caps the string with a ")" if necessary
(defun terminate-string (s)
    (if *printing-a-string*
      (progn
	(setq  *printing-a-string*  nil)
	(format s ") show ~%")
	)))
