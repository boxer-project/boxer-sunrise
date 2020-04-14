;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    New (and hopefully final) key bindings



Modification History (most recent at top)

 2/05/07 changed #+ mcl's and lwwins to apple and win32 - in this file they refer
         to physical devices, also "apple" seems to be the only feature that digitool
         and lispworks share for referring to the mac.
 6/06/05 added escape-key binding for com-abort
 1/09/04 added com-hardcopy-lw-window for lispworks ctrl-P
10/20/04 removed escape key binding (com-christmas-tree)
 4/21/03 merged current LW and MCL files
 9/02/02 changed ctrl-~ binding from com-unmodify-document to
         com-toggle-modified-flag
 5/22/01 added backspace equivalents for {cmd, opt}-delete and search mode delete
 2/15/01 merged current LW and MCL files
12/20/00 added com-fill-box on c-m-f-key
11/04/00 added com-follow-mouse to mouse-click-on-sprite for LWWIN
         com-mouse-br-pop-up to mouse-click-on-bottom-right
 7/06/00 added LWWIN mouse-double-click binding, other double click tuning
 3/26/00 more PC binding fine tuning
12/05/99 added PC mouse bindings, reorganized mice bindings into functional groups
11/28/99 added lispworks PC bindings
01/06/99 added mac bindings for HOME, END, PAGE-UP, PAGE-DOWN and DELX keys
01/06/99 started logging changes: source = boxer version 2.3beta+2


|#

(in-package :boxer)


;;;;; New Key Bindings


;;; standard keys


;;; Defines all the "normal" (no ctrl- or meta- or super- or...) keys
;;; to be self inserting
#-excl
(let ((vanilla-key-codes-not-to-define
       '#.(mapcar #'char-code '(#\| #\[ #\] #\{ #\} #\return #\tab #\delete))))
  (dotimes (key-code #o177)
    (let ((char-to-insert (code-char key-code)))
      (unless (member key-code vanilla-key-codes-not-to-define)
	(let ((key-name (lookup-key-name key-code 0)))
	  (when (null key-name)
	    (error "Key name for key ~D was not found" key-code))
	  (boxer-eval::defboxer-key-internal
	   key-name
	   #'(lambda ()
               ;(reset-region)
               ;; mac behavior instead...
               (let ((r (or *region-being-defined* (get-current-region))))
                 (unless (null r) (editor-kill-region r)))
	       (with-multiple-execution
		   #-opengl(add-redisplay-clue (point-row) ':insert)
		 (insert-cha *point* char-to-insert :moving))
               (mark-file-box-dirty (point-row))
               boxer-eval::*novalue*))
	  (boxer-command-define
	   key-name
	   (format nil
		   "Inserts the ~C character at the cursor."
		   char-to-insert)))))))

;;; there is a bug in the excl 3.0 compiler which causes multiple LET's to
;;; not get their own bindings so we have to use lambda
#+excl
(let ((vanilla-key-codes-not-to-define
       '#.(mapcar #'char-code '(#\| #\[ #\]))))
  (dotimes (key-code #o177)
    (funcall
     #'(lambda (char-to-insert)
	 (unless (member key-code vanilla-key-codes-not-to-define)
	   (let ((key-name (lookup-key-name key-code 0)))
	     (when (null key-name)
	       (error "Key name for key ~d was not found" key-code))
	     (boxer-eval::defboxer-key-internal
	      key-name
	      #'(lambda ()
		  ;(reset-region)
                  ;; mac behavior instead...
                  (let ((r (or *region-being-defined* (get-current-region))))
                    (unless (null r) (editor-kill-region r)))
		  (with-multiple-execution
		      #-opengl(add-redisplay-clue (point-row) ':insert)
		    (insert-cha *point* char-to-insert :moving))
                  (mark-file-box-dirty (point-row))
                  boxer-eval::*novalue*))
	     (boxer-command-define
	      key-name
	      (format nil
		      "Insert the ~C character at the cursor."
		      char-to-insert)))))
     (code-char key-code))))

;; the return of parens
(defself-inserting-key BOXER-USER::|(-KEY| #\()
(defself-inserting-key BOXER-USER::|)-KEY| #\))


(boxer-eval::defboxer-key (bu::g-key 1) com-abort)
;; added escape 6/5/05
(boxer-eval::defboxer-key bu::escape-key com-abort)

(boxer-eval::defboxer-key bu::space-key com-space)

(boxer-eval::defboxer-key bu::return-key com-return)

;; emacsy (optional)
(boxer-eval::defboxer-key (bu::a-key 1) com-beginning-of-row)
(boxer-eval::defboxer-key (bu::e-key 1) com-end-of-row)

;; these are shadowed farther down by the font change keys
(boxer-eval::defboxer-key (bu::<-key 2) com-beginning-of-box)
(boxer-eval::defboxer-key (bu::>-key 2) com-end-of-box)

(boxer-eval::defboxer-key (bu::<-key 3) com-goto-top-level)

(boxer-eval::defboxer-key (bu::k-key 1) com-kill-to-end-of-row)


;;;; [] Making Boxes

(boxer-eval::defboxer-key bu::[-key com-make-and-enter-box)
#+sun (boxer-eval::defboxer-key bu::L3-key com-make-and-enter-box)

(boxer-eval::defboxer-key bu::{-key com-make-and-enter-data-box)
#+sun (boxer-eval::defboxer-key bu::L5-key com-make-and-enter-data-box)

(boxer-eval::defboxer-key (bu::t-key 2) com-make-turtle-box)
#+sun (boxer-eval::defboxer-key bu::L7-key com-make-turtle-box)
#+sun (boxer-eval::defboxer-key (bu::L7-key 3) com-make-turtle-box)

(boxer-eval::defboxer-key (bu::s-key 2) com-make-sprite-box)
#+sun (boxer-eval::defboxer-key (bu::L7-key 1) com-make-sprite-box)

(boxer-eval::defboxer-key (bu::g-key 2) com-make-graphics-box)
#+sun (boxer-eval::defboxer-key (bu::L7-key 2) com-make-graphics-box)

#+sun (boxer-eval::defboxer-key (bu::L5-key 1) com-make-and-enter-toolbox)

(boxer-eval::defboxer-key (bu::p-key 2) com-place-port)
#+sun (boxer-eval::defboxer-key (bu::L3-key 1) com-place-port)

(boxer-eval::defboxer-key (bu::p-key 3) com-make-port)
#+sun (boxer-eval::defboxer-key (bu::L3-key 2) com-make-port)


;;;; [] Cutting and Pasting

(boxer-eval::defboxer-key (bu::x-key 1) com-cut-region)
#+sun (boxer-eval::defboxer-key bu::l10-key com-cut-region)

(boxer-eval::defboxer-key (bu::c-key 1) com-copy-region)
#+sun (boxer-eval::defboxer-key bu::l6-key com-copy-region)

(boxer-eval::defboxer-key (bu::v-key 1) com-yank)
#+sun (boxer-eval::defboxer-key bu::l8-key com-yank)

(boxer-eval::defboxer-key (bu::v-key 2) com-retrieve)
(boxer-eval::defboxer-key (bu::y-key 1) com-retrieve)
#+sun (boxer-eval::defboxer-key bu::l2-key com-retrieve)

;;;; [] Other Important

#+sun (boxer-eval::defboxer-key bu::L1-key com-abort) ; STOP key
(boxer-eval::defboxer-key (bu::.-key 1) com-abort)

;; Find
(boxer-eval::defboxer-key (bu::f-key 1) com-search-forward)
#+sun(boxer-eval::defboxer-key bu::l9-key com-search-forward)

(boxer-eval::defboxer-key (bu::f-key 2) com-search-backward)
#+sun(boxer-eval::defboxer-key (bu::l9-key 2) com-search-backward)

(eval-when (eval load)
  (let ((vanilla-key-codes-not-to-define '#.(mapcar #'char-code
						    '(#\| #\[ #\]))))
    (dotimes (key-code #o177)
      (let ((char-to-insert (code-char key-code)))
	(unless (member key-code vanilla-key-codes-not-to-define)
	  (let ((key-name (lookup-key-name key-code 0)))
	    (when (null key-name)
	      (error "Key name for key ~D was not found" key-code))
	    (defboxer-mode-key-internal key-name (search-mode)
	      #'(lambda () (com-search-char char-to-insert)
			boxer-eval::*novalue*)))))))
  )

(defsearch-mode-key bu::[-key com-search-doit-box)
#+sun
(defsearch-mode-key bu::L3-key com-search-doit-box)

(defsearch-mode-key bu::{-key com-search-data-box)
#+sun
(defsearch-mode-key bu::L5-key com-search-data-box)

#+sun
(defsearch-mode-key bu::L7-key com-search-sprite-box)

#+sun
(defsearch-mode-key (bu::L3-key 1) com-search-port)
(defsearch-mode-key (bu::space-key 2) com-search-port)

#+sun
(defsearch-mode-key bu::F3-key com-search-named-box)
(defsearch-mode-key bu::\|-key com-search-named-box)
(defsearch-mode-key bu::]-key com-search-exit-named-box)
(defsearch-mode-key bu::}-key com-search-exit-named-box)

(defsearch-mode-key (bu::f-key 1) com-search-forward-again)
#+sun(defsearch-mode-key bu::l9-key com-search-forward-again)

(defsearch-mode-key (bu::f-key 2) com-search-backward-again)
#+sun(defsearch-mode-key (bu::l9-key 2) com-search-backward-again)

#+sun
(defsearch-mode-key bu::L1-key      com-end-search)
(defsearch-mode-key bu::escape-key  com-end-search)
(defsearch-mode-key (bu::\'-key 1) com-quote-search-char)
(defsearch-mode-key (bu::\"-key 1) com-quote-search-char)
(defsearch-mode-key (bu::g-key 1)  com-abort-search)
(defsearch-mode-key bu::delete-key  com-delete-search-char)
(defsearch-mode-key bu::backspace-key  com-delete-search-char)
(defsearch-mode-key (bu::^-key 1)  com-expand-search)

(defsearch-mode-key (bu::.-key 1) com-abort-search)
(defsearch-mode-key bu::help-key com-search-help)
(defsearch-mode-key (bu::r-key 1) com-force-redisplay-all)


;; Doit
(boxer-eval::defboxer-key bu::line-key com-doit-now)
;(boxer-eval::defboxer-key bu::shift-return-key com-doit-now)
#+(or dec mcl (and sun solaris)) (boxer-eval::defboxer-key bu::enter-key com-doit-now)

;; Step
(boxer-eval::defboxer-key (bu::line-key 1) com-step)
(boxer-eval::defboxer-key (bu::line-key 2) com-step) ; not approved
#+apple (boxer-eval::defboxer-key (bu::return-key 1) com-doit-now) ; should be com-step
#+lispworks (boxer-eval::defboxer-key (bu::return-key 1) com-doit-now) ; PC only has enter-key

;; Name-Tab
(boxer-eval::defboxer-key bu::\|-key com-name-box)

;; Unbox
(boxer-eval::defboxer-key (bu::@-key 1) com-unboxify)

;; Refresh Display
(boxer-eval::defboxer-key (bu::r-key 1) com-force-redisplay)

;; Help

(boxer-eval::defboxer-key bu::help-key com-help)
(boxer-eval::defboxer-key (bu::help-key 1) com-prompt)
(boxer-eval::defboxer-key (bu::help-key 2) com-document-key)
;; on the PC the Insert-key is in the same place as the mac help key
#+lispworks
(progn
  (boxer-eval::defboxer-key bu::insert-key com-help)
  (boxer-eval::defboxer-key (bu::insert-key 1) com-prompt)
  (boxer-eval::defboxer-key (bu::insert-key 2) com-document-key)
  )


(boxer-eval::defboxer-key (bu::h-key 1) com-help)

;; Toggle Transparency
(boxer-eval::defboxer-key (bu::t-key 1) com-toggle-box-transparency)

;; Print Screen
(boxer-eval::defboxer-key (bu::p-key 1)
              #+lwwin com-hardcopy-lw-window
              #+mcl   com-hardcopy-mac-window
              #-(or lwwin mcl) com-print-screen)

#+sun (boxer-eval::defboxer-key bu::R2-key com-print-screen)

#+sun (boxer-eval::defboxer-key (bu::R2-key 2) com-print-screen-to-file)

;; Zoom
(boxer-eval::defboxer-key (bu::z-key 1) com-move-to-port-target)

; removed 10/20/04
;(boxer-eval::defboxer-key bu::escape-key com-christmas-tree)

(boxer-eval::defboxer-key (bu::\'-key 1) com-quote-self-insert)
(boxer-eval::defboxer-key (bu::\"-key 1) com-quote-self-insert)

;;; Characters
(boxer-eval::defboxer-key bu::left-arrow-key com-backward-cha)
(boxer-eval::defboxer-key bu::Right-Arrow-key com-forward-cha)

;; Words
(boxer-eval::defboxer-key (bu::Left-Arrow-key 1) com-backward-word)
(boxer-eval::defboxer-key (bu::Right-Arrow-key 1) com-forward-word)

;; Lines
(boxer-eval::defboxer-key bu::up-arrow-key com-previous-row)
(boxer-eval::defboxer-key bu::down-arrow-key com-next-row)
(boxer-eval::defboxer-key (bu::Left-Arrow-key 2) com-beginning-of-row)
(boxer-eval::defboxer-key (bu::Right-Arrow-key 2) com-end-of-row)

;; Box Scroll
(boxer-eval::defboxer-key (bu::up-arrow-key 1) com-scroll-up-one-screen-box)
(boxer-eval::defboxer-key (bu::down-arrow-key 1) com-scroll-dn-one-screen-box)
#+apple (boxer-eval::defboxer-key bu::page-up-key com-scroll-up-one-screen-box)
#+apple (boxer-eval::defboxer-key bu::page-down-key com-scroll-dn-one-screen-box)
#+lispworks (boxer-eval::defboxer-key bu::page-up-key com-scroll-up-one-screen-box)
#+lispworks (boxer-eval::defboxer-key bu::page-down-key com-scroll-dn-one-screen-box)
#+sun (boxer-eval::defboxer-key bu::R9-key com-scroll-up-one-screen-box)   ; PgUp
#+sun (boxer-eval::defboxer-key bu::R15-key com-scroll-dn-one-screen-box)  ; PgDn
;; for now....
#+sun (boxer-eval::defboxer-key (bu::R9-key 1) com-scroll-up-row)
#+sun (boxer-eval::defboxer-key (bu::R15-key 1) com-scroll-dn-row)

;; Global Box
(boxer-eval::defboxer-key (bu::up-arrow-key 2) com-beginning-of-box)
(boxer-eval::defboxer-key (bu::down-arrow-key 2) com-end-of-box)
#+apple (boxer-eval::defboxer-key bu::home-key com-beginning-of-box)
#+apple (boxer-eval::defboxer-key bu::end-key  com-end-of-box)
#+lispworks (boxer-eval::defboxer-key bu::home-key com-beginning-of-box)
#+lispworks (boxer-eval::defboxer-key bu::end-key  com-end-of-box)
#+sun (boxer-eval::defboxer-key bu::R7-key com-beginning-of-box)  ; Home
#+sun (boxer-eval::defboxer-key bu::R13-key com-end-of-box)       ; End

;; Among Box Navigation
(boxer-eval::defboxer-key (bu::up-arrow-key 3) com-shrink-box)
(boxer-eval::defboxer-key (bu::right-arrow-key 3) com-enter-next-box)
(boxer-eval::defboxer-key (bu::left-arrow-key 3) com-enter-previous-box)

(boxer-eval::defboxer-key bu::tab-key com-move-to-next-box)
;(boxer-eval::defboxer-key bu::shift-tab-key com-move-to-previous-box) ; can't generate shift-TAB
(boxer-eval::defboxer-key (bu::tab-key 1) com-enter-next-box)
(boxer-eval::defboxer-key (bu::tab-key 2) com-enter-previous-box)

(boxer-eval::defboxer-key bu::]-key com-exit-box)
(boxer-eval::defboxer-key bu::}-key com-shrink-box)


;;;; [] Deleting

(boxer-eval::defboxer-key bu::delete-key com-rubout)
;;; by popular demand...
(boxer-eval::defboxer-key bu::backspace-key com-rubout)
(boxer-eval::defboxer-key (bu::backspace-key 1) com-rubout)

(boxer-eval::defboxer-key (bu::d-key 1) com-delete)
#+apple (boxer-eval::defboxer-key bu::delx-key com-delete)
#+sun (boxer-eval::defboxer-key bu::ins-key com-delete)
#+lispworks (boxer-eval::defboxer-key bu::delete-key com-delete)

(boxer-eval::defboxer-key (bu::delete-key 2) com-rubout-word)
(boxer-eval::defboxer-key (bu::delete-key 1) com-delete-word)

(boxer-eval::defboxer-key (bu::backspace-key 2) com-rubout-word)
(boxer-eval::defboxer-key (bu::backspace-key 1) com-delete-word)
#+apple (boxer-eval::defboxer-key (bu::delx-key 1) com-delete-word)
#+apple (boxer-eval::defboxer-key (bu::delx-key 2) com-delete-word)
#+lispworks (boxer-eval::defboxer-key (bu::delete-key 1) com-delete-word)
#+lispworks (boxer-eval::defboxer-key (bu::delete-key 2) com-delete-word)

(boxer-eval::defboxer-key (bu::delete-key 3) com-delete-line)
(boxer-eval::defboxer-key (bu::backspace-key 3) com-delete-line)

;;;; [] Format and Caps

;(boxer-eval::defboxer-key (bu::i-key 1) com-italics)
;(boxer-eval::defboxer-key (bu::b-key 1) com-boldface)

;;; and case
(boxer-eval::defboxer-key (bu::c-key 2) com-capitalize-word)
(boxer-eval::defboxer-key (bu::u-key 2) com-uppercase-word)
(boxer-eval::defboxer-key (bu::l-key 2) com-lowercase-word)


(boxer-eval::defboxer-key (bu::>-key 2) com-fat)

(boxer-eval::defboxer-key (bu::<-key 2) com-nutri-system)


;;;; saving/going to locations

(boxer-eval::defboxer-key (bu::space-key 1) com-set-mark)
(boxer-eval::defboxer-key (bu::space-key 2) com-goto-previous-place)

(boxer-eval::defboxer-key (bu::x-key 2) com-exchange-point-and-mark)

(boxer-eval::defboxer-key (bu::/-key 1) com-point-to-register)
(boxer-eval::defboxer-key (bu::/-key 2) com-register-to-point)
(boxer-eval::defboxer-key (bu::j-key 1) com-register-to-point)

;;;; [] Miscellaneous

(boxer-eval::defboxer-key (bu::return-key 2) com-open-line)

(boxer-eval::defboxer-key (bu::f-key 3) com-fill-box)

;;; Defines all the "control" (ctrl-, meta-, or ctrl-meta- ) number
;;; keys to act as a numeric argument

(dotimes (control-bits 3)  ; note the 1+ further down
  (do ((key-code #o60 (1+ key-code)))
      ((= key-code #o72))
    (let ((key-name (lookup-key-name key-code (1+ control-bits))))
      (when (null key-name)
	(error "Key name for key ~d was not found" key-code))
      (boxer-eval::defboxer-key-internal
	  key-name
	  (intern (symbol-format nil "COM-INCREMENT-NUMERIC-ARG-BY-~D"
			  (- key-code #o60)))))))

(boxer-eval::defboxer-key (bu::?-key 1) com-prompt)
(boxer-eval::defboxer-key (bu::?-key 2) com-document-key)
(boxer-eval::defboxer-key (bu::?-key 3) com-document-key)

(boxer-eval::defboxer-key (bu::v-key 3) com-toggle-vanilla-mode)


;;; files

(boxer-eval::defboxer-key (bu::s-key 1) com-save-document)
#+sun
(boxer-eval::defboxer-key (bu::s-key 3) com-box-save-as) ; on key because not avail on menu

(boxer-eval::defboxer-key (bu::o-key 1) com-open-box-file)

(boxer-eval::defboxer-key (bu::n-key 2) com-new-file-box)

(boxer-eval::defboxer-key (bu::~-key 1) com-toggle-modified-flag) ;com-unmodify-document



;;;; MICE

;; plain clicks on middle of boxes

;; generic 3 (equal) button mice like symbolics machines and suns...
#+(or sun symbolics dec3100)
(progn
  (boxer-eval::defboxer-key bu::mouse-middle com-mouse-define-region)
  (boxer-eval::defboxer-key bu::mouse-right  com-mouse-expand-box)
  (boxer-eval::defboxer-key bu::mouse-left  com-mouse-collapse-box)

  (boxer-eval::defboxer-key bu::mouse-left-twice com-mouse-shrink-box)
  (boxer-eval::defboxer-key bu::mouse-middle-twice com-mouse-doit-now)
  (boxer-eval::defboxer-key bu::mouse-right-twice com-mouse-set-outermost-box)

  (boxer-eval::defboxer-key bu::ctrl-mouse-left com-mouse-shrink-box)
  (boxer-eval::defboxer-key bu::ctrl-mouse-right com-mouse-set-outermost-box)

  (boxer-eval::defboxer-key bu::graphics-mouse-right com-mouse-expand-box)
  (boxer-eval::defboxer-key bu::graphics-mouse-left  com-mouse-collapse-box)

  (boxer-eval::defboxer-key bu::graphics-mouse-right-twice com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::graphics-mouse-left-twice com-mouse-shrink-box)
  )

;; mac one button mouse... (option and command shifts are used
;; in place of left and right)
#+apple
(progn
  (boxer-eval::defboxer-key bu::mouse-click com-mouse-define-region)
  (boxer-eval::defboxer-key bu::command-mouse-click  com-mouse-expand-box)
  (boxer-eval::defboxer-key bu::option-mouse-click  com-mouse-collapse-box)

  (boxer-eval::defboxer-key bu::option-mouse-double-click com-mouse-shrink-box)
  (boxer-eval::defboxer-key bu::mouse-double-click com-mouse-doit-now)
  (boxer-eval::defboxer-key bu::command-mouse-double-click com-mouse-set-outermost-box)

  (boxer-eval::defboxer-key bu::option-mouse-click-on-graphics com-mouse-collapse-box)
  (boxer-eval::defboxer-key bu::command-mouse-click-on-graphics com-mouse-expand-box)

  (boxer-eval::defboxer-key bu::option-mouse-double-click-on-graphics
	        com-mouse-shrink-box)
  (boxer-eval::defboxer-key bu::command-mouse-double-click-on-graphics
	        com-mouse-set-outermost-box)
  )

;; PC 3 button mouse.  Mouse buttons on the PC are asymmetric, the left
;; button is the common frequently used one, the right button is for
;; power and menu items.  The middle button is mostly unused
;; the basic model is to emulate the 1 button mac mouse using the left button,
;; reserving the right button for the double clicked funs
#+win32
(progn
;  #-opengl
  (boxer-eval::defboxer-key bu::mouse-click       com-mouse-define-region)
;  #+opengl ;; temporary
;  (boxer-eval::defboxer-key bu::mouse-click       com-mouse-move-point)
  (boxer-eval::defboxer-key bu::alt-mouse-click   com-mouse-expand-box)
#-OPENGL
  (boxer-eval::defboxer-key bu::ctrl-mouse-click  com-mouse-collapse-box)
  #+opengl
  (boxer-eval::defboxer-key bu::ctrl-mouse-click  com-mouse-define-region)

  (boxer-eval::defboxer-key bu::ctrl-mouse-right-click com-mouse-shrink-box)
  (boxer-eval::defboxer-key bu::mouse-double-click com-mouse-doit-now)
  ;; make this a popup to be more windows like
  (boxer-eval::defboxer-key bu::mouse-right-click      com-mouse-doit-now)
  (boxer-eval::defboxer-key bu::alt-mouse-right-click  com-mouse-set-outermost-box)

  (boxer-eval::defboxer-key bu::ctrl-mouse-click-on-graphics  com-mouse-collapse-box)
  (boxer-eval::defboxer-key bu::alt-mouse-click-on-graphics   com-mouse-expand-box)

  (boxer-eval::defboxer-key bu::ctrl-mouse-right-click-on-graphics com-mouse-shrink-box)
  (boxer-eval::defboxer-key bu::alt-mouse-right-click-on-graphics  com-mouse-set-outermost-box)
  )



;; sprite clicks
#+(or sun symbolics dec3100)
(boxer-eval::defboxer-key bu::sprite-mouse-middle com-sprite-follow-mouse)
#+apple
(boxer-eval::defboxer-key bu::mouse-click-on-sprite com-sprite-follow-mouse)
#+win32
(boxer-eval::defboxer-key bu::mouse-click-on-sprite com-sprite-follow-mouse)



;;; border mouse coms

;; the resize tab...

;; 3 button (these are sort of old)
#+(or sun symbolics dec3100)
(progn
  (boxer-eval::defboxer-key bu::bottom-right-mouse-middle com-mouse-resize-box)
  (boxer-eval::defboxer-key bu::bottom-right-mouse-left com-mouse-br-corner-collapse-box)
  (boxer-eval::defboxer-key bu::bottom-right-mouse-right com-mouse-br-corner-expand-box)
  (boxer-eval::defboxer-key bu::bottom-right-mouse-left-twice com-mouse-super-shrink-box)
  (boxer-eval::defboxer-key bu::bottom-right-mouse-right-twice com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::bottom-right-mouse-middle-twice com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::ctrl-bottom-right-mouse-middle com-mouse-resize-box)
  (boxer-eval::defboxer-key bu::ctrl-bottom-right-mouse-left
                com-mouse-br-corner-super-shrink-box)
  (boxer-eval::defboxer-key bu::ctrl-bottom-right-mouse-right
                com-mouse-br-corner-set-outermost-box)
  (boxer-eval::defboxer-key bu::meta-bottom-right-mouse-middle com-mouse-resize-box)
  (boxer-eval::defboxer-key bu::meta-bottom-right-mouse-left
                com-mouse-br-corner-super-shrink-box)
  (boxer-eval::defboxer-key bu::meta-bottom-right-mouse-right
                com-mouse-br-corner-set-outermost-box)
  (boxer-eval::defboxer-key bu::top-left-mouse-middle com-mouse-tl-corner-collapse-box)
  (boxer-eval::defboxer-key bu::top-left-mouse-middle-twice com-mouse-super-shrink-box)
  (boxer-eval::defboxer-key bu::top-left-mouse-left com-mouse-tl-corner-collapse-box)
  (boxer-eval::defboxer-key bu::top-left-mouse-left-twice com-mouse-super-shrink-box)
  (boxer-eval::defboxer-key bu::top-left-mouse-right com-mouse-tl-corner-collapse-box)
  (boxer-eval::defboxer-key bu::top-left-mouse-right-twice com-mouse-super-shrink-box)
  )

;; Border mouse coms for the Mac
#+apple
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-bottom-right com-mouse-br-pop-up)
  (boxer-eval::defboxer-key bu::mouse-click-on-top-left com-mouse-tl-corner-collapse-box)
  (boxer-eval::defboxer-key bu::mouse-double-click-on-top-left com-mouse-super-shrink-box)

  (boxer-eval::defboxer-key bu::mouse-double-click-on-bottom-right com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::command-mouse-click-on-bottom-right com-mouse-br-corner-expand-box)
  (boxer-eval::defboxer-key bu::option-mouse-click-on-bottom-right com-mouse-br-corner-collapse-box)
  (boxer-eval::defboxer-key bu::mouse-double-click-on-top-right com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::command-mouse-click-on-top-left com-mouse-tl-pop-up)
  (boxer-eval::defboxer-key bu::option-mouse-click-on-top-left com-mouse-tl-corner-toggle-closet)
  )


#+win32
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-bottom-right com-mouse-br-pop-up)
  (boxer-eval::defboxer-key bu::mouse-right-click-on-bottom-right com-mouse-br-pop-up)
  (boxer-eval::defboxer-key bu::mouse-click-on-top-left com-mouse-tl-corner-collapse-box)
  (boxer-eval::defboxer-key bu::mouse-right-click-on-top-left com-mouse-tl-pop-up)
  (boxer-eval::defboxer-key bu::mouse-double-click-on-top-left com-mouse-super-shrink-box)
  (boxer-eval::defboxer-key bu::mouse-click-on-top-right com-mouse-tr-corner-expand-box)
  (boxer-eval::defboxer-key bu::mouse-double-click-on-bottom-right com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::alt-mouse-click-on-bottom-right com-mouse-br-corner-expand-box)
  (boxer-eval::defboxer-key bu::ctrl-mouse-click-on-bottom-right com-mouse-br-corner-collapse-box)
  (boxer-eval::defboxer-key bu::mouse-double-click-on-top-right com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::ctrl-mouse-click-on-top-left com-mouse-tl-corner-toggle-closet)
  (boxer-eval::defboxer-key bu::alt-mouse-click-on-top-left com-mouse-tl-corner-toggle-closet)
  )



;; names

;; 3 button (these are sort of old)
#+(or sun symbolics dec3100)
(progn
  (boxer-eval::defboxer-key bu::name-handle-mouse-left      com-mouse-border-name-box)
  (boxer-eval::defboxer-key bu::name-handle-mouse-middle    com-mouse-border-name-box)
  (boxer-eval::defboxer-key bu::name-handle-mouse-right     com-mouse-border-name-box)

  (boxer-eval::defboxer-key bu::name-mouse-left      com-mouse-collapse-box)
  (boxer-eval::defboxer-key bu::name-mouse-middle    com-mouse-move-point)
  (boxer-eval::defboxer-key bu::name-mouse-right     com-mouse-expand-box)
  )

#+apple
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-name-handle  com-mouse-border-name-box)
  (boxer-eval::defboxer-key bu::mouse-click-on-name         com-mouse-move-point)
  )

#+win32
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-name-handle com-mouse-border-name-box)
  (boxer-eval::defboxer-key bu::mouse-click-on-name        com-mouse-move-point)
  )



;; toggle view

#+(or sun symbolics dec3100)
(progn
  (boxer-eval::defboxer-key bu::bottom-left-mouse-left com-mouse-bl-corner-toggle-box-view)
  (boxer-eval::defboxer-key bu::bottom-left-mouse-middle com-mouse-bl-corner-toggle-box-view)
  (boxer-eval::defboxer-key bu::bottom-left-mouse-right com-mouse-bl-corner-toggle-box-view)
  )

#+apple
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-bottom-left com-mouse-bl-corner-toggle-box-view)
  (boxer-eval::defboxer-key bu::command-mouse-click-on-bottom-left com-mouse-bl-pop-up)
  )

#+win32
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-bottom-left
                com-mouse-bl-corner-toggle-box-view)
  (boxer-eval::defboxer-key bu::mouse-right-click-on-bottom-left com-mouse-bl-pop-up)
  )

;; toggle closet

#+(or sun symbolics dec3100)
(progn
  (boxer-eval::defboxer-key bu::top-right-mouse-left com-mouse-tr-corner-toggle-closet)
  (boxer-eval::defboxer-key bu::top-right-mouse-right com-mouse-tr-corner-toggle-closet)
  (boxer-eval::defboxer-key bu::top-right-mouse-middle com-mouse-tr-corner-toggle-closet)
  )

#+apple
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-top-right com-mouse-tr-corner-expand-box)
  (boxer-eval::defboxer-key bu::command-mouse-click-on-top-right com-mouse-tr-pop-up)
  (boxer-eval::defboxer-key bu::option-mouse-click-on-top-right com-mouse-tr-corner-toggle-closet)
  )

#+win32
(progn
  (boxer-eval::defboxer-key bu::mouse-right-click-on-top-right  com-mouse-tr-pop-up)
  (boxer-eval::defboxer-key bu::mouse-click-on-top-right        com-mouse-expand-box)
  (boxer-eval::defboxer-key bu::mouse-double-click-on-top-right com-mouse-set-outermost-box)
  (boxer-eval::defboxer-key bu::ctrl-mouse-click-on-top-right   com-mouse-tr-corner-toggle-closet)
  (boxer-eval::defboxer-key bu::alt-mouse-click-on-top-right    com-mouse-tr-corner-toggle-closet)
  )



;; toggle type

#+(or sun symbolics dec3100)
(progn
  (boxer-eval::defboxer-key bu::type-mouse-left       com-mouse-type-tag-pop-up)
  (boxer-eval::defboxer-key bu::type-mouse-middle     com-mouse-type-tag-pop-up)
  (boxer-eval::defboxer-key bu::type-mouse-right      com-mouse-type-tag-pop-up)
  )

#+apple
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-type   com-mouse-border-toggle-type)
  (boxer-eval::defboxer-key bu::command-mouse-click-on-type   com-mouse-type-tag-pop-up)
  )

#+win32
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-type  com-mouse-border-toggle-type)
  (boxer-eval::defboxer-key bu::mouse-right-click-on-type com-mouse-type-tag-pop-up)
  )

;; scrolling

#+(or sun symbolics dec3100)
(progn
  (boxer-eval::defboxer-key bu::scroll-bar-mouse-middle       com-mouse-scroll-box)

  (boxer-eval::defboxer-key bu::scroll-bar-mouse-left         com-mouse-page-scroll-box)
  (boxer-eval::defboxer-key bu::scroll-bar-mouse-right        com-mouse-page-scroll-box)

  (boxer-eval::defboxer-key bu::scroll-bar-mouse-left-twice   com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::scroll-bar-mouse-middle-twice com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::scroll-bar-mouse-right-twice  com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::ctrl-scroll-bar-mouse-left    com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::ctrl-scroll-bar-mouse-middle  com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::ctrl-scroll-bar-mouse-right   com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::meta-scroll-bar-mouse-left    com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::meta-scroll-bar-mouse-middle  com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::meta-scroll-bar-mouse-right   com-mouse-limit-scroll-box)
  )

#+apple
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-scroll-bar                com-mouse-scroll-box)
  (boxer-eval::defboxer-key bu::command-mouse-click-on-scroll-bar        com-mouse-page-scroll-box)
  (boxer-eval::defboxer-key bu::option-mouse-click-on-scroll-bar         com-mouse-page-scroll-box)
  (boxer-eval::defboxer-key bu::mouse-double-click-on-scroll-bar         com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::command-mouse-double-click-on-scroll-bar com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::option-mouse-double-click-on-scroll-bar  com-mouse-limit-scroll-box)
  )

#+win32
(progn
  (boxer-eval::defboxer-key bu::mouse-click-on-scroll-bar            com-mouse-scroll-box)
  (boxer-eval::defboxer-key bu::mouse-right-click-on-scroll-bar      com-mouse-page-scroll)
  (boxer-eval::defboxer-key bu::ctrl-mouse-click-on-scroll-bar       com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::alt-scroll-bar-mouse-click-on-       com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::ctrl-mouse-right-click-on-scroll-bar com-mouse-limit-scroll-box)
  (boxer-eval::defboxer-key bu::alt-mouse-right-click-on-scroll-bar  com-mouse-limit-scroll-box)
  )

;;;; temporary

(boxer-eval::defboxer-key bu::F9-key com-toggle-closets)
(boxer-eval::defboxer-key bu::R1-key com-prompt)
#+sun(boxer-eval::defboxer-key (bu::R5-key 1) com-break)
#+apple(boxer-eval::defboxer-key (bu::escape-key 1) com-break)
#+lispworks (boxer-eval::defboxer-key (bu::escape-key 1) com-break)
#+(and lispworks win32) (boxer-eval::defboxer-key bu::pause-key com-break)
#+(and lispworks macosx)(boxer-eval::defboxer-key (bu::F13-key 3) com-break)
#+(and sun solaris) (boxer-eval::defboxer-key (bu::enter-key 1) com-break)
;; adds pause breaks when in dribble recording mode
;; recording mode checks for this particular keypress (can't look
;; for names at the recording level)
#+apple
(boxer-eval::defboxer-key (bu::f15-key 3) com-noop)
#+apple
(boxer-eval::defboxer-key (bu::f14-key 3) com-noop)



;;;
(eval-when (load)
  (boxer-eval::boxer-toplevel-set 'bu::new-box-properties (make-new-box-properties-box))
  )
