;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$


 Copyright 1991 - 1998 Regents of the University of California

  Additional Portions Copyright 1998 - 2005 PyxiSystems LLC


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

#-(or lispworks mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+(or lispworks mcl)       (in-package :boxer)




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
	  (eval::defboxer-key-internal 
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
               eval::*novalue*))
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
	     (eval::defboxer-key-internal 
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
                  eval::*novalue*))
	     (boxer-command-define 
	      key-name 
	      (format nil
		      "Insert the ~C character at the cursor."
		      char-to-insert)))))
     (code-char key-code))))

;; the return of parens
(defself-inserting-key BOXER-USER::|(-KEY| #\()
(defself-inserting-key BOXER-USER::|)-KEY| #\))


(defboxer-key (bu::g-key 1) com-abort)
;; added escape 6/5/05
(defboxer-key bu::escape-key com-abort)

(defboxer-key bu::space-key com-space)

(defboxer-key bu::return-key com-return)

;; emacsy (optional)
(defboxer-key (bu::a-key 1) com-beginning-of-row)
(defboxer-key (bu::e-key 1) com-end-of-row)

;; these are shadowed farther down by the font change keys
(defboxer-key (bu::<-key 2) com-beginning-of-box)
(defboxer-key (bu::>-key 2) com-end-of-box)

(defboxer-key (bu::<-key 3) com-goto-top-level)

(defboxer-key (bu::k-key 1) com-kill-to-end-of-row)


;;;; [] Making Boxes

(defboxer-key bu::[-key com-make-and-enter-box)
#+sun (defboxer-key bu::L3-key com-make-and-enter-box)

(defboxer-key bu::{-key com-make-and-enter-data-box)
#+sun (defboxer-key bu::L5-key com-make-and-enter-data-box)

(defboxer-key (bu::t-key 2) com-make-turtle-box)
#+sun (defboxer-key bu::L7-key com-make-turtle-box)
#+sun (defboxer-key (bu::L7-key 3) com-make-turtle-box)

(defboxer-key (bu::s-key 2) com-make-sprite-box)
#+sun (defboxer-key (bu::L7-key 1) com-make-sprite-box)

(defboxer-key (bu::g-key 2) com-make-graphics-box)
#+sun (defboxer-key (bu::L7-key 2) com-make-graphics-box)

#+sun (defboxer-key (bu::L5-key 1) com-make-and-enter-toolbox)

(defboxer-key (bu::p-key 2) com-place-port)
#+sun (defboxer-key (bu::L3-key 1) com-place-port)

(defboxer-key (bu::p-key 3) com-make-port)
#+sun (defboxer-key (bu::L3-key 2) com-make-port)


;;;; [] Cutting and Pasting

(defboxer-key (bu::x-key 1) com-cut-region)
#+sun (defboxer-key bu::l10-key com-cut-region)

(defboxer-key (bu::c-key 1) com-copy-region)
#+sun (defboxer-key bu::l6-key com-copy-region)

(defboxer-key (bu::v-key 1) com-yank)
#+sun (defboxer-key bu::l8-key com-yank)

(defboxer-key (bu::v-key 2) com-retrieve)
(defboxer-key (bu::y-key 1) com-retrieve)
#+sun (defboxer-key bu::l2-key com-retrieve)

;;;; [] Other Important

#+sun (defboxer-key bu::L1-key com-abort) ; STOP key
(defboxer-key (bu::.-key 1) com-abort)

;; Find
(defboxer-key (bu::f-key 1) com-search-forward)
#+sun(defboxer-key bu::l9-key com-search-forward)

(defboxer-key (bu::f-key 2) com-search-backward)
#+sun(defboxer-key (bu::l9-key 2) com-search-backward)

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
			eval::*novalue*)))))))
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
(defboxer-key bu::line-key com-doit-now)
;(defboxer-key bu::shift-return-key com-doit-now)
#+(or dec mcl (and sun solaris)) (defboxer-key bu::enter-key com-doit-now)

;; Step
(defboxer-key (bu::line-key 1) com-step)
(defboxer-key (bu::line-key 2) com-step) ; not approved
#+apple (defboxer-key (bu::return-key 1) com-doit-now) ; should be com-step
#+lispworks (defboxer-key (bu::return-key 1) com-doit-now) ; PC only has enter-key

;; Name-Tab
(defboxer-key bu::\|-key com-name-box)

;; Unbox
(defboxer-key (bu::@-key 1) com-unboxify)

;; Refresh Display
(defboxer-key (bu::r-key 1) com-force-redisplay)

;; Help

(defboxer-key bu::help-key com-help)
(defboxer-key (bu::help-key 1) com-prompt)
(defboxer-key (bu::help-key 2) com-document-key)
;; on the PC the Insert-key is in the same place as the mac help key
#+lispworks
(progn
  (defboxer-key bu::insert-key com-help)
  (defboxer-key (bu::insert-key 1) com-prompt)
  (defboxer-key (bu::insert-key 2) com-document-key)
  )


(defboxer-key (bu::h-key 1) com-help)

;; Toggle Transparency
(defboxer-key (bu::t-key 1) com-toggle-box-transparency)

;; Print Screen
(defboxer-key (bu::p-key 1)
              #+lwwin com-hardcopy-lw-window
              #+mcl   com-hardcopy-mac-window
              #-(or lwwin mcl) com-print-screen)

#+sun (defboxer-key bu::R2-key com-print-screen)

#+sun (defboxer-key (bu::R2-key 2) com-print-screen-to-file)

;; Zoom
(defboxer-key (bu::z-key 1) com-move-to-port-target)

; removed 10/20/04
;(defboxer-key bu::escape-key com-christmas-tree)

(defboxer-key (bu::\'-key 1) com-quote-self-insert)
(defboxer-key (bu::\"-key 1) com-quote-self-insert)

;;; Characters
(defboxer-key bu::left-arrow-key com-backward-cha)
(defboxer-key bu::Right-Arrow-key com-forward-cha)

;; Words
(defboxer-key (bu::Left-Arrow-key 1) com-backward-word)
(defboxer-key (bu::Right-Arrow-key 1) com-forward-word)

;; Lines
(defboxer-key bu::up-arrow-key com-previous-row)
(defboxer-key bu::down-arrow-key com-next-row)
(defboxer-key (bu::Left-Arrow-key 2) com-beginning-of-row)
(defboxer-key (bu::Right-Arrow-key 2) com-end-of-row)

;; Box Scroll
(defboxer-key (bu::up-arrow-key 1) com-scroll-up-one-screen-box) 
(defboxer-key (bu::down-arrow-key 1) com-scroll-dn-one-screen-box)
#+apple (defboxer-key bu::page-up-key com-scroll-up-one-screen-box)
#+apple (defboxer-key bu::page-down-key com-scroll-dn-one-screen-box) 
#+lispworks (defboxer-key bu::page-up-key com-scroll-up-one-screen-box)
#+lispworks (defboxer-key bu::page-down-key com-scroll-dn-one-screen-box) 
#+sun (defboxer-key bu::R9-key com-scroll-up-one-screen-box)   ; PgUp
#+sun (defboxer-key bu::R15-key com-scroll-dn-one-screen-box)  ; PgDn
;; for now....
#+sun (defboxer-key (bu::R9-key 1) com-scroll-up-row)
#+sun (defboxer-key (bu::R15-key 1) com-scroll-dn-row)

;; Global Box
(defboxer-key (bu::up-arrow-key 2) com-beginning-of-box)
(defboxer-key (bu::down-arrow-key 2) com-end-of-box)
#+apple (defboxer-key bu::home-key com-beginning-of-box)
#+apple (defboxer-key bu::end-key  com-end-of-box)
#+lispworks (defboxer-key bu::home-key com-beginning-of-box)
#+lispworks (defboxer-key bu::end-key  com-end-of-box)
#+sun (defboxer-key bu::R7-key com-beginning-of-box)  ; Home
#+sun (defboxer-key bu::R13-key com-end-of-box)       ; End

;; Among Box Navigation
(defboxer-key (bu::up-arrow-key 3) com-shrink-box)
(defboxer-key (bu::right-arrow-key 3) com-enter-next-box)
(defboxer-key (bu::left-arrow-key 3) com-enter-previous-box)

(defboxer-key bu::tab-key com-move-to-next-box)
;(defboxer-key bu::shift-tab-key com-move-to-previous-box) ; can't generate shift-TAB
(defboxer-key (bu::tab-key 1) com-enter-next-box)
(defboxer-key (bu::tab-key 2) com-enter-previous-box)

(defboxer-key bu::]-key com-exit-box)
(defboxer-key bu::}-key com-shrink-box)


;;;; [] Deleting

(defboxer-key bu::delete-key com-rubout)
;;; by popular demand...
(defboxer-key bu::backspace-key com-rubout)
(defboxer-key (bu::backspace-key 1) com-rubout)

(defboxer-key (bu::d-key 1) com-delete)
#+apple (defboxer-key bu::delx-key com-delete)
#+sun (defboxer-key bu::ins-key com-delete)
#+lispworks (defboxer-key bu::delete-key com-delete)

(defboxer-key (bu::delete-key 2) com-rubout-word)
(defboxer-key (bu::delete-key 1) com-delete-word)

(defboxer-key (bu::backspace-key 2) com-rubout-word)
(defboxer-key (bu::backspace-key 1) com-delete-word)
#+apple (defboxer-key (bu::delx-key 1) com-delete-word)
#+apple (defboxer-key (bu::delx-key 2) com-delete-word)
#+lispworks (defboxer-key (bu::delete-key 1) com-delete-word)
#+lispworks (defboxer-key (bu::delete-key 2) com-delete-word)

(defboxer-key (bu::delete-key 3) com-delete-line)
(defboxer-key (bu::backspace-key 3) com-delete-line)

;;;; [] Format and Caps

;(defboxer-key (bu::i-key 1) com-italics)
;(defboxer-key (bu::b-key 1) com-boldface)

;;; and case
(defboxer-key (bu::c-key 2) com-capitalize-word)
(defboxer-key (bu::u-key 2) com-uppercase-word)
(defboxer-key (bu::l-key 2) com-lowercase-word)


(defboxer-key (bu::>-key 2) com-fat)

(defboxer-key (bu::<-key 2) com-nutri-system)


;;;; saving/going to locations

(defboxer-key (bu::space-key 1) com-set-mark)
(defboxer-key (bu::space-key 2) com-goto-previous-place)

(defboxer-key (bu::x-key 2) com-exchange-point-and-mark)

(defboxer-key (bu::/-key 1) com-point-to-register)
(defboxer-key (bu::/-key 2) com-register-to-point)
(defboxer-key (bu::j-key 1) com-register-to-point)

;;;; [] Miscellaneous

(defboxer-key (bu::return-key 2) com-open-line)

(defboxer-key (bu::f-key 3) com-fill-box)

;;; Defines all the "control" (ctrl-, meta-, or ctrl-meta- ) number
;;; keys to act as a numeric argument

(dotimes (control-bits 3)  ; note the 1+ further down
  (do ((key-code #o60 (1+ key-code)))
      ((= key-code #o72))
    (let ((key-name (lookup-key-name key-code (1+ control-bits))))
      (when (null key-name)
	(error "Key name for key ~d was not found" key-code))
      (eval::defboxer-key-internal 
	  key-name
	  (intern (symbol-format nil "COM-INCREMENT-NUMERIC-ARG-BY-~D"
			  (- key-code #o60)))))))

(defboxer-key (bu::?-key 1) com-prompt)
(defboxer-key (bu::?-key 2) com-document-key)
(defboxer-key (bu::?-key 3) com-document-key)

(defboxer-key (bu::v-key 3) com-toggle-vanilla-mode)


;;; files

(defboxer-key (bu::s-key 1) com-save-document)
#+sun
(defboxer-key (bu::s-key 3) com-box-save-as) ; on key because not avail on menu

(defboxer-key (bu::o-key 1) com-open-box-file)

(defboxer-key (bu::n-key 2) com-new-file-box)

(defboxer-key (bu::~-key 1) com-toggle-modified-flag) ;com-unmodify-document



;;;; MICE

;; plain clicks on middle of boxes

;; generic 3 (equal) button mice like symbolics machines and suns...
#+(or sun symbolics dec3100)
(progn
  (defboxer-key bu::mouse-middle com-mouse-define-region)
  (defboxer-key bu::mouse-right  com-mouse-expand-box)
  (defboxer-key bu::mouse-left  com-mouse-collapse-box)

  (defboxer-key bu::mouse-left-twice com-mouse-shrink-box)
  (defboxer-key bu::mouse-middle-twice com-mouse-doit-now)
  (defboxer-key bu::mouse-right-twice com-mouse-set-outermost-box)

  (defboxer-key bu::ctrl-mouse-left com-mouse-shrink-box)
  (defboxer-key bu::ctrl-mouse-right com-mouse-set-outermost-box)

  (defboxer-key bu::graphics-mouse-right com-mouse-expand-box)
  (defboxer-key bu::graphics-mouse-left  com-mouse-collapse-box)

  (defboxer-key bu::graphics-mouse-right-twice com-mouse-set-outermost-box)
  (defboxer-key bu::graphics-mouse-left-twice com-mouse-shrink-box)
  )

;; mac one button mouse... (option and command shifts are used
;; in place of left and right)
#+apple
(progn
  (defboxer-key bu::mouse-click com-mouse-define-region)
  (defboxer-key bu::command-mouse-click  com-mouse-expand-box)
  (defboxer-key bu::option-mouse-click  com-mouse-collapse-box)

  (defboxer-key bu::option-mouse-double-click com-mouse-shrink-box)
  (defboxer-key bu::mouse-double-click com-mouse-doit-now)
  (defboxer-key bu::command-mouse-double-click com-mouse-set-outermost-box)

  (defboxer-key bu::option-mouse-click-on-graphics com-mouse-collapse-box)
  (defboxer-key bu::command-mouse-click-on-graphics com-mouse-expand-box)

  (defboxer-key bu::option-mouse-double-click-on-graphics
	        com-mouse-shrink-box)
  (defboxer-key bu::command-mouse-double-click-on-graphics
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
  (defboxer-key bu::mouse-click       com-mouse-define-region)
;  #+opengl ;; temporary
;  (defboxer-key bu::mouse-click       com-mouse-move-point)
  (defboxer-key bu::alt-mouse-click   com-mouse-expand-box)
#-OPENGL
  (defboxer-key bu::ctrl-mouse-click  com-mouse-collapse-box)
  #+opengl
  (defboxer-key bu::ctrl-mouse-click  com-mouse-define-region)

  (defboxer-key bu::ctrl-mouse-right-click com-mouse-shrink-box)
  (defboxer-key bu::mouse-double-click com-mouse-doit-now)
  ;; make this a popup to be more windows like
  (defboxer-key bu::mouse-right-click      com-mouse-doit-now)
  (defboxer-key bu::alt-mouse-right-click  com-mouse-set-outermost-box)

  (defboxer-key bu::ctrl-mouse-click-on-graphics  com-mouse-collapse-box)
  (defboxer-key bu::alt-mouse-click-on-graphics   com-mouse-expand-box)

  (defboxer-key bu::ctrl-mouse-right-click-on-graphics com-mouse-shrink-box)
  (defboxer-key bu::alt-mouse-right-click-on-graphics  com-mouse-set-outermost-box)
  )



;; sprite clicks
#+(or sun symbolics dec3100)
(defboxer-key bu::sprite-mouse-middle com-sprite-follow-mouse)
#+apple
(defboxer-key bu::mouse-click-on-sprite com-sprite-follow-mouse)
#+win32
(defboxer-key bu::mouse-click-on-sprite com-sprite-follow-mouse)



;;; border mouse coms

;; the resize tab...

;; 3 button (these are sort of old)
#+(or sun symbolics dec3100)
(progn
  (defboxer-key bu::bottom-right-mouse-middle com-mouse-resize-box)
  (defboxer-key bu::bottom-right-mouse-left com-mouse-br-corner-collapse-box)
  (defboxer-key bu::bottom-right-mouse-right com-mouse-br-corner-expand-box)
  (defboxer-key bu::bottom-right-mouse-left-twice com-mouse-super-shrink-box)
  (defboxer-key bu::bottom-right-mouse-right-twice com-mouse-set-outermost-box)
  (defboxer-key bu::bottom-right-mouse-middle-twice com-mouse-set-outermost-box)
  (defboxer-key bu::ctrl-bottom-right-mouse-middle com-mouse-resize-box)
  (defboxer-key bu::ctrl-bottom-right-mouse-left
                com-mouse-br-corner-super-shrink-box)
  (defboxer-key bu::ctrl-bottom-right-mouse-right
                com-mouse-br-corner-set-outermost-box)
  (defboxer-key bu::meta-bottom-right-mouse-middle com-mouse-resize-box)
  (defboxer-key bu::meta-bottom-right-mouse-left 
                com-mouse-br-corner-super-shrink-box)
  (defboxer-key bu::meta-bottom-right-mouse-right
                com-mouse-br-corner-set-outermost-box)
  (defboxer-key bu::top-left-mouse-middle com-mouse-tl-corner-collapse-box)
  (defboxer-key bu::top-left-mouse-middle-twice com-mouse-super-shrink-box)
  (defboxer-key bu::top-left-mouse-left com-mouse-tl-corner-collapse-box)
  (defboxer-key bu::top-left-mouse-left-twice com-mouse-super-shrink-box)
  (defboxer-key bu::top-left-mouse-right com-mouse-tl-corner-collapse-box)
  (defboxer-key bu::top-left-mouse-right-twice com-mouse-super-shrink-box)
  )

;; Border mouse coms for the Mac
#+apple
(progn
  (defboxer-key bu::mouse-click-on-bottom-right com-mouse-br-pop-up)
  (defboxer-key bu::mouse-click-on-top-left com-mouse-tl-corner-collapse-box)
  (defboxer-key bu::mouse-double-click-on-top-left com-mouse-super-shrink-box)

  (defboxer-key bu::mouse-double-click-on-bottom-right com-mouse-set-outermost-box)
  (defboxer-key bu::command-mouse-click-on-bottom-right com-mouse-br-corner-expand-box)
  (defboxer-key bu::option-mouse-click-on-bottom-right com-mouse-br-corner-collapse-box)
  (defboxer-key bu::mouse-double-click-on-top-right com-mouse-set-outermost-box)
  (defboxer-key bu::command-mouse-click-on-top-left com-mouse-tl-pop-up)
  (defboxer-key bu::option-mouse-click-on-top-left com-mouse-tl-corner-toggle-closet)
  )


#+win32
(progn
  (defboxer-key bu::mouse-click-on-bottom-right com-mouse-br-pop-up)
  (defboxer-key bu::mouse-right-click-on-bottom-right com-mouse-br-pop-up)
  (defboxer-key bu::mouse-click-on-top-left com-mouse-tl-corner-collapse-box)
  (defboxer-key bu::mouse-right-click-on-top-left com-mouse-tl-pop-up)
  (defboxer-key bu::mouse-double-click-on-top-left com-mouse-super-shrink-box)
  (defboxer-key bu::mouse-click-on-top-right com-mouse-tr-corner-expand-box)
  (defboxer-key bu::mouse-double-click-on-bottom-right com-mouse-set-outermost-box)
  (defboxer-key bu::alt-mouse-click-on-bottom-right com-mouse-br-corner-expand-box)
  (defboxer-key bu::ctrl-mouse-click-on-bottom-right com-mouse-br-corner-collapse-box)
  (defboxer-key bu::mouse-double-click-on-top-right com-mouse-set-outermost-box)
  (defboxer-key bu::ctrl-mouse-click-on-top-left com-mouse-tl-corner-toggle-closet)
  (defboxer-key bu::alt-mouse-click-on-top-left com-mouse-tl-corner-toggle-closet)
  )



;; names

;; 3 button (these are sort of old)
#+(or sun symbolics dec3100)
(progn
  (defboxer-key bu::name-handle-mouse-left      com-mouse-border-name-box)
  (defboxer-key bu::name-handle-mouse-middle    com-mouse-border-name-box)
  (defboxer-key bu::name-handle-mouse-right     com-mouse-border-name-box)

  (defboxer-key bu::name-mouse-left      com-mouse-collapse-box)
  (defboxer-key bu::name-mouse-middle    com-mouse-move-point)
  (defboxer-key bu::name-mouse-right     com-mouse-expand-box)
  )

#+apple
(progn
  (defboxer-key bu::mouse-click-on-name-handle  com-mouse-border-name-box)
  (defboxer-key bu::mouse-click-on-name         com-mouse-move-point)
  )

#+win32
(progn
  (defboxer-key bu::mouse-click-on-name-handle com-mouse-border-name-box)
  (defboxer-key bu::mouse-click-on-name        com-mouse-move-point)
  )



;; toggle view

#+(or sun symbolics dec3100)
(progn
  (defboxer-key bu::bottom-left-mouse-left com-mouse-bl-corner-toggle-box-view)
  (defboxer-key bu::bottom-left-mouse-middle com-mouse-bl-corner-toggle-box-view)
  (defboxer-key bu::bottom-left-mouse-right com-mouse-bl-corner-toggle-box-view)
  )

#+apple
(progn 
  (defboxer-key bu::mouse-click-on-bottom-left com-mouse-bl-corner-toggle-box-view)
  (defboxer-key bu::command-mouse-click-on-bottom-left com-mouse-bl-pop-up)
  )

#+win32
(progn
  (defboxer-key bu::mouse-click-on-bottom-left
                com-mouse-bl-corner-toggle-box-view)
  (defboxer-key bu::mouse-right-click-on-bottom-left com-mouse-bl-pop-up)
  )

;; toggle closet

#+(or sun symbolics dec3100)
(progn
  (defboxer-key bu::top-right-mouse-left com-mouse-tr-corner-toggle-closet)
  (defboxer-key bu::top-right-mouse-right com-mouse-tr-corner-toggle-closet)
  (defboxer-key bu::top-right-mouse-middle com-mouse-tr-corner-toggle-closet)
  )

#+apple
(progn
  (defboxer-key bu::mouse-click-on-top-right com-mouse-tr-corner-expand-box)
  (defboxer-key bu::command-mouse-click-on-top-right com-mouse-tr-pop-up)
  (defboxer-key bu::option-mouse-click-on-top-right com-mouse-tr-corner-toggle-closet)
  )

#+win32
(progn
  (defboxer-key bu::mouse-right-click-on-top-right  com-mouse-tr-pop-up)
  (defboxer-key bu::mouse-click-on-top-right        com-mouse-expand-box)
  (defboxer-key bu::mouse-double-click-on-top-right com-mouse-set-outermost-box)
  (defboxer-key bu::ctrl-mouse-click-on-top-right   com-mouse-tr-corner-toggle-closet)
  (defboxer-key bu::alt-mouse-click-on-top-right    com-mouse-tr-corner-toggle-closet)
  )



;; toggle type

#+(or sun symbolics dec3100)
(progn
  (defboxer-key bu::type-mouse-left       com-mouse-type-tag-pop-up)
  (defboxer-key bu::type-mouse-middle     com-mouse-type-tag-pop-up)
  (defboxer-key bu::type-mouse-right      com-mouse-type-tag-pop-up)
  )

#+apple
(progn 
  (defboxer-key bu::mouse-click-on-type   com-mouse-border-toggle-type)
  (defboxer-key bu::command-mouse-click-on-type   com-mouse-type-tag-pop-up)
  )

#+win32
(progn
  (defboxer-key bu::mouse-click-on-type  com-mouse-border-toggle-type)
  (defboxer-key bu::mouse-right-click-on-type com-mouse-type-tag-pop-up)
  )

;; scrolling

#+(or sun symbolics dec3100)
(progn
  (defboxer-key bu::scroll-bar-mouse-middle       com-mouse-scroll-box)

  (defboxer-key bu::scroll-bar-mouse-left         com-mouse-page-scroll-box)
  (defboxer-key bu::scroll-bar-mouse-right        com-mouse-page-scroll-box)

  (defboxer-key bu::scroll-bar-mouse-left-twice   com-mouse-limit-scroll-box)
  (defboxer-key bu::scroll-bar-mouse-middle-twice com-mouse-limit-scroll-box)
  (defboxer-key bu::scroll-bar-mouse-right-twice  com-mouse-limit-scroll-box)
  (defboxer-key bu::ctrl-scroll-bar-mouse-left    com-mouse-limit-scroll-box)
  (defboxer-key bu::ctrl-scroll-bar-mouse-middle  com-mouse-limit-scroll-box)
  (defboxer-key bu::ctrl-scroll-bar-mouse-right   com-mouse-limit-scroll-box)
  (defboxer-key bu::meta-scroll-bar-mouse-left    com-mouse-limit-scroll-box)
  (defboxer-key bu::meta-scroll-bar-mouse-middle  com-mouse-limit-scroll-box)
  (defboxer-key bu::meta-scroll-bar-mouse-right   com-mouse-limit-scroll-box)
  )

#+apple
(progn
  (defboxer-key bu::mouse-click-on-scroll-bar                com-mouse-scroll-box)
  (defboxer-key bu::command-mouse-click-on-scroll-bar        com-mouse-page-scroll-box)
  (defboxer-key bu::option-mouse-click-on-scroll-bar         com-mouse-page-scroll-box)
  (defboxer-key bu::mouse-double-click-on-scroll-bar         com-mouse-limit-scroll-box)
  (defboxer-key bu::command-mouse-double-click-on-scroll-bar com-mouse-limit-scroll-box)
  (defboxer-key bu::option-mouse-double-click-on-scroll-bar  com-mouse-limit-scroll-box)
  )

#+win32
(progn
  (defboxer-key bu::mouse-click-on-scroll-bar            com-mouse-scroll-box)
  (defboxer-key bu::mouse-right-click-on-scroll-bar      com-mouse-page-scroll)
  (defboxer-key bu::ctrl-mouse-click-on-scroll-bar       com-mouse-limit-scroll-box)
  (defboxer-key bu::alt-scroll-bar-mouse-click-on-       com-mouse-limit-scroll-box)
  (defboxer-key bu::ctrl-mouse-right-click-on-scroll-bar com-mouse-limit-scroll-box)
  (defboxer-key bu::alt-mouse-right-click-on-scroll-bar  com-mouse-limit-scroll-box)
  )

;;;; temporary

(defboxer-key bu::F9-key com-toggle-closets)
(defboxer-key bu::R1-key com-prompt)
#+sun(defboxer-key (bu::R5-key 1) com-break)
#+apple(defboxer-key (bu::escape-key 1) com-break)
#+lispworks (defboxer-key (bu::escape-key 1) com-break)
#+(and lispworks win32) (defboxer-key bu::pause-key com-break)
#+(and lispworks macosx)(defboxer-key (bu::F13-key 3) com-break)
#+(and sun solaris) (defboxer-key (bu::enter-key 1) com-break)
;; adds pause breaks when in dribble recording mode
;; recording mode checks for this particular keypress (can't look
;; for names at the recording level)
#+apple
(defboxer-key (bu::f15-key 3) com-noop)
#+apple
(defboxer-key (bu::f14-key 3) com-noop)



;;;
(eval-when (load)
  (eval::boxer-toplevel-set 'bu::new-box-properties (make-new-box-properties-box))
  )