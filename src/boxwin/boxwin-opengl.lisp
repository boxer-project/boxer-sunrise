
;;;;
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
;;;;                                           +-Data--+
;;;;                  This file is part of the | BOXER | system
;;;;                                           +-------+
;;;;
;;;;
;;;;
;;;;  Modification History (most recent at top)
;;;;
;;;;   7/29/13 *font-sub-size-menu*
;;;;   3/31/13 change "Name/Spelling" menu item to the more indicative "Find Functions..."
;;;;   1/ 7/12 incr-bar: use capi::apply-in-pane-process for loadbar as well as message-pane mods
;;;;  12/17/12 :finished-launching to handle-osx-events, liberalized args in :destroy-callback for boxer-frame
;;;;   9/22/12 removed fixnum lossage in set-cursor{pos,-size}, blinker structure
;;;;  12/13/11 added Link to File to "File" menu
;;;;     load-progress-frame, incr-bar, used in window-system-specific-start-boxer-1
;;;;  12/17/10 with-open-blinker(s)
;;;;   6/23/10 put timestamps back into debug printing during startup
;;;;   3/21/10 file export menu added
;;;;   1/10/10 finished crash reporter, installed in boxer-system-error-restart{-loop}
;;;;   1/03/10 defvar *starting-window-{width, height}*, (possibly) set by prefs,
;;;;           checked by window-system-specific-start-boxer-1
;;;;  12/13/09 image-to-bitmap rewritten to use OpenGL stuff
;;;;  11/23/09 resize-handler, resize-handler-utility, *suppressed-actions*
;;;;   7/06/09 fixed typo in with-mouse-tracking which caused the body to run continously
;;;;           even if the mouse was not moved
;;;;   6/06/09 main loop now uses boxer::repaint-with-cursor-relocation
;;;;  10/30/08 boxer-click-handler for the mac encodes multi button mice as shifted clicks
;;;;   2/25/08 added QUIT item to file menu for #+win32
;;;;   1/02/08 abort-event? and abort-gesture? check for #\escape
;;;;  12/20/05 paste, paste-{test,pict}, image-to-bitmap
;;;;  12/15/05 window-system-specific-start-boxer-1 uses lw:lisp-image-name to
;;;;           set starting directory instead of hcl::get-working-directory
;;;;   9/18/05 changed to #+lispworks4 for capi::interface-keys-style
;;;;   7/20/05 *fullscreen-window-p* changed from nil to T
;;;;  12/28/04 added "Print" back to File menu
;;;;  12/22/04 Help menu changes: (boxwin-lw.lisp)
;;;;  12/21/04 new 30 day expiration for window-system-specific-start-boxer,
;;;;           valid-key-or-quit support function
;;;;   6/15/04 removed mouse documentation code (moved to mousedoc.lisp)
;;;;   7/10/03 fixed bug in window-system-specific-start-boxer which required
;;;;           license key even with expiration date
;;;;   6/26/03 commented out "Link to Windows File" and "Print" menu items in the
;;;;           "File" menu. Put them back in when they actually do something...
;;;;   6/22/03 window-system-specific-start-boxer-1 loads double clicked boxes
;;;;           in as file boxes not as top level world
;;;;   6/11/03 finished license handling in window-system-specific-start-boxer
;;;;   6/10/03 added capi::interface-keys-style method for emacs style key handling
;;;;   4/19/03 merged current LW and MCL files, updated copyright
;;;;   3/29/03 hack to make set-window-name work better
;;;;  12/01/02 changed order of "Place" menu to put "Toplevel" at the top
;;;;  10/23/02 removed "Forget Here" from places menu until "UC clean" reimplementation
;;;;  10/06/02 load-boxer-extensions added to window-system-specific-start-boxer
;;;;   9/12/02 load-startup-file function added which uses *uc-free-starting-box-file*
;;;;           window-system-specific-start-boxer changed to use load-startup-file
;;;;   6/05/02 moved start box file loading in window-system-specific-start-boxer &
;;;;           restored previous order, current order is setup-editor (generic),
;;;;           setup-evaluator, FINALLY check for start box and (re)setup-editor
;;;;   6/02/02 window-system-specific-start-boxer setup-evaluator needs to come BEFORE
;;;;           setup-editor
;;;;   3/13/02 boxer-pane-mouse-down? flushes any pending input because if we are
;;;;           checking the state of the mouse buttons, we don't want any mouse down
;;;;           state to appear as a click
;;;;   1/15/02 added boxer-pane-mouse-x and boxer-pane-mouse-y
;;;;   1/11/02 added new hints to name-pane def to fix bug in file save window resizing
;;;;  11/05/01 commented out graphics popup documentation from document-mouse-dispatch
;;;;  10/16/01 popup-only? arg added to mouse doc functions
;;;;  10/28/01 added shifted motion event handlers to :input model to fix bug
;;;;  10/23/01 shift-key-pressed? stub implemented
;;;;  10/22/01 mouse-button-state, mouse-{down,left,middle,right}?
;;;;  10/19/01 key-down?, control-key? & alt-key?
;;;;   5/21/01 no-more-input? fixed to actually check to input queue
;;;;   5/15/01 added user-event-in-queue? to check for keys & clicks (and NOT mouse doc)
;;;;           in maybe-unify-mouse-click
;;;;           undocument-mouse-dispatch fixed bad logic in call to popup-undoc-view-flip
;;;;   5/14/01 removed stepper
;;;;   5/13/01 maybe-unify-mouse-click uses new function peek-next-key-or-mouse-event
;;;;           added fullscreen support to window-system-specific-start-boxer
;;;;   5/11/01 get-boxer-event filters it's output to check for key/mouse events and
;;;;           to ignore other funcallable "events" like document-mouse
;;;;   5/10/01 Fixed keyboard menu equivalents from "Opt" to "Alt"
;;;;   5/01/01 make sure redisplayed is called before any popup documentation
;;;;   4/29/01 mouse doc functions done & debugged
;;;;   4/23/01 mouse doc status, (un)document-mouse
;;;;   4/11/01 updated mouse doc status line strings
;;;;   3/17/01 added keyboard equivalents to menu titles
;;;;   3/14/01 started porting mouse documentation utilities
;;;;   3/07/01 mouse release handlers now hack possible multiply pressed buttons
;;;;   3/04/01 aded :control and :meta clauses for button release in window input def
;;;;   2/17/01 added *fullscreen-window-p*
;;;;  10/23/00 resize-handler checks for already correct outermost screen box size
;;;;           BEFORE calling a full redisplay.  This fixes redisplays triggered by
;;;;           changing the size of the status-line
;;;;  10/18/00 added suppression capabilities to boxer-expose-window-handler
;;;;           using the with-suppressed-exposure-handling macro
;;;;  10/16/00 changed name-pane from capi:display-pane to capi:title-pane
;;;;  10/15/00 :print-function changed to :title-function in menu defs
;;;;  10/14/00 installed print and enabled functions in window menus
;;;;   7/12/00 added get-character-input
;;;;   7/11/00 uncommented prefs init in start-boxer
;;;;   6/28/00 keyboard-interrupt handling added
;;;;   5/29/00 CORRECTLY installed prefs in edit menu
;;;;   5/25/00 installed prefs in edit menu
;;;;   4/12/00 finally got sizing right...
;;;;   4/10/00 :best-width/height totally worthless, trying plain :width/height...
;;;;   4/06/00 :best-width/height startup window sizing
;;;;   4/03/00 have to include extra input models for tracking mouse coords during hold
;;;;  12/03/99 added mouse support
;;;;  12/18/98 started file
;;;;

(in-package :boxer-window)

;(load (example-file "opengl/examples/load"))
;(use-package :opengl)

;; Vars

#+cocoa
(defvar *cocoa-boxer-interface* nil)

(defconstant *number-of-mouse-buttons* 3)

(defvar *redisplayable-windows* nil
  "This is a list of all the windows which should be redisplayed when
   REDISPLAY is called." )

(defvar *redisplayable-window-outermost-box-alist* nil
  "An alist that keeps track of the outermost screen box for each
   redisplayable window in *redisplayable-windows*. ")

(defvar *point-blinker* nil)

;; this should be initialized from some system parameter
;; and also adjustable as a preference...
;; Graphics Ports can now use the OS highlight color, as specified via
;;  the Windows Control Panel (Display > Appearance > Advanced)
;;  the Mac OS X System Preferences (General > Highlight Color)
;;  the X11/Motif resources colorHighlight and colorHighlightText
;; You can supply :background :color_highlight
;; (color::get-color-spec 'win32::color_highlight)
;; should get set on window creation and probably checked whenever the boxer
;; window gets the focus back

(defvar *blinker-alpha-value* .3)

(defvar *blinker-color* (make-ogl-color .3 .3 .9 .5))

(defun update-blinker-color ()
  #+win32
  (let ((bc (color:get-color-spec 'win32::color_highlight)))
    (setq *blinker-color* (make-ogl-color (color:color-red bc)
                                          (color:color-green bc)
                                          (color:color-blue bc)
                                          *blinker-alpha-value*))))

;; alternatively, we could make the *boxer-frame* inherit from
;; the capi::title-pane class amd use the :title init arg
;(defvar *title-pane* (make-instance 'title-pane :text "Boxer Title Pane"
;                                    ;; 12 pt Times bold for the title
;                                    :font (gp:gf nil "times" :bold :roman 12))) ;


(defvar  *boxer-status-pane-height* 15.)

;; the status line, called name pane for historic reasons
;; there's probably still code out there that references it
(defvar *name-pane*)

(defparameter *boxer-pane-minimum-width*  300)
(defparameter *boxer-pane-minimum-height* 200)

(defparameter *boxer-window-left-margin* 50)
(defparameter *boxer-window-right-margin* 50)


;;;;; Menus

(defvar *font-sub-font-menu*
  (make-instance 'capi::menu :title "Font"
                 :items
                 (list (make-instance 'capi::menu-component
                                      :interaction :single-selection
                                      :popup-callback 'set-font-menu-selection
                                      :items boxer::*font-families*
                                      :callback 'font-menu-action))))

;; numbers are from draw-low-lw
(defvar *font-sub-size-menu*
  (make-instance 'capi::menu :title "Size"
                 :items-function
                 #'(lambda (interface)
                     (declare (ignore interface))
                     (list (make-instance 'capi::menu-component
                                          :interaction :single-selection
                                          :popup-callback 'set-font-size-menu-selection
                                          ; :items box::*bfd-font-size-values*
                                          ; :print-function 'box::font-size-menu-item-name
                                          :items
                                          (mapcar #'(lambda (data)
                                                      (make-instance 'capi:menu-item
                                                                     :data data
                                                                     :print-function
                                                                     'box::font-size-menu-item-name))
                                                  boxer::*bfd-font-size-values*)
                                      :callback 'font-size-menu-action)))))
;                 :items
;                 (list (make-instance 'capi::menu-component
;                                      :interaction :single-selection
;                                      :popup-callback 'set-font-size-menu-selection
;                                      ; :items box::*bfd-font-size-values*
;                                      ; :print-function 'box::font-size-menu-item-name
;                                      :items
;                                      (mapcar #'(lambda (data)
;                                                  (make-instance 'capi:menu-item
;                                                                 :data data
;                                                                 :print-function
;                                                                 'box::font-size-menu-item-name))
;                                              boxer::*bfd-font-size-values*)
;                                      :callback 'font-size-menu-action))))

(defvar *font-sub-style-menu*
  (make-instance 'capi::menu :title "Style"
                 :items
                 (list (make-instance 'capi:menu-component
                                      :interaction :multiple-selection
                                      :popup-callback 'set-font-style-menu-selection
                                      :items '(:plain :bold :italic)
                                      :callback 'font-style-menu-action))))

(defvar *font-sub-color-menu*
  (make-instance 'capi::menu :title "Color"
                 ; items need to be init'ed AFTER the colors have been setup
                 ; so we can use the color as the data of the menu-items
                 ; this is just a placeholder
                 :items
                 (list (make-instance 'capi::menu-component
                                      :interaction :single-selection
                                      :popup-callback 'set-font-color-menu-selection
                                      :items '(:black :white :red :green :blue
                                               :cyan :magenta :yellow :purple :gray :orange)
                                      :callback 'font-color-menu-action))))

(defun open-recent-items (interface)
  "Build the menu items for recently opened files."
  (mapcar #'(lambda (x)
              (let* ((path (cdr (assoc :path x)))
                     (text (file-namestring path)))
                (make-instance 'capi:menu-item :data path :text text)))
    (boxer::get-recent-files)))

(defvar *file-open-recent-menu*
  (make-instance 'capi::menu-component
                 :items
                 (list
                   (make-instance 'capi:menu-item :title "Open..."
                                      :accelerator #\o :callback 'open-file)
                   (make-instance 'capi::menu :title "Open Recent"
                                      :items-function 'open-recent-items
                                      :callback #'(lambda (data interface)
                                        (boxer::com-open-box-file t data))))))

(defvar *file-export-menu*
  (make-instance 'capi::menu :title "Export..."
                 :items
                 (list (make-instance 'capi:menu-component
                                      :interaction :single-selection
                                      :items-function 'box::file-export-items-function
                                      :callback 'box::file-export-menu-action))))

#+cocoa
(capi:define-interface cocoa-boxer-interface (capi:cocoa-default-application-interface)
  ()
  (:menus
   (application-menu "BoxerApp"
               ((:component
                 (
                  ("About Boxer"
                   :callback 'about-boxer-function
                   :callback-type :none)
                 ))
                (:component
                 (
                  ("Preferences..." :callback 'menu-prefs)))
                (:component
                 ()
                 ;; This is a special named component where the CAPI will
                 ;; attach the standard Services menu.
                 :name :application-services)
                (:component
                 (
                  ("Hide Boxer"
                   :accelerator "accelerator-h"
                   :callback-data :hidden)
                  ("Hide Others"
                   :accelerator "accelerator-meta-h"
                   :callback-data :others-hidden)
                  ("Show All"
                   :callback-data :all-normal
                   :callback #'(setf capi:top-level-interface-display-state)
                   :callback-type :data-interface)
                  ))
                (:component
                 (
                  ("Quit"
                   :accelerator #\q :callback-type :interface :callback 'capi:destroy))
                 )))
  (file-menu "File" ((:component
                      (
                       ("New"))))))
  (:menu-bar application-menu file-menu)
  (:default-initargs
   :title "Boxer"
   ;; sgithens TODO -
   ;;:application-menu 'application-menu
   ;;:message-callback 'handle-OSX-events
   ))

(eval-when (compile load eval)
  (capi:define-interface load-progress-frame ()
    ()
    (:panes
    (loadbar-pane capi::progress-bar)
    (message-pane capi:display-pane))
    (:layouts
    (progree-layout capi:column-layout
                    '(loadbar-pane message-pane)
                    :columns 1 :rows 2 :x-uniform-size-p t))
  ;  (:menus
  ;   (min-menu ""
  ;             ((:component
  ;               (
  ;                ("Quit"
  ;                 :callback 'capi:destroy))))))
    ;(:menu-bar min-menu)
    (:default-initargs
    :title "Loading Boxer..."
    :auto-menus nil
    :best-x 250 :best-y 500 :best-width 500 :best-height 50
    :window-styles '(:borderless :always-on-top :ignores-keyboard-input)))
)

;; might have to go to ignore-errors if problems continue
(defmethod incr-bar ((self load-progress-frame) percentage
                     &optional newtext (cr? T))
  (let ((loadbar-pane (slot-value self 'loadbar-pane)))
    (capi::apply-in-pane-process loadbar-pane
                                 #'(setf capi:range-slug-start)
                                 percentage loadbar-pane))
  (when (not (null newtext))
    (let* ((message-pane (slot-value self 'message-pane))
           (existing-text (capi:display-pane-text message-pane))
           (new-text (progn
                       (cond ((listp existing-text))
                             ((stringp existing-text)
                              (cond ((string= existing-text "")
                                     (setq existing-text nil))
                                    (t (setq existing-text (list existing-text))))))
                       (cond ((null existing-text)
                              (list newtext))
                             ((null cr?)
                              (append (butlast existing-text)
                                      (list
                                       (concatenate 'string (car (last existing-text))
                                                    " " newtext))))
                             (t
                              (append existing-text (list newtext)))))))
      (capi::apply-in-pane-process message-pane
                                   #'(setf capi::display-pane-text)
                                   new-text message-pane))))

(eval-when (compile load eval)
(capi:define-interface boxer-frame ()
  ()
  (:panes
   (name-pane capi:title-pane :text "status line"
              :min-width nil :max-width :screen-width
              :visible-min-height *boxer-status-pane-height*
              :visible-max-height *boxer-status-pane-height*)
   (boxer-pane opengl::opengl-pane
               :configuration '(:rgba t :double-buffered t :aux 1)
               :input-model '(((:button-1 :press) boxer-click-1-handler)
                              ((:button-2 :press) boxer-click-2-handler)
                              ((:button-3 :press) boxer-click-3-handler)
                              ;;shifted single clicks
                              ((:button-1 :press :control) boxer-c-click-1-handler)
                              ((:button-2 :press :control) boxer-c-click-2-handler)
                              ((:button-3 :press :control) boxer-c-click-3-handler)
                              ((:button-1 :press :meta)    boxer-a-click-1-handler)
                              ((:button-2 :press :meta)    boxer-a-click-2-handler)
                              ((:button-3 :press :meta)    boxer-a-click-3-handler)
                              ((:button-1 :press :control :meta) boxer-c-a-click-1-handler)
                              ((:button-2 :press :control :meta) boxer-c-a-click-2-handler)
                              ((:button-3 :press :control :meta) boxer-c-a-click-3-handler)
                              ;; double clicks
                              ((:button-1 :second-press) boxer-dclick-1-handler)
                              ((:button-2 :second-press) boxer-dclick-2-handler)
                              ((:button-3 :second-press) boxer-dclick-3-handler)
                              ;; shifted double clicks
                              ((:button-1 :second-press :control)
                               boxer-c-dclick-1-handler)
                              ((:button-2 :second-press :control)
                               boxer-c-dclick-2-handler)
                              ((:button-3 :second-press :control)
                               boxer-c-dclick-3-handler)
                              ((:button-1 :second-press :meta)
                               boxer-a-dclick-1-handler)
                              ((:button-2 :second-press :meta)
                               boxer-a-dclick-2-handler)
                              ((:button-3 :second-press :meta)
                               boxer-a-dclick-3-handler)
                              ((:button-1 :second-press :control :meta)
                               boxer-c-a-dclick-1-handler)
                              ((:button-2 :second-press :control :meta)
                               boxer-c-a-dclick-2-handler)
                              ((:button-3 :second-press :control :meta)
                               boxer-c-a-dclick-3-handler)
                              ;; mouse button release
                              ((:button-1 :release) boxer-mouse-release-1-handler)
                              ((:button-2 :release) boxer-mouse-release-2-handler)
                              ((:button-3 :release) boxer-mouse-release-3-handler)
                              ((:button-1 :release :control)
                               boxer-mouse-release-1-handler)
                              ((:button-2 :release :control)
                               boxer-mouse-release-2-handler)
                              ((:button-3 :release :control)
                               boxer-mouse-release-3-handler)
                              ((:button-1 :release :meta)
                               boxer-mouse-release-1-handler)
                              ((:button-2 :release :meta)
                               boxer-mouse-release-2-handler)
                              ((:button-3 :release :meta)
                               boxer-mouse-release-3-handler)
                              ((:button-1 :release :control :meta)
                               boxer-mouse-release-1-handler)
                              ((:button-2 :release :control :meta)
                               boxer-mouse-release-2-handler)
                              ((:button-3 :release :control :meta)
                               boxer-mouse-release-3-handler)
                              ;; updates the mouse tracking vars
                              (:motion boxer-track-and-doc-mouse-handler)
                              ;; have to track while the mouse is down too...
                              ((:button-1 :motion) boxer-track-mouse-handler)
                              ((:button-2 :motion) boxer-track-mouse-handler)
                              ((:button-3 :motion) boxer-track-mouse-handler)
                              ;; track shifted mouse down...
                              ((:button-1 :motion :control)
                               boxer-track-mouse-handler)
                              ((:button-2 :motion :control)
                               boxer-track-mouse-handler)
                              ((:button-3 :motion :control)
                               boxer-track-mouse-handler)
                              ((:button-1 :motion :meta) boxer-track-mouse-handler)
                              ((:button-2 :motion :meta) boxer-track-mouse-handler)
                              ((:button-3 :motion :meta) boxer-track-mouse-handler)
                              ((:button-1 :motion :control :meta)
                               boxer-track-mouse-handler)
                              ((:button-2 :motion :control :meta)
                               boxer-track-mouse-handler)
                              ((:button-3 :motion :control :meta)
                               boxer-track-mouse-handler)
                              ;; what are keys ?
                              ;((:key :press)  boxer-key-handler)
                              ;((#\. :control :press) boxer-abort-handler)
                              ;((#\g :control :press) boxer-abort-handler)
                              (:gesture-spec gesture-spec-handler))
               :display-callback 'boxer-expose-window-handler
               :resize-callback 'resize-handler
               :visible-min-width  *boxer-pane-minimum-width*
               :visible-min-height *boxer-pane-minimum-height*
               ))
  (:layouts
   (boxer-layout capi:column-layout
                 '(name-pane boxer-pane)
                 :columns 1 :rows 2 :y-gap 1 :x-uniform-size-p t))
  ;; menu item actions are defined in lw-menu.lisp
  (:menus
    (file-menu "File" (
                       ;; The Open and Open Recent components are generated and inserted programatically at runtime.
                       (:component
                        (("Save" :accelerator #\s :callback 'save-document
                                 :enabled-function 'save-menu-item-enabled-function)
                         ("Save As..." :callback 'save-document-as)
                         ("Close" :callback 'close-file-box)))
                       (:component
                        (("File Toggle" :callback 'menu-file-toggle  ;; Mark box as file
                                        :title-function 'menu-file-toggle-print)
                         ("Mark Box as File, Save..." :callback 'save-box-as) )) ; Previously "Save Box As..."
                       (:component
                        (("Link to File" :callback 'open-xref)))
                       (:component
                        (("Print" :accelerator #\p :callback 'window-hardcopy
                                  :callback-type :interface)))
                       #+win32 ; Macs hang this on the application menu
                       (:component
                        (("Quit" :callback 'lw-quit)))))
    (edit-menu "Edit" ((:component
                        (("Cut" :accelerator #\x  :callback 'menu-cut-region)
                         ("Copy" :accelerator #\c  :callback 'menu-copy-region)
                         ("Paste" :accelerator #\v :callback 'menu-clipboard-paste)
                         ("Yank" :accelerator #\y :callback 'menu-retrieve)
                         ;; Previously in 2014, before fixing up `paste` to check if the
                         ;; last clipboard item was from boxer/lisp we had:
                         ;; Paste - bound to to #'menu-yank -> boxer::com-yank
                         ;; Paste from Clipboard #'menu-clipboard-paste -> (bw::paste *boxer-frame*)
                         ;; Paste Graphics - #'menu-paste-graphics -> (bw::paste-pict)
                         ))
                       (:component
                        (("Select Box" :callback 'menu-select-box-contents
                                       :enabled-function 'box-check-menu-item-enabled?)))
                       (:component (("Preferences..." :callback 'menu-prefs)))
                       ("Find" :accelerator #\f :callback 'menu-find)))
    (make-menu "Make" ((:component
                        (("Data	{" :callback 'menu-data-box)
                         ("Doit	[" :callback 'menu-doit-box)))
                       (:component
                        (("Turtle" :accelerator #\T
                                   :callback 'menu-turtle-box)
                         ("Graphics" ;:accelerator "alt-g"
                          :callback 'menu-graphics-box)
                         ("Sprite" ;:accelerator "alt-s"
                          :callback 'menu-sprite-box)))
                       (:component
                        (("Port" :accelerator #\P
                          :callback 'menu-port)))
                       ("Unbox" :accelerator #\@ :callback 'menu-unbox)))
    (box-menu "Box" ((:component (("Name	|" :callback 'menu-name
                                            :enabled-function
                                            'box-check-menu-item-enabled?)))
                     (:component
                      (("Closet" :accelerator #\O
                                 :callback 'menu-closet-flip
                                 :title-function 'closet-menu-item-print
                                 :enabled-function 'box-check-menu-item-enabled?)
                       ("Graphics" :accelerator #\}
                                   :callback 'menu-graphics-flip
                                   :title-function 'graphics-menu-item-print
                                   :enabled-function 'graphics-flip-menu-item-enabled?)
                       ("Data/Doit" :accelerator #\]
                                    :callback 'menu-data-doit-flip
                                    :title-function 'type-menu-item-print
                                    :enabled-function
                                    'type-flip-menu-item-enabled?)
                       ("Transparency" :accelerator #\e
                                       :callback 'menu-transparency-flip
                                       :title-function 'trans-menu-item-print
                                       :enabled-function
                                       'box-check-menu-item-enabled?)))
                     display-props-sub-menu
                     boxtops-sub-menu
                     #+win32 ; broken on the macs...
                     (:component (("Box Properties"
                                   :callback 'menu-box-properties
                                   :enabled-function
                                   'box-check-menu-item-enabled?)))
                     ("Key/Mouse Mode	Ctrl-Alt-V"
                      :callback 'menu-key-mouse-mode
                      :title-function 'vanilla-menu-item-print)))
    (do-menu "Do" ((:component (("Do Line" #+cocoa :accelerator #+cocoa #\CR
                                           :callback 'menu-do-line)))
                   ;(:component (("Step" :callback 'menu-step)))
                   (:component (("Stop	Ctrl-." :callback 'menu-stop)))))
    (font-menu "Font" (("Zoom +" :accelerator #\=
                        :callback 'menu-font-bigger)
                       ("Zoom -" :accelerator #\-
                        :callback 'menu-font-smaller)
                       ;*font-sub-font-menu* *font-sub-size-menu*
                       ;*font-sub-style-menu* *font-sub-color-menu*
                       ))
    (place-menu "Places" ((:component (("Top Level" :callback 'menu-top-level)))
                          (:component (("Set Mark	Ctrl-Space"
                                        :callback 'menu-set-mark)
                                       ("Last Mark" ;:accelerator "Alt-Space"
                                        :callback 'menu-last-mark)))
                          (:component (("Zoom to Target" :accelerator #\z
                                                         :callback 'menu-zoom-to-target)))
                          (:component (("Remember Here" :accelerator #\/
                                                        :callback 'menu-remember-here))))
                :items-function 'calculate-places)
    (help-menu "Help" ((:component (("Inputs" :callback 'menu-inputs-help)
                                    ("Find Functions..." :accelerator #\?
                                                     :callback 'menu-name-spelling-help)
                                    ("Key/Mouse" ;:accelerator "Alt-?"
                                     :callback 'menu-key-mouse-help)))
                       (:component (("Repaint" :accelerator #\r :callback 'menu-redisplay)))))
    ;; submenus
    (display-props-sub-menu "Display Properties..."
                            (("Zoom Control" :callback 'menu-zoom-toggle
                                             :title-function 'menu-zoom-print)
                             ("Shrink Exit"  :callback 'menu-exit-shrink-toggle
                                             :title-function 'menu-exit-shrink-print)
                             ("Auto Fill" :callback 'menu-auto-fill-toggle
                                          :title-function 'menu-auto-fill-print)
                             ("Shrink Proof" :callback 'menu-shrink-proof-toggle
                                             :title-function 'menu-shrink-proof-print)))
    (boxtops-sub-menu "Boxtops..."
                      (:standard :folder :name-only :graphics-named-boxtop)
                      :print-function #'string-capitalize
                      :callback #'menu-boxtop-callback)
    )
   (:menu-bar file-menu edit-menu make-menu box-menu do-menu font-menu place-menu help-menu)
;;; ;  (:menu-bar file-menu)
  (:default-initargs
   :title "Boxer"
   :width  *boxer-frame-initial-width*
   :height *boxer-frame-initial-height*
   :confirm-destroy-function 'check-for-unsaved-boxes
   :destroy-callback #'(lambda (&rest args)
                         (declare (ignore args ))
                         (user::quit))
   ))
)

(defvar *osx-events-log* nil)
(defvar *pending-osx-events* nil)

(defun handle-OSX-events (interface message &rest args)
  (declare (ignore interface))
  (push (cons message args) *osx-events-log*)
  (case message
    (:open-file
     (if (null *display-bootstrapping-no-boxes-yet*)
         ;; might want to do this if we are running in eval (e.g. long simulation) and the user
         ;; double clicks a file
         (safe-open-double-clicked-file args)
       (push (cons message args) *pending-osx-events*)))
    #+lispworks6.1 (:finished-launching)
    (t (status-line-display 'boxer::boxer-editor-error
                            (format nil "Unhandled OSX event: ~A" message)))))

(defun safe-open-double-clicked-file (args)
  (ignore-errors
    (cond ((not (null args))
           (if (listp (car args))
               (dolist (p (car args))
                 (boxer::insert-cha boxer::*point* (boxer::read-internal p)))
             (boxer::insert-cha boxer::*point*
                                (boxer::read-internal (car args))))
           (boxer::repaint)))))

(defvar *boxer-frame-initial-width* 800) ;(- (screen-width (convert-to-screen)) 200)
(defvar *boxer-frame-initial-height* 600);(- (screen-height (convert-to-screen))100)

;; some menu inits have to be preformed after the window is displayed
(defun fixup-menus ()
  ;; font color menu needs to be updated
  ;*font-sub-color-menu*
  ;; all the font sub menus need to be added to the font menu
  (setf (capi::menu-items (slot-value *boxer-frame* 'font-menu))
        (append (capi::menu-items (slot-value *boxer-frame* 'font-menu))
                (list *font-sub-font-menu* *font-sub-size-menu*
                      *font-sub-style-menu* *font-sub-color-menu*)))
  (setf (capi::menu-items (slot-value *boxer-frame* 'bw::file-menu))
        (let ((fmis (capi::menu-items (slot-value *boxer-frame* 'bw::file-menu))))
          (append (butlast fmis) (list bw::*file-export-menu*) (last fmis))))
  ;; Recent files
  ;; (boxer::reset-recent-files)
  (setf (capi::menu-items (slot-value *boxer-frame* 'bw::file-menu))
        (let ((fmis (capi::menu-items (slot-value *boxer-frame* 'bw::file-menu))))
          (append  (list bw::*file-open-recent-menu*) fmis))))

(defun window-system-specific-make-boxer ()
  (setq *boxer-frame* (make-instance 'boxer-frame))
  ;; after creation, set some variables
  (setq *boxer-pane* (slot-value *boxer-frame* 'boxer-pane)
        *name-pane*  (slot-value *boxer-frame* 'name-pane))
  (push *boxer-pane* *redisplayable-windows*)
  (setq *point-blinker* (make-blinker *boxer-pane*))
  #+cocoa
  (capi:set-application-interface (setq *cocoa-boxer-interface*
                                        (make-instance 'cocoa-boxer-interface)))
  )

;; *** should be acceptable to (setf (graphics-state-pattern ...
;; the number vectors are lists of either 1 or 0
(defun make-pattern (number-vectors)
  (declare (ignore number-vectors))
  )

(defvar *old-world* nil)

(defvar *fullscreen-window-p* T
  "Should boxer occupy the entire screen when starting up ?")

;; 0 is interpreted as do whatever the window system wants
(defvar *starting-window-width*  0)
(defvar *starting-window-height* 0)

(defvar *setup-editor-with-news?* t)

(defvar *uc-free-starting-box-file* "boxer-init.box")

(defvar *default-starting-box-file* "start.box")

(defvar *display-bootstrapping-no-boxes-yet* t)

(defun window-system-specific-start-boxer ()
  (let ((boot-start-time (get-internal-real-time))
        (progress-bar (make-instance 'load-progress-frame)))
    (flet ((start-boxer-progress (fstring time percentage)
             (incr-bar progress-bar percentage
                       (format nil fstring (- time boot-start-time)))))
      (capi:display progress-bar)
      (start-boxer-progress "Starting ~D" (get-internal-real-time) 10)
      (when (member "-debug" sys:*line-arguments-list* :test #'string-equal)
        (break "Start Boxer"))
      (when (boxer::box? *old-world*)
        (setf (boxer::slot-value *old-world* 'boxer::screen-objs) nil))
      (setq boxer-eval::*current-process* nil)
      (setq *old-world* boxer::*initial-box*)
      ;; extensions
      (setq boxer::*starting-directory-pathname* (lw:lisp-image-name))
      ;; sgithens TODO - Removing extensions for now March 7, 2020
      ;; (boxer::load-boxer-extensions)
      ;; (start-boxer-progress "Loaded Extensions ~D" (get-internal-real-time) 20)

      ;; load prefs if they exists
      (let ((pf (boxer::default-lw-pref-file-name)))
        (when (and pf (probe-file pf))
          (boxer::handle-preference-initializations pf)))
      (start-boxer-progress "Initialized Preferences ~D"
                            (get-internal-real-time) 30)
      ;; maybe set the size of the boxer window...
      ;; check window size prefs, they will be overidden by the following
      ;; fullscreen-window check
      (let ((screen (capi:convert-to-screen)))
        (when (> *starting-window-width* 0)
          (capi:set-hint-table *boxer-frame* (list :width *starting-window-width*)))
        (when (> *starting-window-height* 0)
          (capi:set-hint-table *boxer-frame* (list :height *starting-window-height*)))
        ;; fullscreen check AFTER prefs are loaded but BEFORE display ?
        (when *fullscreen-window-p*
          (capi:set-hint-table *boxer-frame*
                          (list :x 0 :y 0
                                :width (- (capi:screen-width screen) 10)
                                :height (- (capi:screen-height screen) 120)))))
      (start-boxer-progress "Setting Hints ~D" (get-internal-real-time) 40)
      (capi:display *boxer-frame*)
;     (break "After Display...")
      (start-boxer-progress "Display ~D" (get-internal-real-time) 50)
      (when (member "-debug" sys:*line-arguments-list* :test #'string-equal)
        (opengl:describe-configuration *boxer-pane*))
      ;; move to inits
;     (let ((gs (gp::get-graphics-state *boxer-pane*)))
;     (setf (gp::graphics-state-foreground gs) boxer::*foreground-color*))
      ;; opengl equivalent would be...
      (opengl:rendering-on (*boxer-pane*)
                    (initialize-ogl-color-pool)
                    (boxer::initialize-gray-patterns)
                    (boxer::initialize-colors)
                    (ogl-set-color box::*foreground-color*)
                    ;; do other OpenGL inits...
                    (setq *ogl-current-color-vector* (make-ogl-color 0.0 0.0 0.0)
                          *blinker-color* (make-ogl-color .3 .3 .9 .5))
                    (opengl:gl-enable opengl:*gl-scissor-test*)
                    (opengl::gl-enable opengl::*gl-line-smooth*)
                    (opengl::gl-enable opengl::*gl-polygon-smooth*)
                    (opengl::gl-enable opengl::*gl-blend*)
                    (opengl::gl-blend-func opengl::*gl-src-alpha* opengl::*gl-one-minus-src-alpha*)
                    (opengl::gl-hint opengl::*gl-line-smooth-hint* opengl::*gl-nicest*))
      (boxer::fill-bootstrapped-font-caches)
      ;; #+freetype-fonts
      (boxer::load-freetype-faces)
      (let ((boxer::%private-graphics-list nil))
        ;; needed by shape-box updater in the redisplay inits but not set until
        ;; (boxer-eval::setup-evaluator) farther down
        (run-redisplay-inits))

      (start-boxer-progress "RDP inits ~D" (get-internal-real-time) 60)
      (boxer::load-appdata)
      (fixup-menus)
      (setup-editor *old-world*)
      (setq *display-bootstrapping-no-boxes-yet* nil)
      (start-boxer-progress "Editor ~D" (get-internal-real-time) 70)
      (boxer-eval::setup-evaluator)
      (start-boxer-progress "Eval ~D" (get-internal-real-time) 80)
      ;; should handle double clicked files here...
      (multiple-value-bind (start-box as-world?)
          (load-startup-file)
        (when (boxer::box? start-box)
          (cond ((not (null as-world?))
                 (setup-editor start-box))
                (t (setup-editor (boxer::make-box (list (list start-box))))))))
      (unless boxer::*boxer-version-info*
        (setq boxer::*boxer-version-info*
              (format nil "~:(~A~) Boxer" (machine-instance))))
      (set-cursor-visibility *point-blinker* t)
      ;; wait a sec
      ;; now that everything is defined, we can safely run redisplay
      (resize-handler-utility)
      ;; and check for initial double clicked file box
      (when (eq :open-file (caar *pending-osx-events*))
        (safe-open-double-clicked-file (cdar *pending-osx-events*))
        (setq *pending-osx-events* nil))
      (start-boxer-progress "Starting Command Loop ~D" (get-internal-real-time) 100)
      (sleep 1)
      (capi:destroy progress-bar)
      (boxer-process-top-level-fn *boxer-pane*))))

;; check cdr of SYSTEM:*LINE-ARGUMENTS-LIST*
(defun load-startup-file ()
  (cond ((not (null (cadr sys::*line-arguments-list*)))
         (ignore-errors
           (boxer::load-binary-box-internal (cadr sys::*line-arguments-list*))))
        ((not (null *setup-editor-with-news?*))
         (cond ((probe-file *uc-free-starting-box-file*)
                (ignore-errors (values (boxer::load-binary-box-internal
                                        *uc-free-starting-box-file*)
                                       T)))
               ((probe-file *default-starting-box-file*)
                (if (not (null boxer::*uc-copyright-free*))
                    (start-box-copyright-warning)
                  (ignore-errors (values (boxer::load-binary-box-internal
                                          *default-starting-box-file*)
                                         T))))))))

(defun start-box-copyright-warning ()
  (boxer::make-box '(("Warning: start.box file detected")
                     ("The name of the initial box file has been changed")
                     ("to boxer-init.box"))))


;;; We would like to make the editor somewhat reentrant for things like
;;; recursive edit levels this allows us to do things like call the
;;; evaluator inside of an INPUT box

;;; THis should really be moved into a machine independent file....

;;; what about *generic-port* and *suitcase-region* ?

(defmacro boxer-editor-bindings (recursive-p &body body)
  `(progv '(*region-being-defined* boxer::*editor-numeric-argument*
      boxer::*propagate-modified-messages?*)
          `(nil nil ,',recursive-p)
     (unwind-protect
   (progn . ,body)
;       (when (not (null *region-being-defined*))
;       (flush-region *region-being-defined*))
)))

(defun boxer-process-top-level-fn (window)
  (declare (ignore window))
  (boxer::enter (boxer::point-box))
  (boxer::force-repaint)
  (boxer-command-loop))

(defun boxer-command-loop ()
  (boxer-system-error-restart-loop
    (boxer-editor-bindings nil
      (boxer-command-loop-internal))))

(defvar just-redisplayed? nil)

(defvar *boxer-command-loop-event-handlers-queue* nil
  "A list of functions to funcall at the bottom of boxer-command-loop.")

(defvar *typeahead-during-eval* nil)

(defvar *double-click-pause-time* 0.4 "Time to wait for a possible second click")

(defvar *double-click-wander* 5
  "Number of pixels the mouse is allowed to shift between clicks")

(defun user-event-in-queue? ()
  (dolist (ev *boxer-eval-queue*)
    (when (or (mouse-event? ev) (key-event? ev)) (return T))))

(defun peek-next-key-or-mouse-event ()
  (loop (let ((ev (car *boxer-eval-queue*)))
          (cond ((null ev) (return nil))
                ((mouse-event? ev) (return ev))
                ((key-event?   ev) (return ev))
                (t (pop *boxer-eval-queue*))))))

;;; Mouse handling, mostly copied from clx
(defstruct (mouse-event (:conc-name mouse-event-)
      (:predicate mouse-event?))
  ;; these are required slots that handle-boxer-input uses
  (type :mouse-click) ;; other option is :mouse-hold
  (window *boxer-pane*)
  (x-pos 0)
  (y-pos 0)
  (click 0) ;; which button is down
  (bits 0)  ;; which shift bits are down
  ;; these are (+++ not) used in click processing
  (last-time-stamp 0)
  (number-of-clicks 1))

;; pause and wait for another possible click
(defun maybe-unify-mouse-click (click)
  (let ((button (mouse-event-click click))
        (bits   (mouse-event-bits  click))
        (x-pos  (mouse-event-x-pos click))
        (y-pos  (mouse-event-y-pos click))
        ;(time  (mouse-event-last-time-stamp click))
        )
    #-win32 (declare (ignore button))
    (mp::process-wait-with-timeout "Double Click Wait" *double-click-pause-time*
                                   #'(lambda () (user-event-in-queue?)))
    (let ((new-input (peek-next-key-or-mouse-event)))
      (cond ((mouse-event? new-input)
             (cond ((and #+win32 (= (mod button 3) (mod (mouse-event-click new-input) 3))
                         ;; only need to button match for PC (3) button mouse
                         (= bits (mouse-event-bits new-input))
                         (boxer::<& (boxer::abs& (boxer::-& x-pos (mouse-event-x-pos
                                                                   new-input)))
                                    *double-click-wander*)
                         (boxer::<& (boxer::abs& (boxer::-& y-pos (mouse-event-y-pos
                                                                   new-input)))
                                    *double-click-wander*))
                    ;; looks like a double click
                    (cond ((> (mouse-event-number-of-clicks new-input) 1)
                           (handle-boxer-input (pop *boxer-eval-queue*)))
                          (t ;; looks like the event system recorded it as
                             ;; a pair of single clicks, throw out the second
                             ;; one and bash fields in the 1st one
                             (pop *boxer-eval-queue*)
                             (setf (mouse-event-click click)
                                   (+ (mouse-event-click click) 3)
                                   ;; if we allow wandering, should we use the pos
                                   ;; of the 1st or 2nd click
                                   (mouse-event-last-time-stamp click)
                                   (mouse-event-last-time-stamp new-input))
                             (incf (mouse-event-number-of-clicks click))
                             (handle-boxer-input click))))
                   (t (handle-boxer-input click))))
            (t (handle-boxer-input click))))))

(defun boxer-command-loop-internal ()
;  (setf (ccl::window-process *boxer-frame*) boxer::*boxer-process*)
  (flush-input)
  (loop
    (catch 'boxer::boxer-editor-top-level
      (let ((input (pop *boxer-eval-queue*)))
        (cond ((null input)
               (unless just-redisplayed?
                 (boxer::repaint-with-cursor-relocation) (setq just-redisplayed? t))
               (mp::process-allow-scheduling)
               (when (no-more-input?)
                 (boxer-idle-function)
                 ;; be a good multi-processing citizen
                 ;; NOTE: when the idle function is fixed to actually document
                 ;; the mouse, we will need to change this to
                 ;; mp::process-allow-scheduling so it can run continously...
                 (mp::process-wait "Boxer Input"
                                   #'(lambda ()
                                       (not (null (car *boxer-eval-queue*)))))))
              ((system:gesture-spec-p input)
               ;; We are adding this gesture condition in addition to the key-event? because at some point
               ;; during a lispworks major version change, the ability to encode the modifier keys as part of
               ;; the reader char seems to have gone away.  By adding an option to push an entire gesture-spec
               ;; to the *boxer-eval-queue* we can just manually pick out the char-code and input bits.
               (let* ((data (sys::gesture-spec-data input))
                      (charcode (input-gesture->char-code input))
                      (charbits (convert-gesture-spec-modifier input)))
                     (handle-boxer-input charcode charbits)
                     (setq just-redisplayed? nil)))
              ((key-event? input)
               (handle-boxer-input (input-code input) (input-bits input))
               (setq just-redisplayed? nil))
              ((mouse-event? input)
               ;; be sure to call redisplay BEFORE the
               ;; processing of any mouse actions to
               ;; insure that we have an up to date view
               ;; of the editor
               ;;
               ;; also check for double click by pausing and looking for a
               ;; double click event
               (when (null just-redisplayed?) (boxer::repaint))
               (maybe-unify-mouse-click input)
               (setq just-redisplayed? nil))
              ((and (symbolp input) (not (null (symbol-function input))))
               (when (null just-redisplayed?) (boxer::repaint-with-cursor-relocation))
               (funcall input)
               (setq just-redisplayed? nil))
              ((and (consp input)
                    (or (functionp (car input))
                        (and (symbolp (car input))
                             (not (null (symbol-function (car input)))))))
               (apply (car input) (cdr input)))
              ((not (null boxer::*boxer-system-hacker*))
               (error "Unknown object, ~A, in event queue" input)))))))

(defun beep () (capi::beep-pane))

(defun click-sound () (capi::beep-pane))

(defvar *boxer-eval-queue* nil)

(defun queue-event (event)
 ; (when (characterp event) (setq *dribble* (nconc (list event) *dribble*)))
  (setq *boxer-eval-queue* (nconc *boxer-eval-queue* (list event))))

(defvar *double-click-wait-interval* .3
  "Number of seconds to wait for another (possible) mouse click")

(defvar *literal-input?* nil)
(defvar *literal-input*  nil)

(defmacro boxer-system-error-restart (&body body)
  (let ((warned-about-error-already-gensym (gensym)))
    `(let ((,warned-about-error-already-gensym nil))
       (restart-bind
  ((BOXER-CONTINUE
    #'(lambda () (throw 'system-error-restart-loop nil))
    :report-function
    #'(lambda (stream)
        (unless (or ,warned-about-error-already-gensym
        boxer::*boxer-system-hacker*
        boxer::*inside-lisp-breakpoint-p*)
    (beep) (beep) (beep)
    ;; this mechanism is a crock.
    (setq ,warned-about-error-already-gensym t))
        (format stream "--> Return to Boxer <--")))
   (BOXER-TOP-LEVEL
    #'(lambda () (boxer::com-goto-top-level)
             (throw 'system-error-restart-loop nil))
    :report-function
    #'(lambda (stream)
        (format stream "--> GOTO Top Level then return to Boxer <--"))))
  (handler-bind
   ((error
     #'(lambda (c)
         (cond ((or ,warned-about-error-already-gensym
        (not *automagic-lisp-error-handling*))
          (invoke-debugger c))
         (t
          (dotimes (i 3) (beep))
          ;(format t "~%Lisp Error:~A" c)
                      (when *report-crash* (write-crash-report))
          (boxer::boxer-editor-error "Lisp Error:~A" c)
          (invoke-restart 'BOXER-CONTINUE))))))
    (catch 'system-error-restart-loop
      . ,body)
    (setq ,warned-about-error-already-gensym nil))))))

(defmacro boxer-system-error-restart-loop (&body body)
  (let ((warned-about-error-already-gensym (gensym)))
    `(let ((,warned-about-error-already-gensym nil))
       (restart-bind
   ((BOXER-CONTINUE
     #'(lambda () (throw 'system-error-restart-loop nil))
     :report-function
     #'(lambda (stream)
         (unless (or ,warned-about-error-already-gensym
         boxer::*boxer-system-hacker*
         boxer::*inside-lisp-breakpoint-p*)
     (beep) (beep) (beep)
     ;; this mechanism is a crock.
     (setq ,warned-about-error-already-gensym t))
         (format stream "--> Return to Boxer <--")))
    (BOXER-TOP-LEVEL
     #'(lambda () (boxer::com-goto-top-level)
              (throw 'system-error-restart-loop nil))
     :report-function
     #'(lambda (stream)
         (format stream "--> GOTO Top Level then return to Boxer <--")))
          (ABORT
           #'(lambda () (boxer::com-abort))
           :report-function
           #'(lambda (stream) (format stream "Aborted"))))
   (handler-bind
     ((error
       #'(lambda (c)
           (cond ((or ,warned-about-error-already-gensym
          (not *automagic-lisp-error-handling*))
            (invoke-debugger c))
           (t
            (dotimes (i 3) (beep))
            ;(format t "~%Lisp Error:~A" c)
                        (when *report-crash* (write-crash-report))
            (boxer::boxer-editor-error "Lisp Error:~A" c)
            (invoke-restart 'BOXER-CONTINUE))))))
     (loop (catch 'system-error-restart-loop
                   (progn ,@body)
                   (flush-input)
             (setq ,warned-about-error-already-gensym nil))))))))

;; a hook for other stuff, on the mac, this ensures heap size
(defun boxer-idle-function ()
  #+mcl (ensure-macheap)
  )

(defun handle-event-internal (event &optional bits)
  (boxer-system-error-restart
;    (boxer-editor-bindings nil
    ;  Wrong place, this needs to be wrapped around the top level loop
      (catch 'boxer::boxer-editor-top-level
        (handle-boxer-input event bits)
        (setq just-redisplayed? nil)
        ;; if there is no more input, then redisplay
        (when (no-more-input?)
          (boxer::repaint)
          (setq just-redisplayed? t)
          (boxer-idle-function)))))

(defvar *suppress-event-queueing?* nil)

;; input handlers
(defvar *event-id* 0)

(defun event-id () *event-id*)
(defun next-event-id () (incf *event-id*))

;; these are interface stubs, the main work is done after
;; Single Clicks...
;; the only work done here is to setup *mouse-down-p* for use by MOUSE-BUTTONS

(defvar *shift-clicks-emulates-multi-buttons* t
  "Mac Emulation, left mouse simulates mac mouse")

(defconstant *click-1-byte-selector* (byte 1 0))
(defconstant *click-2-byte-selector* (byte 1 1))
(defconstant *click-3-byte-selector* (byte 1 2))

(defun boxer-click-1-handler (w x y)
  (declare (ignore w))
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 1))
        (t (setq *mouse-down-p* (dpb 1 *click-1-byte-selector* *mouse-down-p*))))
  (boxer-click-handler x y 0))

(defun boxer-click-2-handler (w x y)
  (declare (ignore w))
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 2))
        (t (setq *mouse-down-p* (dpb 1 *click-2-byte-selector* *mouse-down-p*))))
  (boxer-click-handler x y 1))

(defun boxer-click-3-handler (w x y)
  (declare (ignore w))
  (cond ((null *mouse-down-p*) (setq *mouse-down-p* 4))
        (t (setq *mouse-down-p* (dpb 1 *click-3-byte-selector* *mouse-down-p*))))
  (boxer-click-handler x y 2))

;; shifted singled clicks...

(defun boxer-c-click-1-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 1) (boxer-click-handler x y 0 nil 1))

(defun boxer-c-click-2-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 2) (boxer-click-handler x y 1 nil 1))

(defun boxer-c-click-3-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 4) (boxer-click-handler x y 2 nil 1))

(defun boxer-a-click-1-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 1) (boxer-click-handler x y 0 nil 2))

(defun boxer-a-click-2-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 2) (boxer-click-handler x y 1 nil 2))

(defun boxer-a-click-3-handler (w x y)
  (declare (ignore w))
  (setq *mouse-down-p* 4) (boxer-click-handler x y 2 nil 2))

(defun boxer-c-a-click-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 nil 3))
(defun boxer-c-a-click-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 nil 3))
(defun boxer-c-a-click-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 nil 3))


;; double clicks
(defun boxer-dclick-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 t))

(defun boxer-dclick-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 t))
(defun boxer-dclick-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 t))

;; shifted double clicks
(defun boxer-c-dclick-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 t 1))
(defun boxer-c-dclick-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 t 1))
(defun boxer-c-dclick-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 t 1))

(defun boxer-a-dclick-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 t 2))
(defun boxer-a-dclick-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 t 2))
(defun boxer-a-dclick-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 t 2))

(defun boxer-c-a-dclick-1-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 1) (boxer-click-handler x y 0 t 3))
(defun boxer-c-a-dclick-2-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 2) (boxer-click-handler x y 1 t 3))
(defun boxer-c-a-dclick-3-handler (w x y)
  (declare (ignore w)) (setq *mouse-down-p* 4) (boxer-click-handler x y 2 t 3))


;; mouse motion
(defvar *track-mouse-x* 0)
(defvar *track-mouse-y* 0)
(defvar *mouse-down-p* nil)

;; this is called by the :motion input type
(defun boxer-track-and-doc-mouse-handler (w x y)
  (declare (ignore w))
  (next-event-id)
  (setq *track-mouse-x* x *track-mouse-y* y)
  (document-mouse))

;; tracking when a button is down...
(defun boxer-track-mouse-handler (w x y)
  (declare (ignore w))
  (setq *track-mouse-x* x *track-mouse-y* y))

(defun boxer-pane-mouse-position ()
  ;; must allow track mouse handler in the interface(boxer) process to run
  (mp::process-allow-scheduling)
  (values *track-mouse-x* *track-mouse-y*))

(defun boxer-pane-mouse-x ()  (mp::process-allow-scheduling) *track-mouse-x*)
(defun boxer-pane-mouse-y ()  (mp::process-allow-scheduling) *track-mouse-y*)

;; this is called by the (:button :release) input type
(defun boxer-mouse-release-1-handler (w x y)
  (declare (ignore w x y))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 1) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-1-byte-selector* *mouse-down-p*)))))

(defun boxer-mouse-release-2-handler (w x y)
  (declare (ignore w x y))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 2) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-2-byte-selector* *mouse-down-p*)))))

(defun boxer-mouse-release-3-handler (w x y)
  (declare (ignore w x y))
  (cond ((null *mouse-down-p*))
        ((box::=& *mouse-down-p* 4) (setq *mouse-down-p* nil))
        (t (setq *mouse-down-p* (dpb 0 *click-3-byte-selector* *mouse-down-p*)))))

(defun boxer-pane-mouse-down? ()
  (flush-input)
  ;; flush any pending input because if we are checking the state of the mouse
  ;; buttons, we don't want any mouse down state to appear as a click
  *mouse-down-p*)

;; VK_SHIFT   = #x10
;; VK_CONTROL = #x11
;; VK_MENU    = #x12  the ALT key
;; VK_CAPS    = #x14 CapsLock
#+win32
(fli:define-foreign-function (%get-key-state "GetAsyncKeyState") ((key :integer))
  :language :c
  :calling-convention :stdcall
  :result-type (:unsigned :short)
  :module "user32.dll")

#+mac
(defun %get-key-state (key-code) (declare (ignore key-code)) 0)

(defun key-down? (virtual-key-code)
  (not (box::zerop& (boxer::ldb& (byte 1 15) (%get-key-state virtual-key-code)))))

(defun shift-key?    () (key-down? #x10))
(defun control-key?  () (key-down? #x11))
(defun alt-key?      () (key-down? #x12))
(defun capslock-key? () (key-down? #x14))

;; synonym usd in com-mouse-define-region
(defun shift-key-pressed? () (key-down? #x10))


;; the central mouse click handling function
;; this may need to encode further (3 button mouse on a mac)
(defun boxer-click-handler (x y button &optional double? (bits 0))
  ;; 1st set the tracking vars...
  (setq *track-mouse-x* x *track-mouse-y* y)
  (next-event-id)
  ;; reset any popup docs
  (undocument-mouse)
  ;; queue an eventp
  #+mac
  (when (not (zerop button)) ;; encode multibutton  mice on mac as shifted clicks
    (setq button 0 bits (min 3 (+ bits 1))))
  (let ((me (make-mouse-event :x-pos x :y-pos y :bits bits
                              :click  (+ button (if double? #+mac 1 #+win32 3 0))
                              :number-of-clicks (if double? 2 1)
                              :last-time-stamp (get-internal-real-time)
                              :type :mouse-hold))) ; or  :mouse-click
    (queue-event me)
    nil))

;;; Mouse tracking

;; stub for now
(defmacro with-mouse-cursor ((cursor) &body body)
  cursor
  `(progn . ,body))

(defmacro with-mouse-tracking (((original-x-variable original-x-value)
        (original-y-variable original-y-value)
        &key
                                event-skip timeout action
                                (body-function-name (gensym)))
             &body body)
  (declare (ignore event-skip timeout))
  `(let ((,original-x-variable ,original-x-value)
         (,original-y-variable ,original-y-value)
         (moved-p nil))
     (flet ((,body-function-name () . ,body))
       (with-mouse-cursor (,action)
         (do ((last-mouse-x -1) (last-mouse-y -1))
             ((if *dribble-playback*
                  (progn (update-dribble-mouse-state)
                         (box::zerop& (dribble-mouse-state-buttons)))
                  (not (boxer-pane-mouse-down?)))
              ;; record a button up state so the loop can terminate on
              ;; playback
              (record-mouse-state 0 ,original-x-variable ,original-y-variable)
              (values ,original-x-variable ,original-y-variable moved-p))
           (cond ((not (null *dribble-playback*))
                  (let ((dx (dribble-mouse-state-x)) (dy (dribble-mouse-state-y)))
                    (setq ,original-x-variable dx ,original-y-variable dy)))
                 (t
                  (multiple-value-setq  (,original-x-variable ,original-y-variable)
                      (boxer-pane-mouse-position))
                  (unless (and (= ,original-x-variable last-mouse-x)
                               (= ,original-y-variable last-mouse-y))
                    (record-mouse-state 2 ; anything non zero will do
                                        ,original-x-variable ,original-y-variable))))
           (unless moved-p
             (unless (and (= ,original-x-variable ,original-x-value)
                          (= ,original-y-variable ,original-y-value))
               (setq moved-p t)))
           (unless (and (= ,original-x-variable last-mouse-x)
                        (= ,original-y-variable last-mouse-y))
             (setq last-mouse-x ,original-x-variable last-mouse-y ,original-y-variable)
             (,body-function-name)))))))

(defun mouse-window-coords (&key (wait-action nil) (relative-to *boxer-pane*))
  (declare (ignore relative-to))
  (cond ((not (null *dribble-playback*))
         (update-dribble-mouse-state)
         (values (dribble-mouse-state-x) (dribble-mouse-state-y)))
        (t
         (case wait-action
           (nil) ; don't wait at all
           ((:down :button-press)
            ;; check for the mouse to be up before checking for the mouse to
            ;; be down.  If we dont do this, then consecutive calls to
            ;; xxx-on-click will just return immediately after the 1st click
            ;; without waiting for the click
            (with-mouse-cursor (:retarget)
              (mp::process-wait "Mouse Wait"
                                #'(lambda ()(not (boxer-pane-mouse-down?))))
              (mp::process-wait "Mouse Wait"
                                #'(lambda ()(boxer-pane-mouse-down?)))
              ;; there should be a mouse event in the queue now so flush it
              (flush-input)))
           (:up
            (with-mouse-cursor (:retarget)
              (mp::process-wait "Mouse Wait"
                                #'(lambda ()(not (boxer-pane-mouse-down?)))))))
         (multiple-value-bind (mx my)
             (boxer-pane-mouse-position)
           (record-mouse-state 0 mx my)
           (values mx my)))))

(defun abort-event? (char)
  (and (characterp char)
       (or ;; sgithens TODO (char= char #\control-\g)
           (char= char #\escape))))
           ;; sgithens TODO (char= char #\control-\.))))

(defun abort-gesture? (g)
  (and (typep g 'sys::gesture-spec)
       (let ((code (sys::gesture-spec-data g)))
         (and (numberp code)
              (or (= code #.(char-code #\Escape))
                  (and (= (sys::gesture-spec-modifiers g) 2) ; note, raw gspec modifiers
                       (or (= code #.(char-code #\G))
                           (= code #.(char-code #\g))
                           (= code #.(char-code #\.)))))))))

;;for debugging
(defvar *saved-keys* nil)
(defvar *save-key-length* 40)

(defun save-key (char)
  (if (> (length *saved-keys*) *save-key-length*)
      (setq *saved-keys* (nconc (cdr *saved-keys*) (list char)))
    (setq *saved-keys* (nconc *saved-keys* (list char)))))

;; the main key handler

(defun gesture-spec-handler (w x y gesture)
  (declare (ignore w x y))
  ;; reset any popup docs...
  (next-event-id)
  (undocument-mouse)
  (save-key gesture)
  (cond ((abort-gesture? gesture)
         (if (or boxer::*evaluation-in-progress?*
                 boxer-eval::*enable-interrupt-polling-in-editor*)
             (boxer-interrupt)
           (queue-event gesture)))
        (t (queue-event gesture)))
  )

(defun get-boxer-input (&optional (window *boxer-pane*))
  (declare (ignore window))
  (let ((*literal-input?* t))
    (mp::process-wait "Input" #'(lambda () (not (null *boxer-eval-queue*)))))
  (loop (let ((ev (pop *boxer-eval-queue*)))
          (when (or (key-event? ev) (mouse-event? ev))
            (return ev)))))

(defun get-character-input (window &key (plain-char-wanted? nil))
  (let ((input (get-boxer-input)))
    (cond ((key-event? input)
           (if (and plain-char-wanted?
                    (or (not (zerop (input-bits input)))
                        #+win32
                        (member (code-char (input-code input))
                                *pc-function-key-chars*)
                        #+cocoa
                        (member (input-code input)
                                *lwm-system-char-events*)))
             (progn
               (beep)
               (status-line-display 'boxer::boxer-editor-error
                                           "Please use only alphanumeric keys")
               (get-character-input window :plain-char-wanted? t))
             (values
              (code-char (input-code input))
              (input-bits input))))
          ((and (eq input 'boxer::com-abort)
                (not (null boxer::*input-throw-tag*)))
           (throw boxer::*input-throw-tag* t))
          (t
           ;; prompt for correct input
           (beep)
           (status-line-display 'boxer::boxer-editor-error
                                       "Please press a key instead")
           (get-character-input window
                                :plain-char-wanted? plain-char-wanted?)))))


;; unused, see handlers in top level interface def
(defun boxer-abort-handler (w x y)
  (declare (ignore w x y))
  (format t "~&ABORT !!"))

;; error
(defvar *automagic-lisp-error-handling* nil)



;; mouse state
;;

(defun mouse-button-state ()
  (if *dribble-playback*
      (progn (update-dribble-mouse-state)
             (dribble-mouse-state-buttons))
      (let* ((raw-state (boxer-pane-mouse-down?))
             (answer (cond ((null raw-state) 0)
                           ;; encode shifts for mac compatibility
                           (t
                            (case raw-state
                              (1 ; left, main, mac compat button
                               (cond ((and (alt-key?) (control-key?)) 5)
                                     ((alt-key?) 1)
                                     ((control-key?) 4)
                                     (t 2)))
                              (2
                               (cond ((and (alt-key?) (control-key?)) 13)
                                     ((alt-key?) 9)
                                     ((control-key?) 12)
                                     (t 10)))
                              (4
                               (cond ((and (alt-key?) (control-key?)) 21)
                                     ((alt-key?) 17)
                                     ((control-key?) 20)
                                     (t 18))))))))
        (record-mouse-state answer 0 0)
        answer)))

;; mouse state prims
(defun mouse-down? ()   (not (box::zerop& (mouse-button-state))))
(defun mouse-left? ()   (box::<& 0 (mouse-button-state) 8))
(defun mouse-middle? () (box::<& 8 (mouse-button-state) 16))
(defun mouse-right? ()  (box::<& 16 (mouse-button-state) 24))

;; stub, for now
(defun warp-pointer (window x y)
  (declare (ignore window x y))
  nil)

(defmacro with-mouse-tracking-inside (((original-x-variable original-x-value)
               (original-y-variable original-y-value)
              min-x min-y
              max-x max-y &rest keys)
              &body body)
  `(with-mouse-tracking ((,original-x-variable ,original-x-value)
                         (,original-y-variable ,original-y-value) ,@keys)
     ;; if the mouse has strayed,
     ;; then send it back
     (cond
       ((box::<& ,original-x-variable ,min-x)
  (cond ((box::<& ,original-y-variable ,min-y)
         (warp-pointer *boxer-pane* ,min-x ,min-y))
        ((box::>& ,original-y-variable ,max-y)
         (warp-pointer *boxer-pane* ,min-x ,max-y))
        (t
         (warp-pointer *boxer-pane* ,min-x ,original-y-variable))))
       ((box::>& ,original-x-variable ,max-x)
  (cond ((box::<& ,original-y-variable ,min-y)
         (warp-pointer *boxer-pane* ,max-x ,min-y))
        ((box::>& ,original-y-variable ,max-y)
         (warp-pointer *boxer-pane* ,max-x ,max-y))
        (t
         (warp-pointer *boxer-pane* ,max-x ,original-y-variable))))
       ((box::<& ,original-y-variable ,min-y)
  (warp-pointer *boxer-pane* ,original-x-variable ,min-y))
       ((box::>& ,original-y-variable ,max-y)
  (warp-pointer *boxer-pane* ,original-x-variable ,max-y))
       (t (progn . ,body)))))



;;; should go into opengl-utils.lisp if it ends up working....
(defun ogl-init (width height)
  (opengl:gl-matrix-mode opengl:*gl-projection*)
  (opengl:gl-load-identity)
  ;; orthographic projection, 0,0 = top,left
  ;; Note:GL-Ortho wants double-floats as args (and insists on the mac)
  (opengl:gl-ortho (coerce 0.0 'double-float)            (coerce (float width) 'double-float)
            (coerce (float height) 'double-float) (coerce 0.0 'double-float)
            (coerce -1.0 'double-float)           (coerce 1.0 'double-float)))



;; ?? is this called as a result of a display ?
;; might have to funcall through a symbol which changes
;; at the end of the graphics variable bootstrapping process
;; redisplay
;; x y wid hei define the invalidated region

(defvar *expose-window-handler-function* 'bootstrap-expose-window-function)

;; we use this to supress full redisplays around file operations and eval
;; there are 2? possible ways that redisplay can be called asynchronously, via the
;; expose window handler or the resize window handler
(defvar *suppress-expose-handler* nil)
(defvar *suppressed-actions* nil)

(defmacro with-suppressed-exposure-handling (&body body)
  `(unwind-protect
       (progn (setq *suppress-expose-handler* t)
         . ,body)
     (setq *suppress-expose-handler* nil
           *suppressed-actions* nil)))

(defun boxer-expose-window-handler (pane x y wid hei)
  (declare (ignore x y))
  (cond ((not (null *display-bootstrapping-no-boxes-yet*))
         ;(rendering-on (pane) (ogl-init wid hei))
         )
        (t
         (opengl:rendering-on (pane) (ogl-init wid hei))  ;(ogl-reshape wid hei)
         (redraw-status-line)
         (boxer::force-repaint))))

(defun bootstrap-expose-window-function (wid hei)
  (declare (ignore wid hei))
  ;; a stub
  nil)

(defun expose-window-function (wid hei)
  (declare (ignore wid hei))
;  (unless *suppress-expose-handler*
    (redraw-status-line)
    (boxer::force-repaint))
;)

;; careful, this called during startup before the outermost screen box
;; is created (by the 1st call to redisplay)
(defun resize-handler (window x y width height)
  (declare (ignore x y))
  (when (eq window *boxer-pane*)
    (cond ((not (null *display-bootstrapping-no-boxes-yet*))
           ;(rendering-on (*boxer-pane*) (gl-viewport 0 0 width height))
           )
          ((null *suppress-expose-handler*)
           (resize-handler-utility width height)
           (unless (null (Outermost-screen-box)) (boxer::force-repaint)))
          (t ; something is running (probably eval)
           (unless (member 'resize-handler-utility *suppressed-actions*)
             (push 'resize-handler-utility *suppressed-actions*))))))

(defun resize-handler-utility (&optional width height)
  (opengl:rendering-on (*boxer-pane*)
    (if (null width)
        (multiple-value-bind (ww wh)
            (window-inside-size *boxer-pane*)
          (ogl-reshape ww wh))
        (ogl-reshape width height)))
  ;; fill up the *boxer-pane* width/height caches
  ;; reset the outermost screen box
  (let ((osb (outermost-screen-box)))
    (unless (null osb)
      (multiple-value-bind (obwid obhei)
          (box::outermost-screen-box-size *boxer-pane*)
        (unless (and (= obwid (box::screen-obj-wid osb))
                     (= obhei (box::screen-obj-hei osb)))
          (box::set-fixed-size osb obwid obhei))))))

;; ****stubs

;; on the mac, this checks to OS event queue to look for pending unhandled events
;; which might not
(defun no-more-input? () (not (peek-next-key-or-mouse-event)))
(defun valid-input? () (not (no-more-input?)))

(defun flush-input () (setq *boxer-eval-queue* nil))

(defvar *noisy-abort-key-chars* nil)

(defvar *interrupt-flag* nil)

(defun boxer-interrupt () (setq *interrupt-flag* t *boxer-eval-queue* nil))

;; this may need to force a process switch to give the input handlers a chance
;; to run
(defun keyboard-interrupt? (window)
  (declare (ignore window))
  (if *interrupt-flag*
       (progn (setq *interrupt-flag* nil) t)
       (values nil (valid-input?))))

;; ****more stubs

(defun set-mouse-cursor (cursor)
  "Changes the current style of the mouse cursor using a keyword. Currently supported keywords are:
    :retarget (crosshairs)"
  (set-mouse-cursor-internal cursor))

(defun set-mouse-cursor-internal (cursor)
  ;; These are all currently referenced at some point in the code from previous
  ;; eras. We'll need to revisit them with some UI design. - sgithens 2020-10-09
  (cond
    ((eq cursor :retarget) ; retarget is when we are choosing the box target for a new port
     (setf (capi:simple-pane-cursor *boxer-pane*) :crosshair))
    ((eq cursor :hotspot) nil) ; TODO
    ((eq cursor :type-tab) nil) ; TODO
    ((eq cursor :name-box) nil) ; TODO
    ((eq cursor :normal) nil) ; TODO
    ((eq cursor :expandspot) nil) ; TODO
    ((eq cursor :suitcase) nil) ; TODO
    (t nil)))

(defun reset-mouse-cursor ()
  "Sets the current mouse cursor back to the system default."
  (setf (capi:simple-pane-cursor *boxer-pane*) nil))

;;;; Blinkers, mostly copied from clx

;; if there ever is more than 1 window, this ought to become an alist
;; of windows and blinkers
;; and we should extend the blinker structure to point to the owning window...
(defvar *boxer-window-blinker-alist* nil)

(defun sheet-blinker-list (window)
  (let ((entry (box::fast-assq window *boxer-window-blinker-alist*)))
    (unless (null entry)
      (cdr entry))))

(defun %set-sheet-blinker-list (window new-list)
  (let ((entry (box::fast-assq window *boxer-window-blinker-alist*)))
     (if (null entry)
   (push new-list *boxer-window-blinker-alist*)
   (setf (cdr entry) new-list)))
  new-list)

(defsetf sheet-blinker-list %set-sheet-blinker-list)

(defstruct (blinker (:conc-name blinker-)
        (:constructor %make-blinker))
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (visibility nil)
  (window nil))

(defun make-blinker (window &rest plist)
  (let ((blinker (apply #'%make-blinker :window window plist))
  (entry (box::fast-assq window *boxer-window-blinker-alist*)))
    (if (null entry)
  (push (list window blinker) *boxer-window-blinker-alist*)
  (setf (cdr entry) (nconc (list blinker) (cdr entry))))
    (when (blinker-visibility blinker)
      (draw-blinker blinker))
    blinker))

;; Of course the nice thing to do would be to make this generic and
;; define blinkers with DEFCLASS but I'm worried about speed at
;;  the moment. Also there is a VERY limited number of blinker types

;; for now, enable alpha blending for regions...
;(gl-enable *gl-blend*)

(defun draw-blinker (blinker)
  (with-pen-color (*blinker-color*)
    (box::with-blending-on
      (box::draw-rectangle alu-seta
                           (blinker-width blinker) (blinker-height blinker)
                           (blinker-x blinker)     (blinker-y blinker)))))

;;;; Region Row Blinkers...
(defstruct (region-row-blinker (:include blinker)
             (:predicate region-row-blinker?)
             (:constructor %make-region-row-blinker))
  (uid nil))

(defun make-region-row-blinker (window &rest plist)
  (let ((blinker (apply #'%make-region-row-blinker :window window plist))
  (entry (box::fast-assq window *boxer-window-blinker-alist*)))
    (if (null entry)
  (push (list window blinker) *boxer-window-blinker-alist*)
  (setf (cdr entry) (nconc (list blinker) (cdr entry))))
    (when (blinker-visibility blinker)
      (draw-region-row-blinker blinker))
    blinker))

(defun draw-region-row-blinker (blinker)
  (box::draw-rectangle alu-xor
   (blinker-width blinker) (blinker-height blinker)
   (blinker-x blinker)     (blinker-y blinker)))


;;; these are used by others

;; In OpenGL, just draw, no need to erase as we are double buffering...

(defmacro with-open-blinker ((blinker) &body body)
  blinker
  `(progn . ,body))

;; this looks unused ?
(defmacro with-open-blinkers (blinker-list &body body)
  blinker-list
  `(progn . ,body))

(defun set-cursor-visibility (blinker new-vis)
  (setf (blinker-visibility blinker) new-vis))

;; This is a crock. depends too much on *point-blinker* being the correct
;; thing need to change the window representation so we can ask a window
;; which of its blinkers corresponds to the MAIN cursor.  We do this for
;; now cause the only window that need this is the *boxer-pane*

;; OpenGL note: no more with-open-blinker, just change the vars
(defun set-cursorpos (window x y)
  (declare (ignore window))
  (setf (blinker-x *point-blinker*) (round x)
        (blinker-y *point-blinker*) (round y)))

(defun set-cursor-size (cursor wid hei)
  (when (and (not (null boxer::*boxer-system-hacker*))
         (or (< wid 0) (< hei 0)))
      (cerror "Set Value to 0"
        "Blinker Width or Height is < 0"))
  (setf (blinker-width  cursor) (max (round wid) 0))
  (setf (blinker-height cursor) (max (round hei) 0)))

;;;
(defun window-system-dependent-redraw-status-line (string)
  (setf (capi::title-pane-text *name-pane*) string))


;;; following copied from boxwin-clx.

(defun redisplayable-window? (x)
  (member x *redisplayable-windows*))

(defun kill-redisplayable-window (window)
;  (Destroy-Window window)
  (setq *redisplayable-windows* (delete window *redisplayable-windows*))
  (let ((pair (assoc window *redisplayable-window-outermost-box-alist*)))
    (unless (null pair)
      (setq *redisplayable-window-outermost-box-alist*
      (delete pair *redisplayable-window-outermost-box-alist*)))))

(defun outermost-screen-box (&optional (window *boxer-pane*))
  (cdr (assoc window *redisplayable-window-outermost-box-alist*)))

(defun set-window-name (newname)
  (setf (capi::interface-title *boxer-frame*) "")
  ;; hack, seems to be some sort of caching issue which suppresses updates
  ;; under certain coditions...
  (setf (capi::interface-title *boxer-frame*) newname))

(defun set-outermost-screen-box-in-window (window new-outermost-screen-box)
  (let ((pair (assoc window *redisplayable-window-outermost-box-alist*)))
    (if (null pair)
  (push (cons window new-outermost-screen-box)
               *redisplayable-window-outermost-box-alist*)
  (setf (cdr pair) new-outermost-screen-box))))

;;; "About Boxer" window ?
(defun about-boxer-function ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  (system-version)))

;;; System clipboard

(defun paste-text ()
  (let ((string (capi::clipboard *boxer-pane* :string)))
    (unless (null string)
      (dotimes (i (length string))
        (let ((char (aref string i)))
          (if (member char '(#\Newline #\Return #\Linefeed))
              (boxer::insert-row boxer::*point*
                                 (boxer::make-initialized-row) :moving)
              (boxer::insert-cha boxer::*point* char :moving)))))))

(defun image-to-bitmap (image)
  (unless (null image)
    (let* ((wid (gp:image-width image)) (hei (gp:image-height image))
           (bim (make-offscreen-bitmap *boxer-pane* wid hei)))
      (boxer::copy-image-to-bitmap image bim wid hei)
      (values bim wid hei))))

(defun paste-pict (&optional (img (capi::clipboard *boxer-pane* :image)
                                  img-supplied-p))
  (multiple-value-bind (bm wid hei)
      (image-to-bitmap img)
    (unless (null bm)
      ;; memory leak ?
      (unless img-supplied-p (gp:free-image *boxer-pane* img))
      (let* ((gb (boxer::make-box '(())))
             (gs (boxer::make-graphics-sheet wid hei gb)))
        (setf (boxer::graphics-sheet-bit-array gs) bm)
        (setf (boxer::graphics-sheet-bit-array-dirty? gs) T)
        (setf (boxer::graphics-info gb) gs)
        (setf (boxer::display-style-graphics-mode?
               (boxer::display-style-list gb)) T)
        (boxer::insert-cha boxer::*point* gb :moving)))))

(defmethod paste ((self boxer-frame))
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


;;;;  Crash reporting

(defvar *report-crash* t)

(defun crash-reporting-filename () "~/boxerlogs.txt")

;; should add date/time too
(defun bug-report-header (stream)
  (let* ((system-string (system-version))
         (xtens (mapcar #'box::boxer-extension-pretty-name box::*boxer-extensions*)))
    (format stream "~A ~
                   ~%  with Extensions: ~A ~
                   ~%  in ~A ~A ~
                   ~%  running on ~A, a ~A ~
                   ~%  under ~A ~A ~%~%"
            system-string xtens
            (lisp-implementation-type) (lisp-implementation-version)
            (machine-instance) (machine-type)
            (software-type) (software-version))))

(defun write-crash-report-internal (&optional (stream *error-output*))
  (bug-report-header stream)
  (let ((*print-readably* nil)
        (*print-miser-width* 40)
        (*print-circle* t)
        (*print-pretty* t))
    (dbg::output-backtrace :verbose :stream stream)))

(defun write-crash-report ()
  (ignore-errors
    (with-open-file (stream (crash-reporting-filename) :direction ':output
                            :if-exists ':append :if-does-not-exist ':create)
      (let ((*terminal-io* stream)
            (*standard-output* stream)
            (*debug-io* stream))
        (write-crash-report-internal stream)))))
