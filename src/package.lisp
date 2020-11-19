;;;;
;;;;     Boxer
;;;;     Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;     Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;     used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;     Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;     https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                          +-Data--+
;;;;                 This file is part of the | BOXER | system
;;;;                                          +-------+
;;;;
;;;;
;;;;  This file contains the package declarations and other symbol import/exports for Boxer.
;;;;

(defpackage :boxer-user
  (:use)
  (:nicknames :bu :boxer-users :pkg-bu-package :pkg-boxer-user-package)
  (:export :input :inputs :true :false :port-to :datafy)
  )

(defpackage :boxer
  (:use :common-lisp :boxer-user)
  (:nicknames :box)
  (:shadow :once-only)
  (:export :symbol-format
           :*boxer-frame*
           :with-collection
           :collect
           :getprop
           :make-box
           :removeprop
           :static-variables-alist
           :setup-editor
           :svref&
           :cross-file-link-insert-self-action
           :run-redisplay-inits
           :fast-assq
           :block-compile-class
           :deffile-property-handler
           :recompile-boxer :load-boxer :make-boxer :start-boxer :boxer
           :fix
           :fixr
           :+&  :fixnum-plus  :-&  :fixnum-minus  :*&  :fixnum-times
           :=&  :fixnum-=  :<&  :fixnum-<  :>&  :fixnum->  :<=&  :fixnum-<=
           :>=&  :fixnum->=  :1+&  :1-&  :zerop&  :max&  :min&  :svref& :incf& :decf&
           :float-plus :float-minus :float-times
           :ldb& :dpb& :logior& :logand& :logxor&
           :defsubst :dotimes&
           :barf :with-collection :collect :neq :sind :cosd
           :fast-delq :fast-memq :fast-assq
           :between :inclusive-between? :intern-keyword :symbol-format
           :string-case :char-case
           :once-only :control-character-display-prefix
           :%drawing-window :%drawing-array
           :%drawing-font-map
           :scale-x :scale-y
           :x-out-of-bounds? :y-out-of-bounds?
           ;; font vars
           :%drawing-font :%drawing-fit
           :%drawing-font-cha-wid
           :%drawing-font-cha-hei
           :%drawing-font-cha-wid-table
           ;; inits
           :def-redisplay-initialization
           :run-redisplay-inits
           ;; useful macros
           :defsubst

           :wait-with-timeout :storage-chunk?
           :*outermost-dumping-box* :*initial-box*
           :getprop :removeprop :read-only-box?
           :dump-boxer-thing :putprop :load-box-on-login?
           :superior-box :box? :fast-delq :superior?
           :link-target :link-port :link-type
           :contained-links :branch-links
           :port-branch-link :target-branch-link
           :set-port-to-box :plist
           :inferior-link :dump-list-preamble
           :deffile-property-handler :box-text-string :make-vc
           :find-lowest-common-
           :enter :point-box :box? :virtual-copy
           :*boxer-version-info* :redraw-status-line
           :box-text-string :setup-editor :*initial-box*
           :make-box :shrink :box-or-port-target
           :append-row :make-row
           :storage-chunk? :display-style :set-display-style
           ;;   #+(or lispworks mcl) :block-compile-class
           :modified :name-string
           :dump-plist-internal :dump-plist-length
           :ut-day :ut-month :ut-tz
           :defsubst :virtual-copy-error-clause
           :with-collection :collect
           ;; instance variables and generic function names
           :static-variable-cache :cached-code
           :static-variables-alist
           ;; type checking
           :fast-eval-doit-box? :fast-eval-data-box?
           :fast-eval-port-box? :fast-eval-sprite-box?
           :fast-editor-doit-box? :fast-editor-data-box?
           :fast-editor-sprite-box? :fast-editor-port-box?
           :doit-box? :data-box?
           :sprite-box? :port-box?
           ;; other predicates
           :named-box?
           ;; PCL stuff
           ;;   #+clos clos::slot-value #+clos clos::defmethod
           ;;   #+clos clos::bcm-class-and-instantiable-superiors-symbol
           ;; Boxer/CL fixup
           :fast-assq :fast-delq
           :fast-del-if :fast-memq
           :compiler-let
           :fix :fixr :symbol-format
           :+& :-& :*&
           :=& :<& :>&
           :<=& :>=&
           :/=&
           :1+& :1-&
           :incf& :decf&
           :zerop& :svref& :dotimes& :svlength
           )
           )

(defpackage :boxer-eval
  (:use :common-lisp)
  (:use :boxer)
  (:shadow :debug)
  (:nicknames :eval)
  (:export :LOOKUP-STATIC-VARIABLE-IN-BOX-ONLY
           :REMOVE-STATIC-VARIABLE
           :ADD-STATIC-VARIABLE-PAIR
           :LOOKUP-STATIC-VARIABLE-IN-BOX-ONLY
           :REMOVE-STATIC-VARIABLE
           :defboxer-primitive
           :*LEXICAL-VARIABLES-ROOT*
           :defboxer-primitive :primitive-signal-error
           :signal-error :true? :false?
           :*lexical-variables-root* :add-static-variable-pair
           :evaluator-delete-self-action
           :lookup-static-variable-in-box-only
           :remove-static-variable
           :defboxer-key :funcall-boxer-key
           :set-exports
           :boxer-boolean
           :list-rest :numberize
           :set-and-save-state-variables
           :recursive-funcall-invoke
           :restore-state-variables
           :boxer-symeval)
  )

(defpackage :boxnet
  (:use :common-lisp :boxer-eval :boxer)
  (:export :fill-box-from-server :fill-box
    :storage-chunk-plist-half-length
    :dump-storage-chunk-plist-items
    :cross-file-port?
    :*dump-out-file-box-inferiors?*
    :in-bfs-environment?
    :no-inferiors-for-file?
    :dump-cross-file-port-reference
    :load-server-box-id
    :load-cross-file-contained-links
    :load-cross-file-port-branch-links
    :load-cross-file-target-branch-links
    :load-cross-file-target-ends
    :load-cross-file-link-id
    :no-cross-file-links?
    :cross-file-link-insert-self-action
    :cross-file-link-delete-self-action
    :cross-file-port-insert-self-action
    :cross-file-port-delete-self-action
    :cross-file-port-branch-links
    :articulate-target-branch
    :dump-box-url :load-url :url-string?
    :url-box? :read-internal-url))

(defpackage :boxer-window
  (:use :common-lisp :boxer)
  (:nicknames :bw)
  (:export :*glyph-pane*
           :outermost-screen-box))

(in-package :boxer)



#+(and clos (not pcl))
(use-package 'clos)

;;;; Set up the packages that we will be using

;;; useful symbols from the boxer pkg (mostly from macros.lisp)
;;; that we want ALL other packages to see.  There are import
;;; statements below for specific symbols that should be seen
;;; in specific packages

(DEFMACRO DEFPROP (SYM VALUE INDICATOR)
          `(SETF (GET ',SYM ',INDICATOR) ',VALUE))

;; Some useful variables and functions for dealing with the BU package

(defvar pkg-boxer-user-package (find-package 'boxer-user))
(defvar pkg-bu-package (find-package 'boxer-user))




;;;; Setting Up the Window System Package
;;;
;;;  This relies on the result of (get-window-system-info) defined
;;;  and called in boxsys
;;;
;;;  All of this is done HERE to insure a consistent LOAD and COMPILE time
;;;  package Environment.  Putting all this info in the relevant files leads
;;;  to obscure package lossage.  Sigh.

;; Inherit External Symbols from BW
(USE-PACKAGE 'BOXER-WINDOW)
;; These symbols are the interface between the window system and BOXER
;; Look at the beginning of the DRAW.LISP file for more interface and some
;; definitions too.
;; Most of the Boxer Redisplay talks to a virtual bitmap.  These functions
;; are defined in the DRAW.LISP file.  The ones that actually talk to windows
;; are shown here. Mouse functions need to added here also...



;;; From the draw-low-xxx files

;;; What we need from BOXER

;;; shadow the symbol from SCL preferring the PCL one

;;; What BOXER uses
(export
 '(bw::alu-andca bw::alu-seta bw::alu-xor bw::alu-and bw::alu-ior bw::alu-setz
                 bw::*foreground-color* bw::*background-color*
                 bw::main-screen bw::sheet-screen-array bw::prepare-sheet
                 bw::sheet-font-map bw::make-pattern
                 bw::sheet-inside-left bw::sheet-inside-top
                 bw::sheet-inside-width bw::sheet-inside-height bw::window-inside-size
                 bw::window-depth bw::window-pixel
                 bw::%bitblt-in-screen bw::%bitblt-to-screen bw::%bitblt-from-screen
                 bw::%bitblt-tile-to-screen bw::%draw-point
                 bw::%draw-rectangle bw::%erase-rectangle bw::%draw-line bw::%draw-poly
                 bw::boxer-points->window-system-points bw::%draw-arc bw::%draw-filled-arc
                 bw::set-outermost-screen-box-in-window
                 bw::draw-cha bw::cha-wid bw::cha-hei
                 bw::%draw-cha bw::%draw-string
                 bw::string-wid bw::string-hei
                 bw::font-char-height
                 ;; bitmap functions
                 bw::make-offscreen-bitmap bw::copy-offscreen-bitmap bw::free-offscreen-bitmap
                 bw::offscreen-bitmap-width bw::offscreen-bitmap-height
                 bw::offscreen-bitmap-depth bw::with-system-dependent-bitmap-drawing
                 bw::with-drawing-port
                 bw::offscreen-bitmap-image bw::set-offscreen-bitmap-image
                 bw::offscreen-pixel bw::image-pixel
                 bw::with-window-system-dependent-clipping bw::with-turtle-clipping
                 bw::%draw-rectangle-on-offscreen-bitmap
                 bw::%draw-line-on-offscreen-bitmap
                 bw::draw-string-to-offscreen-bitmap
                 bw::rebind-font-info bw::set-font-info
                 bw::%make-color bw::color? bw::color= bw::with-pen-color bw::%set-pen-color
                 bw::%set-pen-size bw::pixel-rgb-values
                 bw::set-mouse-cursor bw::reset-mouse-cursor bw::with-mouse-cursor bw::beep
                 bw::mouse-window-coords bw::mouse-button-state
                 bw::deallocate-system-dependent-structures
                 bw::max-window-coord bw::min-window-coord)
 (find-package 'boxer-window))

;;; From the boxwin-xxx files

(export '(bw::*redisplayable-windows*
          bw::outermost-screen-box
          ;; useful variables...
          bw::*boxer-pane* bw::*name-pane* bw::*boxer-frame*
          bw::*point-blinker* bw::*mouse-blinker* bw::*sprite-blinker*
          bw::mini-boxer-command-loop
          ;; Window operations
          bw::set-outermost-screen-box-in-window bw::beep
          bw::window-system-dependent-redraw-status-line bw::clear-window
          ;; input functions
          bw::get-character-input bw::get-boxer-input
          bw::get-any-event;; not used by boxer but useful for debugging
          ;; cursor and blinker manipulation
          bw::*number-of-mouse-buttons*
          bw::with-mouse-tracking
          bw::with-mouse-tracking-inside;; these  bind...
          bw::mouse-x bw::mouse-y
          bw::mouse-button-state
          bw::mouse-window-coords
          bw::warp-pointer
          bw::make-blinker bw::draw-blinker
          bw::make-region-row-blinker bw::draw-region-row-blinker
          bw::with-open-blinker bw::with-open-blinkers
          bw::set-cursorpos bw::set-cursor-size
          bw::set-cursor-visibility bw::set-window-name)
        (find-package 'boxer-window))

(import '(boxer::setup-redisplay boxer::setup-editor boxer::not-null
                                 boxer::redisplay boxer::handle-boxer-input
                                 boxer::deftype-checking-macros
                                 boxer::*cursor-blinker-wid*
                                 boxer::*cursor-blinker-min-hei*
                                 boxer::window-system-specific-make-boxer
                                 boxer::window-system-specific-start-boxer
                                 #+X
                                 boxer::*default-keymap-translation-filename*
                                 boxer::status-line-display boxer::redraw-status-line
                                 boxer::force-redisplay
                                 ;;
                                 boxer::%local-clip-lef boxer::%local-clip-top
                                 boxer::%local-clip-rig boxer::%local-clip-bot
                                 boxer::%origin-x-offset boxer::%origin-y-offset
                                 ;;
                                 ;;  boxer::key-event? boxer::mouse-event?
                                 boxer::input-code boxer::input-bits
                                 ;; dribble symbols
                                 boxer::*dribble-playback*
                                 boxer::update-dribble-mouse-state
                                 boxer::dribble-mouse-state-x
                                 boxer::dribble-mouse-state-y
                                 boxer::dribble-mouse-state-buttons
                                 boxer::record-mouse-state)
  (find-package 'boxer-window))

;;; from the keydef-low-xxx.lisp files

(export '(bw::key-event? bw::mouse-event?
                         bw::key-or-button-event? bw::key-event-values
                         bw::mouse-event-window bw::mouse-event-type
                         bw::mouse-event-x-pos bw::mouse-event-y-pos
                         bw::mouse-event-click bw::mouse-event-bits)
        (find-package 'boxer-window))

(import '(bw::mouse-click-names bw::mouse-state-names bw::lookup-click-name)
  (find-package 'boxer))

;;; most of these are either redundant or obsolete.
(EXPORT '(BW::REGION-X BW::REGION-Y
                       BW::REGION-WID BW::REGION-HEI BW::REGION-VISIBILITY
                       ;; functions that let the windows do the walking
                       BW::BOXER-TOP-OF-STACK-GROUP-BINDINGS)
        (FIND-PACKAGE 'BOXER-WINDOW))


;;; Set up package for the evaluator.

;;; Set up package for the boxer network.


(use-package 'boxnet)

;; redefined in file-prims

;;; The shadowing in-line in MCL-PRELUDE no longer works, so do it here.
;; (shadow '(cl:defclass cl:defmethod cl:slot-value cl:with-slots))
;; (shadowing-import '(box::defclass box::defmethod box::slot-value box::with-slots)
                                    ;;    (find-package :boxnet))

