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
           :string-case :char-case)
           )

(defpackage :boxer-eval
  (:use :common-lisp)
  (:use :boxer)
  (:nicknames :eval)
  (:export :LOOKUP-STATIC-VARIABLE-IN-BOX-ONLY
           :REMOVE-STATIC-VARIABLE
           :ADD-STATIC-VARIABLE-PAIR
           :LOOKUP-STATIC-VARIABLE-IN-BOX-ONLY
           :REMOVE-STATIC-VARIABLE
           :defboxer-primitive
           :*LEXICAL-VARIABLES-ROOT*)
  )

(defpackage :boxnet (:use :common-lisp :boxer-eval :boxer))

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

;; external symbols that we need to reference from BOXER
;; the symbols must be present before we export them
;; :: won't create the symbol.

(export '(bu::input bu::inputs bu::true bu::false bu::port-to bu::datafy)
        (find-package 'boxer-user))

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

(import '(boxer::once-only boxer::control-character-display-prefix
                           boxer::%drawing-window boxer::%drawing-array
                           boxer::%drawing-font-map
                           boxer::scale-x boxer::scale-y
                           boxer::x-out-of-bounds? boxer::y-out-of-bounds?
                           ;; font vars
                           boxer::%drawing-font boxer::%drawing-fit
                           boxer::%drawing-font-cha-wid
                           boxer::%drawing-font-cha-hei
                           boxer::%drawing-font-cha-wid-table
                           ;; inits
                           boxer::def-redisplay-initialization
                           boxer::run-redisplay-inits
                           ;; useful macros
                           boxer::defsubst)
  (find-package 'boxer-window))

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

(export '(boxer-eval::*lexical-variables-root* boxer-eval::add-static-variable-pair
                                               boxer-eval::evaluator-delete-self-action
                                               boxer-eval::lookup-static-variable-in-box-only
                                               boxer-eval::remove-static-variable
                                               boxer-eval::defboxer-key boxer-eval::funcall-boxer-key
                                               boxer-eval::set-exports
                                               boxer-eval::boxer-boolean
                                               boxer-eval::list-rest boxer-eval::numberize
                                               boxer-eval::set-and-save-state-variables
                                               boxer-eval::recursive-funcall-invoke
                                               boxer-eval::restore-state-variables
                                               boxer-eval::boxer-symeval)
        (find-package :boxer-eval))

(import '(boxer::defsubst boxer::virtual-copy-error-clause
                          boxer::with-collection boxer::collect
                          ;; instance variables and generic function names
                          boxer::static-variable-cache boxer::cached-code
                          boxer::static-variables-alist
                          ;; type checking
                          boxer::fast-eval-doit-box? boxer::fast-eval-data-box?
                          boxer::fast-eval-port-box? boxer::fast-eval-sprite-box?
                          boxer::fast-editor-doit-box? boxer::fast-editor-data-box?
                          boxer::fast-editor-sprite-box? boxer::fast-editor-port-box?
                          boxer::doit-box? boxer::data-box?
                          boxer::sprite-box? boxer::port-box?
                          ;; other predicates
                          boxer::named-box?
                          ;; PCL stuff
                          ;;   #+clos clos::slot-value #+clos clos::defmethod
                          ;;   #+clos clos::bcm-class-and-instantiable-superiors-symbol
                          ;; Boxer/CL fixup
                          boxer::fast-assq boxer::fast-delq
                          boxer::fast-del-if boxer::fast-memq
                          boxer::compiler-let)
  (find-package 'boxer-eval))

(import '(boxer::fix boxer::fixr boxer::symbol-format
                     boxer::+& boxer::-& boxer::*&
                     boxer::=& boxer::<& boxer::>&
                     boxer::<=& boxer::>=&
                     boxer::/=&
                     boxer::1+& boxer::1-&
                     boxer::incf& boxer::decf&
                     boxer::zerop& boxer::svref& boxer::dotimes& boxer::svlength)
  'boxer-eval)

#+lispworks
(shadow 'boxer-eval::debug (find-package :boxer-eval))

(import '(boxer-eval::defboxer-key boxer-eval::defboxer-primitive
                                   ; Until Lucid fixes the DEFSTRUCT lossage you have to be in the EVAL package
                                   ; when doing defrecursive-*-primitives.
                                   ;;        boxer-eval::defrecursive-funcall-primitive
                                   ;;	  boxer-eval::defrecursive-eval-primitive
                                   boxer-eval::set-exports)
  (find-package 'boxer))

;;; Set up package for the boxer network.

(import '(boxer::wait-with-timeout boxer::storage-chunk?
                                   boxer::*outermost-dumping-box* boxer::*initial-box*
                                   boxer::getprop boxer::removeprop boxer::read-only-box?
                                   boxer::dump-boxer-thing boxer::putprop boxer::load-box-on-login?
                                   boxer::superior-box boxer::box? boxer::fast-delq boxer::superior?
                                   boxer::link-target boxer::link-port boxer::link-type
                                   boxer::contained-links boxer::branch-links
                                   boxer::port-branch-link boxer::target-branch-link
                                   boxer::set-port-to-box boxer::plist
                                   boxer::inferior-link boxer::dump-list-preamble
                                   boxer::deffile-property-handler boxer::box-text-string box::make-vc
                                   boxer::find-lowest-common-superior-box
                                   boxer-eval::defboxer-primitive boxer-eval::primitive-signal-error
                                   boxer-eval::signal-error boxer-eval::true? boxer-eval::false?
                                   boxer::enter boxer::point-box boxer::box? boxer::virtual-copy
                                   boxer::*boxer-version-info* boxer::redraw-status-line
                                   boxer::box-text-string boxer::setup-editor boxer::*initial-box*
                                   boxer::make-box boxer::shrink boxer::box-or-port-target
                                   boxer::append-row boxer::make-row
                                   boxer::storage-chunk? boxer::display-style boxer::set-display-style
                                   ;;   #+(or lispworks mcl) boxer::block-compile-class
                                   boxer::modified boxer::name-string
                                   boxer::dump-plist-internal boxer::dump-plist-length
                                   boxer::ut-day boxer::ut-month boxer::ut-tz)
  'boxnet)

(export '(boxnet::fill-box-from-server boxnet::fill-box
                                       boxnet::storage-chunk-plist-half-length
                                       boxnet::dump-storage-chunk-plist-items
                                       boxnet::cross-file-port?
                                       boxnet::*dump-out-file-box-inferiors?*
                                       boxnet::in-bfs-environment?
                                       boxnet::no-inferiors-for-file?
                                       boxnet::dump-cross-file-port-reference
                                       boxnet::load-server-box-id
                                       boxnet::load-cross-file-contained-links
                                       boxnet::load-cross-file-port-branch-links
                                       boxnet::load-cross-file-target-branch-links
                                       boxnet::load-cross-file-target-ends
                                       boxnet::load-cross-file-link-id
                                       boxnet::no-cross-file-links?
                                       boxnet::cross-file-link-insert-self-action
                                       boxnet::cross-file-link-delete-self-action
                                       boxnet::cross-file-port-insert-self-action
                                       boxnet::cross-file-port-delete-self-action
                                       boxnet::cross-file-port-branch-links
                                       boxnet::articulate-target-branch
                                       boxnet::dump-box-url boxnet::load-url boxnet::url-string?
                                       boxnet::url-box? boxnet::read-internal-url)
        (find-package 'boxnet))

(use-package 'boxnet)

;; redefined in file-prims

;;; The shadowing in-line in MCL-PRELUDE no longer works, so do it here.
;; (shadow '(cl:defclass cl:defmethod cl:slot-value cl:with-slots))
;; (shadowing-import '(box::defclass box::defmethod box::slot-value box::with-slots)
                                    ;;    (find-package :boxnet))

