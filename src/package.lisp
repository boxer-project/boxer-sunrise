;;;;
;;;;     Boxer
;;;;     Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
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

(proclaim '(optimize (debug 3)))

#-lispworks
(defpackage :opengl (:use :common-lisp)
  (:export
    :*GL-AUX-BUFFERS*
    :*GL-AUX1*
    :*GL-BACK*
    :*GL-BLEND*
    :*GL-COLOR-BUFFER-BIT*
    :*GL-CURRENT-COLOR*
    :*GL-LINES*
    :*GL-LINE-LOOP*
    :*GL-LINE-SMOOTH-HINT*
    :*GL-LINE-SMOOTH*
    :*GL-LINE-STIPPLE*
    :*GL-LINE-STRIP*
    :*GL-LINE-WIDTH*
    :*GL-NICEST*
    :*GL-ONE-MINUS-SRC-ALPHA*
    :*GL-POLYGON*
    :*GL-POLYGON-SMOOTH*
    :*GL-PROJECTION*
    :*GL-SCISSOR-TEST*
    :*GL-SRC-ALPHA*
    :*gl-rgba*
    :free-gl-vector
    :gl-begin
    :gl-blend-func
    :gl-clear
    :gl-clear-color
    :gl-color4-fv
    :gl-disable
    :gl-draw-buffer
    :gl-enable
    :gl-end
    :gl-flush
    :gl-get-booleanv
    :gl-get-floatv
    :gl-get-integerv
    :gl-hint
    :gl-is-enabled
    :gl-line-width
    :gl-load-identity
    :gl-matrix-mode
    :gl-ortho
    :gl-point-size
    :gl-rectf
    :gl-scissor
    :gl-translatef
    :gl-vector-aref
    :gl-vertex2-f
    :gl-viewport
    :make-gl-vector
    :rendering-on
))

(defpackage :boxer-user
  (:use)
  (:nicknames :bu :boxer-users :pkg-bu-package :pkg-boxer-user-package)
  (:export :input :inputs :true :false :port-to :datafy)
  )

(defpackage :boxer
  (:use :common-lisp )
  (:nicknames :box)
  (:shadow :once-only)
  (:export :symbol-format :set-font-info
           :draw-line
           :*boxer-frame*
           :*current-opengl-font*
           :with-collection
           :collect
           :sheet-inside-width :sheet-inside-height
           :with-window-system-dependent-clipping :with-turtle-clipping
           :alu-andca :alu-seta :alu-xor :alu-and :alu-ior :alu-setz
           :port-to
           :getprop
           :make-box
           :removeprop
           :static-variables-alist
           :setup-editor
           :svref&
           :cross-file-link-insert-self-action
           :fast-assq
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
           :once-only
           :%drawing-window :%drawing-array
           :%drawing-height
           :%drawing-width
           :%drawing-half-height
           :%drawing-half-width
           :scale-x :scale-y
           :x-out-of-bounds? :y-out-of-bounds?
           :max-window-coord :min-window-coord

           ;; font vars
           :%drawing-font :%drawing-fit
           :%drawing-font-cha-wid
           :%drawing-font-cha-hei
           :%drawing-font-cha-ascent
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
           :setup-redisplay :setup-editor :not-null
           :redisplay :handle-boxer-input
           :deftype-checking-macros
           :*cursor-blinker-wid*
           :*cursor-blinker-min-hei*
           :window-system-specific-make-boxer
           :window-system-specific-start-boxer
           :status-line-display :redraw-status-line
           :force-redisplay
           :%local-clip-lef :%local-clip-top
           :%local-clip-rig :%local-clip-bot
           :%origin-x-offset :%origin-y-offset
           :input-code :input-bits
           ;; dribble symbols
           :*dribble-playback*
           :update-dribble-mouse-state
           :dribble-mouse-state-x
           :dribble-mouse-state-y
           :dribble-mouse-state-buttons
           :record-mouse-state

           :mouse-click-names :mouse-state-names :lookup-click-name
           :system-version
           )
           )

(defpackage :boxer-wrap
  (:use :common-lisp)
  (:use :boxer)
  (:export :draw-wrap-line))

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
  (:export :*foreground-color* :*background-color*
           :prepare-sheet
           :window-inside-size
           :window-pixel
           :%bitblt-to-screen :%bitblt-from-screen
           :%draw-point
           :%draw-rectangle :%erase-rectangle :%draw-line :%draw-poly
           :boxer-points->window-system-points :%draw-arc :%draw-filled-arc
           :set-outermost-screen-box-in-window
           :draw-cha :cha-wid :cha-hei
           :%draw-cha :%draw-string
           :string-wid :string-hei
           ;; bitmap functions
           :make-offscreen-bitmap :copy-offscreen-bitmap :free-offscreen-bitmap
           :offscreen-bitmap-width :offscreen-bitmap-height
           :offscreen-bitmap-depth :with-system-dependent-bitmap-drawing
           :with-drawing-port
           :offscreen-bitmap-image :set-offscreen-bitmap-image
           :offscreen-pixel :image-pixel

           :rebind-font-info
           :%make-color :color? :color= :with-pen-color :%set-pen-color
           :%set-pen-size :pixel-rgb-values
           :set-mouse-cursor :reset-mouse-cursor :with-mouse-cursor :beep
           :mouse-window-coords :mouse-button-state

           :*redisplayable-windows*
           :outermost-screen-box
           ;; useful variables...
           :*boxer-pane* :*name-pane* :*boxer-frame*
           :*point-blinker* :*mouse-blinker* :*sprite-blinker*
           ;; Window operations
           :set-outermost-screen-box-in-window
           :window-system-dependent-redraw-status-line :clear-window
           ;; input functions
           :get-character-input :get-boxer-input
           ;; cursor and blinker manipulation
           :with-mouse-tracking
           :with-mouse-tracking-inside;; these  bind...
           :mouse-x :mouse-y
           :mouse-button-state
           :mouse-window-coords
           :warp-pointer
           :make-blinker :draw-blinker
           :make-region-row-blinker :draw-region-row-blinker
           :set-cursorpos :set-cursor-size
           :set-cursor-visibility :set-window-name

           :key-event? :mouse-event?
           :mouse-event-window :mouse-event-type
           :mouse-event-x-pos :mouse-event-y-pos
           :mouse-event-click :mouse-event-bits
           ))

(defpackage :boxer-lw-capi
  (:use :common-lisp))

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

;; Inherit External Symbols from BW
(USE-PACKAGE 'BOXER-WINDOW)

(use-package 'boxnet)
