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

#+emscripten
(defpackage :log
  (:use)
  (:export :config :debug :info :error :warn))

(defpackage :boxer-user
  (:use)
  (:nicknames :bu :boxer-users :pkg-bu-package :pkg-boxer-user-package)
  (:export :input :inputs :true :false :port-to :datafy)
  )

(defpackage :boxer-draw-bridge
  (:use :common-lisp)
  (:export
           :%draw-line
           :%set-pen-color
           :%string-hei
           :%string-wid
           :%draw-rectangle
           :%make-boxer-gl-model
           :%refresh-gpu-model-matrix
           :%string-ascent
           :%draw-string
           :%cha-wid
           :%find-glyph
           :%draw-cha
           :%clear-window
           :%pixblt-from-screen
           ))

(defpackage :boxer
  (:use :common-lisp :boxer-draw-bridge)
  (:nicknames :box)
  (:shadow :once-only)
  (:export :box :plist-subclass
           *capogi-font-directory* *resources-dir* *shaders-dir*
           *suppress-expose-handler* *SUPPRESSED-ACTIONS*
           :+degs->rads+

           :boxer-gl-model
           :cur-tick
           :needs-update
           :draw
           :reset-meshes

           :*mouse-down-p*
           :boxer-pane-mouse-down?
           :*track-mouse-x*
           :*track-mouse-y*
           :*document-mouse-x*
           :*document-mouse-y*
           :boxer-pane-mouse-position

           :content-origin :resolution :update-gpu-matrices
           :set-pen-color :create-transform-matrix :init-freetype-fonts
           :string-wid :string-hei :find-cached-font :clear-window
           :backdrop-color :cha-hei :opengl-font-fontspec
           :*cur-gl-model-screen-obj* :*font-size-baseline* :cha-wid
           :get-visible-screen-objs :pixel->color
           :*foreground-color* :*background-color* :with-pen-color
           :update-framerate :drawing-on-window :repaint-window

           :make-ogl-pixmap :ogl-pixmap-width :ogl-pixmap-height :ogl-pixmap-texture
           :ogl-pixmap-data :ogl-pixmap-update-texture-p :*pixmap-data-type* :*pixmap-data-format*
           :pixmap-pixel

           :graphics-canvas :graphics-canvas-pixmap :enable :disable :get-canvas-mesh :op-count
           :graphics-canvas-pen-color-cmd :graphics-canvas-pen-size-cmd :graphics-canvas-pen-font-cmd
           :clear-graphics-canvas :clear :resize :boxgl-device-pen-color
           :viewport-height :viewport-width

           :*update-bitmap?*

           :boxgl-device :boxgl-device-pen-size :boxgl-device-projection-matrix
           :boxgl-device-transform-matrix :boxgl-device-model-matrix
           :create-ortho-matrix :line-stipple

           :gl-reshape :with-ogl-font :ogl-font-height :ogl-font-width :ogl-font-ascent

           :*boxer-pane* :*name-pane* :*boxer-frame*
           :gesture-spec-modifiers :gesture-spec-data :make-gesture-spec
           :input-gesture->char-code
           :key-to-keep-shifted? :gesture-spec-p
           :symbol-format :set-font-info
           :draw-line
           :reset-global-scrolling
           :*boxer-frame*
           :*current-opengl-font*
           :with-collection
           :collect
           :with-turtle-clipping
           :alu-andca :alu-seta :alu-xor :alu-and :alu-ior :alu-setz
           :port-to
           :getprop
           :make-box
           :removeprop
           :static-variables-alist
           :setup-editor
           :svref&
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
           :%drawing-window
           :%drawing-height
           :%drawing-width
           :%drawing-half-height
           :%drawing-half-width
           :x-out-of-bounds? :y-out-of-bounds?
           :max-window-coord :min-window-coord

           ;; font vars
           :%drawing-font-cha-hei
           :%drawing-font-cha-ascent
           ;; inits
           :def-redisplay-initialization
           :run-redisplay-inits

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
           :window-system-specific-make-boxer
           :window-system-specific-start-boxer
           :status-line-display :redraw-status-line
           :force-redisplay
           :input-code :input-bits
           :mouse-click-names :mouse-state-names :lookup-click-name
           :system-version
           :horizontal-scroll :vertical-scroll :zoom-level
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
    :no-inferiors-for-file?
    :no-cross-file-links?
    :cross-file-port-branch-links
    :dump-box-url :load-url :url-string?
    :url-box? :read-internal-url))

(defpackage :boxer-window
  (:use :common-lisp :boxer)
  (:nicknames :bw)
  (:export ;;
          ;;  :%bitblt-from-screen
          ;;  :%draw-point
          ;;  :%draw-rectangle :%erase-rectangle :%draw-line :%draw-poly
          ;;  :draw-cha
          ;;  :%draw-cha :%draw-string

           ;; bitmap functions
           :make-offscreen-bitmap :copy-offscreen-bitmap :free-offscreen-bitmap
           :offscreen-bitmap-width :offscreen-bitmap-height
           :offscreen-bitmap-depth
           :offscreen-bitmap-image :set-offscreen-bitmap-image
           :offscreen-pixel :image-pixel

           :rebind-font-info
           :%make-color :color? :color=
           :%set-pen-size :pixel-rgb-values
           :set-mouse-cursor :reset-mouse-cursor :with-mouse-cursor :beep
           :mouse-window-coords :mouse-button-state

           :outermost-screen-box
           ;; useful variables...

           :*point-blinker* :*mouse-blinker* :*sprite-blinker*
           ;; Window operations
           :window-system-dependent-redraw-status-line
           ;; input functions
           :get-character-input :get-boxer-input
           ;; cursor and blinker manipulation
           :with-mouse-tracking
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

;; Some useful variables and functions for dealing with the BU package
(defvar pkg-boxer-user-package (find-package 'boxer-user))
(defvar pkg-bu-package (find-package 'boxer-user))

;; Inherit External Symbols from BW
(USE-PACKAGE 'BOXER-WINDOW)
(use-package 'boxnet)
