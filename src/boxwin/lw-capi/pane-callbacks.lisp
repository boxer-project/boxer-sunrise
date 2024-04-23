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
;;;;

(in-package :boxer-window)

(defparameter *boxer-pane-initialized* nil)

(defun boxer-pane-display-callback (canvas x y wid hei)
  ;; canvas ignore

  (unless *boxer-pane-initialized*
    ;; modernGL inits
    (opengl:rendering-on (*boxer-pane*)
      (boxer::opengl-enables)

      (setf bw::*boxgl-device* (slot-value
                                  (boxer::make-boxwin-330gl-device bw::*boxer-frame* bw::*boxer-pane* :wid wid :hei hei)
                                  'boxer::draw-device))
      (setf (boxer::boxgl-device-ortho-matrix bw::*boxgl-device*)
            (boxer::create-ortho-matrix wid hei))
      (opengl:gl-viewport 0 0 wid hei)
      (boxer::update-matrices-ubo bw::*boxgl-device*)
      (%set-pen-color box::*foreground-color*))

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
              boxer::*boxtop-text-font*         arial-16-bold
      ))
    ;; #+freetype-fonts
    (boxer::load-freetype-faces)
    (let ((boxer::%private-graphics-list nil))
      ;; needed by shape-box updater in the redisplay inits but not set until
      ;; (boxer-eval::setup-evaluator) farther down
      (run-redisplay-inits))
      (resize-handler-utility)
      (setf *boxer-pane-initialized* t)

    (opengl:rendering-on (*boxer-pane*)
      (log:debug "~%max-texture-size: ~A"  (gl:get-integer :max-texture-size))
      (setf boxer::*freetype-glyph-atlas* (boxer::make-glyph-atlas))
      (log:debug "~%Just created texture atlas: ~A" boxer::*freetype-glyph-atlas*))
    )

  (unless boxer::*evaluation-in-progress?*
    (opengl:rendering-on (*boxer-pane*)
      (resize-handler canvas x y wid hei)
      (setf (boxer::boxgl-device-ortho-matrix bw::*boxgl-device*)
            (boxer::create-ortho-matrix wid hei))

      ;; When resizing set the scrolling back to the top left corner, so the margins
      ;; don't get stuck. In the future we might want to be smarter, such as if we were
      ;; in the lower right corner all the way, we would stay there while resizing.
      (setf (horizontal-scroll *boxer-pane*) 0)
      (setf (vertical-scroll *boxer-pane*) 0)
      (boxer::set-transform bw::*boxgl-device* 0 0)

      (opengl:gl-viewport 0 0 wid hei)
      (boxer::update-matrices-ubo bw::*boxgl-device*))))
