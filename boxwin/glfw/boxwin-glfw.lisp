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
;;;;  Work In Progress! GLFW Engine
;;;;
(in-package :boxer-window)

;;;
;;; Re-implemented Utilities from boxwin-opengl and other places
;;;

(defun window-system-dependent-redraw-status-line (string)
  ;; (capi:apply-in-pane-process *name-pane* #'(lambda ()
  ;;                                            (setf (capi::title-pane-text *name-pane*) string)))
  )

(defun beep ()
  ;; (capi::beep-pane)
)


;;;
;;; GLFW Pane and Frame Classes and Methods
;;;

(defparameter scr-width 800)
(defparameter scr-height 600)

(defparameter *current-input-code* nil)
(defparameter *current-input-bits* 0)

(cl-glfw3:def-key-callback key-callback (window key scancode action mod-keys)
  (declare (ignore window scancode))
  (let ((bits 0)
        (key-value (cffi:foreign-enum-value '%glfw::key key)))
    (when (member :control mod-keys)
      (setf bits 2))
    (cond
      ((and (eq action :press) (eq key :backspace))
       (handle-boxer-input (code-char 8)))
      ((and (eq action :press) (eq key :enter))
       (handle-boxer-input (code-char 13) bits))
      ((and (eq action :press) (eq key :escape))
       (handle-boxer-input (code-char 27)))
      ((and (eq action :press) (member key '(:up :down :left :right)))
       (handle-boxer-input key bits (box::key-to-keep-shifted? key)))
      ((and (or (eq action :repeat) (eq action :press)) (< key-value 340))
       (setf *current-input-code* key-value
             *current-input-bits* bits))
      ((and (eq action :release) (< key-value 340))
       (when *current-input-code*
         (handle-boxer-input *current-input-code* *current-input-bits*))
       (setf *current-input-code* nil
             *current-input-bits* 0))
      (t
       nil))))

(cl-glfw3:def-char-callback char-callback (window codepoint)
  (declare  (ignore window))
  (setf *current-input-code* nil
        *current-input-bits* 0)
  (boxer::handle-boxer-input codepoint))

;; glfw: whenever the window size changed (by OS or user resize) this callback function executes"
(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (setf scr-width w scr-height h))

(defclass glfw-boxer-pane (boxer::boxer-canvas)
  ())

(defmethod boxer::viewport-width ((self glfw-boxer-pane))
  scr-width)

(defmethod boxer::viewport-height ((self glfw-boxer-pane))
  scr-height)

(defclass glfw-boxer-name-pane ()
  ())

(defclass glfw-boxer-frame ()
  ((boxer-pane :initform (make-instance 'glfw-boxer-pane))
   (name-pane  :initform (make-instance 'glfw-boxer-name-pane))))

(defmethod display ((self glfw-boxer-frame))
  ;; (with-body-in-main-thread ()
    (cl-glfw3:with-init-window (:title "LearnOpenGL" :width scr-width :height scr-height
                                :context-version-major 3
                                :context-version-minor 3
                                :opengl-profile #x00032001
                                #+os-macosx :opengl-forward-compat #+os-macosx t)
        (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)
        (cl-glfw3:set-char-callback 'char-callback)
        (cl-glfw3:set-key-callback 'key-callback)
        (cl-glfw3:set-window-size-callback 'update-viewport)

        ;; START Duplicated from pane-callbacks
        (boxer-opengl::opengl-enables)
        (setf bw::*boxgl-device* (boxer-opengl::make-boxgl-device scr-width scr-height))
        (setf (boxer::boxgl-device-projection-matrix bw::*boxgl-device*)
              (create-ortho-matrix scr-width scr-height))

        (update-gpu-matrices)
        (set-pen-color box::*foreground-color*)

        (let ((boxer::%private-graphics-list nil))
          ;; needed by shape-box updater in the redisplay inits but not set until
          ;; (boxer-eval::setup-evaluator) farther down
          (run-redisplay-inits))

        (init-freetype-fonts)
        ;; END pane-callbacks duplication

        (loop until (cl-glfw3:window-should-close-p)
            do (progn
              ;; per-frame-time-logic
              ;; --------------------
              (let ((current-frame (%cl-glfw3:get-time)))
                (setf box::delta-time (- current-frame box::last-frame)
                      box::last-frame current-frame))

              ;; render
              ;; ------
              ;; (setf (boxer::name *initial-box*) (format nil "The world: ~A" blinker-time))

              ;; Duplicated from repaint.lisp repaint-window
              ;; (boxer::REDISPLAYING-WINDOW (*boxer-pane*)
              (boxer::check-for-window-resize)
              (update-gpu-matrices)
                         (clear-window (backdrop-color *boxer-pane*))
                         (boxer::repaint-guts)
                        ;;  (repaint-mouse-docs)
                        ;;  (let ((cur-transform (boxer::boxgl-device-transform-matrix bw::*boxgl-device*)))
                        ;;    (set-transform bw::*boxgl-device* 0 0)
                        ;;    (repaint-dev-overlay process-state-label)
                        ;;    (setf (boxer::boxgl-device-transform-matrix bw::*boxgl-device*) cur-transform))
                        ;;  (when flush-buffer? (swap-graphics-buffers window))
                        ;;  )


              (cl-glfw3:swap-buffers)
              (cl-glfw3:poll-events)))))

;;;
;;; Window System Specific Make and Start Functions
;;;

(defun window-system-specific-make-boxer ()

  (setf *boxer-frame* (make-instance 'glfw-boxer-frame))
  ;; ;; after creation, set some variables
  (setf *boxer-pane* (slot-value *boxer-frame* 'boxer-pane)
        *name-pane*  (slot-value *boxer-frame* 'name-pane))
  (setf (boxer::point-blinker *boxer-pane*) (boxer::make-blinker))

  ;; load prefs if they exists
  ;; (let ((pf (boxer::default-lw-pref-file-name)))
  ;;   (when (and pf (probe-file pf))
  ;;     (boxer::handle-preference-initializations pf)))

  ;; maybe set the size of the boxer window...
  ;; check window size prefs, they will be overidden by the following
  ;; fullscreen-window check
  ;; (let ((screen (capi:convert-to-screen)))
  ;;   (when (> *starting-window-width* 0)
  ;;     (capi:set-hint-table *boxer-frame* (list :width *starting-window-width*)))
  ;;   (when (> *starting-window-height* 0)
  ;;     (capi:set-hint-table *boxer-frame* (list :height *starting-window-height*)))
  ;;   ;; fullscreen check AFTER prefs are loaded but BEFORE display ?
  ;;   (when *fullscreen-window-p*
  ;;     (capi:set-hint-table *boxer-frame*
  ;;                     (list :x 0 :y 0
  ;;                           :width (- (capi:screen-width screen) 10)
  ;;                           :height (- (capi:screen-height screen) 120)))))

  ;; START Duplicated from pane-callbacks

  (boxer::initialize-fonts)
  ;; END
  ;; START copied from disdep.lisp
  ;; (def-redisplay-initialization
  (progn ;; moved here because FD's need init'd colors
         (setq boxer::*default-font-descriptor* (boxer::make-bfd -1 boxer::*default-font*)
               boxer::*current-font-descriptor* (boxer::make-bfd -1 boxer::*default-font*))
        ;;  (drawing-on-window (boxer-window::*boxer-pane*)
        ;;                     (set-font-info *normal-font-no*))
                            )
                            ;; )
  ;; END

  ;; (capi:display boxer-window::*boxer-frame*)
  ;; (display *boxer-frame*)
  )

(defun window-system-specific-start-boxer ()
  (setq boxer-eval::*current-process* nil)

  ;; (boxer::load-appdata)
  (setup-editor boxer::*initial-box*)
  ;; (setq *display-bootstrapping-no-boxes-yet* nil)

  (boxer-eval::setup-evaluator)

  ;; (load-startup-file)

  (unless boxer::*boxer-version-info*
    (setq boxer::*boxer-version-info*
          (format nil "~:(~A~) Boxer" (machine-instance))))

  ;; (boxer-process-top-level-fn *boxer-pane*)
  ;;; START contents of boxer-process-top-level-fn
  (boxer::enter (boxer::point-box))
  (display *boxer-frame*))

(defun start-glfw-boxer (project-dir)
  (setf boxer::*capogi-font-directory* (merge-pathnames "data/boxersunrise.app/Contents/Resources/Fonts/" project-dir))
  (setf boxer::*resources-dir* (merge-pathnames "data/boxersunrise.app/Contents/Resources/" project-dir))
  (setf boxer::*shaders-dir* (merge-pathnames "src/draw-low-opengl330/shaders/" project-dir))

  #+sbcl (sb-int:set-floating-point-modes :traps nil)

  (window-system-specific-make-boxer)
  (window-system-specific-start-boxer))
