; -*- Mode:LISP; Syntax: Common-Lisp; Package:Boxer; -*-
#|



    Copyright 1995 - 1996 Regents of the University of California

 Enhancements and Modifications Copyright 1998 - 2003 Pyxisystems LLC

                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


Modification History (most recent at top)

 8/04/03 fixed typo in deallocate-av-info
 4/22/03 merged current LW and MCL files, no diffs, updated copyright


|#


(in-package :boxer)

;; testing stubs
;; we simulate the behavior of a QT file with a text file.
;; Each "frame" will be a line of text
;; I'll mark each Stub function which needs to be converted to a
;; true Quicktime handling function with a ****stub****

;; we'll hang QT info on the plist for now
(defmethod av-info ((box box))  (getf (slot-value box 'plist) 'av-info))

(defmethod set-av-info ((box box) av-info)
  (setf (getf (slot-value box 'plist) 'av-info) av-info))

;; ****stub**** ?
;; it's not clear we need any these slots since they may actually reflect
;; values derived from internal QT datastructures
;; Expect some of the automatically generated defstruct functions
;; to be used outside this file like:
;;    COPY-AV-INFO
;;
(defstruct av-info
  (file nil)  ; pathname or string
  (data nil)  ; a place to cache the data from the file if we want (e.g. after get-resource ?)
  (frame-pointer 0) ; QT dependent number, use mt's idea of seconds at higher level
  (playback-rate 1) ; a number, or perhaps some yet to be determined keywords
  (preferred-width  50) ; these are likely to be just QT interface functions
  (preferred-height 50) ; though it might be advantageous to cache these values
  ;; what else ????
  ;; looking at other QT players, there seem to be other state vars like
  ;; looping and back-and-forth flags.
  ;; also audio stuff like volume....
  )

;;****stub****
;; used to return any system dependent QT info that the GC can't reach
;; <avi> is a an av-info struct...
(defun deallocate-av-info (avi)
  (declare (ignore avi))
  t)

;; sort of non optimal, the alternative is to add more slots or share
;; existing slots in the av-info struct (the way sprite instance vars do)
(defun get-frame-interface-box (av-box)
  (eval::lookup-static-variable-in-box-only av-box 'bu::frame))

(defun get-playback-interface-box (av-box)
  (eval::lookup-static-variable-in-box-only av-box 'bu::playback-speed))


;;****stub****
;; returns the length of the av in internal-av-time
(defun av-length (av-info)
  ;; this is using the hack array implementation
  (let ((d (av-info-data av-info)))
    (if (null d) 0 (length d))))

;; all drawing is assumed to take place in a window where the origin has
;; been set to be the upper left hand corner of the graphics box where
;; the drawing is supposed to take place.
;; see drawing-on-turtle-slate for prims and various redisplay methods
;; If you need to draw in global coords, you can use the %origin-x-offset and
;; %origin-y-offset vars to get the current translation.  You MUST set it
;; back when you are done

;;****stub****
;; This is called in redisp.lisp by the REDISPLAY-INFERIORS-PASS-2-SB
;; method for graphics-screen-boxes
;; wid and hei are passed in case we need that info to win
;; should just display a still picture of the current frame
(defun draw-current-av-info (av x y &optional wid hei)
  (let ((data (av-info-data av)) (idx (av-info-frame-pointer av)))
    (unless (null data)
      (%draw-string alu-seta 3 (svref data idx) x (+ y (round hei 2))))))

;; not used at all in a QT implementation...
(defvar *normal-frame-pause-time* 1)

;; not used at all in a QT implementation...
(defun pause-time-from-rate (rate)
  (cond ((eq rate 'bu::normal) *normal-frame-pause-time*)
        ((eq rate 'bu::fastest) 0)
        ((numberp rate) (/  .1 (abs rate)))
        (t (error "~A is not a legal playback rate"))))

;; ****stub****
;; Note: need to also handle audio only
;; note that start can be < stop which means to play backward
;;
;; This is used in the prims PLAY and PLAY-FOR at the end of this file
;;
;; The <time> arg should be in internal-av-time units
;; the wid and hei args represent the available space in the screen-box
(defun play-frames (av x y wid hei &optional time)
  (let* ((rate (av-info-playback-rate av))
         (frame-pause-time (pause-time-from-rate rate))
         (start (av-info-frame-pointer av))
         (end (av-length av))
         (data (av-info-data av)))
    (if (and (numberp rate) (minusp rate))
        (do ((idx start (1- idx)) (elapsed-time 0 (1+ elapsed-time)))
            ((or (and time (> elapsed-time time)) (< idx 0)))
          (erase-rectangle wid hei 0 0)
          (%draw-string alu-seta 3 (svref data idx) x (+ y (round hei 2)))
          (setf (av-info-frame-pointer av) idx)
          (eval::check-for-interrupt :av-stop "Stopped!")
          (sleep frame-pause-time))
        (do ((idx start (1+ idx)) (elapsed-time 0 (1+ elapsed-time)))
            ((or (and time (>= elapsed-time time)) (>= idx end)))
          (erase-rectangle wid hei 0 0)
          (%draw-string alu-seta 3 (svref data idx) x (+ y (round hei 2)))
          (setf (av-info-frame-pointer av) idx)
          (eval::check-for-interrupt :av-stop "Stopped!")
          (sleep frame-pause-time)))))

;; pure boxer, these make the editor interface to internal QT values
;; you can use these as models for interfaces to other internal state
;; variables.  To make them work, you will have to write
;; the update-<state variable name> boxer function.
(defun make-frame-interface-box (current-value)
  (let ((box (make-box `((,current-value)) 'data-box "Frame")))
    (add-closet-row box (make-row (list (make-box '(("Update-Frame"))
                                                  'doit-box
                                                  "Modified-Trigger"))))
    box))

(defun make-playback-interface-box (current-value)
  (let ((box (make-box `((,current-value)) 'data-box "Playback-Speed")))
    (add-closet-row box (make-row (list (make-box '(("Update-Playback-Speed"))
                                                  'doit-box
                                                  "Modified-Trigger"))))
    box))

;; these are supposed to translate between the value of internal video
;; state values and whatever representation we choose to use at the user level
;; For example between seconds and frame numbers
;; we pass the av-info in case we need some additional information to
;; make the translation
;;
;; These are used by the various update-<state varable> boxer functions
;; on the triggers of interface boxes.  Changes to the internal QT state
;; should use these to update the current value of the interface boxes.
;;
;; ****stub****
(defun boxer-av-time->internal-av-time (boxer-frame-value)
  boxer-frame-value)

;; ****stub****
(defun internal-av-time->boxer-av-time (internal-frame-value)
  internal-frame-value)

;; ****stub****
(defun boxer-av-rate->internal-av-rate (boxer-playback-speed)
  boxer-playback-speed)

;; ****stub****
(defun internal-av-rate->boxer-av-rate (internal-playback-speed)
  internal-playback-speed)

;; pure boxer stuff though it does use the defstruct accessors which may
;; be plain functions in a real QT implementation...
(defun make-av-box (av-info)
  (let* ((box (make-box '(()) 'data-box))
         (1st-row (first-inferior-row box)))
    ;; normal graphics box stuff
    (setf (graphics-sheet box)
          (make-graphics-sheet (av-info-preferred-width av-info)
                               (av-info-preferred-height av-info) box))
    ;; give the box a graphics sheet
    (when *default-graphics-view-on?*
      (setf (display-style-graphics-mode? (display-style-list box)) t))
    ;; show the graphics side if thats what we think is right
    ;; av-stuff...
    (set-av-info box av-info)
    (append-cha 1st-row (make-frame-interface-box
                         (internal-av-time->boxer-av-time
                          (av-info-frame-pointer av-info))))
    (append-cha 1st-row (make-playback-interface-box
                         (internal-av-rate->boxer-av-rate
                          (av-info-playback-rate av-info))))
    ;; add any other interface boxes here
    box))

;;****stub****
;; mac implementation could check signatures and file types....
(defun av-file? (pathname) (not (null pathname)))

;;****stub****
;; associates info in <pathname> with a structure
;; the structure is then attached to a box.
;; Slots (like current-frame) in the structure which may not be derived
;; from the file should be intialized
;;
(defun set-av-info-from-file (info pathname)
  ;; this is probably the minimal thing for any implementation...
  (setf (av-info-file info) pathname)
  ;; actually, the defaults are probably pretty good, these are
  ;; here more for form's sake
  (setf (av-info-playback-rate info) 'bu::normal)
  (setf (av-info-frame-pointer info) 0)
  ;; this is the hack version which puts each line of text into the data array
  (let ((lines nil) (eof-value (cons 'a 'b)))
    (with-open-file (s pathname)
      (do ((line (read-line s nil eof-value) (read-line s nil eof-value)))
          ((eq line eof-value)
           (setf (av-info-data info)
                 (make-array (length lines) :initial-contents lines)))
        (setq lines (nconc lines (list line))))))
  info)

(defboxer-command com-make-av-box ()
  " makes an AV box, file dialog to select QT file"
  (status-line-display 'boxer-editor-error "Choose a video file...")
  (let* ((pa (or #+mcl (ccl::choose-file-dialog) nil))
         (info (make-av-info))
         (av-box (make-av-box info)))
    (when (av-file? pa) (set-av-info-from-file info pa))
    (unless (null av-box) (insert-cha *point* av-box))
    (add-redisplay-clue (point-row) :insert)
    eval::*novalue*))

;; here temporarily
;; as in option-a-key (a for av ? can't use "v" because it's already taken)
(defboxer-key (bu::a-key 2) com-make-av-box)

;; Primitives
(defboxer-primitive bu::set-av-file ((bu::port-to av-box)
                                     (eval::dont-copy filename))
  (let* ((box (box-or-port-target av-box))
         (info (av-info box)))
    (if (null info)
        ;; errors for now, possible to make be graceful and just convert...
        (eval::primitive-signal-error :av-error "The box is not an AV box")
        (progn
          (set-av-info-from-file info (box-text-string filename))
          (modified-graphics box)))
    eval::*novalue*))

;; utilities for update functions
(defun get-relevant-av-box ()
  (do ((box (static-root) (superior-box box)))
      ((not (box? box)) nil)
    (when (not (null (av-info box))) (return box))))

(defboxer-primitive bu::update-frame ()
  (let* ((av-box (get-relevant-av-box))
         (info (and av-box (av-info av-box)))
         (interface-box (get-frame-interface-box av-box)))
    (cond ((null av-box)
           (eval::primitive-signal-error :av-error "No AV box"))
          ((null interface-box)
           (eval::primitive-signal-error :av-error
                                         "Frame interface box is missing"))
          (t (let* ((frame-from-box (extract-item-from-editor-box interface-box))
                    (blength (internal-av-time->boxer-av-time (av-length info)))
                    (new-frame (cond ((or (not (numberp frame-from-box))
                                          (< frame-from-box 0))
                                      (bash-box-to-number interface-box 0)
                                      0)
                                     ((> frame-from-box blength)
                                      (bash-box-to-number interface-box blength)
                                      blength)
                                     (t frame-from-box))))
               (setf (av-info-frame-pointer info)
                     (boxer-av-time->internal-av-time new-frame)))))
    (modified-graphics av-box))
  eval::*novalue*)

;;
(defun legal-playback-value? (box-value)
  (or (numberp box-value)
      (fast-memq box-value '(bu::normal bu::fastest))))

(defun convert-playback-value (box-value)
  (if (numberp box-value)
      (boxer-av-rate->internal-av-rate box-value)
      box-value))

(defboxer-primitive bu::update-playback-speed ()
  (let* ((av-box (get-relevant-av-box))
         (info (and av-box (av-info av-box)))
         (interface-box (get-playback-interface-box av-box)))
    (cond ((null av-box)
           (eval::primitive-signal-error :av-error "No AV box"))
          ((null interface-box)
           (eval::primitive-signal-error :av-error
                                         "Playback interface box is missing"))
          (t (let* ((rate-from-box (extract-item-from-editor-box interface-box)))
               (unless (legal-playback-value? rate-from-box)
                 (bash-box-to-single-value interface-box 'bu::normal)
                 (setq rate-from-box 'bu::normal))
               (setf (av-info-playback-rate info)
                     (convert-playback-value rate-from-box))))))
  eval::*novalue*)

;; these are in the "implicit" arg style, use TELL to specify which av-box
(defboxer-primitive bu::set-time-counter ((eval::numberize new-time))
  (let ((av-box (get-relevant-av-box)))
    (if (null av-box)
        (eval::primitive-signal-error :av-error "Can't find AV box")
        (let* ((info (av-info av-box))
               (blength (internal-av-time->boxer-av-time (av-length info))))
          (cond ((< new-time 0)
                 (eval::primitive-signal-error :av-error
                                               "The new time should be > 0"))
                ((> new-time blength)
                 (eval::primitive-signal-error :av-error
                                               "The new time should be < "
                                               blength))
                (t (setf (av-info-frame-pointer info)
                         (boxer-av-time->internal-av-time new-time))))))
    (modified-graphics av-box))
  eval::*novalue*)

(defboxer-primitive bu::set-playback-speed ((eval::dont-copy new-speed))
  (let ((av-box (get-relevant-av-box)))
    (if (null av-box)
        (eval::primitive-signal-error :av-error "Can't find AV box")
        (let ((info (av-info av-box))
              (new-value (car (flat-box-items new-speed))))
          (if (not (legal-playback-value? new-value))
              (eval::primitive-signal-error :av-error new-speed
                                            "is not a legal playback value")
              (setf (av-info-playback-rate info)
                    (convert-playback-value new-value))))))
  eval::*novalue*)

;; ****stub?****
;; you need to decide if the drawing-on-turtle-slate is necessary for
;; your purposes, also, remember that ALL primitives operate inside an
;; DRAWING-ON-WINDOW environment
(defboxer-primitive bu::play ()
  (let ((av-box (get-relevant-av-box)))
    (if (null av-box)
        (eval::primitive-signal-error :av-error "Can't find AV box")
        (let ((screen-box (car (get-visible-screen-objs av-box)))
              (gs (graphics-sheet av-box)))
          ;; we just grab one screen-box instead of displaying in all
          ;; of them like normal sprite graphics.  It seems unlikely that
          ;; unrolling the playing of frames would be fast enough to make
          ;; this work in the multi screen-box case anyway
          ;;
          ;; This SetOrigin's and clips to the inside top left corner
          ;; of the screen-box.  Skip
          (drawing-on-turtle-slate screen-box
            (play-frames (av-info av-box) 0 0
                         (graphics-sheet-draw-wid gs)
                         (graphics-sheet-draw-hei gs)))
          ;; inform the GB to redisplay so that the sprite graphics
          ;; layer will redraw itself on top of the av graphics
          (modified-graphics av-box))))
  eval::*novalue*)

(defboxer-primitive bu::play-for ((eval::numberize time))
  (let ((av-box (get-relevant-av-box)))
    (if (null av-box)
        (eval::primitive-signal-error :av-error "Can't find AV box")
        (let ((screen-box (car (get-visible-screen-objs av-box)))
              (gs (graphics-sheet av-box)))
          (drawing-on-turtle-slate screen-box
            (play-frames (av-info av-box) 0 0
                         (graphics-sheet-draw-wid gs)
                         (graphics-sheet-draw-hei gs)
                         (boxer-av-time->internal-av-time time)))
          (modified-graphics av-box))))
  eval::*novalue*)

(defboxer-primitive bu::av-length ()
  (let ((av-box (get-relevant-av-box)))
    (if (null av-box)
        (eval::primitive-signal-error :av-error "Can't find AV box")
        (av-length (av-info av-box)))))

