;;;;
;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                         +-Data--+
;;;;                This file is part of the | BOXER | system
;;;;                                         +-------+
;;;;
;;;;
;;;;  This file contains primitives that are obsolete, from portions of code that has
;;;;  been removed from the system, such as support for managing email.
(in-package :boxer)

;;; 2021-06-30 It hasn't been included in the asdf component for a while, but properly
;;; retiring email functionality for now.

(boxer-eval:defboxer-primitive bu::mail ((boxer-eval::dont-copy address) (bu::port-to message))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::mail-document ((boxer-eval::dont-copy address))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::get-mail ((boxer-eval::dont-copy mailbox) delete-messages?)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::delete-message ((bu::port-to message))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::mail-message? ((bu::port-to message))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::finger (user)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

;;; from mailfile.lisp

(boxer-eval:defboxer-primitive bu::new-get-mail ((eval::dont-copy mailbox) delete-messages?)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::open-inbox ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::open-mail (mailfile)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

;;; 2021-07-03 Obsoleted primitives from gopher.lisp

(boxer-eval:defboxer-primitive bu::telnet (host)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Telnet is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::gopher-search (host (boxer-eval::numberize port)
                                            select-string search-string)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Gopher web functionality is not currently supported.")
  boxer-eval::*novalue*)

;; 2021-07-19 These have been obsolete since the 2014 release, but am moving them here from grprim1.lisp
;; to declutter that space.

(defvar *signal-error-for-sprite-pen-XOR* t)

(defsprite-function bu::px ()
  (sprite turtle)
  (if *signal-error-for-sprite-pen-XOR*
      (boxer-eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
    (set-pen turtle 'bu::reverse))
  boxer-eval::*novalue*)

(defsprite-function bu::penxor ()
  (sprite turtle)
  (if *signal-error-for-sprite-pen-XOR*
      (boxer-eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
    (set-pen turtle 'bu::reverse))
  boxer-eval::*novalue*)

(defsprite-function bu::penreverse ()
  (sprite turtle)
    (if *signal-error-for-sprite-pen-XOR*
      (boxer-eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
      (set-pen turtle 'bu::reverse))
  boxer-eval::*novalue*)

(defsprite-function bu::pr ()
  (sprite turtle)
    (if *signal-error-for-sprite-pen-XOR*
      (boxer-eval::primitive-signal-error :obsolete
                                    "XOR pens are no longer supported")
      (set-pen turtle 'bu::reverse))
  boxer-eval::*novalue*)

;; 2021-12-12  Primitives from net-prims for sending boxes back and forth between different
;; computers. It's actually pretty cool, and would be nice to modernize some day, but the
;; current implementation is built on a socket implementation from when Boxer was being
;; used on Sun machines.

(boxer-eval::defboxer-primitive bu::send-box ((boxer-eval::dont-copy where)(bu::port-to box))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Socket based sending features are not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::enable-boxer-send-polling ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Socket based sending features are not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::disable-boxer-send-polling ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Socket based sending features are not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::enable-boxer-send-interrupts ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Socket based sending features are not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::disable-boxer-send-interrupts ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Socket based sending features are not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::receive-boxer-send ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Socket based sending features are not currently supported.")
  boxer-eval::*novalue*)

;; Pretty wild stuff from recursive-prims
(boxer-eval::defboxer-primitive bu::holding-position ((list-rest what))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Holding-position is not currently supported.")
  boxer-eval::*novalue*)

(boxer-eval::defboxer-primitive bu::with-sprites-hidden ((bu::port-to graphics-box)
                                                         (list-rest what))
  (boxer-eval::primitive-signal-error :obsolete
                                      "with-sprites-hidden is not currently supported.")
  boxer-eval::*novalue*)

;; Things from sysprims

(boxer-eval::defboxer-primitive bu::show-font-info ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Fonts are just regular font files now.")
  boxer-eval::*novalue*)

(boxer-eval:defboxer-primitive bu::toggle-fonts ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Major font refactoring (bitmap -> vector) is done now.")
  boxer-eval::*novalue*)

;;;
;;; Obsolete or currently unused System/User Preferences from sysprims.lisp
;;;

;;
;; Stepper and Evaluator Prefs
;;

;; sgithens 2022-03-25 Removing these steppers preferences for now, since the stepper is currently
;;                     out of commission.
;; (defboxer-preference bu::step-wait-for-key-press (true-or-false)
;;   ((boxer-eval::*step-wait-for-key-press* :boolean
;;                                           (boxer-eval::boxer-boolean boxer-eval::*step-wait-for-key-press*))
;;    #+capi evaluator #-capi evaluator-settings
;;    ("Should the Stepper wait for a key press ")
;;    ("before going on to the next step ?")
;;    ("(The Stepper shows Boxer execution one step at a time.)"))
;;   (setq boxer-eval::*step-wait-for-key-press* true-or-false)
;;   boxer-eval::*novalue*)
;;
;; (defboxer-preference bu::step-time ((boxer-eval::numberize seconds))
;;   ((boxer-eval::*step-sleep-time* :number boxer-eval::*step-sleep-time*)
;;    #+capi evaluator #-capi evaluator-settings
;;    ("How many seconds should the Stepper pause between steps")
;;    ("(The Stepper shows Boxer execution one step at a time.)"))
;;   (setq boxer-eval::*step-sleep-time* seconds)
;;   boxer-eval::*novalue*)

;; sgithens 2022-03-25 Removing this for now, since optimally we should just always been repainting. There may be some
;;                     use case for this variable in the future.
;; (defboxer-preference bu::update-display-during-eval (true-or-false)
;;   ((*repaint-during-eval?* :keyword
;;                            (boxer-eval::boxer-boolean boxer-eval::*warn-about-primitive-shadowing*))
;;    #+capi evaluator #-capi evaluator-settings
;;    ("Should the screen be repainted during eval ? Valid entries are ALWAYS, NEVER and CHANGED-GRAPHICS"))
;;   (setq *repaint-during-eval?* true-or-false)
;;   boxer-eval::*novalue*)

;;
;; Editor Preferences
;;

;; sgithens 2021-03-28 Removing this for now as we are consolidating keyboards for all 3 platforms. This may or may not
;;                     be useful again in the future.
;;
;; (defboxer-preference bu::input-device-names (machine-type)
;;   ((*current-input-device-platform* :keyword
;;                                     (make-box
;;                                      `((,*current-input-device-platform*))))
;;    #+capi editor #-capi editor-settings
;;    ("Which set of names should be used to refer to ")
;;    ("special (control) keys or mouse actions ?")
;;    ("(Different platforms may use different names.)"))
;;   (let ((canonicalized-name (intern (string-upcase machine-type)
;;                                     (find-package 'keyword))))
;;     (if (fast-memq canonicalized-name *defined-input-device-platforms*)
;;       (make-input-devices canonicalized-name)
;;       (boxer-eval::primitive-signal-error :preference
;;                                           "The machine-type, " machine-type
;;                                           ", does not have a defined set of input devices"))
;;     boxer-eval::*novalue*))

;;
;; Network Preferences
;;

;;; Network stuff

;; sgithens 2021-03-08 Removing these network email preferences as email support is currently broken
;;                     and we aren't sure whether we will include this functionality going forward.
;;
;; (defboxer-preference bu::user-mail-address (address)
;;   ((boxnet::*user-mail-address* :string
;;                                 (make-box `((,boxnet::*user-mail-address*))))
;;    #+capi network #-capi network-settings
;;    ("What Internet address should identify you in various network dealings ?"))
;;   (let* ((newname address)
;;          (@pos (position #\@ newname)))
;;     ;; need some sort of consistency checking on the name here
;;     (if (null @pos)
;;       (boxer-eval::primitive-signal-error :preferences-error
;;                                           newname
;;                                           " Does not look like a valid address")
;;       (let ((user (subseq newname 0 @pos)) (host (subseq newname (1+ @pos))))
;;         (setq boxnet::*user-mail-address* newname
;;               boxnet::*pop-user* user
;;               boxnet::*pop-host* host)))
;;     boxer-eval::*novalue*))

;; (defboxer-preference bu::mail-relay-host (host)
;;   ((boxnet::*smtp-relay-host* :string (make-box `((,boxnet::*smtp-relay-host*))))
;;    #+capi network #-capi network-settings
;;    ("What computer should be responsible for ")
;;    ("relaying mail to the Internet ?"))
;;   (let ((newname host))
;;     ;; need some sort of consistency checking on the name here
;;     (setq boxnet::*smtp-relay-host* newname)
;;     boxer-eval::*novalue*))

;; ;; should have a hook to access the MIME type dialog

;; (defboxer-preference bu::query-for-unkown-mime-type (true-or-false)
;;   ((boxnet::*query-for-unknown-mime-type* :boolean
;;                                           (boxer-eval::boxer-boolean boxnet::*query-for-unknown-mime-type*))
;;    #+capi network #-capi network-settings
;;    ("Should a dialog popup if an unknown")
;;    ("MIME (mail attachment) type is encountered ?"))
;;   (setq boxnet::*query-for-unknown-mime-type* true-or-false)
;;   boxer-eval::*novalue*)

;; (defboxer-preference bu::mail-inbox-file (filename)
;;   ((boxnet::*inbox-pathname* :string (make-box `((,boxnet::*inbox-pathname*))))
;;    #+capi network #-capi network-settings
;;    ("Which File should new mail be placed in"))
;;   (let ((newpath filename))
;;     ;; should reality check here (at least directory should exist)
;;     (setq boxnet::*inbox-pathname* newpath)
;;     boxer-eval::*novalue*))
