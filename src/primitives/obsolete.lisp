;;;;
;;;;    Boxer
;;;;    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay
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
