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


;; 2021-06-30 It hasn't been included in the asdf component for a while, but properly
;; retiring email functionality for now.

(defboxer-primitive bu::mail ((boxer-eval::dont-copy address) (bu::port-to message))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(defboxer-primitive bu::mail-document ((boxer-eval::dont-copy address))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(defboxer-primitive bu::get-mail ((boxer-eval::dont-copy mailbox) delete-messages?)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(defboxer-primitive bu::delete-message ((bu::port-to message))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(defboxer-primitive bu::mail-message? ((bu::port-to message))
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(defboxer-primitive bu::finger (user)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

;; from mailfile.lisp

(defboxer-primitive bu::new-get-mail ((eval::dont-copy mailbox) delete-messages?)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(defboxer-primitive bu::open-inbox ()
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)

(defboxer-primitive bu::open-mail (mailfile)
  (boxer-eval::primitive-signal-error :obsolete
                                      "Email functionality is not currently supported.")
  boxer-eval::*novalue*)
