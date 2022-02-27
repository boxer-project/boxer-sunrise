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
;;;; These are utility methods that are being extracted that depend on lispworks libraries.
;;;;

(in-package :boxer)

;; what about capi:simple-pane-visible-size ?
(defun sheet-inside-height (window)
  #+lispworks(gp:port-height window)
  #-lispworks 1000) ; TODO

(defun sheet-inside-width  (window)
  #+lispworks(gp:port-width  window)
  #-lispworks 1000) ; TODO

(defun window-inside-size (w)
  (values (sheet-inside-width  w) (sheet-inside-height w)))
