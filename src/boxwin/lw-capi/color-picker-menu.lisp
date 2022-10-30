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
;;;;      Reusable color dropdown menu to use for all the toolbar items that are colors,
;;;;      such as font color, box background color, and box border colors.
;;;;
(in-package :boxer-window)

(defclass color-picker-menu (capi:option-pane)
  ()
  (:default-initargs
   :visible-max-width '(:character 8)
   ;; We use the symbols for the colors, rather than the colors themselves since they
   ;; are likely not created yet at this point.
   :items (list  (make-instance 'capi::menu-item
                                 :title "" :data nil)
                 (make-instance 'capi::menu-item
                                 :title "Black" :data 'boxer::*black*)
                 (make-instance 'capi::menu-item
                                 :title "White" :data 'boxer::*white*)
                 (make-instance 'capi::menu-item
                                 :title "Red" :data 'boxer::*red*)
                 (make-instance 'capi::menu-item
                                 :title "Green" :data 'boxer::*green*)
                 (make-instance 'capi::menu-item
                                 :title "Blue" :data 'boxer::*blue*)
                 (make-instance 'capi::menu-item
                                 :title "Cyan" :data 'boxer::*cyan*)
                 (make-instance 'capi::menu-item
                                 :title "Magenta" :data 'boxer::*magenta*)
                 (make-instance 'capi::menu-item
                                 :title "Yellow" :data 'boxer::*yellow*)
                 (make-instance 'capi::menu-item
                                 :title "Orange" :data 'boxer::*orange*)
                 (make-instance 'capi::menu-item
                                 :title "Purple" :data 'boxer::*purple*)
                 (make-instance 'capi::menu-item
                                 :title "Gray" :data 'boxer::*gray*))))
