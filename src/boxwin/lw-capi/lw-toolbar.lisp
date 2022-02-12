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
;;;;  This file contains utility functions for the Lispworks CAPI version of the toolbar (fonts, start/stop etc)
;;;;  and bottom status bar (mouse mode, zoom level etc)
;;;;
(in-package :boxer-window)

(defun update-toolbar-font-buttons ()
  "This will look at the current font descriptor and update all the toolbar items, for style, font
  family, size, color etc for the toolbars.

  Eventually this will also scan through a currently selected region if one exists, to determine what
  parts can be set to a value, and which ones must be blanked out since the values differ in the
  selected region."
  (let* ((current-font (boxer::bfd-font-no (get-current-font)))
         (font-name (boxer::font-name current-font))
         (font-size (boxer::font-size current-font))
         ;; (font-color we're going to have to peek in to the ogl colors, see the html export for examples)
         (bold-font (boxer::bold-font? current-font))
         (italic-font (boxer::italic-font? current-font))
         (run-button (slot-value *boxer-frame* 'run-toolbar-button))
         (stop-button (slot-value *boxer-frame* 'stop-toolbar-button))
         (font-button (slot-value *boxer-frame* 'change-font-toolbar-button))
         (size-button (slot-value *boxer-frame* 'change-fontsize-toolbar-button))
         (color-button (slot-value *boxer-frame* 'change-fontcolor-toolbar-button))
         (status-bar (slot-value *boxer-frame* 'status-bar-pane))
         (current-color (boxer::bfd-color (get-current-font))))

    (setf (capi:choice-selected-item font-button)
          (capi:get-collection-item font-button (capi:find-string-in-collection font-button font-name))

          (capi:choice-selected-item size-button)
          (capi:get-collection-item size-button (capi:find-string-in-collection size-button (format nil "~A" font-size)))

          (capi:choice-selected-item color-button)
          (capi:get-collection-item color-button (or (position current-color (capi::collection-items color-button)
                                                               :test #'(lambda (fs it)
                                                                         (color= fs (symbol-value (capi::menu-item-data it))))) 0)))
    (if italic-font
      (setf (capi:item-selected (slot-value *boxer-frame* 'change-italics-toolbar-button)) t)
      (setf (capi:item-selected (slot-value *boxer-frame* 'change-italics-toolbar-button)) nil))
    (if bold-font
      (setf (capi:item-selected (slot-value *boxer-frame* 'change-bold-toolbar-button)) t)
      (setf (capi:item-selected (slot-value *boxer-frame* 'change-bold-toolbar-button)) nil))

    ;; Set the appropriate run/stop icons depending on if we are in eval
    (cond (*suppress-expose-handler*
           (setf (capi:toolbar-button-image run-button) 1)
           (setf (capi:toolbar-button-image stop-button) 3))
          (t
           (setf (capi:toolbar-button-image run-button) 0)
           (setf (capi:toolbar-button-image stop-button) 2)))

    ;; Bottom Status Line
    (setf (capi:title-pane-text status-bar)
      (format nil "~A ~40tFont Zoom ~A%"
        (vanilla-menu-item-print nil)
        (round (* 100 boxer::*font-size-baseline*))))
  ))
