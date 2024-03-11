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

(defun make-box-decoration-toolbar-items ()
  (list (make-instance 'capi:toolbar-component
         :items
         (list
           (make-instance 'color-picker-menu
             :toolbar-title "Background" :name "BackgroundColor"
             :help-key "Change the box background color."
             :title "Background"
             :title-gap 0
             :title-font (gp:make-font-description :size 11)
             :title-position :bottom
             :title-adjust :center
             :selection-callback #'(lambda (color interface)
                                     (if color
                                       (let* ((rgb-hex-color (boxer::rgb->rgb-hex color)))
                                         (boxer::set-css-style (boxer::box-point-is-in) :background-color rgb-hex-color))
                                       (boxer::remove-css-style (boxer::box-point-is-in) :background-color))
                                     (boxer::repaint)))
           (make-instance 'color-picker-menu
             :toolbar-title "Border" :name "BorderColor"
             :help-key "Change the box border color."
             :title "Border"
             :title-gap 0
             :title-font (gp:make-font-description :size 11)
             :title-position :bottom
             :title-adjust :center
             :selection-callback #'(lambda (color interface)
                                     (if color
                                       (let* ((rgb-hex-color (boxer::rgb->rgb-hex color)))
                                         (boxer::set-css-style (boxer::box-point-is-in) :border-color rgb-hex-color))
                                       (boxer::remove-css-style (boxer::box-point-is-in) :border-color))
                                     (boxer::repaint)))))
  ))

(defun make-action-toolbar-items ()
  (list
    (make-instance 'capi:toolbar-component
         :items
         (list
           (make-instance 'capi:toolbar-button
                           :text "Run/Stop" :name "RunStop" :image 4 :selected-image 5
                           :help-key "Evaluate the current line. Stop evaluation when running."
                           :selection-callback (lambda (huh frame)
                                                 (cond (*suppress-expose-handler* ; I think this means we're not evaluating
                                                       (menu-stop nil nil))
                                                       ((menu-do-line nil nil)
                                                     )))
                           :retract-callback (lambda (huh frame)
                                                 (cond (*suppress-expose-handler* ; I think this means we're not evaluating
                                                       (menu-stop nil nil))
                                                       ((menu-do-line nil nil)
                                                     ))))
           (make-instance 'capi:toolbar-button
                           :text "Closet" :name "Closet" :image 6 :selected-image 7
                           :help-key "Open the box closet."
                           :selection-callback (lambda (huh frame)
                                                 (boxer::com-toggle-closets)
                                                 (boxer::repaint)
                                                 (update-toolbar-items))
                           :retract-callback (lambda (huh frame)
                                                 (boxer::com-toggle-closets)
                                                 (boxer::repaint)
                                                 (update-toolbar-items)))
           (make-instance 'capi:toolbar-button
                           :text "Top Level" :name "TopLevel" :image 8 :selected-image 9
                           :help-key "Switch to top level mouse/key definitions."
                           :selection-callback (lambda (huh frame)
                                                 (boxer::com-toggle-vanilla-mode)
                                                 (boxer::repaint))
                           :retract-callback (lambda (huh frame)
                                                 (boxer::com-toggle-vanilla-mode)
                                                 (boxer::repaint)))
           (make-instance 'capi:toolbar-button
                           :text "Save File Box" :name "SaveFileBox" :image 10 :selected-image 11
                           :help-key "Save current file box."
                           :selection-callback (lambda (huh frame)
                                                 (boxer::com-save-document)
                                                 (boxer::repaint))
                           :retract-callback (lambda (huh frame)
                                                 (boxer::com-save-document)
                                                 (boxer::repaint))))
         :interaction :multiple-selection
         :default-image-set
           (capi:make-general-image-set
            :image-count 12
            :id (gp:read-external-image
                  (merge-pathnames "./images/boxer16x16icons.png" boxer::*resources-dir*))))
  ))

(defun make-text-buttons-toolbar-items ()
  "We are making these seperate from the dropdowns so it can be a multiple select toolbar,
  also, so the buttons line up with the dropdowns (when the dropdowns have labels, otherwise
  these icons sort of float in the middle)."
  (list (make-instance 'capi:toolbar-component
          :items (list
                   (make-instance 'capi:toolbar-button
                         :text "Bold" :name "Bold" :image 0 :selected-image 1
                         :help-key "Make text bold."
                         ; :interaction :multiple-selection
                         :selection-callback #'(lambda (huh self) (font-style-menu-action :bold 0))
                         :retract-callback #'(lambda (huh self) (font-style-menu-action :bold 0)))
                   (make-instance 'capi:toolbar-button
                         :text "Italics" :name "Italics" :image 2 :selected-image 3
                         :help-key "Make text italic."
                         :selection-callback #'(lambda (huh self) (font-style-menu-action :italic 0))
                         :retract-callback #'(lambda (huh self) (font-style-menu-action :italic 0))))
          :interaction :multiple-selection
          :default-image-set (capi:make-general-image-set
                                :image-count 10
                                :id (gp:read-external-image
                                      (merge-pathnames "./images/boxer16x16icons.png" boxer::*resources-dir*)))
  )))

(defun make-text-dropdown-toolbar-items ()
  (list (make-instance 'capi:toolbar-component
         :items
         (list
         (make-instance 'capi:option-pane
           :toolbar-title "Font" :name "Font"
           :help-key "Choose the font family."
           :title "Font"
           :title-gap 0
           :title-position :bottom
           :title-font (gp:make-font-description :size 11)
           :title-adjust :center
           :background :transparent
           :items boxer::*font-families*
           :visible-max-width '(:character 15)
           :selection-callback #'(lambda (font interface)
                                   (font-menu-action font 0)))
         (make-instance 'capi:option-pane
           :toolbar-title "Font Size" :name "FontSize"
           :help-key "Set the font size."
           :title "Size"
           :title-font (gp:make-font-description :size 11)
           :title-gap 0
           :title-position :bottom
           :title-adjust :center
           :items '("8" "9" "10" "11" "12" "14" "16" "18" "20" "22" "24" "26" "28" "36" "48" "72")
           :visible-max-width '(:character 3)
           :selection-callback #'(lambda (size interface)
                                   (font-size-menu-action (parse-integer size) 0)))
         (make-instance 'color-picker-menu
           :toolbar-title "Font Color" :name "FontColor"
           :help-key "Choose the font color."
           :title "Color"
           :title-font (gp:make-font-description :size 11)
           :title-gap 0
           :title-position :bottom
           :title-adjust :center
           :selection-callback #'(lambda (color interface)
                                   (if color
                                     (font-color-menu-action color nil))))))))

(defun update-toolbar-button (item)
  "Update a single item on the Boxer toolbar. We do this by inspecting it's unique `name` slot and adjusting
  it accordingly based on that."
  (let* ((current-font (boxer::bfd-font-no (get-current-font)))
         (font-name (boxer::font-name current-font))
         (font-size (boxer::font-size current-font))
         ;; (font-color we're going to have to peek in to the ogl colors, see the html export for examples)
         (bold-font (boxer::bold-font? current-font))
         (italic-font (boxer::italic-font? current-font))
         (status-bar (slot-value *boxer-frame* 'status-bar-pane))
         (current-color (boxer::bfd-color (get-current-font)))
         (point-box (boxer::box-point-is-in))
         (background-color (boxer::get-css-style point-box :background-color))
         (border-color (boxer::get-css-style point-box :border-color))
         (file-modified nil))

    (if point-box
      (multiple-value-bind (origin-type file-format read-only? fmodified?)
        (boxer::get-box-file-props (boxer::current-file-box point-box))
        (setf file-modified fmodified?)))

      (let ((name (slot-value item 'capi::name)))
        (cond ((and *suppress-expose-handler* (not (equal name "RunStop")))
              (setf (capi:simple-pane-enabled item) nil)
              )
              ((and (not *suppress-expose-handler*) (not (equal name "RunStop")))
               (setf (capi:simple-pane-enabled item) t)))

        (cond ((equal name "Font")
               (setf (capi:choice-selected-item item)
                     ;; sgithens TODO 2022-04-06 We need to improve things to show arbitrary fonts in case there are fonts used
                     ;; that we don't have bundled.
                     (if (capi:find-string-in-collection item font-name)
                         (capi:get-collection-item item (capi:find-string-in-collection item font-name))
                         0)))
              ((equal name "FontSize")
               (setf (capi:choice-selected-item item)
                     (capi:get-collection-item item (capi:find-string-in-collection item (format nil "~A" font-size)))))
              ((equal name "FontColor")
               (setf (capi:choice-selected-item item)
                     (capi:get-collection-item item (or (position current-color (capi::collection-items item)
                                                               :test #'(lambda (fs it)
                                                                         (color= fs (capi::menu-item-data it)))) 0))))
              ((equal name "Bold")
               (setf (capi:item-selected item) bold-font)
              )
              ((equal name "Italics")
               (setf (capi:item-selected item) italic-font)
              )
              ((equal name "RunStop")
               (setf (capi:item-selected item) *suppress-expose-handler*)
              )
              ((equal name "Closet")
               (setf (capi:item-selected item) (and point-box (boxer::closet-opened? point-box))))
              ((equal name "TopLevel")
               (setf (capi:item-selected item)
                     (boxer::fast-memq boxer::*global-top-level-mode* boxer::*active-modes*)))
              ((equal name "SaveFileBox")
               (setf (capi:item-selected item) file-modified))
              ((equal name "BackgroundColor")
               (if background-color
                 (setf (capi:choice-selected-item item)
                       (capi:get-collection-item item (or (position (boxer::rgb-hex->rgb background-color) (capi::collection-items item)
                                                                                       :test #'(lambda (fs it)
                                                                                                 (color= fs (capi::menu-item-data it)))) 0)))
                 (setf (capi:choice-selected-item item)
                       (capi:get-collection-item item 0))))
              ((equal name "BorderColor")
               (if border-color
                 (setf (capi:choice-selected-item item)
                       (capi:get-collection-item item (or (position (boxer::rgb-hex->rgb border-color) (capi::collection-items item)
                                                                                   :test #'(lambda (fs it)
                                                                                             (color= fs (capi::menu-item-data it)))) 0)))
                 (setf (capi:choice-selected-item item)
                       (capi:get-collection-item item 0))))))))

(defun update-toolbar-items (&optional (children  (list (slot-value *boxer-frame* 'bw::text-toolbar)
                                                        (slot-value *boxer-frame* 'bw::text-buttons-toolbar)
                                                        (slot-value *boxer-frame* 'bw::actions-toolbar)
                                                        (slot-value *boxer-frame* 'bw::box-decoration-toolbar))))
  "A recursive method which runs through any number of capi toolbar, toolbar-button, and toolbar-component objects,
  which are all capi:collections. Whenever we hit an actual toolbar-button we update it."
  (for:for ((item over children))
    (cond
      ((or (typep item (find-class 'capi:toolbar-button))
            (typep item (find-class 'capi:option-pane)))
       (update-toolbar-button item))
      ((typep item (find-class 'capi:collection))
       (update-toolbar-items (capi:collection-items item)))
      ((typep item (find-class 'capi:layout))
       (update-toolbar-items (capi::layout-panes item)))
      (t
       (log:error "Un-handled toolbar item: ~A" item)))))

(defun update-toolbar-font-buttons ()
  (update-toolbar-items)
  ;; Bottom Status Line
  (setf (capi:title-pane-text (slot-value *boxer-frame* 'status-bar-pane))
        (format nil "~A ~40tZoom ~A%"
          (if (boxer::fast-memq boxer::*global-top-level-mode* boxer::*active-modes*)
            "System Mouse/Key Actions"
            "Mouse/Key Redefinitions Active")
          (round (* 100 (zoom-level *boxer-pane*))))))
