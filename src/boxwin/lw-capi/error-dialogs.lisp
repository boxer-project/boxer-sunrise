;;;;        Boxer
;;;;        Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;        Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;        used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;        Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;        https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                             +-Data--+
;;;;                    This file is part of the | BOXER | system
;;;;                                             +-------+
;;;;
;;;;          Error Dialogs for showing backtraces and restart/quit options
;;;;
(in-package :boxer-window)

(capi:define-interface error-dialog ()
  ()
  (:panes
   (logo capi:output-pane
    :display-callback 'load-boxer-logo
    :visible-min-width 128
    :visible-min-height 128)
   (message capi:message-pane
            ::visible-min-height '(:character 7)
            :text
            "Boxer encountered an error in its own code.

Below is detailed information you can report to the Boxer team to help fix the issue.

You can try to restart Boxer, but some bugs may be persistent, and (rarely) might
spoil your Boxer world. Quitting is a reliable fresh start.")
   (backtrace capi:editor-pane
              :flag 'minimal-example
              :text "This is the backtrace"
              :buffer-name :TEMP
              :enabled :read-only
              :visible-min-width '(character 80)
              :visible-min-height '(character 15)
              :visible-max-width '(character 80)
              :visible-max-height '(character 15))
   (continue capi:push-button
             :text "Restart"
             :callback #'(lambda (data int)
                           (capi:exit-dialog :continue)))
   (quit capi:push-button
         :text "Quit Boxer"
         :callback #'(lambda (data int)
                           (capi:exit-dialog :quit)))
  )
  (:layouts
   (dialog capi:row-layout
           '(logo controls)
           :gap 20)
   (controls capi:column-layout
           '(message backtrace buttons))
   (buttons capi:row-layout
            '(nil continue quit)
            :ratios '(20 1 1))
  )
  (:default-initargs
   :title "Boxer Error"
   :background :transparent
   :visible-border t))

(defun load-boxer-logo (pane x y wid hei)
  (with-slots (logo) (capi:top-level-interface pane)
    (gp:draw-image logo
      (gp:load-image pane
        (gp:read-external-image
          (cl-fad:merge-pathnames-as-file boxer::*resources-dir* "images/boxer-sunrise-icon.png"))) 0 0)))

(defun show-error-dialog (error-description)
  (let ((dialog (make-instance 'error-dialog)))
    (setf (capi:editor-pane-text (slot-value dialog 'backtrace)) error-description)
    (case (capi:display-dialog dialog :owner *boxer-pane*)
      (:continue
       (invoke-restart 'BOXER-CONTINUE))
      (:quit
       (lw:quit)))))
