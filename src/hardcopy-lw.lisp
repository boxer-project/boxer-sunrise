; -*- Mode:LISP; Syntax: Common-Lisp; Package:Boxer-Window; -*-
#|



              Copyright 2004 - 2005 Pyxisystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+

Printing Support for Windows


Modification History (most recent at top)

 1/04/05 simple printing works now
12/28/04 started file


|#

(in-package :boxer-window)

;; This will be passed the *boxer-frame*
(defun window-hardcopy (window)
  ;; Setup the printer for a 1 page document
  (let ((printer (capi:print-dialog :owner window
                                    :print-pages-p nil
                                    :print-copies-p t))
        ;; 
        (pane (slot-value window 'boxer-pane)))
    (when printer
      (capi:with-print-job (printer-port :pane pane :printer printer)
        (multiple-value-bind (page-width page-height)
            (capi:get-page-area printer)
          ;; Compute a transform which will fit the best dimension
          ;; to the paper size dimension, whilst preserving the
          ;; aspect ratio.
          (let* ((drawing-width (sheet-inside-width pane))
                 (drawing-height (sheet-inside-height pane))
                 (widen-p (> (/ page-width page-height)
                             (/ drawing-width drawing-height)))
                 (page-transform-x 0)
                 (page-transform-y 0)
                 (page-transform-width (if widen-p
                                           (* page-width
                                              (/ drawing-height page-height))
                                         drawing-width))
                 (page-transform-height (if widen-p
                                            drawing-height
                                          (* page-height
                                             (/ drawing-width page-width)))))
            (capi:with-document-pages (page 1 1) ; all on one page
              (capi:with-page-transform (page-transform-x
                                         page-transform-y
                                         page-transform-width
                                         page-transform-height)
                ;; this is emulating boxer::redisplaying-window without binding the port
                ;; because the port should be bound by the printing routines to the printer port
                (let ((boxer::*COMPLETE-REDISPLAY-IN-PROGRESS?* t)
                      (boxer::*redisplay-window* *boxer-pane*)
                      (boxer::*outermost-screen-box* (boxer::outermost-screen-box *boxer-pane*))
                      (boxer::%drawing-array printer-port))
                  (boxer::drawing-on-window (printer-port)
                    (boxer::redisplay-guts)))))))))))


;; testing
#|
(let ((printer (capi:print-dialog :print-pages-p nil
                                    :print-copies-p t)))
    (when printer
      (capi:with-print-job (printer-port :printer printer)
        (multiple-value-bind (page-width page-height)
            (capi:get-page-area printer)
          (values page-width page-height)))))


|#
