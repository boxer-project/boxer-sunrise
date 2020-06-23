#|
    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


 This file contains the `start-boxer` function for use in the MacOS .app bundle.

|#
(in-package :boxer)

(defun start-boxer ()
    (setf boxer-window::*capogi-font-directory*
        (make-pathname :directory
            (append (butlast (pathname-directory (lw:lisp-image-name)))
            '("Resources" "Fonts"))))
    (boxer-window::window-system-specific-make-boxer)
    (boxer-window::window-system-specific-start-boxer-1))
