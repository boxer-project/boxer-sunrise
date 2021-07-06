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
;;;;        Boxer Styles, centrally collected set of fonts and styles to allow for redefintion
;;;;        and future theming support.  When possible going forward, we try to use existing names from
;;;;        HTML CSS, whose boxes, borders, and fonts actually match up fairly closely to what we need
;;;;        in Boxer.
;;;;
;;;;        Currently, just a centrally collected set of defvar's, but will have more "style sheet" type
;;;;        functionality in the future.
(in-package :boxer)

(defvar *mouse-shrink-corner--background-color* #(:RGB 1.0 0.9 0.0))
