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


(defvar *solarized-base03* #(:rgb-hex "#002b36"))
(defvar *solarized-base02* #(:rgb-hex "#073642"))
(defvar *solarized-base01* #(:rgb-hex "#586e75"))
(defvar *solarized-base00* #(:rgb-hex "#657b83"))
(defvar *solarized-base0* #(:rgb-hex "#839496"))
(defvar *solarized-base1* #(:rgb-hex "#93a1a1"))
(defvar *solarized-base2* #(:rgb-hex "#eee8d5"))
(defvar *solarized-base3* #(:rgb-hex "#fdf6e3"))
(defvar *solarized-yellow* #(:rgb-hex "#b58900"))
(defvar *solarized-orange* #(:rgb-hex "#cb4b16"))
(defvar *solarized-red* #(:rgb-hex "#dc322f"))
(defvar *solarized-magenta* #(:rgb-hex "#d33682"))
(defvar *solarized-violet* #(:rgb-hex "#6c71c4"))
(defvar *solarized-blue* #(:rgb-hex "#268bd2"))
(defvar *solarized-cyan* #(:rgb-hex "#2aa198"))
(defvar *solarized-green* #(:rgb-hex "#859900"))

(defun rgb-hex->rgb (color)
  (let ((hexstring (aref color 1)))
    `#(:rgb ,(/ (parse-integer (subseq hexstring 1 3) :radix 16) 255)
            ,(/ (parse-integer (subseq hexstring 3 5) :radix 16) 255)
            ,(/ (parse-integer (subseq hexstring 5 7) :radix 16) 255))))

(defun rgb-hex->ogl (color)
  (bw::ogl-convert-color (rgb-hex->rgb color)))

(defun solarized-dark-theme ()
  (setf *background-color* (rgb-hex->ogl *solarized-base03*))
  (setf *foreground-color* (rgb-hex->ogl *solarized-base0*))
  (setf *default-border-color* (rgb-hex->ogl *solarized-base1*))
)
