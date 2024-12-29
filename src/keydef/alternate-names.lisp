;;;;
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
;;;;
;;;;
;;;;       This file contains utilities looking up analogous keynames from other platforms.
;;;;
(in-package :boxer)

;; stubbified in anticipation of "clean" reimplementation (10/07/02)
;; "clean" replacement should take a key/click name and return the
;; analogous key/click name in another platform
;; in addition, we should try and link up with DEFINE-INPUT-DEVICES & friends
;; in keydef-high.lisp

;(defun alternate-platform-input-name (name) name)

;; alternate platform names should return a list instead to allow for the
;; future possibility of multiple alternate platforms...
;; In addition, we have handle-boxer-input (keydef-high.lisp) pass key codes
;; and shift bits to handle-boxer-key to make this work better
;;
;; We don't want to precons all the possible alternate input names, but we will
;; use a lookup array as a cache of alternate names
;; Note: We also look for alternative names HERE, instead of producing the entire
;; list of possible names for handle-boxer-input in order to limit the check
;; for alternatives to the case where an existing binding is missing

(defun alternate-platform-input-names (code bits &key (platform *initial-platform*))
  (let ((existing (aref *alternate-key-names* code bits)))
    (cond ((eq existing :no-alternates) nil)
      ((not (null existing)) ; cached alternates...
                             existing)
      (t
       ;; calculate and cache
       (let ((handled-shifts
              (unless (zerop& bits)
                (list (nth (1-& bits) (input-device-shift-list
                                       *current-input-device-platform*)))))
             (alternates nil))
         (dolist (platform *defined-input-device-platforms*)
           (let ((other-shift (unless (zerop& bits)
                                (nth (1-& bits)
                                     (input-device-shift-list platform)))))
             (unless (or (null other-shift)
                         (member other-shift handled-shifts
                                 :test #'string-equal))
               (push other-shift handled-shifts)
               (push (intern-in-bu-package
                      (concatenate 'string
                                   other-shift "-"
                                   ;; get the plain (unshifted) key name
                                   (string (lookup-key-name code 0))))
                     alternates))))
         (cond ((null alternates)
                (setf (aref *alternate-key-names* code bits)
                      :no-alternates)
                nil)
           (t (setf (aref *alternate-key-names* code bits)
                    alternates))))))))

;; mostly right, it still doesn't hack the translation of the actual click
;; name i.e. GRAPHICS-MOUSE-MIDDLE <==> MOUSE-CLICK-ON-GRAPHICS but since we
;; are unlikely to be looking at any (working) LispM code, it's probably OK
(defun alternate-platform-click-names (click bits area &key (platform *initial-platform*))
  (let* ((lookup-table (if (null area)
                         *alternate-mouse-click-name-translation-table*
                         (get area 'alternate-click-name-table)))
         (existing (aref lookup-table click bits)))
    (cond ((eq existing :no-alternates) nil)
      ((not (null existing)) ; cached alternates...
                             existing)
      (t
       ;; calculate and cache
       (let ((handled-shifts
              (unless (zerop& bits)
                (list (nth (1-& bits) (input-device-shift-list
                                       *current-input-device-platform*)))))
             (alternates nil))
         (dolist (platform *defined-input-device-platforms*)
           (let ((other-shift (unless (zerop& bits)
                                (nth (1-& bits)
                                     (input-device-shift-list platform)))))
             (unless (or (null other-shift)
                         (member other-shift handled-shifts
                                 :test #'string-equal))
               (push other-shift handled-shifts)
               (push (intern-in-bu-package
                      (concatenate 'string
                                   other-shift "-"
                                   (string (lookup-click-name click 0 area))))
                     alternates))))
         (cond ((null alternates)
                (setf (aref lookup-table click bits) :no-alternates)
                nil)
           (t (setf (aref lookup-table click bits) alternates))))))))
