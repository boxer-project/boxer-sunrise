; -*- Mode:LISP; Syntax: Common-Lisp; Package:Boxer; -*-
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


    registration utilities for Lispworks OpenGL port for macs



|#

(in-package :boxer)

;; On the mac, license info is stored in files....
;; other candidates:
;; #$kCurrentUserFolderType only applies to the current user which might not
;;  be a constant
;; #$kapplicationsupportfoldertype has a problem with teacher/student installs
;;   because of insifficient priviledges- either writing the file or updating
;;   the "magic" files for the demo

(defun pyxi-support-folder ()
  (make-pathname :directory '(:absolute "Users" "Shared" "PyxiSystems")))
;  (let ((app-support (ccl::findfolder #$konsystemdisk
;                                      #$kSharedUserDataFolderType)))
;    (cond ((pathnamep app-support)
;           (make-pathname :directory (append (pathname-directory app-support)
;                                             '("PyxiSystems"))))
;          (t nil))))

(defun license-pathname ()
  (let ((psf (pyxi-support-folder)))
    (unless (null psf) (merge-pathnames "license" psf))))

(defun demo-date-pathname ()
  (let ((psf (pyxi-support-folder)))
    (unless (null psf) (merge-pathnames "demo-date" psf))))

(defun demo-magic-pathname ()
  (let ((psf (pyxi-support-folder)))
    (unless (null psf) (merge-pathnames "demo-magic" psf))))


(defun get-boxer-license-key ()
  (let ((lp (license-pathname)))
    (when (and lp (probe-file lp)) (with-open-file (s lp) (read-line s)))))

(defun set-boxer-license-key (keystring)
  (let ((lp (license-pathname)))
    (unless (null lp)
      (with-open-file (out lp :direction :output :if-exists :supersede)
        (write-line keystring out)))))


(defun boxer-license-dialog (&optional offer-demo?)
  (let* ((group1 (make-instance 'capi:text-input-pane :x 10 :y 40
                                :width 20 :height 15
                                :max-characters 4 :enabled t))
         (group2 (make-instance 'capi:text-input-pane :x 80 :y 40
                               :max-width 30
                               :max-characters 4 :enabled t))
         (group3 (make-instance 'capi:text-input-pane :x 150 :y 40
                               :max-width 30
                               :max-characters 4 :enabled t))
         (group4 (make-instance 'capi:text-input-pane :x 220 :y 40
                               :max-width 30
                               :max-characters 4 :enabled t))
         (license-text (make-instance 'capi:title-pane :x 20 :y 20
                                      :text "License Key Entry"))
         (items (list
                ;; the license number in 4 digit fields...
                license-text
                group1
                (make-instance 'capi:title-pane :x 70 :y 40 :text "-")
                group2
                (make-instance 'capi:title-pane :x 140 :y 40 :text "-")
                group3
                (make-instance 'capi:title-pane :x 210 :y 40 :text "-")
                group4
                ;; other buttons
                (make-instance 'capi:push-button :x 60 :y 70 :width 50 :height 20
                               :text "OK"
                               :selection-callback
                               #'(lambda (&rest ignore)
                                   (declare (ignore ignore))
                                   (capi:exit-dialog
                                    (concatenate
                                     'string
                                     (capi:text-input-pane-text group1)
                                     (capi:text-input-pane-text group2)
                                     (capi:text-input-pane-text group3)
                                     (capi:text-input-pane-text group4)))))
                (make-instance 'capi:push-button :x 190 :y 70 :width 50 :height 20
                               :text "Cancel"
                               :selection-callback 'capi:abort-dialog)))
         (ld (capi:make-container
              (make-instance
               'capi:pinboard-layout :min-width 300 :min-height 120
               :description
               (if offer-demo?
                   (append items
                           (list (make-instance 'capi:push-button
                                                :x 120 :y 70 :width 50 :height 20
                                                :text "Demo"
                                                :selection-callback
                                                #'(lambda (&rest ignore)
                                                    (declare (ignore ignore))
                                                    (capi:exit-dialog "0")))))
                 items)
               :title "License Key Entry"))))
    (capi:display-dialog ld :modal t)))

;; temp....
(defun valid-boxer-license? () T)
;  (let ((existing (get-boxer-license-key)))
;    (cond ((null existing) nil)
;          (t (valid-license-number (read-from-string existing))))))





;;; Valid boxer license keys
;;; 4 fields of 4 base 10 numbers
(defconstant *license-min* 1000000000000000)
(defconstant *license-prime-A* 8377)
(defconstant *license-prime-B* 9277)

(defvar *license-key-stream* T)

(defun valid-license-number (n)
  (and (> n *license-min*)
       (zerop (mod n *license-prime-A*))
       (zerop (mod n *license-prime-B*))))

;;;This should NOT appear in the app !!!!
#|
(defun generate-license-keys (&key (min-key *license-min*) (n-keys 10))
  (do ((n (* (ceiling (/ min-key (max *license-prime-A*
                                      *license-prime-B*)))
             *license-prime-B*)
          (+ n *license-prime-B*))
       (count 0))
      ((>= count n-keys) count)
    (when (zerop (mod n *license-prime-A*))
      ;; output a key
      (print-license-key n)
      (incf count))))
|#



(defun print-license-key (n)
  (format *license-key-stream* "~:19,'0,'-,4D~%" n))


;;; New expiration scheme for demo, no more hard coded date
;;;
;;; Weaknesses
;;;    1) expiration scheme just a simple (unencrypted flag)
;;;    2) if (unencrypted) date file is constantly reset, the magic numbers
;;;       won't increment - fixed, dates are now encrypted
;;;    3) flush pyxi support files, redownload
;;;    4) encryptation keys are hardcoded in the boxer app making them
;;;       easy to discover
;;;    Conclusion: the current protection scheme is probably proof against
;;;    casual copying but wont stop a halfway competent hacker

;;; eventually move this to a bundled file after digitool makes the switch
;;; the resource records the date the app expired so we could conceivably
;;; pass that info upwards to generate a more informative error message...
;;; for 30 day expiration, write a magic number out for each day
;;; anything but a number in this array indicates no startup
;;; these essentially function as one time pads except they sit in the app
;;; Note: all these numbers were achieved via (random most-positive-fixnum)
(defvar *magic-expiration-array*
  #(34734175  271118789 276516700 38802561  25471640  135929489 405980097
    509323053 430523074 231983845 149157935 269856512 406603131 102862860
    296363948 105592821 197139497 513778864 209276327 133492344 494102449
    535204507 213166693 115796836 467770347 423059588 400308497 51688408
    158928445 436662981))

(defconstant *expiration-code* 240353669)

(defvar *magic-date-array*
  #(453620908 34734175  271118789 276516700 38802561  25471640  135929489
    405980097 509323053 430523074 231983845 149157935 269856512 406603131
    102862860 296363948 105592821 197139497 513778864 209276327 133492344
    494102449 535204507 213166693 115796836 467770347 423059588 400308497
    51688408  158928445))

(defun expired-boxer? ()
  (or ;(not (null (ccl::get-resource :BXEP 128)))
   ;; need to find a Windows way of modifying the app
      (let ((magic (get-demo-magic)))
        (when (numberp magic) (= *expiration-code* magic)))))

(defun %now-date ()
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour))
    (+ (* year 10000) (* month 100) date)))


;; stubs (on windows, they interacted with the registry)
(defun expire-boxer () )

(defun demo-boxer-license? ()
  (let ((existing (get-boxer-license-key)))
    (cond ((null existing) nil)
          (t (zerop (read-from-string existing))))))

;; if things aren't what they are supposed be, we'll try and fix them for now
;; when we are more sure of things, we can be more punitive - up to,
;; and including (ccl:quit)
(defvar *demo-nice?* nil)

(defun unexpected-magic-info ()
  (cond ((not (null *demo-nice?*))
         (initialize-demo-date))
        (t
         (capi:display-message "Warning:Corrupt Demo License Info")
         (lw::quit))))

(defun initialize-demo-date ()
  (write-demo-info (svref *magic-expiration-array* 0) (%now-date) 0))

(defun write-demo-info (magic date
                              &optional
                              (place (position magic
                                               *magic-expiration-array*
                                               :test #'=)))
  (cond ((null place)
         (error "Trying to write an invalid demo code:~D" magic))
        (t
         (let ((dmp (demo-magic-pathname)) (ddp (demo-date-pathname)))
           (unless (null dmp)
             (with-open-file (out dmp :direction :output :if-exists :supersede)
               (format out "~D" magic)))
           (unless (null ddp)
             (with-open-file (out ddp :direction :output :if-exists :supersede)
               (format out "~D" (+ date (svref *magic-date-array* place)))))))))

(defun get-demo-magic (&optional (mpath (demo-magic-pathname)))
  (with-open-file (m-in mpath)
    (let ((ml (read-line m-in nil nil)))
      (when (stringp ml) (read-from-string ml nil nil)))))

(defun get-demo-date (dpath magic)
  (let ((place (position magic *magic-expiration-array* :test #'=)))
    (cond ((null place)
           (error "Invalid demo code: ~D" magic))
          (t
           (with-open-file (d-in dpath)
             (let ((dl (read-line d-in  nil nil)))
               (when (stringp dl)
                 (- (read-from-string dl nil nil)
                    (svref *magic-date-array* place)))))))))

(defun demo-banner (place)
  (setq *boxer-version-info*
        (cond ((numberp place)
               (format nil "Boxer Free Demo: Day ~D" (1+ place)))
              ((eq place :last) "Boxer Free Demo: Last Day")
              ((eq place :reset) "Boxer Free Demo")
              (t "Boxer Free Demo"))))


;; write the next magic number if it's been more than a day since the last date,
;; maybe expired the boxer
;; also setup message

(defun demo-next-day ()
  (let ((dmp (demo-magic-pathname))(ddp (demo-date-pathname)))
    (cond ((and dmp (probe-file dmp) ddp (probe-file ddp))
           ;; all this had better be true
           (let* ((magic-number (get-demo-magic dmp))
                  (last-date (get-demo-date ddp magic-number))
                  (place (when (numberp magic-number)
                           (position magic-number *magic-expiration-array*
                                     :test #'=)))
                  (last-place (1- (length *magic-expiration-array*)))
                  (now (%now-date)))
             (cond ((not (numberp place))
                    (unexpected-magic-info))
                   ((not (numberp last-date))
                    (demo-banner :reset)
                    (initialize-demo-date))
                   ((< place last-place)
                    ;; the steady state case
                    (cond ((> now last-date)
                           (write-demo-info (svref *magic-expiration-array*
                                                   (1+ place))
                                            now)
                           (demo-banner (1+ place)))
                          ((= now last-date)
                           ;; same day, don't increment
                           (demo-banner place))
                          (t ;; tampering ?
                           (write-demo-info (svref *magic-expiration-array*
                                                   place)
                                            now)
                           (demo-banner place))))
                   ((>= place last-place)
                    (expire-boxer)
                    (demo-banner :last)))))
          (t
           ;; somethings wrong...
           (unexpected-magic-info)
           ))))

;; display of icons for file boxes....

;; stub (just a rectangle...)
(defun draw-file-icon-image (x y)
  (draw-rectangle alu-seta 32 32 x y))


