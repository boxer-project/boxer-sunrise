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

(in-package :boxnet)

(defmethod url-box? ((box boxer::box))
  (and (boxer::storage-chunk? box)
       ;(read-only-box? box)
       (getf (slot-value box 'boxer::plist) :url)))

(defun url-string? (String)
  (when (stringp string)
    (let ((cc (position #\: string)))
      (when cc
        (find-class (intern (string-upcase (format nil "~A-URL"
                                                   (subseq string 0 cc)))
                            (find-package "BOXNET"))
                    nil)))))

;; this is called from dumper code
(defmethod dump-box-url ((box boxer::box) stream)
  (let ((url (getf (slot-value box 'boxer::plist) :url)))
    (if (null url)
        (error "~A does not have a url" box)
        (dump-url url stream))))

(defmethod set-url-flags ((box boxer::box))
  (setf (slot-value box 'boxer::flags)
        (boxer::set-box-flag-storage-chunk?
         (boxer::set-box-flag-read-only-box? (slot-value box 'boxer::flags)
                                             t)
         t)))

(defun make-box-from-url (url-string &optional (type 'boxer::data-box) name no-decode)
  (let ((url (make-url-from-string url-string no-decode))
        (box (boxer::make-uninitialized-box type)))
    (shared-initialize box t)
    ;; fix up various attributes of the box
    (unless (null name)
      (boxer::set-name box (boxer::make-name-row
                            (list (if (and (typep name 'string)
                                           (not (typep name 'simple-string)))
                                      ;; adjustable strings blow out make-name-row
                                      (copy-seq name)
                                      name)))))
    (shrink box)
    (set-url-flags box)
    (putprop box url :url)
    (boxer::set-border-style box (if (not (null (boxer::exports box)))
                                   :thick-dashed
                                   :thick))
    box))

;;; Main function for making URL's
(defun make-url-from-string (string &optional no-decode)
  (let* ((place (position #\: string))
         (type (subseq string 0 place))
         (scheme-string (subseq string (1+& place)))
         (class (find-class (intern
                             (boxer::symbol-format nil "~A-URL"
                                                   (string-upcase type))
                            (find-package "BOXNET")))))
    (if (null class)
        (error "Don't know how to make a ~A url" type)
        #-embedded-boxer
        (make-instance class
                       :scheme-string (if no-decode
                                          scheme-string
                                          (quri:url-decode scheme-string))))))

(defmethod fill-box-from-url ((box boxer::box))
  (let ((url (getf (slot-value box 'plist) :url)))
    (unless (null url)
      (fill-box-using-url url box)
      ;; if there is a cached boxtop, remove it
      (let ((cb (getprop box :cached-boxtop)))
        (unless (null cb)
          (when (box::graphics-sheet? cb)
            (let ((bm (box::graphics-sheet-bit-array cb)))
              (unless (null bm) (box::ogl-free-pixmap bm))))
          (removeprop box :cached-boxtop)))
      (boxer::modified box)
      (boxer::mark-file-box-clean box))))



;; this is basically boxer::initialize-box-from-box with extra
;; handling to reconcile the slots and properties in the embedded box
;; with the slots and properties in the box file being read in.
(defmethod initialize-box-from-net-box ((box boxer::box) netbox)
  (let ((old-ds (display-style box))
        (old-flags (slot-value box 'boxer::flags)))
    (boxer::initialize-box-from-box box netbox)
    ;; the vanilla init will take the display style from the net box
    (case old-ds
      (:shrunk (shrink box))
      (:supershrunk (boxer::supershrink box)))
    ;; make sure certain flags in the existing box are preserved...
    (when (and (boxer::box-flag-storage-chunk? old-flags)
               (not (storage-chunk? box)))
      (setf (storage-chunk? box) t))
    (when (and (boxer::box-flag-read-only-box? old-flags)
               (not (read-only-box? box)))
      (setf (read-only-box? box) t))
    ;; make sure the border is (or stays) thick because
    ;; if the net box was not saved with thick borders, it will bash
    ;; the existing border style to thin
    (boxer::set-border-style box (if (not (null (boxer::exports box)))
                                   :thick-dashed
                                   :thick))
  box))
