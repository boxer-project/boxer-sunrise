;;;;
;;;;     Boxer
;;;;     Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;     Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;     used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;     Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;     https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                          +-Data--+
;;;;                 This file is part of the | BOXER | system
;;;;                                          +-------+
;;;;
;;;;      Code for reading and writing the new .boxer document format, which is encoded in a zip file
;;;;      .docx, .pages, .odt and other office/document formats.

(in-package :boxer)

(defun get-default-template ()
  "We have this special function to generate our own template rather than use the default, because when we deliver
  binaries on LispWorks, the system temporary directory from the image can get cached, and the folders for temporary
  directory locations are always different between instances of users/machines. Using this forces a lookup of the
  current temporary directory, rather than reusing the temporary directory root from the machine that the macOS/platform
  Application was built on."
  (format nil "~A%" (pathname (cl-fad::get-default-temporary-directory))))

(defun save-box-to-boxer-document-format-zipped (box filename)
  "Saves a box to the zipped boxer document format.
  We are associating this format with the .boxer file extension.

  The zip manifest file structure looks as follows:
  + ./
    + boxer/
      - document.box
  "
  ;; 1. Create a zip manifest
  (zip:with-output-to-zipfile (zf filename :if-exists :supersede)
    ;; 2. Stream data to a temp file
    (cl-fad:with-open-temporary-file (temp-stream :direction :io :element-type '(unsigned-byte 8)
                                      :template (get-default-template))
      (dump-top-level-box box nil temp-stream)
      (close temp-stream)
      ;; 3. Add that temp-file as a zipfile entry
      ;;    It would be nice to do this with a bi-direction stream or something, rather than a physical
      ;;    intermediate file.
      (with-open-file (file-stream (pathname temp-stream) :direction :input :element-type '(unsigned-byte 8))
        (zip:write-zipentry zf "boxer/document.box" file-stream)))))

(defun load-boxer-document-zipped (filename)
  "Given the filename of a .boxer document in zipped format with an binary box inside at
  boxer/document.box, open the file and return the box CLOS object for use."
  (zip:with-zipfile (zf filename)
    (let ((box-file-entry (zip:get-zipfile-entry "boxer/document.box" zf)))
      (cl-fad:with-open-temporary-file (temp-stream :direction :io :element-type '(unsigned-byte 8)
                                        :template (get-default-template))
        (zip:zipfile-entry-contents box-file-entry temp-stream)
        (close temp-stream)
        (load-binary-box-internal (pathname temp-stream))))))
