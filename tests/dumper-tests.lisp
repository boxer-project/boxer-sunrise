(in-package :boxer-sunrise-test)

(plan nil)

(let ((henri-sun-v5 (merge-pathnames "data/boxfiles-v5/henri-sun-v5.box"
                                     (make-pathname :directory (pathname-directory *load-truename*))))
      (www-box-v12 (merge-pathnames "data/boxfiles-v12/hello-www.box"
                                     (make-pathname :directory (pathname-directory *load-truename*)))))
  (is (boxer::get-box-file-format-version henri-sun-v5) 5)
  (is (boxer::get-box-file-format-version www-box-v12) 12)
)

;; 2022-09-03 Preparing tests for the new zipped file format

(defun simple-name-timestamp ()
  "Returns a simple timestamp in the form 2022-09-15_10-23. Avoids using any punctuation that might keep
  if from being used in a valid filename."
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time))
    (format nil "~A-~A-~A_~A-~A-~A" year mon day hour min sec)))

;;; For the new data format .boxer
;;; - Have an existing boxer world in .box format
;;; - Open it up and resave it in .boxer format
;;; - Manually unzip the zip file, then compare the md5's of the original one and zipped one

;; Basic test of creating a box structure, saving it, re-opening it, and verifying a few bits
(let* ((boxer::*supress-graphics-recording?* t)
      (boxer::*draw-status-line* nil)
      (current-time (simple-name-timestamp))
      (new-tests-dir (cl-fad:merge-pathnames-as-directory cl-user::*boxer-project-dir*
                                                          (format nil "tests/data/testing/~A/" current-time)))
      (zip-results-dir (cl-fad:merge-pathnames-as-directory new-tests-dir "unzip-results/"))
      (newbox (make-instance 'boxer::data-box))
      (reopened-box nil)
      (zipped-reopened-box nil))

  (uiop:ensure-all-directories-exist (list new-tests-dir zip-results-dir))
  (setf (boxer::name newbox) "WORLD")
  (boxer::bash-box-to-number newbox 42)
  (is (format nil "~A" newbox) "#<DATA-BOX 42 >")

  (boxer::dump-top-level-box newbox
    (cl-fad:merge-pathnames-as-file new-tests-dir "test42.box"))
  (setf reopened-box
        (boxer::read-internal
          (cl-fad:merge-pathnames-as-file new-tests-dir "test42.box")))

  (is (format nil "~A" reopened-box) "#<DATA-BOX 42 >")

  ;; Save to the new .boxer format
  (boxer::save-box-to-boxer-document-format-zipped newbox
    (cl-fad:merge-pathnames-as-file new-tests-dir "test42.boxer"))

  ;; Unzip the freshly created .boxer file
  (zip:unzip (cl-fad:merge-pathnames-as-file new-tests-dir "test42.boxer") zip-results-dir)

  ;; Ensure the existence of /boxer/document.box
  (ok (cl-fad:merge-pathnames-as-file zip-results-dir "boxer/document.box"))

  ;; Open it to verify the contents
  (setf zipped-reopened-box
        (boxer::read-internal
          (cl-fad:merge-pathnames-as-file zip-results-dir "boxer/document.box")))

  (is (format nil "~A" zipped-reopened-box) "#<DATA-BOX 42 >")

  ;; Compare the md5 of the original .box file and the freshly unzipped one
  (is (md5:md5sum-file (cl-fad:merge-pathnames-as-file zip-results-dir "boxer/document.box"))
      (md5:md5sum-file (cl-fad:merge-pathnames-as-file new-tests-dir "test42.box"))
    :test #'equalp)
)

(finalize)
