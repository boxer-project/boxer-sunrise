(in-package :boxer-sunrise-test)

(plan nil)

(defun make-simple-box (name contents)
  "An easy way to make a box quick for testing. Takes the name for the box and contents.
  Contents should be a single string that will populate the first row of the box."
  (let ((togo (make-instance 'boxer::data-box)))
    (setf (boxer::name togo) name)
    (boxer::bash-box-to-single-value togo contents)
    togo))

;; Tests for defun file-type
(let* ((boxer::*supress-graphics-recording?* t)
       (boxer::*draw-status-line* nil)
       (current-time (simple-name-timestamp))
       (tests-dir (cl-fad:merge-pathnames-as-directory cl-user::*boxer-project-dir*
                                                           (format nil "tests/data/testing/~A/" current-time)))
       (boxer-document (merge-pathnames "data/boxfiles-boxer/test42.boxer"
                                      (make-pathname :directory (pathname-directory *load-truename*))))
       (a-text-file (merge-pathnames "data/format-tests/hello.txt"
                                      (make-pathname :directory (pathname-directory *load-truename*))))
       (a-png (merge-pathnames "data/format-tests/really-a-png.box"
                                      (make-pathname :directory (pathname-directory *load-truename*))))
       (www-box-v12 (merge-pathnames "data/boxfiles-v12/hello-www.box"
                                      (make-pathname :directory (pathname-directory *load-truename*))))
       (text-box-to-save  (make-simple-box "txtbox" "ABCD"))
       (text-box-filename (cl-fad:merge-pathnames-as-file tests-dir "text-box.txt"))
       (bin-box-to-save   (make-simple-box "binbox" "EFG"))
       (bin-box-filename  (cl-fad:merge-pathnames-as-file tests-dir "bin-box.box"))
       (zip-box-to-save   (make-simple-box "zipbox" "HIJK"))
       (zip-box-filename  (cl-fad:merge-pathnames-as-file tests-dir "zip-box.boxer"))

       (text-box-to-save2  (make-simple-box "txtbox" "ABCD2"))
       (text-box-filename2 (cl-fad:merge-pathnames-as-file tests-dir "text-box2.txt"))
       (bin-box-to-save2   (make-simple-box "binbox" "EFG2"))
       (bin-box-filename2  (cl-fad:merge-pathnames-as-file tests-dir "bin-box2.box"))
       (zip-box-to-save2   (make-simple-box "zipbox" "HIJK2"))
       (zip-box-filename2  (cl-fad:merge-pathnames-as-file tests-dir "zip-box2.boxer")))

  (is (boxer::file-type boxer-document) :application/boxer.document)
  (is (boxer::file-type a-text-file) :text)
  ; (is (boxer::file-type a-png) :text)
  (is (boxer::file-type www-box-v12) :application/box)

(is (format nil "~A" (boxer::read-internal-1 boxer-document)) "#<DATA-BOX 42 >")
(is (format nil "~A" (boxer::read-internal-1 a-text-file)) "#<DATA-BOX This i... >")
;; TODO sgithens, We need to alter the file-type code a bit to still check the box magic
;; number if it can probe the file.
; (is (format nil "~A" (boxer::read-internal-1 a-png)) "#<DATA-BOX PNG >")
(is (format nil "~A" (boxer::read-internal-1 www-box-v12)) "#<DATA-BOX Hello ... >")

;; Some tests for save-generic from file-prims.lisp

(boxer::save-generic text-box-to-save text-box-filename :format :text/plain)
(boxer::save-generic bin-box-to-save bin-box-filename :format :application/box)
(boxer::save-generic zip-box-to-save zip-box-filename :format :application/boxer.document)

(is (format nil "~A" (boxer::read-internal-1 text-box-filename)) "#<DATA-BOX ABCD >")
(is (format nil "~A" (boxer::read-internal-1 bin-box-filename)) "#<DATA-BOX EFG >")
(is (format nil "~A" (boxer::read-internal-1 zip-box-filename)) "#<DATA-BOX HIJK >")

;; make sure it works detecting the format on it's own (no :format parameter...)
(boxer::save-generic text-box-to-save2 text-box-filename2 :format :text/plain)
(boxer::save-generic bin-box-to-save2 bin-box-filename2)
(boxer::save-generic zip-box-to-save2 zip-box-filename2)

(is (format nil "~A" (boxer::read-internal-1 text-box-filename2)) "#<DATA-BOX ABCD2 >")
(is (format nil "~A" (boxer::read-internal-1 bin-box-filename2)) "#<DATA-BOX EFG2 >")
(is (format nil "~A" (boxer::read-internal-1 zip-box-filename2)) "#<DATA-BOX HIJK2 >")

;; Unique filename tests
(is (boxer::untitled-filename (merge-pathnames "data/unique-filenames/test1/"
                                      (make-pathname :directory (pathname-directory *load-truename*)))) "Untitled.box")
(is (boxer::untitled-filename (merge-pathnames "data/unique-filenames/test2/"
                                      (make-pathname :directory (pathname-directory *load-truename*)))) "Untitled 3.box")

)



(finalize)
