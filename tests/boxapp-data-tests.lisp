(in-package :boxer-sunrise-test)

(plan nil)

;; Tests for Recent Files

;;;
;;; get-boxapp-filepath
;;;

(is (boxer::get-boxapp-data-filepath) #P"~/Library/Application Support/Boxer/boxapp-data.lisp" :test #'uiop:pathname-equal)

(boxer::reset-recent-files)

;; Starting out the recent files should be empty
(is (length (boxer::get-recent-files)) 0)

(boxer::add-recent-file "/Users/sgithens/first/file.txt")

(is (length (boxer::get-recent-files)) 1)

(is (cdr (assoc :path (car (boxer::get-recent-files)))) "/Users/sgithens/first/file.txt")

(boxer::add-recent-file "/Users/sgithens/second/file.txt")

(is (length (boxer::get-recent-files)) 2)

(boxer::add-recent-file "/Users/sgithens/third/file.box" "Third File")

(is (length (boxer::get-recent-files)) 3)

;; After adding second again, the order should be Second, Third, First

(boxer::add-recent-file "/Users/sgithens/second/file.txt")

(is (length (boxer::get-recent-files)) 3 "Testing double entries")

(finalize)
