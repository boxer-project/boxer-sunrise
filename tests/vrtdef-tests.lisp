(in-package :boxer-sunrise2-test)

(plan nil)

;; This test is absolutely critical to run regularly in
;; the number of fields in the vc-rows-entry structure
;; ever changes. It's pretty wierd, see comments on
;; vc-rows-entry and vc-rows-entry? in vrtdef.lisp
;;
;; Basically the underlying struct is setup to be stored
;; as a plain vector, and the predicate we have essentially
;; depends on the length of this simple vector.
(is (length (boxer::%make-vc-rows-entry)) 10)


(let ((vcr (boxer::%make-vc-rows-entry)))
  (is t   (boxer::vc-rows-entry? vcr))
  (is nil (boxer::vc-rows-entry? (subseq vcr 0 9))))

(finalize)
