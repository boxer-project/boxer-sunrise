;; Unit tests for various files of our openGL shaders work
(in-package :boxer-sunrise-test)

(plan nil)

(let ((fvec (boxer::float-vector 0.0 4 5 1.2 0 23 19)))
  (is fvec #(0.0 4 5 1.2 0 23 19) :test #'equalp)
  (map 'vector (lambda (it) (ok (floatp it))) fvec)
)

(finalize)
