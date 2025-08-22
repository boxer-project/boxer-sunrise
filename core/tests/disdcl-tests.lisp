(in-package :boxer-sunrise-test)

(plan nil)

(let ((test-sb (make-instance 'boxer::screen-box)))

  ;; test to make sure the accessors always deal in fixnums regardless of
  ;; what might get jammed in the slot
  (setf (slot-value test-sb 'boxer::wid) 4.0)
  (is (boxer::screen-obj-wid test-sb) 4)
  (ok (integerp (boxer::screen-obj-wid test-sb)))
  (ok (not (floatp (boxer::screen-obj-wid test-sb))))

  (setf (boxer::screen-obj-wid test-sb) 5.2)
  (is (boxer::screen-obj-wid test-sb) 5)
  (ok (integerp (boxer::screen-obj-wid test-sb)))
  (ok (not (floatp (boxer::screen-obj-wid test-sb))))

  (setf (slot-value test-sb 'boxer::hei) 4.0)
  (is (boxer::screen-obj-hei test-sb) 4)
  (ok (integerp (boxer::screen-obj-hei test-sb)))
  (ok (not (floatp (boxer::screen-obj-hei test-sb))))

  (setf (boxer::screen-obj-hei test-sb) 5.2)
  (is (boxer::screen-obj-hei test-sb) 5)
  (ok (integerp (boxer::screen-obj-hei test-sb)))
  (ok (not (floatp (boxer::screen-obj-hei test-sb))))
)

(finalize)
