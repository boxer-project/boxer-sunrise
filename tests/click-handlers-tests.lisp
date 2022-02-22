(in-package :boxer-sunrise-test)

(plan nil)

;; Currently wander is set to 5 pixels

(is (bw::check-click-wander '(0 . 0) '(0 . 0)) t)

(is (bw::check-click-wander '(2 . 3) '(4 . 5)) t)

(is (bw::check-click-wander '(0 . 0) '(4 . 4)) t)

(is (bw::check-click-wander '(0 . 0) '(5 . 5)) nil)

(is (bw::check-click-wander '(0 . 0) '(6 . 6)) nil)

(is (bw::check-click-wander '(2 . 3) '(8 . 3)) nil)

(finalize)
