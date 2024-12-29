(in-package :boxer-sunrise-test)

(plan nil)

(let ((boxer::%drawing-height      200)
      (boxer::%drawing-width       300)
      (boxer::%drawing-half-height 100)
      (boxer::%drawing-half-width  150))

(is (boxer-wrap::wrap-y-coord-top 150) -50)
(is (boxer-wrap::wrap-y-coord-bottom -125) 75)
(is (boxer-wrap::wrap-x-coord-left -175) 125)
(is (boxer-wrap::wrap-x-coord-right 200) -100)

(ok (boxer-wrap::beyond-top? 100.1))
(ok (null (boxer-wrap::beyond-top? 100)))
(ok (null (boxer-wrap::beyond-top? -24)))

(ok (null (boxer-wrap::beyond-bottom? 34)))
(ok (null (boxer-wrap::beyond-bottom? -100.0)))
(ok (boxer-wrap::beyond-bottom? -100.1))

(ok (boxer-wrap::beyond-left? -150.1))
(ok (null (boxer-wrap::beyond-left? -150)))
(ok (null (boxer-wrap::beyond-left? 23)))

(ok (boxer-wrap::beyond-right? 150.1))
(ok (null (boxer-wrap::beyond-right? 150)))
(ok (null (boxer-wrap::beyond-right? -24)))

(is (boxer-wrap::top-x-intercept 100 200  2) 50 "x-intercept 1")
(is (boxer-wrap::top-x-intercept   0 250 -2) 75 "x-intercept 2")

(is (boxer-wrap::bottom-x-intercept -50 -250  2)  25)
(is (boxer-wrap::bottom-x-intercept -50 -150 -2) -75)

(is (boxer-wrap::left-y-intercept -200 -50 1/3) -100/3)

(is (boxer-wrap::right-y-intercept 250 125 75/50) -25)

(let ((draw-count 0))
  (labels ((test-draw-line (x0 y0 x1 y1)
             (progn
               (format t "~%test-draw-line: x0: ~A y0: ~A x1: ~A y1: ~A" x0 y0 x1 y1)
               (incf draw-count))
             ))
    (boxer-wrap::draw-wrap-line 0 0 10 10 #'test-draw-line)
    (is draw-count 1 "Simple wrap line")

    ;; I believe this test will exhaust the stack
    (setf boxer::%drawing-height 451)
    (setf boxer::%drawing-width 334)
    (setf boxer::%drawing-half-width 225.5)
    (setf boxer::%drawing-half-height 167.0)
    (boxer-wrap::draw-wrap-line 225.5 144.136 225.5 144.136 #'test-draw-line)
    (is 1 1 "2nd case")

    ;; Stack  overflow from dragging CATT with pen segments
    (setf boxer::%drawing-height 462)
    (setf boxer::%drawing-width 787)
    (setf boxer::%drawing-half-width 393.5)
    (setf boxer::%drawing-half-height 231.0)
    (boxer-wrap::draw-wrap-line -393.5 342.0 -410.5 343.0 #'test-draw-line)
    (is 1 1 "3rd case")

    (setf boxer::%drawing-height 434)
    (setf boxer::%drawing-width 861)
    (setf boxer::%drawing-half-width 430.5)
    (setf boxer::%drawing-half-height 217.0)
    (boxer-wrap::draw-wrap-line -227.5 215.5 -233.5 217.5 #'test-draw-line)
    (is 1 1 "4th case")

    ;; Division by zero in top-x-intercept
    (setf boxer::%drawing-height 394)
    (setf boxer::%drawing-width 484)
    (setf boxer::%drawing-half-width 242.0)
    (setf boxer::%drawing-half-height 197.0)
    (boxer-wrap::draw-wrap-line -277.5 399.5 -297.5 399.5 #'test-draw-line)
    (is 1 1 "5th case")

)))

(finalize)
