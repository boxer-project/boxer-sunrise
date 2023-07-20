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

)

(finalize)
