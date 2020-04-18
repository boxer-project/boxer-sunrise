(in-package :boxer-sunrise2-test)

(plan nil)

(boxer-window::load-capogi-font-cache)
(is (length boxer-window::*capogi-font-cache*) 0 :test #'>)

; At the time writing there should be 4 major font families
(is (length boxer-window::*capogi-font-cache*) 4)



(finalize)
