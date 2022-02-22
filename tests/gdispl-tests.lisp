(in-package :boxer-sunrise-test)

(plan nil)

(is (boxer::ensure-legal-window-coordinate 10) 10)
(is (boxer::ensure-legal-window-coordinate (boxer::max-window-coord)) (boxer::max-window-coord))
(is (boxer::ensure-legal-window-coordinate (+ (boxer::max-window-coord) 1)) (boxer::max-window-coord))

(is (boxer::ensure-legal-window-coordinate -10) -10)
(is (boxer::ensure-legal-window-coordinate (boxer::min-window-coord)) (boxer::min-window-coord))
(is (boxer::ensure-legal-window-coordinate (- (boxer::min-window-coord) 1)) (boxer::min-window-coord))


(finalize)
