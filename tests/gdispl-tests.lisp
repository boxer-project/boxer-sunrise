(in-package :boxer-sunrise-test)

(plan nil)

;; Tests for allocate-window->boxer-command


  (is  (boxer::allocate-window->boxer-command #(4 #(:RGB 1.0 0.0 0.0 1.0)))
       #(36 #(:RGB 1.0 0.0 0.0 1.0)) :test #'equalp)

  (is  (boxer::allocate-window->boxer-command #(36 #(:RGB 1.0 0.0 0.0 1.0)))
       #(36 #(:RGB 1.0 0.0 0.0 1.0)) :test #'equalp)

(let ((boxer::%drawing-half-width 50)
      (boxer::%drawing-half-height 25)
      (bitmap-before #(15 "blah" 40 30 20 50))
      (bitmap-after nil))
  ;; need to test: x-transform, y-transform, coerce, nil
  ;; boxer-centered-bitmap contains all 3
  ;; '(nil :x-transform :y-transform :coerce :coerce)
  ;; Result should be: #(47 "blah" 40 30 20.0 50.0)
;;   (setf bitmap-after (boxer::allocate-window->boxer-command #(15 "blah" 40 30 20 50)))
  (is  (boxer::allocate-window->boxer-command #(15 "blah" 40 30 20 50))
       #(47 "blah" -10.0 -5.0 20.0 50.0) :test #'equalp)

  )


(finalize)
