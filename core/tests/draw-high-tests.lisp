(in-package :boxer-sunrise-test)

(plan nil)

(let* ((clip-stack-1 '((16   29 628 407)))
       (clip-stack-2 '((16   29 628 407)
                       (34 -425 689 693))))
  (is (boxer::calculate-clip-rectangle clip-stack-1)
      '(16 29 628 407)
      "Clipping rectangle 1 level deep")
  (is (boxer::calculate-clip-rectangle clip-stack-2)
      '(34 29 628 407)
      "Clipping rectangle 2 levels deep"))

(finalize)
