(in-package :boxer-sunrise-test)

(plan nil)

(let ((atlas (make-instance 'boxer::glyph-atlas)))
    (is (boxer::closest-font-size atlas 15)
        16 "Closest glyph-atlas size: 15 -> 16")
    (is (boxer::closest-font-size atlas 12)
        12 "Closest glyph-atlas size: 12 -> 12")
    (is (boxer::closest-font-size atlas 7)
        8 "Closest glyph-atlas size: 7 -> 8")
    (is (boxer::closest-font-size atlas 73)
        288 "Closest glyph-atlas size: 73 -> 288")
    (is (boxer::closest-font-size atlas 289)
        288 "Closest glyph-atlas size: 289 -> 288"))

(finalize)
