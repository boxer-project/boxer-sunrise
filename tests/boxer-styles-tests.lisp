(in-package :boxer-sunrise-test)

(plan nil)

;;; Tests for rgb->rgb-hex
(is (boxer::rgb->rgb-hex #(:RGB 0.0D0 0.02345961332321167D0 0.9932165145874023D0 1.0D0))
    #(:rgb-hex "#0005FD") :test #'equalp)

;;; Tests for setting and getting css values from plist-subclasses. In this case box.
(let ((box (make-simple-box "CSS Test" "This is a box.")))
  ;; Starting out there should be no property
  (is (boxer::getprop box :css-styles nil) nil)

  (boxer::set-css-style box :background-color '#(:rgb-hex "#FFEEDD"))
  (is (length (boxer::getprop box :css-styles nil)) 2)
  (is (boxer::get-css-style box :background-color) '#(:rgb-hex "#FFEEDD") :test #'equalp)
  (is (boxer::get-css-style box :border-color) nil)


  (boxer::set-css-style box :border-color '#(:rgb-hex "#AABBCC"))
  (is (length (boxer::getprop box :css-styles nil)) 4)
  (is (boxer::get-css-style box :background-color) '#(:rgb-hex "#FFEEDD") :test #'equalp)
  (is (boxer::get-css-style box :border-color) '#(:rgb-hex "#AABBCC") :test #'equalp)


  (boxer::set-css-style box :background-color '#(:rgb-hex "#001122"))
  (is (length (boxer::getprop box :css-styles nil)) 4)
  (is (boxer::get-css-style box :background-color) '#(:rgb-hex "#001122") :test #'equalp)
  (is (boxer::get-css-style box :border-color) '#(:rgb-hex "#AABBCC") :test #'equalp)

  (boxer::remove-css-style box :border-color)
  (is (length (boxer::getprop box :css-styles nil)) 2)
  (is (boxer::get-css-style box :background-color) '#(:rgb-hex "#001122") :test #'equalp)
  (is (boxer::get-css-style box :border-color) nil)
)



; todo the below are important for the new border/background color work. FINISH THEM
; (let ((new-box (boxer::make-initialized-box-for-editor))
;       (copied-box (boxer::make-initialized-box-for-editor)))
;     ;; without the style plist entries
;     (setf (boxer::name new-box) "A test Box")

;     (is (boxer::name new-box) "A test Box")

;     (setf copied-box (boxer::copy-box new-box))

;     (is (boxer::name copied-box) "A test Box")

;     (setf (boxer::name copied-box) "A copied Box")

;     (is (boxer::name new-box) "A test Box")
;     (is (boxer::name copied-box) "A copied Box")



;     ;; tests to make sure the styles are copied using our copy hooks
;     (setf (getf (boxer::plist new-box) 'boxer::border-background) "#123456")

;     (is (getf (boxer::plist new-box) 'boxer::border-background) "#123456")
;     (is (getf (boxer::plist copied-box) 'boxer::border-background) nil)

;     (setf copied-box (boxer::copy-box new-box))
;     (is (getf (boxer::plist new-box) 'boxer::border-background) "#123456")
;     (is (getf (boxer::plist copied-box) 'boxer::border-background) "#123456")

;     (format t "the plist: ~A" (boxer::plist new-box))
;     (format t "the plis2: ~A" (boxer::plist copied-box))

; )




(finalize)
