(in-package :boxer-sunrise-test)

(plan nil)

(is (boxer::remove-shift-bit 0) 0)
(is (boxer::remove-shift-bit 1) 0)
(is (boxer::remove-shift-bit 2) 2)
(is (boxer::remove-shift-bit 3) 2)
(is (boxer::remove-shift-bit 4) 4)
(is (boxer::remove-shift-bit 5) 4)
(is (boxer::remove-shift-bit 6) 6)
(is (boxer::remove-shift-bit 7) 6)
(is (boxer::remove-shift-bit 8) 8)
(is (boxer::remove-shift-bit 9) 8)
(is (boxer::remove-shift-bit 10) 10)
(is (boxer::remove-shift-bit 11) 10)
(is (boxer::remove-shift-bit 12) 12)
(is (boxer::remove-shift-bit 13) 12)
(is (boxer::remove-shift-bit 14) 14)
(is (boxer::remove-shift-bit 15) 14)

;;; The tables `*default-mouse-click-name-translation-table*` and
;;; `*key-names*` are usually bound by now.

;; Check the #\a key (97 ascii as a safe example)
(is (symbol-name (boxer::lookup-key-name 97 0)) "A-KEY")
(is (symbol-name (boxer::lookup-key-name 97 1)) "SHIFT-A-KEY")
(is (symbol-name (boxer::lookup-key-name 97 2)) "CONTROL-A-KEY")

(is (symbol-name (boxer::lookup-click-name 0 0)) "MOUSE-CLICK")
(is (symbol-name (boxer::lookup-click-name 2 1)) "SHIFT-MOUSE-RIGHT-CLICK")

;;; Tests for lookup-input-name, which can take a character, number, mouse-event
;;; or gesture-spec

;; character
(is (symbol-name (boxer::lookup-input-name #\a)) "A-KEY")

;; number
(is (symbol-name (boxer::lookup-input-name 51)) "3-KEY")

;; mouse-event
;; This may require having more of the editor system spun up as it calculates boundaries
;; and click areas.
; (is (symbol-name (boxer::lookup-input-name (bw::make-mouse-event))) "MOUSE-CLICK")

;; gesture-spec
#+lispworks (is (symbol-name (boxer::lookup-input-name (sys:make-gesture-spec 97 0))) "A-KEY")

(finalize)
