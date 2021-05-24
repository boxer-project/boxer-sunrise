(in-package :boxer-sunrise2-test)

(plan nil)

(defparameter *empty-box* (make-instance 'boxer::data-box))
(let ((row (make-instance 'boxer::row)))
  (setf (boxer::chas-array row) (boxer::make-chas-array))
  (boxer::set-first-inferior-row *empty-box* row))

;; Each entry in this list will contain an associative list with the following keys:
;;  name - Name of test
;;  expected-eval - Expected Eval objects to be returned from chunk-row. This is the second
;;                  return value.
;;  input - what will become the chas-array of the row
(defparameter chunk-row-tests '(
  ((:name . "Just a row with the number 1")
   (:expected-eval . (1))
   (:input . "1"))
  ((:name . "Basic 1 + 2")
   (:expected-eval . (1 boxer-user::+ 2))
   (:input . "1 + 2"))
  ;; These are the original tests that used `defchunkertest`
  ((:name . "Symbol no spaces")
   (:expected-eval . (boxer-user::foobar))
   (:input . "foobar"))
  ((:name . "Symbol preceeding spaces")
   (:expected-eval . (boxer-user::foobar))
   (:input . "  foobar"))
  ((:name . "Symbol trailing spaces")
   (:expected-eval . (boxer-user::foobar))
   (:input . "foobar "))
  ((:name . "Symbol bounding spaces")
   (:expected-eval . (boxer-user::foobar))
   (:input . " foobar  "))
  ((:name . "2 symbols")
   (:expected-eval . (boxer-user::foo boxer-user::bar))
   (:input . "foo bar"))
  ((:name . "2 symbols (a)")
   (:expected-eval . (boxer-user::foo boxer-user::bar))
   (:input . "foo   bar"))
  ((:name . "2 symbols (b)")
   (:expected-eval . (boxer-user::foo boxer-user::bar))
   (:input . " foo bar "))

;; (boxer::defchunkertest "1 box" basic *test-box*)
;; (boxer::defchunkertest "2 boxes" basic *test-box* *test-box*)
;; (boxer::defchunkertest "2 boxes + space " basic *test-box* " " *test-box*)

;; (boxer::defchunkertest "box, symbol (a)" basic *test-box* " asd")
;; (boxer::defchunkertest "box, symbol (b)" basic *test-box* "asd")
;; (boxer::defchunkertest "symbol, box (a)" basic "asf " *test-box*)
;; (boxer::defchunkertest "symbol, box (b)" basic "asf" *test-box*)

  ((:name . "Beginning label no spaces")
   (:expected-eval . (boxer-user::bar))
   (:input . "Foo:bar"))
  ((:name . "Beginning label trailing space")
   (:expected-eval . (boxer-user::bar))
   (:input . "Foo: bar "))
  ((:name . "Beginning label preceding space")
   (:expected-eval . (boxer-user::bar))
   (:input . "Foo :bar "))
  ((:name . "Beginning Label all preceeding & trailing spaces")
   (:expected-eval . (boxer-user::bar))
   (:input . "Foo : Bar"))
  ((:name .  "Beginning Label Multiple spaces")
   (:expected-eval . (boxer-user::bar))
   (:input . " Foo  :  bar"))

  ((:name .  "List Test Option: Beginning Label Multiple spaces")
   (:expected-eval . (boxer-user::bar))
   (:input . (" Foo  : " " bar")))

  ;; ((:name . "Beginning Label, box element, no spaces")
  ;;  (:expected-eval . ())
  ;;  (:input . (" Foo:" *empty-box*)))
  ;; ((:name . )
  ;;  (:expected-eval . )
  ;;  (:input . ))
  ;; ((:name . )
  ;;  (:expected-eval . )
  ;;  (:input . ))

   ;; (boxer::DEFCHUNKERTEST  LABEL
;;   )
;; (boxer::DEFCHUNKERTEST "Beginning Label & box element" LABEL
;;   "Foo :" *test-box* " ")
;; (boxer::DEFCHUNKERTEST "Beginning label, box, multiple spaces" LABEL
;;   "foo  :  " *test-box*)
;; (boxer::DEFCHUNKERTEST "Trailing label" LABEL *test-box* " wow label:stuff")
;; (boxer::DEFCHUNKERTEST "middle label, box no spaces" LABEL
;;   "asd " *test-box* "label:stuff end")
;; (boxer::DEFCHUNKERTEST "Box label" LABEL "as " *test-box* " :stuff asd")
;; (boxer::DEFCHUNKERTEST "Box label & element" LABEL
;;   "asfda " *test-box* ":" *test-box* " end")

  ((:name . "Box label & element, multiple spaces")
   (:expected-eval . (boxer-user::end))
   (:input .  "foo   :   end"))
  ((:name . "Label with no element")
   (:expected-eval . (boxer-user::stuff))
   (:input . "stuff label: "))
  ((:name . "Colon, No label")
   (:expected-eval . (boxer-user::element))
   (:input . "  : element"))
  ((:name . "Initial colon")
   (:expected-eval . (boxer-user::element))
   (:input . ": element"))

  ((:name . "Comment no spaces")
   (:expected-eval . (boxer-user::foo))
   (:input . " foo;comment"))
  ((:name . "Label and comment")
   (:expected-eval . (boxer-user::asd boxer-user::wow))
   (:input . "asd label: wow ; and a comment"))

;; (boxer::DEFCHUNKERTEST "comment with box" COMMENT
;;   "a box ; in the " *test-box* " comment")

  ((:name . "Label & comment No spaces")
   (:expected-eval . (boxer-user::item))
   (:input . "label:item;comment"))
  ((:name . "Multiple properties, no spaces")
   (:expected-eval . (#(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::EVAL-IT
                        #(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::@ BOXER-USER::FOO))))
   (:input . "!@foo"))
  ((:name . "Multiple properties, multiple spaces")
   (:expected-eval . (#(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::EVAL-IT
                        #(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::@ BOXER-USER::WOW))))
   (:input . " !  @ wow "))
  ((:name . "Multiple properties, intervening label")
   (:expected-eval . (#(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::EVAL-IT
                        #(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::@ BOXER-USER::FOO))))
   (:input . "! label : @ foo "))
  ((:name . "Multiple properties with label")
   (:expected-eval . (#(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::EVAL-IT
                        #(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::@ BOXER-USER::STUFF))))
   (:input . "!@label:stuff"))
  ((:name . "Label, then multiple properties")
   (:expected-eval . (#(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::@
                        #(BOXER-EVAL::SPECIAL-EVAL-TOKEN BOXER-USER::EVAL-IT BOXER-USER::FOO))))
   (:input . "label : @! foo "))

   ((:name . "Beginning Label, box element, no spaces")
   (:expected-eval . ())
   (:input . (:empty-box)))
  ;; ((:name . )
  ;;  (:expected-eval . )
  ;;  (:input . ))
))

(defparameter pointers nil)
(defparameter eval-objects nil)

(defparameter basic-row (make-instance 'boxer::row))

(dolist (x chunk-row-tests)
  (setq basic-row (make-instance 'boxer::row))
  (setf (boxer::chas-array basic-row) (boxer::make-chas-array))
  (if (stringp (cdr (assoc :input x)))
    (map nil #'(lambda (c)
                (boxer::insert-cha-at-cha-no basic-row c 0))
            (reverse (cdr (assoc :input x))))
    (dolist (y (reverse (cdr (assoc :input x))))
      (cond ((stringp y) (map nil #'(lambda (c)
                (boxer::insert-cha-at-cha-no basic-row c 0))
            (reverse y)))
            ((eq y :empty-box) (boxer::insert-cha-at-cha-no basic-row (make-instance 'boxer::data-box) 0))
            (t (break "not handling this yet...")))
    ))
  (multiple-value-setq (pointers eval-objects)
    (boxer::chunk-row basic-row))
  (is eval-objects (cdr (assoc :expected-eval x)) (cdr (assoc :name x)) :test #'equalp)
  )

(finalize)
