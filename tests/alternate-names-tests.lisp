(in-package :boxer-sunrise-test)

(plan nil)

#+mac
(progn
  (is (boxer::alternate-platform-input-names 97 2 :platform :mac) '(boxer-user::ctrl-a-key))
  (is (boxer::alternate-platform-input-names 97 4 :platform :mac) '(boxer-user::alt-a-key))
)

#+win32
(progn
  (is (boxer::alternate-platform-input-names 97 2 :platform :ibm-pc) '(boxer-user::control-a-key))
  (is (boxer::alternate-platform-input-names 97 4 :platform :ibm-pc) '(boxer-user::s))
)

(finalize)
