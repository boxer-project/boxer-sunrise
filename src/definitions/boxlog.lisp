;; This is a temporary workaround for ecl/emscripten which does not
;; support bordeaux-threads and anything that depends on it like cl-fad,
;; log4cl, etc

(defun log:config (level)
  nil)

(defun log:debug (msg &rest params)
  nil)

(defun log:info (msg &rest params)
  nil)

(defun log:error (msg &rest params)
  nil)

(defun log:warn (msg &rest params)
  nil)
