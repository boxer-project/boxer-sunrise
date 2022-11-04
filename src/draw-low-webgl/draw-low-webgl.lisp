(in-package :boxer)


(defclass boxer-resource (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'boxer-client))

(defclass boxer-client (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!"))))

(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room boxer-resource) user)
  (broadcast room "~a has joined ~a" (name user) (name room)))

(defmethod hunchensocket:client-disconnected ((room boxer-resource) user)
  (broadcast room "~a has left ~a" (name user) (name room)))

(defmethod hunchensocket:text-message-received ((room boxer-resource) user message)
  ; (broadcast room "~a says ~a" (name user) message)
  (format t "~%Got websocket text: ~A" message)
  (let* ((split-message (uiop:split-string message))
         (key-name (car split-message))
         (modifier (cadr split-message))
         (mod-code 0))
    ; (if (equal modifier "Control")
      ; (setf mod-code 1))
    (cond
    ;; https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
    ((equal (length message) 1)
     ;This must be a single unicode character."
    ;  (bw::queue-event (sys:make-gesture-spec (char-code (aref key-name 0)) mod-code))
     (bw::queue-event (sys:make-gesture-spec (char-code (aref message 0)) mod-code))

    )
    ((equal "Enter" key-name)
     (bw::queue-event (sys:make-gesture-spec (char-code #\return) 0))
    )
  ))
)

(defclass webgl-device (drawing-device) ())

(defmethod add-cha ((device webgl-device) char x y)
  (when (hunchentoot:started-p *boxer-server*)
    (broadcast *boxer-client* "[\"cha\", \"~a\", ~a, ~a]"
      char (+ %origin-x-offset x) (+ %origin-y-offset y)))
)

(defmethod add-circle ((device webgl-device) x y radius &optional filled?)
  (when (hunchentoot:started-p *boxer-server*)
    (broadcast *boxer-client* "[\"circle\", ~a, ~a, ~a]"
      radius (+ %origin-x-offset x) (+ %origin-y-offset y)))
)

(defmethod add-line ((device webgl-device) x0 y0 x1 y1)
  (when (hunchentoot:started-p *boxer-server*)
    (broadcast *boxer-client* "[\"line\", ~a, ~a, ~a, ~a]"
      (+ %origin-x-offset x0) (+ %origin-y-offset y0) (+ %origin-x-offset x1) (+ %origin-y-offset y1)) )
)

; (defmethod add-lines ((device webgl-device) &rest x-and-y-s)
;   (when (hunchentoot:started-p *boxer-server*)
;   (do* ((vertices (car x-and-y-s) (cddr vertices))
;           (x (car vertices)  (car vertices))
;           (y (cadr vertices) (cadr vertices)))
;       ((null y)
;       (unless (null x) ; both run out @ same time
;         (error "Unpaired vertex in ~A" x-and-y-s)))
;       (broadcast *boxer-client* "[\"line\", ~a, ~a, ~a, ~a]"
;         (+ %origin-x-offset x0) (+ %origin-y-offset y0) (+ %origin-x-offset x1) (+ %origin-y-offset y1))
;       (opengl:gl-vertex2-f (ogl-type x 'float) (ogl-type y 'float)))

;     (broadcast *boxer-client* "[\"line\", ~a, ~a, ~a, ~a]"
;       (+ %origin-x-offset x0) (+ %origin-y-offset y0) (+ %origin-x-offset x1) (+ %origin-y-offset y1)) )
; )

(defmethod add-string ((device webgl-device) font-no string x y)
  (when (hunchentoot:started-p *boxer-server*)
    (broadcast *boxer-client* "[\"string\", \"~a\", ~a, ~a]"
      string (+ %origin-x-offset x) (+ %origin-y-offset y)) )
)

(defmethod swap-buffers ((device webgl-device))
  (when (hunchentoot:started-p *boxer-server*)
    (broadcast *boxer-client* "[\"swap\"]"))
)

(defvar *boxer-client* (make-instance 'boxer-resource :name "/boxer"))

(defun find-room (request)
  ; (find (hunchentoot:script-name request) *chat-rooms* :test #'string= :key #'name)
  *boxer-client*
  )


(pushnew 'find-room hunchensocket:*websocket-dispatch-table*)
(defvar *boxer-server* (make-instance 'hunchensocket:websocket-acceptor :port 12345))

(defun make-webgl-device ()
  (hunchentoot:start *boxer-server*))

