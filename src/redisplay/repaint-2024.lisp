(in-package :boxer)

(defmethod dimensions ((self screen-row) &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  (with-slots (wid hei actual-obj screen-chas baseline x-offset y-offset)
    self
    (let* ((infs-new-wid 0)
           (infs-new-hei 0)
           (new-baseline 0)
           (inf-x-offset first-inf-x-offset)
           (inf-y-offset first-inf-y-offset))
      ;; Watch out, this is a little flaky since we aren't going through the
      ;; normal deletion protocol for inferior screen boxes
      (clear-storage-vector screen-chas)

      (with-font-hacking ((row-fds actual-obj))
        ;; If the line is empty we still need to set the initial height
        (setf infs-new-hei (cha-hei))

        (do* ((cha-no 0 (+ cha-no 1))
              (inf-actual-obj (cha-at-cha-no actual-obj cha-no)
                              (cha-at-cha-no actual-obj cha-no))
              (inf-screen-obj))

          ;; do* check /return
          ((null inf-actual-obj)
           (cond ((null inf-actual-obj)
                  (values infs-new-wid infs-new-hei))))

          ;; do* statements
          ;; handle any font changes first
          (check-and-handle-font-changes cha-no)
          ;; now match screen and editor...
          (setq inf-screen-obj
                (if (cha? inf-actual-obj)
                  (make-screen-cha inf-actual-obj)
                  (let ((new-obj (allocate-screen-obj-for-use-in
                                  inf-actual-obj (lowest-screen-box self))))
                    (set-screen-obj-offsets new-obj inf-x-offset inf-y-offset)
                    new-obj)))
          (append-screen-cha self inf-screen-obj)
          ;; At this point we know that inf-screen-obj and inf-actual-obj
          ;; match. If it wants to (and is a screen-box) let inf-screen-obj do
          ;; redisplay-pass-1.
          (cond
            ((screen-cha? inf-screen-obj)
             ;; must be a screen cha so the ROW has to check for clipping
             ;; and increment its own infs-screen-objs parameters
             (setf infs-new-wid (+   infs-new-wid (cha-wid inf-screen-obj))
                   infs-new-hei (max infs-new-hei (cha-hei))
                   baseline (max new-baseline (cha-ascent))
                   inf-x-offset (+ inf-x-offset wid)))
            (t
             ;; must be a box so let the box do some work...
             ;; that is, redisplay if it wants to and then make its
             ;; contribution to all the infs-screen-objs parameters
             (multiple-value-bind (next-wid next-hei) (dimensions inf-screen-obj)
                (setf infs-new-wid (+   infs-new-wid next-wid)
                      infs-new-hei (max infs-new-hei next-hei)
                      inf-x-offset (+ inf-x-offset wid))
             )

            ))))
      (setf wid infs-new-wid
            hei infs-new-hei
            first-inf-x-offset 0
            first-inf-y-offset 0))
    (values wid hei)))

(defmethod internal-dimensions ((self graphics-screen-box) &optional
                                                           (first-inf-x-offset 0) (first-inf-y-offset 0) ignore)
  (let* ((graphics-sheet (graphics-sheet (screen-obj-actual-obj self)))
         (desired-wid (graphics-sheet-draw-wid graphics-sheet))
         (desired-hei (graphics-sheet-draw-hei graphics-sheet)))
    ;; first make-sure that there is a screen object for the graphics sheet
    (when (not (graphics-screen-sheet? (screen-sheet self)))
      (let ((screen-sheet (allocate-screen-sheet-for-use-in graphics-sheet
                                                            self)))
        (set-screen-sheet self screen-sheet)))
    (let ((screen-sheet (screen-sheet self)))
      ;; now adjust the slots of the graphics-screen-sheet
      (unless (= first-inf-x-offset
                 (graphics-screen-sheet-x-offset screen-sheet))
        (set-graphics-screen-sheet-x-offset screen-sheet first-inf-x-offset))
      (unless (= first-inf-y-offset
                 (graphics-screen-sheet-y-offset screen-sheet))
        (set-graphics-screen-sheet-y-offset screen-sheet
                                            first-inf-y-offset))
      (setf (graphics-screen-sheet-actual-obj screen-sheet) graphics-sheet))
    ;; make sure we have the Right graphics-sheet
    (unless (eq graphics-sheet
                (graphics-screen-sheet-actual-obj (screen-sheet self)))
      (setf (graphics-screen-sheet-actual-obj (screen-sheet self))
            graphics-sheet)
      )
    ;; error check, remove this SOON !!!!!!!
    (IF (NOT (GRAPHICS-SCREEN-SHEET? (SCREEN-ROWS SELF)))
        (BARF "The object ~S, inside of ~S is not a GRAPHICS-SHEET. "
              (SCREEN-ROWS SELF) SELF)
        (VALUES desired-wid desired-hei))))

(defmethod internal-dimensions ((self screen-box) &optional
                                                  (first-inf-x-offset 0) (first-inf-y-offset 0) (scroll-to-inf nil))
  "This is like repaint inferiors
   This is the size of everything in the box without the borders"
  (with-slots (actual-obj box-type screen-rows scroll-x-offset scroll-y-offset) self
    (port-redisplaying-history (actual-obj)
      (multiple-value-bind (il it ir ib)
                           (box-borders-widths box-type self)
        (declare (ignore ir ib))
        (cond ((and (port-box? actual-obj)
                    (port-has-been-displayed-enough? actual-obj))
               ;; Port redisplayed enough
               ;; todo, go back and grab this bit
               )
              (t
               ;; If the port has an ellipsis marker when it shouldn't,
               ;; then erase and remove it
               (when (and (port-box? actual-obj)
                          (box-ellipsis-style? screen-rows))
                 ;; PORTNOTE: still needed ?
                 ;(funcall (get screen-rows 'erase-self) il it)
                 (setq screen-rows (allocate-storage-vector 8)))

               ;; Here is the loop to accumulate the row dimensions
               (let ((infs-new-wid 0)
                     (infs-new-hei 0)
                     (inf-x-offset first-inf-x-offset)
                     (inf-y-offset first-inf-y-offset)
                     )

                 ;; Make sure the screen-rows vector is large enough

                 (do*
                   ;; Vars
                   ((row-no 0 (1+ row-no))
                    (inf-actual-obj (or scroll-to-inf
                                        (first-inferior-row actual-obj))
                                    (next-row inf-actual-obj))
                    (inf-screen-obj (screen-row-at-row-no self row-no inf-actual-obj)
                                    (screen-row-at-row-no self row-no inf-actual-obj)))
                   ;; End / Result
                   ((or (null inf-actual-obj)
                        (eq (display-style self) ':shrunk))
                    (if (eq (display-style self) ':shrunk)
                      (values *shrunk-box-wid* *shrunk-box-hei* nil nil)
                      (values infs-new-wid infs-new-hei nil nil)))
                   ;; Step
                   (setf (slot-value inf-screen-obj 'actual-obj) inf-actual-obj)
                   (setf (screen-box inf-screen-obj) self)
                   (setf (slot-value inf-actual-obj 'screen-objs) (list (cons self inf-screen-obj)))
                   ;;  (break "what...")
                   (set-screen-obj-offsets inf-screen-obj inf-x-offset inf-y-offset)
                   ;; fixing this up now...
                   (multiple-value-bind (row-width row-height)
                     (dimensions inf-screen-obj)
                     (setf inf-y-offset (+ inf-y-offset row-height)
                           infs-new-wid (max infs-new-wid row-width)
                           infs-new-hei (+ infs-new-hei row-height)))
                 ))))))))

(defmethod dimensions ((self screen-box) &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  (with-slots (wid hei actual-obj scroll-to-actual-row box-type) self
    (let ((new-box-type (class-name (class-of actual-obj)))
          (new-display-style (display-style actual-obj))
          (boxtop (boxtop actual-obj)))
      (format t "~% dimension screen-box: new-box-type: ~A new-display-style: ~A" new-box-type new-display-style)
      (cond ((and (eq new-display-style :supershrunk)
                  (not (eq self *outermost-screen-box*)))
             ;; Supershrunk
             (multiple-value-bind (sswid sshei)
                                  (super-shrunk-size)
                                  (set-display-style self :supershrunk)
                                  (setf wid sswid hei sshei)
                                  (format t "~% Case supershrunk: wid: ~A hei: ~A" wid hei)
                                  ;; make sure to punt the inf screen objs or else they may try
                                  ;; and redisplay themselves (like after change-graphics)
                                  ;; (unless (graphics-screen-box? self)
                                  ;;   (rp1-sb-punt-extra-screen-objs self (first-screen-row self)))
                                    ))
       ((and (eq new-display-style :shrunk)
              (not (eq self *outermost-screen-box*))
              (not (null boxtop)))
        ;; If there is a boxtop
        (set-display-style self :shrunk)
        (multiple-value-bind (btwid bthei) (boxtop-size boxtop actual-obj)
                             (setf wid btwid hei bthei)
                             (format t "~% Case Shrunk: wid: ~A hei: ~A" wid hei)
                            ;;  (unless (graphics-screen-box? self)
                            ;;    (rp1-sb-punt-extra-screen-objs self (first-screen-row self)))
                               ))
       (t
         (set-display-style self new-display-style)
         (when (neq box-type new-box-type) (setq box-type new-box-type))
           (multiple-value-bind (l-border-wid t-border-wid r-border-wid b-border-wid)
                                (box-borders-widths new-box-type self)
             (multiple-value-bind (min-wid min-hei)
                                  (box-borders-minimum-size new-box-type self)
               (multiple-value-bind (internal-wid internal-hei)
                                    (internal-dimensions self l-border-wid t-border-wid)
                 (setf wid (max min-wid (+  internal-wid l-border-wid r-border-wid))
                       hei (max min-hei (+ internal-hei t-border-wid b-border-wid)))))
             (when (fixed-size? actual-obj)
               (multiple-value-bind (fixed-wid fixed-hei) (fixed-size actual-obj)
                 (setf wid fixed-wid
                       hei fixed-hei)))
            (format t "~% Case regular: wid: ~A hei: ~A" wid hei)
          )
       )))
    (values wid hei)))

(defmethod dimensions ((self sprite-screen-box) &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  )

(defun repaint-fill-dimensions (outer-screen-box pane-width pane-height)
  (multiple-value-bind (wid hei) (dimensions outer-screen-box)
    (format t "~% repaint-fill-dimensions: wid: ~A hei: ~A pane-width: ~A pane-height: ~A"
      wid hei pane-width pane-height)
    (setf (screen-obj-wid outer-screen-box) (max pane-width wid)
          (screen-obj-hei outer-screen-box) (max pane-height hei))
    (capi:set-horizontal-scroll-parameters bw::*boxer-pane*
      :max-range (+ 40 (screen-obj-wid outer-screen-box)) :min-range 0)
    (capi:set-vertical-scroll-parameters bw::*boxer-pane*
      :max-range (+ 40 (screen-obj-hei outer-screen-box)) :min-range 0)))
