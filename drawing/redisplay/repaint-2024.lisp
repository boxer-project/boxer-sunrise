(in-package :boxer)

(defvar *box-depth-mult* 1)

(defmethod update-scene-graph ((self screen-obj) parent x-offset y-offset depth)
  (setf (local-matrix self) (create-transform-matrix x-offset y-offset (* depth *box-depth-mult*)))
  (setf (local-internal-matrix self) (create-transform-matrix x-offset y-offset (+ .5 (* depth *box-depth-mult*))))
  (setf (world-matrix self) (3d-matrices:m* (world-internal-matrix parent) (local-matrix self)))
  (setf (world-x-offset self) (+ (world-x-offset parent) x-offset)
        (world-y-offset self) (+ (world-y-offset parent) y-offset))
  (setf (world-internal-matrix self) (3d-matrices:m* (world-internal-matrix parent) (local-internal-matrix self))))

(defmethod update-scene-graph ((self screen-box) parent x-offset y-offset depth)
  (setf (depth self) depth)
  (let ((x-internal-offset (+ x-offset (scroll-x-offset self)))
        (y-internal-offset (+ y-offset (scroll-y-offset self))))

    (when (eq (display-style self) :shrunk)
      (incf x-internal-offset 1)
      (incf y-internal-offset 1))

    (setf (local-matrix self) (create-transform-matrix x-offset y-offset (* depth *box-depth-mult*)))
    (setf (local-internal-matrix self) (create-transform-matrix x-internal-offset y-internal-offset (+ .5 (* depth *box-depth-mult*))))
    (setf (world-matrix self) (3d-matrices:m* (world-internal-matrix parent) (local-matrix self)))
    (setf (world-x-offset self) (+ (world-x-offset parent) x-internal-offset)
          (world-y-offset self) (+ (world-y-offset parent) y-internal-offset))
    (setf (world-internal-matrix self) (3d-matrices:m* (world-internal-matrix parent) (local-internal-matrix self)))))

(defmethod dimensions ((self screen-row) depth &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  (with-slots (wid hei actual-obj screen-chas baseline x-offset y-offset)
    self
    (let* ((infs-new-wid 0)
           (infs-new-hei 0)
          ;;  (new-baseline 0)
           (inf-x-offset first-inf-x-offset)
           (inf-y-offset first-inf-y-offset))
      ;; Watch out, this is a little flaky since we aren't going through the
      ;; normal deletion protocol for inferior screen boxes
      (clear-storage-vector screen-chas)

      (with-font-hacking ((row-fds actual-obj))
        ;; If the line is empty we still need to set the initial height
        (setf infs-new-hei (cha-hei))
        (setf baseline (cha-ascent))

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
                  inf-actual-obj
                  (let ((new-obj (allocate-screen-obj-for-use-in
                                  inf-actual-obj (lowest-screen-box self))))
                    (set-screen-obj-offsets new-obj infs-new-wid inf-y-offset)
                    ;; Scene graph hacking
                    (update-scene-graph new-obj self infs-new-wid inf-y-offset depth)

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
                   baseline (max baseline (cha-ascent))
                   inf-x-offset (+ inf-x-offset infs-new-wid))) ;; sgithens This may need to be 'wid' as well
            (t
             ;; must be a box so let the box do some work...
             ;; that is, redisplay if it wants to and then make its
             ;; contribution to all the infs-screen-objs parameters
             (multiple-value-bind (next-wid next-hei) (dimensions inf-screen-obj depth)
                (setf infs-new-wid (+   infs-new-wid next-wid)
                      infs-new-hei (max infs-new-hei next-hei)
                      inf-x-offset (+ inf-x-offset next-wid))))))) ;; sgithens This may need to be 'wid'
      (setf wid infs-new-wid
            hei infs-new-hei
            first-inf-x-offset 0
            first-inf-y-offset 0))
    (values wid hei)))

(defmethod internal-dimensions ((self graphics-screen-box) depth &optional
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
      (set-graphics-screen-sheet-x-offset screen-sheet first-inf-x-offset)
      (set-graphics-screen-sheet-y-offset screen-sheet first-inf-y-offset)
      (setf (screen-obj-actual-obj screen-sheet) graphics-sheet)
      (update-scene-graph (screen-sheet self) self first-inf-x-offset first-inf-y-offset depth))
    ;; make sure we have the Right graphics-sheet
    (unless (eq graphics-sheet
                (screen-obj-actual-obj (screen-sheet self)))
      (setf (screen-obj-actual-obj (screen-sheet self))
            graphics-sheet)
      )
    ;; error check, remove this SOON !!!!!!!
    (IF (NOT (GRAPHICS-SCREEN-SHEET? (SCREEN-ROWS SELF)))
        (BARF "The object ~S, inside of ~S is not a GRAPHICS-SHEET. "
              (SCREEN-ROWS SELF) SELF)
        (VALUES desired-wid desired-hei))))

(defmethod internal-dimensions ((self screen-box) depth &optional
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
               ;; The Actual Box is part of a circular structure AND
               ;; we have already displayed the port the required number
               ;; of times, so we erase and remove whatever is in
               ;; the box, then...
               (when (and (not-null screen-rows)
                         (not (box-ellipsis-style? screen-rows)))
                 (queue-for-deallocation-screen-rows-from screen-rows 0)
                 (kill-screen-rows-from self 0))
               ;; put a Box ellipsis marker into the
               ;; inferiors slot of the screen box
               (setq screen-rows *box-ellipsis-current-style*)
               ;; then return the necessary values
               (funcall (get *box-ellipsis-current-style* 'size)))
              (t
               ;; If the port has an ellipsis marker when it shouldn't,
               ;; then erase and remove it
               (when (and (port-box? actual-obj)
                          (box-ellipsis-style? screen-rows))
                 ;; PORTNOTE: still needed ?
                 (setq screen-rows (allocate-storage-vector 8)))

               ;; Here is the loop to accumulate the row dimensions
               (let ((infs-new-wid 0)
                     (infs-new-hei 0)
                     (inf-x-offset first-inf-x-offset)
                     (inf-y-offset first-inf-y-offset))

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
                    ;; If there are less rows than the last repaint we'll need to trim the extra
                    ;; screen-rows
                    (sv-delete-to-end screen-rows row-no)
                    (if (eq (display-style self) ':shrunk)
                      (values *shrunk-box-wid* *shrunk-box-hei* nil nil)
                      (values infs-new-wid infs-new-hei nil nil)))
                   ;; Step
                   (setf (slot-value inf-screen-obj 'actual-obj) inf-actual-obj)
                   (setf (screen-box inf-screen-obj) self)
                   (setf (slot-value inf-actual-obj 'screen-objs) (list (cons self inf-screen-obj)))

                   ;; These offsets are for screen rows
                   (set-screen-obj-offsets inf-screen-obj inf-x-offset inf-y-offset)
                   ;; Screen graph hacking
                   (update-scene-graph inf-screen-obj self inf-x-offset inf-y-offset depth)

                   (multiple-value-bind (row-width row-height)
                     (dimensions inf-screen-obj depth)
                     (setf inf-y-offset (+ inf-y-offset row-height)
                           infs-new-wid (max infs-new-wid row-width)
                           infs-new-hei (+ infs-new-hei row-height)))))))))))

(defmethod dimensions ((self screen-box) depth &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  ;; Increment for screen-box, but now for screen-rows
  (setf depth (1+ depth))

  (with-slots (wid hei actual-obj scroll-to-actual-row box-type content-wid content-hei scroll-y-offset) self
    (let ((new-box-type (class-name (class-of actual-obj)))
          (new-display-style (display-style actual-obj))
          (boxtop (boxtop actual-obj)))
      (log:debug "~% dimension screen-box: new-box-type: ~A new-display-style: ~A" new-box-type new-display-style)
      (cond ((and (eq new-display-style :supershrunk)
                  (not (eq self *outermost-screen-box*)))
             ;; Supershrunk
             (multiple-value-bind (sswid sshei)
                                  (super-shrunk-size)
                                  (set-display-style self :supershrunk)
                                  (setf wid sswid hei sshei)
                                  (log:debug "~% Case supershrunk: wid: ~A hei: ~A" wid hei)))
       ((and (eq new-display-style :shrunk)
              (not (eq self *outermost-screen-box*)))
        ;; If there is a boxtop
        (set-display-style self :shrunk)
        (cond ((not (null boxtop))
               (multiple-value-bind (btwid bthei) (boxtop-size boxtop actual-obj)
                                    (setf wid btwid hei bthei)
                                    (log:debug "~% Case Shrunk 1 (boxtop): wid: ~A hei: ~A" wid hei)))
              (t
               (multiple-value-bind (l-border-wid t-border-wid r-border-wid b-border-wid)
                                    (box-borders-widths new-box-type self)
                 (multiple-value-bind (min-wid min-hei)
                                      (box-borders-minimum-size new-box-type self)
                   (setf wid min-wid
                         hei (max min-hei (+ (cha-hei) t-border-wid b-border-wid)))
                   (log:debug "~% case Shrunk 2: wid: ~A hei: ~A" wid hei))))))
       ((graphics-screen-box? self)
        ;; Currently graphics boxes never get clipped and always take their full canvas size
        (set-display-style self :normal)
        (when (neq box-type new-box-type)
          (setq box-type new-box-type))
        (multiple-value-bind (l-border-wid t-border-wid r-border-wid b-border-wid)
                             (box-borders-widths new-box-type self)
          (multiple-value-bind (internal-wid internal-hei)
                               (internal-dimensions self depth l-border-wid t-border-wid)
            (multiple-value-bind (min-wid min-hei)
                                 (box-borders-minimum-size new-box-type self)
              (setf wid         (max min-wid (+ internal-wid l-border-wid r-border-wid))
                    hei         (max min-hei (+ internal-hei t-border-wid b-border-wid))
                    content-wid internal-wid
                    content-hei internal-hei)))))
       (t
         (set-display-style self :normal)
         (when (neq box-type new-box-type)
           (setq box-type new-box-type))
         (multiple-value-bind (l-border-wid t-border-wid r-border-wid b-border-wid)
                              (box-borders-widths new-box-type self)
           (multiple-value-bind (internal-wid internal-hei)
                                (internal-dimensions self depth l-border-wid t-border-wid)
             (multiple-value-bind (min-wid min-hei)
                                  (box-borders-minimum-size new-box-type self)
               (setf wid         (max min-wid (+  internal-wid l-border-wid r-border-wid))
                     hei         (max min-hei (+ internal-hei t-border-wid b-border-wid))
                     content-wid internal-wid
                     content-hei internal-hei)
               (when (fixed-size? actual-obj)
                 (multiple-value-bind (fixed-wid fixed-hei) (fixed-size actual-obj)
                   (setf wid (max min-wid fixed-wid)
                         hei (max min-hei fixed-hei))))

               (setf (screen-obj-x-got-clipped? self) (> internal-wid wid))
               (setf (screen-obj-y-got-clipped? self) (> (- internal-hei (* *border-inside-space* 2)) (- hei t-border-wid b-border-wid (* *border-inside-space* 2)))) ;; sgithens TODO These calculations need to come from our new Boxer Box Model

               ;; Update the y-offset based on a scroll-to-actual-row adjustment.
               ;;
               ;; Should we be able to set a scroll row if we're not using scroll bars at all? I believe in the
               ;; old repaint algorithm, it would add scroll bars, if there hadn't been any yet.
               (when (and (screen-obj-y-got-clipped? self) scroll-to-actual-row)
                 (let* ((scrolled-to-scr-row (cdar (actual-obj-screen-objs scroll-to-actual-row)))
                        (row-y-offset (screen-obj-y-offset scrolled-to-scr-row)))
                   (setf scroll-y-offset (- (- row-y-offset (baseline scrolled-to-scr-row) *border-inside-space*) ))
                   (setf scroll-to-actual-row nil)))

               (log:debug "~% Case regular: wid: ~A hei: ~A" wid hei)
          )))
       )))
    (values wid hei)))

(defmethod dimensions ((self sprite-screen-box) depth &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  )

(defun repaint-fill-dimensions (outer-screen-box pane-width pane-height)
  (setf (local-matrix outer-screen-box) (create-transform-matrix 9 9)) ;  (* depth *box-depth-mult*)))
  (setf (local-internal-matrix outer-screen-box) (create-transform-matrix 9 9)) ; (+ .5 (* depth *box-depth-mult*))))
  (setf (world-matrix outer-screen-box) (3d-matrices:m* (3d-matrices:meye 4) (local-matrix outer-screen-box)))
  (setf (world-x-offset outer-screen-box) 9
        (world-y-offset outer-screen-box) 9)
  (setf (world-internal-matrix outer-screen-box) (3d-matrices:m* (3d-matrices:meye 4) (local-internal-matrix outer-screen-box)))

  (multiple-value-bind (wid hei) (dimensions outer-screen-box 1)
    (log:debug "~% repaint-fill-dimensions: wid: ~A hei: ~A pane-width: ~A pane-height: ~A"
      wid hei pane-width pane-height)
    (setf (screen-obj-wid outer-screen-box) (if (eq (view-layout *boxer-pane*) :canvas-view)
                                              (first (page-size *boxer-pane*))
                                              (max pane-width wid))
          (screen-obj-hei outer-screen-box) (if (eq (view-layout *boxer-pane*) :canvas-view)
                                              (second (page-size *boxer-pane*))
                                              (max pane-height hei))
          (content-hei bw::*boxer-pane*)    (max pane-height hei)
          (content-wid bw::*boxer-pane*)    (max pane-width wid))

    #+lispworks
    (capi:apply-in-pane-process *boxer-frame*
      (lambda () (bw::update-outer-scrollbars outer-screen-box *boxer-frame*)))))
