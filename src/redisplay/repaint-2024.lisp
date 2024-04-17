(in-package :boxer)

(defmethod dimensions ((self screen-row) &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
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
                   baseline (max baseline (cha-ascent))
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

                   (set-screen-obj-offsets inf-screen-obj inf-x-offset inf-y-offset)

                   (multiple-value-bind (row-width row-height)
                     (dimensions inf-screen-obj)
                     (setf inf-y-offset (+ inf-y-offset row-height)
                           infs-new-wid (max infs-new-wid row-width)
                           infs-new-hei (+ infs-new-hei row-height)))))))))))

(defmethod dimensions ((self screen-box) &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  (with-slots (wid hei actual-obj scroll-to-actual-row box-type content-wid content-hei) self
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
       (t
         (set-display-style self :normal)
         (when (neq box-type new-box-type)
           (setq box-type new-box-type))
         (multiple-value-bind (l-border-wid t-border-wid r-border-wid b-border-wid)
                              (box-borders-widths new-box-type self)
           (multiple-value-bind (internal-wid internal-hei)
                                (internal-dimensions self l-border-wid t-border-wid)
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
               (setf (screen-obj-y-got-clipped? self) (> (- internal-hei 50) hei)) ;; sgithens - just hacking values for now

               (log:debug "~% Case regular: wid: ~A hei: ~A" wid hei)
          )))
       )))
    (values wid hei)))

(defmethod dimensions ((self sprite-screen-box) &optional (first-inf-x-offset 0) (first-inf-y-offset 0))
  )

(defun update-outer-scrollbars (screen-box frame)
  ;; A few things.
  ;; 1. If the outermost screen box fits on the entire canvas (either way) then that scroll-bar should
  ;;    not be enabled.
  ;; 2. After that we need to look the total size of the outermost screen-box and work our slug sizes to that

  ;; Vertical scrolling
  (let* ((pane-hei         (capi:simple-pane-visible-height (slot-value *boxer-frame* 'bw::outer-vertical-scroll)))
         (box-hei          (screen-obj-hei screen-box))
         ;; The slug size is the proportion: (pane-hei / content) * pane-hei = slug-size
         ;; pane-hei here is the height of the scroll-bar pane. Content is the size of the GL canvas, ie. outer-screen-box full size
         (vert-slug-size   (* (/ pane-hei box-hei) pane-hei)))
    ;; (format t "~%2pane-hei: ~A box-hei: ~A %vert-slug-size ~A" pane-hei box-hei vert-slug-size)
    (capi:range-set-sizes (slot-value *boxer-frame* 'bw::outer-vertical-scroll)
                          :start 0
                          :end (+ 40 (* (zoom-level *boxer-pane*) box-hei))
                          :slug-start (- (vertical-scroll *boxer-pane*))
                          :slug-end (+ vert-slug-size (- (vertical-scroll *boxer-pane*)))
                          :redisplay t)
    )

  ;; Horizontal Scrolling
  (let* ((pane-wid         (capi:simple-pane-visible-width (slot-value *boxer-frame* 'bw::outer-horizontal-scroll)))
         (zoom             (zoom-level *boxer-pane*))
         (box-wid          (* zoom (screen-obj-wid screen-box)))
         ;; The slug size is the proportion: (pane-wid / content) * pane-wid = slug-size
         ;; pane-wid here is the width of the scroll-bar pane. Content is the size of the GL canvas, ie. outer-screen-box full size
         (horiz-slug-size   (* (/ pane-wid box-wid) pane-wid))
         (end (+ -40 (* zoom box-wid)))
         (slug-start (- (horizontal-scroll *boxer-pane*)))
         (slug-end (+ horiz-slug-size (- (horizontal-scroll *boxer-pane*)))))

    (format t "~%2update horiz scroll: pane-wid: ~A box-wid: ~A
  start: 0 end: ~A slug-start: ~A slug-end: ~A" pane-wid box-wid end slug-start (float slug-end))

      (capi:range-set-sizes (slot-value *boxer-frame* 'bw::outer-horizontal-scroll)
                          :start 0
                          :end end
                          :slug-start slug-start
                          :slug-end slug-end
                          :redisplay t)))

(defun repaint-fill-dimensions (outer-screen-box pane-width pane-height)
  (multiple-value-bind (wid hei) (dimensions outer-screen-box)
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

    (capi:apply-in-pane-process *boxer-frame*
      (lambda () (update-outer-scrollbars outer-screen-box *boxer-frame*)))))
