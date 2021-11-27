;; -*- Mode:LISP;Syntax: Common-Lisp; Package:BOXER;-*-
#|


 $Header$

 $Log$


 Copyright 1996 - 2000 Regents of the University of California

 Enhancements and Modifications Copyright 2004 Pyxisystems LLC


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    Landscapes...

TODO: 
1) cache parsed palettes on boxes for fast palette swapping


Modification History (most recent at top)

 7/10/04 updated landscapes for OSX boxer, looks like pixels still need device
         independency work
 1/03/00 fixed array rank reversal in write-text-file-from-landscape
12/20/99 added write-text-file-from-landscape
11/01/99 foreign-data-set: changed svref& to svref
 7/15/98 more out of bounds support for foreign-data-set, virtual-copy-foreign-data
 7/14/98 add out of bounds support for landscape virtual-copy-rows methods
 7/09/98 fixed typo in ls-redisplay
 6/11/98 changed default-landscape-display to use truncate instead of round when 
         computing palette index
 5/27/98 make-landscape-slot-descriptor hacks possible number boxes
         fixed set-ls/landscape-zoom, mouse-patch-coords to give boxer error
 5/26/98 fixed landscape-redisplay to give boxer error instead of blowing out.
 5/25/98 changed patch-rc and patch-xy-position from explicit patch arg to ASK syntax
 5/22/98 print-vc-landscape-hook: make printed landscape point to the newly containing box
         also add the patch prims cause they won't be copied normally, also hookup
         the patch-row to the new box
         copy-landscape makes new patch-row for uniqueness and clears the
         editor-structure-cache
 5/19/98 better error reporting for PATCH
 5/18/98 removed <landscape> arg from landscape-redisplay, ls-zoom ,set-ls-zoom
         <landscape> is determined implicitly by scoping, added landscape-, ls- synonyms
        
 5/18/98 started logging changes: source = landscape v0.7

|#

#-(or mcl lispm) (in-package 'boxer :use '(lisp) :nicknames '(box))
#+mcl            (in-package :boxer)


(declare-boxer-extension "Landscapes" :version 1.5 :comments "Highly Experimental")

;****************************************************************
;;; Landscape proper starts here...

(defstruct (landscape-slot-descriptor (:conc-name lsd-)
                                      (:constructor
                                       %make-landscape-slot-descriptor)
                                      (:print-function print-landscape-slot-descriptor))
  name
  (default 0)
  (min 0)
  (max 100))

(defun print-landscape-slot-descriptor (lsd stream level)
  (declare (ignore level))
  (format stream "#<LSD ~S (~D)  ~D ==> ~D >" 
          (lsd-name lsd) (lsd-default lsd) (lsd-min lsd) (lsd-max lsd)))

;; palettes are now simple-vectors of pixel values

(defvar *default-palette* nil)


(defvar *current-palette* *default-palette*
  "bound in UPDATE-LANDSCAPE-INTERNAL for the use of the display function")


;; :sum or a number specifying slot offset of interest
(defvar *default-ls-display-multi-value-behavior* 0)

(defvar *current-ls-multi-value-behavior* 
  *default-ls-display-multi-value-behavior*)

(defvar *current-display-max* 100)
(defvar *current-display-min* 0)

;; The basic landscape datastructure is a 2-D array
;; Conceptually, the landscape has a header with global info such as 
;; how to handle out-of-bounds patches. ALso, various caches will be
;; neccessary such as a pixmap display cache and a cache for box structure
;; that may be linked to particular patches

(defstruct (landscape (:copier %copy-landscape)
                      (:print-function %print-landscape))
  (data nil)
  (template nil) ;parsed slots ?
  ;; slots
  (patch-width  1) ; in pixels
  (patch-height 1)
  (scroll-row 0)
  (scroll-col 0)
  (zoom 1)
  (rotation 0)
  ;; caches
  (pixmap-cache nil)
  (editor-structure-cache nil)
  ;;eventual place for compiled display functions
  (display-function-cache 'default-landscape-display)
  (palette *default-palette*)
  (display-slot *default-ls-display-multi-value-behavior*)
  ;; backpointer
  (box nil)
  (patch-row (make-row '(()))))

(defun %print-landscape (ls stream level)
  (declare (ignore level))
  (format stream "#<LANDSCAPE ~Dx~D ~A>"
          (landscape-cols ls) (landscape-rows ls)
          (if (listp (landscape-template ls))
              (mapcar #'lsd-name (landscape-template ls))
              (lsd-name (landscape-template ls)))))

;; landscape will be on a box's PLIST for now
(defmethod box-landscape ((box box))  (getf (slot-value box 'plist) :landscape))
(defmethod set-landscape ((box box) landscape)
  (setf (getf (slot-value box 'plist) :landscape) landscape))

;; generic box or vc versions
(defun box-or-vc-landscape (box)
  (cond ((box? box) (box-landscape box))
        ((virtual-copy? box) (getf (vc-graphics box) 'landscape))))

(defun set-box-or-vc-landscape (box landscape)
  (cond ((box? box) (set-landscape box landscape))
        ((virtual-copy? box) (setf (getf (vc-graphics box) 'landscape) landscape))))

(defun landscape-rows (ls) (array-dimension (landscape-data ls) 1))
(defun landscape-cols (ls) (array-dimension (landscape-data ls) 0))

;; evaluator interface
(defclass landscape-patch-reference
  (foreign-data)
  ((landscape :initform nil :initarg :landscape)
   (row :initform 0 :initarg :row)
   (col :initform 0 :initarg :col))
  (:metaclass block-compile-class))

;; slot names of patches call this
;make-patch-slot-data

(defclass landscape-patch-slot-reference
  (landscape-patch-reference)
  ((slotpos :initform 0 :initarg :slotpos))
  (:metaclass block-compile-class))

;; basic scoping function
(defun get-landscape (&optional (root (static-root)))
  (cond ((typep root 'landscape-patch-reference)
         ;; this can happen if we use TELL on a landscape-patch-reference
         (slot-value root 'landscape))
        ((and (boundp 'eval::*for-each-variable-object*)
              (landscape-p eval::*for-each-variable-object*))
         ;; this happens inside of for-each-patch
         eval::*for-each-variable-object*)
        (t ;; normal scoping through the editor
         (do* ((box root (when (box? box) (superior-box box)))
               (ls (and box (box-landscape box)) (and box (box-landscape box))))
              ((or (null box) (not (null ls))) ls)))))

(defun get-landscape-box (&optional (root (static-root)))
  (if (typep root 'landscape-patch-reference)
      (landscape-box (slot-value root 'landscape)) ; null check ?
      (do* ((box root (when (box? box) (superior-box box)))
            (ls (and box (box-landscape box)) (and box (box-landscape box))))
           ((or (null box) (not (null ls))) box))))

;; this is basically split into 2 parts
;; 1) make a graphics box
;; 2) make a landscape structure followed by
;; 3) set the bit-array of the graphics-sheet to be the pixmap cache of the
;;    landscape-this lets us automagically win with the redisplay
;; by default, we'll make 
;;    pixel-size = 1
;;    scroll r/c = 0
;;    pixmap wid/hei = landscape wid/hei
;;    zoom = 1
;;    rotation = 0

(defun make-landscape-box (rows cols template)
  (let* ((slots (parse-landscape-template template))
         (ldata (make-array (list cols rows)))
         (box (make-initialized-box :type 'data-box))
         (pixmap (make-offscreen-bitmap *boxer-pane* cols rows))
         (graphics-sheet (make-graphics-sheet cols rows box)))
    ;; now initialize the data
    (if (listp slots)
        (let ((initial-values (mapcar #'(lambda (x) (lsd-default x)) slots))
              (data-size (length slots)))
          (dotimes (r rows) (dotimes (c cols)
                              (setf (aref ldata c r)
                                    (make-array data-size 
                                                :initial-contents 
                                                initial-values)))))
        (dotimes (r rows) (dotimes (c cols) 
                            (setf (aref ldata c r) (lsd-default slots)))))
    ;; make the graphics sheet
    (setf (graphics-sheet box) graphics-sheet)
    ;; prefer the graphics representation
    (setf (display-style-graphics-mode? (display-style-list box)) t)
    ;; now make the actual landscape structure using the newly created data
    ;; and setting all the other slots to their default values
    (let ((landscape (make-landscape :data ldata
                                     :template slots :pixmap-cache pixmap :box box)))
      (set-superior-box (landscape-patch-row landscape) box)
      (set-landscape box landscape)
      ;; update the pixmap from the initial data
      (update-landscape-internal landscape))
    ;; share the pixmap
    (setf (graphics-sheet-bit-array graphics-sheet) pixmap)
    ;; add prims for slot access
    (add-patch-prims slots box)
    ;; make a concrete copy of the template with the proper updating triggers
    (let ((template-copy (make-template-box-from-slot-descriptors slots)))
      (append-cha (first-inferior-row box) template-copy))
    box))

;;*** NOTE: at some point, change this to make compiled-boxer objects a la build
;; then we can back hack the evaluator to no longer check for boxer-function?
;; instead of compiled-boxer-function?
(defun add-patch-prims (template box)
  (if (listp template)
      (let ((offset 0))
        (dolist (slot template)
          (eval::add-static-variable-pair
           box (lsd-name slot)
           ;; these functions return foreign-data objects based on
           ;; the current bindings of row and col
           (eval::make-interpreted-procedure-from-list `((bu::%ls-slot-reference
                                                          ,offset))))
          (incf& offset)))
      ;; The name "value" refers to the patch for single valued landscapes
      (eval::add-static-variable-pair 
       box 'bu::value
       (eval::make-interpreted-procedure-from-list `((bu::%patch-self-reference))))))

(defboxer-primitive bu::%patch-self-reference ()
  (let ((ls (get-landscape))
        (row (numberize-or-nil (eval::boxer-symeval 'bu::row)))
        (col (numberize-or-nil (eval::boxer-symeval 'bu::column))))
    (cond ((null ls) 
           (eval::primitive-signal-error
            :landscape "Tying to access a patch name from outside a landscape"))
          ((or (null row) (null col))
           (eval::primitive-signal-error
            :landscape "Trying to access a patch name from outside a patch"))
          (t (make-instance 'landscape-patch-reference
               :landscape ls :row (1-& row) :col (1-& col))))))

;; this is an inner loop function, typically being called inside of
;; for-each-patch and such so it is worthwhile to optimize the hell out of it
;; NOTE: we are guaranteed that <slotpos> will be an integer because we
;; control the usage of this function via the slot access mechanism 
;; embedded in MAKE-LANDSCAPE-BOX above
;; The row and col use the boxer varible lookup mechanism, the iterators
;; work by rebinding these values, NEIGHBOR returns an object which also
;; "binds" these vars 
#|
(defboxer-primitive bu::%ls-slot-reference (slotpos)
  (make-instance 'landscape-patch-slot-reference 
    :landscape (get-landscape)
    :row (numberize-or-error (eval::boxer-symeval 'bu::row))
    :col (numberize-or-error (eval::boxer-symeval 'bu::column))
    :slotpos slotpos))
|#

;; error checking version...
(defboxer-primitive bu::%ls-slot-reference ((eval::dont-copy slotpos))
  (let ((ls (get-landscape))
        (row (numberize-or-nil (eval::boxer-symeval 'bu::row)))
        (col (numberize-or-nil (eval::boxer-symeval 'bu::column))))
    (cond ((null ls) 
           (eval::primitive-signal-error
            :landscape "Tying to access a patch name from outside a landscape"))
          ((or (null row) (null col))
           (eval::primitive-signal-error
            :landscape "Tying to access a patch name from outside a patch"))
          (t (make-instance 'landscape-patch-slot-reference
               :landscape ls :row (1-& row) :col (1-& col) :slotpos slotpos)))))

(defun make-template-box-from-slot-descriptors (slotds)
  (if (listp slotds)
      (make-box (mapcar #'(lambda (lsd) (make-row (list (make-box-from-lsd lsd))))
                        slotds)
                'data-box
                "Template")
      (make-box-from-lsd slotds "Template")))

;; NOTE: supplied-name is only provided for single value landscape template
(defun make-box-from-lsd (slotd &optional supplied-name)
  (let ((return (make-box (list (list (lsd-default slotd)))
                          'data-box (or supplied-name (lsd-name slotd))))
        (minb (make-box (list (list (lsd-min slotd))) 'data-box "Minimum"))
        (maxb (make-box (list (list (lsd-max slotd))) 'data-box "Maximum")))
    ;; first fix the closets of the min and max boxes
    (add-closet-row minb 
                    (make-row (list (make-box (if supplied-name
                                                  '((bu::update-template-slot-minimum))
                                                  `((bu::update-template-slot-minimum
                                                     ,(lsd-name slotd))))
                                              'doit-box "Modified-Trigger"))))
    (add-closet-row maxb 
                    (make-row (list (make-box (if supplied-name
                                                  '((bu::update-template-slot-maximum))
                                                  `((bu::update-template-slot-maximum
                                                    ,(lsd-name slotd))))
                                              'doit-box "Modified-Trigger"))))
    (add-closet-row return
                    (make-row (list (make-box (if supplied-name
                                                  '((bu::update-template-slot-default))
                                                  `((bu::update-template-slot-default
                                                     ,(lsd-name slotd))))
                                              'doit-box "Modified-Trigger")
                                    minb maxb)))
    return))


;; in the simplest case, this can be a databox with a default value in it
;; (type checking on default values? numbers only ?)
;; The more complicated case would be 1+ named data boxes representing fields
;; of data with (possible) default values in each box
;;
;; from the parse, we will determine 
;;  1) the structure of the landscape
;;  2) reserved accessor/mutator names for particular fields
;;  3) default and min/max values for those fields
;;  4) the format of data returned by PATCH
;;  ?) C style enumeration ???

;; a hook for (perhaps later) adding automagic enumeration to slot values
(defun safe-numberize-or-nil (x)
  (cond ((numberp x) x)
        ((or (box? x) (virtual-copy? x) (virtual-port? x))
         (let ((box-rows (get-box-rows x)))
	   (when (null (cdr box-rows))
	     (let ((entries (evrow-pointers (car (get-box-rows x)))))
	       (when (and entries (null (cdr entries)))
	         (let ((object (access-evrow-element x (car entries))))
		   (when (numberp object) 
		     object)))))))
        (t nil)))

(defun patch-slot-value-from-box (box) (numberize-or-nil box))

(defun parse-landscape-template (box)
  (let ((slots nil))
    (dolist (row-items (raw-unboxed-items box))
      (dolist (item row-items)
        (when (or (box? item) (virtual-copy? item)) ; make sure box is named ??
          (push (make-landscape-slot-descriptor item) slots))))
    (cond ((null slots)
           ;; degenerate 1-D case
           (make-landscape-slot-descriptor box t))
          ((null (cdr slots))
           (car slots))
          (t (nreverse slots)))))

;; this is the place to do reality checks on default values...
;; don't hack enumeration yet, just look for numbers
;; special handling for simple case of (possibly) unnamed single valued template
(defvar *default-patch-slot-value* 0)

(defun make-landscape-slot-descriptor (box &optional 1-level?)
  (let* ((n (or (patch-slot-value-from-box box) *default-patch-slot-value*))
         (name (if 1-level? :toplevel (box-name box)))
         (minbox (unless (numberp box) (lookup-variable-in-box-only box 'bu::minimum)))
         (maxbox (unless (numberp box) (lookup-variable-in-box-only box 'bu::maximum)))
         (lsd (%make-landscape-slot-descriptor :name name)))
    ;; set ranges and defaults, if we can
    (unless (null minbox) 
      (let ((minvalue (numberize-or-nil minbox)))
        (when (numberp minvalue) (setf (lsd-min lsd) minvalue))))
    (unless (null maxbox) 
      (let ((maxvalue (numberize-or-nil maxbox)))
        (when (numberp maxvalue) (setf (lsd-max lsd) maxvalue))))
    (unless (null n) (setf (lsd-default lsd) n))
    ;; reality and error checking (need more)...
    (when (or (null n) (null name))
      (eval::primitive-signal-error :landscape box "is a bad template slot"))
    lsd))

;;;; Copying functions
(defun copy-landscape (ls)
  (let ((newls (%copy-landscape ls)))
    ;; now we need to fix up the slots
    ;; the pixmap cache should be copied and handled at a higher level
    (setf (landscape-pixmap-cache newls) nil)
    ;; patch row should be unique
    (setf (landscape-patch-row newls) (make-row '()))
    ;; clear the structure cache
    (setf (landscape-editor-structure-cache newls) nil)
    ;; the data should be copied
    (setf (landscape-data newls) (copy-landscape-data (landscape-data ls)))
    ;; the slot descriptors need to be copied because they can be modified
    ;; via the min/max/default update functions for each landscape
    (setf (landscape-template newls) 
          (let ((old-template (landscape-template ls)))
            (if (listp old-template)
                (mapcar #'copy-landscape-slot-descriptor old-template)
                (copy-landscape-slot-descriptor old-template))))
    newls))

(defun copy-landscape-data (data)
  (let* ((ads (array-dimensions data)) (new-data (make-array ads)))
    ;; now fill the array
    (dotimes (r (car ads)) 
      (dotimes (c (cadr ads))
        (setf (aref new-data r c) 
              (let ((orig-patch (aref data r c)))
                (if (simple-vector-p orig-patch) (copy-seq orig-patch) orig-patch)))))
    new-data))

;; what about caching max/min values to establish range for pallette indices ?


;;; the core redisplay routines updates the pixmap cache and then
;;; the calling function can then either blit the pixmap to the screen or
;;; else set-force-redisplay-infs? on the screen-boxes so they'll update
;;; during the next redisplay

;;; starting at the scroll-{r,c}, loop through each patch, writing into
;;; the pixmap until >= pixmap-{width,height}
(defun update-landscape-internal (ls)
  (let* ((gworld (landscape-pixmap-cache ls))
         ;; pixmap stuff
         (pix-wid (offscreen-bitmap-width gworld))
         (pix-hei (offscreen-bitmap-height gworld))
         (depth (offscreen-bitmap-depth gworld))
         (setpixfun (ecase depth
                      (1 #'%set-1pixel) (8 #'%set-8pixel) 
                      (16 #'%set-16pixel) (32 #'%set-32pixel)))
         (pixmap (#_GetGworldPixmap gworld))
         (row-bytes (ldb& #.(byte 14 0) (ccl::rref pixmap :pixmap.rowbytes)))
         (pix-addr (#_GetPixBaseAddr pixmap))
         ;; landsape stuff
         (data (landscape-data ls))
         (start-row (landscape-scroll-row ls))
         (start-col (landscape-scroll-col ls))
         (pwid (landscape-patch-width ls))
         (phei (landscape-patch-height ls))
         (template (landscape-template ls))         
         (fun (landscape-display-function-cache ls))
         ;; bind specials for default display function
         (*current-palette* (or (landscape-palette ls) *default-palette*))
         (*current-ls-multi-value-behavior*
          (or (landscape-display-slot ls) 
              *default-ls-display-multi-value-behavior*))
         (lsd (if (listp template) (nth *current-ls-multi-value-behavior* template)
                  template))
         (*current-display-max* (lsd-max lsd))
         (*current-display-min* (lsd-min lsd))
         ;; should refer to zoom and rotation vars also...
         (current-x 0) (current-y 0))
    ;; funcall the transform
    (do ((row start-row (1+ row)))
        ((>= current-y pix-hei))
      (setq current-x 0)
      (do ((col start-col (1+ col)))
          ((>= current-x pix-wid))
        (let ((pixel-value (funcall fun (aref data col row))))
          (dotimes (local-x pwid)
            (cond ((>= current-x pix-wid) (return))
                  (t (dotimes (local-y phei)
                       (cond ((>= (+ local-y current-y) pix-hei) (return))
                             (t
                              (funcall setpixfun pix-addr pixel-value
                                       (+& current-x local-x) (+& current-y local-y)
                                       row-bytes)))))))
          (incf& current-x pwid)))
      (incf& current-y phei))))
 

(defun set-landscape-zoom (ls factor)
  (setf (landscape-patch-width ls) factor (landscape-patch-height ls) factor))

(defboxer-primitive bu::ls-zoom ()
  (let ((ls (get-landscape)))
    (if (null ls)
        (eval::primitive-signal-error :landscape "Can't find landscape")
        (landscape-patch-width ls))))

;; synonym
(defboxer-primitive bu::landscape-zoom ()
  (let ((ls (get-landscape)))
    (if (null ls)
        (eval::primitive-signal-error :landscape "Can't find landscape")
        (landscape-patch-width ls))))

(defboxer-primitive bu::set-ls-zoom ((eval::numberize zoom))
  (unless (and (integerp zoom) (> zoom 0))
    (eval::primitive-signal-error :landscape "Zoom, " zoom ", is not an integer > 0"))
  (let* ((lbox (get-landscape-box))
         (ls (when lbox (box-or-vc-landscape lbox)))
         (gs (when lbox (graphics-sheet lbox))))
    (when (null ls)
      (eval::primitive-signal-error :landscape "Can't find landscape"))
    (cond ((= zoom (landscape-patch-width ls)))
          (t
           (let ((new-wid (* (landscape-cols ls) zoom))
                 (new-hei (* (landscape-rows ls) zoom)))
             (resize-graphics-sheet gs new-wid new-hei)
             (setf (graphics-sheet-draw-wid gs) new-wid
                   (graphics-sheet-draw-hei gs) new-hei)
             (set-landscape-zoom ls zoom)
             ;; reattach the landscape and the bitmap
             (setf (landscape-pixmap-cache ls) (graphics-sheet-bit-array gs))
             (update-landscape-internal ls))
           (modified lbox)
           (dolist (sb (screen-objs lbox)) (set-force-redisplay-infs? sb)))))
  eval::*novalue*)

(defboxer-primitive bu::set-landscape-zoom ((eval::numberize zoom))
  (unless (and (integerp zoom) (> zoom 0))
    (eval::primitive-signal-error :landscape "Zoom, " zoom ", is not an integer > 0"))
  (let* ((lbox (get-landscape-box))
         (ls (when lbox (box-or-vc-landscape lbox)))
         (gs (when lbox (graphics-sheet lbox))))
    (when (null ls)
      (eval::primitive-signal-error :landscape "Can't find landscape"))
    (cond ((= zoom (landscape-patch-width ls)))
          (t
           (let ((new-wid (* (landscape-cols ls) zoom))
                 (new-hei (* (landscape-rows ls) zoom)))
             (resize-graphics-sheet gs new-wid new-hei)
             (setf (graphics-sheet-draw-wid gs) new-wid
                   (graphics-sheet-draw-hei gs) new-hei)
             (set-landscape-zoom ls zoom)
             ;; reattach the landscape and the bitmap
             (setf (landscape-pixmap-cache ls) (graphics-sheet-bit-array gs))
             (update-landscape-internal ls))
           (modified lbox)
           (dolist (sb (screen-objs lbox)) (set-force-redisplay-infs? sb)))))
  eval::*novalue*)


;; transform function should take 1 arg (internally, it gets passed the
;; contents of the patch (number in simple case, short vector of values
;; for more complex landscapes) and should produce a pixel value
;; NOTE: NOT a "color" but an actual pixel value suitable for %set-Npixel
;; handling of non 1x1 pixel patches should be handled at the higher level

;; palettes are now simple-vectors of pixel values
;; don't specialize yet cause pixel type will vary with screen current depth
;; and in particular, 32bit pixels won't fit into fixnum arrays

(defun make-palette-from-box (box) 
  (make-palette-from-colors (flat-box-items box)))

(defun make-palette-from-colors (colors)
  (let ((rgbpixfun (ecase (window-depth *boxer-frame*)
                     (1 #'boxer-rgb-values->1pixel)
                     (8 #'boxer-rgb-values->8pixel)
                     (16 #'boxer-rgb-values->16pixel)
                     (32 #'boxer-rgb-values->32pixel)))
        (return-palette (make-array (length colors)))
        ;; assume color list if uniform, if 1st color entry is a list, the
        ;; rest of the colors will be triples, otherwise, assume box or VC
        (raw-entries? (listp (car colors))))
    (do* ((entrys colors (cdr entrys))
          (entry (car entrys) (car entrys))
          (i 0 (1+& i)))
         ((null entrys) return-palette)
      (let* ((entry-items (if raw-entries? entry (flat-box-items entry)))
             (pixel (funcall rgbpixfun 
                             (car entry-items)(cadr entry-items)
                             (caddr entry-items))))
        (setf (svref return-palette i) pixel)))))

;; *current-palette*, *current-display-max*, *current-display-min* should be bound...
(defun default-landscape-display (patch-value)
  (let ((pvalue 
         (cond ((numberp patch-value) patch-value)
               ((numberp *current-ls-multi-value-behavior*)
                (if (>= *current-ls-multi-value-behavior* (length patch-value))
                    (svref patch-value 0)
                    (svref patch-value 
                           *current-ls-multi-value-behavior*)))
               (t ; use the sum of all the values in the patch
                (reduce #'+ patch-value))))
        (clength (length *current-palette*))        
        (dlength (max 1 (- *current-display-max* *current-display-min*)))
        (data-offset (if (< 0 *current-display-min*) (- *current-display-min*) 0)))
    (svref *current-palette* 
           (min& (max& (truncate (* (+ data-offset pvalue) (/ clength dlength))) 0)
                 (1-& clength)))))

;; useful for debugging
(defmacro with-ls-display-vars ((ls) &body body)
  `(let* ((*current-ls-multi-value-behavior* (landscape-display-slot ,ls))
         (*current-palette* (landscape-palette ,ls))
         (slotds (landscape-template ,ls))
         (lsd (if (listp slotds) (nth (landscape-display-slot ,ls) slotds) slotds))
         (*current-display-max* (lsd-max lsd))
         (*current-display-min* (lsd-min lsd)))
    . ,body))

;; brute iteration, copying issues must be dealt with at a higher level
;; possibly by making a copy for reference, then updating the original data
;(defmacro do-landscape-patches ((patch-var landscape) &body body)
;  `(let ((,patch-var
;    ))



;;;; Landscape file system iterface function...

;; dump functions, 
;; dump plist can have an extra :landscape <landscape> to them
(defun landscape-dump-plist-length (box) (if (box-landscape box) 2 0))

(defun landscape-dump-plist-internal (box stream)
  (let ((ls (box-landscape box)))
    (cond ((null ls))
          (t
           (dump-boxer-thing :landscape stream)
           ;; the dumper doesn't hack structures so we transform them to simple 
           ;; vectors.  This also gives us the opportunity the fix things up...
           (dump-boxer-thing
            (vector (landscape-data ls)
                    (if (listp (landscape-template ls))
                        (mapcar #'(lambda (slotd) (vector (lsd-name    slotd)
                                                          (lsd-default slotd)
                                                          (lsd-min     slotd)
                                                          (lsd-max     slotd)))
                                (landscape-template ls))
                        (vector (lsd-name    (landscape-template ls))
                                (lsd-default (landscape-template ls))
                                (lsd-min     (landscape-template ls))
                                (lsd-max     (landscape-template ls))))
                    (landscape-patch-width ls) (landscape-patch-height ls)
                    (landscape-scroll-row ls) (landscape-scroll-col ls)
                    (landscape-zoom ls) (landscape-rotation ls)
                    ;; no pixmap cache...
                    ;; what about (landscape-editor-structure-cache ls) ?
                    ;; REMEMBER to add new slots to the end to 
                    ;; preserve old file functionality...
                    (landscape-display-function-cache ls)
                    ;; need to dump out color info rather than raw pixels....
                    (let* ((orig-palette (landscape-palette ls))
                           (plength (length orig-palette))
                           (dump-palette (make-array plength))
                           (depth (offscreen-bitmap-depth
                                   (landscape-pixmap-cache ls)))                           
                           (colorfun (ecase depth
                                       (1 #'1pixel->boxer-rgb-values)
                                       (8 #'8pixel->boxer-rgb-values)
                                       (16 #'16pixel->boxer-rgb-values)
                                       (32 #'32pixel->boxer-rgb-values))))
                      (dotimes (i plength)
                        (multiple-value-bind (r g b)
                            (funcall colorfun (svref& orig-palette i))
                          (setf (svref& dump-palette i) (list r g b))))
                      dump-palette))
            stream)))))

;; remember that the landscape was dumped out (and will be read in) as a vector
;; this function transforms it back to an actual ls structure
;; remember to return the transformed value...
(defun landscape-load-function (box fs-vector-landscape)
  ;; first make a landscape from the data...
  (let ((ls (make-landscape :data (svref fs-vector-landscape 0)
                            :template (if (listp (svref fs-vector-landscape 1))
                                          (mapcar #'(lambda (v)
                                                      (%make-landscape-slot-descriptor
                                                       :name    (svref v 0)
                                                       :default (svref v 1)
                                                       :min     (svref v 2)
                                                       :max     (svref v 3)))
                                                  (svref fs-vector-landscape 1))
                                          (let ((v (svref fs-vector-landscape 1)))
                                            (%make-landscape-slot-descriptor
                                             :name    (svref v 0)
                                             :default (svref v 1)
                                             :min     (svref v 2)
                                             :max     (svref v 3))))
                            :patch-width  (svref fs-vector-landscape 2)
                            :patch-height (svref fs-vector-landscape 3)
                            :scroll-row   (svref fs-vector-landscape 4)
                            :scroll-col   (svref fs-vector-landscape 5)
                            :zoom         (svref fs-vector-landscape 6)
                            :rotation     (svref fs-vector-landscape 7)
                            :display-function-cache (svref fs-vector-landscape 8)
                            :palette      (svref fs-vector-landscape 9))))    
    ;; hook it up to the box
    (set-landscape box ls) (setf (landscape-box ls) box) 
    (set-superior-box (landscape-patch-row ls) box)
    ;; add the special slot prims
    (add-patch-prims (landscape-template ls) box)
    ;; connect the pixmap
    (setf (landscape-pixmap-cache ls) 
          (graphics-sheet-bit-array (graphics-sheet box)))
    ;; transform the palette back to pixel values from color info
    (let* ((palette (landscape-palette ls))
           (depth (offscreen-bitmap-depth (landscape-pixmap-cache ls)))
           (rgbpixfun (ecase depth
                        (1 #'mcl-rgb->1pixel) (8 #'mcl-rgb->8pixel)
                        (16 #'mcl-rgb->16pixel) (32 #'mcl-rgb->32pixel))))
      (dotimes (i (length palette))
        (unless (typep (svref& palette i) 'fixnum)
          (setf (svref& palette i) 
                (funcall rgbpixfun (reallocate-pixel-color (svref& palette i)))))))
    ls))

(setf (get :landscape 'load-module-function) 'landscape-load-function)



;;; Landscape copying (virtual and otherwise) hook functions

(defun edvc-landscape-hook (eb vc)
  (let ((ls (box-landscape eb)))
    (unless (null ls)
      (let ((newls (copy-landscape ls)))
        ;; hookup the pixmap cache
        (setf (landscape-pixmap-cache newls) 
              (graphics-sheet-bit-array 
               (graphics-info-graphics-sheet (vc-graphics vc))))
        (setf (getf (vc-graphics vc) 'landscape) newls)))))

(defun vcvc-landscape-hook (ovc vc)
  (let ((ls (getf (vc-graphics ovc) 'landscape)))
    (unless (null ls)
      (let ((newls (copy-landscape ls)))
        ;; hookup the pixmap cache
        (setf (landscape-pixmap-cache newls) 
              (graphics-sheet-bit-array 
               (graphics-info-graphics-sheet (vc-graphics vc))))
        (setf (getf (vc-graphics vc) 'landscape) newls)))))

(defun print-vc-landscape-hook (vc eb)
  (let ((ls (getf (vc-graphics vc) 'landscape)))
    (unless (null ls)
      ;; the landscape should point to the box
      (setf (landscape-box ls) eb)
      ;; copy the slot access prims cause they won't be transferred using the normal
      ;; structure copying methods
      (add-patch-prims (landscape-template ls) eb)
      (set-superior-box (landscape-patch-row ls) eb)
      (set-landscape eb ls))))




;;; Primitives



(defboxer-primitive bu::make-landscape ((eval::numberize r) (eval::numberize c)
                                        template)
  (make-landscape-box r c (box-or-port-target template)))

(defboxer-primitive bu::landscape-redisplay ()
  (let* ((landscape-box (get-landscape-box))
         (landscape (when (box? landscape-box) (box-landscape landscape-box))))
    (if (null landscape)
        (eval::primitive-signal-error :landscape "Can't find landscape")
        (progn (update-landscape-internal landscape)
               ;; blit to screen for updates in the middle of loops
               ;; using :foreground is a temporary crock, should make :none work
               (clearscreen landscape-box :foreground)
               ;(modified-graphics landscape-box)
               ))
    eval::*novalue*))

;; synonym
(defboxer-primitive bu::ls-redisplay ()
  (let* ((landscape-box (get-landscape-box))
         (landscape (when (box? landscape-box) (box-landscape landscape-box))))
    (if (null landscape)
        (eval::primitive-signal-error :landscape "Can't find landscape")
        (progn (update-landscape-internal landscape)
               ;; blit to screen for updates in the middle of loops
               ;; using :foreground is a temporary crock, should make :none work
               (clearscreen landscape-box :foreground)
               ;(modified-graphics landscape-box)
               ))
    eval::*novalue*))

;; NUMBER-OF-ROWS is already in use (make it generic?)
(defboxer-primitive bu::ls-number-of-rows ((bu::port-to ls))
  (let* ((landscape-box (box-or-port-target ls))
         (landscape (when (box? landscape-box) (box-or-vc-landscape landscape-box))))
    (if (null landscape)
        (eval::primitive-signal-error :landscape ls " is not a landscape")
        (landscape-rows landscape))))

(defboxer-primitive bu::ls-number-of-cols ((bu::port-to ls))
  (let* ((landscape-box (box-or-port-target ls))
         (landscape (when (box? landscape-box) (box-or-vc-landscape landscape-box))))
    (if (null landscape)
        (eval::primitive-signal-error :landscape ls " is not a landscape")
        (landscape-cols landscape))))

;; the args are 1 based but get converted to 0-based internally...
(defboxer-primitive bu::patch ((eval::numberize row) (eval::numberize col))
  (let ((ls (get-landscape)))
    (cond  ((null ls) (eval::primitive-signal-error :landscape "Can't find Landscape"))
           ((not (and (typep row 'fixnum) (plusp row)
                      (typep col 'fixnum) (plusp col)))
            ;; bounds checking too ??
            (eval::primitive-signal-error :landscape "Bad arg"))
           (t
            ;(port-to (make-editor-box-for-patch ls (1-& row) (1-& col)))
            (port-to (make-instance 'landscape-patch-reference 
                       :landscape ls :row (1-& row) :col (1-& col)))))))

;; the concrete version of neighbor
(defboxer-primitive bu::neighbor ((eval::numberize delta-r) (eval::numberize delta-c))
  (let ((current-r (numberize-or-nil (eval::boxer-symeval 'bu::row)))
        (current-c (numberize-or-nil (eval::boxer-symeval 'bu::column)))
        ;; NOTE: current-r,c are in boxer 1 based coords
        (landscape (get-landscape)))
    (cond ((and (numberp current-r) (numberp current-c) landscape)
           (port-to ;(make-editor-box-for-patch (get-landscape)
                    ;                           (+ current-r delta-r) 
                    ;                           (+ current-c delta-c))
                    (make-instance 'landscape-patch-reference
                      :landscape landscape 
                      :row (+ current-r -1 delta-r)
                      :col (+ current-c -1 delta-c))
                    ))
          (t (eval::primitive-signal-error 
              :landscape "Neighbor only works from inside a patch")))))


;; this can accept out of range args (for example, as a result of using NEIGHBOR
;; on an edge patch) 
(defun make-editor-box-for-patch (ls row col)
  (flet ((make-out-of-bounds-patch-data (template)
           (if (listp template)
               (make-array (length template)
                           :initial-contents (mapcar #'lsd-default template))
               (lsd-default template)))
         (out-of-bounds? ()
           (or (<& row 0) (>=& row (landscape-rows ls))
               (<& col 0) (>=& col (landscape-cols ls)))))
    (let* ((existing (dolist (x (landscape-editor-structure-cache ls))
                      (when (and (=& row (svref x 0)) (=& col (svref& x 1)))
                        (return (svref& x 2)))))
          (template (landscape-template ls))
          (patch-data (if (out-of-bounds?)
                          (make-out-of-bounds-patch-data template)
                          (aref (landscape-data ls) col row))))
      (if (not (null existing))
          ;; if there is an existing box, need to update it from the current values...
          (if (numberp patch-data)
              (progn (bash-box-to-number existing patch-data) existing)
              (do* ((i 0 (1+ i))
                    (slotds template (cdr slotds)) (lsd (car slotds) (car slotds)))
                   ((null slotds) existing)
                (let ((var (lookup-variable-in-box-only existing (lsd-name lsd))))
                  (when (box? var) ; should we error here ??
                    (bash-box-to-number var (svref patch-data i))))))
          ;; make a new editor box and cache it...
          (let ((edbox (cond ((numberp patch-data)
                               (make-box (list (list patch-data))))
                              (t (make-box 
                                  (list (with-collection
                                          (do* ((i 0 (1+ i))
                                                (slots template (cdr slots))
                                                (slot (car slots) (car slots)))
                                               ((null slots))
                                            (collect 
                                              (let ((slot-box
                                                     (make-box 
                                                      (list (list (svref patch-data i)))
                                                      'data-box
                                                      (lsd-name slot))))
                                                (add-closet-row slot-box
                                                                (make-row
                                                                 (list 
                                                                  (make-box
                                                                   `((bu::update-slot-value
                                                                      ,(1+ row) ,(1+ col)
                                                                      ,(lsd-name slot)))
                                                                   'doit-box
                                                                   "Modified-Trigger"))))
                                                slot-box))))))))))
            ;; make reserved names available in closet
            (let ((patch-closet (make-row (list (make-box `((,(1+& col))) 'data-box "Column")
                                                (make-box `((,(1+& row))) 'data-box "Row")
                                                ;; color box(?)
                                                ;; x,y
                                                ))))
              (add-closet-row edbox patch-closet))
            ;; for scoping...
            (append-cha (landscape-patch-row ls) edbox)          
            ;; NOTE: still need to search and GC these editor boxes
            (push (vector row col edbox) (landscape-editor-structure-cache ls))
            edbox)))))

;; editor copying...

(defun copy-landscape-special-property (from-box to-box)
  (let ((from-ls (box-landscape from-box)))
    (unless (null from-ls)
      (let* ((to-ls (copy-landscape from-ls))
             (graphics-sheet (graphics-sheet to-box))
             (pixmap (and graphics-sheet (graphics-sheet-bit-array graphics-sheet))))
        ;; hook up the pixmap cache
        (if (null pixmap)
            (error "No pixmap available to attach to the new landscape")
            (setf (landscape-pixmap-cache to-ls) pixmap))
        (set-landscape to-box to-ls)))))

;;; Initializations...
(eval-when (load)
  (setq *default-palette*
        ;; need to change this, it won't work unless the boxer-frame is open
        (make-palette-from-colors '((100 0 0)    ; red
                                    (100 60 0)  ; orange
                                    (100 100 0)  ; yellow
                                    (0 100 0)    ; green
                                    (0 0 100)    ; blue
                                    (20 0 69)   ; indigo
                                    (61 14 100) ; violet
                                    )))

  ;; file system hooks...
  (unless (member 'landscape-dump-plist-length *dump-plist-length-hook*)
    (push 'landscape-dump-plist-length *dump-plist-length-hook*))
  (unless (member 'landscape-dump-plist-internal *dump-plist-internal-hook*)
    (push 'landscape-dump-plist-internal *dump-plist-internal-hook*))
  (unless (member :landscape *load-module-init-keywords*)
    (push :landscape *load-module-init-keywords*))
  ;; Copying Hooks
  (unless (member 'copy-landscape-special-property *copy-special-box-properties-hook*)
    (push 'copy-landscape-special-property *copy-special-box-properties-hook*))
  (unless (member 'edvc-landscape-hook *edvc-special-structures-hook*)
    (push 'edvc-landscape-hook *edvc-special-structures-hook*))
  (unless (member 'vcvc-landscape-hook *vcvc-special-structures-hook*)
    (push 'vcvc-landscape-hook *vcvc-special-structures-hook*))
  (unless (member 'print-vc-landscape-hook *print-vc-special-structures-hook*)
    (push 'print-vc-landscape-hook *print-vc-special-structures-hook*))

  )


;;; methods

; foreign-data-set <fd> <new-value>

(defmethod foreign-data-set ((fd landscape-patch-reference) new)
  ;; need to parse the new-value arg which can have named boxes inside
  ;; should reality check to see if the data matches the template
  (let* ((template (landscape-template (slot-value fd 'landscape)))
         (new-value (cond ((numberp new) new)
                         (t (box-or-port-target new))))
         (col (slot-value fd 'col)) (row (slot-value fd 'row))
         (lsdata (landscape-data (slot-value fd 'landscape)))
         (sizes (array-dimensions lsdata)))
    (cond ((or (>=& col (car sizes)) (>=& row (cadr sizes)))) ; do nothing
          ((and (listp template) (not (null (cdr template))))
           (do* ((i 0 (1+& i))
                 (dvec (aref lsdata col row))
                 (lsds template (cdr lsds))
                 (lsd (car lsds) (car lsds)))
                ((null lsds))
             (let* ((val (lookup-variable-in-box-only new-value (lsd-name lsd)))
                    (nval (and val (numberize-or-nil val))))
               (unless (null nval) (setf (svref& dvec i) nval)))))
          (t (let ((val (numberize-or-nil new-value)))
               (setf (aref lsdata col row) val))))))

(defmethod foreign-data-set ((fd landscape-patch-slot-reference) new-value)
  (let* ((col (slot-value fd 'col)) (row (slot-value fd 'row))
         (lsdata (landscape-data (slot-value fd 'landscape)))
         (sizes (array-dimensions lsdata)))
    (unless (or (>=& col (car sizes)) (>=& row (cadr sizes)))
      (setf (svref (aref lsdata col row) (slot-value fd 'slotpos))
            (numberize-or-error new-value)))))

;; virtual-copy-foreign-data <fd>
(defmethod virtual-copy-foreign-data ((fd landscape-patch-reference))
  (let* ((ls (slot-value fd 'landscape)) (lsdata (landscape-data ls))
         (row (slot-value fd 'row)) (col (slot-value fd 'col))
         (sizes (array-dimensions lsdata))
         (pvalue (if (or (>=& col (car sizes)) (>=& row (cadr sizes)))
                   (let ((template (landscape-template ls)))
                     (if (listp template)
                       (make-array (length template)
                                   :initial-contents (mapcar #'lsd-default template))
                       (lsd-default template)))
                   (aref lsdata col row))))
    (if (numberp pvalue) 
      pvalue
      (make-virtual-copy :rows (with-collection
                                 (do* ((i 0 (1+& i))
                                       (lsds (landscape-template 
                                              (slot-value fd 'landscape))
                                             (cdr lsds))
                                       (lsd (car lsds) (car lsds))
                                       (name (lsd-name lsd) (lsd-name lsd)))
                                      ((null lsds))
                                   (collect 
                                     (make-evrow-from-entry 
                                      (make-virtual-copy 
                                       :rows (list (list (svref& pvalue i)))
                                       :name name)))))))))

;; should be a number....
(defmethod virtual-copy-foreign-data ((fd landscape-patch-slot-reference))
  (let* ((col (slot-value fd 'col)) (row (slot-value fd 'row))
         (ls (slot-value fd 'landscape)) (lsdata (landscape-data ls))
         (sizes (array-dimensions lsdata)))
    (if (or (>=& col (car sizes)) (>=& row (cadr sizes)))
        ;; out of bounds reference so get from template
        (let ((template (landscape-template ls)))
          (if (listp template)
              (make-array (length template) 
                          :initial-contents (mapcar #'lsd-default template))
              (lsd-default template)))
        ;; otherwise grab data from array
        (svref& `(aref lsdata col row) (slot-value fd 'slotpos)))))

;; make-editor-box-from-foreign-data <fd>
(defmethod make-editor-box-from-foreign-data ((fd landscape-patch-reference))
  (make-editor-box-for-patch (slot-value fd 'landscape)
                             (slot-value fd 'row) (slot-value fd 'col)))

;; quick and dirty, doesn't have the proper "portness"
(defmethod make-editor-box-from-foreign-data ((fd landscape-patch-slot-reference))
  (make-box (list (list
                   (svref& (aref (landscape-data (slot-value fd 'landscape))
                                 (slot-value fd 'col) (slot-value fd 'row))
                           (slot-value fd 'slotpos))))
            'data-box))

;; note that the FD can have out of bounds offsets as a result of NEIGHBOR
(defmethod virtual-copy-rows ((fd landscape-patch-reference) &optional time who)
  (declare (ignore time who))
  (let* ((ls (slot-value fd 'landscape)) (lsdata (landscape-data ls))
         (row (slot-value fd 'row)) (col (slot-value fd 'col))
         (sizes (array-dimensions lsdata))
         (data (if (or (>=& col (car sizes)) (>=& row (cadr sizes)))
                   (let ((template (landscape-template ls)))
                     (if (listp template)
                       (make-array (length template)
                                   :initial-contents (mapcar #'lsd-default template))
                       (lsd-default template)))
                   (aref lsdata col row))))
    (cond ((numberp data) (list (make-evrow-from-entry data)))
          (t 
           (with-collection
             (do* ((i 0 (1+ i))
                   (slots (landscape-template ls) (cdr slots))
                   (slot (car slots) (car slots)))
                  ((null slots))
               (collect 
                 (let ((slot-box (make-vc (list (make-evrow-from-entry
                                                 (svref data i)))
                                          'data-box
                                          (lsd-name slot))))
                   (make-evrow-from-entry slot-box)))))))))

(defmethod virtual-copy-rows ((fd landscape-patch-slot-reference) &optional time who)
  (declare (ignore time who))
  (let* ((ls (slot-value fd 'landscape)) (lsdata (landscape-data ls))
         (row (slot-value fd 'row)) (col (slot-value fd 'col))
         (sizes (array-dimensions lsdata))
         (data (if (or (>=& col (car sizes)) (>=& row (cadr sizes)))
                   (let ((template (landscape-template ls)))
                     (if (listp template)
                       (make-array (length template)
                                   :initial-contents (mapcar #'lsd-default template))
                       (lsd-default template)))
                   (aref lsdata col row))))
    (list (make-evrow-from-entry (if (numberp data) data
                                     (svref& data (slot-value fd 'slotpos)))))))
  

;; just bind the row and column, all the other stuff is procedural based on
;; the current value of row and column
(defmethod lookup-variable-in-foreign-data ((fd landscape-patch-reference) var)
  (or (case var
        (bu::row (1+& (slot-value fd 'row)))
        (bu::column (1+& (slot-value fd 'col))))
      (let* ((ls (slot-value fd 'landscape))
             (lbox (when ls (landscape-box ls)))
             (sv (when lbox (eval::lookup-static-variable-internal lbox var))))
        (when sv (eval::static-variable-value sv)))))

;; ???? Alternatively, we could just default to the patch-reference method
;(defmethod foreign-data-environment ((fd landscape-patch-slot-reference) var)
;  (declare (ignore var))
;  nil)

(defmethod eval::boxer-symeval-dots-list-fd (error-symbol 
                                             (fd landscape-patch-reference) list)
  (declare (ignore error-symbol))
  (cond ((null (cdr list))
         (let ((template (landscape-template (slot-value fd 'landscape)))
               (index 0))
           (cond ((listp template)
                  (dolist (lsd template eval::*novalue*)
                    (when (eq (car list) (lsd-name lsd))
                      (return (port-to
                               (make-instance 'landscape-patch-slot-reference
                                 :landscape (slot-value fd 'landscape)
                                 :row (slot-value fd 'row) :col (slot-value fd 'col)
                                 :slotpos index))))
                    (incf index)))
                 ((eq (car list) 'bu::value) (port-to fd))
                 (t eval::*novalue*))))
        (t eval::*novalue*)))


;;; reading in foreign data
(defboxer-primitive bu::read-landscape-from-text-file (template file)
  (make-landscape-from-text-file template
                                 (box-text-string (box-or-port-target file))))

(defun make-landscape-from-text-file (template file &optional rows cols)
  (with-open-file (fs file)
    ;; if rows and cols is not supplied, get it from the 1st 2 lines of the file
    (when (null rows)
      (setq cols (read-from-string (read-line fs))
            rows (read-from-string (read-line fs))))
    (let* ((box (make-landscape-box rows cols template))
           (ls (box-landscape box)))
      (fill-landscape-from-text-file-data ls fs rows cols)
      box)))

;; eventually, this should dispatch based on preferences, for now
;; we only handle single value landscapes and we write out ONE
;; line (values are TAB separated) for each row in the landscape
(defun write-text-file-from-landscape (ls pathname)
  (cond ((listp (landscape-template ls))
         (eval::primitive-signal-error
          :landscape "Can only write out single valued landscapes"))
        (t
         (let* ((data (landscape-data ls))
                (dims (array-dimensions data))
                (csize (car  dims)) 
                (rsize (cadr dims)))
           (with-open-file (s pathname :direction :output 
                              :if-exists :supersede :if-does-not-exist :create)
             (dotimes (row (1- rsize))
               (dotimes (col (1- csize))
                 (format s "~A" (aref data col row))
                 (format s "~C" #\tab))
               ;; the last entry doesn;t need a trailing TAB
               (format s "~A" (aref data (1- csize) row))
               (terpri s))
             ;; last row doesn't need a trailing CR
             (let ((row (1- rsize)))
               (dotimes (col (1- csize))
                 (format s "~A" (aref data col row))
                 (format s "~C" #\tab))
               ;; the last entry doesn;t need a trailing TAB
               (format s "~A" (aref data (1- csize) row))))))))

(defboxer-primitive bu::write-text-file-from-landscape ((bu::port-to landscape)
                                                        (eval::dont-copy filename))
  (let ((lsb (box-or-port-target landscape)))
    (cond ((null (box-landscape lsb))
           (eval::primitive-signal-error :landscape
                                         "The box, " landscape ", is not a landscape"))
          (t (write-text-file-from-landscape (box-landscape lsb)
                                             (box-text-string filename)))))
  eval::*novalue*)

;; filestream should be positioned at the beginning of the data
;; HOU ASCII files have strips
(defun fill-landscape-from-text-file-data (ls filestream rows cols)
  (let* (;(eof-value  (list 'end))
         (data (landscape-data ls))
         (simple-p (not (simple-vector-p (aref data 0 0))))
         (line (make-array (* 5 cols) :element-type 'base-character
                           :adjustable t :fill-pointer 0))
         )
    (dotimes (r rows)
      (let (;(line (read-line filestream nil eof-value)) ;; this version CONSes
            (col 0)
            (pointer 0))
        (fill-line-buffer filestream line) ;; non consing, line is bound above
        (loop
          (multiple-value-bind (number idx)
                               (read-from-string line nil nil :start pointer)
            (cond ((or (null number) (>= col cols)) (return))
                  (t (if simple-p 
                         (setf (aref data col r) number)
                         (setf (svref (aref data col r) 0) number))
                     (setq col (1+ col))
                     (setq pointer idx)))))))))

(defun fill-line-buffer (stream &optional buffer)
  (unless (ccl:stream-eofp stream)
    (let ((line (or buffer (Make-Array 10 :Element-Type 'base-Character
                                       :Adjustable T :Fill-Pointer 0)))
          (char nil))
      (setf (fill-pointer buffer) 0)
      (do () ((or (null (setq char (ccl:stream-tyi stream)))
                  (eq char #\CR))
              (when  (eq (ccl:stream-peek stream) #\LF) (ccl:stream-tyi stream))
              buffer)
        (vector-push-extend char line)))))

;;;; Primitives....

;; trigger procedures...
;;
;; these use the cumbersome method of finding the landscape, then walking back
;; down to get the interface box to derive the value.  THere really ought to be
;; a simple way to grab the box which fires a trigger but there isn't at the 
;; momemnt so we have to crock this up (like the sprite update prims do)
(defboxer-primitive bu::update-template-slot-default ((eval::list-rest slot-name))
  (let* ((ls-box (get-landscape-box))
         (ls (when ls-box (box-or-vc-landscape ls-box)))
         (tbox (when ls-box (lookup-variable-in-box-only ls-box 'bu::template))))
    (cond ((null tbox) 
           (eval::primitive-signal-error :landscape
                                         "Can't find template to update"))
          (t
           (let ((slot-box (if (null slot-name)
                               tbox
                               (lookup-variable-in-box-only tbox (car slot-name)))))
             (cond ((null slot-box)
                    (eval::primitive-signal-error :landscape 
                                                  "Can't find slot, "
                                                  (car slot-name)
                                                  ", in template"))
                   (t (let* ((newval (patch-slot-value-from-box slot-box))
                             (slotds (landscape-template ls))
                             (slotd (if (listp slotds)
                                        (car (member (car slot-name) slotds
                                                     :test #'(lambda (a b)
                                                               (eq a (lsd-name b)))))
                                        slotds)))
                        (when (and slotd (numberp newval))
                          (setf (lsd-default slotd) newval)))))))))
  eval::*novalue*)

(defboxer-primitive bu::update-template-slot-minimum ((eval::list-rest slot-name))
  (let* ((ls-box (get-landscape-box))
         (ls (when ls-box (box-landscape ls-box)))
         (tbox (when ls-box (lookup-variable-in-box-only ls-box 'bu::template))))
    (cond ((null tbox) 
           (eval::primitive-signal-error :landscape
                                         "Can't find template to update"))
          (t
           (let* ((slot-box (if (null slot-name)
                                tbox
                                (lookup-variable-in-box-only tbox (car slot-name))))
                  (minbox (when slot-box 
                            (lookup-variable-in-box-only slot-box 'bu::minimum))))
             (cond ((null slot-box)
                    (eval::primitive-signal-error :landscape 
                                                  "Can't find slot, "
                                                  (car slot-name)
                                                  ", in template"))
                   ((null minbox)
                    (eval::primitive-signal-error :landscape 
                                                  "Can't find minimum box for "
                                                  (car slot-name)
                                                  " in template"))
                   (t (let* ((newval (patch-slot-value-from-box minbox))
                             (slotds (landscape-template ls))
                             (slotd (if (listp slotds)
                                        (car (member (car slot-name) slotds
                                                     :test #'(lambda (a b)
                                                               (eq a (lsd-name b)))))
                                        slotds)))
                        (when (and slotd (numberp newval))
                          (setf (lsd-min slotd) newval)))))))))
  eval::*novalue*)

(defboxer-primitive bu::update-template-slot-maximum ((eval::list-rest slot-name))
  (let* ((ls-box (get-landscape-box))
         (ls (when ls-box (box-landscape ls-box)))
         (tbox (when ls-box (lookup-variable-in-box-only ls-box 'bu::template))))
    (cond ((null tbox) 
           (eval::primitive-signal-error :landscape
                                         "Can't find template to update"))
          (t
           (let* ((slot-box (if (null slot-name)
                                tbox
                                (lookup-variable-in-box-only tbox (car slot-name))))
                  (maxbox (when slot-box 
                            (lookup-variable-in-box-only slot-box 'bu::maximum))))
             (cond ((null slot-box)
                    (eval::primitive-signal-error :landscape 
                                                  "Can't find slot, "
                                                  (car slot-name)
                                                  ", in template"))
                   ((null maxbox)
                    (eval::primitive-signal-error :landscape 
                                                  "Can't find maximum box for "
                                                  (car slot-name)
                                                  " in template"))
                   (t (let* ((newval (patch-slot-value-from-box maxbox))
                             (slotds (landscape-template ls))
                             (slotd (if (listp slotds)
                                        (car (member (car slot-name) slotds
                                                     :test #'(lambda (a b)
                                                               (eq a (lsd-name b)))))
                                        slotds)))
                        (when (and slotd (numberp newval))
                          (setf (lsd-max slotd) newval)))))))))
  eval::*novalue*)

;;; This appears in the closet of concrete links to patches
(defboxer-primitive bu::update-slot-value ((eval::numberize row) 
                                           (eval::numberize col)
                                           (eval::list-rest slot-name))
  (let* ((ls-box (get-landscape-box))
         (ls (when ls-box (box-landscape ls-box))))
    (cond ((null ls)
           (eval::primitive-signal-error 
            "Trying to update a patch value outside a landscape"))
          (t
           (let ((vbox (eval::boxer-symeval (car slot-name))))
             (cond ((null vbox)
                    (eval::primitive-signal-error "Can't find value for patch update"))
                   (t
                    (let* ((data (landscape-data ls))
                           (val (patch-slot-value-from-box vbox))
                           (entry (aref data (1- col) (1- row))))
                      (if (simple-vector-p entry)
                          (setf (svref entry (position (car slot-name)
                                                       (landscape-template ls)
                                                       :test #'(lambda (a b) 
                                                                 (eq a (lsd-name b)))))
                                val)
                          (setf (aref data (1- col) (1- row)) val)))))))))
  eval::*novalue*)

;; patch iterators need to push a dynamic frame with all the special names
;; bound to procs which do "the right thing"


;;; trash prims
#|
(defboxer-primitive bu::init-landscapes ()
  (set-rainbow-palette '(0 5 10 15 20 25 30))
  eval::*novalue*)
|#

;(defboxer-primitive bu::palette-offsets ()
;  (make-vc (list (make-evrow-from-entries (mapcar #'car *default-palette*)))))


;; Landscape arg is implicit a la TELL style for these next 2...
(defboxer-primitive bu::landscape-display-slot ((eval::list-rest slot-name))
  (let ((ls (get-landscape)))
    (if (null ls) (eval::primitive-signal-error :landscape "Can't find landscape")
        (let* ((slots (landscape-template ls))
               (slot-idx (if (listp slots)
                             (position (car slot-name) slots
                                       :test
                                       #'(lambda (a b) (eq a (lsd-name b))))
                             0)))
          (if (numberp slot-idx)
              (setf (landscape-display-slot ls) slot-idx)
              (eval::primitive-signal-error :landscape 
                                            "Couldn't find matching slot for: "
                                            (car slot-name))))))
  eval::*novalue*)

;; eventually allow caching of parsed palette for fast palette swapping.  Need to
;; hack modified hooks to flush palette cache 1st
(defboxer-primitive bu::set-palette (rgbs)
  (let ((ls (get-landscape)))
     (if (null ls) (eval::primitive-signal-error :landscape "Can't find landscape")
         (setf (landscape-palette ls)
               (make-palette-from-box (box-or-port-target rgbs))))
  eval::*novalue*))

;;; Mousing around and position prims

;; returns 0-based (values row# col#)
;; should hack scroll-x,y
(defun array->patch-coords (landscape array-x array-y)
  (let ((r (floor array-y (landscape-patch-height landscape)))
        (c (floor array-x (landscape-patch-width landscape))))
    (values (min& (max& r 0) (1-& (landscape-rows landscape)))
            (min& (max& c 0) (1-& (landscape-cols landscape))))))

;; returns 0-based (values x y)
;; the (x,y) corresponds to the top left corner of the patch, if
;; you want the middle of the patch, need to add (round ph,pw 2)

(defun patch->array-coords (landscape row col)
  (let ((pw (landscape-patch-width landscape))
        (ph (landscape-patch-height landscape)))
    (values (* col pw) (* row ph))))

(defun mouse-patch-coords (abs-mouse-x abs-mouse-y)
  (let* ((landscape-box (get-landscape-box))
         (landscape (when landscape-box (box-landscape landscape-box)))
	 (screen-boxes (and landscape-box (displayed-screen-objs landscape-box))))
    (if (null screen-boxes)
	(eval::primitive-signal-error :mouse-error "Landscape Box is not visible")
	(do* ((sbs screen-boxes (cdr sbs))
	      (sb (car sbs) (car sbs)))
	     ((null (cdr sbs))		
	      (multiple-value-bind (bx by)
		  (xy-inside-box sb)
		(multiple-value-bind (row col)
		    (array->patch-coords
		     landscape (-& abs-mouse-x bx) (-& abs-mouse-y by))
                  (port-to (make-instance 'landscape-patch-reference
                             :landscape landscape :row row :col col)))))
	  (multiple-value-bind (bx by)
	      (xy-inside-box sb)
	    (when (and (<& bx abs-mouse-x (+& bx (screen-obj-wid sb)))
		       (<& by abs-mouse-y (+& by (screen-obj-hei sb))))
	      (multiple-value-bind (row col)
		  (array->patch-coords 
		   landscape (-& abs-mouse-x bx) (-& abs-mouse-y by))
		(return
                 (port-to (make-instance 'landscape-patch-reference
                             :landscape landscape :row row :col col))))))))))

(defboxer-primitive bu::mouse-patch ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims)
    (mouse-patch-coords x y)))

(defboxer-primitive bu::mouse-patch-on-release ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :up)
    (mouse-patch-coords x y)))

(defboxer-primitive bu::mouse-patch-on-click ()
  (multiple-value-bind (x y)
      (mouse-window-coords-for-prims :down)
    (mouse-patch-coords x y)))


;; changed to use ASK syntax for specifying patch
;; old version follow

(defboxer-primitive bu::patch-rc ()
  (let ((ls (get-landscape))
        (row (safe-numberize-or-nil (eval::boxer-symeval 'bu::row)))
        (col (safe-numberize-or-nil (eval::boxer-symeval 'bu::column))))
    (cond ((null ls) 
           (eval::primitive-signal-error
            :landscape "Tying to access a patch name from outside a landscape"))
          ((or (null row) (null col))
           (eval::primitive-signal-error
            :landscape "Trying to access a patch name from outside a patch"))
          (t (make-vc (list (make-evrow-from-entries (list row col))))))))

(defboxer-primitive bu::patch-xy-position ()
  (let* ((lsbox (get-landscape-box))
         (row (safe-numberize-or-nil (eval::boxer-symeval 'bu::row)))
         (col (safe-numberize-or-nil (eval::boxer-symeval 'bu::column))))
    (cond ((null lsbox) 
           (eval::primitive-signal-error
            :landscape "Tying to access a patch name from outside a landscape"))
          ((or (null row) (null col))
           (eval::primitive-signal-error
            :landscape "Trying to access a patch name from outside a patch"))
          (t (multiple-value-bind (array-x array-y)
                 (patch->array-coords (box-landscape lsbox) (1-& row) (1-& col))
               (with-graphics-vars-bound (lsbox)
                 (make-vc (list (make-evrow-from-entries 
                                 (list (user-coordinate-x array-x) 
                                       (user-coordinate-y array-y)))))))))))


#|

(defboxer-primitive bu::patch-rc ((bu::port-to patch))
  (let ((realpatch (box-or-port-target patch)))
    (cond ((typep realpatch 'landscape-patch-reference)
           (make-vc (list (make-evrow-from-entries 
                           (list (1+& (slot-value realpatch 'row))
                                 (1+& (slot-value realpatch 'col)))))))
          (t (let ((r (safe-numberize-or-nil 
                       (lookup-variable-in-box-only realpatch 'bu::row)))
                   (c (safe-numberize-or-nil 
                       (lookup-variable-in-box-only realpatch 'bu::column))))
               (cond ((or (null r) (null c))
                      (eval::primitive-signal-error 
                       :landscape patch " is not a patch"))
                     (t (make-vc (list (make-evrow-from-entries (list r c)))))))))))

(defboxer-primitive bu::patch-xy-position ((bu::port-to patch))
  (let* ((realpatch (box-or-port-target patch))
         (lsbox (get-landscape-box realpatch)))
    (multiple-value-bind (array-x array-y)
        (cond ((typep realpatch 'landscape-patch-reference)
               (patch->array-coords (slot-value realpatch 'landscape)
                                    (slot-value realpatch 'row)
                                    (slot-value realpatch 'col)))
              (t (let ((r (safe-numberize-or-nil 
                           (lookup-variable-in-box-only realpatch 'bu::row)))
                       (c (safe-numberize-or-nil 
                           (lookup-variable-in-box-only realpatch 'bu::column))))
                   (cond ((or (null r) (null c) (null lsbox))
                          (eval::primitive-signal-error 
                           :landscape patch " is not a patch"))
                         (t (patch->array-coords (box-landscape lsbox)
                                                 (1-& r) (1-& c)))))))
      (with-graphics-vars-bound (lsbox)
        (make-vc (list (make-evrow-from-entries 
                        (list (user-coordinate-x array-x) 
                              (user-coordinate-y array-y)))))))))
|#

(defsprite-function bu::patch-under () (sprite turtle)
  (let ((ls (get-landscape)))
    (multiple-value-bind (row col)
        (array->patch-coords ls
                             (array-coordinate-x (absolute-x-position turtle))
                             (array-coordinate-y (absolute-y-position turtle)))
      (port-to (make-instance 'landscape-patch-reference 
                 :landscape ls :row row :col col)))))
  
(defboxer-primitive bu::snip-landscape ((bu::port-to landscape)
                                        (eval::numberize r) (eval::numberize c)
                                        (eval::numberize wid) (eval::numberize hei))
  (cond ((or (not (and (integerp r) (plusp r)))
             (not (and (integerp c) (plusp c)))
             (not (and (integerp wid) (plusp wid)))
             (not (and (integerp hei) (plusp hei))))
         (eval::primitive-signal-error 
          :landscape "Indices and dimensions should be positiive integers"))
        (t (let* ((ls-box (box-or-port-target landscape))
                  (ls (box-or-vc-landscape ls-box)))
             (cond ((null ls) 
                    (eval::primitive-signal-error :landscape
                                                  landscape " is not a landscape"))
                   (t ))))))

;; other possibilities are NTSC(.299 .587 .114), SMPTE(.2122 .7013 .0865)
(defvar *graphics->landscape-rgb-weights* '(1/3 1/3 1/3))

(defboxer-primitive bu::make-landscape-from-graphics ((bu::port-to graphics-box) template)
  (let ((graphics-sheet (graphics-sheet (box-or-port-target graphics-box))))
    (cond ((null graphics-sheet)
           (eval::primitive-signal-error :landscape 
                                         graphics-box " does not have graphics"))
          ((null (graphics-sheet-bit-array graphics-sheet))
           (eval::primitive-signal-error :landscape 
                                         graphics-box " does not have bitmap graphics"))
          (t
           (let* ((wid (graphics-sheet-draw-wid graphics-sheet))
                  (hei (graphics-sheet-draw-hei graphics-sheet))
                  ;; landscape vars
                  (boxls (make-landscape-box hei wid (box-or-port-target template)))
                  (ls (box-landscape boxls))
                  (data (landscape-data ls))
                  (single-value? (not (listp (landscape-template ls))))
                  ;; graphics vars
                  (pix (graphics-sheet-bit-array graphics-sheet))
                  (pixdepth (offscreen-bitmap-depth pix))
                  (pixdata (#_GetGWorldPixmap pix))
                  (row-bytes (ldb& #.(byte 14 0) (ccl::rref pixdata :pixmap.rowbytes)))
                  (pix-addr (#_GetPixBaseAddr pixdata))
                  (pixfun (case pixdepth
                            (1 #'%get-1pixel)   (8 #'%get-8pixel)
                            (16 #'%get-16pixel) (32 #'%get-32pixel)))
                  (rgbfun (case pixdepth
                            (1  #'1pixel->boxer-rgb-values) 
                            (8  #'8pixel->boxer-rgb-values)
                            (16 #'16pixel->boxer-rgb-values)
                            (32 #'32pixel->boxer-rgb-values)))
                  (rweight (car   *graphics->landscape-rgb-weights*))
                  (gweight (cadr  *graphics->landscape-rgb-weights*))
                  (bweight (caddr *graphics->landscape-rgb-weights*)))
             ;; fill the data from the graphics
             (dotimes (x wid)
               (dotimes (y hei)
                 (multiple-value-bind (r g b)
                     (funcall rgbfun (funcall pixfun pix-addr x y row-bytes))
                   ;; should scale to template.value.maximum...
                   (let ((value (+ (* r rweight) (* g gweight) (* b bweight))))
                     (if single-value?
                         (setf (aref data x y) value)
                         (setf (svref& (aref data x y) 0) value))))))
             ;; update the pixmap cache
             (update-landscape-internal ls)
             boxls)))))
                               
;; obsolete
(defboxer-primitive bu::%set-patch-value ((bu::port-to ls)
                                          (eval::numberize row)
                                          (eval::numberize col)
                                          new-value)
  (let* ((landscape-box (box-or-port-target ls))
         (landscape (when (box? landscape-box) (box-landscape landscape-box))))
    (if (null landscape)
        (eval::primitive-signal-error :landscape ls " is not a landscape")
        (let* ((data (landscape-data landscape))
               (current-value (aref data col row)))
          (cond ((numberp current-value)
                 (setf (aref data col row) (numberize-or-error new-value)))
                (t
                 (let ((new-values (car (raw-unboxed-items new-value))))
                   (dotimes (i (length current-value))
                     (let ((new-value  (nth i new-values)))
                       (unless (null new-value)
                         (setf (svref current-value i) new-value))))))))))
  eval::*novalue*)
                   
(defboxer-primitive bu::compatible? ((bu::port-to ls1) (bu::port-to ls2))
  (let ((l1 (box-landscape (box-or-port-target ls1)))
        (l2 (box-landscape (box-or-port-target ls2))))
    (eval::boxer-boolean
     (and l1 l2
          (template= (landscape-template l1) (landscape-template l2))))))

(defun template= (t1 t2)
  (cond ((listp t1)
         (when (listp t2)
           (do* ((slots1 t1 (cdr slots1)) (lsd1 (car slots1) (car slots1))
                 (slots2 t2 (cdr slots2)) (lsd2 (car slots2) (car slots2)))
                ((or (null lsd1) (null lsd2))
                 ;; if we run out of one we should run out of both for T
                 (and (null lsd1) (null lsd2)))
             (unless (lsd= lsd1 lsd2) (return nil)))))
        (t (unless (listp t2) (lsd= t1 t2)))))

(defun lsd= (s1 s2) (eq (lsd-name s1) (lsd-name s2)))

  
;; this is a tmp patch
(defun access-pointer-element (element)
  (if (or (numberp element) (symbolp element) (box? element))
      element
      (if (fast-chunk? element)
	  (chunk-chunk element)
	  element)))
