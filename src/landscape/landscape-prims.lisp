;; -*- Mode:LISP;Syntax: Common-Lisp; Package:EVAL;-*-
#|


 $Header$

 $Log$


 Copyright 1996 - 1998 Regents of the University of California


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+



    Landscapes Prims 


Modification History (most recent at top)

7/02/98 fixed bug in handling single values landscapes in %dynamically-set-patch-vars
        let prims handle possible VC landscapes
6/18/98 changed for-each-patch to set local row and column vars in each slot access function
        using %dynamically-set-patch-vars so that ^ will work correctly
5/18/98 added "landscape-" synonyms for "ls-" prims ( -replace-, -multiple-, -add-)
5/18/98 started logging changes: source = landscape v0.7


|#

(in-package :eval)


;****************************************************************

(defrecursive-eval-primitive bu::pbp ((bu::port-to target) (bu::port-to source)
                                      (numberize at-r) (numberize at-c) (list-rest what))
  :state-variables (*for-each-variable-object* 
		    *for-each-counter* 
                    *for-each-length*
		    *for-each-box*
                    *for-each-list*)
  :before (let* ((tbox (boxer::box-or-port-target target))
                 (sbox (boxer::box-or-port-target source))
                 (tls (boxer::box-or-vc-landscape tbox))
                 (sls (boxer::box-or-vc-landscape sbox))
                 (tpatch (make-instance 'boxer::landscape-patch-reference
                           :landscape tls :row 0 :col 0))
                 (spatch (make-instance 'boxer::landscape-patch-reference
                           :landscape sls :row 0 :col 0))
                 (loop-line (append what (list (boxer::port-to tpatch) (boxer::port-to spatch)))))
            (cond ((null tls)
                   (primitive-signal-error :landscape
                                           "Target, " target " is not a landscape box"))
                  ((null sls)
                   (primitive-signal-error :landscape
                                           "Source, " source " is not a landscape box"))
                  ;; it should be ok to PBP between incompatible landscapes...
;                  ((not (boxer::template= (boxer::landscape-template tls)
;                                          (boxer::landscape-template sls)))
;                   (primitive-signal-error :landscape
;                                           "Target and Source landscapes are incompatible"
;                                           target source))
                  ((or (not (and (integerp at-r) (plusp at-r))) 
                       (not (and (integerp at-c) (plusp at-c))))
                   (primitive-signal-error :landscape
                                           "Bad arg for row or column" at-r at-c))
                  (t (set-and-save-state-variables
                      (vector tpatch spatch)
                      ;; col and row counters for tgt and src and the reset value for tgt col
                      (vector (1- at-c) (1- at-r) 0 0 (1- at-c))
                      ;; terminating values for row and col
                      (vector (1- (min (array-dimension (boxer::landscape-data tls) 0)
                                       (+& at-c -1
                                           (array-dimension (boxer::landscape-data sls) 0))))
                              (1- (min (array-dimension (boxer::landscape-data tls) 1)
                                       (+& at-r -1
                                           (array-dimension (boxer::landscape-data sls) 1)))))
                      tbox ; unused?
                      loop-line)
                  ;; do we need to bind bu::row and bu::column ???
                  (recursive-eval-invoke loop-line))))
  :after (cond ((and (>=& (svref& *for-each-counter* 0) (svref& *for-each-length* 0))
                     (>=& (svref& *for-each-counter* 1) (svref& *for-each-length* 1)))
                (restore-state-variables)
                nil)
               (t 
                ; adjust the counters
                (if (=& (svref& *for-each-counter* 0) (svref& *for-each-length* 0))
                    (setf (svref& *for-each-counter* 0) (svref& *for-each-counter* 4)
                          (svref& *for-each-counter* 2) 0
                          (svref& *for-each-counter* 1) (1+& (svref& *for-each-counter* 1))
                          (svref& *for-each-counter* 3) (1+& (svref& *for-each-counter* 3)))
                    (setf (svref& *for-each-counter* 0) (1+& (svref& *for-each-counter* 0))
                          (svref& *for-each-counter* 2) (1+& (svref& *for-each-counter* 2))))
                ;; set the patch vars to the new counter values
                ;; 1st the target patch, then the source patch
                (setf (slot-value (svref& *for-each-variable-object* 0) 'boxer::col)
                      (svref& *for-each-counter* 0)
                      (slot-value (svref& *for-each-variable-object* 0) 'boxer::row)
                      (svref& *for-each-counter* 1))
                (setf (slot-value (svref& *for-each-variable-object* 1) 'boxer::col)
                      (svref& *for-each-counter* 2)
                      (slot-value (svref& *for-each-variable-object* 1) 'boxer::row)
                      (svref& *for-each-counter* 3))
                *for-each-list*)))

(defrecursive-eval-primitive bu::combine ((bu::port-to target) (bu::port-to source)
                                          (list-rest what))
  :state-variables (*for-each-variable-object* 
		    *for-each-counter* 
                    *for-each-length*
		    *for-each-box*
                    *for-each-list*)
  :before (let* ((tbox (boxer::box-or-port-target target))
                 (sbox (boxer::box-or-port-target source))
                 (tls (boxer::box-or-vc-landscape tbox))
                 (sls (boxer::box-or-vc-landscape sbox))
                 (tpatch (make-instance 'boxer::landscape-patch-reference
                           :landscape tls :row 0 :col 0))
                 (spatch (make-instance 'boxer::landscape-patch-reference
                           :landscape sls :row 0 :col 0))
                 (loop-line (append what (list (boxer::port-to tpatch) (boxer::port-to spatch)))))
            (cond ((null tls)
                   (primitive-signal-error :landscape
                                           "Target, " target " is not a landscape box"))
                  ((null sls)
                   (primitive-signal-error :landscape
                                           "Source, " source " is not a landscape box"))
                  (t (set-and-save-state-variables
                      (vector tpatch spatch)
                      ;; col and row counters for tgt and src
                      (vector 0 0)
                      ;; terminating values for row and col
                      (vector (1- (min (array-dimension (boxer::landscape-data tls) 0)
                                       (array-dimension (boxer::landscape-data sls) 0)))
                              (1- (min (array-dimension (boxer::landscape-data tls) 1) 
                                       (array-dimension (boxer::landscape-data sls) 1))))
                      tbox ; unused?
                      loop-line)
                  ;; do we need to bind bu::row and bu::column ???
                  (recursive-eval-invoke loop-line))))
  :after (cond ((and (>=& (svref& *for-each-counter* 0) (svref& *for-each-length* 0))
                     (>=& (svref& *for-each-counter* 1) (svref& *for-each-length* 1)))
                (restore-state-variables)
                nil)
               (t 
                ; adjust the counters
                (if (=& (svref& *for-each-counter* 0) (svref& *for-each-length* 0))
                    (setf (svref& *for-each-counter* 0) 0
                          (svref& *for-each-counter* 1) (1+& (svref& *for-each-counter* 1)))
                    (setf (svref& *for-each-counter* 0) (1+& (svref& *for-each-counter* 0))))
                ;; set the patch vars to the new counter values
                ;; 1st the target patch, then the source patch
                (setf (slot-value (svref& *for-each-variable-object* 0) 'boxer::col)
                      (svref& *for-each-counter* 0)
                      (slot-value (svref& *for-each-variable-object* 0) 'boxer::row)
                      (svref& *for-each-counter* 1))
                (setf (slot-value (svref& *for-each-variable-object* 1) 'boxer::col)
                      (svref& *for-each-counter* 0)
                      (slot-value (svref& *for-each-variable-object* 1) 'boxer::row)
                      (svref& *for-each-counter* 1))
                *for-each-list*)))

;; this works by rebinding bu::row and bu::column
(defrecursive-eval-primitive bu::for-each-patch ((bu::port-to landscape)
                                                 (list-rest what))
  :state-variables (*for-each-variable-object* 
		    *for-each-counter* 
                    *for-each-length*
		    *for-each-box*
                    *for-each-list*
                    ;; vars from TELL
                    *old-tell-special-frame* *lexical-variables-root*)
  :before (let* ((box (boxer::box-or-port-target landscape))
                 (ls (boxer::box-or-vc-landscape box))) (setq boxer::*foo* ls)
            (if (null ls) 
                (primitive-signal-error :landscape 
                                        landscape " is not a landscape box")
                (set-and-save-state-variables
                 ;; special handling for single value landscapes...
                 ls
                 (vector 0 0) ; row and col counters
                 (vector (1- (array-dimension (boxer::landscape-data ls) 0))  ; cols
                         (1- (array-dimension (boxer::landscape-data ls) 1))) ; rows 
                 box
                 what
                 *pdl* box))
            (dynamically-bind-patch-vars ls)
            (recursive-eval-invoke what))
  :after (cond ((and (>=& (svref& *for-each-counter* 0) (svref& *for-each-length* 0))
                     (>=& (svref& *for-each-counter* 1) (svref& *for-each-length* 1)))
                (restore-state-variables)
                (dynamically-unbind-variables)
                nil)
               (t 
                ; adjust the counters
                (if (=& (svref& *for-each-counter* 0) (svref& *for-each-length* 0))
                    (setf (svref& *for-each-counter* 0) 0
                          (svref& *for-each-counter* 1) (1+& (svref& *for-each-counter* 1)))
                    (setf (svref& *for-each-counter* 0) (1+& (svref& *for-each-counter* 0))))
                ;; update the bindings...
                (let ((ls (boxer::box-or-vc-landscape *for-each-box*)))
                  (%dynamically-set-patch-vars ls (1+& (svref& *for-each-counter* 1))
                                               (1+& (svref& *for-each-counter* 0))))
                                             
                ;; now the body
                *for-each-list*))
  :unwind-protect-form (progn (restore-state-variables)
                              (dynamically-unbind-variables)
                              nil))
  

; patch-bindings

(defun dynamically-bind-patch-vars (ls)
  ;; We push the marker first, so unbind can always stop at a marker.
  (%dynamic-variable-push '*dynamic-binding-boundary* nil)
  (%dynamic-variable-push 'bu::row 1)
  (%dynamic-variable-push 'bu::column 1)
  ;; now add the particular patch prims, this is basically dynamically binding
  ;;the same sorts of items as add-patch-prims does to the landscape box
  (let ((template (boxer::landscape-template ls)))
    (if (listp template)
      (let ((offset 0))
        (dolist (slot template)
          (%dynamic-variable-push
           (boxer::lsd-name slot)
           ;; these functions return foreign-data objects based on
           ;; the current bindings of row and col
           (let ((sfun (make-interpreted-procedure-from-list
                        `((bu::%ls-slot-reference ,offset)))))
             (setf (eval::interpreted-boxer-function-locals sfun)
                   (list (eval::make-static-variable 'bu::row 1)
                         (eval::make-static-variable 'bu::column 1)))
             sfun))
          (incf& offset)))
      ;; The name "value" refers to the patch for single valued landscapes
      (%dynamic-variable-push
       'bu::value (let ((sfun (make-interpreted-procedure-from-list 
                               `((bu::%patch-self-reference)))))
                    (setf (interpreted-boxer-function-locals sfun)
                          (list (eval::make-static-variable 'bu::row 1)
                                (eval::make-static-variable 'bu::column 1)))
                    sfun)))))

(defun %dynamically-set-patch-vars (ls r c)
  (%dynamic-variable-set 'bu::row r)
  (%dynamic-variable-set 'bu::column c)
  ;; now bash the local values in the slot accessor functions
  (unless (null ls)
    (let ((template (boxer::landscape-template ls)))
      (if (listp template)
          (dolist (slot template)
            (let ((fun (dynamic-variable-lookup (boxer::lsd-name slot))))
              (unless (null fun)
                (let* ((bindings (interpreted-boxer-function-locals fun))
                       (rbinding (fast-assq 'bu::row bindings))
                       (cbinding (fast-assq 'bu::column bindings)))
                (setf (static-variable-value rbinding) r)
                (setf (static-variable-value cbinding) c)))))
          (let ((fun (dynamic-variable-lookup 'bu::value)))
            (unless (null fun)
              (let* ((bindings (interpreted-boxer-function-locals fun))
                     (rbinding (fast-assq 'bu::row bindings))
                     (cbinding (fast-assq 'bu::column bindings)))
                (setf (static-variable-value rbinding) r)
                (setf (static-variable-value cbinding) c))))))))

(defmacro do-landscape-area ((patch-var ls &optional (r 0) (c 0) wid hei) &body body)
  (let ((ldvar (gensym)) (max-r (gensym)) (max-c (gensym))
        (row-idx (gensym)) (col-idx (gensym)))
    `(let* ((,ldvar (boxer::landscape-data ,ls))
            (,max-r (if ,hei
                        (min (array-dimension ,ldvar 1) (+ ,r ,hei))
                        (array-dimension ,ldvar 1)))
            (,max-c (if ,wid
                        (min (array-dimension ,ldvar 0) (+ ,c ,wid))
                        (array-dimension ,ldvar 0))))
       (macrolet ((%set-sv-patch (newvalue)
                    `(setf (aref ,',ldvar ,',col-idx ,',row-idx)
                           ,newvalue)))
         (do ((,row-idx ,r (1+& ,row-idx)))
             ((>=& ,row-idx ,max-r))
           (do* ((,col-idx ,c (1+& ,col-idx)))                 
                ((>=& ,col-idx ,max-c))
             (let ((,patch-var (aref ,ldvar ,col-idx ,row-idx)))
               . ,body)))))))

(defmacro do-common-landscape-area ((tgt-pvar src-pvar tgt-ls src-ls
                                              &optional (r 0) (c 0) wid hei)
                                    &body body)
  (let ((tgt-ldvar (gensym)) (src-ldvar (gensym))
        (max-r (gensym)) (max-c (gensym))
        (trow-idx (gensym)) (tcol-idx (gensym))
        (scol-idx (gensym)) (srow-idx (gensym)))
    `(let* ((,tgt-ldvar (boxer::landscape-data ,tgt-ls))
            (,src-ldvar (boxer::landscape-data ,src-ls))
            (,max-r (boxer::min& (array-dimension ,tgt-ldvar 1)
                                 (+& (array-dimension ,src-ldvar 1) ,r)
                                 (+& ,r (if ,hei ,hei (expt 2 16)))))
            (,max-c (boxer::min& (array-dimension ,tgt-ldvar 0)
                                 (+& (array-dimension ,src-ldvar 0) ,c)
                                 (+& ,c (if ,wid ,wid ,(expt 2 16))))))
       (macrolet ((%set-sv-target-patch (newvalue)
                    `(setf (aref ,',tgt-ldvar ,',tcol-idx ,',trow-idx)
                           ,newvalue)))
         ;; need this for setting single valued patches
         (do ((,trow-idx ,r (1+& ,trow-idx))
              (,srow-idx 0  (1+& ,srow-idx)))
             ((>=& ,trow-idx ,max-r))
           (do* ((,tcol-idx ,c (1+& ,tcol-idx))
                 (,scol-idx 0  (1+& ,scol-idx)))
                ((>=& ,tcol-idx ,max-c))
             (let ((,tgt-pvar (aref ,tgt-ldvar ,tcol-idx ,trow-idx))
                   (,src-pvar (aref ,src-ldvar ,scol-idx ,srow-idx)))
               . ,body)))))))

;; returns a number or nil (numberize-or-nil doesn't accept null args...)
(defun check-region-arg (arg) (unless (null arg) (boxer::numberize-or-nil arg)))

(defboxer-primitive bu::ls-replace ((bu::port-to target) (bu::port-to source)
                                    (eval::list-rest region-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target source))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((or (null tls))
           (primitive-signal-error :landscape 
                                   "The Target Box, " target " is not a landscape"))
          ((null region-args)
           (ls-replace-internal tls (or sls srcnum) 0 0))
          (t 
           ;; looks like there are more args on the line so we need to recursize eval
           ;; to grab the rest of the args, we use a different replace prim to do it with
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-replace-at-rc target source region-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::landscape-replace ((bu::port-to target) (bu::port-to source)
                                           (eval::list-rest region-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target source))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((or (null tls))
           (primitive-signal-error :landscape 
                                   "The Target Box, " target " is not a landscape"))
          ((null region-args)
           (ls-replace-internal tls (or sls srcnum) 0 0))
          (t 
           ;; looks like there are more args on the line so we need to recursize eval
           ;; to grab the rest of the args, we use a different replace prim to do it with
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-replace-at-rc target source region-args)))
            'boxer::doit-box)))))

;; these are used for more detailed versions of ls-replace
(defboxer-primitive bu::ls-replace-at-rc ((bu::port-to target) (bu::port-to src)
                                          (eval::numberize r) (eval::numberize c)
                                          (eval::list-rest size-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((null size-args)
           (ls-replace-internal tls (or sls srcnum) (1- r) (1- c)))
          (t 
           ;; looks like there are more that just the rc args specified so try again           
           ;; with the full blown version of
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-replace-in-region target src r c size-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::landscape-replace-at-rc ((bu::port-to target) (bu::port-to src)
                                                 (eval::numberize r) (eval::numberize c)
                                                 (eval::list-rest size-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((null size-args)
           (ls-replace-internal tls (or sls srcnum) (1- r) (1- c)))
          (t 
           ;; looks like there are more that just the rc args specified so try again           
           ;; with the full blown version of
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-replace-in-region target src r c size-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::ls-replace-in-region ((bu::port-to target) (bu::port-to src)
                                              (eval::numberize r) (eval::numberize c)
                                              (eval::numberize wid) (eval::numberize hei))
  (let* ((tls (boxer::box-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (ls-replace-internal tls (or sls srcnum) (1- r) (1- c) wid hei)))

(defboxer-primitive bu::landscape-replace-in-region ((bu::port-to target) (bu::port-to src)
                                                     (eval::numberize r) (eval::numberize c)
                                                     (eval::numberize wid) (eval::numberize hei))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (ls-replace-internal tls (or sls srcnum) (1- r) (1- c) wid hei)))

;; expects 0-based rc args
;; wid and hei, if supplied, will be numbers already so no need to check
(defun ls-replace-internal (tls src r c &optional wid hei)
  (cond ((or (not (>=& r 0)) (not (>=& c 0)))
         (primitive-signal-error :landscape
                                 r " or " c " is a bad index for a landscape"))
        ((numberp src)
         (do-landscape-area (patch tls r c wid hei)
           (cond ((numberp patch) (%set-sv-patch src))
                 (t (dotimes (i (length patch)) (setf (svref& patch i) src))))))
        ((boxer::landscape-p src)
         (if (not (boxer::template= (boxer::landscape-template tls)
                                    (boxer::landscape-template src)))
           (primitive-signal-error :landscape
                                   "The source and target landscapes are incompatible")
          (do-common-landscape-area (target-patch source-patch tls src r c wid hei)
            (cond ((numberp target-patch)
                   (%set-sv-target-patch source-patch))
             (t (dotimes (i (length target-patch))
                 (setf (svref& target-patch i) (svref& source-patch i))))))))
        (t (primitive-signal-error :landscape 
                                   "The source, is not a landscape or number")))
  eval::*novalue*)

(defboxer-primitive bu::ls-multiply ((bu::port-to target) (bu::port-to source)
                                    (eval::list-rest region-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target source))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((or (null tls))
           (primitive-signal-error :landscape 
                                   "The Target Box, " target " is not a landscape"))
          ((null region-args)
           (ls-multiply-internal tls (or sls srcnum) 0 0))
          (t 
           ;; looks like there are more args on the line so we need to recursize eval
           ;; to grab the rest of the args, we use a different replace prim to do it with
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-multiply-at-rc target source region-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::landscape-multiply ((bu::port-to target) (bu::port-to source)
                                            (eval::list-rest region-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target source))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((or (null tls))
           (primitive-signal-error :landscape 
                                   "The Target Box, " target " is not a landscape"))
          ((null region-args)
           (ls-multiply-internal tls (or sls srcnum) 0 0))
          (t 
           ;; looks like there are more args on the line so we need to recursize eval
           ;; to grab the rest of the args, we use a different replace prim to do it with
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-multiply-at-rc target source region-args)))
            'boxer::doit-box)))))

;; these are used for more detailed versions of ls-replace
(defboxer-primitive bu::ls-multiply-at-rc ((bu::port-to target) (bu::port-to src)
                                          (eval::numberize r) (eval::numberize c)
                                          (eval::list-rest size-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((null size-args)
           (ls-multiply-internal tls (or sls srcnum) (1- r) (1- c)))
          (t 
           ;; looks like there are more that just the rc args specified so try again           
           ;; with the full blown version of
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-multiply-in-region target src r c size-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::landscape-multiply-at-rc ((bu::port-to target) (bu::port-to src)
                                                  (eval::numberize r) (eval::numberize c)
                                                  (eval::list-rest size-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((null size-args)
           (ls-multiply-internal tls (or sls srcnum) (1- r) (1- c)))
          (t 
           ;; looks like there are more that just the rc args specified so try again           
           ;; with the full blown version of
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-multiply-in-region target src r c size-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::ls-multiply-in-region ((bu::port-to target) (bu::port-to src)
                                              (eval::numberize r) (eval::numberize c)
                                              (eval::numberize wid) (eval::numberize hei))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (ls-multiply-internal tls (or sls srcnum) (1- r) (1- c) wid hei)))

(defboxer-primitive bu::landscape-multiply-in-region ((bu::port-to target) (bu::port-to src)
                                                      (eval::numberize r) (eval::numberize c)
                                                      (eval::numberize wid) (eval::numberize hei))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (ls-multiply-internal tls (or sls srcnum) (1- r) (1- c) wid hei)))

;; expects 0-based rc args
;; wid and hei, if supplied, will be numbers already so no need to check
(defun ls-multiply-internal (tls src r c &optional wid hei)
  (cond ((or (not (>=& r 0)) (not (>=& c 0)))
         (primitive-signal-error :landscape
                                 r " or " c " is a bad index for a landscape"))
        ((numberp src)
         (do-landscape-area (patch tls r c wid hei)
           (cond ((numberp patch) (%set-sv-patch (* patch src)))
                 (t (dotimes (i (length patch))
                      (setf (svref& patch i) (* (svref& patch i) src)))))))
        ((boxer::landscape-p src)
         (if (not (boxer::template= (boxer::landscape-template tls)
                                    (boxer::landscape-template src)))
           (primitive-signal-error :landscape
                                   "The source and target landscapes are incompatible")
           (do-common-landscape-area (target-patch source-patch tls src r c wid hei)
            (cond ((numberp target-patch)
                   (%set-sv-target-patch (* target-patch source-patch)))
             (t (dotimes (i (length target-patch))
                 (setf (svref& target-patch i) (* (svref& source-patch i)
                                                  (svref& target-patch i)))))))))
        (t (primitive-signal-error :landscape 
                                   "The source, is not a landscape or number")))
  eval::*novalue*)

(defboxer-primitive bu::ls-add ((bu::port-to target) (bu::port-to source)
                                    (eval::list-rest region-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target source))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((or (null tls))
           (primitive-signal-error :landscape 
                                   "The Target Box, " target " is not a landscape"))
          ((null region-args)
           (ls-add-internal tls (or sls srcnum) 0 0))
          (t 
           ;; looks like there are more args on the line so we need to recursize eval
           ;; to grab the rest of the args, we use a different replace prim to do it with
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-add-at-rc target source region-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::landscape-add ((bu::port-to target) (bu::port-to source)
                                       (eval::list-rest region-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target source))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((or (null tls))
           (primitive-signal-error :landscape 
                                   "The Target Box, " target " is not a landscape"))
          ((null region-args)
           (ls-add-internal tls (or sls srcnum) 0 0))
          (t 
           ;; looks like there are more args on the line so we need to recursize eval
           ;; to grab the rest of the args, we use a different replace prim to do it with
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-add-at-rc target source region-args)))
            'boxer::doit-box)))))

;; these are used for more detailed versions of ls-replace
(defboxer-primitive bu::ls-add-at-rc ((bu::port-to target) (bu::port-to src)
                                          (eval::numberize r) (eval::numberize c)
                                          (eval::list-rest size-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((null size-args)
           (ls-add-internal tls (or sls srcnum) (1- r) (1- c)))
          (t 
           ;; looks like there are more that just the rc args specified so try again           
           ;; with the full blown version of
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-add-in-region target src r c size-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::landscape-add-at-rc ((bu::port-to target) (bu::port-to src)
                                             (eval::numberize r) (eval::numberize c)
                                             (eval::list-rest size-args))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (cond ((null size-args)
           (ls-add-internal tls (or sls srcnum) (1- r) (1- c)))
          (t 
           ;; looks like there are more that just the rc args specified so try again           
           ;; with the full blown version of
           (setq *sfun-continuation* '*macroexpand-sfun-continuation*)
           (boxer::make-vc
            (list (boxer::make-evrow-from-entries
                   (list* 'bu::ls-add-in-region target src r c size-args)))
            'boxer::doit-box)))))

(defboxer-primitive bu::ls-add-in-region ((bu::port-to target) (bu::port-to src)
                                              (eval::numberize r) (eval::numberize c)
                                              (eval::numberize wid) (eval::numberize hei))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (ls-add-internal tls (or sls srcnum) (1- r) (1- c) wid hei)))

(defboxer-primitive bu::landscape-add-in-region ((bu::port-to target) (bu::port-to src)
                                                 (eval::numberize r) (eval::numberize c)
                                                 (eval::numberize wid) (eval::numberize hei))
  (let* ((tls (boxer::box-or-vc-landscape (boxer::box-or-port-target target)))
         (srcbox (boxer::box-or-port-target src))
         (sls (when (boxer::box? srcbox) (boxer::box-or-vc-landscape srcbox)))
         (srcnum (when (null sls) (boxer::numberize-or-nil srcbox))))
    (ls-add-internal tls (or sls srcnum) (1- r) (1- c) wid hei)))

;; expects 0-based rc args
;; wid and hei, if supplied, will be numbers already so no need to check
(defun ls-add-internal (tls src r c &optional wid hei)
  (cond ((or (not (>=& r 0)) (not (>=& c 0)))
         (primitive-signal-error :landscape
                                 r " or " c " is a bad index for a landscape"))
        ((numberp src)
         (do-landscape-area (patch tls r c wid hei)
           (cond ((numberp patch) (%set-sv-patch (+ patch src)))
                 (t (dotimes (i (length patch))
                      (setf (svref& patch i) (+ (svref& patch i) src)))))))
        ((boxer::landscape-p src)
         (if (not (boxer::template= (boxer::landscape-template tls)
                                    (boxer::landscape-template src)))
           (primitive-signal-error :landscape
                                   "The source and target landscapes are incompatible")
           (do-common-landscape-area (target-patch source-patch tls src r c wid hei)
            (cond ((numberp target-patch)
                   (%set-sv-target-patch (+ target-patch source-patch)))
             (t (dotimes (i (length target-patch))
                 (setf (svref& target-patch i) (+ (svref& source-patch i)
                                                  (svref& target-patch i)))))))))
        (t (primitive-signal-error :landscape 
                                   "The source, is not a landscape or number")))
  eval::*novalue*)


(eval-when (load)
  ;; need to remake all the stack fram caches
  (setup-evaluator)
  )