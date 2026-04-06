(defpackage "ASDF-ECL/CC"
  (:use "CL" "ASDF")
  (:export "CROSS-COMPILE" "CROSS-COMPILE-PLAN" "CLEAR-CC-CACHE"))
(in-package "ASDF-ECL/CC")

(defvar *host-target*
  (c::get-target-info))

(defvar *wasm-target*
  (c:read-target-info "./scripts/target-info.lsp"))

(defparameter *cc-target* *host-target*)
;; (defparameter *cc-cache-dir* #P"/tmp/ecl-cc-cache/")
(defparameter *cc-cache-dir* (uiop:merge-pathnames* "ecl-cc-cache/" (uiop:getcwd)))

;;; CROSS-LOAD-OP and cache invalidation. We default to :minimal - that means
;;; that cross-loader will install bytecmp and simply load the source code. This
;;; should be the fastest, but *features* will differ from the target system and
;;; we will not force recompilation.
(defparameter *cc-load-type* :minimal)
(defvar *cc-last-load* :minimal)

;;; KLUDGE some system definitions test that *FEATURES* contains this or that
;;; variant of :ASDF* and bark otherwise.
;;;
;;; KLUDGE systems may have DEFSYSTEM-DEPENDS-ON that causes LOAD-ASD to try to
;;; load the system -- we need to modify *LOAD-SYSTEM-OPERATION* for that. Not
;;; to be conflated with CROSS-LOAD-UP.
;;;
;;; KLUDGE We directly bind ASDF::*OUTPUT-TRANSLATIONS* because ASDF advertised
;;; API does not work.
(defmacro with-asdf-compilation-unit (() &body body)
  `(with-compilation-unit (:target *cc-target*)
     (flet ((cc-path ()
              (merge-pathnames "**/*.*"
                               (uiop:ensure-directory-pathname *cc-cache-dir*))))
       (let ((asdf::*output-translations* `(((t ,(cc-path)))))
             (*load-system-operation* 'load-source-op)
             (*features* (remove-duplicates
                          ;; sgithens hack
                          (list* :emscripten :text-repl-engine :embedded-boxer :shim-3d-math :asdf :asdf2 :asdf3 :asdf3.1 *features*))))
         ,@body))))

(defun cross-compile (system &rest args
                      &key cache-dir target load-type &allow-other-keys)
  (let ((*cc-cache-dir* (or cache-dir *cc-cache-dir*))
        (*cc-target* (or target *cc-target*))
        (*cc-load-type* (or load-type *cc-load-type*))
        (cc-operation (make-operation 'cross-compile-op)))
    (apply 'operate cc-operation system args)

    (with-asdf-compilation-unit () ;; ensure cache
      (output-file cc-operation system))))

(defun cross-compile-plan (system target)
  (format *debug-io* "-- Plan for ~s -----------------~%" system)
  (let ((*cc-target* target))
    (with-asdf-compilation-unit ()
      (map nil (lambda (a)
                 (format *debug-io* "~24a: ~a~%" (car a) (cdr a)))
           (asdf::plan-actions
            (make-plan 'sequential-plan 'cross-compile-op system))))))

(defun clear-cc-cache (&key (dir *cc-cache-dir*) (force nil))
  (uiop:delete-directory-tree
   dir
   :validate (or force (yes-or-no-p "Do you want to delete recursively ~S?" dir))
   :if-does-not-exist :ignore))

;;;

(defclass cross-load-op (non-propagating-operation) ())

(defmethod operation-done-p ((o cross-load-op) (c system))
  (and (component-loaded-p c)
       (eql *cc-last-load* *cc-load-type*)))

;;; :FORCE :ALL is excessive. We should store the compilation strategy flag as a
;;; compilation artifact and compare it with *CC-LOAD-TYPE*.
(defmethod perform ((o cross-load-op) (c system))
  (setf *cc-last-load* *cc-load-type*)
  (ecase *cc-load-type*
    (:emulate
     (error "Do you still believe in Santa Claus?"))
    (:default
     (operate 'load-op c))
    (:minimal
     (ext:install-bytecodes-compiler)
     (operate 'load-op c)
     (ext:install-c-compiler))
    (:ccmp-host
     (with-compilation-unit (:target *host-target*)
       (operate 'load-op c :force :all)))
    (:bcmp-host
     (with-compilation-unit (:target *host-target*)
       (ext:install-bytecodes-compiler)
       (operate 'load-op c :force :all)
       (ext:install-c-compiler)))
    (:bcmp-target
     (with-compilation-unit (:target *cc-target*)
       (ext:install-bytecodes-compiler)
       (operate 'load-op c :force :all)
       (ext:install-c-compiler)))
    (:load-host
     (with-compilation-unit (:target *host-target*)
       (operate 'load-source-op c :force :all)))
    (:load-target
     (with-compilation-unit (:target *cc-target*)
       (operate 'load-source-op c :force :all)))))

(defclass cross-compile-op (sideway-operation downward-operation)
  ())

(defmethod sideway-operation ((self cross-compile-op))
  'cross-compile-op)

(defmethod downward-operation ((self cross-compile-op))
  'cross-object-op)

;;; CROSS-LOAD-OP happens inside the default environment, while the plan for
;;; cross-compilation should have already set the target features.
(defmethod operate ((self cross-compile-op) (c system) &rest args)
  (declare (ignore args))
  (unless (operation-done-p 'cross-load-op c)
    (operate 'cross-load-op c))
  (with-asdf-compilation-unit ()
    (call-next-method)))

(defmethod perform ((self cross-compile-op) (c system))
  (let* ((system-name (primary-system-name c))
         (inputs (input-files self c))
         (output (output-file self c))
         (init-name (format nil "init_lib_~a"
                            (substitute #\_ nil system-name
                                        :test (lambda (x y)
                                                (declare (ignore x))
                                                (not (alpha-char-p y)))))))
    (c:build-static-library output :lisp-files inputs
                                   :init-name init-name)))

(defmethod input-files ((self cross-compile-op) (c system))
  (let ((visited (make-hash-table :test #'equal))
        (systems nil))
    (labels ((normalize-asdf-system (dep)
               (etypecase dep
                 ((or string symbol)
                  (setf dep (find-system dep)))
                 (system)
                 (cons
                  (ecase (car dep)
                    ;; *features* are bound here to the target.
                    (:feature
                     (destructuring-bind (feature depspec) (cdr dep)
                       (if (member feature *features*)
                           (setf dep (normalize-asdf-system depspec))
                           (setf dep nil))))
                    ;; INV if versions were incompatible, then CROSS-LOAD-OP would bark.
                    (:version
                     (destructuring-bind (depname version) (cdr dep)
                       (declare (ignore version))
                       (setf dep (normalize-asdf-system depname))))
                    ;; Ignore "require", these are used during system loading.
                    (:require))))
               dep)
             (rec (sys)
               (setf sys (normalize-asdf-system sys))
               (when (null sys)
                 (return-from rec))
               (unless (gethash sys visited)
                 (setf (gethash sys visited) t)
                 (push sys systems)
                 (map nil #'rec (component-sideway-dependencies sys)))))
      (rec c)
      (loop for sys in systems
            append (loop for sub in (asdf::sub-components sys :type 'cl-source-file)
                         collect (output-file 'cross-object-op sub))))))

(defmethod output-files ((self cross-compile-op) (c system))
  (let* ((path (component-pathname c))
         (file (make-pathname :name (primary-system-name c) :defaults path)))
    (list (compile-file-pathname file :type :static-library))))

(defclass cross-object-op (downward-operation) ())

(defmethod downward-operation ((self cross-object-op))
  'cross-object-op)

  ;;; Ignore all files that are not CL-SOURCE-FILE.
(defmethod perform ((o cross-object-op) (c t)))

(defmethod perform ((o cross-object-op) (c cl-source-file))
  (let ((input-file (component-pathname c))
        (output-file (output-file o c)))
    (multiple-value-bind (output warnings-p failure-p)
        (compile-file input-file :system-p t :output-file output-file)
      (uiop:check-lisp-compile-results output warnings-p failure-p
                                       "~/asdf-action::format-action/"
                                       (list (cons o c))))))

(defmethod input-files ((o cross-object-op) (c cl-source-file))
  (list (component-pathname c)))

(defmethod output-files ((o cross-object-op) (c cl-source-file))
  (let ((input-file (component-pathname c)))
    (list (compile-file-pathname input-file :type :object))))

#+ (or)
(progn
  (clear-cc-cache :force t)

  (cross-compile-plan "alexandria" *wasm-target*)
  (cross-compile-plan "clim-core" *wasm-target*)
  (cross-compile "clim-core" :target *wasm-target*)

  (let ((*cc-target* *wasm-target*))
    (with-asdf-compilation-unit ()
      (output-files 'cross-compile-op "mcclim"))))
