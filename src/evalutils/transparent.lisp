;;; -*- Syntax: Common-Lisp; Base: 10; Package: EVAL -*-

#|


 $Header: transparent.lisp,v 1.0 90/01/24 22:18:33 boxer Exp $

 $Log:	transparent.lisp,v $
;;;Revision 1.0  90/01/24  22:18:33  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


Modification History

 2/15/03 merged current LW and MCL files, no diffs, updated copyright

|#

(in-package :boxer-eval)

;;;
;;; Transparent Box Support
;;;

(defun set-box-transparency (box transparent-or-not)
  (set-exports box (if transparent-or-not *exporting-all-variables-marker* nil))
  (setf (boxer::display-style-border-style (boxer::display-style-list box))
	(cond ((and transparent-or-not (boxer::storage-chunk? box))
               :thick-dashed)
              (transparent-or-not :dashed)
              ((boxer::storage-chunk? box) :thick)
              (t nil))))

(defconstant *exporting-all-variables-marker* '.export-all-variables.)

;; returns NIL if it is ok to export the list of bindings and the name of
;; the conflicting variable if it is not ok
(defmethod prescan-exported-bindings ((self boxer::box)
				      &optional (bindings-to-be-exported
						 (slot-value self
							     'static-variables-alist)))
  (let ((sup (boxer::superior-box self)))
    (unless (null sup)
      (let ((sup-alist (slot-value sup 'static-variables-alist)))
	(dolist (binding bindings-to-be-exported)
	  (when (fast-assq (car binding) sup-alist) (return (car binding))))))))


;;; When a binding is inserted into a transparent box, the binding pair should
;;; be propagated upward--depending upon the value of the exports slots--and
;;; spliced into the superior box's binding alist.

(defmethod propagate-exported-binding ((box boxer::box) variable value)
  (let ((exports (slot-value box 'boxer::exports)))
    (cond ((null exports))
	  ((or (eq exports *exporting-all-variables-marker*)
	       (fast-memq variable exports))
	   (let ((superior (boxer::superior-box box)))
	     (when (and (boxer::box? superior)
			(not (eq (lookup-static-variable-in-box-only superior
								     variable)
				 value)))
	       (add-static-variable-pair superior variable value)))))))

;;; This is called by the insert-self-action method IF the box being
;;; inserted has a non-NULL exports slot
;;; This mightbe a little faster if I understood the variable caching stuff
;;; but I don't so it just calls add-static-variable-pair instead

(defmethod propagate-all-exported-bindings ((box boxer::box) superior-box)
  (let ((exports (slot-value box 'boxer::exports))
	(static-vars (slot-value box 'static-variables-alist)))
    (when (boxer::box? superior-box)
      (if (eq exports *exporting-all-variables-marker*)
	  (dolist (entry static-vars)
		  (let* ((name (static-variable-name entry))
			 (value (static-variable-value entry))
			 (insert-handler (get name 'boxer::magic-name-insert)))
		    (when (not (eq (lookup-static-variable-in-box-only superior-box
								       name)
				   value))
		      (add-static-variable-pair superior-box name value)
		      (unless (null insert-handler)
			(funcall insert-handler value superior-box)))))
	  (dolist (export exports)
		  (let* ((entry (fast-assq export static-vars))
			 (name (static-variable-name entry))
			 (value (static-variable-value entry))
			 (insert-handler (get name 'boxer::magic-name-insert)))
		    (when (not (eq (lookup-static-variable-in-box-only superior-box
								       name)
				   value))
		      (when (not (null entry))
			(add-static-variable-pair superior-box name value))
		      (unless (null insert-handler)
			(funcall insert-handler value superior-box)))))))))

(defmethod remove-exported-binding ((box boxer::box) variable)
  (let ((exports (slot-value box 'boxer::exports)))
    (cond ((null exports))
	  ((or (eq exports *exporting-all-variables-marker*)
	       (fast-memq variable exports))
	   (let ((superior (boxer::superior-box box)))
	     (when (boxer::box? superior)
	       (remove-static-variable superior variable t)))))))

;;; Called via delete-self-action methods
(defmethod remove-all-exported-bindings ((box boxer::box) superior-box)
  (let ((exports (slot-value box 'boxer::exports))
	(static-vars (slot-value box 'static-variables-alist)))
    (if (eq exports *exporting-all-variables-marker*)
	(dolist (entry static-vars)
	  (let* ((name (static-variable-name entry))
		 (delete-handler (get name 'boxer::magic-name-delete)))
	    (remove-static-variable superior-box name t)
	    (unless (null delete-handler)
	      (funcall delete-handler (static-variable-value entry)
		                      superior-box))))
	(dolist (export exports)
	  (let* ((entry (fast-assq export static-vars))
		 (name (static-variable-name entry))
		 (delete-handler (get name 'boxer::magic-name-delete)))
	    (when (not (null entry))
	      (remove-static-variable superior-box
				      (static-variable-name entry)
				      t))
	    (unless (null delete-handler)
	      (funcall delete-handler (static-variable-value entry)
		                      superior-box)))))))


;;; Here is the interface to transparent boxes
;;; Call this function whenever you want to change the exports of a box
;;; Need to be careful about setting slot for EXPORTS.
;;; remove-all-exported-bindings needs to know what the value WAS and
;;; propagate-all-exported-bindings needs to know what the value WILL BE
(defmethod set-exports ((self boxer::box)
			&optional
			(new-exports *exporting-all-variables-marker*))
  (let ((old-exports (slot-value self 'boxer::exports))
	(static-vars (slot-value self 'static-variables-alist))
	(superior-box (boxer::superior-box self)))
    (cond ((or (eq old-exports    new-exports)
	       (equal old-exports new-exports)))
	  ((null old-exports)
	   (setf (slot-value self 'boxer::exports) new-exports)
	   (unless (null superior-box)
	     (propagate-all-exported-bindings self superior-box)
	     (export-inferior-properties self superior-box)))
	  ((null new-exports)
	   (unless (null superior-box)
	     (remove-all-exported-bindings self superior-box)
	     (unexport-inferior-properties self superior-box))
	   (setf (slot-value self 'boxer::exports) new-exports))
	  ((and (consp old-exports)
		(consp new-exports))
	   ;; Now we have to ADD bindings for names in new-exports that are
	   ;; not in old-exports and REMOVE bindings for names in old-exports
	   ;; that aren't in new-exports
	   (dolist (entry static-vars)
	     (let ((name (static-variable-name entry)))
	       (cond ((and (fast-memq name old-exports)
			   (not (fast-memq name new-exports)))
		      ;; the name was exported but not it shouldn't be
		      (remove-exported-binding self name)))))
	   (setf (slot-value self 'boxer::exports) new-exports)
	   (dolist (entry static-vars)
	     (let ((name (static-variable-name entry)))
	       (cond ((and (not (fast-memq name old-exports))
			   (fast-memq name new-exports))
		      ;; name wasn't exported and now should be
		      (propagate-exported-binding
		       self
		       name
		       (static-variable-value entry)))))))
	  ;; at this point, either new-exports or old-exports is
	  ;; a list and the other one is the *exporting-all-variables-marker*
	  ((consp new-exports)
	   ;; we'll have to REMOVE some bindings
	   (dolist (entry static-vars)
	     (unless (fast-memq (static-variable-name entry) new-exports)
	       (remove-exported-binding self (static-variable-name  entry))))
	   (setf (slot-value self 'boxer::exports) new-exports))
	  ((consp old-exports)
	   ;; we may have to ADD some bindings
	   (setf (slot-value self 'boxer::exports) new-exports)
	   (dolist (entry static-vars)
	     (unless (fast-memq (static-variable-name entry) old-exports)
	       (propagate-exported-binding self
					   (static-variable-name  entry)
					   (static-variable-value entry)))))
	  ;;; just in case
	  (t (error "Don't know how to change exports, ~S to ~S"
		    old-exports new-exports)))))


;;;; Exporting things other than bindings

;; for now, the only important thing is sprites
;; bindings are handled by eval::propagate-all-exported-bindings
(defmethod export-inferior-properties ((self boxer::box) superior)
  (let* ((plist (slot-value self 'boxer::plist))
	 (inf-sprites (getf plist 'boxer::inferior-sprites))
	 (active-sprite (getf plist 'boxer::active-sprite)))
    (unless (null superior)
      (unless (null active-sprite)
        ;; check for a conflict BEFORE adding an active-sprite
        (unless (boxer::getprop superior 'boxer::active-sprite)
	  (boxer::putprop superior active-sprite 'boxer::active-sprite)))
      (when (boxer::graphics-object? (boxer::graphics-info self))
        ;; sprite boxes....
        (let ((existing-sprite (boxer::getprop superior 'boxer::active-sprite)))
          (cond ((null existing-sprite)
                 (boxer::putprop superior self 'boxer::active-sprite))
                ((eq existing-sprite self)
                 (warn "Trying to export an active sprite which is already there"))
                (t
                 ;; for now, we'll prefer most recent, should generate
                 ;; some sort of user level warning here
                 (boxer::putprop superior self 'boxer::active-sprite)))))
      (unless (null inf-sprites)
	(dolist (is inf-sprites) (boxer::add-inferior-sprite superior is)))
      (when (eq *exporting-all-variables-marker*
		(slot-value superior 'box::exports))
	(export-inferior-properties superior (box::superior-box superior))))))


(defmethod unexport-inferior-properties ((self boxer::box) superior
                                         &optional (inf-active-sprite :start))
  (let* ((plist (slot-value self 'boxer::plist))
	 (inf-sprites (getf plist 'boxer::inferior-sprites))
	 (active-sprite (getf plist 'boxer::active-sprite)))
    (unless (null superior)
      (unless (or (null inf-active-sprite)
                  (null active-sprite)
                  (not (eq (boxer::getprop superior 'boxer::active-sprite)
                           active-sprite)))
	(boxer::removeprop superior 'boxer::active-sprite))
      (unless (null inf-sprites)
	(dolist (is inf-sprites)
	  (boxer::remove-inferior-sprite superior is)))
      (when (and (boxer::graphics-object? (boxer::graphics-info self))
                 (eq self (boxer::getprop superior 'boxer::active-sprite)))
        ;; sprite boxes....
	(boxer::removeprop superior 'boxer::active-sprite))
      (when (eq *exporting-all-variables-marker*
		(slot-value superior 'box::exports))
	(unexport-inferior-properties superior
                                      (box::superior-box superior)
                                      (and inf-active-sprite active-sprite))))))


;;; Nobody uses this function.  It is just here to scare you.
#|
;;; DELETE-SELF uses this to unbind the variable in its superior.
;;; Nothing takes care of ports though!
;;; Deleting a variable entails setting its uid to *DELETED-VARIABLE-UID*.
;;; This message used to be called REMOVE-ALL-STATIC-BINDINGS, but did
;;; the same thing.  Why can't DELETE-SELF be fixed to use the name
;;; so it can call REMOVE-STATIC-VARIABLE?

;;; It can be fixed NOW since I put in the flat exports scheme but I'm leaving
;;; it the way it is now cause I don't understand how the variable caching
;;; stuff is supposed to work (specifically, when do I have to flush the
;;; caches explicitly) Right now, it seems that we have to do a tree walk if
;;; we delete a variable (ugghh) -EhL 7/26/87

;;; We don't need this function any more because exports don't work with shared
;;; names anymore.  We can just call remove-static-variable like regular folks
;;; do. 5/12/88

(defmethod remove-all-static-bindings-with-matching-value ((self boxer::box)
							   value)
  (setf (slot-value self 'static-variables-alist)
	(fast-del-if #'(lambda (entry)
			 (cond ((eq (static-variable-value entry) value)
				(setf (static-variable-uid entry)
				      *deleted-variable-uid*)
				(remove-exported-binding
				  self
				  (static-variable-name entry))
				t)
			       (t nil)))
		     (slot-value self 'static-variables-alist))))
|#
