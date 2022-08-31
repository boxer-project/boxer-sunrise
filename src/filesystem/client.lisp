;-*- mode:lisp; syntax:common-lisp;  package:boxnet -*-
#|

 $Header$

 $Log$

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                         +-Data--+
                This file is part of the | BOXER | system
                                         +-------+


                    The Client side of the File system



Modification History (most recent at top)

10/18/05 filename-for-file: use enough-namestring ONLY for sorting
         directory structure, get the name and type from the filename
 8/31/05 in addition to filename, we dump out dirs,name,type in
         storage-chunk-plist-half-length & dump-storage-chunk-plist-items
 2/16/03 merged current LW and MCL file, no diffs, copyright updated
 2/12/01 merged with current MCL file
 9/02/99 fixed case in filename-for-file which was returning a pathname (which
         blew out the dumper) instead of a string
 5/25/99 storage-chunk-plist-half-length and dump-storage-chunk-plist-items
         changed to hack :file boxtop prop boxes with graphical boxtops
 9/23/98 no-inferiors-for-file? changed to make url case mirror file case
 9/23/98 Started Logging changes: source = boxer version 2.3beta+


|#

(in-package :boxnet)

;; sgithens: working around loading issue
(eval-when (compile load eval)
(defvar ID nil)
)

;;;; Utilities for server box info

(defun record-box-info (info
			&optional (bid (sbi-bid info)) (box (sbi-box info)))
  (setf (gethash box *box-bid-table*) info)
  (let ((existing (assoc bid *bid-box-table* :test #'=)))
    (if (null existing)
	(push (cons bid info) *bid-box-table*)
	(setf (cdr existing) info)))
  info)

(defun box-server-info (box &optional (cons-info? t))
  (let ((existing-info (gethash box *box-bid-table*)) (id 0))
    (cond ((not (null existing-info))
	   existing-info)
	  ((and cons-info?
		(not (null (setq id (getprop box :server-box-id)))))
	   ;; looks like there is no info only because the
	   ;; box has not been loaded yet
	   ;; so we make one
	   (let ((new (get-box-info (get-server) id)))
	     (setf (sbi-box new) box)
	     new))
	  (t nil))))

;;; We do not cons info's for BId's
(defun bid-server-info (bid)
  (cdr (assoc bid *bid-box-table* :test #'=)))

(defun mark-timestamp (box)
  (let ((info (box-server-info box)))
    (unless (null info)
      (setf (sbi-local-timestamp info) (slot-value box 'box::tick)))))

(defun refresh-box-bookkeeping (box info)
  (setf (sbi-write-date info) (get-box-write-date (sbi-bid info)))
  (mark-timestamp box))

(defun clear-bfs-state-tables ()
  (clrhash *box-bid-table*) (setq *bid-box-table* nil))


;;;; Box Info Accessors And Mutators

(defsbi-info-slot owner)
(defsbi-info-slot read-date)
(defsbi-info-slot write-date)
(defsbi-info-slot inferiors)
(defsbi-info-slot forwarding-table)
(defsbi-info-slot boxname)
(defsbi-info-slot superior-box-bid)
(defsbi-info-slot server)

(defun box-bid (box &optional (cons-info? nil))
  (let ((existing-info (box-server-info box cons-info?)))
    (if (null existing-info)
	(getprop box :server-box-id)
	(sbi-bid existing-info))))

(defun bid-box (bid)
  (let ((existing-info (bid-server-info bid)))
    (unless (null existing-info) (sbi-box existing-info))))



;;;; Dump-Hierarchical-Box-Prepass and friends...

;; returns the lowest superior box which is marked
;; as a storage chunk (or NIL)
(defun superior-storage-box (box &optional (skip-initial-box? nil))
  (do ((sb (if skip-initial-box? (superior-box box) box)
	   (superior-box sb)))
      ((not (box?  sb)) nil)
    (when (storage-chunk? sb)
      (return sb))))

(defun superior-box-file-bid (box &optional (skip-initial-box? nil))
  (if (and (storage-chunk? box) (not skip-initial-box?))
      (let ((bid (box-bid box)))
	(if (null bid)
	    (when (box? (superior-box box))
	      (superior-box-file-bid (superior-box box)))
	    bid))
      (when (box? (superior-box box))
	(superior-box-file-bid (superior-box box)))))

(defun get-current-superior-bid (box)
  (let ((sb (superior-box box)))
    (if (box? sb)
	(let ((info (box-server-info sb nil)))
	  (if (null info)
	      (get-current-superior-bid sb)
	      (sbi-bid info)))
	(bsui-top-box-id *box-server-user*))))

;; forwarding tables
(defun find-bid (bid table)
  (do* ((remaining-pairs table (cddr remaining-pairs))
	(current-bid (car remaining-pairs) (car remaining-pairs)))
       ((null current-bid) nil)
    (when (= current-bid bid)
      (return (cadr remaining-pairs)))))

;;; returns either NIL or a new ID
;;; walks up the box hierarchy checking for an bid entry in any
;;; forwarding tables it encounters
(defun validate-server-box-id (box id)
  (cond ((not (box? box)) nil)
	(t (let* ((table (get-box-forwarding-table box))
		  (new-id (unless (null table) (find-bid id table))))
	     (cond ((null new-id)
		    (validate-server-box-id (superior-box box) id))
		   (t new-id))))))



;;; Dumper/Loader utilities
;;; The next 3 functions have to be in agreement about when and what to
;;; dump.  They are used in the dumper.lisp file to control dumping of inferiors
;;; as well as what extra info needs to be dumped out so that the box can
;;; later be read in automatically
;;; There are currently three types of auto reading boxes (black boxes)
;;;    o Boxer Server Boxes: distinguished by numeric ID's in the :server-box-id
;;;      plist property for the most part, these are obsolete
;;;    o URL boxes: look for URL (Universal Resource Locator) Structs in the
;;;      plist property.
;;;    o File boxes: look for pathname (or string) in the :associated-file
;;;      property in the plist
;;;

;; the dumper walks through all the inferiors of the box being dumped.  If an
;; inferior box meets this test, then only its top level characteristics are
;; dumped but we do not recurse through the inferior box's inferiors
(defun no-inferiors-for-file? (box)
  (let ((plist (plist box)))
    (and (storage-chunk? box)
         ;; storage-chunk boxes are the granularity of delayed loading
         ;; we now test each of the three cases described above...
         ;; we could probably streamline the logic but at least this way,
         ;; the 3 cases are easily distinguished
         (or (and (getf plist :server-box-id)  ; box server test...
                  (in-bfs-environment?) (not (eq box *outermost-dumping-box*)))
             (and (getf plist :url)
                  (not (eq box *outermost-dumping-box*)))
             (and (getf plist :associated-file)
                  (not (eq box *outermost-dumping-box*)))))))
#|
(defun no-inferiors-for-file? (box)
  (and (storage-chunk? box)
       (or (not (eq box *outermost-dumping-box*))
           (getprop box :url))
       (or (in-bfs-environment?) (read-only-box? box))))
|#

;; sub box relocation table, branch file links, inf file links
(defun storage-chunk-plist-half-length (box)
  (let* ((half-length 0)
         (boxtop-prop (getprop box :boxtop))
         (boxtop (cond ((or (null boxtop-prop)
                            (eq boxtop-prop :standard)
                            (eq boxtop-prop :framed))
                        (boxer::boxtop box))
                       ((eq boxtop-prop :file)
                        (let ((bt (boxer::boxtop box)))
                          (when (boxer::graphics-sheet? bt) bt))))))
    (cond
     ((in-bfs-environment?)
      (when (and (storage-chunk? box)
		 (or (getprop box :server-box-id) (box-bid box)))
	(incf& half-length))
      ;; check for possible cross file link items
      ;; ... in contained-links
      (when (or (not (null (cross-file-contained-links box)))
		(some #'(lambda (link)
			  (and (cross-file-link? link)
			       (lowest-inferior-link? link box)))
		      (contained-links box)))
	(incf& half-length))
      ;; ... in branch links (bleagh, should iterate only once)
      (when (or (not (null (remove-if #'file-branch-link-broken
				      (cross-file-port-branch-links box))))
		(some #'(lambda (link)
			  (and (eq (link-type link)
				   'port-branch-link)
			       (cross-file-link? link)))
		      (branch-links box)))
	(incf& half-length))
      (when (or (not (null (remove-if #'file-branch-link-broken
				      (cross-file-target-branch-links box))))
		(some #'(lambda (link)
			  (and (eq (link-type link)
				   'target-branch-link)
			       (cross-file-link? link)))
		      (branch-links box)))
	(incf& half-length))
      ;; if we are dumping out as a black box, check for boxtop...
      (when boxtop (incf& half-length)))
     ((url-box? box) (incf& half-length) (when boxtop (incf& half-length)))
     ((boxer::file-box? box)
      ;(incf& half-length)
      ;; 8/31/05 in addition to filename, we dump out dirs,name,type
      (incf& half-length 4)
      (when boxtop (incf& half-length))))
    half-length))

(defmethod dump-storage-chunk-plist-items ((self boxer::box) stream)
  (let* ((boxtop-prop (getprop self :boxtop))
         (boxtop (cond ((or (null boxtop-prop)
                            (eq boxtop-prop :standard)
                            (eq boxtop-prop :framed))
                        (boxer::boxtop self))
                       ((eq boxtop-prop :file)
                        (let ((bt (boxer::boxtop self)))
                          (when (boxer::graphics-sheet? bt) bt))))))
    (cond
     ((in-bfs-environment?)
      (when (and (storage-chunk? self)
	         (or (getprop self :server-box-id)
		     (box-bid self)))
        (dump-boxer-thing :server-box-id stream)
        (dump-boxer-thing (or (getprop self :server-box-id)
			      (box-bid self))
			  stream))
      ;; some things, all boxes might have to deal with...
      ;; first check contained file links
      (let ((contained-links-to-dump (cross-file-contained-links self)))
        (dolist (cl (slot-value self 'contained-links))
	  (when (and (cross-file-link? cl) (lowest-inferior-link? cl self))
	    (push cl contained-links-to-dump)))
        (unless (null contained-links-to-dump)
	  (debugging-message "~&Dumping cross file contained links ~A in box ~A"
			     contained-links-to-dump self)
	  (dump-boxer-thing :cross-file-contained-links stream)
	  (dump-list-preamble (length contained-links-to-dump) stream)
	  (dolist (cl contained-links-to-dump)
	    (if (%cross-file-link? contained-links-to-dump)
	      (dump-boxer-thing (cross-file-link-id cl) stream)
	      ;; must be an editor link
	      (dump-boxer-thing (get-link-id cl) stream)))))
      ;; same sort of thing for branch links
      (let ((port-branch-links-to-dump (remove-if #'file-branch-link-broken
						  (cross-file-port-branch-links
						   self))))
        (dolist (bl (slot-value self 'branch-links))
	  (when (and (eq (link-type bl) 'port-branch-link)
		     (not (eq (link-port bl) self))
		     (cross-file-link? bl))
	    (push (get-link-id bl) port-branch-links-to-dump)))
        (unless (null port-branch-links-to-dump)
	  (debugging-message
	   "~&Dumping cross file Port Branch links ~A in box ~A"
	   port-branch-links-to-dump self)
	  (dump-boxer-thing :cross-file-port-branch-links stream)
	  (dump-list-preamble (length port-branch-links-to-dump) stream)
	  (dolist (pbl port-branch-links-to-dump)
	    (if (%cross-file-link? pbl)
	      (dump-boxer-thing (cross-file-link-id pbl) stream)
	      (dump-boxer-thing pbl stream)))))
      (let ((target-branch-links-to-dump
	     (remove-if #'file-branch-link-broken (cross-file-target-branch-links
						   self)))
	    (terminating-target-links nil))
        (dolist (bl (slot-value self 'branch-links))
	  (when (and (eq (link-type bl) 'target-branch-link)
		     (cross-file-link? bl))
	    (if (eq self (link-target bl))
	      ;; check for terminal link nodeness note that the port
	      ;; case is handled in dump-cross-file-port-reference
	      (push (get-link-id bl) terminating-target-links)
	      (push (get-link-id bl) target-branch-links-to-dump))))
        (unless (null target-branch-links-to-dump)
	  (debugging-message
	   "~&Dumping cross file target branch links ~A in box ~A"
	   target-branch-links-to-dump self)
	  (dump-boxer-thing :cross-file-target-branch-links stream)
	  (dump-list-preamble (length target-branch-links-to-dump) stream)
	  (dolist (tbl target-branch-links-to-dump)
	    (cond ((%cross-file-link? tbl)
		   (dump-boxer-thing (cross-file-link-id tbl) stream))
		  (t (dump-boxer-thing tbl stream)))))
        ;; process any terminating target links
        (unless (null terminating-target-links)
	  (debugging-message "~&Dumping cross file link targets ~A in box ~A"
			     terminating-target-links self)
	  (dump-boxer-thing :cross-file-target-ends stream)
	  (dump-boxer-thing terminating-target-links stream)))
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     ((url-box? self)
      (dump-boxer-thing :url stream)
      (dump-box-url self stream)
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     ((boxer::file-box? self)
      (let* ((file (filename-for-file self))
             (dirs (pathname-directory file))
             (name (pathname-name      file))
             (type (or (pathname-type file) :unspecific)))
        (dump-boxer-thing :associated-file stream)
        (dump-boxer-thing file stream)
        ;; dump out the filename components as well for cross platform portability
        (dump-boxer-thing :associated-file-dirs stream)
        (dump-boxer-thing dirs stream)
        (dump-boxer-thing :associated-file-name stream)
        (dump-boxer-thing name stream)
        (dump-boxer-thing :associated-file-type stream)
        (dump-boxer-thing type stream))
      (when boxtop
        (dump-boxer-thing :cached-boxtop stream) (dump-boxer-thing boxtop stream)))
     )))

;; there are 4 case here, the relative filename flag can be on or off and
;; the filename can already be either a relative one or an absolute one
;; If the the filename matches the flag, we just return the filename otherwise
;; we'll have to do some work
(defmethod filename-for-file ((box boxer::box))
  (let* ((local-flag (boxer::relative-filename? box))
         (filename (getprop box :associated-file))
         (relative? (and filename (eq (car (pathname-directory filename))
                                      :relative))))
    (if (or (and relative? local-flag)
            (and (not relative?) (not local-flag)))
        (namestring filename) ; we have what we want
        (let* ((sup-file-box (boxer::current-file-box (superior-box box)))
               (sup-file (getprop sup-file-box :associated-file)))
          (cond ((null sup-file) ; save as absolute
                 (if relative?
                     (namestring (boxer::boxer-new-file-dialog
                                  :prompt
                                  "The Box has no superior filename to merge"
                                  :directory filename
                                  :box box))
                     (namestring filename)))
                ((and local-flag (not relative?))
                 ;; we want to save relative, but have an absolute
                 ;; use enough-namestring only for sorting directory
                 ;; structure otherwise type info can get lost
                 (namestring
                  (make-pathname :directory (pathname-directory
                                             (enough-namestring filename
                                                                sup-file))
                                 :name (pathname-name filename)
                                 :type (pathname-type filename))))
                (t ; must have relative, but want to save absolute
                 (namestring (merge-pathnames filename sup-file))))))))

(defmethod dump-cross-file-port-reference ((self boxer::port-box) stream)
  (let ((existing-id (gethash self *link-id-table*)))
    (cond ((null existing-id)
	   (error "No existing cross file link id for ~A" self))
	  (t
	   (debugging-message "~&Dumping cross file port ~A, id:~D"
			      self existing-id)
	   (dump-boxer-thing :cross-file-link-id stream)
	   (dump-boxer-thing existing-id stream)))))

;;; Loader utilities

(defun load-server-box-id (box value)
  (unless (and (boundp '*loading-via-box-server?*)
	       (null *loading-via-box-server?*))
    ;; should NOT record the server ID if we are READing the box in
    (let* ((trans (member value (current-forwarding-table) :test #'=))
	   (bid (if (null trans) value (cadr trans))))
      (putprop box bid :server-box-id)
      (record-inferior-storage-chunk bid))))

(defun load-cross-file-contained-links (box value)
  (debugging-message "~&Loading Cross File contained-links ~A for ~A"
		     value box)
  (dolist (id value)
    (add-cross-file-contained-link box (%make-inferior-file-link :id id))))

(defun load-cross-file-port-branch-links (box value)
  (debugging-message "~&Loading Cross File port branch links ~A for ~A"
		     value box)
  (dolist (id value)
    (add-cross-file-port-branch-link
     box (%make-file-port-branch-link :id id :lowest box))))

(defun load-cross-file-target-branch-links (box value)
  (debugging-message "~&Loading Cross File target branch links ~A for ~A"
		     value box)
  (dolist (id value)
    (add-cross-file-target-branch-link
     box (%make-file-target-branch-link :id id :lowest box))))

(defun load-cross-file-target-ends (box value)
  (debugging-message "~&Loading Cross File targets ~A for ~A" value box)
  (dolist (id value)
    (add-cross-file-target-branch-link
     box (%make-file-target-branch-link :id id :box box))))

(defun load-cross-file-link-id (box value)
  (debugging-message "~&Loading Cross File port ~A" value)
  ;; also, record the ID with the port as key so we can reuse this
  ;; ID even after the link has been articulated
  (setf (gethash box *link-id-table*) value)
  (add-cross-file-port-branch-link
   box (%make-file-port-branch-link :id value :box box)))




;;; insert/delete-self support for branch links

(defun maybe-remove-file-links (inferior-link &optional top-box)
  (when (and (inferior-file-link-port-branch inferior-link)
	     (file-branch-link-box (inferior-file-link-port-branch
				    inferior-link))
	     (inferior-file-link-target-branch inferior-link)
	     (file-branch-link-box (inferior-file-link-target-branch
				    inferior-link)))
    ;; if BOTH branches are fully articulated (both have
    ;; terminating boxes), then set the port target
    ;; and remove all file-links
    (debugging-message "~&Relinking Cross File Link ~D"
		       (cross-file-link-id inferior-link))
    (set-port-to-box (file-branch-link-box (inferior-file-link-port-branch
					    inferior-link))
		     (file-branch-link-box (inferior-file-link-target-branch
				    inferior-link)))
    (box::modified (file-branch-link-box (inferior-file-link-target-branch
					  inferior-link)))
    ;; now remove file links from the port branch side
    (when (null top-box)
      ;; remember that at this point, we are sure to have both ends of the link
      (setq top-box (find-lowest-common-superior-box
		     (file-branch-link-box (inferior-file-link-port-branch
					    inferior-link))
		     (file-branch-link-box (inferior-file-link-target-branch
					    inferior-link)))))
    (do ((box (file-branch-link-box (inferior-file-link-port-branch
				     inferior-link))
	      (superior-box box)))
	((or (not (box? box)) (eq box top-box)) )
      (delete-cross-file-port-branch-link box inferior-link))
    ;; now remove file links from the target branch side
    (do ((box (file-branch-link-box (inferior-file-link-target-branch
				     inferior-link))
	      (superior-box box)))
	((or (not (box? box)) (eq box top-box)) )
      (delete-cross-file-target-branch-link box inferior-link))
    ;; and then the top
    (delete-cross-file-contained-link top-box inferior-link)))

;;; reads in boxes on the target side and then relinks them
;;; still needs some bullet proofing especially when faced
;;; with (possible) port crackness
(defun articulate-target-branch (plink)
  (let* ((il (file-branch-link-inferior-link plink))
         (tbl (inferior-file-link-target-branch il)))
    (do ((box (file-branch-link-lowest tbl)
              (file-branch-link-lowest tbl)))
        ((or (not (null (file-target-branch-link-box tbl)))
             (not (storage-chunk? (file-branch-link-lowest tbl))))
         ;;
         (maybe-remove-file-links il))
      (fill-box-from-server box))))

(defun relink-file-link (link)
  ;; need to calculate new common superior, then
  ;; (possibly) move the current inferior-file-link and
  ;; then adjust the branches to conform to the new
  ;; location of the inferior-file-link
  (let* ((pl (inferior-file-link-port-branch link))
	 (tl (inferior-file-link-target-branch link))
	 (pbox (when pl
		 (or (file-branch-link-box pl) (file-branch-link-lowest pl))))
	 (tbox (when tl
		 (or (file-branch-link-box tl) (file-branch-link-lowest tl)))))
    (when (and pbox tbox
	       (superior? pbox *initial-box*) (superior? tbox *initial-box*))
      ;; Both ends of the link are back in the hierarchy
      ;; walk upward to the new common superior, if there is
      ;; a broken link, unmark it, otherwise, add a link
      (let ((sup (find-lowest-common-superior-box pbox tbox)))
	(unless (null sup)
	  (do* ((box pbox (superior-box box))
		(existing-link (link-match
				link (cross-file-port-branch-links box))
			       (link-match
				link (cross-file-port-branch-links box))))
	       ((or (not (box? box)) (eq box sup)))
	    (if (null existing-link)
		(add-cross-file-port-branch-link box pl)
		(setf (file-branch-link-broken existing-link) nil)))
	  (do* ((box tbox (superior-box box))
		(existing-link (link-match
				link (cross-file-target-branch-links box))
			       (link-match
				link (cross-file-target-branch-links box))))
	       ((or (not (box? box)) (eq box sup)))
	    (if (null existing-link)
		(add-cross-file-target-branch-link box pl)
		(setf (file-branch-link-broken existing-link) nil)))
	  (add-cross-file-contained-link sup link))))))


(defun remove-file-branch (from-box link &optional (port-branch? t))
  (do ((box from-box (superior-box box)))
      ((not (box? box)) )
    (let* ((blinks (if port-branch? (cross-file-port-branch-links box)
		       (cross-file-target-branch-links box)))
	   (match? (link-match link blinks)))
      (if (null match?)
	  (let*
	      ;; check to see if there is a contained link we can remove
	      ((cls (cross-file-contained-links box))
	       (cl (link-match link cls)))
	    (unless (null cl) (delete-cross-file-contained-link box cl))
	    (return))
	  (if port-branch?
	      (delete-cross-file-port-branch-link   box link)
	      (delete-cross-file-target-branch-link box link))))))

(defun break-file-links (from-box link &optional (port-branch? t))
  (do ((box from-box (superior-box box)))
      ((not (box? box)))
    (let* ((blinks (if port-branch? (cross-file-port-branch-links box)
		       (cross-file-target-branch-links box)))
	   (match (link-match link blinks)))
      (if (null match)
	  (return)
	  (setf (file-branch-link-broken match) t)))))

(defmethod cross-file-link-insert-self-action ((self boxer::box) superior)
  (let ((sup-cl (cross-file-contained-links superior))
	(sup-pbl (cross-file-port-branch-links superior))
	(sup-tbl (cross-file-target-branch-links superior))
	(new-pbl (copy-seq (cross-file-port-branch-links superior)))
	(new-tbl (copy-seq (cross-file-target-branch-links superior))))
    (flet ((link-handler (link port-branch?)
	     (cond ((not (null (file-branch-link-inferior-link link)))
		    ;; inserting a previously established link
		    (relink-file-link (file-branch-link-inferior-link link)))
		   (t
		    ;; must be the new insert case, usually occurring
		    ;; during READs of files
		    (let ((cl (link-match link sup-cl)))
		      (cond
			((not (null cl))
			 ;; made it to the top
			 (setf (file-branch-link-inferior-link link) cl)
			 (if port-branch?
			     (setf (inferior-file-link-port-branch cl) link)
			     (setf (inferior-file-link-target-branch cl)
				   link))
			 (maybe-remove-file-links cl superior))
			(t (let ((bl (link-match
				      link (if port-branch? sup-pbl sup-tbl))))
			     (cond ((null bl)
				    (warn "Don't know how to relink ~A" link))
				   (t	; prefer the upper
				    (unless (null(file-branch-link-box link))
				      (setf (file-branch-link-box bl)
					    (file-branch-link-box link)))
				    (setf (file-branch-link-lowest bl)
					  (file-branch-link-lowest link))
				    (setf (file-branch-link-inferior-link link)
					  (file-branch-link-inferior-link bl))
				    (if port-branch?
					(setq new-pbl
					      (nsubstitute link bl new-pbl))
					(setq new-tbl
					      (nsubstitute
					       link bl new-tbl)))
				    ;; branch link may already be connected
				    (unless (null
					     (file-branch-link-inferior-link
					      bl))
				      (maybe-remove-file-links
				       (file-branch-link-inferior-link
					bl)))))))))))))
      (dolist (bl (cross-file-port-branch-links self)) (link-handler bl t))
      (dolist (bl (cross-file-target-branch-links self)) (link-handler bl nil))
      (set-cross-file-port-branch-links   superior new-pbl)
      (set-cross-file-target-branch-links superior new-tbl))))

(defmethod cross-file-port-insert-self-action ((self boxer::box) superior)
  (let ((sup-cl (cross-file-contained-links superior))
	(sup-pbl (cross-file-port-branch-links superior))
	(link (car (cross-file-port-branch-links self))))
    (cond ((null link) )
	  ((not (null (file-branch-link-inferior-link link)))
	   (relink-file-link (file-branch-link-inferior-link link)))
	  (t (let ((cl (link-match link sup-cl)))
	       (cond ((not (null cl))
		      ;; made it to the top
		      (setf (file-branch-link-inferior-link link) cl)
		      (setf (inferior-file-link-port-branch cl) link)
		      (maybe-remove-file-links cl superior))
		     (t			; part of a branch
		      (let ((bl (link-match link sup-pbl)))
			(cond ((null bl)
			       (warn "Unable to relink cross file  port" self))
			      (t
			       (setf (file-branch-link-box bl)
				     (or (file-branch-link-box link) self))
			       (setf (file-branch-link-inferior-link link)
				     (file-branch-link-inferior-link bl))
			       (delete-cross-file-port-branch-link superior
								   bl)
			       (add-cross-file-port-branch-link superior link)
			       (unless (null
					(file-branch-link-inferior-link bl))
				 (maybe-remove-file-links
				  (file-branch-link-inferior-link
				   bl)))))))))))))

;;; remove everything on this branch which is being deleted from the
;;; current box on up
;;;
;;; then mark as "broken" the lowest box in the other branch
;;; so that (possible) future inserts will have an existing
;;; link to connect to but immediate uses of FILE will not be
;;; confused and try to file this particular link
;;;
;;; superiors of the other branch should then be removed as well

(defmethod cross-file-link-delete-self-action ((self boxer::box) superior)
  (dolist (pbl (cross-file-port-branch-links self))
    (remove-file-branch superior pbl t)
    (let* ((il (file-branch-link-inferior-link pbl))
	   (tl (when il (inferior-file-link-target-branch il))))
      (unless (null tl)
	(remove-file-branch (superior-box (file-branch-link-lowest tl)) tl nil)
	(break-file-links (file-branch-link-lowest tl) tl nil))))
  (dolist (tbl (cross-file-target-branch-links self))
    (remove-file-branch superior tbl nil)
    (let* ((il (file-branch-link-inferior-link tbl))
	   (pl (when il (inferior-file-link-port-branch il))))
      (unless (null pl)
	(remove-file-branch (superior-box (file-branch-link-lowest pl)) pl t)
	(break-file-links (file-branch-link-lowest pl) pl t)))))

(defmethod cross-file-port-delete-self-action ((self boxer::box) superior)
  (let ((pbl (car (cross-file-port-branch-links self))))
    (unless (null pbl)
      (remove-file-branch superior pbl t)
      (let* ((il (file-branch-link-inferior-link pbl))
	     (tl (when il (inferior-file-link-target-branch il))))
	;; and the top part of the target link if it exists
	(unless (null tl)
	  (remove-file-branch (superior-box (file-branch-link-lowest tl))
			      tl nil)
	  (break-file-links (file-branch-link-lowest tl) tl nil))))))



;;; cross file port utilities

(defun unique-cross-file-link-id ()
  (prog1 *cross-file-link-id-counter*
    (incf *cross-file-link-id-counter*)))

;;; note that ports are unique to each link whereas targets can
;;; have many links
(defun get-link-id (link)
  (let ((existing-entry (gethash (link-port link) *link-id-table*)))
    (cond ((null existing-entry)
	   (let ((new-id (unique-cross-file-link-id)))
	     (setf (gethash (link-port link) *link-id-table*) new-id)
	     new-id))
	  (t existing-entry))))

;;; this attribute needs to be saved away in the top level
;;; world (during logout) and restored on login to insure
;;; continuing uniqueness
(defun initialize-cross-file-link-id (id)
  (setq *cross-file-link-id-counter* id))

(eval-when (eval)
(deffile-property-handler :max-cross-file-link-id id
  (debugging-message "Initializing Cross File Link Counter to ~A" id)
  (initialize-cross-file-link-id id))
)

;;; print functions
(defun  %print-file-port-branch-link (obj stream &rest ignore)
  (declare (ignore ignore))
  (format stream "#<File Port Link ~D (~A)"
	  (cross-file-link-id obj) (file-branch-link-box obj)))

(defun  %print-file-target-branch-link (obj stream &rest ignore)
  (declare (ignore ignore))
  (format stream "#<File Target Link ~D (~A)"
	  (cross-file-link-id obj) (file-branch-link-box obj)))

;;; this gets called at each level.  If it looks like speed is
;;; a problem, we can hash the results of the tree walk into a
;;; per transaction table to speed the search
(defun lowest-inferior-link? (inflink box)
  (eq box (find-lowest-common-superior-box (box::link-port inflink)
					   (box::link-target inflink))))

(defun cross-file-link? (blink)
  (not (eq (superior-storage-box (link-port blink))
	   (superior-storage-box (link-target blink)))))

(defun link-match (link link-list)
  (car (member link link-list :test #'file-link-=)))

(defun file-link-= (l1 l2)
  (when (and (%cross-file-link? l1) (%cross-file-link? l2))
    (= (cross-file-link-id l1) (cross-file-link-id l2))))

;;; accessors and mutators
(defun cross-file-contained-links (box)
  (unless (null box) (getprop box :cross-file-contained-links)))

(defun cross-file-port-branch-links (box)
  (unless (null box) (getprop box :cross-file-port-branch-links)))

(defun cross-file-target-branch-links (box)
  (unless (null box) (getprop box :cross-file-target-branch-links)))

(defun no-cross-file-links? (box)
  (let ((plist (box::plist box)))
    (and (null (getf plist :cross-file-contained-links))
	 (null (getf plist :cross-file-port-branch-links))
	 (null (getf plist :cross-file-target-branch-links)))))


(defun set-cross-file-contained-links (box newlinks)
  (unless (null box)
    (if (null newlinks)
	(removeprop box :cross-file-contained-links)
	(putprop box newlinks :cross-file-contained-links))))

(defun set-cross-file-port-branch-links (box newlinks)
  (unless (null box)
    (if (null newlinks)
	(removeprop box :cross-file-port-branch-links)
	(putprop box newlinks :cross-file-port-branch-links))))

(defun set-cross-file-target-branch-links (box newlinks)
  (unless (null box)
    (if (null newlinks)
	(removeprop box :cross-file-target-branch-links)
	(putprop box newlinks :cross-file-target-branch-links))))


(defun add-cross-file-contained-link (box link)
  (unless (null box)
    (let ((cfcl (getprop box :cross-file-contained-links)))
      (unless (member link cfcl :test #'file-link-=)
	(putprop box (nconc cfcl (list link)) :cross-file-contained-links)))))

(defun add-cross-file-port-branch-link (box link)
  (unless (null box)
    (let ((cfpbl (getprop box :cross-file-port-branch-links)))
      (unless (member link cfpbl :test #'file-link-=)
	(putprop box (nconc cfpbl (list link)) :cross-file-port-branch-links)))))

(defun add-cross-file-target-branch-link (box link)
  (unless (null box)
    (let ((cftbl (getprop box :cross-file-target-branch-links)))
      (unless (member link cftbl :test #'file-link-=)
	(putprop box (nconc cftbl (list link)) :cross-file-target-branch-links)))))


(defun delete-cross-file-contained-link (box link)
  (unless (null box)
    (let ((new (delete (cross-file-link-id link)
		       (getprop box :cross-file-contained-links)
		       :test #'(lambda (id ln)
				 (= id (cross-file-link-id ln))))))
      (if (null new)
	  (removeprop box :cross-file-contained-links)
	  (putprop box new :cross-file-contained-links)))))

(defun delete-cross-file-port-branch-link (box link)
  (unless (null box)
    (let ((new (delete (cross-file-link-id link)
		       (getprop box :cross-file-port-branch-links)
		       :test #'(lambda (id ln)
				 (= id (cross-file-link-id ln))))))
      (if (null new)
	  (removeprop box :cross-file-port-branch-links)
	  (putprop box new :cross-file-port-branch-links)))))

(defun delete-cross-file-target-branch-link (box link)
  (unless (null box)
    (let ((new (delete (cross-file-link-id link)
		       (getprop box :cross-file-target-branch-links)
		       :test #'(lambda (id ln)
				 (= id (cross-file-link-id ln))))))
      (if (null new)
	  (removeprop box :cross-file-target-branch-links)
	  (putprop box new :cross-file-target-branch-links)))))


;; a port is a cross file port if the port and the target have
;; are inside (or in the case of the target, is) different file
;; boxes.

;; just walk up the hierarchy until we get to the containing
;; storage-chunk box, then check to see if the port is on a
;; branch-link or an inferior-link
(defun cross-file-port? (port-box)
  (flet ((in-links? (links)
	   (member port-box links
		   :test #'(lambda (item list-item)
			     (eq item (box::link-port list-item))))))
    (do ((sb (superior-box port-box) (superior-box sb)))
	((not (box? sb)) nil)
      (when (storage-chunk? sb)
	(cond ((in-links? (slot-value sb 'box::branch-links))
	       (return T))
	      ((in-links? (slot-value sb 'box::contained-links))
	       (return NIL)))))))

;;; Editor Interface

;; NOTE: server errors can be signalled from inside fill-box-from-server
;; to be safe, callers must either wrap it in side a with-server-errors
;; or else be underneath a primitive
(defmethod fill-box-from-bfs-server ((self box::box))
  (when (box::getprop self :server-box-id)
    (let* ((id (box::getprop self :server-box-id))
	   (new-id (validate-server-box-id self id)))
      (unless (null new-id)
	(setq id new-id))
      (load-binary-box-internal id self)
      ;; need to patch up name row to avoid re-insertion of name/box into
      ;; the superior binding alist
      (when (box::name-row? (slot-value self 'box::name))
	(setf (slot-value (slot-value self 'box::name) 'box::cached-name)
	      (box::get-box-name (slot-value self 'box::name))))
      (box::modified self)
      (mark-timestamp self))))

(defun record-copy-file-info (from-box to-box)
  (let ((old-file-id (boxer::getprop from-box :server-box-id)))
    (unless (null old-file-id)
      (boxer::putprop to-box old-file-id :copy-box-id))))

(defun fake-file-superior? (info)
  (let* ((sup-bid (unless (null info) (sbi-superior-box-bid info)))
	 (sup-file-box (unless (null sup-bid) (bid-box sup-bid))))
    (unless (null sup-file-box)
      (boxer::fake-file-box sup-file-box))))

(defun queue-for-server-deletion (box)
  (let* ((info (box-server-info box nil))
	 (bid (if (null info)
		  (box::getprop box :server-box-id)
		  (sbi-bid info))))
    (cond ((null bid))
	  ((box::superior? box box::*initial-box*)
	   (warn "~A is still connected to the hierarchy" box))
	  ((and info
		;; Watch out for inferiors of Read Only and Foreign Boxes
		(or (and (foreign-world-server? (sbi-server info))
			 (fake-file-superior? box))
		    (not (writeable-server? (sbi-server info)))
		    ;; if any superior is read-only, then don't delete
		    (do ((fbox box
			       ;; can't walk up box structure since it has
			       ;; already been severed
			       (let* ((info (box-server-info fbox))
				      (sup-bid (unless (null info)
						 (sbi-superior-box-bid info))))
				 (and sup-bid (bid-box sup-bid)))))
			((not (box? fbox)) nil)
		      (when (read-only-box? fbox) (return T))))))
	  ((box::fast-memq box *box-bid-delete-list*))
	  ((numberp bid)
	   (unless (null box::*boxer-system-hacker*)
	     (format t "~%Queueing ~A (~D, #x~X)" box bid bid))
	   (push box *box-bid-delete-list*))
	  (t (error "BID, ~A, is not a number" bid)))))

;; should update the file-inferiors and possibly promote the
;; cross file port target tables of the superior storage-chunk

(defmethod storage-chunk-delete-self-action ((self box::box))
  )

(defmethod storage-chunk-insert-self-action ((self box::box))
  )



;;;; Box Server Primary Interface Functions
;;;; to be used by Primitives and Editor Commands

(defun mark-box-flags-as-file (box &optional
			     (load-on-login? nil)
			     (read-only? nil))
  (setf (box::storage-chunk? box) t
	;; setup some defaults...
	(box::load-box-on-login? box) load-on-login?
	(read-only-box? box) read-only?))

(defun unmark-box-flags-as-file (box)
  (setf (box::storage-chunk? box) nil
	(box::load-box-on-login? box) nil
	(read-only-box? box) nil
	(box::copy-file? box) nil))


(defun make-file-control-box (name current-value update-fn)
  (let ((trigger (box::make-box `((,update-fn))
				'box::doit-box
				"Modified-Trigger"))
	(interface-box (box::make-box `((,current-value))
				      'box::data-box
				      name)))
    (box::add-closet-row interface-box (box::make-row `(,trigger)))
    interface-box))


(defun install-file-control-boxes (box)
  (let ((closet (box::closet-row box nil)))
    (unless (boxer-eval::lookup-static-variable-in-box-only box 'bu::load-on-login?)
      (box::append-cha closet
		       (make-file-control-box "Load-on-Login?"
					      (if (box::load-box-on-login? box)
						  "True" "False")
					      "Update-Login-Action")))
    (unless (boxer-eval::lookup-static-variable-in-box-only box 'bu::save-changes?)
      (box::append-cha closet
		       (make-file-control-box "Save-Changes?"
					      (if (read-only-box? box)
						  "False" "True")
					      "Update-Save-Changes")))
    box))

(defun remove-file-control-boxes (box)
  (declare (ignore box))
  )
