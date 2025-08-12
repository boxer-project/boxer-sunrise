;;; -*- Mode:lisp;Syntax:Common-Lisp; Package:BOXER; Base:10.-*-

#|


 $Header: edvc.lisp,v 1.0 90/01/24 22:10:47 boxer Exp $

 $Log:	edvc.lisp,v $
;;;Revision 1.0  90/01/24  22:10:47  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                          +-Data--+
                 This file is part of the | BOXER | system
                                          +-------+

    This file contains procedures that manipulate editor datastructure for the
    benefit of the virtual copy mechanism.  The General flavor of these
    functions are various caching strategies to speed up virtual copy.  See
    the files chunker.lisp and printer.lisp for more info on the
    evaluator/editor datastructure interface.


Modification History (most recent at top)
11/01/09 get-graphics-info-for-newvc, calls to graphics sheet changed to graphics-info with type checking
         no more :associated-turtle slot, use graphics-info instead
 3/20/02 virtual-copy-rows
 1/07/01 merged with Mac release
 6/10/99 virtual-copy-editor-box check for modified rentry BEFORE synthesising
         a default pedigree
 3/19/99 virtual-copy-editor-box fix bug where pedigree of the new VC was
         using the row-entry creation time instead of (now).
 8/23/98 virtual-copy-editor-box changed to grab new VC creation time from
         the rows entry instead of (now) in order to capture the correct
         rows version of deep inferior mods made between rows entry creation and
         virtual copy creation.
 5/22/98 lookup-variable-in-vc-rows-entry-1: put in check to include boxer functions
         along with closet items for cached bindings
 5/22/98 Started logging changes: source = Boxer Version 2.3alpha

|#

(in-package :boxer)



#|

            PORT LINK CACHING (these comments are semi-obsolete)

The current theory on virtual copy demands that we copy the part of the
subtree that contains port links in order to assure that every port that
is suppose to, has a unique target.  Since it would be prohibitively slow
to walk the entire (potentially huge) tree of a boxes inferiors at copy
time, we cache information in the editor to guide the articulation of the
relevant portion of the subtree.

In particular, at any given level of the hierarchy, we have a list of (which is
a subset of all) the inferiors that may contain portions of the tree that need
to be articulated at copy time.  This list is constructed by walking up the
tree at port/target insertion time.  This specifies at any level, all of the
inferiors that contain either ports or port targets.

However, reliance on just this information may result in copying branches of
the tree that don't actually NEED to be copied.  Consider the following:

             Example 1             Example 2

                A                     A         E
               / \                   / \       / \
              /   \                 /   \     F   G
             B     C               B     C
            / \    |             / \     |
           /   \   |            /   \    |
          D    pD  pD          D    pD  pE

"pX" refers to a node which is a port to the second letter.  That is,
"pD" is a port to node "D".  What we want to do is to copy A in both
cases.  Relying solely on inferior port/target information will cause
the entire subtree of A to be copied in both cases.  However, in
Example 2, this would result in the unneccessary copying of the "C"
subtree.

An alternative to caching the inferior port/target information, is to keep
track of inferior links.  Whenever we make a port/target pair, we find their
lowest common superior box and propagate that information upward.  Each
port/target link is described in terms of the superior box which it spans
and each level of box has information about all of the links which exist
within it.  At copy time, the link list is bound and is used as a guide
to the lower levels of inferior port/target information to decide if it
is neccessary to articulate a particular branch.

It may turn out that straight copying based on inferiors is sufficient
and that the smarter link based copying is not worth the extra effort.
We need to evaluate the tradeoffs between size/complexity of the
average datastructure.

|#



(defsubst link-equal (link1 link2)
  (and (eq (link-port link1) (link-port link2))
       (eq (link-target link1) (link-target link2))))

(defsubst inferior-link? (link)
  (with-link-type-checking (link)
    (eq (link-type link) 'inferior-link)))

(defsubst port-branch-link? (link)
  (with-link-type-checking (link)
    (eq (link-type link) 'port-branch-link)))

(defsubst target-branch-link? (link)
  (with-link-type-checking (link)
    (eq (link-type link) 'target-branch-link)))

;; this should replace (delete <mumble> <mumble> :test #'link-equal)
(defun delete-link-from-list (link list)
  (declare (ignore link list))
  nil
  )

(defun INFORM-PORT-THAT-TARGET-IS-GOING-AWAY (port)
  (unless *boxes-being-temporarily-deleted*
    (putprop port t :cracked-port)
;    (let* ((old-name-row (name-row port))
;	   (old-name (unless (null old-name-row) (cached-name old-name-row)))
;	   (new-name-row (if (name-row port)
;			     (make-name-row (list "CRACKED PORT:" (name port)))
;			     (make-name-row (list "CRACKED PORT")))))
;      (unless (null old-name) (setf (cached-name new-name-row) old-name))
;      (set-name port new-name-row))
    ;; quick redisplay hack, should really fix up
    ;; CHECK-AND-HANDLE-BORDER-CHANGES to recognize changes in port crackness
    ;; (dolist (sb (screen-objs port)) (set-force-redisplay-infs? sb))
    (modified port)))

;;; the converse...
(defun inform-port-that-target-has-returned (port)
  (unless *boxes-being-temporarily-deleted*
    (removeprop port :cracked-port)
;    (let ((name-row (name-row port)))
;      (unless (null name-row)
;	(let* ((name (text-string name-row))
;	       (old-name-cache (cached-name name-row))
;	       (cracked? (unless (null name)
;			   (search "CRACKED PORT" name))))
;	  (cond ((and cracked?
;		      (> (length name) #.(length "CRACKED PORT")))
;		 ;; this means that the port HAD a name before it
;		 ;; was cracked so restore the original name
;		 (let ((new-name-row (make-name-row
;				      (list (subseq
;					     name
;					     #.(length "CRACKED PORT: "))))))
;		   (setf (cached-name new-name-row) old-name-cache)
;		   (set-name port new-name-row)
;		   (modified port)))
;		(cracked?
;		 ;; the port didn't have a previous name so remove the name tab
;		 (set-name port nil))))))
    ;; (dolist (sb (screen-objs port)) (set-force-redisplay-infs? sb))
    (modified port)))

(defun cracked-port? (port)
  (and (port-box? port) (getprop port :cracked-port)))



;;; Virtual Copy Statistics
;; there are a bunch of tradeoffs in virtual copy that I dobnt understand yet.
;; In particular, copying ONLY the parts of the tree that NEED
;; to be copied may be more expensive than the extra copying.  We need to get
;; some idea about what the average data type is before we can understand
;; which implementation strategy will be the better one.  Probably some sort
;; of hybrid will work best but we still need the information in order to know
;; when to do which
;;
;; We may want to keep track of vc statistics for several kinds of boxer
;; interactions.  For example, primarily database stuff versus primarily
;; turtle graphics.
;;
;; still need to get some stats on the tradeoff between copying rows
;; and extending inferior multi-pointers

(defun init-vc-statistics (&optional description)
  (unless (null *current-vc-recording-structure*)
    (push *current-vc-recording-structure* *old-vc-recording-structures*))
  (if (null description)
      (setq *current-vc-recording-structure* (make-virtual-copy-statistic))
      (setq *current-vc-recording-structure* (make-virtual-copy-statistic
                                               :description description))))

(eval-when (load)
  (init-vc-statistics)
  )

(defun print-vc-report (&optional (data *current-vc-recording-structure*)
                        (stream *standard-output*))
  ;; report top level info
  (format stream "~%~%~A~%~%~
                  Total Number of Editor Boxes Copied: ~D~%~
                  Total Number of Copies Copied: ~D~%~
                  Number of Copies With Inferior Links: ~D~%~
                  Link Discrimination is: ~S~%"
          (vcs-description data)
          (vcs-top-level-copies data)
          (vcs-vc-of-vcs data)
          (- (vcs-top-level-copies data)
             (vcs-portless-copies data))
          (vcs-link-discrimination-on? data))
  ;; report info about guts of sub-tree articulation
  (format stream "~D Nodes were searched and ~%~
                  ~D Nodes were articulated.~%~
                  The Average search list was ~D Entries long~%"
          (vcs-nodes-searched data)
          (vcs-nodes-articulated data)
          (let ((inf-node-copies (- (+ (vcs-top-level-copies data)
                                       (vcs-vc-of-vcs data))
                                    (vcs-portless-copies data))))
            (if (zerop inf-node-copies)
                0
                (/ (vcs-acc-link-list-length data)
                   (float inf-node-copies)))))
  ;; report on pointer statistics
  (format stream "Pointer Statistics:~%~
                  ~D Single Pointer References~%~
                  ~D Multi  Pointer References~%~
                  The Average Multi-Pointer Length was ~D~%~
                  ~D Rows Had their Caches Flushed due to MP Growth~%"
          (vcs-single-ptr-refs data)
          (vcs-multi-ptr-refs data)
          (if (zerop (vcs-multi-ptr-refs data)) 0
              (/ (vcs-multi-ptr-acc-length data)
                 (float (vcs-multi-ptr-refs data))))
          (vcs-rows-flushed-by-mpg data))
  ;; other stuff...
;  (format stream "~D VC's were consed for Editor Box Row Entries"
;	  (vcs-row-entry-vcs data))
  ;; var lookup info
  (format stream "Variable Lookup Statistics:~%~
                  ~D Virtual Copy Variable Lookups consisting of:~%~
                  ~D Cache Hits in Virtual Copy Variable Lookups~%~
                  ~D Cache Misses in Virtual Copy Variable Lookups~%~
                  ~D Caches were filled in Virtual Copy Variable Lookups~%~
                  ~D Changed Box Variable Lookups consisting of:~%~
                  ~D Cache Hits in Changed Box Variable Lookups~%~
                  ~D Cache Misses in Changed Box Variable Lookups~%~
                  ~D Caches were filled in Changed Box Variable Lookups"
          (vcs-vc-var-lookups data)
          (vcs-vc-var-lookup-cache-hits data)
          (vcs-vc-var-lookup-cache-misses data)
          (vcs-vc-var-lookup-cache-fills data)
          (vcs-vcre-var-lookups data)
          (vcs-vcre-var-lookup-cache-hits data)
          (vcs-vcre-var-lookup-cache-misses data)
          (vcs-vcre-var-lookup-cache-fills data))
  (values))

(defun print-full-vc-report (&optional (stream *standard-output*))
  (dolist (old-stat (reverse *old-vc-recording-structures*))
    (print-vc-report old-stat stream))
  (print-vc-report *current-vc-recording-structure* stream))



(defmethod insert-self-link-action ((box box) &optional (superior (superior-box box)))
  (let ((ports (slot-value box 'ports))
        (saved-inferior-ports   (getprop box 'saved-inferior-ports))
        (saved-inferior-targets (getprop box 'saved-inferior-targets)))
    ;; Handle the link caching for the box itself
    (dolist (port ports)
      (let ((common-node (find-lowest-common-superior-box box port)))
        (cond ((not (null common-node))
               ;; cache inferior-links starting from the common node
               (cache-link common-node (make-link 'inferior-link port box))
               ;; then cache 'port-branch-links starting from the port branch
               ;; this should terminate when it reaches the common superior
               (cache-link port (make-link 'port-branch-link port box))
               ;; finally cache 'target-branch-links starting from the target
               ;; branch.  This should also terminate at the common-node
               (cache-link box (make-link 'target-branch-link port box)))
              ((not (null superior))
               ;; save the target for possible future action
               (putprop superior (append (getprop superior 'saved-inferior-targets)
                                         (list box))
                        'saved-inferior-targets)))))
    ;; now handle any inferior links the box may have
    (dolist (il (contained-links box)) (unless (null superior) (cache-link superior il)))
    ;; Now handle any saved ports or targets
    (dolist (p saved-inferior-ports)
      (let* ((targ (ports p)) (common-node (find-lowest-common-superior-box p targ)))
        ;; cache the appropriate links...
        (unless (null common-node)
          ;; cache inferior-links starting from the common node
          (cache-link common-node (make-link 'inferior-link p targ))
          ;; then cache 'port-branch-links starting from the port branch
          ;; this should terminate when it reaches the common superior
          (cache-link p (make-link 'port-branch-link p targ))
          ;; finally cache 'target-branch-links starting from the target
          ;; branch.  This should also terminate at the common-node
          (cache-link targ (make-link 'target-branch-link p targ)))
        ;; now connect the port to it's target
        (let ((pt (ports p)))
          (unless (null pt) (add-port pt p)))))
    (dolist (targ saved-inferior-targets)
      (dolist (p (ports targ))
        (let ((common-node (find-lowest-common-superior-box box p)))
          (unless (null common-node)
            ;; cache inferior-links starting from the common node
            (cache-link common-node (make-link 'inferior-link p targ))
            ;; then cache 'port-branch-links starting from the port branch
            ;; this should terminate when it reaches the common superior
            (cache-link p (make-link 'port-branch-link p targ))
            ;; finally cache 'target-branch-links starting from the target
            ;; branch.  This should also terminate at the common-node
            (cache-link targ (make-link 'target-branch-link p targ))))
        ;; reconnect any ports
        (inform-port-that-target-has-returned p)))))

(defmethod insert-self-link-action ((box port-box) &optional superior)
  (let* ((ports (slot-value box 'ports))
         (common-node (and ports (find-lowest-common-superior-box box ports))))
    (cond ((not (null common-node))
           ;; cache inferior-links starting from the common node
           (cache-link common-node (make-link 'inferior-link box ports))
           ;; then cache 'port-branch-links starting from the port
           ;; this should terminate when it reaches the common superior
           (cache-link box (make-link 'port-branch-link box ports))
           ;; finally cache 'target-branch-links starting from the target
           ;; branch.  This should also terminate at the common-node
           (cache-link ports (make-link 'target-branch-link box ports)))
          ((not (null superior))
           ;; save the port for possible future action
           (putprop superior (list box) 'saved-inferior-ports)))))


#|
(defmethod insert-self-action ((box virtual-copy-subclass) &optional superior)
  (let ((ports (slot-value (the box box) 'ports))
        (saved-inferior-ports   (getprop box 'saved-inferior-ports))
        (saved-inferior-targets (getprop box 'saved-inferior-targets)))
    ;; If the box is either a Port or a port target, then we need to...
    ;; Handle the port caching of the box itself
    (cond ((null ports))
          ((consp ports)
           ;; if ports is a list, then we are dealing with a target
           (dolist (port ports)
             (let ((common-node (find-lowest-common-superior-box box port)))
               (unless (null common-node)
                 ;; cache inferior-links starting from the common node
                 (cache-link common-node (make-link 'inferior-link port box))
                 ;; then cache 'port-branch-links starting from the port branch
                 ;; this should terminate when it reaches the common superior
                 (cache-link port (make-link 'port-branch-link port box))
                 ;; finally cache 'target-branch-links starting from the target
                 ;; branch.  This should also terminate at the common-node
                 (cache-link box (make-link 'target-branch-link port box))))))
          (t
           ;; must be a port then...
           (let ((common-node (find-lowest-common-superior-box box ports)))
             (unless (null common-node)
               ;; cache inferior-links starting from the common node
               (cache-link common-node (make-link 'inferior-link box ports))
               ;; then cache 'port-branch-links starting from the port
               ;; this should terminate when it reaches the common superior
               (cache-link box (make-link 'port-branch-link box ports))
               ;; finally cache 'target-branch-links starting from the target
               ;; branch.  This should also terminate at the common-node
               (cache-link ports (make-link 'target-branch-link box ports))))))

    ;; Now handle any inferior links that the box may have
    (dolist (il (contained-links box))
      (unless (null superior)
        (cache-link superior il)))

    ;; now handle any saved ports or targets
    ;; they need to be relinked
    (dolist (p saved-inferior-ports)
      (let* ((targ (ports p))
             (common-node (find-lowest-common-superior-box p targ)))
        ;; cache the appropriate links...
        (unless (null common-node)
               ;; cache inferior-links starting from the common node
               (cache-link common-node (make-link 'inferior-link p targ))
               ;; then cache 'port-branch-links starting from the port branch
               ;; this should terminate when it reaches the common superior
               (cache-link p (make-link 'port-branch-link p targ))
               ;; finally cache 'target-branch-links starting from the target
               ;; branch.  This should also terminate at the common-node
               (cache-link targ (make-link 'target-branch-link p targ)))
        ;; now connect the port to it's target
        (add-port (ports p) p)))
    (dolist (targ saved-inferior-targets)
      (dolist (p (ports targ))
        (let ((common-node (find-lowest-common-superior-box box p)))
          (unless (null common-node)
            ;; cache inferior-links starting from the common node
            (cache-link common-node (make-link 'inferior-link p targ))
            ;; then cache 'port-branch-links starting from the port branch
            ;; this should terminate when it reaches the common superior
            (cache-link p (make-link 'port-branch-link p targ))
            ;; finally cache 'target-branch-links starting from the target
            ;; branch.  This should also terminate at the common-node
            (cache-link targ (make-link 'target-branch-link p targ))))
        ;; reconnect any ports
        (inform-port-that-target-has-returned p)))))
|#



(defmethod delete-self-link-action ((box box)
                                    &optional (superior (superior-box box)))
  (let ((ports (slot-value box 'ports))
        (branch-links (slot-value box 'branch-links)))
    ;; handle port link caching for the box itself...
    (dolist (port ports)
      (let ((common-node (find-lowest-common-superior-box box port)))
        (unless (null common-node)
          ;; remove inferior-links starting from the common node
          (remove-link common-node (make-link 'inferior-link port box))
          ;; remove 'port-branch-links starting from the port branch
          ;; this should terminate when it reaches the common superior
          (remove-link port (make-link 'port-branch-link port box))
          ;; remove 'target-branch-links starting from the target
          ;; branch.  This should also terminate at the common-node
          (remove-link box (make-link 'target-branch-link port box)))))
    ;; Now remove all inferior links from superiors of this Box
    (dolist (il (contained-links box))
      (unless (null superior) (remove-link superior il)))
    ;; now handle branches
    ;; all if the links in which this box was a branch, are no longer valid
    ;; and we have to remove them from all the caches that they are a member of
    ;; we also have to preserve inferior port/target info in case we want to
    ;; insert this box back into the hierarchy.  Stuff this info onto the PLIST
    (let ((saved-inferior-ports   nil)
          (saved-inferior-targets nil))
      (dolist (bl branch-links)
        (cond ((port-branch-link? bl)
               ;; remove the link from the port side...
               (remove-link (link-port bl) bl)
               ;; and the target side...
               (remove-link (link-target bl) bl)
               ;; now inform the target that we are going away
               (remove-port (link-target bl) (link-port bl))
               ;; finally, push the port onto the save list
               (push (link-port bl) saved-inferior-ports))
              ((target-branch-link? bl)
               ;; remove the link from the port side...
               (remove-link (link-port bl) bl)
               ;; and the target side...
               (remove-link (link-target bl) bl)
               ;; finally, push the target onto the save list
               ;; tell all the ports about it
               (inform-port-that-target-is-going-away (link-port bl))
               (push (link-target bl) saved-inferior-targets))))
      ;; now put the saved lists in the boxes plist for (possible) use later
      (putprop box saved-inferior-ports   'saved-inferior-ports)
      (putprop box saved-inferior-targets 'saved-inferior-targets))))

(defmethod delete-self-link-action ((box port-box) &optional superior)
  (let ((ports (slot-value box 'ports)))
    ;; handle the port link caching for the port itself
    (let ((common-node (find-lowest-common-superior-box box ports)))
      (unless (null common-node)
        ;; remove inferior-links starting from the common node
        (remove-link common-node (make-link 'inferior-link box ports))
        ;; then remove 'port-branch-links starting from the port branch
        ;; this should terminate when it reaches the common superior
        (remove-link box (make-link 'port-branch-link box ports))
        ;; finally remove 'target-branch-links starting from the target
        ;; branch.  This should also terminate at the common-node
        (remove-link ports
                     (make-link 'target-branch-link box ports))))))

#|
(defmethod delete-self-action ((box virtual-copy-subclass) &optional superior)
  (let ((ports (slot-value (the box box) 'ports))
        (branch-links (slot-value box 'branch-links)))
    ;; If the box is either a Port or a port target, then we need to...
    ;; Handle Port Caching for the Box itself first
    (cond ((null ports))
          ((consp ports)
           ;; if ports is a list, then we are dealing with a target
           (dolist (port ports)
             (let ((common-node (find-lowest-common-superior-box box port)))
               (unless (null common-node)
                 ;; remove inferior-links starting from the common node
                 (remove-link common-node (make-link 'inferior-link port box))
                 ;; remove 'port-branch-links starting from the port branch
                 ;; this should terminate when it reaches the common superior
                 (remove-link port (make-link 'port-branch-link port box))
                 ;; remove 'target-branch-links starting from the target
                 ;; branch.  This should also terminate at the common-node
                 (remove-link box (make-link 'target-branch-link port box))))))
          (t
           ;; must be a port then...
           (let ((common-node (find-lowest-common-superior-box box ports)))
             (unless (null common-node)
               ;; remove inferior-links starting from the common node
               (remove-link common-node (make-link 'inferior-link box ports))
               ;; then remove 'port-branch-links starting from the port branch
               ;; this should terminate when it reaches the common superior
               (remove-link box (make-link 'port-branch-link box ports))
               ;; finally remove 'target-branch-links starting from the target
               ;; branch.  This should also terminate at the common-node
               (remove-link ports
                            (make-link 'target-branch-link box ports))))))

    ;; Now remove all inferior links from superiors of this Box
    (dolist (il (contained-links box))
      (unless (null superior)
        (remove-link superior il)))

    ;; now handle branches
    ;; all if the links in which this box was a branch, are no longer valid
    ;; and we have to remove them from all the caches that they are a member of
    ;; we also have to preserve inferior port/target info in case we want to
    ;; insert this box back into the hierarchy.  Stuff this info onto the PLIST
    (let ((saved-inferior-ports   nil)
          (saved-inferior-targets nil))
      (dolist (bl branch-links)
        (cond ((port-branch-link? bl)
               ;; remove the link from the port side...
               (remove-link (link-port bl) bl)
               ;; and the target side...
               (remove-link (link-target bl) bl)
               ;; now inform the target that we are going away
               (remove-port (link-target bl) (link-port bl))
               ;; finally, push the port onto the save list
               (push (link-port bl) saved-inferior-ports))
              ((target-branch-link? bl)
               ;; remove the link from the port side...
               (remove-link (link-port bl) bl)
               ;; and the target side...
               (remove-link (link-target bl) bl)
               ;; inform the ports that the target is going away in case we
               ;; want to mark them broken or whatever
               (dolist (p (ports (link-target bl)))
                 (inform-port-that-target-is-going-away p))
               ;; finally, push the target onto the save list
               (push (link-target bl) saved-inferior-targets))))
      ;; now put the saved lists in the boxes plist for (possible) use later
      (putprop box saved-inferior-ports   'saved-inferior-ports)
      (putprop box saved-inferior-targets 'saved-inferior-targets))))
|#



(defmethod cache-link ((box virtual-copy-subclass) link
                       &optional (superior-row (superior-row box)))
  (if (inferior-link? link)
      (unless (member link (slot-value box 'contained-links)
                      :test #'link-equal)
        (push link (slot-value box 'contained-links))
        (when (row? superior-row)
          (cache-link superior-row link)))
      (unless (or (member link (slot-value box 'contained-links)
                          :test #'link-equal)
                  (member link (slot-value box 'branch-links)
                          :test #'link-equal))
        (push link (slot-value box 'branch-links))
        (when (row? superior-row)
          (cache-link superior-row link)))))

(defmethod cache-link ((row row) link
                       &optional (superior-box (superior-box row)))
  (unless (member link (slot-value row 'inferior-links) :test #'link-equal)
    (push link (slot-value row 'inferior-links)))
  ;; boxes are responsible for terminating
  (cache-link superior-box link))

(defmethod remove-link ((box virtual-copy-subclass) link
                        &optional (superior-row (superior-row box)))
  (if (inferior-link? link)
      (when (member link (slot-value box 'contained-links) :test #'link-equal)
        (setf (slot-value box 'contained-links)
              (delete link (slot-value box 'contained-links)
                      :test #'link-equal))
        (when (row? superior-row)
          (remove-link superior-row link)))
      (when (or (member link (slot-value box 'branch-links)
                        :test #'link-equal)
                (member link (slot-value box 'contained-links)
                        :test #'link-equal))
        (setf (slot-value box 'branch-links)
              (delete link (slot-value box 'branch-links) :test #'link-equal))
        (when (row? superior-row)
          (remove-link superior-row link)))))

(defmethod remove-link ((row row) link
                        &optional (superior-box (superior-box row)))
  (when (member link (slot-value row 'inferior-links) :test #'link-equal)
    (setf (slot-value row 'inferior-links)
          (delete link (slot-value row 'inferior-links) :test #'link-equal)))
  (unless (null superior-box) (remove-link superior-box link)))

;;; These are useful for debugging

(defmethod clear-port-link-caches ((box virtual-copy-subclass))
  (setf (slot-value box 'contained-links) nil)
  (setf (slot-value box 'branch-links) nil)
  (do-box-rows ((row box))
    (clear-port-link-caches row)))

(defmethod clear-port-link-caches ((row row))
  (setf (slot-value row 'inferior-links) nil)
  (do-row-chas ((cha row))
    (when (box? cha)
      (clear-port-link-caches cha))))




;;;; Functions that virtual copy will call

;;; This is the main conversion routine from EDITOR to VC at the row level.
;;  Note that the EDITOR-ROW has already (or will) cache the results of
;;  calling the CHUNKER.

;;; There are two possibilities for converting the chunks of an Editor row to
;;; an evrow.  These are based on the value of *links-to-be-copied*.  If
;;; a branch of a link that needs to be copied exists within the row, then the
;;; appropriate chunks that contain them need to be bashed to point to VC's
;;; instead of the boxes. Otherwise, the cached chunking information is valid
;;;  and we can just use it.

(defun make-evrow-from-row (row &optional sup-vc)
  #+lispworks (declare (values evrow inner-links?))
  (let ((cv? (slot-value row 'cached?))
        (chunks (slot-value row 'cached-chunks))
        (evrow? (or (slot-value row 'cached-evrow))))
    (labels ((cache-evrow (evrow)
               (setf (slot-value row 'cached-evrow) evrow)
               evrow)
             (new-chunk (val raw-chunk ptr)
               (let ((nc (copy-chunk raw-chunk)))
                 (setf (chunk-chunk nc) (virtual-copy-editor-box val))
                 (extend-pointer ptr sup-vc (tick) nc row)))
             (link-member? (link link-list)
               (member link link-list :test #'link-equal))
             ;;
             ;; There are three versions of maybe-fix-pointer corresponding to
             ;; 3 different strategies for for copying links
             ;; 1: The minimal work version is to copy any sub-box with non NIL
             ;;    links.  This will result in some number of unneccessary
             ;;    copies being created but MAY be faster
             ;; 2: Check to see if the sub-boxes links are a member of the
             ;;    top level link list of the box being copied.  This may be
             ;;    slow since we need to check the (possibly long) top level
             ;;    link list for each node with non NIL links
             ;; 3: Same as [2] except we prune the link list that we search
             ;;    at each level.  This may be slow since we MUST search
             ;;    the current link list for EVERY link in the current node
             ;;    rather than stopping when we find a match so that we can
             ;;    generate a correct and complete link list
             ;;    for the next level.
             ;;
             ;; Strategy 3: prune *links-to-be-copied* at every level....
             ;;
             (maybe-fix-pointer (pointer)
               ;; if the pointer is part of a link,then make a new one with the
               ;; appropriate substitutions, otherwise, just return the pointer
               (let* ((raw-chunk (get-pointer-value pointer nil))
                      (value (chunk-chunk raw-chunk)))
                 (cond ((or (numberp value) (symbolp value))
                        pointer)
                       ;; must be a box or port
                       (t
                        (let ((bl (slot-value value 'branch-links))
                              (cl (slot-value value 'contained-links)))
                          (cond ((and (null bl) (null cl))
                                 pointer)
                                (t
                                 (let ((relevant-links nil))
                                   (dolist (l cl)
                                     (when (link-member? l
                                                         *links-to-be-copied*)
                                       (push l relevant-links)))
                                   (dolist (l bl)
                                     (when (link-member? l
                                                         *links-to-be-copied*)
                                       (push l relevant-links)))
                                   (cond  ((null relevant-links)
                                           ;; no matches, so return the
                                           ;; original pointer
                                           (record-inferior-node-search)
                                           (record-search-list-length
                                             (length *links-to-be-copied*))
                                           pointer)
                                          (t
                                           ;; otherwise rebind
                                           ;; *links-to-be-copied* and continue
                                           ;; to articulate the tree
                                           (record-search-list-length
                                               (length *links-to-be-copied*))
                                           (let ((*links-to-be-copied*
                                                  ;; wonder how useful
                                                  ;; this nreverse is
                                                   (nreverse relevant-links)))
                                             (record-inferior-node-search)
                                             (record-inferior-node-copy value)
                                             (values (new-chunk
                                                       value
                                                       raw-chunk
                                                       pointer)
                                                     t))))))))))))
             ;; close the labels subform
             )
            ;; now make the evrow. Special case the situation when
            ;; no chunks need to be bashed
    (cond ((and cv? (null chunks))
           (or evrow? (cache-evrow (make-evrow :original-row row))))
          ((and cv?
                (null (cdr chunks))
                (only-formatting-chunk? (get-pointer-value (car chunks)
                                                           nil)))
           (or evrow?
               (cache-evrow (make-evrow :row-format
                                        (get-pointer-value (car chunks) nil)
                                        :original-row row))))
          ((and cv? (null (slot-value row 'inferior-links)))
           (or evrow?
               (cache-evrow (make-evrow :pointers chunks :original-row row))))
          (t
           (let ((chunk-was-bashed nil))
             (if (null evrow?)
                 (let ((new-chunks (if cv? chunks (chunks row t))))
                   (cond ((null new-chunks)
                          (values (make-empty-evrow) nil))
                         ((and (null (cdr new-chunks))
                               (only-formatting-chunk?
                                (get-pointer-value (car new-chunks)
                                                   nil)))
                          (values (make-evrow :row-format
                                              (get-pointer-value
                                               (car new-chunks) nil))
                                  nil))
                         (t
                          (values (make-evrow
                                   :pointers
                                   (with-collection
                                       (dolist (chunk new-chunks)
                                         (multiple-value-bind (ch new?)
                                             (maybe-fix-pointer chunk)
                                           (setq chunk-was-bashed
                                                 (or chunk-was-bashed
                                                     new?))
                                           (collect ch))))
                                   :original-row row)
                                  chunk-was-bashed))))
                 (progn
                   (dolist (p (evrow-pointers evrow?))
                     (multiple-value-bind (ignore new?)
                         (maybe-fix-pointer p)
                       (declare (ignore ignore))
                       (setq chunk-was-bashed
                             (or chunk-was-bashed new?))))
                   (values evrow? chunk-was-bashed)))))))))

;;;; other maybe-fix-pointer strategies...

             ;; Strategy 1: non NIL links are copied....
             ;;
;	     (maybe-fix-pointer (pointer)
;	       (let* ((raw-chunk (get-pointer-value pointer nil))
;		      (value (chunk-chunk raw-chunk)))
;		 (cond ((or (numberp value) (symbolp value))
;			pointer)
;		       ;; must be a port or a box
;		       (t
;			(cond ((and (null (slot-value value 'branch-links))
;				    (null (slot-value value 'contained-links)))
;			       pointer)
;			      (t
;			       (record-inferior-node-search)
;			       (record-inferior-node-copy)
;			       (values (new-chunk value raw-chunk pointer) t)))))))
             ;;
             ;; Strategy 2: check at each level....
             ;;
;	     (maybe-fix-pointer (pointer)
;	       ;; if the pointer is part of a link, then make a new one with
;	       ;; the appropriate substitutions, otherwise, just return the pointer
;	       (let* ((raw-chunk (get-pointer-value pointer nil))
;		      (value (chunk-chunk raw-chunk))
;		      ;; this is only neccessary for metering info
;		      (length (length *links-to-be-copied*)))
;		 (cond ((or (numberp value) (symbolp value))
;			pointer)
;		       ;; must be a box or port
;		       (t
;			(let ((bl (slot-value value 'branch-links))
;			      (cl (slot-value value 'contained-links)))
;			  (cond ((and (null bl) (null cl))
;				 pointer)
;				;; check the contained links
;				((dolist (l cl nil)
;				   (when (link-member? l *links-to-be-copied*)
;				     (return t)))
;				 (record-inferior-node-search)
;				 (record-inferior-node-copy)
;				 (record-search-list-length
;				   (length *links-to-be-copied*))
;				 (values (new-chunk value raw-chunk pointer) t))
;				;; check the branch links
;				((dolist (l bl nil)
;				   (when (link-member? l *links-to-be-copied*)
;				     (return t)))
;				 (record-inferior-node-search)
;				 (record-inferior-node-copy)
;				 (record-search-list-length
;				   (length *links-to-be-copied*))
;				 (values (new-chunk value raw-chunk pointer) t))
;				(t
;				 (record-inferior-node-search)
;				 (record-search-list-length
;				   (length *links-to-be-copied*))
;				 ;; no links match so return the original pointer
;				 pointer)))))))





;;; port retargetting utilities

(defun record-target-pair (target target-vc)
  (push (cons target target-vc) *virtual-copy-target-alist*))

(defun record-port-copy (vp)
  (push vp *virtual-copy-ports-left-to-process*))

;;; This ought to check the virtual-copy-rows slot of the box
;;; need to figure out how to keep it valid first....

(defun top-level-virtual-copy-editor-box (eb &optional top-level? for-cache?)
  (record-top-level-copy eb)
  (cond ((port-box? eb)
         (make-virtual-port :target (or (getprop eb 'retargetting-vc) (ports eb))
                            :name (and top-level? (editor-box-name-symbol eb))))
        (t
         (with-top-level-vc-vars (eb)
           (flet ((retarget-ports ()
                    (dolist (vp *virtual-copy-ports-left-to-process*)
                      (let* ((current-target (vp-target vp))
                             (new-target (cdr
                                          (fast-assq
                                           current-target
                                           *virtual-copy-target-alist*))))
                        (unless (null new-target)
                          (when (virtual-copy? new-target)
                            (setf (vc-port-target? new-target) t))
                          (setf (vp-target vp) new-target))))))
              (with-virtual-port-retargetting
                (let ((vc (virtual-copy-editor-box eb top-level? for-cache?)))
                  (retarget-ports)
                  (retarget-vps)
                  (when for-cache? (setf (vc-creation-time vc) -2))
                  vc)))))))

;;; This REALLY belongs in editor.lisp but I dont want to recompile it now
;;; returns either NIL or a symbol in the BU package
(defun editor-box-name-symbol (box)
  (let ((name-row (slot-value box 'name)))
    (when (not (null name-row))
      (cached-name name-row))))

;;; encodes any possibly relevant graphical info of the graphics box
;;; use a PLIST for now, the functions which look at this are in
;;; realprinter.lisp

(defun get-graphics-info-for-newvc (editor-box &optional for-cache?)
  (let ((info nil))
    (when (graphics-sheet? (graphics-info editor-box))
      (let ((new-sheet (copy-graphics-sheet (graphics-info editor-box) nil)))
        (unless for-cache?
          (queue-non-lisp-structure-for-deallocation new-sheet))
        (push new-sheet info)
        (push 'graphics-sheet info))
      ;; if there is a graphics sheet, there might be a movie
      ;; 2024-04-17 av-info removed

      ;; other image types (jpeg ?, gif ?)
      )
    (when (sprite-box? editor-box)
      (push (copy-graphics-object (graphics-info editor-box)) info)
      (push 'turtle info))
    info))

(defun virtual-copy-editor-box (eb &optional (top-level? t) (for-cache? nil))
  (if (port-box? eb)
      (let ((vp (make-virtual-port
                 :target (or (getprop eb 'retargetting-vc) (ports eb))
                 :name (when top-level? (editor-box-name-symbol eb)))))
        (record-port-copy vp)
        vp)
      (let* ((newvc (make-virtual-copy
                      :type
                      (class-name (class-of eb));maybe we should keep a pointer
                                                ; to the class object instead
                      :name (when top-level? (editor-box-name-symbol eb))
;		      :pedigree (list `(,eb . ,now))
                      :progenitor eb
                      :closets (slot-value eb 'closets)
                      ;; THIS is the place where we should handle relevant
                      ;; graphics properties such as graphics-sheets or turtles
                      :graphics (get-graphics-info-for-newvc eb for-cache?)
                      :exports (and top-level? (not (null (exports eb))))))
             (inlinks? nil))
        ;; now we figure out the inlinks? and the evrows
        (multiple-value-bind (evrws links? new? rentry)
            (virtual-copy-rows eb nil newvc)
          (let* ((vcp (vc-rows-entry-pedigree rentry))
                 (now (now))
                 ;; the creation time of the new VC should match the creation
                 ;; time of the rows entry so that mods of deep inferior rows
                 ;; made between the rows entry and the current copy
                 ;; are properly ignored
                 (ctime (if (null rentry) now (vcis-creation-time rentry))))
            (setq inlinks? links?)
            (setf (vc-rows newvc) evrws)
            (unless (or (null links?) new?)
              (dolist (er evrws)
                (update-evrow-for-new-vc er rentry newvc links?)))
            ;; now setup the pedigree and the creation time
            ;;
            (setf (vc-creation-time newvc) ctime)
            (setf (vc-pedigree newvc)
                  ;; the pedigree is used unless there isn't one
                  ;; 6/10/99 switched 1st 2 clauses, check for modified BEFORE
                  (cond ((vc-rows-entry-modified? rentry)
                         `((,eb . ,now) (,rentry . ,now) ,@vcp))
                        ((null vcp)
                         (vc-debugging "Synthesizing a pedigree for ~A" eb)
                         `((,eb . ,ctime) (,rentry . ,ctime)))
                        (t `(,@vcp))))))
        (when (not (null inlinks?))
          (setf (vc-inlinks? newvc) inlinks?)
          (setf (vc-modified? newvc) t))
        ;; if eb is a port target, then we need to
        ;; save the info since we MAY want to retarget ports
        (when (consp (ports eb))
          (record-target-pair eb newvc))
        ;; hooks
        (dolist (hook *edvc-special-structures-hook*) (funcall hook eb newvc))
        newvc)))



;;;;; Cache and Mash...

;;; Various results are cached on editor objects because they are more
;;; permanent (relativley speaking) than VC's.  However, many  times we
;;; want to access those caches via a VC (or chain of VC's).  For that
;;; purpose, we retain a backpointer to the editor object in the form of
;;; a vc-progenitor slot in the VC's.  The slot is cleared if the VC is
;;; modified.  Sometimes we do something to a VC and want to save away
;;; the result in an editor-box cache.  This is allowable only if the
;;; editor-box has the same structure as the VC, that is, no modifications
;;; have occurred during the chain of virtual copying from the original
;;; editor box AND no modification have been MADE to the editor box since
;;; the beginnin of the chain of virtual copies.  Modification of any member
;;; of the virtual copy chain is handled by clearing the backpointer.
;;; Whether or not the editor box has changed is handled by:

(defmethod editor-box-cacheable? ((editor-box box) creation-time)
  (or (minusp (the fixnum creation-time))
      (let ((vcrs (slot-value editor-box 'virtual-copy-rows)))
        (cond ((null vcrs) t)
              (t
               ;; all we have to check is the newest version and that one
               ;; will be the first one since new versions are PUSH'd on
               (>= creation-time (vc-rows-entry-time (car vcrs))))))))


;;; if an editor box has been modifed during an eval, we need to loop
;;; throught the proper set of virtual copy rows in order to generate the
;;; current version of a box's code (this CAN be cached because the result
;;; will  be flushed by any subsequent calls to CHANGE)

(defun get-current-editor-box-items (box)
  (multiple-value-bind (rows inlinks new vc-rows-entry)
      (virtual-copy-rows box)
    (declare (ignore inlinks new))
    (flet ((handle-pointer-for-eval (p)
             (multiple-value-bind (raw-chunk exact?)
                 (get-pointer-value p vc-rows-entry)
               (let ((value (if (chunk-p raw-chunk)
                                (chunk-chunk raw-chunk)
                                raw-chunk)))
                 (cond ((or (virtual-copy? value) (virtual-port? value))
                        value)
                       ((and (chunk-p raw-chunk) (getf (chunk-plist raw-chunk) :eval-prop))
                        (handle-eval-props (getf (chunk-plist raw-chunk) :eval-prop)
                                           value))
                       (t (coerce-object-for-evaluator value)))))))
      (mapcar #'(lambda (r)
                  (mapcar #'handle-pointer-for-eval (evrow-pointers r)))
              rows))))




;;;; Caching (and DeCaching) evaluator objects on Editor objects

;;; The basic problem is that I/O can potentially call modified during
;;; an evaluation.  Therefore, we can't use calls to modified for decaching
;;; evaluator related caches.  We would also like to preserve as much eval
;;; structure as possible cause its SO EXPENSIVE to build it.  On the other
;;; hand, it is important to keep multipointers short and avoid having to
;;; search through invalid multipointer slots left over from previous
;;; evaluations.
;;;
;;; The current scheme involves a global deallocation queue which is bound
;;; around invocations of DOIT.  Mutations or calls which
;;; expand multipointers cause the containing row to be placed on the
;;; de-allocation queue IF the multipointer is increased beyond a
;;; pre-determined length.  The tradeoff to be determined is the cost of
;;; searching through extra garbage vs the cost of building an evrow
;;; from scratch
;;;
;;; Note that there is actually two levels of decaching possible here.  One
;;; situation is when the structure of the (real) row has been changed thus
;;; rendering ALL of the evaluator related caches invalid.  In this case, the
;;; mutator will have consed up a new row and the owning box will have several
;;; versions of its inferior rows based on timestamp.This means that we have to
;;; flush ALL of the editor row's caches as well as those of its superior box
;;; AFTER the evaluation is completed.  However, the modified method for rows
;;; will flush all of the appropriate caches in this case and what we need to
;;; do is to make sure that the superior box gets its vc-rows flushed AFTER
;;; the eval is done.
;;;
;;; The other case is less severe in that the formatting info of the row
;;; is still the same and what has happened is that the multi-pointers have
;;; got too long.  This means that the chunking information is still valid and
;;; we can win by flushing just the evrow cache (retaining the cached-chunks)
;;; This situation is most likely to arise through either articulation of
;;; port/target containing subtrees or as a result of symeval having to
;;; return something unique (in case we want to port to it)
;;;

;;; we can make this a constant later so the compiler will fold it in instead
;;; of having to do a special variable reference EVERY time we want to extend
;;; a multipointer
(defvar *maximum-desired-multipointer-length* 10.)

(defvar *editor-object-evaluator-decaching-queue* nil)

(defvar *editor-object-multipointer-decaching-queue* nil)

(defun queue-editor-box-for-eval-decaching (box)
  (push box *editor-object-evaluator-decaching-queue*))

(defun queue-editor-row-for-mp-decaching (row)
  (push row *editor-object-multipointer-decaching-queue*))

;;; Here we are guaranteeing that the new-object is a copy
;;; of the current object(s) on the pointer and that we are

(defun extend-pointer (ptr who when new-object &optional row)
  (append-value-to-pointer ptr who when new-object)
  (when (and (not-null row)
             (> (length (pointer-value-internal ptr))
                *maximum-desired-multipointer-length*))
    (queue-editor-row-for-mp-decaching row)
    (record-row-decache-from-mpg))
  ptr)




(defun make-vc-rows-entry (time rows &optional
                                   pedigree inlinks? native?)
  (%make-vc-rows-entry :time time :rows rows :creation-time time
                       :pedigree pedigree :inlinks? inlinks?
                       :single-is-exact? native?))

#| Obsolete...

;; the vc-rows-entry struct is supposed to store enuff info the
;; make a vc to be used by get-pointer-value.  This is the function
;; to call to make that vc

(defun make-vc-from-vc-rows-entry (vc-rows-entry &optional pedigree)
  (make-virtual-copy :pedigree (or pedigree
                                   (vc-rows-entry-pedigree vc-rows-entry))
                     :creation-time (vc-rows-entry-time vc-rows-entry)))

(defun vc-from-vc-rows-entry (vc-rows-entry &optional newvc box)
  (cond ((null newvc)
         (record-row-entry-vc-creation)
         (if (vc-rows-entry-modified? vc-rows-entry)
             (make-vc-from-vc-rows-entry
              vc-rows-entry
              (acons box (now);shouldn't this be vc-rows-entry instead of box?
                     (vc-rows-entry-pedigree
                      vc-rows-entry)))
             (make-vc-from-vc-rows-entry
              vc-rows-entry)))
        (t newvc)))

|#




;;; we want to avoid multiply updating the same box. The alternative to
;;; checking is to have some sort of unique ID stuffed on the box
;;; this ought to be faster IF the queue remains relatively short
(defun queue-editor-object-for-mutation (box)
  (unless (or (not (boundp '*editor-objects-to-be-modified*))
              (fast-memq box *editor-objects-to-be-modified*))
    (push box *editor-objects-to-be-modified*)))

;;; note that this CAN be called when *evaluation-in-progress?* is T
;;; by the redisplay in order to set up the editor for redisplay
;;; this might be faster if we could mutate the rows instead of bashing them
;;; then replacing them.  Unfortunately,we have to get the port targets
;;; relinked correctly which makes it harder than simply iterating down
;;; the evrows

(defun process-editor-object-mutation-queue (queue)
  (dolist (editor-box queue)
    (flet
        ((update-editor-object (box)
           (cond ((sv-box-interface? box)
                  (funcall (special-box-interface-update-function box) box))
                 (t
                  (let ((last-vc-rows-entry
                         (car (slot-value box 'virtual-copy-rows))))
                    (let ((ers (make-editor-rows-from-evrows
                                (vc-rows-entry-rows last-vc-rows-entry)
                                last-vc-rows-entry
                                (vc-rows-entry-time last-vc-rows-entry) box)))
                      ;; bash existing rows AFTER creating the new ones
                      (kill-box-contents box)
                      (insert-row-at-row-no box (car ers) 0)
                      (do ((ers (cdr ers) (cdr ers))
                           (previous-row (car ers)))
                          ((null ers))
                        (insert-row-after-row box (car ers) previous-row)
                        (setq previous-row (car ers))))
                    (modified-for-new-contents box)
                    ;; do we have to reset the cache timestamp here ?
                    ;; this will get done when the modified queue is run
;		    (when (null *evaluation-in-progress?*)
                      ;; we have to flush the vc-rows because:
                      ;; 1] they can end up being shared among editor boxes and
                      ;; 2] the time stamps in multi-pointers will be obsolete
;		      (setf (slot-value box 'virtual-copy-rows) nil))
                    )))))
           (update-editor-object editor-box))))

(defun modified-for-new-contents (box)
  (flet ((reset-screen-box-for-new-contents (sb)
           (setf (slot-value sb 'scroll-x-offset) 0
                 (slot-value sb 'scroll-y-offset) 0)))
    (dolist (screen-box (screen-objs box))
      (reset-screen-box-for-new-contents screen-box))
    (modified box)))


;;; Use this whenever the target of a port (being mutated) is an editor box
(defmethod modify-editor-structure ((editor-box box) &optional skip-trigger?)
  ;; queue changes to the editor structure
  (queue-editor-object-for-mutation editor-box)
  ;; set the modified? slot on the vc-rows-entry
  (let ((vcrs (car (slot-value editor-box 'virtual-copy-rows))))
    (unless (null vcrs)
      (setf (vc-rows-entry-modified? vcrs) t)))
  (flush-editor-box-caches editor-box)
  (when (null skip-trigger?)
    ;; update just the timestamp of the box (avoid walking up the tree)
    (setf (actual-obj-tick editor-box) (tick))
    ;; then check the trigger
    (maybe-run-trigger editor-box 'bu::modified-trigger)))

(defmethod flush-editor-box-caches ((editor-box box))
  ;; flush local caches
  (setf (slot-value editor-box 'cached-code) nil)
  ;; make sure that any boxes which have cached variable references
  ;; to this box get their caches invalidated...
  (dolist (entry (slot-value editor-box 'static-variables-alist))
    (setf (boxer-eval::static-variable-uid entry) (boxer-eval::make-cached-variable-uid)))
  (setf (slot-value editor-box 'static-variable-cache) nil)
  (decache-build-function editor-box))

;; no one seems to be using this...
;(defmacro with-editor-object-decaching (&body body)
;  `(let ((*editor-object-evaluator-decaching-queue* nil)
;	 (*editor-object-multipointer-decaching-queue* nil))
;     (flet ((decache-box (box)
;	      (setf (slot-value box 'virtual-copy-rows) nil))
;	    (decache-multipointers-from-row (row)
;	      (setf (slot-value row 'cached-evrow) nil)
;	      (dolist (chunk (slot-value row 'cached-chunks))
;		(scrunch-multipointer->single chunk))))
;       (unwind-protect
;	   (let ((*eval-in-progress T))
;	     . ,body)
;	 (dolist (box *editor-object-evaluator-decaching-queue*)
;	   (decache-box box))
;	 (dolist (row *editor-object-multipointer-decaching-queue*)
;	   (decache-multipointers-from-row row))))))

;;; Box Methods

(defmethod virtual-copy-rows ((self box) &optional time whos-asking)
  #+lispworks (declare (values rows inlinks? new? rows-entry))
  ;; if the box is out on disk, bring it in
  ;; remember that this can lose or be Cancelled by the user
  (when (and (null (slot-value self 'first-inferior-row))
             (storage-chunk? self))
    (boxnet::fill-box-from-server self))
  (flet ((assnv (item list)
           ;; sort of like assoc but the items will be vc-rows-entries
           ;; and the comparision will be numeric (specifically #'>=)
           (dolist (i list
                      (progn
                        (trace-vc "Rows/Time Search failed, using last entry"
                                  item list)
                        (car (last list))))
                       ;; return the last entry if all else fails
             (when (>= item (vc-rows-entry-time i))
               (return i)))))
    (let ((rows (slot-value self 'virtual-copy-rows)))
      (trace-vc "Entering Virtual-Copy-Rows..." self time whos-asking rows)
      (cond ((null rows)
             ;; need to make the rows
             (trace-vc "Making new vc rows" self)
             (let ((new-rows nil) (links? nil))
               (dolist (r (data-rows self))
                 (multiple-value-bind (row inner-links?)
                   (make-evrow-from-row r whos-asking)
                   (push row new-rows)
                   (setq links? (or links? inner-links?))))
               (setq new-rows (nreverse new-rows))
               (let ((new-entry (make-vc-rows-entry
                                 0
                                 new-rows
                                 nil
                                 links?
                                 t)))
                 (setf (vcis-pedigree new-entry) (acons new-entry 0 nil))
                 (setf (vc-rows-entry-editor-box-backpointer new-entry) self)
                 (setf (slot-value self 'virtual-copy-rows)
                       (list new-entry))
                 (values new-rows links? t new-entry))))
            ((null time)
             ;; use the most recent set of rows
             (trace-vc "Null Time, Using most recent set of Rows")
             (values (vc-rows-entry-rows (car rows))
                     (vc-rows-entry-inlinks? (car rows))
                     nil
                     (car rows)))
            (t
             ;; otherwise, search the list looking for a row-entry-time
             ;; which is YOUNGER than the time arg
             (trace-vc "Searching Through Rows" rows time)
             (let ((rows-entry (assnv time rows)))
               (values (vc-rows-entry-rows     rows-entry)
                       (vc-rows-entry-inlinks? rows-entry)
                       nil
                       rows-entry)))))))

(defmethod change-virtual-copy-rows ((self box) new-rows
                                     &optional pedigree inlinks?
                                     unique-inferiors)
  (when (null (slot-value self 'virtual-copy-rows))
    ;; make sure that the old version is there before adding new versions
    ;; in particular, the code that calls this had better do so BEFORE
    ;; any editor structure is munged
    (let ((new-entry (make-vc-rows-entry
                      0
                      (mapcar #'(lambda (r) (make-evrow-from-row r))
                              (rows self)))))
      (setf (vcis-pedigree new-entry) (acons new-entry 0 nil))
      (setf (vc-rows-entry-single-is-exact? new-entry) t)
      (setf (vc-rows-entry-editor-box-backpointer new-entry) self)
      (setf (slot-value self 'virtual-copy-rows) (list new-entry))))
  (let ((old-vcrs (car (slot-value self 'virtual-copy-rows))))
    (when (eq pedigree :dont-touch)
      ;; this means that we should uses the previous pedigree
      (setq pedigree (if (vcis-modified? old-vcrs)
                         (acons old-vcrs (now) (vcis-pedigree old-vcrs))
                         (vcis-pedigree old-vcrs))))
    (when (eq inlinks? :dont-touch)
      (setq inlinks? (vcis-inlinks? old-vcrs)))
    (setf (slot-value self 'virtual-copy-rows)
          (push (let ((new-entry (make-vc-rows-entry (now) new-rows
                                                     pedigree inlinks?)))
                  (setf (vc-rows-entry-editor-box-backpointer new-entry) self)
                  (setf (vc-rows-entry-single-is-exact? new-entry)
                        unique-inferiors)
                  new-entry)
                (slot-value self 'virtual-copy-rows)))))




;;; this seem to work well enough...
;; what his REALLY wants to check is if there is more than
;; one set of virtual copy rows.  If there is, then the
;; BOX has been CHANGEd

(defmethod editor-box-changed? ((eb box))
  (not (null (cdr (slot-value eb 'virtual-copy-rows)))))

;;; looking up variables in CHANGE'd boxes
;;; this is similiar to Lookup-Variable-In-Virtual-Copy
;;; except that it make and uses STATIC-VARIABLEs instead of just plain
;;; values since that is what the interface in bind.lisp is expecting
;;; It may end up using a different caching strategy since the tradeoffs
;;; (based on frequency of multiple variable lookup) will be substantially
;;; different for editor boxes which have been CHANGEd vs virtual copies
;;;
;;; The current strategy is to loop through EVERYTHING and generate a
;;; completely valid cache.  A cache miss can then be taken to mean
;;; that the variable is not in the box
;;;

(defun lookup-variable-in-vc-rows-entry (vc-rows-entry
                                         variable
                                         &optional
                                         editor-box
                                         (scope t)
                                         (exact-inferior-required? t))
  (or (lookup-variable-in-vc-rows-entry-1 vc-rows-entry
                                          variable
                                          editor-box
                                          exact-inferior-required?)
      (let ((sup (vc-rows-entry-editor-box-backpointer vc-rows-entry)))
        (unless (or (null sup) (not scope))
          (let ((sup-box (superior-box sup)))
            (unless (null sup-box)
              (boxer-eval::lookup-static-variable-internal sup-box variable)))))))


(defun lookup-variable-in-vc-rows-entry-1 (vc-rows-entry
                                           variable
                                           &optional
                                           editor-box
                                           (exact-inferior-required? t))
  (declare (ignore exact-inferior-required?)) ; no longer used
  (record-vcre-var-lookup)
  (cond ((eq (vc-rows-entry-cached-binding-alist vc-rows-entry) *no-names-in-vc-marker*)
         (record-vcre-var-lookup-cache-hit) nil)
        (t
         (when (null (vc-rows-entry-cached-binding-alist vc-rows-entry))
           ;; looks like we have to fill the cache...
           (record-vcre-var-lookup-cache-fill)
           ;; first, we loop through the box's inferiors
           ;; punting on exports for the moment
           (dolist (vr (vcis-rows vc-rows-entry))
             (dolist (ptr (evrow-pointers vr))
               (multiple-value-bind (item exact?)
                   (get-pointer-value ptr vc-rows-entry)
                 (let* ((chunk-p (chunk-p item))
                        (val (if chunk-p (chunk-chunk item) item)))
                   (cond ((or (symbolp val) (numberp val)))
                         ((virtual-copy? val)
                          (let ((name (vc-name val)))
                            (cond ((null name))
                                  (exact?
                                   ;; box has a name so cache the name and the
                                   ;; value unless the match is not exact.  We
                                   ;; can only cache exact matches cause we may
                                   ;; later want to Port-To
                                   ;; the name that we find in the cache
                                   (push (boxer-eval::make-static-variable
                                          name val boxer-eval::*non-caching-variable-uid*)
                                         (vc-rows-entry-cached-binding-alist
                                          vc-rows-entry)))
                                  (t
                                   ;; we dont have an exact
                                   ;; match but we want one
                                   (let* ((newval (virtual-copy val :top-level? t))
                                          (svar
                                           (boxer-eval::make-static-variable
                                            name newval boxer-eval::*non-caching-variable-uid*)))
                                     ;; might as well cache it
                                     (push svar (vc-rows-entry-cached-binding-alist
                                                 vc-rows-entry))
                                     ;; put it into the row structure
                                     (extend-pointer
                                      ptr vc-rows-entry (vcis-creation-time vc-rows-entry)
                                      (if chunk-p
                                          (let ((nc (copy-chunk item)))
                                            (setf (chunk-chunk nc) newval) nc)
                                          newval)
                                      (evrow-original-row vr))
                                     (setf (vcis-modified? vc-rows-entry) t))))))
                         ((virtual-port? val)
                          (let ((name (vp-name val)))
                            (cond ((null name))
                                  (t
                                   ;; box has a name so cache
                                   ;; the name and the value
                                   (let ((svar
                                          (boxer-eval::make-static-variable
                                           name val boxer-eval::*non-caching-variable-uid*)))
                                     (push svar
                                           (vc-rows-entry-cached-binding-alist
                                            vc-rows-entry)))))))
                         ((box? val)
                          (let ((name (editor-box-name-symbol val)))
                            (cond ((null name))
                                  (t
                                   ;; box has a name so cache the name and
                                   ;; a virtual copy of the value.
                                   (let* ((new-val (virtual-copy val :top-level? t))
                                          (svar
                                           (boxer-eval::make-static-variable
                                            name new-val
                                            boxer-eval::*non-caching-variable-uid*)))
                                     ;; instantiate the new value into
                                     ;; the structure
                                     (extend-pointer
                                      ptr vc-rows-entry (vcis-creation-time vc-rows-entry)
                                      (if chunk-p
                                          (let ((nc (copy-chunk item)))
                                            (setf (chunk-chunk nc) new-val) nc)
                                          new-val)
                                      (evrow-original-row vr))
                                     (push svar
                                           (vc-rows-entry-cached-binding-alist
                                            vc-rows-entry))
                                     (setf (vcis-modified? vc-rows-entry) t)))))))))))
           ;; since it is an editor box, we need to look in the
           ;; closet as well since closets aren't supposed to
           ;; affected by CHANGE
           ;;
           ;; Still need to think about whether it will be faster to
           ;; go through the box's alist and then check to see if
           ;; the result is in the closet or to iterate through the
           ;; items in the closet
           (let ((closet (slot-value editor-box 'closets)))
             (labels ((in-closet? (value)
                        (when (box? value)
                          (or (eq (superior-row value) closet)
                              (let ((superior-box (superior-box value)))
                                (and superior-box
                                     (not (null
                                           (slot-value superior-box 'exports)))
                                     (in-closet? superior-box)))))))
               (unless (null closet)
                 (dolist (item (slot-value editor-box 'static-variables-alist))
                   (when (or (in-closet? (boxer-eval::static-variable-value item))
                             (boxer-eval::boxer-function? (boxer-eval::static-variable-value item)))
                     (push item
                           (vc-rows-entry-cached-binding-alist
                            vc-rows-entry)))))))
           ;; the cache is now as filled as it ever will be
           ;;
           ;; If there are no bindings pushed onto the cached binding list,
           ;; then set the marker so we don't loop through the box again.
           (when (null (vc-rows-entry-cached-binding-alist vc-rows-entry))
             (setf (vc-rows-entry-cached-binding-alist vc-rows-entry)
                   *no-names-in-vc-marker*)
             (return-from lookup-variable-in-vc-rows-entry-1 nil)))
         ;; now check the cache
         (when (consp (vc-rows-entry-cached-binding-alist vc-rows-entry))
           (let ((val? (fast-assq variable
                                  (vc-rows-entry-cached-binding-alist
                                   vc-rows-entry))))
             (cond ((null val?)
                    (record-vcre-var-lookup-cache-miss))
                   (t
                    (record-vcre-var-lookup-cache-hit)))
             (return-from lookup-variable-in-vc-rows-entry-1 val?))))))



;;;; Mapping from chunked representation to rows and cha-no's
;;;  Mutators which are passed ports to editor boxes will need these
;;;  as well as the stepper/debugger
;;;
;;; these function fit in the following matrix:
;;;
;;;                      beginning of chunk   end of chunk
;;; selection: by index |      xxx               xxx
;;;            by EQness|      xxx               xxx
;;;    some general test|       *                 *
;;;
;;;  The General test functions take an optional keyword arg of :test
;;;  which is passed 2 args, the ptr (2nd arg) and the pointer that we
;;;  are checking as we cdr down the rows list of pointers
;;;  The default test is #'eq
;;;
;;;  The optional whitespace? arg specifies whether to include the
;;;  surrounding whitspace as part of the chunk.
;;;  For example:
;;;    foo   bar  baz []
;;;       ^  ^ ^ ^
;;;       |  | | |
;;;       |  | | --- end of chunk with whitespace
;;;       |  | --- end of chunk without whitspace
;;;       |  --- beginning of chunk without whitespace
;;;       --- beginning of chunk with whitspace
;;;

(defun beginning-of-nth-chunk-cha-no (row n &optional whitespace?)
  (let ((idx 0)
        (cha-no 0))
    (dolist (chunk (chunks row))
      (let ((raw-chunk (get-pointer-value chunk nil)))
        (cond ((= idx n)
               (if (null whitespace?)
                   (return (+ cha-no
                              (formatting-info-length
                                (chunk-left-format raw-chunk))))
                   (return cha-no)))
              (t
               (incf idx)
               (incf cha-no
                     (formatting-info-length (chunk-left-format raw-chunk)))
               (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                                 (symbolp (chunk-chunk raw-chunk)))
                             (formatting-info-length (chunk-pname raw-chunk))
                             1))))))))

(defun end-of-nth-chunk-cha-no (row n &optional whitespace?)
  (let ((idx 0)
        (cha-no 0))
    (dolist (chunk (chunks row))
      (let ((raw-chunk (get-pointer-value chunk nil)))
        (incf cha-no (formatting-info-length (chunk-left-format raw-chunk)))
        (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                             (symbolp (chunk-chunk raw-chunk)))
                         (formatting-info-length (chunk-pname raw-chunk))
                         1))
        (cond ((= idx n)
               (if (null whitespace?)
                   (return cha-no)
                   (return (+ cha-no
                              (formatting-info-length
                                (chunk-right-format raw-chunk))))))
              (t (incf idx)))))))

(defun beginning-of-chunk-cha-no (row ptr &optional whitespace?)
  (let ((cha-no 0))
    (dolist (current-ptr (chunks row))
      (let ((raw-chunk (get-pointer-value current-ptr nil)))
        (cond ((eq ptr current-ptr)
               (if (null whitespace?)
                   (return (+ cha-no
                              (formatting-info-length
                                (chunk-left-format raw-chunk))))
                   (return cha-no)))
              (t
               (incf cha-no (formatting-info-length (chunk-left-format raw-chunk)))
               (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                                 (symbolp (chunk-chunk raw-chunk)))
                             (formatting-info-length (chunk-pname raw-chunk))
                             1))))))))

(defun end-of-chunk-cha-no (row ptr &optional whitespace?)
  (let ((cha-no 0))
    (dolist (current-ptr (chunks row))
      (let ((raw-chunk (get-pointer-value current-ptr nil)))
        (incf cha-no (formatting-info-length (chunk-left-format raw-chunk)))
        (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                             (symbolp (chunk-chunk raw-chunk)))
                         (formatting-info-length (chunk-pname raw-chunk))
                         1))
        (cond ((eq ptr current-ptr)
               (if (null whitespace?)
                   (return cha-no)
                   (return (+ cha-no
                              (formatting-info-length
                                (chunk-right-format raw-chunk)))))))))))

(defun find-beginning-of-chunk-cha-no (row ptr &optional whitespace?
                                           &key (test #'eq))
  (let ((cha-no 0))
    (dolist (current-ptr (chunks row))
      (let ((raw-chunk (get-pointer-value current-ptr nil)))
        (cond ((funcall test ptr current-ptr)
               (if (null whitespace?)
                   (return (+ cha-no
                              (formatting-info-length
                                (chunk-left-format raw-chunk))))
                   (return cha-no)))
              (t
               (incf cha-no
                     (formatting-info-length (chunk-left-format raw-chunk)))
               (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                                 (symbolp (chunk-chunk raw-chunk)))
                             (formatting-info-length (chunk-pname raw-chunk))
                             1))))))))

(defun find-end-of-chunk-cha-no (row ptr &optional whitespace?
                                     &key (test #'eq))
  (let ((cha-no 0))
    (dolist (current-ptr (chunks row))
      (let ((raw-chunk (get-pointer-value current-ptr nil)))
        (incf cha-no (formatting-info-length (chunk-left-format raw-chunk)))
        (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                             (symbolp (chunk-chunk raw-chunk)))
                         (formatting-info-length (chunk-pname raw-chunk))
                         1))
        (cond ((funcall test ptr current-ptr)
               (if (null whitespace?)
                   (return cha-no)
                   (return (+ cha-no
                              (formatting-info-length
                                (chunk-right-format raw-chunk)))))))))))

(defun chunk-at-cha-no (row target-cha-no &optional (prefer-left? t))
  (let* ((cha-no 0) (chunks (chunks row)) (last (car (last chunks))))
    (dolist (current-ptr chunks)
      (let ((raw-chunk (get-pointer-value current-ptr nil)))
        (when (only-formatting-chunk? raw-chunk) (return nil))
        (incf cha-no (formatting-info-length (chunk-left-format raw-chunk)))
        (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                             (symbolp (chunk-chunk raw-chunk)))
                         (formatting-info-length (chunk-pname raw-chunk))
                         1))
        (when (> (if (or prefer-left? (eq current-ptr last))
                     (+& cha-no (formatting-info-length
                                 (chunk-right-format raw-chunk)))
                     cha-no)
                  target-cha-no)
          (return current-ptr))))))

(defun chunk-no-at-cha-no (row target-cha-no &optional (prefer-left? t))
  (let* ((cha-no 0) (chunks (chunks row)) (chunk-no 0)
         (last (car (last chunks))))
    (dolist (current-ptr chunks (1-& chunk-no))
      (let ((raw-chunk (get-pointer-value current-ptr nil)))
        (incf cha-no (formatting-info-length (chunk-left-format raw-chunk)))
        (incf cha-no (if (or (numberp (chunk-chunk raw-chunk))
                             (symbolp (chunk-chunk raw-chunk)))
                         (formatting-info-length (chunk-pname raw-chunk))
                         1))
        (cond ((> (if (or prefer-left? (eq current-ptr last))
                       (+& cha-no (formatting-info-length
                                   (chunk-right-format raw-chunk)))
                       cha-no)
                   target-cha-no)
               (return chunk-no))
              ((only-formatting-chunk? raw-chunk))
              (t (incf& chunk-no)))))))


