;;; -*- Syntax: Common-Lisp ; Base: 10; Package: EVAL -*-

#|


 $Header: eval-eval.lisp,v 1.0 90/01/24 22:11:14 boxer Exp $

 $Log:	eval-eval.lisp,v $
;;;Revision 1.0  90/01/24  22:11:14  boxer
;;;Initial revision
;;;

        Copyright 1987 - 1996 Regents of the University of California

     Enhancements and Modifications Copyright 1999 - 2003 Pyxisystems LLC


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+



Modification History (most recent at top)

 2/15/03 merged current LW and MCL files, no diffs, updated copyright
|#

;;;

#-(or lispworks mcl Lispm) (in-package 'eval)
#+(or lispworks mcl)       (in-package :eval)

(defun boxer-eval (iline &key (process-state nil))
  (evaluator-body iline process-state))
