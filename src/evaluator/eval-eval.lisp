;;; -*- Syntax: Common-Lisp ; Base: 10; Package: EVAL -*-

#|


 $Header: eval-eval.lisp,v 1.0 90/01/24 22:11:14 boxer Exp $

 $Log:	eval-eval.lisp,v $
;;;Revision 1.0  90/01/24  22:11:14  boxer
;;;Initial revision
;;;

    Boxer
    Copyright 1985-2020 Andrea A. diSessa and the Estate of Edward H. Lay

    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.

    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.

    https://opensource.org/licenses/BSD-3-Clause


                                      +-------+
             This file is part of the | Boxer | System
                                      +-Data--+



Modification History (most recent at top)

 2/15/03 merged current LW and MCL files, no diffs, updated copyright
|#

;;;

(in-package :boxer-eval)

(defun boxer-eval (iline &key (process-state nil))
  (evaluator-body iline process-state))
