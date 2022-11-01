;;;;
;;;;    Boxer
;;;;    Copyright 1985-2022 Andrea A. diSessa and the Estate of Edward H. Lay
;;;;
;;;;    Portions of this code may be copyright 1982-1985 Massachusetts Institute of Technology. Those portions may be
;;;;    used for any purpose, including commercial ones, providing that notice of MIT copyright is retained.
;;;;
;;;;    Licensed under the 3-Clause BSD license. You may not use this file except in compliance with this license.
;;;;
;;;;    https://opensource.org/licenses/BSD-3-Clause
;;;;
;;;;
;;;;                                   +------+
;;;;          This file is part of the |Boxer | System
;;;;                                   +-Data-+
;;;;

(in-package :boxer-eval)

(defun boxer-step-eval (iline &key process-state)
  (compiler-let ((*compiling-stepper* t))
    (evaluator-body iline process-state)))
