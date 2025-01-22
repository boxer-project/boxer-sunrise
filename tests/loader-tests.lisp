(in-package :boxer-sunrise-test)

(plan nil)

(let ((boxer::*draw-status-line* nil)
      (henri-sun-v5 (merge-pathnames "data/boxfiles-v5/henri-sun-v5.box"
                                     (make-pathname :directory (pathname-directory *load-truename*)))))

    ;; Ensure this is the actual original bits, and has never been resaved
    ;; and updated/fixed as part of the save process.
    (is "24C084326289A1021BB0EF266838EF00"
        (with-output-to-string (str)
            (loop for x across
            (md5:md5sum-file henri-sun-v5)
                do (format str "~2,'0X" x))))

    (is t
        (boxer:data-box? (boxer::load-binary-box-internal henri-sun-v5)))

)
(finalize)
