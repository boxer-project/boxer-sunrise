(in-package :boxer-sunrise-test)

(plan nil)

(let ((henri-sun-v5 (merge-pathnames "data/boxfiles-v5/henri-sun-v5.box"
                                     (make-pathname :directory (pathname-directory *load-truename*))))
      (www-box-v12 (merge-pathnames "data/boxfiles-v12/hello-www.box"
                                     (make-pathname :directory (pathname-directory *load-truename*)))))
  (is (boxer::get-box-file-format-version henri-sun-v5) 5)
  (is (boxer::get-box-file-format-version www-box-v12) 12)
)

(finalize)
