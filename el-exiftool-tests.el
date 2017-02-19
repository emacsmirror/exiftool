(require 'el-exiftool)
(require 'ert)

(ert-deftest read-write-test ()
  (let ((test-filename "test1.png"))
    (let ((temp-filename (make-temp-file "el-exiftool-"
					 nil (concat "-" test-filename)))
	  (tag "xmp:Marked")
	  (value "True"))
      (copy-file test-filename temp-filename t)
      (el-exiftool-write temp-filename (cons tag value))
      (should (equal (el-exiftool-read temp-filename tag)
		     value))
      (delete-file temp-filename))))
