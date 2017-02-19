;; This file is part of el-exiftool.

;; el-exiftool is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; el-exiftool is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with el-exiftool.  If not, see <http://www.gnu.org/licenses/>.

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
