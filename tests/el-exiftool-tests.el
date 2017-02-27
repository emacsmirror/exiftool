;;; el-exiftool.el --- Elisp wrapper around exiftool ;; -*- lexical-binding: t -*-

;; Elisp wrapper around exiftool
;; Copyright (C) 2017 by Arun I
;;
;; Author: Arun I <arunisaac@systemreboot.net>
;; Keywords: data
;; Homepage: https://git.systemreboot.net/el-exiftool

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

;;; Commentary:
;;
;; Unit testing for el-exiftool

;;; Code:

(defvar el-exiftool-tests--tag-value
  '(("Marked" . "True")
    ("Creator" . "foo")
    ("Rights" . "bar")))

(require 'el-exiftool)
(require 'ert)

(defmacro with-temp-test-file (test-file temp-file &rest body)
  "Copy TEST-FILE to temporary file, put path in TEMP-FILE, evaluate BODY."
  (declare (indent defun))
  `(let ((,temp-file (make-temp-file "el-exiftool-"
					 nil (concat "-" ,test-file))))
     (copy-file ,test-file ,temp-file t)
     ,@body
     (delete-file ,temp-file)))

(ert-deftest read-write-test ()
  (with-temp-test-file "test1.png" temp-file
    (apply 'el-exiftool-write temp-file el-exiftool-tests--tag-value)
    (should (equal (apply 'el-exiftool-read temp-file (mapcar 'car el-exiftool-tests--tag-value))
		   el-exiftool-tests--tag-value))))

(ert-deftest delete-test ()
  (with-temp-test-file "test1.png" temp-file
    (el-exiftool-write temp-file (car el-exiftool-tests--tag-value))
    (let ((delete-pair
	   (cons (caar el-exiftool-tests--tag-value) "")))
      (el-exiftool-write temp-file delete-pair)
      (should (equal (car (el-exiftool-read temp-file "Marked"))
		     delete-pair)))))

(ert-deftest copy-test ()
  (with-temp-test-file "test1.png" temp-1
    (with-temp-test-file "test2.jpg" temp-2
      (apply 'el-exiftool-write temp-1 el-exiftool-tests--tag-value)
      (el-exiftool-copy temp-1 temp-2)
      (let ((tags (mapcar 'car el-exiftool-tests--tag-value)))
	(should (equal (apply 'el-exiftool-read temp-1 tags)
		       (apply 'el-exiftool-read temp-2 tags)))))))

(provide 'el-exiftool-tests)

;;; el-exiftool-tests.el ends here
