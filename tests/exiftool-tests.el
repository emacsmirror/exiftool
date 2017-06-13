;;; exiftool.el --- Elisp wrapper around exiftool ;; -*- lexical-binding: t -*-

;; Elisp wrapper around exiftool
;; Copyright (C) 2017 by Arun I
;;
;; Author: Arun I <arunisaac@systemreboot.net>
;; Keywords: data
;; Homepage: https://git.systemreboot.net/exiftool.el

;; This file is part of exiftool.el.

;; exiftool.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; exiftool.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with exiftool.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Unit testing for exiftool.el

;;; Code:

(defvar exiftool-tests--tag-value
  '(("Marked" . "True")
    ("Creator" . "foo")
    ("Rights" . "bar")))

(require 'exiftool)
(require 'cl-lib)
(require 'ert)

(defmacro with-temp-test-file (test-file temp-file &rest body)
  "Copy TEST-FILE to temporary file, put path in TEMP-FILE, evaluate BODY."
  (declare (indent defun))
  `(let ((,temp-file (make-temp-file "exiftool-"
					 nil (concat "-" ,test-file))))
     (copy-file ,test-file ,temp-file t)
     ,@body
     (delete-file ,temp-file)))

(ert-deftest read-write-test ()
  (with-temp-test-file "test1.png" temp-file
    (apply 'exiftool-write temp-file exiftool-tests--tag-value)
    (should (equal (apply 'exiftool-read temp-file (mapcar 'car exiftool-tests--tag-value))
		   exiftool-tests--tag-value))))

(ert-deftest delete-test ()
  (with-temp-test-file "test1.png" temp-file
    (exiftool-write temp-file (car exiftool-tests--tag-value))
    (let ((delete-pair
	   (cons (caar exiftool-tests--tag-value) "")))
      (exiftool-write temp-file delete-pair)
      (should (equal (car (exiftool-read temp-file "Marked"))
		     delete-pair)))))

(ert-deftest copy-all-test ()
  (with-temp-test-file "test1.png" temp-1
    (with-temp-test-file "test2.png" temp-2
      (apply 'exiftool-write temp-1 exiftool-tests--tag-value)
      (exiftool-copy temp-1 temp-2)
      (let ((tags (mapcar 'car exiftool-tests--tag-value)))
	(should (equal (apply 'exiftool-read temp-1 tags)
		       (apply 'exiftool-read temp-2 tags)))))))

(ert-deftest copy-some-test ()
  (with-temp-test-file "test1.png" temp-1
    (with-temp-test-file "test2.png" temp-2
      (apply 'exiftool-write temp-1 exiftool-tests--tag-value)
      (let* ((all-tags (mapcar 'car exiftool-tests--tag-value))
	     (some-tags (cl-subseq all-tags 2)))
	(apply 'exiftool-copy temp-1 temp-2 some-tags)
	(should (and (not (equal (apply 'exiftool-read temp-1 all-tags)
				 (apply 'exiftool-read temp-2 all-tags)))
		     (equal (apply 'exiftool-read temp-1 some-tags)
			    (apply 'exiftool-read temp-2 some-tags))))))))

(provide 'exiftool-tests)

;;; exiftool-tests.el ends here
