;;; exiftool.el --- Elisp wrapper around exiftool ;; -*- lexical-binding: t -*-

;; Elisp wrapper around exiftool
;; Copyright (C) 2017, 2019, 2021 by Arun I
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

(require 'exiftool)
(require 'cl-lib)
(require 'ert)

(defvar exiftool-tests--tag-value
  '(("Marked" . "True")
    ("Creator" . "foo")
    ("Rights" . "-")
    ("Publisher" . "")
    ("Source" . "http://example.com")
    ("Subject" . "foo:bar")
    ("Description" . "foo: bar")))

(defvar exiftool-tests--tags
  (mapcar (cl-function
	    (lambda ((tag . value))
	      tag))
	  exiftool-tests--tag-value))

(defmacro with-temp-test-file (test-file temp-file &rest body)
  "Copy TEST-FILE to temporary file, put path in TEMP-FILE, evaluate BODY."
  (declare (indent defun))
  `(let ((,temp-file (make-temp-file
		      "exiftool-" nil (concat "-" ,test-file))))
     (copy-file ,(expand-file-name test-file "tests") ,temp-file t)
     ,@body
     (delete-file ,temp-file)))

(ert-deftest read-write-test ()
  (with-temp-test-file "test1.png" temp-file
    (apply 'exiftool-write temp-file exiftool-tests--tag-value)
    (should (equal (apply 'exiftool-read temp-file exiftool-tests--tags)
		   exiftool-tests--tag-value))))

(ert-deftest delete-test ()
  (with-temp-test-file "test1.png" temp-file
    (exiftool-write temp-file (nth 0 exiftool-tests--tag-value))
    (let ((delete-pair
	   (cons (caar exiftool-tests--tag-value) "")))
      (exiftool-write temp-file delete-pair)
      (should (equal (nth 0 (exiftool-read temp-file "Marked"))
		     delete-pair)))))

(ert-deftest copy-all-test ()
  (with-temp-test-file "test1.png" temp-1
    (with-temp-test-file "test2.png" temp-2
      (apply 'exiftool-write temp-1 exiftool-tests--tag-value)
      (exiftool-copy temp-1 temp-2)
      (should (equal (apply 'exiftool-read temp-1 exiftool-tests--tags)
		     (apply 'exiftool-read temp-2 exiftool-tests--tags))))))

(ert-deftest copy-some-test ()
  (with-temp-test-file "test1.png" temp-1
    (with-temp-test-file "test2.png" temp-2
      (apply 'exiftool-write temp-1 exiftool-tests--tag-value)
      (let ((some-tags (cl-subseq exiftool-tests--tags 2)))
	(apply 'exiftool-copy temp-1 temp-2 some-tags)
	(should (and (not (equal (apply 'exiftool-read temp-1 exiftool-tests--tags)
				 (apply 'exiftool-read temp-2 exiftool-tests--tags)))
		     (equal (apply 'exiftool-read temp-1 some-tags)
			    (apply 'exiftool-read temp-2 some-tags))))))))

(ert-deftest read-file-not-found-test ()
  (should-error (exiftool-read "non-existent-file.png")
                :type 'file-missing))

(ert-deftest copy-file-not-found-test ()
  (should-error (exiftool-copy "non-existent-file-1.png"
                               "non-existent-file-2.png")
                :type 'file-missing))

(ert-deftest write-file-not-found-test ()
  (should-error (exiftool-write "non-existent-file.png")
                :type 'file-missing))

(provide 'exiftool-tests)

;;; exiftool-tests.el ends here
