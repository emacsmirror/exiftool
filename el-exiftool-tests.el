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

(require 'el-exiftool)
(require 'ert)

(ert-deftest read-write-test ()
  (let ((test-filename "test1.png"))
    (let ((temp-filename (make-temp-file "el-exiftool-"
					 nil (concat "-" test-filename)))
	  (tag-value-alist '(("Marked" . "True"))))
      (copy-file test-filename temp-filename t)
      (apply 'el-exiftool-write temp-filename tag-value-alist)
      (should (equal (el-exiftool-read temp-filename (caar tag-value-alist))
		     tag-value-alist))
      (delete-file temp-filename))))

(provide 'el-exiftool-tests)

;;; el-exiftool-tests.el ends here
