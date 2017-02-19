;; -*- lexical-binding: t -*-

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

(require 'subr-x)
(require 'cl)

(defun el-exiftool--tq-sync-query (tq question regexp &optional closure)
  (let ((response))
    (tq-enqueue tq question regexp closure
		(lambda (closure answer) (setq response answer)))
    (while (not response)
      (accept-process-output))
    response))

(defun el-exiftool-run ()
  (when-let (exiftool (get-process "exiftool"))
    (delete-process exiftool))
  (start-process "exiftool" "exiftool" "exiftool" "-stay_open" "True" "-@" "-"))

(let ((tq (tq-create (el-exiftool-run))))
  (defun el-exiftool-command (&rest args)
    (string-trim
     (let ((suffix "{ready}\n"))
       (string-remove-suffix
	suffix (el-exiftool--tq-sync-query
		tq (concat (string-join args "\n")
			   "\n-execute\n")
		suffix))))))

(defun el-exiftool-read (filepath tag)
  (el-exiftool-command
   "-s" "-s" "-s" (format "-%s" tag) filepath))

(defun el-exiftool-copy (source destination)
  (el-exiftool-command "-overwrite_original"
		    "-tagsFromFile" source
		    "-all:all" destination)
  (message "Tags from %s copied to %s" source destination)
  destination)

(cl-defun el-exiftool-write (filepath (tag . value))
  (unless (string-blank-p value)
    (el-exiftool-command
     "-m" "-overwrite_original"
     (format "-%s=%s" tag value) filepath)))

(provide 'el-exiftool)
