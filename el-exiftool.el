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
;; el-exiftool is an elisp wrapper around exiftool.  Exiftool supports
;; reading and writing metadata in various formats including EXIF, XMP
;; and IPTC.

;;; Code:

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
  "Start an exiftool process if one is not already running.
If an exiftool process is already running, delete it, and create
a new one. Return the process object of the newly created
process."
  (when-let (exiftool (get-process "exiftool"))
    (delete-process exiftool))
  (start-process "exiftool" "exiftool" "exiftool" "-stay_open" "True" "-@" "-"))

(let ((tq (tq-create (el-exiftool-run))))
  (defun el-exiftool-command (&rest args)
    "Execute a command in the currently running exiftool process.
If there is no running exiftool process, a new one will be
created. ARGS are arguments of the command to be run, as provided
to the exiftool command line application."
    (string-trim
     (let ((suffix "{ready}\n"))
       (string-remove-suffix
	suffix (el-exiftool--tq-sync-query
		tq (concat (string-join args "\n")
			   "\n-execute\n")
		suffix))))))

(defun el-exiftool-read (file &rest tags)
  "Read TAGs from FILE, and return an alist mapping tag names to corresponding values.

\(fn FILE TAG...)"
  (mapcar
   (lambda (line)
     (cl-destructuring-bind
	 (tag value) (split-string line ": ")
       (let ((value (if (equal value "-") "" value)))
	 (cons tag value))))
   (split-string
    (apply 'el-exiftool-command
	   "-s" "-s" "-f"
	   (append
	    (mapcar (apply-partially 'format "-%s") tags)
	    (list file)))
    "\n+")))

(defun el-exiftool-copy (source destination)
  "Copy tags from SOURCE file to DESTINATION file."
  (el-exiftool-command "-overwrite_original"
		    "-tagsFromFile" source
		    "-all:all" destination)
  (message "Tags from %s copied to %s" source destination)
  destination)

(defun el-exiftool-write (file &rest tag-value-alist)
  "Write tags to FILE.

The metadata to be written is specified as (TAG . VALUE) pairs.

\(fn FILE (TAG . VALUE)...)"
  (apply 'el-exiftool-command
	 "-m" "-overwrite_original"
	 (append
	  (mapcar
	   (cl-function
	    (lambda ((tag . value))
	      (format "-%s=%s" tag value)))
	   tag-value-alist)
	  (list file))))

(provide 'el-exiftool)

;;; el-exiftool.el ends here
