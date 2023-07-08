;;; exiftool.el --- Elisp wrapper around ExifTool
;;; Copyright © 2023 Arun Isaac <arunisaac@systemreboot.net>
;;;
;;; This file is part of exiftool.el.
;;;
;;; exiftool.el is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; exiftool.el is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with exiftool.el.  If not, see
;;; <https://www.gnu.org/licenses/>.

(define-module (emacs-exiftool-package)
  #:use-module ((gnu packages emacs-xyz) #:prefix guix:)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public emacs-exiftool
  (package
    (inherit guix:emacs-exiftool)
    (source (local-file ".."
                        "emacs-exiftool-checkout"
                        #:recursive? #t
                        #:select? (or (git-predicate (dirname (current-source-directory)))
                                      (const #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments guix:emacs-exiftool)
       ((#:test-command _ #~'())
        #~(list "make"))))))

emacs-exiftool
