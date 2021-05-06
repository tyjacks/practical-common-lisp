;; This prevents issues with some
;; CL implementations using the
;; same names as functions I 
;; wrote in portable-pathnames.lisp

;; It needs to be LOADed, then
;; in the REPL or at the top of
;; the package use the following
;; (in-package :com.gigamonkeys.pathnames)
(in-package :cl-user)

(defpackage :com.gigamonkeys.pathnames
  (:use :common-lisp)
  (:export 
    :list-directory
    :file-exists-p
    :directory-pathname-p
    :pathname-as-directory
    :pathname-as-file
    :walk-directory
    :directory-p
    :file-p))
