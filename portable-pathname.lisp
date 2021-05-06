;; Prevent overwritting built-in functions
;; of the same name
(in-package :com.gigamonkeys.pathnames)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
    (not (component-present-p (pathname-name p)))
    (not (component-present-p (pathname-type p)))
    p))

(defun pathname-as-directory (name)
  "Interal to package
   Can be accessed by com.gigamonkeys.pathnames::pathname-as-directory"
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
        :directory (append (or (pathname-directory pathname) (list :relative))
                           (list (file-namestring pathname)))
        :name      nil
        :type      nil
        :defaults pathname)
      pathname)))

(defun directory-wildcard (dirname)
  "CLISP doesn't return files w/out extension unless
   :type is NIL so set to NIL. All other implementations
   are :wild."
  (make-pathname
    :name :wild
    :type #-clisp :wild #+clisp nil
    :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))
