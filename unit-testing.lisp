;;;; Unit testing framework
;;;; From Practical Common Lisp

(defmacro check (form)
  `(report-result ,form ',form))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))
