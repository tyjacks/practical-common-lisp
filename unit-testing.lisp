;;;; Unit testing framework
;;;; From Practical Common Lisp

(defmacro with-gensyms ((&rest names) &body body)
  "Bind each var name passed in to a unique name."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defun test-+ ()
  (check 
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5 6) 15)))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
