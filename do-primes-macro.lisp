;;;; DO-PRIMES Macro
;;; Define a macro similar to DOTIMES and DOLIST
;;; but iterate of primes

(defun primep (number)
  "Test if number is prime."
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "Return next prime greaterthan or equal to number."
  (loop for n from number when (primep n) return n))

;(do-primes (p 0 19)
;  "How we want to be able to call the macro."
;  (format t "~d " p))

; How to write the loop using DO
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

(defmacro do-primes-v1 (var-and-range &rest body)
  "simple version--have to pull vars out of parameter list individually."
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

(defmacro do-primes-v2 ((var start end) &body body)
  "streamlined version using a destructuring parameter list.
   The end form gets evaluated more than once. This can be
   verified by passing an expression instead of a literal number."
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

(defmacro do-primes-v3 ((var start end) &body body)
  "the end form is evaluated a single time by binding
   it to a variable, using GENSYM to use a unique name
   that is unlikely to be used by the caller."
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
     ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  "Bind each var name passed in to a unique name."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes-v4 ((var start end) &body body)
  "Use a macro to handle the GENSYMS part of the do-primes macro."
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
     ,@body)))

