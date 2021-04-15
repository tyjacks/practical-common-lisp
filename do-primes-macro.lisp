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

(defmacro do-primes ((var start end) &body body)
  "streamlined version using a destructuring parameter list."
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
