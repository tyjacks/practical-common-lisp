; Small Database Example
; Practical Common Lisp
; Chapter 3

; Create a single record representing a CD
(defun make-cd (title artist rating ripped)
	(list :title title :artist artist :rating rating :ripped ripped))

; Create a global var to hold the CDs
(defvar *db* nil)

; Add records to db
(defun add-record (cd) (push cd *db*))

; Print db in readable format
(defun dump-db ()
	(dolist (cd *db*)
		; the second arg below is a formatting directive
		; to specify the output format.
		; ~ is the CL equivalent of % in python
		(format t "~{~a:~10t~a~%~}~%" cd)))

; An equivalent, but concise, version of dump-db
(defun concise-dump-db ()
	(format t "~{~{~a:~10t~a~%~}~%~}" *db*))

; User input prompt
(defun prompt-read (prompt)
	(format *query-io* "~a: " prompt)
	(force-output *query-io*)
	(read-line *query-io*))

; Ask for user input to make a CD
(defun prompt-for-cd ()
	(make-cd
		(prompt-read "Title")
		(prompt-read "Artist")
		(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) ;Look for an integer, otherwise 0
		(y-or-n-p "Ripped [y/n]"))) ;User input must start with a y or n, case-insensitive

; More than one record at a time
(defun add-cds ()
	(loop (add-record (prompt-for-cd))
		(if (not (y-or-n-p "Another? [y/n]: ")) (return))))

; Write db to file
(defun save-db (filename)
	(with-open-file (out filename
					 :direction :output
					 :if-exists :supersede)
		(with-standard-io-syntax
			(print *db* out))))

; Load file into session
; Note: this will clobber what is already in memory
(defun load-db (filename)
	(with-open-file (in filename)
		(with-standard-io-syntax
			(setf *db* (read in)))))

; Query the database
(defun select-by-artist (artist)
	(remove-if-not
	 #'(lambda (cd) (equal (getf cd :artist) artist))
	 *db*))

; Abstraction of a database query 
(defun select (selector-fn)
	(remove-if-not selector-fn *db*))

; New version of artist selector
(defun artist-selector (artist)
	#'(lambda (cd) (equal (getf cd :artist) artist)))

; Generalize the predicate for the selector
; Usage: (select (where :artist "foo artist" :rating foo-num))
;(defun where (&key title artist rating (ripped nil ripped-p))
;	#'(lambda (cd)
;		(and
;			(if title    (equal (getf cd :title)  title) t)
;			(if artist   (equal (getf cd :artist) artist) t)
;			(if rating   (equal (getf cd :rating) rating) t)
;			(if ripped-p (equal (getf cd :ripped) ripped) t))))

; Refactoring where function using a macro
(defun make-comparison-expr (field value)
	`(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
	(loop while fields
		collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
	`#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

; Update the selected record(s)
; Usage: (update (where :attr "val") :other-attr "new-val") 
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
	(setf *db*
		  (mapcar
		   #'(lambda (row)
				(when (funcall selector-fn row)
					(if title    (setf (getf row :title) title))
					(if artist   (setf (getf row :artist) artist))
					(if rating   (setf (getf row :rating) rating))
					(if ripped-p (setf (getf row :ripped) ripped)))
				row) *db*)))

; Remove rows from the db
; Usage: (delete-rows (where :attr "val"))
(defun delete-rows (selector-fn)
	(setf *db* (remove-if selector-fn *db*)))
