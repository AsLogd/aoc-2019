(defun read-numbers ()
	"Returns a list of numbers read from stdin"
	(let ((l ())) ;list of variable/initial-value pairs
		(loop for i = (read *standard-input* nil :eof)
			until (eq i :eof)
			do (push i l)
		)
		(return-from read-numbers l)
	)
)

(defun write-numbers (container)
	(loop for i in container
		do (format t "~d~%" i)
	)
)
;(write-numbers (read-numbers))

(defun compute-masses (mass-list)
	(reduce '+ 
		(map 'list (lambda (mass)
			(- (floor (/ mass 3)) 2)
		) mass-list)
	)
)

(prin1 (compute-masses (read-numbers)))