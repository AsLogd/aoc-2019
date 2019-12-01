(defun read-numbers ()
	(progn
		(let ((l ()))
			(loop for i = (read *standard-input* nil :eof) 
				until (eq i :eof)
				do (progn
					(push i l)
				)
			)
			(return-from read-numbers l)
		)
	)
)

(defun write-numbers (container)
	(progn
		(loop for i in container
			do (progn
				(format t "~d~%" i)
			)
		)
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