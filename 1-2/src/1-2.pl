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

(defun compute-mass (mass)
	(- (floor (/ mass 3)) 2)
)

(defun compute-mass-extra (mass)
	(let ((fuel (compute-mass mass)))
		(if (<= fuel 0)
			0
			(+ fuel (compute-mass-extra fuel))
		)
	)
)

(defun compute-masses (mass-list)
	(reduce '+ 
		(map 'list #'compute-mass-extra mass-list)
	)
)

(prin1 (compute-masses (read-numbers)))