(defun split-string (input separator)
	(loop
		for x from 0 below (length input)
		with begin = 0
		with end = (length input)
		with res = ()
		do (progn 
			(if (string= (char input x) ",")
				(progn 
					(setq end x)
					(setq res (cons (parse-integer (subseq input begin end)) res))
					(setq begin (+ 1 x))
					(setq end (length input))
				)
			)
		)
		finally (return (setq res (cons (parse-integer (subseq input begin end)) res)))
	)
)

(defun read-program ()
	(let ((in (read)))
		(map 'list #'parse-integer (split-string in ","))
	)
)

;;(print (read-program))

(split-string "1,2,3,4,1204,1,42" ",")
