(defun split-string (input separator)
	(let ((result ()))
		(loop )
	)
)

(defun read-program ()
	(let ((in (read)))
		(map 'list #'parse-integer (split-string in ","))
	)
)

(print (read-program))