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
					(setq res (cons (subseq input begin end) res) )
					(setq begin (+ 1 x))
					(setq end (length input))
				)
			)
		)
		finally (return (reverse (setq res (cons (subseq input begin end) res))))
	)
)

(defun set-nth (list n val)
	(loop
		for i from 0
		for j in list
		collect
			(if (= i n)
				val
				j
			)
	)
 )

(defun read-memory ()
	(let ((in (read-line)))
		(map 'list #'parse-integer (split-string in ","))
	)
)


(defun compute-instr (op a b)
	(case op
		(1 (+ a b))
		(2 (* a b))
	)
)

(defun execute-instr (at-op at-a at-b at-result memory)
	(let (
		(op (nth at-op memory))
		(a-ref (nth at-a memory))
		(b-ref (nth at-b memory))
		(res-ref (nth at-result memory))
	)
		(let(
			(a-value (nth a-ref memory))
			(b-value (nth b-ref memory))
		)
			(let (
				(result (compute-instr op a-value b-value))
			)
				;;(print memory)
				;;(format t "---> op: ~d at-a: ~d a-val: ~d  at-b: ~d b-val: ~d res-ref: ~d result: ~d" op a-ref a-value b-ref b-value res-ref result)
				(setq memory (set-nth memory res-ref result))
				;;(print memory)
			)
		)
	)
)

(defun parse-instr (pc memory)
	(execute-instr pc (+ 1 pc) (+ 2 pc) (+ 3 pc) memory)
)

(defun program-ends (pc memory)
	(let (
		(op (nth pc memory))
	)
		; if op is not an instruction
		(if (not (find op '(1 2)))
			(progn
				; if other than 99 - error
				(when (not (eq op 99))
					(print "Something went wrong")
				)
				; if 99 halt program
				(return-from program-ends T)
			)
			; if op is an instruction, keep going
			(return-from program-ends nil)
		)
	)
)

(defun memory-format (memory)
	(format nil "~{~a,~}" memory)
)

(defun execute-program (memory)
	(loop
		for pc = 0 then (+ 4 pc)
		do (setq memory (parse-instr pc memory))
		until (program-ends (+ 4 pc) memory)
		finally (princ (memory-format memory))
	)
)

(execute-program (read-memory))