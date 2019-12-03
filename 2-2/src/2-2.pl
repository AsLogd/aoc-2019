(defun split-string (input separator)
	"Returns a list of the substrings separated by 'separator' in 'input'"
	(loop
		for x from 0 below (length input)
		with begin = 0
		with end = (length input)
		with res = ()
		do
			; once we find a comma
			(when (string= (char input x) separator)
				; set current pos as the end of the current chunk
				(setq end x)
				; res is the current chunk
				(setq res (cons (subseq input begin end) res) )
				; the next chunk begins at x+1
				(setq begin (+ 1 x))
				; the chunk is assumed to be the rest of the string
				; unless we find another comma
				(setq end (length input))
			)
		finally (return (reverse (setq res (cons (subseq input begin end) res))))
	)
)

(defun set-nth (list n val)
	"Substitutes the 'n'th element in 'list' by 'val'"
	(loop
		for i from 0
		for j in list
		collect
			; if we in n, take 'val' instead of old value
			(if (= i n)
				val
				j
			)
	)
 )

(defun read-memory ()
	"Read the program memory in comma separated string format. Returns an int list"
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
	"Returns the memory after executing the instruction beginning at at-op"
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
	"Reads and executes the instruction at pc"
	(execute-instr pc (+ 1 pc) (+ 2 pc) (+ 3 pc) memory)
)

(defun program-ends (pc memory)
	"Checks if the program should continue"
	(let (
		(op (nth pc memory))
	)
		(if (not (find op '(1 2)))
			; if op is not an instruction
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
	"Executes the program in 'memory'"
	(loop
		for pc = 0 then (+ 4 pc)
		do (setq memory (parse-instr pc memory))
		until (program-ends (+ 4 pc) memory)
		finally (return (car memory))
	)
)

(defvar *target-value* 19690720)

(defun gravity-assist (memory)
	(loop for noun from 99 downto 0 do
		(loop for verb from 0 upto 99 do
			(let (
				(candidate-memory memory)
				(val 0)
			)
				(setq candidate-memory (set-nth candidate-memory 1 noun))
				(setq candidate-memory (set-nth candidate-memory 2 verb))
				(setq val (execute-program candidate-memory))
				;(format t "noun: ~d verb: ~d target: ~d distance: ~d ~%"
				;	noun verb *target-value* (- *target-value* val)
				;)
				(let (
					(calibrated (eq *target-value* val))
				)
					(when calibrated
						(return-from gravity-assist (list noun verb))
					)
				)
			)
		)
		finally (print "Couldn't calibrate gravity-assist")
	)
)

(defvar *calibration* (gravity-assist (read-memory)))
(defvar *noun* (car *calibration*))
(defvar *verb* (nth 1 *calibration*))
(format t "noun: ~d verb: ~d result: ~d" *noun* *verb* (+ (* 100 *noun*) *verb*) )