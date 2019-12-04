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

(defun is-dir (dir dir-s)
	(eq dir-s dir)
)

(defun compute-path-bounds (path)
	(loop
		for (dir val) in path
		
		when (is-dir 'up dir)
			sum val into b-up
		when (is-dir 'down dir)
			sum val into b-down
		when (is-dir 'left dir)
			sum val into b-left
		when (is-dir 'right dir)
			sum val into b-right

		finally (return (
			list 
				:up (or b-up 0)
				:down (or b-down  0)
				:left (or b-left  0)
				:right (or b-right 0)
		))
	)
)

(defun create-grid (path-a path-b)
	(let (
		(bounds-a ())
		(bounds-b ())
		(bounds-total ())
	)
		(setq bounds-a (compute-path-bounds path-a))
		(setq bounds-b (compute-path-bounds path-b))
		(setq bounds-total (
			list 
				:up (max (getf bounds-a :up)(getf bounds-b :up))
				:down (max (getf bounds-a :down)(getf bounds-b :down))
				:left (max (getf bounds-a :left)(getf bounds-b :left))
				:right (max (getf bounds-a :right)(getf bounds-b :right))
		))
		(let (
			(width (+ (getf bounds-total :left) (getf bounds-total :right)))
			(height (+ (getf bounds-total :up) (getf bounds-total :down)))
		)
			(let (
				(grid (make-array '(width height) :initial-element nil)
			)
				
			)

		)
	)
)

#|
(defun closest-intersection (path-a path-b) 
	(let (
		(grid (create-grid path-a path-b))
	)
	)
)|#

(defun get-dir (in)
	(let (
		(in-code (char-code in))
	)
		(case in-code
			(85 'up) 
			(68 'down) 
			(76 'left) 
			(82 'right) 
		)
	)
)

(defun create-segment (in)
	(list
		(get-dir (char in 0))
		(parse-integer (subseq in 1))
	)
)

(defun read-paths ()
	(let (
		(in-a (read-line))
		(in-b (read-line))
	)
		(list 
			(map 'list #'create-segment (split-string in-a ","))
			(map 'list #'create-segment (split-string in-b ","))
		)
	)
)

(defvar *paths* (read-paths))
(print (create-grid (nth 0 *paths*) (nth 1 *paths*)))