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
			(width (+ 1 (+ (getf bounds-total :left) (getf bounds-total :right))))
			(height (+ 1 (+ (getf bounds-total :up) (getf bounds-total :down))))
			(src-y (getf bounds-total :up))
			(src-x (getf bounds-total :left))
		)
			(let (
				(grid (list 
					:src (make-pos src-x src-y) 
					:content (make-array (list width height) :initial-element #\.)))
			)
				(setf (aref (getf grid :content) src-x src-y) #\o)
				(return-from create-grid grid)
			)

		)
	)
)

(defun print-grid (grid)
	(destructuring-bind (n m) (array-dimensions (getf grid :content))
		(loop for i from 0 below n do
			(loop for j from 0 below m do
				(format t "~C" (aref grid i j))
				finally (format t "~%"))))
)

(defun get-i-x (i x dir)
	(case dir
		(left (- x i))
		(right (+ x i))
		(otherwise x)
	)
)

(defun get-i-y (i y dir)
	(case dir
		(up (- y i))
		(down (+ y i))
		(otherwise y)
	)
)

(defun make-pos (x y)
	(list :x x :y y)
)

(defun pos-distance (a b)
	(make-pos 
		(abs (- (getf a :x) (getf b :x)))
		(abs (- (getf a :y) (getf b :y)))
	)
)

(defun make-intersection (start pos)
	(list
		:pos pos
		:distance (pos-distance start pos)
	)
)

(defun add-segment (grid start segment)
	(let 
		(
			(content (getf grid :content))
			(dir (getf grid :dir))
			(start-x (getf start :x))
			(start-y (getf start :y))
			(val (getf segment :val))
		)
		(let 
			(
				(pos (list start-x start-y))
				(x start-x)
				(y start-y)
				(pos-ref ())
				(intersections ())
			)
			(print "addsegmetn")

			(loop for i from 1 upto val do 
				(progn
					(setq x (get-i-x i x dir))
					(setq y (get-i-y i y dir))
					(if (char= (aref content x y) #\-)
						(progn
							(setf (aref content x y) #\X)

							(setq intersections 
								(cons (make-intersection start pos) intersections)
							)

						)
						(setf (aref content x y) #\-)
					)
				)
				finally (return 
					(list 
						:pos (make-pos x y) 
						:intersections intersections
					)
				)
			)
		)
	)
)

(defun add-path (grid path)
	(let 
		(
			(start (getf grid :src))
			(result ())
			(intersections ())
		)
		(print "add path")
		(print path)
		(loop for segment in path do 
			(print segment)

			(setq result (add-segment grid start segment))
			
			(setq start (getf result :pos))
			(setq intersections (getf result :intersections))
			finally (return intersections)
		)
	)
)

(defun closest-intersection (path-a path-b) 
	(let (
		(grid (create-grid path-a path-b))
		(intersections ())
	)
		(print "ble")

		(setq intersections (add-path grid path-a))
		(print "Intersections A:")
		(print intersections)
		(setq intersections (add-path grid path-b))
		(print "Intersections B:")
		(print intersections)
	)
)

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
		:dir (get-dir (char in 0))
		:val (parse-integer (subseq in 1))
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
(closest-intersection (first *paths*) (second *paths*))
