(do
	(set y 1)
	(set x 5)
	(while
		(neq (get x) 1)
		(do	
			(set y (+ (get y) (get x)))
			(set x (+ (get x) -1))
			(print (get y))
		)
	)
	(get y)
)
			