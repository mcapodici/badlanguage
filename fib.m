(do
	(set f1 0)
	(set f2 1)
	(set x 0)
	(while
		(neq (get x) 50)
		(do	
			(set x (+ (get x) 1))
			(set next (+ (get f1) (get f2)))
			(set f1 (get f2))
			(set f2 (get next))
			(print (get f2))
		)
	)
)