(do
	(set last "")
	(while 
		(neq (get last) "q")
		(do
			(set last (input))
			(print (get last))
		)
	)
)
			