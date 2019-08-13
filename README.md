# Bad Language

Bad language is a simple lipsy language to help teach myself to write a compiler in Haskell.

I have not used any formal methods here I have just made it up as I have gone along. By compiling to JS there isn't really much 'compiling' to do as JS is quite rich and has the constructs I need to just translate.

Have a look at the .m files (m is a random extension name, you can choose anything) and you get an idea of what this language can do. Not much at the moment, but just enough for a Fibonacci Sequence.

## Examples

### Fibonacci

```
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
```

### Echo

```
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
```

## Running

You can build and run the program using [Haskell Stack](https://docs.haskellstack.org/en/stable/README).
