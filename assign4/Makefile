test:
	scalac -cp scala-parser-combinators.jar *.scala
	perl runTests.pl
	rm -f *.class

clean:
	rm -f *.class

pers:
	scalac -cp scala-parser-combinators.jar *.scala
	scala -cp scala-parser-combinators.jar:. Checker list.fun
	rm -f *.class	
