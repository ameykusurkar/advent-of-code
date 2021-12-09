.PHONY: run example

example: solution
	cat example.txt | ./solution

run: solution
	cat input.txt | ./solution

solution: solution.hs
	ghc solution.hs
