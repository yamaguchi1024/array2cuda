haskell:
	stack ghc array2cuda.hs
	./array2cuda
clean:
	rm array2cuda.o array2cuda.hi array2cuda
