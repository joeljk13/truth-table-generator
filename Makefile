gen: gen.hs
	ghc -O2 gen.hs

.PHONY: clean
clean:
	rm -f gen gen.hi gen.o
