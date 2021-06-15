test: c.c HS.hs
	ghc -c c.c
	ghc -c HS.hs
	ghc c.o HS.o -o test


.PHONY: clean

clean:
	rm -f c.o
	rm -f HS.hi
	rm -f HS.o
	rm -f HS_stub.h
	rm -f test
