all: unused.csv

Extract: Extract.hs
	ghc -package ghc -fforce-recomp --make Extract.hs -main-is Extract.main -o Extract -fwrite-ide-info

Extract.hie: Extract

uses.facts: Extract Extract.hie
	./Extract

unused.csv: uses.facts test.dl
	souffle -F. -D. test.dl
	cat unused.csv

clean:
	rm -f *.csv
	rm -f *.facts
	rm -f Extract
	rm -f Extract.hie
	rm -f Extract.hi
	rm -f Extract.o
	
