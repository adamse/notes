all: unused.csv

packages = $(addprefix -package , ghc)

extract_outputs = Extract
extract_outputs += $(addprefix Extract., hie hi o)

${extract_outputs}: Extract.hs
	ghc ${packages} -fforce-recomp --make Extract.hs -main-is Extract.main -o Extract -fwrite-ide-info

Extract.hie: Extract

facts = $(addsuffix .facts, uses defined exported)

${facts} : Extract Extract.hie
	./Extract

unused.csv: ${facts} unused.dl
	souffle -F. -D. unused.dl
	cat unused.csv

clean:
	rm -f *.csv
	rm -f ${facts}
	rm -f ${extract_outputs}
