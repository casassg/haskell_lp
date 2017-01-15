all: reader.c program
	dlg -ci parser.dlg scan.c
	g++ -o reader reader.c scan.c err.c

reader.c: reader.g
	antlr -gt reader.g

program: program.hs
	ghc --make program.hs

clean: 
	rm -f *.o reader.c scan.c err.c parser.dlg tokens.h mode.h reader program.hi program
