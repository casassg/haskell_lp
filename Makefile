all: compiler.c
	dlg -ci parser.dlg scan.c
	g++ -o compiler compiler.c scan.c err.c

compiler.c: compiler.g
	antlr -gt compiler.g

clean: 
	rm -f *.o compiler.c scan.c err.c parser.dlg tokens.h mode.h compiler
