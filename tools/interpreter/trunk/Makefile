MONO = /usr/bin/mono
NCC = /usr/bin/ncc.exe
GLOBAL_OPTS = -pedantic-lexer
COMPILE = $(MONO) $(NCC) $(GLOBAL_OPTS)

all:	evallib mainexe

evallib:
	$(COMPILE) -r Nemerle.Compiler.dll -t library -o eval.dll eval.n

mainexe:
	$(COMPILE) -r eval.dll main.n

clean:
	rm *.dll *.exe *~
