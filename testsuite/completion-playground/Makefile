#

TOP = ../../..
include $(TOP)/config.mak

all: compiler compl3.exe test

compiler: Nemerle.dll Nemerle.Compiler.dll
	rm -fr Nemerle.Macros.dll

Nemerle.dll: ../../out.stage3/$@
	cp -f ../../out.stage3/$@ .

Nemerle.Compiler.dll: ../../out.stage3/$@
	cp -f ../../out.stage3/$@ .

Nemerle.Macros.dll: ../../out.stage3/$@
	cp -f ../../out.stage3/$@ .

compl3.exe: compl3.n
	$(ENGINE) ../../out.stage3/ncc.exe compl3.n -r Nemerle.Compiler -o $@

test: compl-s.n compl-s2.n Nemerle.Macros.dll
	$(ENGINE) ./compl3.exe compl-s.n
	$(ENGINE) ./compl3.exe compl-s2.n

clean:
	rm -fr *exe *dll