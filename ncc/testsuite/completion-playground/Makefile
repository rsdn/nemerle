#

TOP = ../../..
include $(TOP)/config.mak

all: compiler compl2.exe test

compiler: Nemerle.dll Nemerle.Compiler.dll
	rm -fr Nemerle.Macros.dll

Nemerle.dll: ../../out.stage3/$@
	cp -f ../../out.stage3/$@ .

Nemerle.Compiler.dll: ../../out.stage3/$@
	cp -f ../../out.stage3/$@ .

Nemerle.Macros.dll: ../../out.stage3/$@
	cp -f ../../out.stage3/$@ .

compl2.exe: compl2.n
	$(ENGINE) ../../out.stage3/ncc.exe compl2.n -r Nemerle.Compiler -o compl2.exe

test: compl-s.n compl-s2.n Nemerle.Macros.dll
	$(ENGINE) ./compl2.exe compl-s.n
	$(ENGINE) ./compl2.exe compl-s2.n

clean:
	rm -fr *exe *dll