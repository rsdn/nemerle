all: NemerleCodeProvider.dll test-d.n
PROVIDER_SOURCES=NemerleCodeCompiler.n NemerleCodeProvider.n NemerleCodeGenerator.n

NemerleCodeProvider.dll: $(PROVIDER_SOURCES)
	mono ../out.stage3/ncc.exe $(PROVIDER_SOURCES) -target-library -o NemerleCodeProvider.dll -r Nemerle.Compiler

test-d.n: NemerleCodeProvider.dll
	xsd test.xml
	xsd test.xsd /c /g:Nemerle.Compiler.NemerleCodeProvider,NemerleCodeProvider.dll
	mv test.n test-c.n 
	xsd test.xsd /d /g:Nemerle.Compiler.NemerleCodeProvider,NemerleCodeProvider.dll
	mv test.n test-d.n 
