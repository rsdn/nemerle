all: NemerleCodeProvider.dll test-d.n

NemerleCodeProvider.dll: *.cs
	mcs *.cs -target:library -o NemerleCodeProvider.dll

test-d.n: NemerleCodeProvider.dll
	xsd test.xml
	xsd test.xsd /c /g:Nemerle.Contrib.NemerleCodeProvider,NemerleCodeProvider.dll
	mv test.n test-c.n 
	xsd test.xsd /d /g:Nemerle.Contrib.NemerleCodeProvider,NemerleCodeProvider.dll
	mv test.n test-d.n 
