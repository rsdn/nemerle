NCC     = ../../boot/ncc.exe
SOURCES = httpd.n config.n logger.n

NEMERLE_XML_DLL_SOURCES = xmltemplate.n
SIOUX_DLL_SOURCES = application.n fit/fit.n

all: Nemerle.Xml.dll Sioux.dll httpd.exe tester.exe

httpd.exe: $(SOURCES) Sioux.dll
	$(NCC) -texe -r:System.Xml -r:Sioux -out:$@ $(SOURCES)

Nemerle.Xml.dll: $(NEMERLE_XML_DLL_SOURCES)
	$(NCC) -tdll -r:System.Xml -out:$@  $(NEMERLE_XML_DLL_SOURCES)

Sioux.dll: $(SIOUX_DLL_SOURCES) Nemerle.Xml.dll
	$(NCC) -tdll -r:System.Xml -r:Nemerle.Xml -r:System.Web -out:$@  $(SIOUX_DLL_SOURCES)

tester.exe: tester.n
	$(NCC) -texe -r:System.Xml -r:Nemerle.Xml -out:$@ $^

clean:
	rm -f httpd.exe

v:
	set -xe; for f in fit/*.xml ; do \
		xmllint --noout --valid $$f; done
