NCC     = ../../boot/ncc.exe
SOURCES = httpd.n config.n logger.n

NEMERLE_XML_DLL_SOURCES = xmltemplate.n

all: Nemerle.Xml.dll httpd.exe tester.exe

httpd.exe: $(SOURCES)
	$(NCC) -texe -r:System.Xml -out:$@ $(SOURCES)

Nemerle.Xml.dll: $(NEMERLE_XML_DLL_SOURCES)
	$(NCC) -tdll -r:System.Xml -out:$@  $(NEMERLE_XML_DLL_SOURCES)

tester.exe: tester.n
	$(NCC) -texe -r:System.Xml -r:Nemerle.Xml -out:$@ $^

clean:
	rm -f httpd.exe
