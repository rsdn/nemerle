#
# Sioux Makefile
#

include ../../config.mak

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = ../../boot
NCC = $(NCC_DIR)/ncc.exe

SIOUX_DLL_SOURCES = config.n logger.n httpd.n request.n response.n application.n
SIOUX_EXE_SOURCES = sioux.n

NEMERLE_XML_DLL_SOURCES = xmltemplate.n

APPLICATION_SOURCES = fit/fit.n fit/submission.n

all: copy-dlls Nemerle.Xml.dll Sioux.dll $(APPLICATION_DLL_NAME) httpd.exe

Nemerle.Xml.dll: $(NEMERLE_XML_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Xml -out:$@  $(NEMERLE_XML_DLL_SOURCES)

Sioux.dll: Nemerle.Xml.dll $(SIOUX_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Xml -r:System.Web -r:Nemerle.Xml -out:$@ $(SIOUX_DLL_SOURCES)

Sioux.Fit.dll: Nemerle.Xml.dll Sioux.dll $(APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Xml -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(APPLICATION_SOURCES)

httpd.exe: $(SOURCES) Sioux.dll Sioux.Fit.dll
	$(EXECUTE) $(NCC) -texe -r:System.Xml -r:System.Web -r:Sioux -r:Sioux.Fit -out:$@ $(SIOUX_EXE_SOURCES)

clean:
	rm -f *.exe *.dll *.pdb *~

copy-dlls:
	cp -f $(NCC_DIR)/*.dll .

xml-check:
	set -xe; for f in fit/*.xml ; do xmllint --noout --valid $$f; done

