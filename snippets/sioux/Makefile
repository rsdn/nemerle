#
# Sioux Makefile
#

NCC_DIR = ../../boot/
NCC = $(NCC_DIR)/ncc.exe

SIOUX_DLL_SOURCES = config.n logger.n httpd.n request.n response.n application.n
SIOUX_EXE_SOURCES = sioux.n

NEMERLE_XML_DLL_SOURCES = xmltemplate.n

APPLICATION_DLL_NAME = Sioux.Fit.dll
APPLICATION_SOURCES = fit/fit.n fit/submission.n

all: copy-dlls Nemerle.Xml.dll Sioux.dll $(APPLICATION_DLL_NAME) httpd.exe

Nemerle.Xml.dll: $(NEMERLE_XML_DLL_SOURCES)
	$(NCC) -tdll -r:System.Xml -out:$@  $(NEMERLE_XML_DLL_SOURCES)

Sioux.dll: $(SIOUX_DLL_SOURCES)
	$(NCC) -tdll -r:System.Xml -r:System.Web -r:Nemerle.Xml -out:$@ $(SIOUX_DLL_SOURCES)

$(APPLICATION_DLL_NAME): $(APPLICATION_SOURCES)
	$(NCC) -tdll -r:System.Xml -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(APPLICATION_SOURCES)

httpd.exe: $(SOURCES) Sioux.dll $(APPLICATION_DLL_NAME)
	$(NCC) -texe -r:System.Xml -r:System.Web -r:Sioux -r:$(APPLICATION_DLL_NAME) -out:$@ $(SIOUX_EXE_SOURCES)

clean:
	rm -f *.exe Nemerle.Xml.dll Sioux.dll $(APPLICATON_DLL_NAME) *.pdb *~

copy-dlls:
	cp -f $(NCC_DIR)/*.dll .

xml-check:
	set -xe; for f in fit/*.xml ; do xmllint --noout --valid $$f; done

