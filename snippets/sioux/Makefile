#
# Sioux Makefile
#

include ~/nemerle/trunk/config.mak

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = ~/nemerle/trunk/boot
NCC = $(NCC_DIR)/ncc.exe -q
APP_DIR = webapps

SIOUX_DLL_SOURCES = config.n logger.n request.n response.n application.n cookie.n session.n sessionid.n httpd.n app_chooser.n
SIOUX_EXE_SOURCES = sioux.n

SOURCES = $(SIOUX_DLL_SOURCES) $(SIOUX_EXE_SOURCES)

NEMERLE_XML_DLL_SOURCES = xmltemplate.n

CSL_APPLICATION_SOURCES = csl04/csl.n csl04/submission.n
FIT_APPLICATION_SOURCES = fit/fit.n fit/submission.n
HELLO_APPLICATION_SOURCES = docs/hello/hello.n
COOKIES_APPLICATION_SOURCES = docs/cookies/cookies.n
REQUEST_INFO_APPLICATION_SOURCES = docs/request_info/request_info.n
REQUEST_HEADERS_APPLICATION_SOURCES = docs/request_headers/request_headers.n
REQUEST_PARAMS_APPLICATION_SOURCES = docs/request_parameters/request_parameters.n
SESSIONS_APPLICATION_SOURCES = docs/sessions/sessions.n
DOCS_SOURCES = docs.n

all: Nemerle.Xml.dll Sioux.dll httpd.exe Hello.dll Cookies.dll RequestInfo.dll RequestHeaders.dll RequestParameters.dll Sessions.dll Docs.dll

Nemerle.Xml.dll: $(NEMERLE_XML_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -out:$@  $(NEMERLE_XML_DLL_SOURCES)

Sioux.dll: Nemerle.Xml.dll $(SIOUX_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -out:$@ $(SIOUX_DLL_SOURCES)

Csl.dll: Nemerle.Xml.dll Sioux.dll $(CSL_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(CSL_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

Fit.dll: Nemerle.Xml.dll Sioux.dll $(FIT_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(FIT_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

Hello.dll: Nemerle.Xml.dll Sioux.dll $(HELLO_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(HELLO_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

Cookies.dll: Nemerle.Xml.dll Sioux.dll $(COOKIES_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(COOKIES_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

RequestInfo.dll: Nemerle.Xml.dll Sioux.dll $(REQUEST_INFO_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(REQUEST_INFO_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

RequestHeaders.dll: Nemerle.Xml.dll Sioux.dll $(REQUEST_HEADERS_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(REQUEST_HEADERS_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

RequestParameters.dll: Nemerle.Xml.dll Sioux.dll $(REQUEST_PARAMS_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(REQUEST_PARAMS_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

Sessions.dll: Nemerle.Xml.dll Sioux.dll $(SESSIONS_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(SESSIONS_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

Docs.dll: Nemerle.Xml.dll Sioux.dll $(DOCS_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(DOCS_SOURCES)
	cp $@ $(APP_DIR)

httpd.exe: $(SOURCES) Sioux.dll 
	$(EXECUTE) $(NCC) -texe -r:System.Web -r:Sioux -out:$@ $(SIOUX_EXE_SOURCES)

clean:
	rm -f *.exe *.dll *.pdb *~ webapps/*.dll

copy-dlls:
	cp -f $(NCC_DIR)/*.dll .

copy-apps: Csl.dll Hello.dll
	cp Csl.dll webapps;cp Hello.dll webapps

xml-check:
	set -xe; for f in fit/*.xml ; do xmllint --noout --valid $$f; done

run: 
	$(EXECUTE) ./httpd.exe -c httpd.conf -l logger 