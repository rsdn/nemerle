#
# Copyright (c) 2003, 2004 The University of Wroclaw.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#    2. Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#    3. The name of the University may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
# NO EVENT SHALL THE UNIVERSITY BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
# TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

#
# Sioux Makefile
#

include ../../config.mak

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = ../../ncc/out.stage3
NCC = $(NCC_DIR)/ncc.exe -q
APP_DIR = webapps

SIOUX_DLL_SOURCES = config.n logger.n request.n response.n application.n cookie.n session.n sessionid.n httpd.n app_chooser.n
SIOUX_EXE_SOURCES = sioux.n

SOURCES = $(SIOUX_DLL_SOURCES) $(SIOUX_EXE_SOURCES)

NEMERLE_XML_DLL_SOURCES = xmltemplate.n xmlpipeline.n generators.n 


CSL_APPLICATION_SOURCES = csl04/csl.n csl04/submission.n
CSL_APPLICATION_DLL = Fit.dll

FIT_APPLICATION_SOURCES = fit/fit.n fit/submission.n
FIT_APPLICATION_DLL = Csl.dll

HELLO_APPLICATION_SOURCES = docs/hello/hello.n
HELLO_APPLICATION_DLL = Hello.dll

COOKIES_APPLICATION_SOURCES = docs/cookies/cookies.n
COOKIES_APPLICATION_DLL = Cookies.dll

REQUEST_INFO_APPLICATION_SOURCES = docs/request_info/request_info.n
REQUEST_INFO_APPLICATION_DLL = RequestInfo.dll

REQUEST_HEADERS_APPLICATION_SOURCES = docs/request_headers/request_headers.n
REQUEST_HEADERS_APPLICATION_DLL = RequestHeaders.dll

REQUEST_PARAMS_APPLICATION_SOURCES = docs/request_parameters/request_parameters.n
REQUEST_PARAMS_APPLICATION_DLL = RequestParameters.dll

SESSIONS_APPLICATION_SOURCES = docs/sessions/sessions.n
SESSIONS_APPLICATION_DLL = Sessions.dll

DIRGENERATOR_APPLICATION_SOURCES = docs/directory_generator/directory_generator.n
DIRGENERATOR_APPLICATION_DLL = Dirgenerator.dll

DOCS_SOURCES = docs.n

EXAMPLES = HELLO_APPLICATION             \
	COOKIES_APPLICATION              \
	SESSIONS_APPLICATION             \
	REQUEST_INFO_APPLICATION         \
	REQUEST_HEADERS_APPLICATION      \
	REQUEST_PARAMS_APPLICATION       \
	DIRGENERATOR_APPLICATION

all: Nemerle.Xml.dll Sioux.dll httpd.exe Docs.dll $(EXAMPLES)

Nemerle.Xml.dll: $(NEMERLE_XML_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -out:$@  $(NEMERLE_XML_DLL_SOURCES)

Sioux.dll: Nemerle.Xml.dll $(SIOUX_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -out:$@ $(SIOUX_DLL_SOURCES)

Docs.dll:  Nemerle.Xml.dll Sioux.dll $(DOCS_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(DOCS_SOURCES)
	cp $@ $(APP_DIR)

$(EXAMPLES): Nemerle.Xml.dll Sioux.dll $($@_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$($@_DLL) $($@_SOURCES)
	cp $($@_DLL) $(APP_DIR)

httpd.exe: $(SOURCES) Sioux.dll 
	$(EXECUTE) $(NCC) -texe -r:System.Web -r:Sioux -out:$@ $(SIOUX_EXE_SOURCES)

clean:
	rm -f *.exe *.dll *.pdb *~ webapps/*.dll

copy-dlls:
	cp -f $(NCC_DIR)/*.dll .

xml-check:
	set -xe; for f in fit/*.xml ; do xmllint --noout --valid $$f; done

run: 
	$(EXECUTE) ./httpd.exe -c httpd.conf -l logger 
