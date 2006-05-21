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

TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(NCC_DIR)/ncc.exe -q
APP_DIR = webapps

SIOUX_DLL_SOURCES = config.n logger.n request.n response.n application.n cookie.n session.n sessionid.n httpd.n app_chooser.n 
SIOUX_EXE_SOURCES = sioux.n

SOURCES = $(SIOUX_DLL_SOURCES) $(SIOUX_EXE_SOURCES)

NEMERLE_XML_DLL_SOURCES = xmltemplate.n xmlpipeline.n generators.n transformers.n

NEMERLE_DLLS = Nemerle.dll


CSL_APPLICATION_SOURCES = csl04/csl.n csl04/submission.n
FIT_APPLICATION_SOURCES = fit/fit.n fit/submission.n
HELLO_APPLICATION_SOURCES = docs/hello/hello.n
COOKIES_APPLICATION_SOURCES = docs/cookies/cookies.n
REQUEST_INFO_APPLICATION_SOURCES = docs/request_info/request_info.n
REQUEST_HEADERS_APPLICATION_SOURCES = docs/request_headers/request_headers.n
REQUEST_PARAMS_APPLICATION_SOURCES = docs/request_parameters/request_parameters.n
SESSIONS_APPLICATION_SOURCES = docs/sessions/sessions.n
DIRGENERATOR_APPLICATION_SOURCES = docs/directory_generator/directory_generator.n
UPLOAD_APPLICATION_SOURCES = docs/upload/upload.n

DOCS_SOURCES = docs.n

######################################################
# TARGETS
######################################################

all: $(NEMERLE_DLLS) Nemerle.Xml.dll Sioux.dll httpd.exe Docs.dll examples

Nemerle.Xml.dll: $(NEMERLE_XML_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -out:$@  $(NEMERLE_XML_DLL_SOURCES)

Sioux.dll: Nemerle.Xml.dll $(SIOUX_DLL_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -out:$@ $(SIOUX_DLL_SOURCES)

Docs.dll:  Nemerle.Xml.dll Sioux.dll $(DOCS_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(DOCS_SOURCES)
	cp $@ $(APP_DIR)

examples: Cookies.dll RequestInfo.dll RequestHeaders.dll RequestParameters.dll Sessions.dll Dirgenerator.dll Upload.dll

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

Dirgenerator.dll: Nemerle.Xml.dll Sioux.dll $(DIRGENERATOR_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(DIRGENERATOR_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

Upload.dll: Nemerle.Xml.dll Sioux.dll $(UPLOAD_APPLICATION_SOURCES)
	$(EXECUTE) $(NCC) -tdll -r:System.Web -r:Nemerle.Xml -r:Sioux -out:$@ $(UPLOAD_APPLICATION_SOURCES)
	cp $@ $(APP_DIR)

httpd.exe: $(SOURCES) Sioux.dll 
	$(EXECUTE) $(NCC) -texe -r:System.Web -r:Sioux -out:$@ $(SIOUX_EXE_SOURCES)

clean:
	rm -f *.exe *.dll *.pdb *~ webapps/*.dll upload/*

$(NEMERLE_DLLS): $(NCC_DIR)/$@
	cp -f $(NCC_DIR)/$@ $@

xml-check:
	set -xe; for f in fit/*.xml ; do xmllint --noout --valid $$f; done

run: 
	$(EXECUTE) ./httpd.exe -c httpd.conf -l logger 
