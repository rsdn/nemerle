#
# Copyright (c) 2003-2005 The University of Wroclaw.
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
# Include configuration determined by configure script.
TOP = ../..
include $(TOP)/config.mak


############################################################
# VARIABLES
############################################################

STAGE1    = $(TOP)/ncc/out.stage1
STAGE3    = $(TOP)/ncc/out.stage3
TESTN     = $(TOP)/ncc/testsuite/test.n

export PATH = $(shell echo $$PATH):$(shell pwd)/$(STAGE3)/
export MONO_PATH = $(shell echo $$MONO_PATH):$(shell pwd)/$(STAGE3)

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
VERIFY = $(shell if test $(NET_VERSION) = "2.0"; then cygpath -m $(PEVERIFY); else echo ""; fi)

TEST_FILES = positive/*.n negative/*.n

############################################################
# OUTPUT
############################################################

CP = @echo CP      $1

############################################################
# TARGETS
############################################################

all:
	cp $(STAGE3)/Nemerle.dll .
	cp $(STAGE3)/Nemerle.Compiler.dll .
	$(EXECUTE) $(STAGE3)/ncc.exe -out test.exe $(TESTN)
	$(EXECUTE) ./test.exe -s -r "$(EXECUTE)" -n "$(STAGE3)/ncc.exe" -p "$(NEM_FLAGS)" -verify "$(VERIFY)" $(TEST_FILES)
	rm -f *.dll *.exe xml-*.xml *.netmodule ext_test.out test_out.txt

test-stage1:
	cp $(STAGE1)/Nemerle.stage1.dll .
	cp $(STAGE1)/Nemerle.Compiler.stage1.dll .
	$(EXECUTE) ../../boot/ncc.exe -no-stdlib -r:Nemerle.stage1 -r:System -out test.exe $(TESTN)
	$(EXECUTE) ./test.exe -s -r "$(EXECUTE)" -n "$(STAGE1)/ncc.exe" \
		-p "-no-stdlib -r:Nemerle.stage1 -r:System $(NEM_FLAGS)" \
		-verify "$(VERIFY)" $(TEST_FILES)
	rm -f *.dll *.exe xml-*.xml *.netmodule ext_test.out test_out.txt

all-mcs:
	$(MAKE) all TEST_FILES=frommcs/*.n
	
clean:
	$(Q) cd positive && $(MAKE) clean
	$(Q) cd negative && $(MAKE) clean
	rm -f *.exe *.il *.dll *.netmodule ext_test.out core core.[0-9]* xml-*.xml
	rm -f test.err test_error.log a.out test_out.txt
