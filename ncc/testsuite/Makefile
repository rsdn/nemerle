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
STAGE2    = $(TOP)/ncc/out.stage2
STAGE3    = $(TOP)/ncc/out.stage3
TESTN     = $(TOP)/ncc/testsuite/test.n
NEM_DLLS  = Nemerle.dll Nemerle.Compiler.dll Nemerle.Macros.dll

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
VERIFY = $(shell if test $(NET_VERSION) = "2.0"; then cygpath -m $(PEVERIFY); else echo ""; fi)

TEST_FILES = positive/*.n negative/*.n
ADDITIONAL_FLAGS = -nowarn:10003

############################################################
# OUTPUT
############################################################

CP = @echo CP      $1

############################################################
# TARGETS
############################################################

all: clean test
	rm -f *.dll *.mdb *.pdb *.exe xml-*.xml *.netmodule ext_test.out test_out.txt

test: test.exe $(NEM_DLLS)
	echo $(TEST_FILES) | xargs -n 200 \
	$(EXECUTE) ./test.exe -unix_colors -s -r "$(NET_ENGINE)" -rp " $(NET_FLAGS)" -p "$(NEM_FLAGS) $(ADDITIONAL_FLAGS)" \
		-verify "$(VERIFY)"

test.exe: test.n
	$(EXECUTE) $(STAGE3)/ncc.exe -r:Nemerle.Compiler -out test.exe $(TESTN)

Nemerle.dll:
	cp $(STAGE3)/Nemerle.dll .

Nemerle.Macros.dll:
	cp $(STAGE3)/Nemerle.Macros.dll .

Nemerle.Compiler.dll:
	cp $(STAGE3)/Nemerle.Compiler.dll .


test-stage1:
	cp $(STAGE1)/Nemerle.stage1.dll .
	cp $(STAGE1)/Nemerle.Compiler.stage1.dll .
	$(EXECUTE) ../../boot/ncc.exe -no-stdlib -r:Nemerle.stage1 \
		-r:Nemerle.Compiler.stage1 -r:System -out test.exe $(TESTN)
	$(EXECUTE) ./test.exe -s -r "$(EXECUTE)" \
		-p "-no-stdlib -r:Nemerle.stage1 -r:System $(NEM_FLAGS) -nowarn:10003" \
		-verify "$(VERIFY)"  $(TEST_FILES)
	rm -f *.dll *.mdb *.exe xml-*.xml *.netmodule ext_test.out test_out.txt

test-generics:
	cp $(STAGE1)/Nemerle.stage1.dll .
	cp $(STAGE1)/Nemerle.Compiler.stage1.dll .
	cp $(STAGE1)/Nemerle.Macros.dll .
	$(EXECUTE) ../../boot/ncc.exe -no-stdlib -r:Nemerle.stage1 \
		-r:Nemerle.Compiler.stage1 -r:System -out test.exe $(TESTN)
	$(EXECUTE) ./test.exe -s -r "$(EXECUTE)" \
		-p "-no-stdlib -r:Nemerle.stage1 -r:System $(NEM_FLAGS) -nowarn:10003,649,10001" \
		-verify "$(VERIFY)"  frommcs/gen*.n
	rm -f *.dll *.mdb *.exe xml-*.xml *.netmodule ext_test.out test_out.txt


test-stage1.5:
	cp $(STAGE1)/Nemerle.stage1.dll .
	cp $(STAGE1)/Nemerle.Compiler.stage1.dll .
	cp $(STAGE1)/Nemerle.Macros.dll .
	cp $(STAGE2)/Nemerle.stage2.dll .
	$(EXECUTE) ../../boot/ncc.exe -no-stdlib -r:Nemerle.stage1 \
		-r:Nemerle.Compiler.stage1 -r:System -out test.exe $(TESTN)
	$(EXECUTE) ./test.exe -s -r "$(EXECUTE)" \
		-p "-no-stdlib -r:Nemerle.stage2 -r:System $(NEM_FLAGS) -nowarn:10003" \
		-verify "$(VERIFY)"  $(TEST_FILES)
	rm -f *.dll *.mdb *.exe xml-*.xml *.netmodule ext_test.out test_out.txt

test-stage2:
	cp $(STAGE2)/Nemerle.stage2.dll .
	cp $(STAGE2)/Nemerle.Compiler.stage2.dll .
	cp $(STAGE2)/Nemerle.Macros.dll .
	$(EXECUTE) ../../boot/ncc.exe -no-stdlib -r:Nemerle.stage2 \
		-r:Nemerle.Compiler.stage2 -r:System -out test.exe $(TESTN)
	$(EXECUTE) ./test.exe -s -r "$(EXECUTE)" \
		-p "-no-stdlib -r:Nemerle.stage2 -r:System $(NEM_FLAGS) -nowarn:10003" \
		-verify "$(VERIFY)"  $(TEST_FILES)
	rm -f *.dll *.mdb *.exe xml-*.xml *.netmodule ext_test.out test_out.txt

clean:
	$(Q) cd positive && $(MAKE) clean
	$(Q) cd negative && $(MAKE) clean
	rm -f *.mdb *.pdb *.exe *.il *.dll *.netmodule ext_test.out core core.[0-9]* xml-*.xml
	rm -f test.err test_error.log a.out test_out.txt

.PHONY: test
