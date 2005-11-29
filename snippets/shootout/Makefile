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

THISDIR = $(shell if cygpath --help 2>/dev/null 1>&2; then echo `cygpath -m $(CURDIR)`; else echo $(CURDIR); fi)
TIME = @/usr/bin/time --format '%C  : real %e, %U+%S (%P%%)' $(EXECUTE) 

############################################################
# TARGETS
############################################################

all:
	$(MAKE) TEST_FILES=$(THISDIR)/*.n -C ../../ncc/testsuite/ test

clean:
	rm -f *.exe *.il *.dll *.pdb *.netmodule ext_test.out core core.[0-9]*
	rm -f test.err test_error.log a.out

run-test: $(TESTNAME).exe cs/$(TESTNAME).exe
	$(TIME) ./$(TESTNAME).exe $(ARGS) > $(TESTNAME).log
	$(TIME) ./cs/$(TESTNAME).exe $(ARGS) > $(TESTNAME)-cs.log

%.exe: %.csharp
	gmcs $< -out:$@

%.exe: %.n
	$(EXECUTE) ../../ncc/out.stage3/ncc.exe $(ADDFLAGS) -i $< -out:$@

t: tests

links:
	ln -sf ../../ncc/out.stage3/*.dll .

tests: links ackermann fibo binarytrees

ackermann:
	$(MAKE) TESTNAME=$@ ARGS=9 run-test

fibo:
	$(MAKE) TESTNAME=$@ ARGS=32 run-test
	
binarytrees:
	$(MAKE) TESTNAME=$@ ARGS=16 run-test
