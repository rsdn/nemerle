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
# Include configuration determined by configure script.

TOPLEVEL=../../..

include $(TOPLEVEL)/config.mak

############################################################
# VARIABLES
############################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)

############################################################
# TARGETS
############################################################

all: docs
	$(EXECUTE) $(TOPLEVEL)/ncc/out.stage3/ncc.exe -out test.exe $(TOPLEVEL)/ncc/testsuite/test.n
	cp $(TOPLEVEL)/ncc/out.stage3/Nemerle.dll .
	$(EXECUTE) ./test.exe -r "$(EXECUTE)" -n "$(TOPLEVEL)/ncc/out.stage3/ncc.exe -D ALL"
	rm -f out.dll out.exe out.netmodule ext_test.out Nemerle.dll

docs: grokking-src-FOO.html grokking-src-BAR.html grokking-src-HELLO.html grokking-src-FIBONACCI1.html grokking-src-FIBONACCI2.html

grokking-src-FOO.html: grokking.n
	$(TOPLEVEL)/misc/htmldumper/dumphtml grokking.n FOO grokking-src-FOO.html

grokking-src-BAR.html: grokking.n
	$(TOPLEVEL)/misc/htmldumper/dumphtml grokking.n BAR grokking-src-BAR.html

grokking-src-HELLO.html: grokking.n
	$(TOPLEVEL)/misc/htmldumper/dumphtml grokking.n HELLO grokking-src-HELLO.html

grokking-src-FIBONACCI1.html: grokking.n
	$(TOPLEVEL)/misc/htmldumper/dumphtml grokking.n FIBONACCI1 grokking-src-FIBONACCI1.html

grokking-src-FIBONACCI2.html: grokking.n
	$(TOPLEVEL)/misc/htmldumper/dumphtml grokking.n FIBONACCI2 grokking-src-FIBONACCI2.html

clean:
	$(MAKE) -C sioux clean
	rm -f *.exe *.il *.dll *.netmodule ext_test.out core core.[0-9]*
	rm -f test.err test_error.log a.out
