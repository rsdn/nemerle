#
# Copyright (c) 2003 The University of Wroclaw.
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

include ../../config.mak

lib_src = ../../lib/aliases.n ../../lib/core.n ../../lib/option.n ../../lib/list.n \
	  ../tree.n ../../lib/hashtable.n ../../lib/stack.n ../../lib/queue.n ../../lib/input.n \
	  ../../lib/getopt.n

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(CSC) /debug+ $(CSCFLAGS)

all:
	$(EXECUTE) ../ncc2.exe -reference $(CSCDLLS)System.dll $(lib_src) test.n
	$(COMPILE) $(OUTFLAG)test.exe out.cs
	$(EXECUTE) ./test.exe -p "../../" -c "$(CSC)" -n "../ncc2.exe" -r "$(EXECUTE)"
	rm -f out.cs out.exe test.exe out.netmodule ext_test.out

clean:
	rm -f *.cs *.exe *.il *.dll *.netmodule ext_test.out core core.[0-9]* test.err test_error.log
