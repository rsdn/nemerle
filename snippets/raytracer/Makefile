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
include ../../config.mak


############################################################
# VARIABLES
############################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
THISDIR = $(shell if cygpath --help 2>/dev/null 1>&2; then echo `cygpath -m $(CURDIR)`; else echo $(CURDIR); fi)

############################################################
# TARGETS
############################################################

all: links run

%.exe: %.n
	$(EXECUTE) ../../ncc/out.stage3/ncc.exe -g $< -o $@

%.exe: %.cs
	gmcs $<

run: ray.exe ray-hand-opt.exe ray-cs.exe ray-compressed.exe ray2.exe ray3.exe
	time $(EXECUTE) ./ray.exe 9 128 > out1.ppm
	time $(EXECUTE) ./ray2.exe 9 128 > out5.ppm
	time $(EXECUTE) ./ray3.exe 9 128 > out6.ppm
	time $(EXECUTE) ./ray-compressed.exe 9 128 > out4.ppm
	time $(EXECUTE) ./ray-hand-opt.exe 9 128 > out2.ppm
	time $(EXECUTE) ./ray-cs.exe 9 128 > out3.ppm

links:
	ln -fs ../../ncc/out.stage3/*.dll .

clean:
	rm -f *.exe *.il *.dll *.netmodule ext_test.out core core.[0-9]*
	rm -f *.ppm *.o a.out *.cmx *.cmi *.pdb *.mdb
