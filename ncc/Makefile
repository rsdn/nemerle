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

include ../config.mak

lib_src = ../lib/core.n ../lib/option.n ../lib/list.n tree.n ../lib/hashtable.n ../lib/stack.n

n_src = \
	$(lib_src) \
	ast.n \
	parsetree.n \
	typedtree.n \
	util.n \
	csglue.n \
	env.n \
	passes.n \
	scan_globals.n \
	tyvars.n \
	tyinfo.n \
	tyutil.n \
	extensions.n \
	tyexpr.n \
	macros.n \
	cgtree.n \
	cgexpr.n \
	cgflat.n \
	xmldump.n \
	typattern.n \
	external.n \
	cgopt.n 
#	lexer.n


cs_src  = lexer.cs main.cs parser.cs
all_src = $(n_src) $(cs_src)
all_cs  = $(cs_src)

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
COMPILE = $(CSC) /optimize+ $(CSCFLAGS)


all: ncc.exe

# Do compiler bootstrap, compare results from stage2 and stage3,
# generate ncc2.exe binary to be put in release for bootstrap.
# Finally run testsuite.
boot: ncc.exe
	rm -f out.cs
	$(EXECUTE) ./ncc.exe $(n_src)
	mv -f out.cs out2.cs
	$(COMPILE) $(OUTFLAG)ncc2.exe $(all_cs) out2.cs
	$(EXECUTE) ./ncc2.exe $(n_src)
	cmp -s out2.cs out.cs
	rm -f out.cs
	$(MAKE) tests

sync:
	mv ../boot/ncc.exe ../boot/ncc-old.exe
	cp ncc2.exe ../boot/ncc.exe

ncc.exe: $(all_src)
	$(EXECUTE) ../boot/ncc.exe $(n_src)
	mv -f out.cs out1.cs
	$(COMPILE) $(OUTFLAG)$@ $(all_cs) out1.cs

../config.mak:
	$(MAKE) -C .. config.mak

.PHONY: boot all tests t test

t test: tests

tests:
	$(MAKE) -C testsuite

parser.cs: parser.jay
	jay -v -t -c parser.jay < `jay -p`/skeleton.cs > parser.cs
	perl -p -i -e 's/(public void yyerror )/virtual $$1/' parser.cs

install: ncc.exe
	if test ! -d $(BINDIR) ; then mkdir -p $(BINDIR) ; fi
	$(INSTALL) -m 755 ncc.exe $(BINDIR)/ncc

clean:
	rm -f out.cs out[0-9].cs parser.cs *.exe y.output
