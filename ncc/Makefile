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

n_src = option.n list.n util.n parsetree.n tree.n scan_globals.n treeutil.n \
	bind_types.n tyvars.n tyinfo.n tyutil.n check_constraints.n \
	ast.n tyexpr.n typedtree.n extensions.n cgtree.n cgexpr.n \
	cgflat.n env.n lib/core.n
cs_src = lexer.cs main.cs parser.cs
all_src = $(n_src) $(cs_src)
all_cs = out.cs $(cs_src)

CSFLAGS = -nowarn:162
COMPILE = $(CSC) $(CSFLAGS)

all: ncc.exe

# Do compiler bootstrap, compare results from stage2 and stage3,
# generate ncc3.exe binary to be put in release for bootstrap.
# Finally run testsuite.
boot: ncc.exe
	$(NET_ENGINE) --debug ncc.exe $(n_src)
	$(COMPILE) -debug+ -o ncc2.exe $(all_cs)
	mv out.cs out2.cs
	$(NET_ENGINE) --debug ncc2.exe $(n_src)
	cmp -s out2.cs out.cs
	$(COMPILE) -o ncc3.exe $(all_cs)
	mv out.cs out3.cs
	$(MAKE) testsuite

sync:
	mv ../boot/ncc.exe ../boot/ncc-old.exe
	cp ncc3.exe ../boot/ncc.exe

ncc.exe: $(all_src)
	$(NET_ENGINE) ../boot/ncc.exe $(n_src)
	$(COMPILE) -debug+ -o $@ $(all_cs)

.PHONY: t

t: testsuite

testsuite:
	$(MAKE) -C t

parser.cs: parser.jay
	jay -v -t -c parser.jay < `jay -p`/skeleton.cs > parser.cs
	perl -p -i -e 's/(public void yyerror )/virtual $$1/' parser.cs


install: ncc.exe
	if test ! -d $(BINDIR) ; then mkdir -p $(BINDIR) ; fi
	$(INSTALL) -m 755 ncc.exe $(BINDIR)/ncc.exe
	$(INSTALL) -m 755 ncc $(BINDIR)/ncc


clean:
	rm -f out.cs out[0-9].cs parser.cs *.exe y.output
