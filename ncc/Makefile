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

lib_src = ../lib/aliases.n ../lib/core.n ../lib/option.n ../lib/list.n \
	  tree.n ../lib/hashtable.n ../lib/stack.n ../lib/queue.n ../lib/input.n \
	  ../lib/getopt.n ../lib/icollection.n ../lib/tree.n ../lib/ienumerator.n \
	  ../lib/ienumerable.n

src = \
	extensions.n \
	globalenv.n \
	localvalue.n \
	typingcontext.n \
	macros.n \
	macroregistry.n \
	typattern.n \
	tyexpr.n \
	members.n \
	tyinfo.n \
	$(lib_src) \
	ast.n \
	parsetree.n \
	typedtree.n \
	util.n \
	passes.n \
	scan_globals.n \
	tyvars.n \
	tyutil.n \
	parser.n \
	main.n \
	flags.n \
	lexer.n \
	tycon.n \
	cgtree.n \
	cgexpr.n \
	cgopt.n \
	external.n \
	xmldump.n \
	cgil.n \


EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NEMFLAG = -self -r $(DLL_PATH)System.dll

all: ncc.exe

# Do compiler bootstrap, compare results from stage2 and stage3,
# generate ncc2.exe binary to be put in release for bootstrap.
# Finally run testsuite.
boot: ncc.exe
	rm -f out.cs
	$(EXECUTE) ./ncc.exe $(NEMFLAG) -texe -out ncc2.exe $(src)
	$(NGEN) ncc2.exe
	$(EXECUTE) ./ncc2.exe $(NEMFLAG) -texe -out ncc3.exe $(src)
	$(DASM) ncc2.exe | grep -v '^//' | grep -v '// GUID' | grep -v MVID > ncc2.il
	$(DASM) ncc3.exe | grep -v '^//' | grep -v '// GUID' | grep -v MVID > ncc3.il  
	diff -u ncc2.il ncc3.il
	rm -f ncc2.il ncc3.il ncc3.exe
	$(MAKE) tests

sync:
	mv ../boot/ncc.exe ../boot/ncc-old.exe
	cp ncc2.exe ../boot/ncc.exe

ncc.exe: $(src)
	$(EXECUTE) ../boot/ncc.exe $(NEMFLAG) -texe -out $@ $(src)

stdlib.dll: $(lib_src)
	$(EXECUTE) ./ncc.exe -tdll -out $@ $(lib_src)

../config.mak:
	$(MAKE) -C .. config.mak

.PHONY: boot all tests t test ncc-il

t test: tests

tests:
	$(MAKE) -C testsuite

install: ncc.exe
	if test ! -d $(BINDIR) ; then mkdir -p $(BINDIR) ; fi
	$(NGEN) ncc.exe
	$(INSTALL) -m 644 ncc.exe.so $(BINDIR)/ncc.so
	$(INSTALL) -m 755 ncc.exe $(BINDIR)/ncc

clean:
	$(MAKE) -C testsuite clean
	rm -f *.exe *.exe.so *.dll.so ncc*.il cgil-test.dll *.pdb core.[0-9]* core
