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

COMPILER_PATH = ../boot/

RUN_COMPILER = $(TIME) $(EXECUTE) $(COMPILER_PATH)ncc.exe -r:$(COMPILER_PATH)stdmacros.dll

GENERIC_SRC = ../lib/aliases.n

NEMERLE_DLL_SRC = \
	../lib/aliases.n		\
	../lib/core.n			\
	../lib/getopt.n			\
	../lib/hashtable.n		\
	../lib/icollection.n		\
	../lib/ienumerable.n		\
	../lib/ienumerator.n		\
	../lib/input.n			\
	../lib/linkedlist.n		\
	../lib/list.n			\
	../lib/option.n			\
	../lib/queue.n			\
	../lib/stack.n			\
	../lib/tree.n			\

NEMERLE_COMPILER_DLL_SRC = \
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
	ast.n \
	tree.n \
	parsetree.n \
	typedtree.n \
	util.n \
	passes.n \
	scan_globals.n \
	tyvars.n \
	tyutil.n \
	parser.n \
	flags.n \
	lexer.n \
	tycon.n \
	cgtree.n \
	cgexpr.n \
	cgopt.n \
	tycodec.n \
	external.n \
	xmldump.n \
	cgil.n \
	cgmatch.n \


NCC_EXE_SRC = main.n

STDMACROS_DLL_SRC = stdmacros.n

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)

all: stage1

stage1:
	$(MAKE) STAGE=$@ COMPILER_PATH=../boot/ stage
	
stage2:
	$(MAKE) STAGE=$@ COMPILER_PATH=out.stage1/ stage
	
stage3:
	$(MAKE) STAGE=$@ COMPILER_PATH=out.stage2/ stage
	
stage:
	mkdir -p out.$(STAGE)
	$(MAKE) stage-files

stage-files: \
		out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll \
		out.$(STAGE)/stdmacros.dll \
		out.$(STAGE)/ncc.exe

lib: out.stage1/Nemerle.dll

boot: stage1 stage2 stage3 compare

compare:
	$(MAKE) compare-one CMP_TARGET=Nemerle.dll
	$(MAKE) compare-one CMP_TARGET=Nemerle.Compiler.dll
	$(MAKE) compare-one CMP_TARGET=stdmacros.dll
	$(MAKE) compare-one CMP_TARGET=ncc.exe

compare-one: out.stage2/$(CMP_TARGET) out.stage2/$(CMP_TARGET) 
	$(DASM) out.stage2/$(CMP_TARGET) | \
		grep -v -E '(^//|// GUID|^\.assembly ncc|^\.module |MVID)' \
		> $(CMP_TARGET)2.il
	$(DASM) out.stage3/$(CMP_TARGET) | \
		grep -v -E '(^//|// GUID|^\.assembly ncc|^\.module |MVID)' \
		> $(CMP_TARGET)3.il
	diff -u $(CMP_TARGET)2.il $(CMP_TARGET)3.il
	rm -f $(CMP_TARGET)2.il $(CMP_TARGET)3.il

out.$(STAGE)/Nemerle.dll: $(COMPILER) $(NEMERLE_DLL_SRC)
	$(RUN_COMPILER) -target-library -out:$@ $(NEMERLE_DLL_SRC)

out.$(STAGE)/Nemerle.Compiler.dll: $(COMPILER) out.$(STAGE)/Nemerle.dll $(NEMERLE_COMPILER_DLL_SRC)
	$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		$(GENERIC_SRC) $(NEMERLE_COMPILER_DLL_SRC)

out.$(STAGE)/stdmacros.dll: $(COMPILER) out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll $(STDMACROS_DLL_SRC)
	$(RUN_COMPILER) -target-library -out:$@ \
		-reference:$(DLL_PATH)System.dll \
		-reference:out.$(STAGE)/Nemerle.dll \
		-reference:out.$(STAGE)/Nemerle.Compiler.dll \
		$(GENERIC_SRC) $(STDMACROS_DLL_SRC)

out.$(STAGE)/ncc.exe: $(COMPILER) out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll $(NCC_EXE_SRC)
	$(RUN_COMPILER) -target-exe -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		-reference:out.$(STAGE)/Nemerle.Compiler.dll \
		$(GENERIC_SRC) $(NCC_EXE_SRC)

sync:
	cd out.stage2 ; \
	for x in * ; do \
		mv -vf ../../boot/$$x ../../boot/$$x.old ; \
		cp -vf $$x ../../boot/$$x ; \
	done

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
