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
include ../config.mak

############################################################
# VARIABLES
############################################################

COMPILER_PATH = ../boot/
COMPILER      = $(COMPILER_PATH)ncc.exe
RUN_COMPILER  = $(Q)$(TIME) $(EXECUTE) $(COMPILER) -no-stdlib $(COMP_FLAGS)

GENERIC_SRC =

NEMERLE_DLL_SRC = \
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
	../lib/idictionaryenumerator.n	\
	../lib/idictionary.n		\
	../lib/dictionaryentry.n	\
	../lib/heap.n			\
	../lib/icomparable.n


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


NCC_EXE_SRC       = main.n
STDMACROS_DLL_SRC = stdmacros.n

DISTFILES = \
	ChangeLog                   \
	Makefile                    \
	$(NEMERLE_COMPILER_DLL_SRC) \
	$(NCC_EXE_SRC)              \
	$(STDMACROS_DLL_SRC)        \


############################################################
# OUTPUT
############################################################

COMPARE = @echo COMPARE $1 $2 $3
DEAS    = @echo DASM    $1 $2
AOT     = @echo NGEN    $1
COMP    = @echo COMPILE $1
MKDIR   = @echo MKDIR   $1
TAR     = @echo TAR     $1
CP      = @echo CP      $1

############################################################
# TARGETS
############################################################

# Default rule is to build a compiler.  It is mainly used by
# developers as they are the only people invoking make from
# inside this directory.  All other people should invoke
# make boot, or have it invoked from the top-level make.
all: stage1

# This is necessary to make sure, that configuration file
# has been generated, and it is up to date.
../config.mak:
	cd .. && $(MAKE) config.mak

stage1:
	$(Q)$(MAKE) STAGE=$@ COMPILER_PATH=../boot/ aux-stage

stage2:
	$(Q)$(MAKE) STAGE=$@ COMPILER_PATH=out.stage1/ aux-stage

stage3:
	$(Q)$(MAKE) STAGE=$@ COMPILER_PATH=out.stage2/ last-stage

aux-stage:
	$(MKDIR) out.$(STAGE)
	$(Q)mkdir -p out.$(STAGE)
	$(Q)$(MAKE) aux-stage-files

aux-stage-files: \
		out.$(STAGE)/aliases.n		\
		out.$(STAGE)/Nemerle.$(STAGE).dll \
		out.$(STAGE)/Nemerle.Compiler.$(STAGE).dll \
		out.$(STAGE)/stdmacros.dll \
		out.$(STAGE)/ncc.exe

last-stage:
	mkdir -p out.$(STAGE)
	$(MAKE) last-stage-files

last-stage-files: \
		out.$(STAGE)/aliases.n		\
		out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll \
		out.$(STAGE)/stdmacros.dll \
		out.$(STAGE)/ncc.exe

lib: out.stage1/Nemerle.stage1.dll

boot: stage1 stage2 stage3 compare ngen-stage3 tests

ngen-stage3:
	$(AOT) Nemerle.dll
	$(Q)$(NGEN) out.stage3/Nemerle.dll
	$(AOT) Nemerle.Compiler.dll
	$(Q)$(NGEN) out.stage3/Nemerle.Compiler.dll
	$(AOT) stdmacros.dll
	$(Q)$(NGEN) out.stage3/stdmacros.dll
	$(AOT) ncc.exe
	$(Q)$(NGEN) out.stage3/ncc.exe

compare:
	$(Q)$(MAKE) compare-one-with-stage CMP_TARGET=Nemerle
	$(Q)$(MAKE) compare-one-with-stage CMP_TARGET=Nemerle.Compiler
	$(Q)$(MAKE) compare-one CMP_TARGET=stdmacros.dll
	$(Q)$(MAKE) compare-one CMP_TARGET=ncc.exe

compare-one:
	$(Q)test -f out.stage2/$(CMP_TARGET)
	$(Q)test -f out.stage3/$(CMP_TARGET) 
	$(DEAS) [stage2] $(CMP_TARGET)
	$(Q)$(DASM) out.stage2/$(CMP_TARGET) | \
		grep -v -E '(^//|// GUID|MVID)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)2.il
	$(DEAS) [stage3] $(CMP_TARGET)
	$(Q)$(DASM) out.stage3/$(CMP_TARGET) | \
		grep -v -E '(^//|// GUID|MVID)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)3.il
	$(COMPARE) [stage2] [stage3] $(CMP_TARGET)
	$(Q)diff -u -w $(CMP_TARGET)2.il $(CMP_TARGET)3.il
	$(Q)rm -f $(CMP_TARGET)2.il $(CMP_TARGET)3.il

compare-one-with-stage:
	$(Q)test -f out.stage2/$(CMP_TARGET).stage2.dll
	$(Q)test -f out.stage3/$(CMP_TARGET).dll
	$(DEAS) [stage2] $(CMP_TARGET)
	$(Q)$(DASM) out.stage2/$(CMP_TARGET).stage2.dll | \
		grep -v -E '(^//|// GUID|MVID)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)2.il
	$(DEAS) [stage3] $(CMP_TARGET)
	$(Q)$(DASM) out.stage3/$(CMP_TARGET).dll | \
		grep -v -E '(^//|// GUID|MVID)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)3.il
	$(COMPARE) [stage2] [stage3] $(CMP_TARGET)
	$(Q)diff -u -w $(CMP_TARGET)2.il $(CMP_TARGET)3.il
	$(Q)rm -f $(CMP_TARGET)2.il $(CMP_TARGET)3.il

../boot/aliases.n: ../lib/aliases.n
	$(Q)cp -f $< $@

out.%/aliases.n: ../lib/aliases.n
	$(Q)cp -f $< $@

out.$(STAGE)/Nemerle.dll out.$(STAGE)/Nemerle.$(STAGE).dll: \
		$(COMPILER) $(COMPILER_PATH)aliases.n $(NEMERLE_DLL_SRC)
	$(COMP) [$(STAGE)] Nemerle.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ $(GENERIC_SRC) $(NEMERLE_DLL_SRC)

out.$(STAGE)/Nemerle.Compiler.dll: \
		$(COMPILER) out.$(STAGE)/Nemerle.dll $(NEMERLE_COMPILER_DLL_SRC)
	$(COMP) [$(STAGE)] Nemerle.Compiler.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		$(GENERIC_SRC) $(NEMERLE_COMPILER_DLL_SRC)

out.$(STAGE)/Nemerle.Compiler.$(STAGE).dll: \
		$(COMPILER) out.$(STAGE)/Nemerle.$(STAGE).dll $(NEMERLE_COMPILER_DLL_SRC)
	$(COMP) [$(STAGE)] Nemerle.Compiler.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.$(STAGE)/Nemerle.$(STAGE).dll \
		$(GENERIC_SRC) $(NEMERLE_COMPILER_DLL_SRC)

out.stage1/stdmacros.dll: $(COMPILER) out.stage1/Nemerle.stage1.dll \
		out.stage1/Nemerle.Compiler.stage1.dll $(STDMACROS_DLL_SRC)
	$(COMP) [stage1] stdmacros.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.stage1/Nemerle.stage1.dll \
		-reference:out.stage1/Nemerle.Compiler.stage1.dll \
		$(GENERIC_SRC) $(STDMACROS_DLL_SRC)

out.stage2/stdmacros.dll: $(COMPILER) out.stage2/Nemerle.stage2.dll \
		out.stage2/Nemerle.Compiler.stage2.dll $(STDMACROS_DLL_SRC)
	$(COMP) [stage2] stdmacros.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.stage2/Nemerle.stage2.dll \
		-reference:out.stage2/Nemerle.Compiler.stage2.dll \
		$(GENERIC_SRC) $(STDMACROS_DLL_SRC)

out.stage3/stdmacros.dll: $(COMPILER) out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll $(STDMACROS_DLL_SRC)
	$(COMP) [stage3] stdmacros.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		-reference:out.$(STAGE)/Nemerle.Compiler.dll \
		$(GENERIC_SRC) $(STDMACROS_DLL_SRC)

out.stage1/ncc.exe: $(COMPILER) out.stage1/Nemerle.stage1.dll \
		out.stage1/Nemerle.Compiler.stage1.dll $(NCC_EXE_SRC)
	$(COMP) [stage1] ncc.exe
	$(Q)$(RUN_COMPILER) -target-exe -out:$@ \
		-reference:out.stage1/Nemerle.stage1.dll \
		-reference:out.stage1/Nemerle.Compiler.stage1.dll \
		$(GENERIC_SRC) $(NCC_EXE_SRC)

out.stage2/ncc.exe: $(COMPILER) out.stage2/Nemerle.stage2.dll \
		out.stage2/Nemerle.Compiler.stage2.dll $(NCC_EXE_SRC)
	$(COMP) [stage2] ncc.exe
	$(Q)$(RUN_COMPILER) -target-exe -out:$@ \
		-reference:out.stage2/Nemerle.stage2.dll \
		-reference:out.stage2/Nemerle.Compiler.stage2.dll \
		$(GENERIC_SRC) $(NCC_EXE_SRC)

out.stage3/ncc.exe: $(COMPILER) out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll $(NCC_EXE_SRC)
	$(COMP) [stage3] ncc.exe
	$(Q)$(RUN_COMPILER) -target-exe -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		-reference:out.$(STAGE)/Nemerle.Compiler.dll \
		$(GENERIC_SRC) $(NCC_EXE_SRC)

sync:
	cd out.stage3 ; \
	for x in *.dll *.exe ; do \
		mv -vf ../../boot/$$x ../../boot/$$x.old ; \
		cp -vf $$x ../../boot/$$x ; \
	done

.PHONY: boot all tests t test ncc-il

t test: tests

tests:
	cd testsuite && $(MAKE)

install: ncc.exe
	if test ! -d $(BINDIR) ; then mkdir -p $(BINDIR) ; fi
	$(NGEN) ncc.exe
	$(INSTALL) -m 644 ncc.exe.so $(BINDIR)/ncc.so
	$(INSTALL) -m 755 ncc.exe $(BINDIR)/ncc

clean:
	cd testsuite && $(MAKE) clean
	rm -f *.exe.so *.dll.so *.pdb core.[0-9]* core
	rm -rf out.stage*
