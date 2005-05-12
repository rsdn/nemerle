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
include ../config.mak

############################################################
# VARIABLES
############################################################

COMPILER_PATH = ../boot/
NCC_FLAGS     = -pedantic-lexer $(NEM_FLAGS)
COMPILER      = $(COMPILER_PATH)ncc.exe
COMPILER_DEP  = $(COMPILER_PATH)ncc.exe $(COMPILER_PATH)Nemerle.Macros.dll
EXECUTE       = $(NET_ENGINE) $(NET_FLAGS)
RUN_COMPILER  = $(Q)$(TIME) $(EXECUTE) $(COMPILER) -no-stdlib -r:mscorlib -r:System $(NCC_FLAGS)

GENERIC_SRC =

NEMERLE_DLL_SRC = \
	../lib/internal-numbered.n	\
	../lib/internal.n		\
	../lib/core.n			\
	../lib/macros.n			\
	../lib/getopt.n			\
	../lib/hashtable.n		\
	../lib/icloneable.n		\
	../lib/icollection.n		\
	../lib/ienumerable.n		\
	../lib/ienumerator.n		\
	../lib/input.n			\
	../lib/linkedlist.n		\
	../lib/list.n			\
	../lib/listenumerator.n		\
	../lib/option.n			\
	../lib/queue.n			\
	../lib/stack.n			\
	../lib/tree.n			\
	../lib/idictionaryenumerator.n	\
	../lib/idictionary.n		\
	../lib/dictionaryentry.n	\
	../lib/heap.n			\
	../lib/icomparable.n		\
	../lib/narray.n			\
	../lib/nstring.n                \
	../lib/pair.n			\
	../lib/set.n			\
	../lib/vector.n			\
	../lib/concurrency.n		\
	../lib/AssemblyInfo.n           \
	../lib/LazyValue.n              \

NEMERLE_COMPILER_DLL_SRC = \
	generation/CompileTypedMethod.n  \
	generation/CompileTypedTree.n	 \
	generation/CompiledTree.n	 \
	generation/HierarchyEmitter.n	 \
	generation/ILEmitter.n		 \
	generation/MatchingCompiler.n	 \
	generation/Optimizer.n		 \
	generation/Postprocessing.n	 \
	generation/Typer3.n		 \
	generation/ImplementsWrapperMaker.n \
	hierarchy/BuiltinMethod.n        \
	hierarchy/ClassMembers.n	 \
	hierarchy/CustomAttribute.n	 \
	hierarchy/DelegateClassGen.n	 \
	hierarchy/MacroClassGen.n	 \
	hierarchy/NamespaceTree.n	 \
	hierarchy/ScanTypeHierarchy.n	 \
	hierarchy/TypeBuilder.n		 \
	hierarchy/TypeInfo.n		 \
	hierarchy/XmlDump.n		 \
	parsing/AST.n			 \
	parsing/Lexer.n			 \
	parsing/MainParser.n		 \
	parsing/MainParser-Extensions.n  \
	parsing/ParseTree.n		 \
	parsing/PreParser.n		 \
	parsing/Utility.n		 \
	typing/ConstantFolder.n		 \
	typing/MacroRegistry.n		 \
	typing/Macros.n			 \
	typing/DecisionTreeBuilder.n     \
	typing/TypedTree.n		 \
	typing/ErrorMessage.n		 \
	typing/LocalContext.n		 \
	typing/LocalValue.n		 \
	typing/MType.n			 \
	typing/MacroColorizer.n		 \
	typing/Messenger.n	         \
	typing/OverloadPossibility.n	 \
	typing/RestartInErrorMode.n	 \
	typing/Solver.n			 \
	typing/StaticTyVar.n		 \
	typing/Subst.n			 \
	typing/TyVar.n			 \
	typing/TyVarEnv.n		 \
	typing/Typer-CallTyper.n	 \
	typing/Typer-DelayedTyping.n	 \
	typing/Typer-PatternTyper.n	 \
	typing/Typer.n			 \
	typing/Typer2.n			 \
	codedom/NemerleCodeCompiler.n	 \
	codedom/NemerleCodeGenerator.n	 \
	codedom/NemerleCodeProvider.n	 \
	external/Codec.n		 \
	external/InternalTypes.n	 \
	external/LibrariesLoader.n	 \
	misc/AssemblyInfo.n		 \
	misc/PrettyPrint.n		 \
	CompilationOptions.n		 \
	passes.n			 \


NCC_EXE_SRC       = main.n

STDMACROS_DLL_SRC = \
        ../macros/core.n \
        ../macros/compiler.n \
        ../macros/assertions.n \
	../macros/operators.n \
        ../macros/io.n \
        ../macros/text.n \
	../macros/AssemblyInfo.n \
	../macros/xml.n \
	../macros/concurrency.n \
	../macros/Data.n \
	../macros/Logging.n \
	../macros/Util.n \
	../macros/Internals.n \
	../macros/Nemerle.n \

############################################################
# OUTPUT
############################################################

COMPARE = @echo COMPARE $1 $2 $3
DEAS    = @echo DASM    $1 $2
AOT     = @echo NGEN    $1
VERIFY  = @echo VERIFY  $1
COMP    = @echo COMPILE $1
MKDIR   = @echo MKDIR   $1
TAR     = @echo TAR     $1
CP      = @echo CP      $1
INST	= @echo INSTALL $1

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
	$(Q)cd .. && $(MAKE) config.mak

stage1:
	$(Q)$(MAKE) STAGE=$@ COMPILER_PATH=../boot/ aux-stage

stage2:
	$(Q)$(MAKE) STAGE=$@ COMPILER_PATH=out.stage1/ aux-stage

stage3:
	$(Q)$(MAKE) STAGE=$@ COMPILER_PATH=out.stage2/ last-stage

t3:
	$(MAKE) STAGE=stage1 NEM_FLAGS="$(NEM_FLAGS) -def:T3" out.stage1/Nemerle.Compiler.stage1.dll

aux-stage:
	$(MKDIR) out.$(STAGE)
	$(Q)mkdir -p out.$(STAGE)
	$(Q)$(MAKE) aux-stage-files

aux-stage-files: \
		out.$(STAGE)/Nemerle.$(STAGE).dll \
		out.$(STAGE)/Nemerle.Compiler.$(STAGE).dll \
		out.$(STAGE)/Nemerle.Macros.dll \
		out.$(STAGE)/ncc.exe

last-stage:
	$(MKDIR) out.$(STAGE)
	$(Q)mkdir -p out.$(STAGE)
	$(Q)$(MAKE) last-stage-files

last-stage-files: \
		out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll \
		out.$(STAGE)/Nemerle.Macros.dll \
		out.$(STAGE)/ncc.exe

lib:
	$(MAKE) STAGE=stage1 out.stage1/Nemerle.stage1.dll

boot: stage1 stage2 stage3 compare ngen-stage3

out.stage3/all.done: \
	out.stage3/Nemerle.Compiler.dll.done \
	out.stage3/Nemerle.Macros.dll.done \
	out.stage3/Nemerle.dll.done \
	out.stage3/ncc.exe.done
	$(Q)touch $@

ngen-stage3: out.stage3/ngen.done

out.stage3/ngen.done: out.stage3/all.done
	$(AOT) Nemerle.dll
	$(Q)cd out.stage3; $(NGEN) Nemerle.dll
	$(AOT) Nemerle.Compiler.dll
	$(Q)cd out.stage3; $(NGEN) Nemerle.Compiler.dll
	$(AOT) Nemerle.Macros.dll
	$(Q)cd out.stage3; $(NGEN) Nemerle.Macros.dll
	$(AOT) ncc.exe
	$(Q)cd out.stage3; $(NGEN) ncc.exe
	$(Q)touch $@

verify:
	$(VERIFY) Nemerle.dll
	$(Q)$(PEVERIFY) out.stage3/Nemerle.dll
	$(VERIFY) Nemerle.Compiler.dll
	$(Q)$(PEVERIFY) out.stage3/Nemerle.Compiler.dll
	$(VERIFY) Nemerle.Macros.dll
	$(Q)$(PEVERIFY) out.stage3/Nemerle.Macros.dll
	$(VERIFY) ncc.exe
	$(Q)$(PEVERIFY) out.stage3/ncc.exe

compare: out.stage3/compare.done

out.stage3/compare.done: out.stage3/all.done
	$(Q)$(MAKE) compare-one-with-stage CMP_TARGET=Nemerle
	$(Q)$(MAKE) compare-one-with-stage CMP_TARGET=Nemerle.Compiler
	$(Q)$(MAKE) compare-one CMP_TARGET=Nemerle.Macros.dll
	$(Q)$(MAKE) compare-one CMP_TARGET=ncc.exe
	$(Q)touch $@

compare-one:
	$(Q)test -f out.stage2/$(CMP_TARGET)
	$(Q)test -f out.stage3/$(CMP_TARGET) 
	$(DEAS) "[stage2]" $(CMP_TARGET)
	$(Q)$(DASM) out.stage2/$(CMP_TARGET) | \
		grep -v -E '(^//|// GUID|MVID|\.ver)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)2.il
	$(DEAS) "[stage3]" $(CMP_TARGET)
	$(Q)$(DASM) out.stage3/$(CMP_TARGET) | \
		grep -v -E '(^//|// GUID|MVID|\.ver)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)3.il
	$(COMPARE) "[stage2]" "[stage3]" $(CMP_TARGET)
	$(Q)diff -u -w $(CMP_TARGET)2.il $(CMP_TARGET)3.il
	$(Q)rm -f $(CMP_TARGET)2.il $(CMP_TARGET)3.il

compare-one-with-stage:
	$(Q)test -f out.stage2/$(CMP_TARGET).stage2.dll
	$(Q)test -f out.stage3/$(CMP_TARGET).dll
	$(DEAS) "[stage2]" $(CMP_TARGET)
	$(Q)$(DASM) out.stage2/$(CMP_TARGET).stage2.dll | \
		grep -v -E '(^//|// GUID|MVID|\.ver)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)2.il
	$(DEAS) "[stage3]" $(CMP_TARGET)
	$(Q)$(DASM) out.stage3/$(CMP_TARGET).dll | \
		grep -v -E '(^//|// GUID|MVID|\.ver)' \
		| sed -e 's/\.stage2//g' > $(CMP_TARGET)3.il
	$(COMPARE) "[stage2] [stage3]" $(CMP_TARGET)
	$(Q)diff -u -w $(CMP_TARGET)2.il $(CMP_TARGET)3.il
	$(Q)rm -f $(CMP_TARGET)2.il $(CMP_TARGET)3.il

out.$(STAGE)/Nemerle.dll out.$(STAGE)/Nemerle.$(STAGE).dll: \
		$(COMPILER_DEP) $(NEMERLE_DLL_SRC)
	$(COMP) "[$(STAGE)]" Nemerle.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ $(GENERIC_SRC) $(NEMERLE_DLL_SRC)
	$(Q)touch $@.done

out.$(STAGE)/Nemerle.Compiler.dll: \
		$(COMPILER_DEP) out.$(STAGE)/Nemerle.dll $(NEMERLE_COMPILER_DLL_SRC)
	$(COMP) "[$(STAGE)]" Nemerle.Compiler.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		$(GENERIC_SRC) $(NEMERLE_COMPILER_DLL_SRC)
	$(Q)touch $@.done

out.$(STAGE)/Nemerle.Compiler.$(STAGE).dll: \
		$(COMPILER_DEP) out.$(STAGE)/Nemerle.$(STAGE).dll $(NEMERLE_COMPILER_DLL_SRC)
	$(COMP) "[$(STAGE)]" Nemerle.Compiler.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.$(STAGE)/Nemerle.$(STAGE).dll \
		$(GENERIC_SRC) $(NEMERLE_COMPILER_DLL_SRC)
	$(Q)touch $@.done

out.stage1/Nemerle.Macros.dll: $(COMPILER_DEP) out.stage1/Nemerle.stage1.dll \
		out.stage1/Nemerle.Compiler.stage1.dll $(STDMACROS_DLL_SRC)
	$(COMP) "[stage1]" Nemerle.Macros.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.stage1/Nemerle.stage1.dll \
		-reference:out.stage1/Nemerle.Compiler.stage1.dll \
		$(GENERIC_SRC) $(STDMACROS_DLL_SRC)
	$(Q)touch $@.done

out.stage2/Nemerle.Macros.dll: $(COMPILER_DEP) out.stage2/Nemerle.stage2.dll \
		out.stage2/Nemerle.Compiler.stage2.dll $(STDMACROS_DLL_SRC)
	$(COMP) "[stage2]" Nemerle.Macros.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.stage2/Nemerle.stage2.dll \
		-reference:out.stage2/Nemerle.Compiler.stage2.dll \
		$(GENERIC_SRC) $(STDMACROS_DLL_SRC)
	$(Q)touch $@.done

out.stage3/Nemerle.Macros.dll: $(COMPILER_DEP) out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll $(STDMACROS_DLL_SRC)
	$(COMP) "[stage3]" Nemerle.Macros.dll
	$(Q)$(RUN_COMPILER) -target-library -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		-reference:out.$(STAGE)/Nemerle.Compiler.dll \
		$(GENERIC_SRC) $(STDMACROS_DLL_SRC)
	$(Q)touch $@.done

out.stage1/ncc.exe: $(COMPILER_DEP) out.stage1/Nemerle.stage1.dll \
		out.stage1/Nemerle.Compiler.stage1.dll $(NCC_EXE_SRC)
	$(COMP) "[stage1]" ncc.exe
	$(Q)$(RUN_COMPILER) -target-exe -out:$@ \
		-reference:out.stage1/Nemerle.stage1.dll \
		-reference:out.stage1/Nemerle.Compiler.stage1.dll \
		$(GENERIC_SRC) $(NCC_EXE_SRC)
	$(Q)touch $@.done

out.stage2/ncc.exe: $(COMPILER_DEP) out.stage2/Nemerle.stage2.dll \
		out.stage2/Nemerle.Compiler.stage2.dll $(NCC_EXE_SRC)
	$(COMP) "[stage2]" ncc.exe
	$(Q)$(RUN_COMPILER) -target-exe -out:$@ \
		-reference:out.stage2/Nemerle.stage2.dll \
		-reference:out.stage2/Nemerle.Compiler.stage2.dll \
		$(GENERIC_SRC) $(NCC_EXE_SRC)
	$(Q)touch $@.done

out.stage3/ncc.exe: $(COMPILER_DEP) out.$(STAGE)/Nemerle.dll \
		out.$(STAGE)/Nemerle.Compiler.dll $(NCC_EXE_SRC)
	$(COMP) "[stage3]" ncc.exe
	$(Q)$(RUN_COMPILER) -target-exe -out:$@ \
		-reference:out.$(STAGE)/Nemerle.dll \
		-reference:out.$(STAGE)/Nemerle.Compiler.dll \
		$(GENERIC_SRC) $(NCC_EXE_SRC)
	$(Q)touch $@.done

doc: 
	$(Q)$(MAKE) doc-xml COMPILER_PATH=out.stage3/  

doc-xml: 
	$(COMP) "[with xml doc generation]" Nemerle.xml
	$(Q)$(RUN_COMPILER) -target-library -out:temp.dll $(NEMERLE_DLL_SRC) \
	-doc Nemerle.xml
	$(COMP) "[with xml doc generation]" Nemerle.Compiler.xml
	$(Q)$(RUN_COMPILER) -target-library -out:temp.dll $(NEMERLE_COMPILER_DLL_SRC) \
	-ref:out.stage3/Nemerle.dll -doc Nemerle.Compiler.xml
	$(COMP) "[with xml doc generation]" Nemerle.Macros.xml
	$(Q)$(RUN_COMPILER) -target-library -out:temp.dll $(STDMACROS_DLL_SRC) \
	-ref:out.stage3/Nemerle.dll -ref:out.stage3/Nemerle.Compiler.dll -doc Nemerle.Macros.xml
	$(Q)rm temp.dll


sync:
	cd out.stage3 ; \
	for x in *.dll *.exe ; do \
		mv -vf ../../boot/$$x ../../boot/$$x.old ; \
		cp -vf $$x ../../boot/$$x ; \
	done

.PHONY: boot all tests t test

t test: tests
tests:
	$(Q)cd testsuite && $(MAKE)

t-mcs test-mcs: tests-mcs
tests-mcs:
	$(Q)cd testsuite/frommcs && $(MAKE)

check:
	$(MAKE) -C .. check

profile:
	$(Q)../misc/profiler/runprof $(COMPILER) -no-stdlib -t:dll \
          -out:out.stage1/Nemerle.stage1.dll -r System.dll \
          $(NEMERLE_DLL_SRC) > n-prof-log
	$(Q)../misc/profiler/runprof $(COMPILER) -no-stdlib -t:dll \
          -r out.stage1/Nemerle.stage1.dll -r System.dll \
          $(NEMERLE_COMPILER_DLL_SRC) > nc-prof-log
	$(Q)../misc/profiler/prof


halfboot:
	touch out.stage1/*
	touch out.stage2/Nemerle.stage2.dll
	$(Q)$(MAKE) boot

install:
	$(Q)if test ! -d $(DESTDIR)$(BINDIR); then $(INSTALL) -d $(DESTDIR)$(BINDIR); fi
	$(Q)$(GACUTIL_COMMAND) out.stage3/Nemerle.dll
	$(Q)$(GACUTIL_COMMAND) out.stage3/Nemerle.Compiler.dll
	$(Q)$(GACUTIL_COMMAND) out.stage3/Nemerle.Macros.dll
	export MONO_PATH=$$MONO_PATH:$$PWD/out.stage3 ; $(NGEN_GAC)
	$(INST) "[$(BINDIR)]" ncc.exe
	$(Q)$(INSTALL) -m 755 out.stage3/ncc.exe $(DESTDIR)$(BINDIR)/ncc.exe
	$(Q)if [ "$(NET_ENGINE)" != "" ] ; then \
		echo "#!/bin/sh" > $(DESTDIR)$(BINDIR)/ncc && \
		echo '$(NET_ENGINE) $(BINDIR)/ncc.exe "$$@"' >> $(DESTDIR)$(BINDIR)/ncc ; \
		chmod 755 $(DESTDIR)$(BINDIR)/ncc ; \
	fi

clean:
	cd testsuite && $(MAKE) clean
	rm -f *.exe.so *.dll.so *.pdb *.xml core.[0-9]* core *prof-log
	rm -rf out.stage* doc
