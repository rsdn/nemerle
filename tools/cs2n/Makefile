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

TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak

INST	= @echo INSTALL $1

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe
MCS = $(CSC)
SNK_COMPILER = -keyfile:$(TOP_LEVEL)/misc/keys/Nemerle.Compiler.snk

NEMERLE_DLLS = Nemerle.dll
ANTLR_DLL = antlr.runtime.dll

GRAMMAR_FILE = csharpgrammar.g
GENERATED_SOURCES_LEXER = CSharpLexer.cs
GENERATED_SOURCES_PARSER = CSharpParser.cs  
GENERATED_SOURCES_TOKEN_TYPES = CSharpParserTokenTypes.cs
GENERATED_TOKEN_DESCRIPTION = CSharpParserTokenTypes.txt

GENERATED_SOURCES = $(GENERATED_SOURCES_LEXER) $(GENERATED_SOURCES_PARSER) $(GENERATED_SOURCES_TOKEN_TYPES) 

NEMERLE_CSHARP_DLL_SOURCES = ExtendedToken.n Emit.n StatementTree.n GotoElimination.n AssemblyInfo.n \
				TranslationOptions.n Message.n
CS2N_EXE_SOURCES = cs2n.n 

ifeq ($(origin GOTO), command line)
	override GOTO = -def:GOTO_ELIMINATION
else
    	override GOTO = 
endif

######################################################
# TARGETS
######################################################

all: cs2n.exe

cs2n.exe: $(NEMERLE_DLLS) $(ANTLR_DLL) Nemerle.CSharp.CS.dll  $(CS2N_EXE_SOURCES)
	$(NCC) -greedy- -texe -r:./antlr.runtime.dll -r:Nemerle.CSharp.CS.dll -r:Nemerle.CSharp.dll -o:$@ $(CS2N_EXE_SOURCES)

Nemerle.CSharp.CS.dll : Nemerle.CSharp.dll $(GENERATED_SOURCES_PARSER) $(ANTLR_DLL)
	$(MCS) -t:library -r:./antlr.runtime.dll -r:Nemerle.CSharp.dll -r:Nemerle.dll -out:$@ $(SNK_COMPILER) $(GENERATED_SOURCES) AssemblyInfo.cs

Nemerle.CSharp.dll : $(NEMERLE_CSHARP_DLL_SOURCES) $(ANTLR_DLL)
	$(NCC) -tdll -r:./antlr.runtime.dll -r:Nemerle.Compiler.dll -o:$@ $(SNK_COMPILER) $(NEMERLE_CSHARP_DLL_SOURCES) 

$(GENERATED_SOURCES_PARSER): $(GRAMMAR_FILE)
	$(ANTLR) $(GRAMMAR_FILE) 2> error_log

$(NEMERLE_DLLS): $(NCC_DIR)/$@
	$(Q)cp -f $(NCC_DIR)/$@ $@

$(ANTLR_DLL): antlr/$@
	@test -f antlr/$@ || ( echo "No antlr.runtime.dll found. Please run ./configure again." && false )
	$(Q)cp -f antlr/$@ $@

dist-clean: clean

# put the antlr-generated stuff in tarball so it will build without it
dist: $(GENERATED_SOURCES_PARSER)

tests-clean:
	rm -f tests/*.n tests/*.exe tests/*.log

test: log

log: 
	./test | tee log

etest:
	./test errors | tee log

install: cs2n.exe
	$(Q)$(GACUTIL_COMMAND) Nemerle.CSharp.dll
	$(Q)$(GACUTIL_COMMAND) Nemerle.CSharp.CS.dll
	$(Q)$(GACUTIL_COMMAND) antlr.runtime.dll
	$(INST) "[$(BINDIR)]" cs2n.exe
	$(Q)$(INSTALL) -m 755 cs2n.exe $(DESTDIR)$(BINDIR)/cs2n.exe
	$(Q)if [ "$(NET_ENGINE)" != "" ] ; then \
		echo "#!/bin/sh" > $(DESTDIR)$(SCRDIR)/cs2n && \
		echo '$(NET_ENGINE) $(BINDIR)/cs2n.exe "$$@"' >> $(DESTDIR)$(SCRDIR)/cs2n ; \
		chmod 755 $(DESTDIR)$(SCRDIR)/cs2n ; \
	fi

uninstall:
	$(RM) Nemerle.CSharp
	$(GACUTIL_UNINSTALL_COMMAND) Nemerle.CSharp
	$(RM) Nemerle.CSharp.CS
	$(GACUTIL_UNINSTALL_COMMAND) Nemerle.CSharp.CS
	$(RM) antlr.runtime
	$(GACUTIL_UNINSTALL_COMMAND) antlr.runtime
	$(RM) $(BINDIR)/cs2n
	$(Q)rm -f $(BINDIR)/cs2n
	$(RM) $(BINDIR)/cs2n.exe
	$(Q)rm -f $(BINDIR)/cs2n.exe


clean:
	rm -f *.exe *.dll *.pdb *~ error_log $(GENERATED_SOURCES) $(GENERATED_TOKEN_DESCRIPTION)
