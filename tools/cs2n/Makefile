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

TOP_LEVEL = ../..

include $(TOP_LEVEL)/config.mak
include config.cs2n.mak

######################################################
#VARIABLES
######################################################

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NCC_DIR = $(TOP_LEVEL)/ncc/out.stage3
NCC = $(EXECUTE) $(NCC_DIR)/ncc.exe -q
MCS = mcs

NEMERLE_DLLS = Nemerle.dll Nemerle.Macros.dll Nemerle.Compiler.dll
ANTLR_DLL = antlr.runtime.dll

GRAMMAR_FILE = csharpgrammar.g
GENERATED_SOURCES_LEXER = CSharpLexer.cs
GENERATED_SOURCES_PARSER = CSharpParser.cs  
GENERATED_SOURCES_TOKEN_TYPES = CSharpParserTokenTypes.cs
GENERATED_TOKEN_DESCRIPTION = CSharpParserTokenTypes.txt

GENERATED_SOURCES = $(GENERATED_SOURCES_LEXER) $(GENERATED_SOURCES_PARSER) $(GENERATED_SOURCES_TOKEN_TYPES) 

NEMERLE_CSHARP_DLL_SOURCES = ExtendedToken.n Emit.n StatementTree.n
CS2N_EXE_SOURCES = cs2n.n

######################################################
# TARGETS
######################################################

all: $(NEMERLE_DLLS) $(ANTLR_DLL) cs2n.exe

cs2n.exe: Nemerle.CSharp.CS.dll  $(CS2N_EXE_SOURCES)
	$(NCC) -texe -r:Nemerle.CSharp.CS.dll -o:$@ $(CS2N_EXE_SOURCES)

Nemerle.CSharp.CS.dll : Nemerle.CSharp.dll $(GENERATED_SOURCES_PARSER)
	$(MCS) /t:library /r:antlr.runtime.dll /r:Nemerle.CSharp.dll /r:Nemerle.dll -out:$@ $(GENERATED_SOURCES)

Nemerle.CSharp.dll : $(NEMERLE_CSHARP_DLL_SOURCES) 
	$(NCC) -tdll -r:antlr.runtime.dll -o:$@ $(NEMERLE_CSHARP_DLL_SOURCES) 

ifneq "$(classpath)" ""

$(GENERATED_SOURCES_PARSER): $(GRAMMAR_FILE)
	java -classpath $(classpath) antlr.Tool $(GRAMMAR_FILE) 2> error_log

endif

$(NEMERLE_DLLS): $(NCC_DIR)/$@
	cp -f $(NCC_DIR)/$@ $@

$(ANTLR_DLL): $(antlrhome)/lib/csharp/src/$@
	cp -f $(antlrhome)/lib/csharp/src/$@ $@

config.cs2n.mak: configure
	./configure

dist-clean: clean
	rm -f  config.cs2n.mak

ifeq "$(classpath)" ""

clean:
	rm -f *.exe *.dll *.pdb *~ error_log

else

clean:
	rm -f *.exe *.dll *.pdb *~ error_log $(GENERATED_SOURCES) $(GENERATED_TOKEN_DESCRIPTION)

endif
