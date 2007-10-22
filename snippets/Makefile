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

EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
THISDIR = $(shell if cygpath --help 2>/dev/null 1>&2; then echo `cygpath -m $(CURDIR)`; else echo $(CURDIR); fi)

############################################################
# TARGETS
############################################################

all:
	$(Q)$(MAKE) TEST_FILES=$(THISDIR)/*.n -C ../ncc/testsuite/ test
	$(Q)$(MAKE) -C synq all
	$(Q)$(MAKE) -C ants-icfp2004 all
	$(Q)$(MAKE) -C ants-icfp2004/simulator all
	$(Q)$(MAKE) -C ants-icfp2004 run
	$(Q)$(MAKE) -C sioux all
	$(Q)$(MAKE) -C shootout all
	$(Q)$(MAKE) -C course all
	$(Q)$(MAKE) -C sokoban all
	$(Q)$(MAKE) -C concurrency all
	$(Q)$(MAKE) -C designpatt all
	$(Q)$(MAKE) -C aop test

myPoll.exe: myPoll.n
	$(EXECUTE) ../ncc/out.stage3/ncc.exe -r Npgsql -r System.Web myPoll.n -o myPoll.exe

blogComments.exe: blogComments.n
	$(EXECUTE) ../ncc/out.stage3/ncc.exe -r System.Web $< -o $@

nswiki.exe: nswiki.n
	$(EXECUTE) ../ncc/out.stage3/ncc.exe -r System.Web $< -o $@

sql: sql.n
	$(EXECUTE) ../ncc/out.stage3/ncc.exe -r Npgsql -r ../macros/Nemerle.Data.Npgsql.dll sql.n -o sql.exe

clean:
	$(MAKE) -C synq clean
	$(MAKE) -C sioux clean
	$(MAKE) -C ants-icfp2004 clean
	$(MAKE) -C course clean
	$(MAKE) -C sokoban clean
	$(MAKE) -C concurrency clean
	$(MAKE) -C aop clean
	rm -f *.exe *.il *.dll *.netmodule ext_test.out core core.[0-9]*
	rm -f test.err test_error.log a.out
