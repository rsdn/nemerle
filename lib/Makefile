#
# Copyright (c) 2004 The University of Wroclaw.
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

src = \
	aliases.n		\
	core.n			\
	getopt.n		\
	hashtable.n		\
	icollection.n		\
	ienumerable.n		\
	ienumerator.n		\
	input.n			\
	linkedlist.n		\
	list.n			\
	option.n		\
	queue.n			\
	stack.n			\
	tree.n			\
	idictionaryenumerator.n \
	idictionary.n		\
	dictionaryentry.n	\
	heap.n			\
	icomparable.n


EXECUTE = $(NET_ENGINE) $(NET_FLAGS)
NEMFLAG = 

all: Nemerle.dll

Nemerle.dll: $(src)
	time $(EXECUTE) ../ncc/ncc.exe $(NEMFLAG) -tdll -out $@ $(src)

../config.mak:
	$(MAKE) -C .. config.mak

clean:
	rm -f *.exe *.exe.so *.dll *.dll.so ncc*.il *.pdb core.[0-9]* core
