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

n_src = option.n list.n util.n parsetree.n tree.n scan_globals.n treeutil.n \
	bind_types.n tyvars.n tyinfo.n tyutil.n check_constraints.n \
	ast.n tyexpr.n typedtree.n extensions.n cgtree.n cgexpr.n \
	cgflat.n env.n
cs_src = lexer.cs main.cs parser.cs
all_src = $(n_src) $(cs_src)
all_cs = env.n.cs $(cs_src)
target = ncc.exe

all: $(target)

# here we just test if new-built compiler is capable of builing
# itself, run make t to check testsuite
test:
	mono --debug ./$(target) lib/core.n $(n_src)

$(target): $(all_src)
	../npc/npc.opt $(n_src)
	mcs -debug+ -o $@ $(all_cs)

t: test
	set -x ; for f in ../t/*.n ; do \
		mono --debug ./$(target) ../lib/core.n $$f || true ; \
	done

parser.cs: parser.jay
	jay -v -t -c parser.jay < `jay -p`/skeleton.cs > parser.cs
	perl -p -i -e 's/(public void yyerror )/virtual $$1/' parser.cs

clean:
	rm -f *.n.cs parser.cs $(target) y.output test.exe
