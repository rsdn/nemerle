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

obj = \
	util.cmo 		\
	ast_util.cmo 		\
	ty_info.cmo 		\
	ty_env.cmo		\
	ty_util.cmo 		\
	env.cmo 		\
	builtin_types.cmo	\
	lexer.cmo 		\
	parser.cmo 		\
	flatten_ns.cmo		\
	ty_bind.cmo 		\
	make_ty_info.cmo	\
	ty_constraints.cmo	\
	name_bind.cmo		\
	ty_expr.cmo		\
	cil_check.cmo		\
	cg_util.cmo		\
	closure.cmo		\
	cg_stack.cmo		\
	cg_expr.cmo		\
	main.cmo
objx = $(obj:.cmo=.cmx)
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
OFLAGS = -g -warn-error A

include make.config

all: npc npc.opt

opt: npc.opt

npc: $(obj)
	$(OCAMLC) $(OFLAGS) -o npc $(obj)

npc.opt: $(objx)
	$(OCAMLOPT) -o npc.opt $(objx)
	
%.cmo: %.ml %.cmi
	$(OCAMLC) $(OFLAGS) $(WARN_I) -c $<
	
parser.cmo: parser.ml parser.cmi
	$(OCAMLC) $(OFLAGS) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OFLAGS) -c $<

%.ml %.mli: %.mly
	ocamlyacc -v $<

lexer.ml: lexer.mll
	ocamllex $<

.deps: parser.ml lexer.ml
	ocamldep *.ml *.mli > .deps

make.config:
	if [ "x$$($(OCAMLC) -w I 2>&1)" = x ] ; then \
		echo WARN_I := -w I ; \
	else \
		echo WARN_I := ; \
	fi > make.config
	
tags:
	otags -vi *.ml ast.mli

clean:
	rm -f *.cm* *.o npc npc.opt 
	rm -f .deps parser.ml parser.mli lexer.ml parser.output tags
	rm -f make.config

.PHONY: t

t:
	$(MAKE) -C t

include .deps
