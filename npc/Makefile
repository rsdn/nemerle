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
	$(MAKE) -C ncc clean

.PHONY: t

t:
	$(MAKE) -C t

include .deps
