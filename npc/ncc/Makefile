n_src = option.n list.n util.n parsetree.n tree.n scan_globals.n treeutil.n \
	bind_types.n tyvars.n tyinfo.n env.n
cs_src = lexer.cs main.cs parser.cs
all_src = $(n_src) $(cs_src)
all_cs = env.n.cs $(cs_src)
target = ncc.exe

all: $(target) test

# here we just test if new-built compiler is capable of builing
# itself, run make t to check testsuite
test:
	mono --debug ./$(target) ../lib/core.n $(n_src)

$(target): $(all_src)
	../npc.opt $(n_src)
	mcs -debug+ -o $@ $(all_cs)

t:
	set -x ; for f in ../t/*.n ; do \
		mono --debug ./$(target) ../lib/core.n $$f || exit 1 ; \
	done

parser.cs: parser.jay
	jay -v -t -c parser.jay < `jay -p`/skeleton.cs > parser.cs
	perl -p -i -e 's/(public void yyerror )/virtual $$1/' parser.cs

clean:
	rm -f *.n.cs parser.cs $(target) y.output test.exe
