n_src = option.n list.n util.n parsetree.n tree.n pass1.n env.n
cs_src = lexer.cs main.cs parser.cs
all_src = $(n_src) $(cs_src)
all_cs = env.n.cs $(cs_src)
target = ncc.exe

all: $(target)
	# here we just test if new-built compiler is capable of builing
	# itself, run make t to check testsuite
	mono --debug ./$(target) ../lib/core.n $(n_src)

$(target): $(all_src)
	../npc $(n_src)
	mcs -debug+ -o $@ $(all_cs)

t:
	set -x ; for f in ../t/*.n ; do \
		mono --debug ./$(target) ../lib/core.n $$f || exit 1 ; \
	done

test-tree: test.exe
	mono --debug ./test.exe

test.exe: option.n tree.n test.n
	../npc $^
	mcs -debug+ -o $@ test.n.cs

parser.cs: parser.jay
	jay -v -t -c parser.jay < `jay -p`/skeleton.cs > parser.cs
	perl -p -i -e 's/(public void yyerror )/virtual $$1/' parser.cs

clean:
	rm -f *.n.cs parser.cs $(target) y.output test.exe
