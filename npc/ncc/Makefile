n_src = option.n list.n util.n parsetree.n tree.n env.n
cs_src = lexer.cs main.cs parser.cs
all_src = $(n_src) $(cs_src)
all_cs = env.n.cs $(cs_src)
target = ncc.exe

all: $(target)
	mono --debug ./$(target) ../lib/core.n ../t/*.n *.n

$(target): $(all_src)
	../npc $(n_src)
	mcs -debug+ -o $@ $(all_cs)

t: test.exe
	mono --debug ./test.exe

test.exe: option.n tree.n test.n
	../npc $^
	mcs -debug+ -o $@ test.n.cs

parser.cs: parser.jay
	jay -v -t -c parser.jay < `jay -p`/skeleton.cs > parser.cs
	perl -p -i -e 's/(public void yyerror )/virtual $$1/' parser.cs

clean:
	rm -f *.n.cs parser.cs $(target) y.output test.exe
