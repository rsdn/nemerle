NPATH   = ../..
NCC     = $(NPATH)/ncc/ncc.exe
LIBS    = $(NPATH)/lib/aliases.n $(NPATH)/lib/core.n \
          $(NPATH)/lib/hashtable.n $(NPATH)/lib/option.n
COMPILE = $(NCC) $(LIBS) -r:/usr/local/lib/System.dll
CS      = mcs /nowarn:162 /debug+

ntrace.exe: out.cs
	$(CS) /out:ntrace.exe out.cs

out.cs: ntrace.n
	$(COMPILE) ntrace.n

