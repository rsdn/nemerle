NPATH   = ../..
NCC     = $(NPATH)/ncc/ncc.exe
LIBS    = $(NPATH)/lib/aliases.n $(NPATH)/lib/core.n \
          $(NPATH)/lib/hashtable.n $(NPATH)/lib/option.n
COMPILE = $(NCC) $(LIBS) -r:System \
                         -r:System.Xml
CS      = mcs /nowarn:162 /debug+

SOURCES = httpd.n config.n logger.n

httpd.exe: out.cs
	$(CS) /out:$@ $<

out.cs: $(SOURCES)
	$(COMPILE) $(SOURCES)

clean:
	rm -f out.cs httpd.exe
