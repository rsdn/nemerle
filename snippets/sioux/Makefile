NPATH   = ../..
NCC     = $(NPATH)/ncc/ncc.exe
LIBS    = $(NPATH)/lib/aliases.n $(NPATH)/lib/core.n \
          $(NPATH)/lib/hashtable.n $(NPATH)/lib/option.n
COMPILE = $(NCC) $(LIBS) -r:/usr/local/lib/System.dll \
                         -r:/usr/local/lib/System.Xml.dll
CS      = mcs /nowarn:162 /debug+

SOURCES = httpd.n config.n logger.n

httpd.exe: out.cs
	$(CS) /out:$@ $<

out.cs: $(SOURCES)
	$(COMPILE) $(SOURCES)

clean:
	rm -f out.cs httpd.exe
