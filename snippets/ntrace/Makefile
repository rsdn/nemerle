include ../../config.mak

NPATH   = ../../boot
COMPILE = $(NET_ENGINE) $(NET_FLAGS) $(NPATH)/ncc.exe
DISTFILES = Makefile README *.n *.c *.h

ntrace.exe: ntrace.n
	$(COMPILE) -texe -out:$@ $<

dist:
	$(Q)cp $(DISTFILES) $(DIR)
	
clean:
	rm -f ntrace.exe
