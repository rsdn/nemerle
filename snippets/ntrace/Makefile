include ../../config.mak

NPATH   = ../../boot
COMPILE = $(NET_ENGINE) $(NET_FLAGS) $(NPATH)/ncc.exe

ntrace.exe: ntrace.n
	$(COMPILE) -texe -out:$@ $<
	
clean:
	rm -f ntrace.exe
