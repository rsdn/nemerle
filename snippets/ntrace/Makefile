NPATH   = ../../boot
COMPILE = $(NPATH)/ncc.exe

ntrace.exe: ntrace.n
	$(COMPILE) -texe -out:$@ $<

clean:
	rm -f ntrace.exe
