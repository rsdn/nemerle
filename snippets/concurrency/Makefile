include ../../config.mak

NPATH   = ../../ncc/out.stage3/
COMPILE = $(NET_ENGINE) $(NET_FLAGS) $(NPATH)/ncc.exe

all: buffer.exe rendezvous.exe onecell.exe readerwriter.exe readerwriterfair.exe test.exe

buffer.exe: buffer.n
	$(COMPILE) -texe -out:$@ $<

rendezvous.exe: rendezvous.n
	$(COMPILE) -texe -out:$@ $<

onecell.exe: onecell.n
	$(COMPILE) -texe -out:$@ $<

readerwriter.exe: readerwriter.n
	$(COMPILE) -texe -out:$@ $<

readerwriterfair.exe: readerwriterfair.n 
	$(COMPILE) -texe -out:$@ $<

test.exe: test.n 
	$(COMPILE) -texe -out:$@ $<

clean:
	rm -f buffer.exe rendezvous.exe onecell.exe readerwriter.exe readerwriterfair.exe test.exe
